unit Book;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls, LCLIntf, LResources, Process,
  Math, IntfGraphics, FPImage, FPReadPNG, FPReadJPEG, GraphType, LazCanvas,
  FileUtil;


type
  { TBook }
  TBook = class(TObject)
  private
    mTitle      : String;
    mAuthors    : String;
    mISBN       : String;
    mFilePath   : String;
    mImagePath  : String;     // original image path (or '')
    mCover      : TImage;
    mIsSelected : Boolean;
    mScaledW    : Integer;    // last pre-scale width we rendered for
    mScaledH    : Integer;    // last pre-scale height we rendered for

    procedure SetFile(AValue: String);
    procedure SetImage(AValue: String);

  public
    constructor Create(Parent: TComponent);
    destructor Destroy; override;

    procedure BookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BookDoubleClick(Sender: TObject);
    procedure BookCoverPaint(Sender: TObject);
    procedure OpenEditDialogAsync(Data: PtrInt);

    // Call this after you change mCover.Width/Height (layout/resizes)
    procedure EnsureScaledToCoverSize;

    property Title     : String  read mTitle     write mTitle;
    property Authors   : String  read mAuthors   write mAuthors;
    property ISBN      : String  read mISBN      write mISBN;
    property FilePath  : String  read mFilePath  write SetFile;
    property ImagePath : String  read mImagePath write SetImage;
    property Cover     : TImage  read mCover;
    property IsSelected: Boolean read mIsSelected write mIsSelected;
  end;

// Allow main code to temporarily disable PDF cover extraction (e.g., during startup load)
procedure SetPdfCoverGenerationEnabled(AEnabled: Boolean);

implementation

uses UnitBookDialog, Forms, unitAppEvents, unitLog, LazUTF8, unitTempUtils;

procedure TBook.OpenEditDialogAsync({%H-}Data: PtrInt);
var
  dlg: TBookEditDialog;
begin
  dlg := TBookEditDialog.Create(nil);
  try
    dlg.LoadBook(Self);
    if dlg.ShowModal = mrOK then
    begin
      EnsureScaledToCoverSize;
      if Assigned(mCover) then mCover.Invalidate;
      // Persist changes immediately
      NotifyBooksChanged;
    end;
  finally
    dlg.Free;
  end;
end;

var
  gPdfCoverEnabled: Boolean = True;

procedure SetPdfCoverGenerationEnabled(AEnabled: Boolean);
begin
  gPdfCoverEnabled := AEnabled;
end;

{------------------------------------------------------------------------------}
{ Helper: try to render first page of a PDF into a PNG using Poppler          }
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{ Basic painting: selection outline                                            }
{------------------------------------------------------------------------------}
procedure TBook.BookCoverPaint(Sender: TObject);
begin
  // Trace paints to diagnose missing covers
  try
    LogDebugFmt('CoverPaint: visible=%s size=%dx%d hasGraphic=%s fp="%s" img="%s"',
      [BoolToStr(mCover.Visible, True), mCover.Width, mCover.Height,
       BoolToStr(Assigned(mCover.Picture) and Assigned(mCover.Picture.Graphic) and (not mCover.Picture.Graphic.Empty), True),
       mFilePath, mImagePath]);
  except
  end;

  if mIsSelected then
  begin
    mCover.Canvas.Brush.Style := bsClear;
    mCover.Canvas.Pen.Width   := 4;
    mCover.Canvas.Pen.Color   := clRed;
    mCover.Canvas.RoundRect(1, 1, mCover.Width - 1, mCover.Height - 1, 10, 10);
  end;
end;

{------------------------------------------------------------------------------}
{ Mouse handlers (hook up in constructor)                                      }
{------------------------------------------------------------------------------}
procedure TBook.BookMouseDown({%H-}Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
var
  i: Integer;
  ctrl: TControl;
  otherBook: TBook;
begin
  // Right-click: open edit dialog immediately
  if Button = mbRight then
  begin
    // Defer to message queue to avoid re-entrancy during mouse handling/drag
    Application.QueueAsyncCall(@OpenEditDialogAsync, 0);
    Exit;
  end;

  // Exclusive select unless Ctrl is held; toggle in Ctrl mode
  if (mCover.Parent <> nil) and not (ssCtrl in Shift) then
  begin
    for i := 0 to mCover.Parent.ControlCount - 1 do
    begin
      ctrl := mCover.Parent.Controls[i];
      if (ctrl is TImage) and (TImage(ctrl).Tag <> 0) then
      begin
        otherBook := TBook(Pointer(TImage(ctrl).Tag));
        if Assigned(otherBook) then
        begin
          otherBook.mIsSelected := False;
          TImage(ctrl).Invalidate;
        end;
      end;
    end;
  end;

  if (ssCtrl in Shift) then
    mIsSelected := not mIsSelected
  else
    mIsSelected := True;

  mCover.Invalidate;
end;

procedure TBook.BookDoubleClick({%H-}Sender: TObject);
begin
  // Open file / details dialog etc. (your existing logic)
end;

{------------------------------------------------------------------------------}
{ Pre-scale loader: draw once into a bitmap matching the control size          }
{------------------------------------------------------------------------------}
procedure TBook.SetImage(AValue: String);
var
  SrcImg: TLazIntfImage;
  Img : TLazIntfImage;
  Canvas: TLazCanvas;
  dstW, dstH, offX, offY: Integer;
  scale: Double;
  W, H: Integer;
  convOut: String;

  function TryConvertImageToPng(const InFile: String; out OutFile: String): Boolean;
  var
    proc: TProcess;
    exe: String;
    base, nameOnly, tmpDir: String;
    i: Integer;
  begin
    Result := False;
    OutFile := '';
    // Only try to convert if not already a PNG and not already a converted temp file
    if UTF8LowerCase(ExtractFileExt(InFile)) = '.png' then Exit(False);
    if Pos('mybookshelf_cover_', UTF8LowerCase(InFile)) > 0 then Exit(False);

    // Build a safe temp output path to avoid extremely long filenames
    tmpDir := GetTempDir(False);
    nameOnly := ExtractFileName(ChangeFileExt(InFile, ''));
    // sanitize and truncate
    for i := 1 to Length(nameOnly) do
      if not (nameOnly[i] in ['A'..'Z','a'..'z','0'..'9','_','-']) then nameOnly[i] := '_';
    if Length(nameOnly) > 48 then
      SetLength(nameOnly, 48);
    OutFile := IncludeTrailingPathDelimiter(tmpDir) + 'mybookshelf_cover_' + nameOnly + '.png';
    // prefer ImageMagick v7 'magick', then v6 'convert'
    exe := FindDefaultExecutablePath('magick');
    if exe = '' then exe := FindDefaultExecutablePath('convert');
    if exe = '' then
    begin
      LogWarn('TryConvertImageToPng: no ImageMagick found (magick/convert)');
      Exit(False);
    end;
    proc := TProcess.Create(nil);
    try
      proc.Executable := exe;
      // For 'magick', first arg is input; for 'convert' similar syntax works
      // add -auto-orient to respect EXIF rotation
      proc.Parameters.Add(InFile);
      proc.Parameters.Add('-auto-orient');
      proc.Parameters.Add(OutFile);
      proc.Options := [poWaitOnExit];
      proc.ShowWindow := swoHide;
      LogInfoFmt('Converting image to PNG: %s -> %s via %s', [InFile, OutFile, exe]);
      proc.Execute;
      Result := FileExists(OutFile) and (proc.ExitStatus = 0);
    finally
      proc.Free;
    end;
  end;
begin
  // Default state
  mImagePath := '';
  mScaledW := 0; mScaledH := 0;

  // Ensure we have a sensible target size (layout usually sets this)
  W := mCover.Width;  H := mCover.Height;
  if (W <= 0) or (H <= 0) then
  begin
    // fallback: match app default 130x200
    W := 130; H := 200;
    mCover.Width := W; mCover.Height := H;
  end;

  if (AValue <> '') and FileExists(AValue) then
  begin
    SrcImg := TLazIntfImage.Create(0, 0);
    Img := TLazIntfImage.Create(W, H);
    Canvas := TLazCanvas.Create(Img);
    try
      try
        SrcImg.LoadFromFile(AValue);

        Img.FillPixels(colTransparent);

        if (SrcImg.Width > 0) and (SrcImg.Height > 0) then
        begin
          scale := Min(W / SrcImg.Width, H / SrcImg.Height);
          if scale > 1 then scale := 1; // avoid upscale
          dstW := Round(SrcImg.Width * scale);
          dstH := Round(SrcImg.Height * scale);
          offX := (W - dstW) div 2;
          offY := (H - dstH) div 2;
          // BUGFIX: draw source image (not the destination buffer)
          Canvas.StretchDraw(offX, offY, dstW, dstH, SrcImg);
          LogDebugFmt('SetImage: src=%dx%d scale=%.3f dst=%dx%d off=%dx%d',
            [SrcImg.Width, SrcImg.Height, scale, dstW, dstH, offX, offY]);
        end;

        // No runtime scaling anymore; we drew at target size
        mCover.Stretch := False;
        mCover.Center  := False;
        mCover.AutoSize:= False;
        // Assign via Bitmap to avoid PNG handle creation issues
        if Assigned(mCover.Picture) then
        begin
          mCover.Picture.Bitmap.SetSize(W, H);
          mCover.Picture.Bitmap.LoadFromIntfImage(Img);
        end;
        mImagePath := AValue;
        mScaledW := W; mScaledH := H;
        LogInfoFmt('SetImage: applied image "%s" target=%dx%d (pre-scaled)', [mImagePath, W, H]);
        Exit;
      except
        // If pre-scale/assignment failed, try converting to PNG, else fall back to direct load + Stretch
        on E: Exception do
        begin
          LogErrorFmt('SetImage: failed to pre-scale "%s": %s', [AValue, E.Message]);
          // Some files are mislabeled or unsupported; attempt to transcode to PNG if ImageMagick is available
          if TryConvertImageToPng(AValue, convOut) and FileExists(convOut) then
          begin
            LogInfoFmt('SetImage: retry with converted PNG "%s"', [convOut]);
            RegisterTempCoverFile(convOut);
            SetImage(convOut);
            Exit;
          end
          else
          begin
            try
              mCover.Stretch := True;
              mCover.Center  := True;
              mCover.AutoSize:= False;
              mCover.Picture.LoadFromFile(AValue);
              mImagePath := AValue;
              // Mark as scaled to current control to avoid immediate rescale loop
              mScaledW := mCover.Width; mScaledH := mCover.Height;
              LogInfoFmt('SetImage: applied image "%s" via direct load (stretch)', [mImagePath]);
              Exit;
            except
              on E2: Exception do LogErrorFmt('SetImage: direct load failed for "%s": %s', [AValue, E2.Message]);
            end;
          end;
        end;
      end;
    finally
      Canvas.Free;
      Img.Free;
      SrcImg.Free;
    end;
  end;

  // Generic fallback
  mCover.Stretch := True;
  mCover.Picture.LoadFromLazarusResource('generic_cover');
  LogWarn('SetImage: using generic cover');
end;

{------------------------------------------------------------------------------}
{ EnsureScaledToCoverSize: re-render if size changed since last pre-scale      }
{------------------------------------------------------------------------------}
procedure TBook.EnsureScaledToCoverSize;
begin
  if (mImagePath <> '') and ((mScaledW <> mCover.Width) or (mScaledH <> mCover.Height)) then
  begin
    LogInfoFmt('EnsureScaledToCoverSize: rescaling from %dx%d to %dx%d for "%s"',
      [mScaledW, mScaledH, mCover.Width, mCover.Height, mImagePath]);
    SetImage(mImagePath);
  end
  else
    LogDebugFmt('EnsureScaledToCoverSize: no-op (scaled=%dx%d, cover=%dx%d, hasImage=%s)',
      [mScaledW, mScaledH, mCover.Width, mCover.Height, BoolToStr(mImagePath<>'', True)]);
end;

{------------------------------------------------------------------------------}
{ File setter: try sibling .png/.jpg, then PDF first-page render if needed     }
{------------------------------------------------------------------------------}
procedure TBook.SetFile(AValue: String);
begin
  if mFilePath = AValue then Exit;
  mFilePath := AValue;
  LogInfoFmt('SetFile: "%s"', [AValue]);

  // If a cover image was already chosen (manually or previously set), don't override it
  if Trim(mImagePath) <> '' then Exit;

  // Otherwise, try sibling images next to the book file
  SetImage(ChangeFileExt(AValue, '.png'));
  if mImagePath = '' then
    SetImage(ChangeFileExt(AValue, '.jpg'));
  // leave PDF covers to the background worker; keep generic cover if none found
end;

{------------------------------------------------------------------------------}
{ Lifecycle                                                                     }
{------------------------------------------------------------------------------}
constructor TBook.Create(Parent: TComponent);
begin
  inherited Create;
  mTitle      := '';
  mAuthors    := '';
  mISBN       := '';
  mFilePath   := '';
  mImagePath  := '';
  mIsSelected := False;
  mScaledW    := 0;
  mScaledH    := 0;

  mCover := TImage.Create(Parent);
  if Parent is TWinControl then
    mCover.Parent := TWinControl(Parent);

  // Desired default control size
  mCover.Width  := 130;
  mCover.Height := 250;

  // Interactions & visuals
  mCover.Stretch   := True;
  mCover.OnPaint   := @BookCoverPaint;
  mCover.OnMouseDown := @BookMouseDown;
  mCover.OnDblClick  := @BookDoubleClick;
  mCover.Cursor    := crHandPoint;
  // Enable drag-and-drop reordering (handled by PanelBackground handlers)
  mCover.DragMode  := dmAutomatic;
  // Back-reference for selection management among sibling controls
  mCover.Tag       := PtrInt(Self);

  // default image
  mCover.Picture.LoadFromLazarusResource('generic_cover');
end;

destructor TBook.Destroy;
begin
  // Do not free mCover here: it is owned by the panel (Owner passed on create)
  // and will be freed automatically with the form. Individual deletions
  // explicitly free the cover before freeing the book.
  mCover := nil;
  inherited Destroy;
end;

end.
