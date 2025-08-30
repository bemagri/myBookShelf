unit Book;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls, LCLIntf, LResources, Process,
  Math, IntfGraphics, FPImage, LazCanvas, FileUtil, LazJPG;


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
    function  TryGenerateCoverFromPDF(const PdfPath: String): String;

  public
    constructor Create(Parent: TComponent);
    destructor Destroy; override;

    procedure BookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BookDoubleClick(Sender: TObject);
    procedure BookCoverPaint(Sender: TObject);

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

var
  gPdfCoverEnabled: Boolean = True;

procedure SetPdfCoverGenerationEnabled(AEnabled: Boolean);
begin
  gPdfCoverEnabled := AEnabled;
end;

{------------------------------------------------------------------------------}
{ Helper: try to render first page of a PDF into a PNG using Poppler          }
{------------------------------------------------------------------------------}
function TBook.TryGenerateCoverFromPDF(const PdfPath: String): String;
var
  Proc: TProcess;
  OutBase, Converter: String;
  Pic: TPicture;
  Img: TLazIntfImage;
  Canvas: TLazCanvas;
  Png: TPortableNetworkGraphic;
  scale: Double;
  dstW, dstH, offX, offY, W, H: Integer;
begin
  Result := '';
  if not gPdfCoverEnabled then Exit;

  // look for pdftoppm in PATH (Poppler utilities)
  Converter := FindDefaultExecutablePath('pdftoppm');
  if Converter = '' then
    Exit; // tool not available, keep default behavior

  OutBase := ChangeFileExt(PdfPath, '');  // e.g., /path/book.pdf -> /path/book

  Proc := TProcess.Create(nil);
  try
    Proc.Executable := Converter;
    // pdftoppm -png -singlefile -f 1 -l 1 <pdf> <out_base>
    Proc.Parameters.Add('-png');
    Proc.Parameters.Add('-singlefile');
    Proc.Parameters.Add('-f'); Proc.Parameters.Add('1');
    Proc.Parameters.Add('-l'); Proc.Parameters.Add('1');
    Proc.Parameters.Add(PdfPath);
    Proc.Parameters.Add(OutBase);
    Proc.Options := [poWaitOnExit];
    Proc.ShowWindow := swoHIDE;
    Proc.Execute;
  finally
    Proc.Free;
  end;

  if FileExists(OutBase + '.png') then
  begin
    Result := OutBase + '.png';
    // Scale down to current cover size
    W := mCover.Width;
    H := mCover.Height;
    if (W > 0) and (H > 0) then
    begin
      Pic := TPicture.Create;
      Img := TLazIntfImage.Create(W, H);
      Canvas := TLazCanvas.Create(Img);
      Png := TPortableNetworkGraphic.Create;
      try
        Pic.LoadFromFile(Result);
        Img.FillPixels(colTransparent);
        if (Pic.Width > 0) and (Pic.Height > 0) then
        begin
          scale := Min(W / Pic.Width, H / Pic.Height);
          if scale > 1 then scale := 1;
          dstW := Round(Pic.Width * scale);
          dstH := Round(Pic.Height * scale);
          offX := (W - dstW) div 2;
          offY := (H - dstH) div 2;
          Canvas.StretchDraw(Rect(offX, offY, offX + dstW, offY + dstH), Pic.Graphic);
        end;
        Png.Assign(Img);
        Png.SaveToFile(Result);
      finally
        Png.Free;
        Canvas.Free;
        Img.Free;
        Pic.Free;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------}
{ Basic painting: selection outline                                            }
{------------------------------------------------------------------------------}
procedure TBook.BookCoverPaint(Sender: TObject);
begin
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
procedure TBook.BookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // You likely toggle selection elsewhere; keep this stub or wire to a callback
end;

procedure TBook.BookDoubleClick(Sender: TObject);
begin
  // Open file / details dialog etc. (your existing logic)
end;

{------------------------------------------------------------------------------}
{ Pre-scale loader: draw once into a bitmap matching the control size          }
{------------------------------------------------------------------------------}
procedure TBook.SetImage(AValue: String);
var
  Pic : TPicture;
  Img : TLazIntfImage;
  Canvas: TLazCanvas;
  Png : TPortableNetworkGraphic;
  dstW, dstH, offX, offY: Integer;
  scale: Double;
  W, H: Integer;
begin
  // Default state
  mImagePath := '';
  mScaledW := 0; mScaledH := 0;

  // Ensure we have a sensible target size (layout usually sets this)
  W := mCover.Width;  H := mCover.Height;
  if (W <= 0) or (H <= 0) then
  begin
    // fallback: honor the common 130x250 default
    W := 130; H := 250;
    mCover.Width := W; mCover.Height := H;
  end;

  if (AValue <> '') and FileExists(AValue) then
  begin
    Pic := TPicture.Create;
    Img := TLazIntfImage.Create(W, H);
    Canvas := TLazCanvas.Create(Img);
    Png := TPortableNetworkGraphic.Create;
    try
      try
        Pic.LoadFromFile(AValue);

        Img.FillPixels(colTransparent);
        
        if (Pic.Width > 0) and (Pic.Height > 0) then
        begin
          scale := Min(W / Pic.Width, H / Pic.Height);
          if scale > 1 then scale := 1; // avoid upscale
          dstW := Round(Pic.Width * scale);
          dstH := Round(Pic.Height * scale);
          offX := (W - dstW) div 2;
          offY := (H - dstH) div 2;
          Canvas.StretchDraw(Rect(offX, offY, offX + dstW, offY + dstH), Pic.Graphic);
        end;

        // No runtime scaling anymore; we drew at target size
        mCover.Stretch := False;
        mCover.Center  := False;
        mCover.AutoSize:= False;

        Png.Assign(Img);
        mCover.Picture.Assign(Png);
        mImagePath := AValue;
        mScaledW := W; mScaledH := H;
        Exit;
      except
        // fall through to generic on any failure
      end;
    finally
      Png.Free;
      Canvas.Free;
      Img.Free;
      Pic.Free;
    end;
  end;

  // Generic fallback
  mCover.Stretch := True;
  mCover.Picture.LoadFromLazarusResource('generic_cover');
end;

{------------------------------------------------------------------------------}
{ EnsureScaledToCoverSize: re-render if size changed since last pre-scale      }
{------------------------------------------------------------------------------}
procedure TBook.EnsureScaledToCoverSize;
begin
  if (mImagePath <> '') and ((mScaledW <> mCover.Width) or (mScaledH <> mCover.Height)) then
    SetImage(mImagePath);
end;

{------------------------------------------------------------------------------}
{ File setter: try sibling .png/.jpg, then PDF first-page render if needed     }
{------------------------------------------------------------------------------}
procedure TBook.SetFile(AValue: String);
var
  ext, gen: String;
begin
  if mFilePath = AValue then Exit;
  mFilePath := AValue;

  // first try sibling images
  SetImage(ChangeFileExt(AValue, '.png'));
  if mImagePath = '' then
    SetImage(ChangeFileExt(AValue, '.jpg'));

  // if still no image and it's a PDF, try to generate one
  ext := LowerCase(ExtractFileExt(AValue));
  if (mImagePath = '') and (ext = '.pdf') then
  begin
    gen := TryGenerateCoverFromPDF(AValue);
    if gen <> '' then
      SetImage(gen);
  end;
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

  // default image
  mCover.Picture.LoadFromLazarusResource('generic_cover');
end;

destructor TBook.Destroy;
begin
  FreeAndNil(mCover);
  inherited Destroy;
end;

end.
