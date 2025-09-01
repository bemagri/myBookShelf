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
procedure TBook.BookMouseDown({%H-}Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
var
  i: Integer;
  ctrl: TControl;
  otherBook: TBook;
begin
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
    SrcImg := TLazIntfImage.Create(0, 0);
    Img := TLazIntfImage.Create(W, H);
    Canvas := TLazCanvas.Create(Img);
    Png := TPortableNetworkGraphic.Create;
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
          Canvas.StretchDraw(offX, offY, dstW, dstH, SrcImg);
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
      SrcImg.Free;
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
begin
  if mFilePath = AValue then Exit;
  mFilePath := AValue;

  // first try sibling images
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
