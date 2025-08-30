unit unitCoverWorker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LCLIntf, Graphics, Math, LazJpeg,
  IntfGraphics, FPImage, GraphType, LazCanvas,
  Book, BookCollection, FileUtil;

{ Call this once after loading your data: it scans the list and enqueues
  only the PDFs that still use the generic cover (i.e. ImagePath=''). }
procedure CoverWorkerEnqueueMissingFromBookList(AList: TBookCollection);

{ Call this whenever you add/edit a single book and want it queued if needed. }
procedure CoverWorkerEnqueueBookIfMissing(B: TBook);

{ Starts the background worker (idempotent). It will exit by itself when the
  queue is empty. Call again later to restart if you enqueue more books. }
procedure CoverWorkerStart;

{ Stops the background worker and clears any pending books }
procedure CoverWorkerStop;

implementation

type
  { Simple worker that drains a TThreadList queue of TBook references }
  TCoverWorker = class(TThread)
  private
    FApplyBook: TBook;
    FApplyImg : String;
    procedure DoApplyCover; // runs in main thread
  protected
    procedure Execute; override;
  end;

var
  GPdfQueue: TThreadList; // holds TBook references
  GWorker  : TCoverWorker;

{--- helpers ------------------------------------------------------------------}

function IsPdf(const Path: String): Boolean;
begin
  Result := LowerCase(ExtractFileExt(Path)) = '.pdf';
end;

function HasGenericCover(B: TBook): Boolean;
begin
  // Our Book.SetImage leaves ImagePath='' when using the generic resource
  Result := (Trim(B.ImagePath) = '');
end;

function GeneratePdfCover(const PdfPath: String; W, H: Integer): String;
var
  OutBase, Converter: String;
  Proc: TProcess;
  Pic: TPicture;
  Img: TLazIntfImage;
  Canvas: TLazCanvas;
  Png: TPortableNetworkGraphic;
  scale: Double;
  dstW, dstH, offX, offY: Integer;
begin
  Result := '';

  // If a sibling PNG or JPG already exists, just return it
  if FileExists(ChangeFileExt(PdfPath, '.png')) then
    Exit(ChangeFileExt(PdfPath, '.png'));
  if FileExists(ChangeFileExt(PdfPath, '.jpg')) then
    Exit(ChangeFileExt(PdfPath, '.jpg'));

  // look for pdftoppm in PATH (Poppler utilities)
  Converter := FindDefaultExecutablePath('pdftoppm');
  if Converter = '' then Exit; // poppler not installed

  OutBase := ChangeFileExt(PdfPath, ''); // /path/book.pdf -> /path/book

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
    // Scale down to requested cover size
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
          Canvas.StretchDraw(offX, offY, dstW, dstH, Pic.Graphic);
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

procedure EnsureQueue;
begin
  if GPdfQueue = nil then
    GPdfQueue := TThreadList.Create;
end;

{--- public API ----------------------------------------------------------------}

procedure CoverWorkerEnqueueMissingFromBookList(AList: TBookCollection);
var
  i: Integer;
  l: TList;
begin
  if AList = nil then Exit;
  EnsureQueue;
  l := GPdfQueue.LockList;
  try
    for i := 0 to AList.Count - 1 do
      if IsPdf(AList.Books[i].FilePath) and HasGenericCover(AList.Books[i]) then
        l.Add(AList.Books[i]);
  finally
    GPdfQueue.UnlockList;
  end;
end;

procedure CoverWorkerEnqueueBookIfMissing(B: TBook);
var
  l: TList;
begin
  if (B = nil) then Exit;
  if not (IsPdf(B.FilePath) and HasGenericCover(B)) then Exit;
  EnsureQueue;
  l := GPdfQueue.LockList;
  try
    if l.IndexOf(B) < 0 then
      l.Add(B);
  finally
    GPdfQueue.UnlockList;
  end;
end;

procedure CoverWorkerStart;
begin
  EnsureQueue;
  if (GWorker = nil) or (GWorker.Finished) then
  begin
    GWorker := TCoverWorker.Create(True);
    GWorker.FreeOnTerminate := True;
    GWorker.Start;
  end;
end;

{ Stops the worker and clears any queued books }
procedure CoverWorkerStop;
var
  l: TList;
begin
  if GWorker <> nil then
  begin
    GWorker.Terminate;
    GWorker.WaitFor;
    GWorker := nil;
  end;
  if GPdfQueue <> nil then
  begin
    l := GPdfQueue.LockList;
    try
      l.Clear;
    finally
      GPdfQueue.UnlockList;
    end;
  end;
end;

{--- worker --------------------------------------------------------------------}

procedure TCoverWorker.DoApplyCover;
begin
  // This runs in the main/UI thread
  try
    if Assigned(FApplyBook) and (FApplyImg <> '') and FileExists(FApplyImg) then
    begin
      FApplyBook.ImagePath := FApplyImg;  // triggers SetImage + pre-scale
      FApplyBook.EnsureScaledToCoverSize; // in case layout changed
    end;
  except
    // ignore UI exceptions, keep worker going
  end;
end;

procedure TCoverWorker.Execute;
var
  l: TList;
  B: TBook;
  Img: String;
begin
  // drain the queue
  while not Terminated do
  begin
    // Pop one item
    B := nil;
    l := GPdfQueue.LockList;
    try
      if (l <> nil) and (l.Count > 0) then
      begin
        B := TBook(l[0]);
        l.Delete(0);
      end;
    finally
      GPdfQueue.UnlockList;
    end;

    if B = nil then
      Break; // queue empty → exit thread

    // Skip if it no longer needs a cover
    if not (IsPdf(B.FilePath) and HasGenericCover(B)) then
    begin
      Sleep(5);
      Continue;
    end;

    // Generate cover (background thread)
    Img := GeneratePdfCover(B.FilePath, B.Cover.Width, B.Cover.Height);

    if (Img <> '') and FileExists(Img) then
    begin
      // Pass data to main thread via fields + Synchronize
      FApplyBook := B;
      FApplyImg  := Img;
      Synchronize(@DoApplyCover);
      FApplyBook := nil;
      FApplyImg  := '';
    end;

    Sleep(5); // be nice to the UI event loop
  end;
end;

end.
