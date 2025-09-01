unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls, LazFileUtils,
  Book, BookCollection, LCLIntf, LResources, StdCtrls, LCLType, IniFiles, unitSettingsDialog,
  unitCoverWorker, unitStorageXML, unitMetadata, LazUTF8;


type

  { TForm1 }
  TForm1 = class(TForm)
    EditSearch: Tedit;
    ComboSort: TComboBox;
    ButtonSettings: Timage;
    ImageToolBar: Timage;
    ButtonAdd: Timage;
    Opendialog1: Topendialog;
    PanelBackground: Tscrollbox;
    procedure FormResize({%H-}Sender: TObject);
    procedure ButtonAddClick({%H-}Sender: TObject);
    procedure ButtonAddMouseEnter({%H-}Sender: TObject);
    procedure ButtonAddMouseLeave({%H-}Sender: TObject);
    procedure ButtonSettingsClick({%H-}Sender: TObject);
    procedure ButtonSettingsMouseEnter({%H-}Sender: TObject);
    procedure ButtonSettingsMouseLeave({%H-}Sender: TObject);
    procedure EditSearchEnter({%H-}Sender: TObject);
    procedure EditSearchExit({%H-}Sender: TObject);
    procedure EditSearchChange({%H-}Sender: TObject);
    procedure EditSearchKeyPress({%H-}Sender: TObject; var Key: Char);
    procedure ComboSortChange({%H-}Sender: TObject);
    procedure FormClose({%H-}Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormKeyDown({%H-}Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure PanelBackgroundClick({%H-}Sender: TObject);
    procedure PanelBackgroundDragDrop({%H-}Sender, Source: TObject; X, Y: Integer);
    procedure PanelBackgroundDragOver({%H-}Sender, {%H-}Source: TObject; {%H-}X, {%H-}Y: Integer;
      {%H-}State: TDragState; var Accept: Boolean);
    procedure PanelBackgroundPaint({%H-}Sender: TObject);
    procedure RearrangeBooksOnScreen();
    procedure PanelBackgroundResize({%H-}Sender: TObject);
    function GetBookIndexAtPoint(X,Y:Integer):Integer;
    procedure UnselectAll;
    function GetCoverIndex(cover:TImage):Integer;
  private
    mAdd,mAddHover,mGear,mGearHover:TPicture;
    LayoutTimer: TTimer;
    procedure LayoutTimerTick(Sender: TObject);
    procedure ApplyFilterAndLayout;
    function AppConfigPath: String;
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  bookList: TBookCollection;
  xSpace, ySpace: integer;
  dataXmlPath: String;
  booksDir: String;
  backgroundTile, toolbar: TPicture;
  coverWidth, coverHeight: Integer;
  optCopyBooks, optRenameBooks, optExtractMeta: Boolean;
  isClosing: Boolean = False;



implementation

{$R *.lfm}

{ Tform1 }

procedure TForm1.FormResize({%H-}Sender: TObject);
begin
  // debounce: restart the timer, don’t layout on every pixel move
  LayoutTimer.Enabled := False;
  LayoutTimer.Enabled := True;
end;

procedure TForm1.LayoutTimerTick(Sender: TObject);
begin
  LayoutTimer.Enabled := False;   // one-shot
  RearrangeBooksOnScreen;
end;

function TForm1.AppConfigPath: String;
begin
  Result := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False)) + 'config.ini';
end;

procedure TForm1.PanelBackgroundClick({%H-}Sender: TObject);
begin
 ActiveControl:=PanelBackground;

 UnselectAll;
 PanelBackground.Invalidate;
end;

procedure TForm1.PanelBackgroundDragDrop({%H-}Sender, Source: TObject; X, Y: Integer);
var src,dest:integer;
begin
 src:=GetCoverIndex(TImage(Source));
 dest:=GetBookIndexAtPoint(X,Y);
   if (src > -1) and (dest > -1) then bookList.SwapBooks(src,dest);
   UnselectAll;
   // After changing book order, recalculate layout so covers move immediately
   RearrangeBooksOnScreen();
end;

procedure TForm1.PanelBackgroundDragOver({%H-}Sender, {%H-}Source: TObject; {%H-}X,
  {%H-}Y: Integer; {%H-}State: TDragState; var Accept: Boolean);
begin
 Accept:=True;
end;

procedure TForm1.PanelBackgroundPaint({%H-}Sender: TObject);
var w,h:Integer;
    x,y:Integer;
begin
  // Safety: if no tile or invalid size, skip custom painting
  if (backgroundTile = nil) or (backgroundTile.Width <= 0) or (backgroundTile.Height <= 0) then
    Exit;

  x:=0;
  y:=0;
  w:=backgroundTile.Width;
  h:=backgroundTile.Height;
  while x < PanelBackground.Canvas.Width do
  begin
    while y < PanelBackground.Canvas.Height do
    begin
      PanelBackground.Canvas.Draw(x,y,backgroundTile.Graphic);
      y:=y+h;
    end;
    x:=x+w;
    y:=0;
  end;
end;

procedure TForm1.RearrangeBooksOnScreen;
var
  visibleCovers: array of TImage;
  i, j, k, countVisible: Integer;
  availW, minGap, rowStart, rowCount: Integer;
  curY: Integer;
  x: Double;
  gap: Double;
  cover: TImage;

  function PanelClientWidth: Integer;
  begin
    // Use client width (exclude borders/scrollbar)
    Result := PanelBackground.ClientWidth;
    if Result <= 0 then Result := PanelBackground.Width;
  end;

  procedure CollectVisible;
  var i : Integer;
  begin
    SetLength(visibleCovers, 0);
    for i := 0 to bookList.Count - 1 do
    begin
      cover := bookList.Books[i].Cover;
      if Assigned(cover) and cover.Visible then
      begin
        SetLength(visibleCovers, Length(visibleCovers) + 1);
        visibleCovers[High(visibleCovers)] := cover;
      end;
    end;
    countVisible := Length(visibleCovers);
  end;

  // Can we fit N items with at least minGap spacing including left+right margins?
  function FitsWithMinGaps(n: Integer; width: Integer; gapPx: Integer): Boolean;
  var need: Integer;
  begin
    // total = n*bookWidth + (n+1)*gap  (edge gaps included)
    need := (n * coverWidth) + ((n + 1) * gapPx);
    Result := need <= width;
  end;

begin
  if (bookList = nil) or isClosing then Exit;
  PanelBackground.DisableAlign;
  try
    availW := PanelClientWidth;
    if availW <= 0 then Exit;

    minGap := xSpace;     // your existing horizontal spacing as the minimum
    curY   := ySpace;     // top margin
    CollectVisible;

    // Early exit: nothing to place
    if countVisible = 0 then Exit;

    // Ensure covers have correct size (in case they were recreated)
    for i := 0 to countVisible - 1 do
    begin
      visibleCovers[i].Width  := coverWidth;
      visibleCovers[i].Height := coverHeight;
      visibleCovers[i].Parent := PanelBackground;
    end;

    rowStart := 0;
    while rowStart < countVisible do
    begin
      // Determine how many items fit in this row with at least minGap gutters.
      rowCount := 1;
      while (rowStart + rowCount < countVisible)
        and FitsWithMinGaps(rowCount + 1, availW, minGap) do
        Inc(rowCount);

      // Compute the gap for this row:
      // - For full rows, distribute leftover width evenly across (rowCount+1) gaps.
      // - For the last row (rowStart+rowCount = countVisible), keep it left-aligned (minGap).
      if (rowStart + rowCount) < countVisible then
      begin
        // Full row → justified
        gap := (availW - (rowCount * coverWidth)) / (rowCount + 1);
        if gap < minGap then gap := minGap; // safety
      end
      else
      begin
        // Last row → left align
        gap := minGap; // safety
      end;

      // Place row items: start at left edge gap, then [cover + gap] repeated.
      x := gap;
      for j := 0 to rowCount - 1 do
      begin
        k := rowStart + j;
        cover := visibleCovers[k];
        cover.Left := Round(x);
        cover.Top  := curY;
        x := x + coverWidth + gap;
      end;

      // Next row Y
      curY := curY + coverHeight + ySpace + 26;
      Inc(rowStart, rowCount);
    end;

    // Optional: ensure panel is tall enough; comment out if not needed.
    // PanelBackground.AutoSize := False;
    // PanelBackground.Height := curY + Yspace;
  finally
    PanelBackground.EnableAlign;
    
    PanelBackground.Invalidate;
  end;
end;


procedure TForm1.PanelBackgroundResize({%H-}Sender: TObject);
begin
 if isClosing then Exit;
 RearrangeBooksOnScreen();

 EditSearch.Left := Width - EditSearch.Width - 20;
 if Assigned(ComboSort) then
   ComboSort.Left := EditSearch.Left - ComboSort.Width - 12;
End;

function TForm1.GetBookIndexAtPoint(X, Y: Integer): Integer;
var i:Integer;
    cover:TImage;
begin
 for i:=0 to bookList.Count-1 do
 begin
  cover:=bookList.Books[i].Cover;
    if Assigned(cover) and cover.Visible and
       (X >= cover.Left) and (X <= cover.Left + cover.Width) and
       (Y >= cover.Top) and (Y <= cover.Top + cover.Height) then
    begin
     result :=i;
     exit;
   end;
 end;
 result:=-1;
end;

procedure TForm1.UnselectAll;
var i:Integer;
begin
 for i:=0 to bookList.Count-1 do
 begin
  bookList.Books[i].isSelected:=False;
 end;
end;

function TForm1.GetCoverIndex(Cover: Timage): Integer;
var i:integer;
begin
 for i:=0 to bookList.count-1 do
 begin
  if Assigned(bookList.books[i].Cover) and (bookList.books[i].Cover = Cover) then
  begin
    result:=i;
    exit;
  end;
 end;
 result:=-1;
end;


procedure TForm1.FormClose({%H-}Sender: TObject; var CloseAction: TCloseAction);
begin
  isClosing := True;
  if Assigned(LayoutTimer) then LayoutTimer.Enabled := False;
  // Ensure background worker thread is stopped before destroying books/controls
  CoverWorkerStop;
  try
    if Assigned(bookList) then
      SaveBooksXML(dataXmlPath, bookList);
  except
    // ignore save errors on shutdown
  end;
  // Free images created at runtime
  FreeAndNil(mAdd);
  FreeAndNil(mAddHover);
  FreeAndNil(mGear);
  FreeAndNil(mGearHover);
  FreeAndNil(backgroundTile);
  FreeAndNil(bookList);
  CloseAction := caFree;
end;

procedure TForm1.ButtonAddClick({%H-}Sender: TObject);
var
  book : TBook;
  i    : Integer;
  src  : String;
  dest : String;
  fname,title,authors,ext : String;
  files: TStringList;

  function CleanName(const s:String):String;
  const bad = '/\?*:<>|"';
  var c:Char;
  begin
    Result := Trim(s);
    for c in bad do
      Result := StringReplace(Result, c, '_', [rfReplaceAll]);
  end;

  procedure ProcessFile(const AFile: String);
  begin
    src := AFile;
    dest := src;
    title := '';
    authors := '';
    if optExtractMeta then
      ExtractBookMetadata(src, title, authors);

    if optCopyBooks then
    begin
      ForceDirectories(booksDir);
      fname := ExtractFileName(src);
      if optRenameBooks and (title <> '') then
      begin
        ext := ExtractFileExt(src);
        fname := CleanName(title);
        if authors <> '' then
          fname := fname + ' - ' + CleanName(authors);
        fname := fname + ext;
      end;
      dest := IncludeTrailingPathDelimiter(booksDir) + fname;
      // Skip copy if source already resides in booksDir and avoid exceptions on failure
      if CompareFilenames(src, dest) <> 0 then
      begin
        try
          CopyFile(src, dest);
        except
          // fall back to original path if copy fails for any reason
          dest := src;
        end;
      end;
    end;

    book:=TBook.Create(PanelBackground);
    book.FilePath:= dest;
    if optExtractMeta then
    begin
      if title <> '' then book.Title := title
      else book.Title := ChangeFileExt(ExtractFileName(dest), '');
      if authors <> '' then book.Authors := authors;
    end
    else
      book.Title := ChangeFileExt(ExtractFileName(dest), '');

    bookList.AddBook(book);
    book.Cover.Width:=coverWidth;
    book.Cover.Height:=coverHeight;
    book.Cover.Parent:=PanelBackground;
    // Ensure the pre-scaled image matches the final cover size
    book.EnsureScaledToCoverSize;
    CoverWorkerEnqueueBookIfMissing(book);
  end;

begin
  if OpenDialog1.Execute then
  begin
    files := TStringList.Create;
    try
      if OpenDialog1.Files.Count > 0 then
        files.Assign(OpenDialog1.Files)
      else if OpenDialog1.FileName <> '' then
        files.Add(OpenDialog1.FileName);

      for i := 0 to files.Count - 1 do
        ProcessFile(files[i]);

      CoverWorkerStart;
      RearrangeBooksOnScreen();
    finally
      files.Free;
    end;
  end;
End;

procedure TForm1.ButtonAddMouseEnter({%H-}Sender: TObject);
begin
  ButtonAdd.Picture := mAddHover;
end;

procedure TForm1.ButtonAddMouseLeave({%H-}Sender: TObject);
begin
  ButtonAdd.Picture := mAdd;
end;

procedure TForm1.ButtonSettingsClick({%H-}Sender: TObject);
begin
SettingsDialog := TSettingsDialog.Create(Self);
  try
    SettingsDialog.ShowModal;
  finally
    SettingsDialog.Free;
  end;
end;

procedure TForm1.ButtonSettingsMouseEnter({%H-}Sender: TObject);
begin
  ButtonSettings.Picture := mGearHover;
end;

procedure TForm1.ButtonSettingsMouseLeave({%H-}Sender: TObject);
begin
  ButtonSettings.Picture := mGear;
end;

procedure TForm1.EditSearchEnter({%H-}Sender: TObject);
begin
  // Use Text for TEdit, not Caption
  EditSearch.Text := '';
end;

procedure TForm1.EditSearchExit({%H-}Sender: TObject);
begin
  // Restore placeholder text when leaving the field
  EditSearch.Text := 'Search...';
end;

procedure TForm1.EditSearchKeyPress({%H-}Sender: TObject; var Key: Char);
begin
if Key = #13 then
begin
   //perform the search here
end;

end;

procedure TForm1.FormCreate({%H-}Sender: TObject);
var
 i:integer;
 cfgDir, cfgPath, dataDir: String;
 ini: TIniFile;
 autoPdfCover: Boolean;
begin
 coverWidth:=130;
 coverHeight:=200;
 xSpace:=40;
 ySpace:=25;

 Form1.KeyPreview:=True;
 ActiveControl:=PanelBackground;


 backgroundTile:=TPicture.Create;
 backgroundTile.LoadFromLazarusResource('shelf');

 PanelBackground.DoubleBuffered := True; // reduce flicker

 Self.OnResize := @FormResize;

 LayoutTimer := TTimer.Create(Self);
 LayoutTimer.Enabled  := False;
 LayoutTimer.Interval := 60;            // ~60ms debounce feels snappy
 LayoutTimer.OnTimer  := @LayoutTimerTick;
 
 mAdd:=TPicture.Create;
 mAddHover:=Tpicture.Create;
 mGear:=Tpicture.Create;
 mGearHover:=Tpicture.Create;
 mAdd.LoadFromLazarusResource('add');
 mAddHover.LoadFromLazarusResource('add_hover');
 mGear.LoadFromLazarusResource('gear');
 mGearHover.LoadFromLazarusResource('gear_hover');
 ButtonAdd.Picture:=mAdd;
 ButtonSettings.Picture:=mGear;
  // Load toolbar image from Lazarus resources instead of large LFM-embedded data
  try
    ImageToolBar.Picture.LoadFromLazarusResource('toolbar');
  except
    // ignore if resource missing; fallback to LFM-embedded picture
  end;

 // Load config.ini if present to resolve paths and options
  cfgDir := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False));
  if not DirectoryExistsUTF8(cfgDir) then CreateDirUTF8(cfgDir);

  cfgPath := cfgDir + 'config.ini';
  ini := TIniFile.Create(cfgPath);
  try
    dataDir        := ini.ReadString('general', 'data_dir', cfgDir);
    booksDir       := ini.ReadString('general', 'books_dir', cfgDir);
    optCopyBooks   := ini.ReadBool('general', 'copy_books', True);
    optRenameBooks := ini.ReadBool('general', 'rename_books', True);
    optExtractMeta := ini.ReadBool('general', 'extract_metadata', True);
    autoPdfCover   := ini.ReadBool('general','auto_pdf_cover', True);
  finally
    ini.Free;
  end;

  if not DirectoryExistsUTF8(dataDir) then CreateDirUTF8(dataDir);
  if not DirectoryExistsUTF8(booksDir) then CreateDirUTF8(booksDir);
  dataXmlPath := IncludeTrailingPathDelimiter(dataDir) + 'books.xml';

 bookList:=TBookCollection.Create;

  // speed up startup: we skipped synchronous PDF generation during load
  SetPdfCoverGenerationEnabled(False);
  try
    if FileExistsUTF8(dataXmlPath) then
      LoadBooksXML(dataXmlPath, bookList, PanelBackground);
  finally
    SetPdfCoverGenerationEnabled(autoPdfCover); // re-enable per settings
  end;

 for i:=0 to bookList.Count-1 do
begin
  with bookList.Books[i] do
  begin
    Cover.Width:=coverWidth;
    Cover.Height:=coverHeight;
    Cover.Parent:=PanelBackground;
    EnsureScaledToCoverSize;
  end;
end;

 RearrangeBooksOnScreen();

 // Restore sort selection and apply
 try
   ini := TIniFile.Create(AppConfigPath);
   try
     ComboSort.ItemIndex := ini.ReadInteger('ui','sort_by', 0);
   finally
     ini.Free;
   end;
   ComboSortChange(nil);
 except
   // ignore
 end;

 // Background: generate covers only where still generic
 CoverWorkerEnqueueMissingFromBookList(bookList);
 CoverWorkerStart;

end;

procedure TForm1.FormKeyDown({%H-}Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
var i:Integer;
    b:TBook;
begin

  if Key = VK_DELETE then
  begin
    for i:= bookList.Count-1 downto 0 do
    begin
      if bookList.Books[i].isSelected = True then
      begin
         // Remove the cover control first (owned by PanelBackground), then free book
         b := bookList.Books[i];
         // Ensure the background worker won't touch this book anymore
         CoverWorkerRemoveBook(b);
         if Assigned(b.Cover) then b.Cover.Free;
         bookList.Remove(b);
         b.Free;
      end;
    end;
    RearrangeBooksOnScreen();
  end;
end;


procedure TForm1.EditSearchChange({%H-}Sender: TObject);
begin
  ApplyFilterAndLayout;
end;

procedure TForm1.ComboSortChange({%H-}Sender: TObject);
begin
  case ComboSort.ItemIndex of
    0: ; // Recently Added (keep current order)
    1: bookList.SortByTitle;
    2: bookList.SortByAuthor;
  end;
  // Persist selection
  try
    with TIniFile.Create(AppConfigPath) do
    try
      WriteInteger('ui', 'sort_by', ComboSort.ItemIndex);
    finally
      Free;
    end;
  except
  end;
  ApplyFilterAndLayout;
end;

procedure TForm1.ApplyFilterAndLayout;
var
  q, lt, la: String;
  i: Integer;
  b: TBook;
  showIt: Boolean;
begin
  if (bookList = nil) then Exit;
  q := UTF8LowerCase(Trim(EditSearch.Text));
  if (q = '') or (q = 'search...') then
  begin
    // show all
    for i := 0 to bookList.Count - 1 do
      if Assigned(bookList.Books[i].Cover) then
        bookList.Books[i].Cover.Visible := True;
  end
  else
  begin
    for i := 0 to bookList.Count - 1 do
    begin
      b := bookList.Books[i];
      lt := UTF8LowerCase(b.Title);
      la := UTF8LowerCase(b.Authors);
      showIt := (Pos(q, lt) > 0) or (Pos(q, la) > 0);
      if Assigned(b.Cover) then b.Cover.Visible := showIt;
    end;
  end;
  RearrangeBooksOnScreen();
end;

initialization
{$i mybookshelf.lrs}

end.
