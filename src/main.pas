unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls, LazFileUtils,
  Book, BookCollection, LCLIntf, LResources, StdCtrls, LCLType, IniFiles, unitSettingsDialog,
  unitCoverWorker, unitStorageXML, unitMetadata;


type

  { Tform1 }
  Tform1 = class(Tform)
    EditSearch: Tedit;
    ButtonSettings: Timage;
    ImageToolBar: Timage;
    ButtonAdd: Timage;
    Opendialog1: Topendialog;
    PanelBackground: Tscrollbox;
    procedure FormResize(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonAddMouseEnter(Sender: TObject);
    procedure ButtonAddMouseLeave(Sender: TObject);
    procedure ButtonSettingsClick(Sender: TObject);
    procedure ButtonSettingsMouseEnter(Sender: TObject);
    procedure ButtonSettingsMouseLeave(Sender: TObject);
    procedure Editsearchenter(Sender: Tobject);
    procedure Editsearchexit(Sender: Tobject);
    procedure Editsearchkeypress(Sender: Tobject; var Key: Char);
    procedure Formclose(Sender: Tobject; var Closeaction: Tcloseaction);
    procedure Formcreate(Sender: Tobject);
    procedure Formkeydown(Sender: Tobject; var Key: Word; Shift: Tshiftstate);
    procedure Panelbackgroundclick(Sender: Tobject);
    procedure Panelbackgrounddragdrop(Sender, Source: Tobject; X, Y: Integer);
    procedure Panelbackgrounddragover(Sender, Source: Tobject; X, Y: Integer;
      State: Tdragstate; var Accept: Boolean);
    procedure Panelbackgroundpaint(Sender: Tobject);
    procedure RearrangeBooksOnScreen();
    procedure Panelbackgroundresize(Sender: Tobject);
    function getBookIndexAtPoint(X,Y:Integer):Integer;
    procedure UnselectAll;
    function getCoverIndex(cover:TImage):Integer;
  private
    mAdd,mAddHover,mGear,mGearHover:TPicture;
    LayoutTimer: TTimer;
    procedure LayoutTimerTick(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: Tform1;
  BookList:TBookCollection;
  Xspace, Yspace:integer;
  dataPath:String;
  booksDir:String;
  background,toolbar:TPicture;
  bookWidth,bookHeight:Integer;
  optCopyBooks,optRenameBooks,optExtractMeta:Boolean;



implementation

{$R *.lfm}

{ Tform1 }

procedure TForm1.FormResize(Sender: TObject);
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

procedure Tform1.Panelbackgroundclick(Sender: Tobject);
begin
 ActiveControl:=PanelBackground;

 UnselectAll;
 PanelBackground.Invalidate;
End;

procedure Tform1.Panelbackgrounddragdrop(Sender, Source: Tobject; X, Y: Integer);
var src,dest:integer;
begin
 src:=getCoverIndex(TImage(Source));
 dest:=getBookIndexAtPoint(X,Y);
   if (src > -1) and (dest > -1) then BookList.SwapBooks(src,dest);
   UnselectAll;
   PanelBackground.Invalidate;
   //RearrangeBooksOnScreen();
End;

procedure Tform1.Panelbackgrounddragover(Sender, Source: Tobject; X,
  Y: Integer; State: Tdragstate; var Accept: Boolean);
begin
 Accept:=True;
End;

procedure Tform1.Panelbackgroundpaint(Sender: Tobject);
var w,h:Integer;
    x,y:Integer;
begin

  x:=0;
  y:=0;
  w:=background.Width;
  h:=background.Height;

  while x < PanelBackground.Canvas.Width do
  begin

    while y < PanelBackground.Canvas.Height do
    begin
      PanelBackground.Canvas.Draw(x,y,background.Graphic);
      y:=y+h;
    end;

    x:=x+w;
    y:=0;
  end;

End;

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
    for i := 0 to BookList.Count - 1 do
    begin
      cover := BookList.Books[i].Cover;
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
    need := (n * bookWidth) + ((n + 1) * gapPx);
    Result := need <= width;
  end;

begin
  PanelBackground.DisableAlign;
  try
    availW := PanelClientWidth;
    if availW <= 0 then Exit;

    minGap := Xspace;     // your existing horizontal spacing as the minimum
    curY   := Yspace;     // top margin
    CollectVisible;

    // Early exit: nothing to place
    if countVisible = 0 then Exit;

    // Ensure covers have correct size (in case they were recreated)
    for i := 0 to countVisible - 1 do
    begin
      visibleCovers[i].Width  := bookWidth;
      visibleCovers[i].Height := bookHeight;
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
        gap := (availW - (rowCount * bookWidth)) / (rowCount + 1);
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
        x := x + bookWidth + gap;
      end;

      // Next row Y
      curY := curY + bookHeight + Yspace + 26;
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


procedure Tform1.Panelbackgroundresize(Sender: Tobject);
begin
 RearrangeBooksOnScreen();

 EditSearch.Left:=Width-EditSearch.Width-20;
End;

function Tform1.Getbookindexatpoint(X, Y: Integer): Integer;
var i:Integer;
    cover:TImage;
begin
 for i:=0 to BookList.Count-1 do
 begin
   cover:=BookList.Books[i].Cover;
    if (X >= cover.Left) and (X <= cover.Left + cover.Width) and
      (Y >= cover.Top) and (Y <= cover.Top + cover.Height) then
    begin
     result :=i;
     exit;
   end;
 end;
 result:=-1;
end;

procedure Tform1.Unselectall;
var i:Integer;
begin
 for i:=0 to BookList.Count-1 do
 begin
  BookList.Books[i].isSelected:=False;
 end;
end;

function Tform1.Getcoverindex(Cover: Timage): Integer;
var i:integer;
begin
 for i:=0 to Booklist.count-1 do
 begin
  if Booklist.books[i].Cover = Cover then
  begin
    result:=i;
    exit;
  end;
 end;
 result:=-1;
end;


procedure Tform1.Formclose(Sender: Tobject; var Closeaction: Tcloseaction);
begin
SaveBooksXML(dataPath, BookList);
BookList.Destroy;
End;

procedure Tform1.ButtonAddClick(Sender: TObject);
var
  book:TBook;
  i:Integer;
  src,dest,fname,title,authors,ext:String;

  function CleanName(const s:String):String;
  const bad = '/\?*:<>|"';
  var c:Char;
  begin
    Result := Trim(s);
    for c in bad do
      Result := StringReplace(Result, c, '_', [rfReplaceAll]);
  end;
begin

if OpenDialog1.Execute then
begin
  for i:= 0 to Opendialog1.Files.Count-1 do
  begin
    src := OpenDialog1.Files.Strings[i];
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
      CopyFile(src, dest);
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

    BookList.AddBook(book);
    book.Cover.Width:=bookWidth;
    book.Cover.Height:=bookHeight;
    book.Cover.Parent:=PanelBackground;
    CoverWorkerEnqueueBookIfMissing(book);

  end;
  CoverWorkerStart;
  RearrangeBooksOnScreen();
end;
End;

procedure Tform1.ButtonAddMouseEnter(Sender: TObject);
begin
  ButtonAdd.Picture := mAddHover;
end;

procedure Tform1.ButtonAddMouseLeave(Sender: TObject);
begin
  ButtonAdd.Picture := mAdd;
end;

procedure Tform1.ButtonSettingsClick(Sender: TObject);
begin
SettingsDialog := TSettingsDialog.Create(Self);
  try
    SettingsDialog.ShowModal;
  finally
    SettingsDialog.Free;
  end;
end;

procedure Tform1.ButtonSettingsMouseEnter(Sender: TObject);
begin
  ButtonSettings.Picture := mGearHover;
end;

procedure Tform1.ButtonSettingsMouseLeave(Sender: TObject);
begin
  ButtonSettings.Picture := mGear;
end;

procedure Tform1.Editsearchenter(Sender: Tobject);
begin
EditSearch.Caption:='';
End;

procedure Tform1.Editsearchexit(Sender: Tobject);
begin
  EditSearch.Caption:='Search...';
End;

procedure Tform1.Editsearchkeypress(Sender: Tobject; var Key: Char);
begin
if Key = #13 then
begin
   //perform the search here
end;

End;

procedure Tform1.Formcreate(Sender: Tobject);
var
 i:integer;
 cfgDir, cfgPath, dataDir: String;
 ini: TIniFile;
 autoPdfCover: Boolean;
begin
 bookWidth:=130;
 bookHeight:=200;
 Xspace:=40;
 Yspace:=25;

 Form1.KeyPreview:=True;
 ActiveControl:=PanelBackground;


 background:=TPicture.Create;
 background.LoadFromLazarusResource('shelf');

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
  dataPath := IncludeTrailingPathDelimiter(dataDir) + 'books.xml';

 BookList:=TBookCollection.Create;

  // speed up startup: we skipped synchronous PDF generation during load
  SetPdfCoverGenerationEnabled(False);
  try
    if FileExistsUTF8(dataPath) then
      LoadBooksXML(dataPath, BookList, PanelBackground);
  finally
    SetPdfCoverGenerationEnabled(autoPdfCover); // re-enable per settings
  end;

 for i:=0 to BookList.Count-1 do
 begin
  with BookList.Books[i] do
  begin
    Cover.Width:=bookWidth;
    Cover.Height:=bookHeight;
    Cover.Parent:=PanelBackground;
    EnsureScaledToCoverSize;
  end;
 end;

 RearrangeBooksOnScreen();

 // Background: generate covers only where still generic
 CoverWorkerEnqueueMissingFromBookList(BookList);
 CoverWorkerStart;

End;

procedure Tform1.Formkeydown(Sender: Tobject; var Key: Word; Shift: Tshiftstate);
var i:Integer;
begin

 if Key = VK_DELETE then
 begin
   for i:= BookList.Count-1 downto 0 do
   begin
    if BookList.Books[i].isSelected = True then
    begin
       BookList.Books[i].Cover.Free;
       BookList.Remove(BookList.Books[i]);
    end;
   end;
   RearrangeBooksOnScreen();
 end;

End;


initialization
{$i mybookshelf.lrs}

end.

