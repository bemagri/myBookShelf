unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Book, BookCollection, LCLIntf, LResources, StdCtrls, LCLType;


type

  { Tform1 }

  Tform1 = class(Tform)
    EditSearch: Tedit;
    Image1: Timage;
    ImageToolBar: Timage;
    ButtonAdd: Timage;
    Opendialog1: Topendialog;
    PanelBackground: Tscrollbox;
    procedure Buttonaddclick(Sender: Tobject);
    procedure Editsearchenter(Sender: Tobject);
    procedure Editsearchexit(Sender: Tobject);
    procedure Editsearchkeypress(Sender: Tobject; var Key: Char);
    procedure Formclose(Sender: Tobject; var Closeaction: Tcloseaction);
    procedure Formcreate(Sender: Tobject);
    procedure Formkeydown(Sender: Tobject; var Key: Word; Shift: Tshiftstate);
    procedure Formpaint(Sender: Tobject);
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
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: Tform1;
  BookList:TBookCollection;
  Xspace, Yspace:integer;
  dataPath:String;
  background:TPicture;
  bookWidth,bookHeight:Integer;


implementation

{$R *.lfm}

{ Tform1 }

procedure Tform1.Formpaint(Sender: Tobject);
begin

End;

procedure Tform1.Panelbackgroundclick(Sender: Tobject);
begin
 ActiveControl:=PanelBackground;

 UnselectAll;
 PanelBackground.Repaint;
End;

procedure Tform1.Panelbackgrounddragdrop(Sender, Source: Tobject; X, Y: Integer);
var src,dest:integer;
begin
 src:=getCoverIndex(TImage(Source));
 dest:=getBookIndexAtPoint(X,Y);
   if (src > -1) and (dest > -1) then BookList.SwapBooks(src,dest);
   UnselectAll;
   RearrangeBooksOnScreen();
End;

procedure Tform1.Panelbackgrounddragover(Sender, Source: Tobject; X,
  Y: Integer; State: Tdragstate; var Accept: Boolean);
begin
 Accept:=True;
End;

procedure Tform1.Panelbackgroundpaint(Sender: Tobject);
begin
  PanelBackground.Canvas.StretchDraw(PanelBackground.Canvas.ClipRect, background.Graphic);
End;

procedure Tform1.Rearrangebooksonscreen;
var i,x,y:Integer;
begin

 x:=0;
 y:=0;

 for i:= 0 to BookList.Count-1 do
 begin
    if X+Xspace > PanelBackground.Width-bookWidth then
    begin
      X:=0;
      Y:=Y+Yspace+bookHeight;
    end;
    with BookList.Books[i] do
    begin
      Cover.Left:=X+Xspace;
      Cover.Top:=Y+Yspace;
      X:=X+Xspace+bookWidth;
    end;
  end;
 PanelBackground.Repaint;

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
   if (cover.Left > X) and (cover.Left - bookWidth < X) and (cover.Top <= Y) and (cover.Top + bookHeight > Y) then
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
BookList.StoreData(dataPath);
BookList.Destroy;
End;

procedure Tform1.Buttonaddclick(Sender: Tobject);
var
  book:TBook;
  i:Integer;
begin

if OpenDialog1.Execute then
begin
  for i:= 0 to Opendialog1.Files.Count-1 do
  begin
  book:=TBook.Create(PanelBackground);
  book.FilePath:= OpenDialog1.Files.Strings[i];
  BookList.AddBook(book);
  book.Cover.Width:=bookWidth;
  book.Cover.Height:=bookHeight;
  book.Cover.Parent:=PanelBackground;
  end;
  RearrangeBooksOnScreen();
end;
End;

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
var i, X, Y:integer;
begin
 bookWidth:=150;
 bookHeight:=200;
 Xspace:=40;
 Yspace:=30;
 X:=0;
 Y:=0;

 Form1.KeyPreview:=True;
 ActiveControl:=PanelBackground;


 background:=TPicture.Create;
 background.LoadFromLazarusResource('back');

 {$IFDEF MSWINDOWS}
 DataPath:= GetEnvironmentVariableUTF8('appdata') + '\mybookshelf'; //fix the data store dir

if not DirectoryExistsUTF8(dataPath) then
    CreateDirUTF8(dataPath);

dataPath:= dataPath + '\data.dat';
 {$ENDIF}

 {$IFDEF UNIX}
 DataPath:= GetEnvironmentVariableUTF8('HOME') + '/.mybookshelf/';

 if not DirectoryExistsUTF8(DataPath) then
    CreateDirUTF8(dataPath);

 dataPath:= dataPath + 'data.dat';
 {$ENDIF}

 BookList:=TBookCollection.Create;

 if FileExistsUTF8(dataPath) then
    BookList.LoadData(dataPath, PanelBackground);


 for i:=0 to BookList.Count-1 do
 begin
  with BookList.Books[i] do
  begin
    Cover.Width:=bookWidth;
    Cover.Height:=bookHeight;
    Cover.Parent:=PanelBackground;
  end;
 end;

 RearrangeBooksOnScreen();

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

