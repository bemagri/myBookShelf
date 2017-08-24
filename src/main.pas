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
    procedure Panelbackgroundpaint(Sender: Tobject);
    procedure RearrangeBooksOnScreen();
    procedure Panelbackgroundresize(Sender: Tobject);
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
 //myBook.DrawCover(PanelBackground.Canvas);
End;

procedure Tform1.Panelbackgroundclick(Sender: Tobject);
var i:integer;
begin
 ActiveControl:=PanelBackground;

 for i:=0 to BookList.Count-1 do
 begin
  BookList.Books[i].isSelected:=False;
 end;
 PanelBackground.Repaint;

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


procedure Tform1.Formclose(Sender: Tobject; var Closeaction: Tcloseaction);
begin
BookList.StoreData(dataPath);
BookList.Destroy;
End;

procedure Tform1.Buttonaddclick(Sender: Tobject);
var
  book:TBook;
begin

if OpenDialog1.Execute then
begin
  book:=TBook.Create(PanelBackground);
  book.FilePath:= OpenDialog1.Filename;
  BookList.AddBook(book);
  book.Cover.Width:=bookWidth;
  book.Cover.Height:=bookHeight;
  book.Cover.Parent:=PanelBackground;
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

 //works only for linux and MacOS
 DataPath:= GetEnvironmentVariable('HOME') + '/.mybookshelf/data.dat'; //fix the data store dir

if not DirectoryExists(GetEnvironmentVariable('HOME') + '/.mybookshelf/') then
    CreateDir(GetEnvironmentVariable('HOME') + '/.mybookshelf/');

 BookList:=TBookCollection.Create;

 if FileExists(dataPath) then
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

