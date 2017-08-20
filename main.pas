unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Book, BookCollection, LCLIntf, LResources;


type

  { Tform1 }

  Tform1 = class(Tform)
    ImageToolBar: Timage;
    ButtonAdd: Timage;
    Opendialog1: Topendialog;
    PanelBackground: Tpanel;
    procedure Buttonaddclick(Sender: Tobject);
    procedure Formclick(Sender: Tobject);
    procedure Formclose(Sender: Tobject; var Closeaction: Tcloseaction);
    procedure Formcreate(Sender: Tobject);
    procedure Formpaint(Sender: Tobject);
  private
    { private declarations }
  public
    procedure Bookpanelpaint(Sender: Tobject);
    { public declarations }
  end;

var
  Form1: Tform1;
  BookList:TBookCollection;
  X,Y, Xdelta, Ydelta:integer;
  dataPath:String;


implementation

{$R *.lfm}

{ Tform1 }

procedure Tform1.Formpaint(Sender: Tobject);
begin
 //myBook.DrawCover(PanelBackground.Canvas);
End;

procedure Tform1.Bookpanelpaint(Sender: Tobject);
var book:TBook;
begin


End;

procedure Tform1.Formclick(Sender: Tobject);
begin
  //OpenDocument(myBook.FilePath);
End;

procedure Tform1.Formclose(Sender: Tobject; var Closeaction: Tcloseaction);
begin
BookList.StoreData(dataPath);
BookList.Destroy;

End;

procedure Tform1.Buttonaddclick(Sender: Tobject);
var
  book:TBook;
  panel:TPanel;
begin

if OpenDialog1.Execute then
begin
  book:=TBook.Create(PanelBackground);
  book.FilePath:= OpenDialog1.Filename;
  BookList.AddBook(book);

  book.Cover.Left:=X+Xdelta;
  book.Cover.Top:=Y+Ydelta;
  book.Cover.Width:=150;
  book.Cover.Height:=200;
  book.Cover.Parent:=PanelBackground;
  X:=X+Xdelta+150;

end;
End;

procedure Tform1.Formcreate(Sender: Tobject);
var i:integer;
    tempBook:TBook;
begin
 Xdelta:=10;
 Ydelta:=10;
 X:=0;
 Y:=0;

 DataPath:= GetEnvironmentVariable('HOME') + '/.mybookshelf/data.dat'; //fix the data store dir

if not DirectoryExists(GetEnvironmentVariable('HOME') + '/.mybookshelf/') then
    CreateDir(GetEnvironmentVariable('HOME') + '/.mybookshelf/');

 BookList:=TBookCollection.Create;
 if FileExists(dataPath) then
    BookList.LoadData(dataPath, PanelBackground);

 for i:=0 to BookList.Count-1 do
 begin
  tempBook:=BookList.Books[i];
  tempBook.Cover.Left:=X+Xdelta;
  tempBook.Cover.Top:=Y+Ydelta;
  tempBook.Cover.Width:=150;
  tempBook.Cover.Height:=200;
  tempBook.Cover.Parent:=PanelBackground;
  X:=X+Xdelta+150;
 end;


End;

initialization
{$i mybookshelf.lrs}

end.

