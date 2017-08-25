unit UnitBookDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Book;

type

  { TBookEditDialog }

  TBookEditDialog = class(Tform)
    ButtonSave: Tbitbtn;
    ButtonCancel: Tbitbtn;
    EditImagePath: Tedit;
    EditTitle: Tedit;
    EditAuthors: Tedit;
    EditISBN: Tedit;
    EditFilePath: Tedit;
    ImageBookCover: Timage;
    Label1: Tlabel;
    Label2: Tlabel;
    Label3: Tlabel;
    Label4: Tlabel;
    Label5: Tlabel;
    Opendialog1: Topendialog;
    Panel1: Tpanel;
    procedure Buttoncancelclick(Sender: Tobject);
    procedure Buttonsaveclick(Sender: Tobject);
    procedure EditFilePathChange(Sender: Tobject);
    procedure Formcreate(Sender: Tobject);
    procedure Imagebookcoverclick(Sender: Tobject);
    procedure LoadBook(Book:TBook);
  private
    mBook:TBook;
    { private declarations }
  public
    { public declarations }
  end;

var
  BookEditDialog: TBookEditDialog;

implementation


{$R *.lfm}

{ TBookEditDialog }

procedure Tbookeditdialog.Formcreate(Sender: Tobject);
begin
  ActiveControl:=ButtonSave;

End;

procedure Tbookeditdialog.Imagebookcoverclick(Sender: Tobject);
begin
  if Opendialog1.Execute then
  begin
       EditImagePath.Text:= Opendialog1.FileName;
       ImageBookCover.Picture.LoadFromFile(EditImagePath.Text);
  end;
End;


procedure Tbookeditdialog.EditFilePathChange(Sender: Tobject);
begin

End;

procedure Tbookeditdialog.Buttonsaveclick(Sender: Tobject);
begin
  //save the book info
  mbook.Title:=EditTitle.Text;
  mBook.Authors:=EditAuthors.Text;
  mBook.ISBN:=editisbn.Text;
  mBook.ImagePath:=editimagepath.Text;
  mBook.FilePath:=EditFilePath.Text;

  Close;
End;

procedure Tbookeditdialog.Buttoncancelclick(Sender: Tobject);
begin
  Close;
End;

procedure Tbookeditdialog.Loadbook(Book: Tbook);
begin
  mBook:=Book;
  ImageBookCover.Picture:=mBook.Cover.Picture;
  EditFilePath.Text:=mBook.FilePath;
  EditTitle.Text:=mBook.Title;
  EditAuthors.Text:=mBook.Authors;
  EditISBN.Text:=mBook.ISBN;
  EditImagePath.Text:=mBook.ImagePath;
end;

end.

