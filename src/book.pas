unit Book;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Graphics, ExtCtrls, LCLIntf, Controls;

type

  { TBook }

  TBook = class(TObject)
    private
      mTitle : String;
      mAuthors:String;
      mISBN:String;
      mFilePath : String;
      mCover : TImage;
      mImagePath : String;
      mImageHeight : Integer;
      mImageWidth : Integer;
      mIsSelected: Boolean;
      procedure Setfile(Avalue: String);
      procedure Setimage(Avalue: String);
    public
      constructor Create(parent:TComponent);
      procedure Bookmousedown(Sender: Tobject; Button: Tmousebutton;
        Shift: Tshiftstate; X, Y: Integer);
      property Cover : TImage read mCover;
      property ImagePath : String read mImagePath write SetImage;
      property FilePath : String read mFilePath write SetFile;
      property isSelected : Boolean read mIsSelected write mIsSelected;
      property Title : String read mTitle write mTitle;
      property Authors : String read mAuthors write mAuthors;
      property ISBN : String read mISBN write mISBN;
      destructor Destroy;
      procedure BookDoubleClick(Sender:TObject);
      procedure BookCoverPaint(Sender:TObject);
  end;

implementation

uses UnitBookDialog;

{ TBook }

procedure Tbook.Setimage(Avalue: String);
begin
  mImagePath:=AValue;
  if not FileExists(Avalue) then
     mCover.Picture.LoadFromLazarusResource('generic_cover')
  else
  begin
     mImagePath:=AValue;
     mCover.Picture.LoadFromFile(mImagePath);
  end;
End;

procedure Tbook.Setfile(Avalue: String);
begin
  if Mfilepath=Avalue then Exit;
  Mfilepath:=Avalue;
  SetImage(ChangeFileExt(Avalue, '.png'));
  SetImage(ChangeFileExt(Avalue, '.jpg'));
end;

constructor Tbook.Create(Parent: Tcomponent);
var pic:TPicture;
begin
  mCover:=TImage.Create(parent);
  pic:=TPicture.Create;
  mCover.Picture:=pic;
  mCover.Stretch:=true;
  mCover.OnDblClick:=@BookDoubleClick;
  mCover.OnPaint:=@BookCoverPaint;
  mCover.OnMouseDown:=@Bookmousedown;
  mIsSelected:=False;
  mTitle:='';
  mAuthors:='';
  mISBN:='';
  mImagePath:='';
end;

procedure Tbook.Bookmousedown(Sender: Tobject; Button: Tmousebutton;
  Shift: Tshiftstate; X, Y: Integer);
var  dialog:TBookEditDialog;
begin
  if Button = TMouseButton.mbRight then
     begin
        mIsSelected:=True;
        mCover.Repaint;
        dialog:= TBookEditDialog.Create(nil);
        dialog.LoadBook(Self);
        dialog.ShowModal;
        mIsSelected:=False;
        mCover.Repaint;
     end;
  if Button = TMouseButton.mbLeft then
     begin
        mIsSelected:= not mIsSelected;
        mCover.Repaint;
     end;
End;


destructor Tbook.Destroy;
begin
  FreeAndNil(mCover);
end;


procedure Tbook.Bookdoubleclick(Sender: Tobject);
begin
  mIsSelected:=True;
  mCover.Repaint;
  OpenDocument(mFilePath);
end;

procedure Tbook.Bookcoverpaint(Sender: Tobject);
begin
  if mIsSelected then
    begin
    mCover.Canvas.Brush.Style:=bsClear;
    mCover.Canvas.Pen.Width:=4;
    mCover.Canvas.Pen.Color:=clRed;
    mCover.Canvas.Rectangle(1,1,mCover.Width,mCover.Height);
    end;
end;


end.

