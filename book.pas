unit Book;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Graphics, ExtCtrls, LCLIntf;

type

  { TBook }

  TBook = class(TObject)
    private
      mTitle : String;
      mFilePath : String;
      mCover : TImage;
      mImagePath : String;
      mImageHeight : Integer;
      mImageWidth : Integer;
      procedure Setfile(Avalue: String);
      procedure Setimage(Avalue: String);
    public
      constructor Create(parent:TComponent);
      property Cover : TImage read mCover;
      property ImagePath : String write SetImage;
      property FilePath : String read mFilePath write SetFile;
      destructor Destroy;
      procedure BookClick(Sender:TObject);

  end;

implementation

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
  SetImage(ChangeFileExt(Avalue, '.jpg'));
end;

constructor Tbook.Create(Parent: Tcomponent);
var pic:TPicture;
begin
  mCover:=TImage.Create(parent);
  pic:=TPicture.Create;
  mCover.Picture:=pic;
  mCover.Stretch:=true;
  mCover.OnClick:=@BookClick;
end;


destructor Tbook.Destroy;
begin
  FreeAndNil(mCover);
end;

procedure Tbook.Bookclick(Sender: Tobject);
begin
  OpenDocument(mFilePath);
end;

end.

