unit bookCollection;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Book;

type

{ TBookCollection }

TBookCollection = class(TObject)
  private
    mList : TList;
    mDataPath:String;
    function Get(Index: Integer): Tbook;

  public
    procedure StoreData(path:String);
    procedure LoadData(path:String; parent:TComponent);
    procedure AddBook(book: Tbook);
    property  Books[Index: Integer]:TBook read Get;
    function Count:Integer;
    constructor Create;
    destructor Destroy;

end;

implementation

{ TBookCollection }

function Tbookcollection.Get(Index: Integer): Tbook;
begin
  result:=(TBook (mList.Items[index]));
End;

procedure Tbookcollection.Addbook(Book: Tbook);
begin
  mList.Add(book);
End;

function Tbookcollection.Count: Integer;
begin
  result:=mList.Count;
end;

constructor Tbookcollection.Create;
begin
  mList:=TList.Create;
end;

destructor Tbookcollection.Destroy;
var i:Integer;
    book:TBook;
begin
  for i:=0 to mList.Count-1 do
      begin
        book:= (TBook(mList.Items[i]));
        FreeAndNil(book);
      end;

  FreeAndNil(mList);
end;


procedure Tbookcollection.Storedata(Path: String);
var
  tfOut: TextFile;
  i:integer;
  temp:TBook;
begin
  // Set the name of the file that will be created
  AssignFile(tfOut, path);

  try
    // Create the file, write some text and close it.
    rewrite(tfOut);
    for i:=0 to mList.Count-1 do
        begin
          temp:= (TBook(mList[i]));
          writeln(tfOut, temp.FilePath);
        end;

    CloseFile(tfOut);

  except
    // If there was an error the reason can be found here
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;

end;

procedure Tbookcollection.Loaddata(Path: String; Parent: Tcomponent);
var tempBook:TBook;
    tempPath:String;
    dataFile:TextFile;
begin
  AssignFile(dataFile, path);

  try
    Reset(dataFile);
    while not EOF(dataFile) do
    begin
      readln(dataFile, tempPath);
      tempBook:=TBook.Create(parent);
      tempBook.FilePath:=tempPath;
      mList.Add(tempBook);
    end;

  finally
    CloseFile(dataFile);
  end;

end;

end.

