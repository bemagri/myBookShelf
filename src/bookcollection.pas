unit bookCollection;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Book;

type

{ TBookCollection }

TBookCollection = class(TObject)
  private
    mList : TFPList;
    function Get(Index: Integer): Tbook;

  public
    procedure StoreData(path:String);
    procedure LoadData(path:String; parent:TComponent);
    procedure AddBook(book: Tbook);
    property  Books[Index: Integer]:TBook read Get;
    procedure Remove(book:TBook);
    function Count:Integer;
    procedure SwapBooks(Source,Dest:Integer);
    constructor Create;
    destructor Destroy; override;

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

procedure Tbookcollection.Remove(Book: Tbook);
begin
  mList.Remove(book);
end;

function Tbookcollection.Count: Integer;
begin
  result:=mList.Count;
end;

procedure Tbookcollection.Swapbooks(Source, Dest: Integer);
begin
 mList.Move(Source,Dest);
end;

constructor Tbookcollection.Create;
begin
  mList:=TFPList.Create;
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
          writeln(tfOut, temp.Title);
          WriteLn(tfOut, temp.Authors);
          WriteLn(tfOut, temp.ISBN);
          writeLn(tfOut, temp.FilePath);
          writeLn(tfOut, temp.ImagePath);
          writeLn(tfOut, '**********************');
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
    title,filepath,imagepath:String;
    authors,isbn:String;
    dataFile:TextFile;
begin
  AssignFile(dataFile, path);

  try
    Reset(dataFile);
    while not EOF(dataFile) do
    begin
      readln(dataFile, title);
      readln(datafile, authors);
      readln(datafile, isbn);
      readln(datafile, filepath);
      readln(datafile, imagepath);
      readln(datafile);

      tempBook:=TBook.Create(parent);
      tempbook.Title:=title;
      tempbook.Authors:=authors;
      tempbook.ISBN:=isbn;
      tempBook.FilePath:=filepath;
      tempBook.ImagePath:=imagepath;
      mList.Add(tempBook);
    end;

  finally
    CloseFile(dataFile);
  end;

end;

end.

