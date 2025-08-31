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
    function Get(Index: Integer): TBook;

  public
    procedure StoreData(path: String);
    procedure LoadData(path: String; parent: TComponent);
    procedure AddBook(book: TBook);
    property  Books[Index: Integer]:TBook read Get;
    procedure Remove(book: TBook);
    function Count: Integer;
    procedure Clear;
    procedure SwapBooks(Source, Dest: Integer);
    constructor Create;
    destructor Destroy; override;

end;

implementation

uses
  unitCoverWorker;

{ TBookCollection }

procedure TBookCollection.Clear;
var
  i : Integer;
  book : TBook;
begin
  CoverWorkerStop;
  for i := mList.Count - 1 downto 0 do
  begin
    book := TBook(mList[i]);
    // Explicitly free the cover control to avoid orphaned images
    if Assigned(book) and Assigned(book.Cover) then
      book.Cover.Free;
    book.Free;               // free the book itself
  end;
  mList.Clear;
end;

function TBookCollection.Get(Index: Integer): TBook;
begin
  Result := TBook(mList.Items[index]);
End;

procedure TBookCollection.AddBook(Book: TBook);
begin
  mList.Add(book);
End;

procedure TBookCollection.Remove(Book: TBook);
begin
  mList.Remove(book);
end;

function TBookCollection.Count: Integer;
begin
  result:=mList.Count;
end;

procedure TBookCollection.SwapBooks(Source, Dest: Integer);
begin
 mList.Move(Source,Dest);
end;

constructor TBookCollection.Create;
begin
  mList:=TFPList.Create;
end;

destructor TBookCollection.Destroy;
var i:Integer;
    book:TBook;
begin
  CoverWorkerStop;
  for i:=0 to mList.Count-1 do
  begin
    book := TBook(mList.Items[i]);
    if Assigned(book) and Assigned(book.Cover) then
      book.Cover.Free;
    FreeAndNil(book);
  end;

  FreeAndNil(mList);
end;


procedure TBookCollection.StoreData(Path: String);
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
          temp:= TBook(mList[i]);
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

procedure TBookCollection.LoadData(Path: String; Parent: TComponent);
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
      tempBook.Title:=title;
      tempBook.Authors:=authors;
      tempBook.ISBN:=isbn;
      tempBook.FilePath:=filepath;
      tempBook.ImagePath:=imagepath;
      mList.Add(tempBook);
    end;

  finally
    CloseFile(dataFile);
  end;

end;

end.
