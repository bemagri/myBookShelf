unit unitStorageXML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, Controls, Book, BookCollection,
  LazUTF8, LazFileUtils;

{ Load books from an XML file into AList. Clears the collection first.
  Parent is where TBook cover controls should be parented (e.g., PanelBackground). }
procedure LoadBooksXML(const FileName: String; aList: TBookCollection; Parent: TWinControl);

{ Save AList to XML. Writes to <FileName>.tmp then atomically renames to FileName.
  Ensures no duplicate entries are written (based on a stable key). }
procedure SaveBooksXML(const FileName: String; aList: TBookCollection);

implementation

function NormLower(const S: String): String; inline;
begin
  Result := UTF8LowerCase(Trim(S));
end;

function KeyFor(const Title, Authors, Isbn, FilePath: String): String;
var fp: String;
begin
  if Trim(FilePath) <> '' then
  begin
    fp := ExpandFileName(FilePath);
    Exit('fp:' + NormLower(fp));
  end;
  if Trim(Isbn) <> '' then
    Exit('isbn:' + NormLower(Isbn));
  Exit('ti:' + NormLower(Title) + '|' + NormLower(Authors));
end;

procedure LoadBooksXML(const FileName: String; aList: TBookCollection; Parent: TWinControl);
var
  Doc   : TXMLDocument;
  Root  : TDOMElement;
  Node  : TDOMNode;
  El    : TDOMElement;
  B     : TBook;
  title, authors, isbn, filep, imagep: String;
  seen  : TStringList;
  key   : String;
begin
  if (aList = nil) or (Parent = nil) then Exit;

  if not FileExistsUTF8(FileName) then
  begin
    // Nothing to load; ensure list is empty
    if aList.Count > 0 then
      aList.Clear;
    Exit;
  end;

  ReadXMLFile(Doc, FileName);
  try
    Root := Doc.DocumentElement;   // <bookshelf version="1">
    if (Root = nil) or (UTF8LowerCase(Root.TagName) <> 'bookshelf') then
      Exit;

    // Start fresh to avoid duplication-on-startup
    aList.Clear;

    seen := TStringList.Create;
    try
      seen.Sorted := True; seen.Duplicates := dupIgnore;

      Node := Root.FirstChild;
      while Node <> nil do
      begin
        if (Node.NodeType = ELEMENT_NODE) then
        begin
          El := TDOMElement(Node);
          if UTF8LowerCase(El.TagName) = 'book' then
          begin
            // DOM returns UnicodeString; convert explicitly to UTF-8 to avoid warnings
            title   := UTF8Encode(El.GetAttribute('title'));
            authors := UTF8Encode(El.GetAttribute('authors'));
            isbn    := UTF8Encode(El.GetAttribute('isbn'));
            filep   := UTF8Encode(El.GetAttribute('file'));
            imagep  := UTF8Encode(El.GetAttribute('image'));

            key := KeyFor(title, authors, isbn, filep);
            if seen.IndexOf(key) < 0 then
            begin
              seen.Add(key);

              // Recreate the book object and add to collection
              B := TBook.Create(Parent);
              B.Title   := title;
              B.Authors := authors;
              B.ISBN    := isbn;
              if filep <> '' then B.FilePath  := filep;   // will try sibling images/pdf cover
              if imagep <> '' then B.ImagePath := imagep; // if a specific cover was saved

              // NOTE: If your BookCollection uses a different adder, adjust this line:
              aList.AddBook(B);
            end;
          end;
        end;
        Node := Node.NextSibling;
      end;
    finally
      seen.Free;
    end;
  finally
     Doc.Free;
  end;
end;

procedure SaveBooksXML(const FileName: String; aList: TBookCollection);
var
  Doc   : TXMLDocument;
  Root  : TDOMElement;
  El    : TDOMElement;
  i     : Integer;
  B     : TBook;
  tmp   : String;
  seen  : TStringList;
  key   : String;
begin
  if aList = nil then Exit;

  // Build XML document
  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement('bookshelf');
    Root.SetAttribute('version','1');
    Doc.AppendChild(Root);

    seen := TStringList.Create;
    try
      seen.Sorted := True; seen.Duplicates := dupIgnore;

      for i := 0 to aList.Count - 1 do
      begin
        B := aList.Books[i];
        key := KeyFor(B.Title, B.Authors, B.ISBN, B.FilePath);
        if seen.IndexOf(key) >= 0 then
          Continue; // skip duplicates in memory

        seen.Add(key);

        El := Doc.CreateElement('book');
        // Convert UTF-8 AnsiString to UnicodeString for XML writer
        El.SetAttribute('title',   UTF8Decode(B.Title));
        El.SetAttribute('authors', UTF8Decode(B.Authors));
        El.SetAttribute('isbn',    UTF8Decode(B.ISBN));
        El.SetAttribute('file',    UTF8Decode(B.FilePath));
        El.SetAttribute('image',   UTF8Decode(B.ImagePath));

        Root.AppendChild(El);
      end;
    finally
      seen.Free;
    end;

    // Atomic write: to .tmp then rename
    tmp := FileName + '.tmp';
    WriteXMLFile(Doc, tmp);
    // Replace using UTF-8 safe file ops
    if FileExistsUTF8(FileName) then
      DeleteFileUTF8(FileName);
    if not RenameFileUTF8(tmp, FileName) then
      raise Exception.CreateFmt('Failed to write %s', [FileName]);
  finally
    Doc.Free;
  end;
end;

end.
