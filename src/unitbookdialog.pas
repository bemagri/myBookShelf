unit UnitBookDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, Sysutils, Fileutil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Book, fphttpclient, fpjson, jsonparser, opensslsockets;

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
    ButtonLookup:TButton;
    procedure Buttoncancelclick(Sender: Tobject);
    procedure Buttonsaveclick(Sender: Tobject);
    procedure EditFilePathChange(Sender: Tobject);
    procedure Formcreate(Sender: Tobject);
    procedure Imagebookcoverclick(Sender: Tobject);
    procedure LoadBook(Book:TBook);
    
    // add handlers/helpers
    procedure ButtonLookupClick(Sender: TObject);
    function DownloadURLToFile(const URL, DestPath: String): Boolean;
    procedure TryLookupByISBN(const isbn: String);
    function HTTPGet(const URL: String): String;
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

  // Make it obvious the cover can be changed
  ImageBookCover.Cursor := crHandPoint;
  ImageBookCover.ShowHint := True;
  ImageBookCover.Hint := 'Click to change cover';
End;

procedure TBookEditDialog.ButtonLookupClick(Sender: TObject);
begin
  if Trim(EditISBN.Text) <> '' then
    TryLookupByISBN(Trim(EditISBN.Text))
  else
    ShowMessage('Enter an ISBN first.');
end;

function TBookEditDialog.DownloadURLToFile(const URL, DestPath: String): Boolean;
var c: TFPHTTPClient;
begin
  Result := False;
  c := TFPHTTPClient.Create(nil);
  try
    c.AllowRedirect := True;
    c.AddHeader('User-Agent','myBookShelf/1.0');
    c.ConnectTimeout := 5000;
    c.IOTimeout := 15000;
    c.Get(URL, DestPath);
    Result := FileExists(DestPath);
  finally
    c.Free;
  end;
end;

function TBookEditDialog.HTTPGet(const URL: String): String;
var c: TFPHTTPClient;
begin
  Result := '';
  c := TFPHTTPClient.Create(nil);
  try
    c.AllowRedirect := True;
    c.AddHeader('User-Agent','myBookShelf/1.0 (+https://example.invalid)');
    c.ConnectTimeout := 5000;
    c.IOTimeout := 8000;
    Result := c.Get(URL);
  finally
    c.Free;
  end;
end;

procedure TBookEditDialog.TryLookupByISBN(const isbn: String);
var
  key, jstr: String;
  jroot, jbook, jauth, jcover: TJSONData;
  title, authorsJoined: String;
  i: Integer;
  base, coverPath: String;

  function FirstNonEmpty(const a,b:String):String;
  begin if a<>'' then exit(a); exit(b); end;

  function TryGetJSON(const u1, u2: String): String;
  begin
    try Result := HTTPGet(u1); if Result<>'' then exit; except end;
    try Result := HTTPGet(u2); except end;
  end;

begin
  // 1) Fetch metadata (HTTPS first, HTTP fallback)
  key := 'ISBN:' + Trim(isbn);
  jstr := TryGetJSON(
    Format('https://openlibrary.org/api/books?bibkeys=%s&format=json&jscmd=data',[key]),
    Format('http://openlibrary.org/api/books?bibkeys=%s&format=json&jscmd=data',[key])
  );

  title := ''; authorsJoined := '';
  if jstr <> '' then
  try
    jroot := GetJSON(jstr);
    try
      jbook := jroot.FindPath(key);
      if (jbook <> nil) then
      begin
        // title
        if jbook.FindPath('title') <> nil then
          title := jbook.FindPath('title').AsString;

        // authors array -> join names with ", "
        if jbook.FindPath('authors') <> nil then
        begin
          jauth := jbook.FindPath('authors');
          if (jauth.JSONType = jtArray) then
            for i := 0 to jauth.Count - 1 do
            begin
              if i > 0 then authorsJoined := authorsJoined + ', ';
              authorsJoined := authorsJoined + jauth.Items[i].FindPath('name').AsString;
            end;
        end;

        // If authors empty, fall back to by_statement (from isbn.json)
        if authorsJoined = '' then
        begin
          jstr := TryGetJSON(
            Format('https://openlibrary.org/isbn/%s.json',[isbn]),
            Format('http://openlibrary.org/isbn/%s.json',[isbn])
          );
          if jstr <> '' then
          try
            jcover := GetJSON(jstr);
            try
              if jcover.FindPath('by_statement') <> nil then
                authorsJoined := jcover.FindPath('by_statement').AsString;
              if (title = '') and (jcover.FindPath('title')<>nil) then
                title := jcover.FindPath('title').AsString;
            finally
              jcover.Free;
            end;
          except end;
        end;
      end;
    finally
      jroot.Free;
    end;
  except
    // ignore; handled below
  end;

  // 2) Apply to UI
  if title <> '' then EditTitle.Text := title;
  if authorsJoined <> '' then EditAuthors.Text := authorsJoined;

  // 3) Attempt cover download next to the chosen file
  if Trim(EditFilePath.Text) <> '' then
  begin
    base := ChangeFileExt(EditFilePath.Text, '');
    coverPath := base + '.jpg';

    // Try HTTPS then HTTP; L size then M size
    if not DownloadURLToFile(
         Format('https://covers.openlibrary.org/b/isbn/%s-L.jpg',[isbn]),
         coverPath) then
      if not DownloadURLToFile(
           Format('http://covers.openlibrary.org/b/isbn/%s-L.jpg',[isbn]),
           coverPath) then
        if not DownloadURLToFile(
             Format('https://covers.openlibrary.org/b/isbn/%s-M.jpg',[isbn]),
             coverPath) then
          DownloadURLToFile(
            Format('http://covers.openlibrary.org/b/isbn/%s-M.jpg',[isbn]),
            coverPath);

    if FileExists(coverPath) then
    begin
      EditImagePath.Text := coverPath;
      try
        ImageBookCover.Picture.LoadFromFile(coverPath);
      except
        // ignore preview errors
      end;
    end;
  end;

  // 4) If nothing changed, tell the user (so it’s not silent)
  if (title = '') and (authorsJoined = '') and
     ((Trim(EditFilePath.Text)='') or not FileExists(ChangeFileExt(EditFilePath.Text,'.jpg'))) then
    ShowMessage('No metadata/cover found for ISBN ' + isbn + '.');
end;

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
  // Set file path first (auto-cover logic may look for sibling images)
  mBook.FilePath:=EditFilePath.Text;
  // Then set explicit image path to ensure it takes precedence
  mBook.ImagePath:=EditImagePath.Text;

  // Ensure UI reflects any new cover choice
  mBook.EnsureScaledToCoverSize;
  ModalResult := mrOK;
End;

procedure Tbookeditdialog.Buttoncancelclick(Sender: Tobject);
begin
  ModalResult := mrCancel;
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
