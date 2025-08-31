unit unitMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

// Extract basic metadata (title, authors) from a book file.
// Supports PDF (via pdfinfo) and EPUB (via unzip and parsing the OPF file).
// Returns True if any metadata was found.
function ExtractBookMetadata(const FileName: String; out Title, Authors: String): Boolean;

implementation

uses
  Process, DOM, XMLRead, LazUTF8, StrUtils, LazFileUtils;

function ExtractPDFMetadata(const FileName: String; out Title, Authors: String): Boolean;
var
  proc: TProcess;
  sl: TStringList;
  line: String;
  i: Integer;
  exe: String;
begin
  Result := False;
  Title := '';
  Authors := '';
  exe := FindDefaultExecutablePath('pdfinfo');
  if exe = '' then exe := 'pdfinfo';
  proc := TProcess.Create(nil);
  sl := TStringList.Create;
  try
    try
      proc.Executable := exe;
      proc.Parameters.Add(FileName);
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      proc.Execute;
      sl.LoadFromStream(proc.Output);
      for i := 0 to sl.Count - 1 do
      begin
        line := sl[i];
        if (Title = '') and AnsiStartsStr('Title:', line) then
          Title := Trim(Copy(line, 7, MaxInt));
        if (Authors = '') and (AnsiStartsStr('Author:', line) or AnsiStartsStr('Authors:', line)) then
          Authors := Trim(Copy(line, Pos(':', line) + 1, MaxInt));
      end;
      Result := (Title <> '') or (Authors <> '');
    except
      Result := False;
    end;
  finally
    sl.Free;
    proc.Free;
  end;
end;

function ExtractEPUBMetadata(const FileName: String; out Title, Authors: String): Boolean;
var
  proc: TProcess;
  sl: TStringList;
  exe, opfPath, line: String;
  xml: TXMLDocument;
  stream: TStringStream;
  meta, node: TDOMNode;
  i: Integer;
  lname: String;
begin
  Result := False;
  Title := '';
  Authors := '';
  exe := FindDefaultExecutablePath('unzip');
  if exe = '' then exe := 'unzip';
  // list files
  proc := TProcess.Create(nil);
  sl := TStringList.Create;
  try
    try
      proc.Executable := exe;
      proc.Parameters.Add('-Z1');
      proc.Parameters.Add(FileName);
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      proc.Execute;
      sl.LoadFromStream(proc.Output);
      opfPath := '';
      for i := 0 to sl.Count - 1 do
      begin
        line := Trim(sl[i]);
        if LowerCase(ExtractFileExt(line)) = '.opf' then
        begin
          opfPath := line;
          Break;
        end;
      end;
    except
      opfPath := '';
    end;
  finally
    sl.Free;
    proc.Free;
  end;
  if opfPath = '' then Exit;
  // extract opf content
  proc := TProcess.Create(nil);
  stream := TStringStream.Create('');
  try
    try
      proc.Executable := exe;
      proc.Parameters.Add('-p');
      proc.Parameters.Add(FileName);
      proc.Parameters.Add(opfPath);
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      proc.Execute;
      stream.CopyFrom(proc.Output, 0);
      stream.Position := 0;
    except
      stream.Size := 0;
    end;
  finally
    proc.Free;
  end;
  try
    try
      ReadXMLFile(xml, stream);
    except
      Exit(False);
    end;
    try
      meta := xml.DocumentElement.FindNode('metadata');
      if meta <> nil then
      begin
        for i := 0 to meta.ChildNodes.Count - 1 do
        begin
          node := meta.ChildNodes[i];
          lname := UTF8LowerCase(node.NodeName);
          if (Title = '') and ((lname = 'dc:title') or (lname = 'title')) then
            Title := UTF8Encode(Trim(node.TextContent));
          if ((lname = 'dc:creator') or (lname = 'creator') or (lname = 'dc:author') or (lname = 'author')) then
          begin
            if Authors <> '' then Authors := Authors + ', ';
            Authors := Authors + UTF8Encode(Trim(node.TextContent));
          end;
        end;
      end;
    finally
      xml.Free;
    end;
  finally
    stream.Free;
  end;
  Result := (Title <> '') or (Authors <> '');
end;

function ExtractBookMetadata(const FileName: String; out Title, Authors: String): Boolean;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if ext = '.pdf' then
    Result := ExtractPDFMetadata(FileName, Title, Authors)
  else if ext = '.epub' then
    Result := ExtractEPUBMetadata(FileName, Title, Authors)
  else
  begin
    Title := '';
    Authors := '';
    Result := False;
  end;
end;

end.
