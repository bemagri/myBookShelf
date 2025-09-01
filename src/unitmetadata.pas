unit unitMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

// Extract basic metadata (title, authors, isbn) from a book file.
// Supports PDF (via pdfinfo) and EPUB (via unzip and parsing the OPF file).
// Returns True if any metadata was found.
function ExtractBookMetadata(const FileName: String; out Title, Authors, Isbn: String): Boolean;

implementation

uses
  Process, DOM, XMLRead, LazUTF8, StrUtils, LazFileUtils, unitLog;

function NormalizeISBN(const S: String): String;
var
  i: Integer; ch: Char; acc: String; src: String;
begin
  // strip common prefixes
  src := StringReplace(S, 'urn:isbn:', '', [rfIgnoreCase]);
  src := StringReplace(src, 'isbn:', '', [rfIgnoreCase]);
  src := StringReplace(src, 'isbn', '', [rfIgnoreCase]);
  acc := '';
  for i := 1 to Length(src) do
  begin
    ch := src[i];
    if (ch >= '0') and (ch <= '9') then acc += ch
    else if (ch = 'x') or (ch = 'X') then acc += ch
    else if (ch = '-') or (ch = ' ') then Continue
    else if (ch = #9) then Continue
    else ;
  end;
  if (Length(acc) = 10) and (acc[Length(acc)] in ['x','X']) then acc[Length(acc)] := 'X';
  if (Length(acc) = 13) or (Length(acc) = 10) then Result := acc else Result := '';
end;

function TryExtractIsbnFromPdfText(const FileName: String): String;
var
  proc: TProcess;
  env: TStringList;
  exe: String;
  outStream: TStringStream;
  text, lower: String;
  i, startPos, endPos: SizeInt;
  window: String;
begin
  Result := '';
  exe := FindDefaultExecutablePath('pdftotext');
  if exe = '' then exe := 'pdftotext';
  LogInfoFmt('pdftotext tool: %s', [exe]);

  proc := TProcess.Create(nil);
  env := TStringList.Create;
  outStream := TStringStream.Create('');
  try
    try
      env.Add('LC_ALL=C'); env.Add('LANG=C');
      env.Add('PATH=' + GetEnvironmentVariable('PATH'));
      proc.Environment := env;
      proc.Executable := exe;
      // Extract first 5 pages to stdout
      proc.Parameters.Add('-f'); proc.Parameters.Add('1');
      proc.Parameters.Add('-l'); proc.Parameters.Add('5');
      proc.Parameters.Add('-nopgbrk');
      proc.Parameters.Add('-layout');
      proc.Parameters.Add(FileName);
      proc.Parameters.Add('-');
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      LogDebugFmt('Running: %s -f 1 -l 5 -nopgbrk -layout %s -', [proc.Executable, FileName]);
      proc.Execute;
      outStream.CopyFrom(proc.Output, 0);
      text := outStream.DataString;
      lower := UTF8LowerCase(text);
      // Heuristic: search for 'isbn' and decode nearby token
      i := 1;
      while i <= Length(lower) - 3 do
      begin
        if Copy(lower, i, 4) = 'isbn' then
        begin
          startPos := i;
          endPos := i + 64; // search window after 'isbn'
          if endPos > Length(text) then endPos := Length(text);
          window := Copy(text, startPos, endPos - startPos + 1);
          Result := NormalizeISBN(window);
          if Result <> '' then Break;
        end;
        Inc(i);
      end;
      if (Result = '') then
      begin
        // Fallback: scan text in chunks for any isbn-like token
        i := 1;
        while i <= Length(text) do
        begin
          endPos := i + 32;
          if endPos > Length(text) then endPos := Length(text);
          window := Copy(text, i, endPos - i + 1);
          Result := NormalizeISBN(window);
          if Result <> '' then Break;
          Inc(i, 16);
        end;
      end;
      LogInfoFmt('pdftotext ISBN guess: "%s"', [Result]);
    except
      on E: Exception do LogErrorFmt('pdftotext failed: %s', [E.Message]);
    end;
  finally
    outStream.Free;
    env.Free;
    proc.Free;
  end;
end;

function ExtractPDFMetadata(const FileName: String; out Title, Authors, Isbn: String): Boolean;
var
  proc: TProcess;
  sl: TStringList;
  line: String;
  i: Integer;
  exe: String;
  env: TStringList;
begin
  Result := False;
  Title := '';
  Authors := '';
  Isbn := '';
  exe := FindDefaultExecutablePath('pdfinfo');
  if exe = '' then exe := 'pdfinfo';
  LogInfoFmt('pdfinfo tool: %s', [exe]);
  proc := TProcess.Create(nil);
  sl := TStringList.Create;
  env := TStringList.Create;
  try
    try
      // Force English output regardless of user locale and preserve PATH
      env.Add('LC_ALL=C');
      env.Add('LANG=C');
      env.Add('PATH=' + GetEnvironmentVariable('PATH'));
      proc.Environment := env;
      proc.Executable := exe;
      proc.Parameters.Add(FileName);
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      LogDebugFmt('Running: %s %s', [proc.Executable, FileName]);
      proc.Execute;
      sl.LoadFromStream(proc.Output);
      LogDebugFmt('pdfinfo exit=%d, output lines=%d', [proc.ExitStatus, sl.Count]);
      for i := 0 to sl.Count - 1 do
      begin
        line := sl[i];
        if (Title = '') and AnsiStartsStr('Title:', line) then
          Title := Trim(Copy(line, 7, MaxInt));
        if (Authors = '') and (AnsiStartsStr('Author:', line) or AnsiStartsStr('Authors:', line)) then
          Authors := Trim(Copy(line, Pos(':', line) + 1, MaxInt));
        if Isbn = '' then
          Isbn := NormalizeISBN(line);
      end;
      // If not found in pdfinfo, try extracting from the first pages text
      if Isbn = '' then
        Isbn := TryExtractIsbnFromPdfText(FileName);
      Result := (Title <> '') or (Authors <> '') or (Isbn <> '');
      LogInfoFmt('PDF metadata parsed: title="%s" authors="%s" isbn="%s" result=%s',
        [Title, Authors, Isbn, BoolToStr(Result, True)]);
    except
      on E: Exception do
      begin
        LogErrorFmt('pdfinfo failed: %s', [E.Message]);
        Result := False;
      end;
    end;
  finally
    sl.Free;
    env.Free;
    proc.Free;
  end;
end;

function ExtractEPUBMetadata(const FileName: String; out Title, Authors, Isbn: String): Boolean;
var
  proc: TProcess;
  sl: TStringList;
  exe, opfPath, line: String;
  xml: TXMLDocument;
  stream: TStringStream;
  meta, node: TDOMNode;
  i: Integer;
  lname: String;
  env: TStringList;
begin
  Result := False;
  Title := '';
  Authors := '';
  Isbn := '';
  exe := FindDefaultExecutablePath('unzip');
  if exe = '' then exe := 'unzip';
  LogInfoFmt('unzip tool: %s', [exe]);
  // list files
  proc := TProcess.Create(nil);
  sl := TStringList.Create;
  env := TStringList.Create;
  try
    try
      env.Add('LC_ALL=C'); env.Add('LANG=C');
      env.Add('PATH=' + GetEnvironmentVariable('PATH'));
      proc.Environment := env;
      proc.Executable := exe;
      proc.Parameters.Add('-Z1');
      proc.Parameters.Add(FileName);
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      LogDebugFmt('Running: %s -Z1 %s', [proc.Executable, FileName]);
      proc.Execute;
      sl.LoadFromStream(proc.Output);
      LogDebugFmt('unzip -Z1 exit=%d, lines=%d', [proc.ExitStatus, sl.Count]);
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
      on E: Exception do
      begin
        LogErrorFmt('unzip -Z1 failed: %s', [E.Message]);
        opfPath := '';
      end;
    end;
  finally
    sl.Free;
    env.Free;
    proc.Free;
  end;
  if opfPath = '' then Exit;
  // extract opf content
  proc := TProcess.Create(nil);
  stream := TStringStream.Create('');
  try
    try
      env := TStringList.Create;
      env.Add('LC_ALL=C'); env.Add('LANG=C');
      env.Add('PATH=' + GetEnvironmentVariable('PATH'));
      proc.Environment := env;
      proc.Executable := exe;
      proc.Parameters.Add('-p');
      proc.Parameters.Add(FileName);
      proc.Parameters.Add(opfPath);
      proc.Options := [poWaitOnExit, poUsePipes];
      proc.ShowWindow := swoHide;
      LogDebugFmt('Running: %s -p %s %s', [proc.Executable, FileName, opfPath]);
      proc.Execute;
      stream.CopyFrom(proc.Output, 0);
      stream.Position := 0;
    except
      on E: Exception do
      begin
        LogErrorFmt('unzip -p failed: %s', [E.Message]);
        stream.Size := 0;
      end;
    end;
  finally
    if Assigned(env) then env.Free;
    proc.Free;
  end;
  try
    try
      ReadXMLFile(xml, stream);
    except
      on E: Exception do
      begin
        LogErrorFmt('ReadXML OPF failed: %s', [E.Message]);
        Exit(False);
      end;
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
          if (lname = 'dc:identifier') or (lname = 'identifier') then
          begin
            if Isbn = '' then Isbn := NormalizeISBN(UTF8Encode(Trim(node.TextContent)));
            if (Isbn = '') and (node is TDOMElement) then
              Isbn := NormalizeISBN(UTF8Encode(TDOMElement(node).GetAttribute('opf:scheme')));
          end;
        end;
      end;
    finally
      xml.Free;
    end;
  finally
    stream.Free;
  end;
  Result := (Title <> '') or (Authors <> '') or (Isbn <> '');
  LogInfoFmt('EPUB metadata parsed: title="%s" authors="%s" isbn="%s" result=%s',
    [Title, Authors, Isbn, BoolToStr(Result, True)]);
end;

function ExtractBookMetadata(const FileName: String; out Title, Authors, Isbn: String): Boolean;
var
  ext: String;
begin
  ext := LowerCase(ExtractFileExt(FileName));
  if ext = '.pdf' then
    Result := ExtractPDFMetadata(FileName, Title, Authors, Isbn)
  else if ext = '.epub' then
    Result := ExtractEPUBMetadata(FileName, Title, Authors, Isbn)
  else
  begin
    Title := '';
    Authors := '';
    Isbn := '';
    Result := False;
  end;
end;

end.
