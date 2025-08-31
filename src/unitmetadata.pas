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
  Process, DOM, XMLRead, LazUTF8, StrUtils, LazFileUtils, unitLog;

function ExtractPDFMetadata(const FileName: String; out Title, Authors: String): Boolean;
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
      end;
      Result := (Title <> '') or (Authors <> '');
      LogInfoFmt('PDF metadata parsed: title="%s" authors="%s" result=%s',
        [Title, Authors, BoolToStr(Result, True)]);
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
  env: TStringList;
begin
  Result := False;
  Title := '';
  Authors := '';
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
        end;
      end;
    finally
      xml.Free;
    end;
  finally
    stream.Free;
  end;
  Result := (Title <> '') or (Authors <> '');
  LogInfoFmt('EPUB metadata parsed: title="%s" authors="%s" result=%s',
    [Title, Authors, BoolToStr(Result, True)]);
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
