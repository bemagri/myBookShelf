unit unitTempUtils;

{$mode objfpc}{$H+}

interface

procedure RegisterTempCoverFile(const Path: string);
procedure CleanupSessionTempCovers;
procedure CleanupOldTempCoverFiles(MaxAgeHours: Integer);

implementation

uses
  Classes, SysUtils, LazFileUtils, DateUtils;

var
  GTempCovers: TStringList;

procedure EnsureList;
begin
  if GTempCovers = nil then
  begin
    GTempCovers := TStringList.Create;
    GTempCovers.Sorted := False;
    GTempCovers.Duplicates := dupIgnore;
  end;
end;

procedure RegisterTempCoverFile(const Path: string);
begin
  if (Trim(Path) = '') then Exit;
  if not FileExistsUTF8(Path) then Exit;
  EnsureList;
  GTempCovers.Add(Path);
end;

procedure CleanupSessionTempCovers;
var i: Integer;
begin
  if GTempCovers = nil then Exit;
  for i := 0 to GTempCovers.Count - 1 do
  begin
    try
      if FileExistsUTF8(GTempCovers[i]) then
        DeleteFileUTF8(GTempCovers[i]);
    except
      // ignore
    end;
  end;
  FreeAndNil(GTempCovers);
end;

procedure CleanupOldTempCoverFiles(MaxAgeHours: Integer);
var
  sr: TSearchRec;
  mask, tmpDir, path: string;
  fileAge: LongInt;
  dt: TDateTime;
  ageHours: Double;
begin
  if MaxAgeHours <= 0 then Exit;
  tmpDir := GetTempDir(False);
  mask := IncludeTrailingPathDelimiter(tmpDir) + 'mybookshelf_cover_*.png';
  if FindFirstUTF8(mask, faAnyFile and faArchive, sr) = 0 then
  try
    repeat
      path := IncludeTrailingPathDelimiter(tmpDir) + sr.Name;
      try
        fileAge := FileAgeUTF8(path);
        if fileAge >= 0 then
        begin
          dt := FileDateToDateTime(fileAge);
          ageHours := HoursBetween(Now, dt);
          if ageHours >= MaxAgeHours then
            DeleteFileUTF8(path);
        end;
      except
        // ignore per-file errors
      end;
    until FindNextUTF8(sr) <> 0;
  finally
    FindCloseUTF8(sr);
  end;
end;

finalization
  // ensure list is freed
  if GTempCovers <> nil then
    GTempCovers.Free;

end.
