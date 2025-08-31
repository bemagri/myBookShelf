unit unitLog;

{$mode objfpc}{$H+}

interface

procedure LogDebug(const Msg: string);
procedure LogInfo(const Msg: string);
procedure LogWarn(const Msg: string);
procedure LogError(const Msg: string);
procedure LogDebugFmt(const Fmt: string; const Args: array of const);
procedure LogInfoFmt(const Fmt: string; const Args: array of const);
procedure LogWarnFmt(const Fmt: string; const Args: array of const);
procedure LogErrorFmt(const Fmt: string; const Args: array of const);

implementation

uses
  SysUtils, LazFileUtils, SyncObjs;

var
  LogLock: TRTLCriticalSection;

function LogFilePath: string;
var dir: string;
begin
  dir := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False));
  if not DirectoryExistsUTF8(dir) then CreateDirUTF8(dir);
  Result := dir + 'mybookshelf.log';
end;

procedure WriteLine(const Level, Msg: string);
var
  f: TextFile;
  path: string;
begin
  path := LogFilePath;
  EnterCriticalSection(LogLock);
  try
    AssignFile(f, path);
    try
      if FileExistsUTF8(path) then
        Append(f)
      else
        Rewrite(f);
      try
        WriteLn(f, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), ' [', Level, '] ', Msg);
      finally
        CloseFile(f);
      end;
    except
      // swallow logging errors
    end;
  finally
    LeaveCriticalSection(LogLock);
  end;
end;

procedure LogDebug(const Msg: string); begin WriteLine('DEBUG', Msg); end;
procedure LogInfo(const Msg: string);  begin WriteLine('INFO',  Msg); end;
procedure LogWarn(const Msg: string);  begin WriteLine('WARN',  Msg); end;
procedure LogError(const Msg: string); begin WriteLine('ERROR', Msg); end;

procedure LogDebugFmt(const Fmt: string; const Args: array of const); begin LogDebug(Format(Fmt, Args)); end;
procedure LogInfoFmt(const Fmt: string; const Args: array of const);  begin LogInfo(Format(Fmt, Args)); end;
procedure LogWarnFmt(const Fmt: string; const Args: array of const);  begin LogWarn(Format(Fmt, Args)); end;
procedure LogErrorFmt(const Fmt: string; const Args: array of const); begin LogError(Format(Fmt, Args)); end;

initialization
  InitCriticalSection(LogLock);

finalization
  DoneCriticalSection(LogLock);

end.

