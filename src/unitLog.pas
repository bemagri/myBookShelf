unit unitLog;

{$mode objfpc}{$H+}

interface

type
  TLogLevel = (llDebug, llInfo, llWarn, llError);

procedure SetLogLevel(Level: TLogLevel);
function  GetLogLevel: TLogLevel;
procedure LogDebug(const Msg: string);
procedure LogInfo(const Msg: string);
procedure LogWarn(const Msg: string);
procedure LogError(const Msg: string);
procedure LogDebugFmt(const Fmt: string; const Args: array of const);
procedure LogInfoFmt(const Fmt: string; const Args: array of const);
procedure LogWarnFmt(const Fmt: string; const Args: array of const);
procedure LogErrorFmt(const Fmt: string; const Args: array of const);

implementation

{$IFDEF MB_LOG}
uses SysUtils, LazFileUtils, SyncObjs;

var
  LogLock: TRTLCriticalSection;
  gMinLevel: TLogLevel = llWarn; // default: only warn+error

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

procedure SetLogLevel(Level: TLogLevel); begin gMinLevel := Level; end;
function  GetLogLevel: TLogLevel; begin Result := gMinLevel; end;
procedure LogDebug(const Msg: string); begin if gMinLevel <= llDebug then WriteLine('DEBUG', Msg); end;
procedure LogInfo(const Msg: string);  begin if gMinLevel <= llInfo  then WriteLine('INFO',  Msg); end;
procedure LogWarn(const Msg: string);  begin if gMinLevel <= llWarn  then WriteLine('WARN',  Msg); end;
procedure LogError(const Msg: string); begin if gMinLevel <= llError then WriteLine('ERROR', Msg); end;
procedure LogDebugFmt(const Fmt: string; const Args: array of const); begin if gMinLevel <= llDebug then LogDebug(Format(Fmt, Args)); end;
procedure LogInfoFmt(const Fmt: string; const Args: array of const);  begin if gMinLevel <= llInfo  then LogInfo(Format(Fmt, Args)); end;
procedure LogWarnFmt(const Fmt: string; const Args: array of const);  begin if gMinLevel <= llWarn  then LogWarn(Format(Fmt, Args)); end;
procedure LogErrorFmt(const Fmt: string; const Args: array of const); begin if gMinLevel <= llError then LogError(Format(Fmt, Args)); end;

initialization
  InitCriticalSection(LogLock);
  try
    case LowerCase(GetEnvironmentVariable('MYBOOKSHELF_LOGLEVEL')) of
      'debug': gMinLevel := llDebug;
      'info':  gMinLevel := llInfo;
      'warn', 'warning': gMinLevel := llWarn;
      'error': gMinLevel := llError;
    end;
  except end;
finalization
  DoneCriticalSection(LogLock);
{$ELSE}
// Logging disabled at compile time; provide no-op stubs
procedure SetLogLevel(Level: TLogLevel); begin end;
function  GetLogLevel: TLogLevel; begin Result := llError; end;
procedure LogDebug(const Msg: string); begin end;
procedure LogInfo(const Msg: string);  begin end;
procedure LogWarn(const Msg: string);  begin end;
procedure LogError(const Msg: string); begin end;
procedure LogDebugFmt(const Fmt: string; const Args: array of const); begin end;
procedure LogInfoFmt(const Fmt: string; const Args: array of const);  begin end;
procedure LogWarnFmt(const Fmt: string; const Args: array of const);  begin end;
procedure LogErrorFmt(const Fmt: string; const Args: array of const); begin end;
{$ENDIF}

end.
