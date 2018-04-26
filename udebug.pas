unit udebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, uLimitedStringList, Dialogs;

procedure DebugLog(Usuario: string; API: string; Mensaje: string);
procedure DebugLog(Usuario: string; API: string; OK: boolean);
procedure InitLog;

implementation

var
  LastDebugTime: TDateTime;
  DebugFile: TLimitedStringList;
  DebugFileName: string;

const
  LIMITE = 500;

procedure DebugLog(Usuario: string; API: string; Mensaje: string);
var
  msBetween: int64;
  msNow: TDateTime;
begin
  if not Assigned(DebugFile) then exit();
  try
    msNow := Now;
    msBetween := MilliSecondsBetween(LastDebugTime, msNow);
    DebugFile.Append(Usuario + ' ' + DateTimeToStr(msNow) + ' ' +
      IntToStr(msBetween) + ' ' + API + ' ' + Mensaje);
    LastDebugTime := msNow;
    DebugFile.SaveToFile(DebugFileName);
  except
    on E: exception do
    begin
      // no hacer nada, no se pudo guardar el log
    end;
  end;
end;

procedure DebugLog(Usuario: string; API: string; OK: boolean);
begin
  if (OK) then
    DebugLog(Usuario, API, 'OK')
  else
    DebugLog(Usuario, API, 'ERROR');
end;

procedure InitLog;
begin
  DebugFile := TLimitedStringList.Create(LIMITE);
  try
    LastDebugTime := Now;
    DebugFileName := GetAppConfigDir(False) + 'debug.log';
    if FileExists(DebugFileName) then
      DebugFile.LoadFromFile(DebugFileName);
    DebugLog('FPC Paymo Widget', 'DEBUG_INIT', True);
  except
    on E: exception do
    begin
      // liberar DebugFile, los logs siguientes no se guardarán
      if Assigned(DebugFile) then
        DebugFile.Free;
    end;
  end;
end;

procedure EndLog;
begin
  DebugLog('FPC Paymo Widget', 'DEBUG_END', True);
  if Assigned(DebugFile) then
    DebugFile.Free;
end;

finalization
  EndLog();

end.
