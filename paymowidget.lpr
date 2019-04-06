program paymowidget;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  {$IFNDEF DEBUG}
  {$IFNDEF DARWIN}
  cmem,
  {$ENDIF}
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  opensslsockets,
  umain,
  upaymo,
  ulogin,
  uresourcestring,
  utasklist, utimeentry, udebug, ulimitedstringlist, uuserlist, uidletime;

{$R *.res}

begin
  Application.Title := 'FPC Paymo Widget';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTimeEntry, frmTimeEntry);
  udebug.InitLog();
  Application.CreateForm(TfrmUserList, frmUserList);
  Application.CreateForm(TfrmIdleTime, frmIdleTime);
  Application.Run;
end.
