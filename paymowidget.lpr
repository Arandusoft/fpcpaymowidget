program paymowidget;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  umain,
  upaymo,
  ulogin,
  uresourcestring,
  utasklist, utimeentry;

{$R *.res}

begin
  Application.Title := 'FPC Paymo Widget';
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTimeEntry, frmTimeEntry);
  Application.Run;
end.
