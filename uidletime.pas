unit uidletime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorSpeedButton, DateUtils, utasklist;

type

  { TfrmIdleTime }

  TfrmIdleTime = class(TForm)
    btnKeepIdleTime: TColorSpeedButton;
    btnDiscardIdleTime: TColorSpeedButton;
    lblDescription1: TLabel;
    lblDescription2: TLabel;
    procedure btnDiscardIdleTimeClick(Sender: TObject);
    procedure btnKeepIdleTimeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    minutes, seconds: integer;
    first_idle: TDateTime;
    form_loaded: boolean;
    procedure UpdateDisplay;
  end;

var
  frmIdleTime: TfrmIdleTime;

implementation
uses
  umain;

{$R *.lfm}

{ TfrmIdleTime }

procedure TfrmIdleTime.btnKeepIdleTimeClick(Sender: TObject);
begin
  form_loaded := False;
  ModalResult := mrCancel;
end;

procedure TfrmIdleTime.FormShow(Sender: TObject);
begin
  form_loaded := True;
  first_idle := IncMinute(now, -IDLETIMECHECK);
  UpdateDisplay;
end;

procedure TfrmIdleTime.UpdateDisplay;
begin
  try
    lblDescription2.Caption := TTaskList.SecondsToHHMMSS(SecondsBetween(now, first_idle));
  except
    //lblDescription2.Caption := 'ERROR!';
  end;
end;

procedure TfrmIdleTime.btnDiscardIdleTimeClick(Sender: TObject);
begin
  form_loaded := False;
  ModalResult := mrOK;
end;

end.

