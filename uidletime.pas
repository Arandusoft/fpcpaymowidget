unit uidletime;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorSpeedButton;

type

  { TfrmIdleTime }

  TfrmIdleTime = class(TForm)
    btnKeepIdleTime: TColorSpeedButton;
    btnDiscardIdleTime: TColorSpeedButton;
    lblDescription1: TLabel;
    lblDescription2: TLabel;
    procedure btnDiscardIdleTimeClick(Sender: TObject);
    procedure btnKeepIdleTimeClick(Sender: TObject);
  private

  public

  end;

var
  frmIdleTime: TfrmIdleTime;

implementation

{$R *.lfm}

{ TfrmIdleTime }

procedure TfrmIdleTime.btnKeepIdleTimeClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmIdleTime.btnDiscardIdleTimeClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.

