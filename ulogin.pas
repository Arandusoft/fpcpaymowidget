unit ulogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLIntF, upaymo, uresourcestring;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnLogin: TButton;
    lblGenerateAPIKey: TLabel;
    edtAPIKey: TLabeledEdit;
    procedure btnLoginClick(Sender: TObject);
    procedure lblGenerateAPIKeyClick(Sender: TObject);
  private

  public

  end;

var
  frmLogin: TfrmLogin;

implementation
uses
  umain;

{$R *.lfm}

{ TfrmLogin }

procedure TfrmLogin.lblGenerateAPIKeyClick(Sender: TObject);
begin
  OpenURL(PAYMOAPIKEYURL);
end;

procedure TfrmLogin.btnLoginClick(Sender: TObject);
begin
  frmMain.Paymo.APIKey := edtAPIKey.Text;
  case frmMain.Paymo.Login of
    prOK: ModalResult := mrOK;
    prTRYAGAIN: ShowMessage(rsTooManyRequestsTryAgainSoon);
    prERROR: ShowMessage(rsErrorCantLogin);
  end;
end;

end.

