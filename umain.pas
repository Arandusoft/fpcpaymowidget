unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  upaymo, fpjson;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    Paymo: TPaymo;
    procedure Login;
    procedure ListProjects;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ulogin;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Paymo := TPaymo.Create;
  Paymo.LoadSettings;
  Login;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Paymo.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ListProjects;
end;

procedure TfrmMain.Login;
begin
  // There is no api key
  if Paymo.APIKEY = '' then
  begin
    try
      frmLogin := TfrmLogin.Create(nil);
      case frmLogin.ShowModal of
        mrOk:
        begin
          Paymo.SaveSettings;
        end;
        mrCancel:
        begin
          Application.Terminate;
        end;
      end;
    finally
      frmLogin.Free;
    end;
  end;

  case Paymo.Login of
    // api limit error
    prTRYAGAIN: ShowMessage('Too many Requests. Try again soon.');
    // login error
    prERROR:
    begin
      ShowMessage('Error: Can''t login. Try generating a new API Key.');
      try
        frmLogin := TfrmLogin.Create(nil);
        case frmLogin.ShowModal of
          mrOk:
          begin
            Paymo.SaveSettings;
          end;
          mrCancel:
          begin
            Application.Terminate;
          end;
        end;
      finally
        frmLogin.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.ListProjects;
var
  i: integer;
  arr: TJSONArray;
begin
  arr := Paymo.ProjectsArray;
  for i:=0 to arr.Count-1 do
  begin
    ListBox1.AddItem(arr[i].GetPath('name').AsString, nil);
  end;
end;

end.
