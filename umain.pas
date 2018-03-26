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
    procedure ListProjects(ListBox: TListBox);
    procedure ListTasks(ListBox: TListBox);
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
  if Paymo.LoggedIn then
  begin
    Paymo.GetTasks();
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Paymo.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ListTasks(ListBox1);
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

procedure TfrmMain.ListProjects(ListBox: TListBox);
var
  i: integer;
  arr: TJSONArray;
begin
  ListBox.Clear;
  arr := Paymo.ProjectsArray;
  for i:=0 to arr.Count-1 do
  begin
    ListBox.AddItem(arr[i].GetPath('name').AsString, nil);
  end;
end;

procedure TfrmMain.ListTasks(ListBox: TListBox);
var
  i: integer;
  arr: TJSONArray;
begin
  ListBox.Clear;
  arr := Paymo.TasksArray;
  for i:=0 to arr.Count-1 do
  begin
    ListBox.AddItem(arr[i].GetPath('name').AsString, nil);
  end;
end;

end.
