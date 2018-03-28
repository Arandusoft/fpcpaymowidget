unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, upaymo, fpjson, uresourcestring, Types, utasklist;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Tasks: TTaskList;
  public
    Paymo: TPaymo;
    procedure Login;
    procedure ListProjects();
    procedure ListTasks();
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
  DoubleBuffered := True;
  Paymo := TPaymo.Create;
  Paymo.LoadSettings;
  Login;
  if Paymo.LoggedIn then
  begin
    Paymo.GetMe();
    //ShowMessage(Paymo.MyData.FormatJSON());
    Paymo.GetTasks();
    Paymo.GetTaskLists();
    Paymo.GetRunningTimer();

    //Paymo.CreateTask('"Hola" Mundo', 'From @FPC', Paymo.TaskListsArray[0].GetPath('id').AsInteger);
    //ShowMessage(Paymo.MyData.FindPath('id').AsString);
    //ShowMessage(Paymo.RunningTimerData.FormatJSON());
    Tasks := TTaskList.Create(Self);
    Tasks.PaymoInstance := Paymo;
    Tasks.Parent := Self;
    Tasks.Align := alClient;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Paymo.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  ListTasks();
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
    prTRYAGAIN: ShowMessage(rsTooManyRequestsTryAgainSoon);
    // login error
    prERROR:
    begin
      ShowMessage(rsErrorCantLoginTryGeneratingANewAPIKey);
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

procedure TfrmMain.ListProjects();
begin

end;

procedure TfrmMain.ListTasks();
begin
  Tasks.RefreshItems;
end;

end.
