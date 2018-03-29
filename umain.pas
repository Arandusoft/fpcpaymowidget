unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Menus, upaymo, fpjson, uresourcestring, Types, utasklist,
  AnimatedPanel, ColorSpeedButton, DefaultTranslator, LCLIntF;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAbout: TColorSpeedButton;
    btnMenu: TColorSpeedButton;
    btnOpenPaymoApp: TColorSpeedButton;
    btnQuit: TColorSpeedButton;
    ilTrayAnimWin: TImageList;
    ilTrayAnimMac: TImageList;
    ilTrayOfflineMac: TImageList;
    ilTrayNormalWin: TImageList;
    ilTrayNormalMac: TImageList;
    ilTrayOfflineWin: TImageList;
    miShow: TMenuItem;
    miAbout: TMenuItem;
    miQuit: TMenuItem;
    pnlMenu: TAnimatedPanel;
    pnlMenuCompany: TLabel;
    pnlMenuUser: TLabel;
    pnlSpacer1: TPanel;
    pnlSpacer2: TPanel;
    pnlTop: TPanel;
    pmTray: TPopupMenu;
    tiTray: TTrayIcon;
    procedure btnMenuClick(Sender: TObject);
    procedure btnMenuPaint(Sender: TObject);
    procedure btnOpenPaymoAppClick(Sender: TObject);
    procedure btnOpenPaymoAppMouseEnter(Sender: TObject);
    procedure btnOpenPaymoAppMouseLeave(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure tiTrayClick(Sender: TObject);
    procedure hideMenu(Sender: TObject);
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
  pnlMenu.Left := 0;
  pnlMenu.Top := 0;
  Paymo := TPaymo.Create;
  Paymo.LoadSettings;
  Login;
  if Paymo.LoggedIn then
  begin
    Paymo.GetMe();
    Paymo.GetCompany();
    pnlMenuUser.Caption := Paymo.MyData.GetPath('name').AsString;
    //ShowMessage(Paymo.CompanyData.FormatJSON());
    pnlMenuCompany.Caption := Paymo.CompanyData.GetPath('name').AsString;

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

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Sender is TTrayIcon then
    Exit;
  CanClose := False;
  Self.Hide;
  Self.ShowInTaskBar := stNever;
end;

procedure TfrmMain.btnMenuPaint(Sender: TObject);
var
  c: TColorSpeedButton;
  h: integer;
begin
  c := TColorSpeedButton(Sender);
  h := c.Height div 2;
  c.Canvas.Pen.Color := clWhite;
  c.Canvas.Line(0, 0, c.Width, 0);
  c.Canvas.Line(0, h, c.Width, h);
  c.Canvas.Line(0, c.Height-1, c.Width, c.Height-1);
end;

procedure TfrmMain.btnMenuClick(Sender: TObject);
begin
  pnlMenu.Visible := True;
  pnlMenu.Width := 0;
  pnlMenu.Animate();
  // change style to disabled
  pnlTop.Enabled := False;
  Tasks.Enabled := False;
end;

procedure TfrmMain.btnOpenPaymoAppClick(Sender: TObject);
begin
  OpenURL('https://app.paymoapp.com/');
end;

procedure TfrmMain.btnOpenPaymoAppMouseEnter(Sender: TObject);
begin
  TControl(Sender).Font.Color := RGBToColor(32, 201, 103);
end;

procedure TfrmMain.btnOpenPaymoAppMouseLeave(Sender: TObject);
begin
  TControl(Sender).Font.Color := RGBToColor(135, 143, 156);
end;

procedure TfrmMain.FormClick(Sender: TObject);
begin
  hideMenu(Self);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  tiTray.Hide;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Paymo.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlMenu.Height := Height;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  tiTray.Show;
  {$IFDEF WINDOWS}
  tiTray.Icons := ilTrayNormalWin;
  tiTray.Animate := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  tiTray.Icons := ilTrayNormalMac;
  tiTray.Animate := True;
  {$ENDIF}
  ListTasks();
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  ShowMessage('Copyright © 2018 Arandú Software');
end;

procedure TfrmMain.miQuitClick(Sender: TObject);
begin
  Self.OnCloseQuery := nil;
  Close;
end;

procedure TfrmMain.tiTrayClick(Sender: TObject);
begin
  Self.ShowInTaskBar := stDefault;
  Self.Show;
end;

procedure TfrmMain.hideMenu(Sender: TObject);
begin
  pnlMenu.Animate();
  // change style back to enabled
  pnlTop.Enabled := True;
  Tasks.Enabled := True;
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
