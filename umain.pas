unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Menus, upaymo, fpjson, uresourcestring, Types, utasklist,
  AnimatedPanel, ColorSpeedButton, DefaultTranslator, LCLIntF, wcthread,
  LMessages;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAbout: TColorSpeedButton;
    btnMenu: TColorSpeedButton;
    btnOpenPaymoApp: TColorSpeedButton;
    btnMenuExit: TColorSpeedButton;
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
    DownloadCompany: TTask;
    DownloadProjects: TTask;
    DownloadTasks: TTask;
    DownloadTaskLists: TTask;
    DownloadRunningTimer: TTask;
    tiTray: TTrayIcon;
    wcThreadDownloader: TWCThread;
    procedure btnMenuClick(Sender: TObject);
    procedure btnMenuPaint(Sender: TObject);
    procedure btnOpenPaymoAppClick(Sender: TObject);
    procedure btnOpenPaymoAppMouseEnter(Sender: TObject);
    procedure btnOpenPaymoAppMouseLeave(Sender: TObject);
    procedure DownloadCompanyExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadMeExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadProjectsExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadRunningTimerExecute(const Sender: TTask; const Msg: Word;
      var Param: Variant);
    procedure DownloadTasksExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure DownloadTaskListsExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure tiTrayClick(Sender: TObject);
    procedure hideMenu(Sender: TObject);
    procedure wcThreadDownloaderAllTasksFinished(const Sender: TWCthread);
  private
    Tasks: TTaskList;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
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
  utimeentry, ulogin;

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
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  {$IFDEF LINUX}
  Application.Minimize;
  {$ELSE}
  Self.Hide;
  Self.ShowInTaskBar := stNever;
  {$ENDIF}
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
  c.Canvas.Line(0, c.Height - 1, c.Width, c.Height - 1);
end;

procedure TfrmMain.btnMenuClick(Sender: TObject);
begin
  pnlMenu.Visible := True;
  pnlMenu.Width := 0;
  pnlMenu.Height := Height;
  pnlMenu.Animate();
  // change style to disabled
  pnlTop.Enabled := False;
  if Assigned(Tasks) then
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

procedure TfrmMain.DownloadCompanyExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  Paymo.GetCompany();
end;

procedure TfrmMain.DownloadMeExecute(const Sender: TTask; const Msg: word;
  var Param: variant);
begin
  Paymo.GetMe();
end;

procedure TfrmMain.DownloadProjectsExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  Paymo.GetProjects();
end;

procedure TfrmMain.DownloadRunningTimerExecute(const Sender: TTask;
  const Msg: Word; var Param: Variant);
begin
  Paymo.GetRunningTimer();
end;

procedure TfrmMain.DownloadTasksExecute(const Sender: TTask; const Msg: word;
  var Param: variant);
begin
  Paymo.GetTasks();
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
  wcThreadDownloader.FinishAllTasks();
  if Assigned(Paymo) then
    Paymo.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlMenu.Height := Height;
  frmTimeEntry.Height := frmMain.Height;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  //frmTimeEntry.Show;
  tiTray.Show;
  {$IFDEF WINDOWS}
  tiTray.Icons := ilTrayNormalWin;
  tiTray.Animate := True;
  {$ENDIF}
  {$IFDEF DARWIN}
  tiTray.Icons := ilTrayNormalMac;
  tiTray.Animate := True;
  {$ENDIF}
  if Paymo.LoggedIn then
  begin
    DownloadCompany.Start;
    DownloadProjects.Start;
    DownloadTasks.Start;
    DownloadTaskLists.Start;
    DownloadRunningTimer.Start;
  end;
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

procedure TfrmMain.DownloadTaskListsExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  Paymo.GetTaskLists();
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
  if Assigned(Tasks) then
    Tasks.Enabled := True;
end;

procedure TfrmMain.wcThreadDownloaderAllTasksFinished(const Sender: TWCthread);
begin
  if not Assigned(Tasks) then
  begin
    pnlMenuUser.Caption := Paymo.MyData.GetPath('name').AsString;
    pnlMenuCompany.Caption := Paymo.CompanyData.GetPath('name').AsString;
    btnMenu.Enabled := True;
    Tasks := TTaskList.Create(Self);
    Tasks.PaymoInstance := Paymo;
    Tasks.Align := alClient;
    ListTasks();
    Tasks.Parent := Self;
  end;
end;

procedure TfrmMain.WMMove(var Message: TLMMove);
var
  l, t: integer;
begin
  inherited WMMove(Message);
  l := Self.Left - frmTimeEntry.Width;
  t  := Self.Top;
  if frmTimeEntry.Left <> l then
    frmTimeEntry.Left := l;
  if frmTimeEntry.Top <> t then
    frmTimeEntry.Top := t;
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
