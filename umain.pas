unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, upaymo, fpjson, uresourcestring, utasklist, AnimatedPanel,
  ColorSpeedButton, DefaultTranslator, LCLIntF, wcthread, LMessages,
  JSONPropStorage, XMLPropStorage, IDEWindowIntf, DateUtils, BGRABitmap,
  BGRABitmapTypes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAbout: TColorSpeedButton;
    btnMenu: TColorSpeedButton;
    btnAddTask: TColorSpeedButton;
    btnOpenPaymoApp: TColorSpeedButton;
    btnMenuExit: TColorSpeedButton;
    btnQuit: TColorSpeedButton;
    ilTrayAnimWin: TImageList;
    ilTrayAnimMac: TImageList;
    ilTrayOfflineMac: TImageList;
    ilTrayNormalWin: TImageList;
    ilTrayNormalMac: TImageList;
    ilTrayOfflineWin: TImageList;
    ilApplication: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    lblTime: TLabel;
    lblStop: TLabel;
    lblProject: TLabel;
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
    pnlTime: TPanel;
    lblTask: TLabel;
    timerEntry: TTimer;
    tiTray: TTrayIcon;
    wcThreadDownloader: TWCThread;
    procedure btnAddTaskClick(Sender: TObject);
    procedure btnAddTaskPaint(Sender: TObject);
    procedure btnMenuClick(Sender: TObject);
    procedure btnMenuPaint(Sender: TObject);
    procedure btnOpenPaymoAppClick(Sender: TObject);
    procedure btnOpenPaymoAppMouseEnter(Sender: TObject);
    procedure btnOpenPaymoAppMouseLeave(Sender: TObject);
    procedure DownloadCompanyExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadCompanyFinish(const Sender: TTask; const Msg: word;
      const Param: variant);
    procedure DownloadMeExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadProjectsExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadProjectsFinish(const Sender: TTask; const Msg: word;
      const Param: variant);
    procedure DownloadRunningTimerExecute(const Sender: TTask;
      const Msg: word; var Param: variant);
    procedure DownloadRunningTimerFinish(const Sender: TTask;
      const Msg: word; const Param: variant);
    procedure DownloadTaskListsFinish(const Sender: TTask; const Msg: word;
      const Param: variant);
    procedure DownloadTasksExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure DownloadTasksFinish(const Sender: TTask; const Msg: word;
      const Param: variant);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblStopClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure DownloadTaskListsExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure timerEntryTimer(Sender: TObject);
    procedure tiTrayClick(Sender: TObject);
    procedure hideMenu(Sender: TObject);
  private
    Tasks: TTaskList;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
  public
    Paymo: TPaymo;
    start_time: TDateTime;
    stop_ok: boolean;
    IconIndex: integer;
    procedure Login;
    procedure ListProjects();
    procedure ListTasks();
    procedure ListTimeEntry();
    procedure ChangeIcon(const AIndex: integer);
    procedure RefreshTimeEntry();
    function StopTimeEntry(): boolean;
    procedure SetFonts(Control: TControl);
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
  SetFonts(Self);
  pnlMenu.AnimWidth := ScaleX(pnlMenu.AnimWidth, 96);
  DoubleBuffered := True;
  pnlMenu.Left := 0;
  pnlMenu.Top := 0;
  Paymo := TPaymo.Create;
  Paymo.LoadSettings;
  Login;
  if Paymo.LoggedIn then
  begin
    try
      DownloadCompany.Start;
      DownloadProjects.Start;
      DownloadTasks.Start;
      DownloadRunningTimer.Start;
      DownloadTaskLists.Start;
    except
    end;
  end;
  // Default position (center of the screen)
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  // Restore position (only works with Position = poDesigned)
  if ForceDirectories(GetAppConfigDir(False)) then
  begin
    JSONPropStorage1.JSONFileName := GetAppConfigDir(False) + 'settings.json';
    JSONPropStorage1.Restore;
  end;
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
  pnlMenu.BringToFront;
  pnlMenu.Visible := True;
  pnlMenu.Width := 0;
  pnlMenu.Height := Height;
  pnlMenu.Animate();
  // change style to disabled
  pnlTop.Enabled := False;
  pnlTime.Enabled := False;
  if Assigned(Tasks) then
    Tasks.Enabled := False;
end;

procedure TfrmMain.btnAddTaskPaint(Sender: TObject);
var
  c: TColorSpeedButton;
  h: integer;
begin
  c := TColorSpeedButton(Sender);
  h := c.Height div 2;
  c.Canvas.Pen.Color := clWhite;
  c.Canvas.Line(h, 0, h, c.Height);
  c.Canvas.Line(0, h, c.Width, h);
end;

procedure TfrmMain.btnAddTaskClick(Sender: TObject);
begin
  frmTimeEntry.PaymoInstance := Paymo;
  frmTimeEntry.Data := nil;
  frmTimeEntry.ShowData;
  frmTimeEntry.Show;
  frmTimeEntry.editSearchProject.Clear;
  frmTimeEntry.editSearchProject.SetFocus;
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
  if not Sender.Terminated then
    Paymo.GetCompany();
end;

procedure TfrmMain.DownloadCompanyFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  pnlMenuUser.Caption := Paymo.MyData.GetPath('name').AsString;
  pnlMenuCompany.Caption := Paymo.CompanyData.GetPath('name').AsString;
end;

procedure TfrmMain.DownloadMeExecute(const Sender: TTask; const Msg: word;
  var Param: variant);
begin
  Paymo.GetMe();
end;

procedure TfrmMain.DownloadProjectsExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  if not Sender.Terminated then
    Paymo.GetProjects();
end;

procedure TfrmMain.DownloadProjectsFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  //ShowMessage('Projects OK');
end;

procedure TfrmMain.DownloadRunningTimerExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  if not Sender.Terminated then
    Paymo.GetRunningTimer();
end;

procedure TfrmMain.DownloadRunningTimerFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  ListTimeEntry();
end;

procedure TfrmMain.DownloadTaskListsFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  //ShowMessage('TaskLists OK');
end;

procedure TfrmMain.DownloadTasksExecute(const Sender: TTask; const Msg: word;
  var Param: variant);
begin
  if not Sender.Terminated then
    Paymo.GetTasks();
end;

procedure TfrmMain.DownloadTasksFinish(const Sender: TTask; const Msg: word;
  const Param: variant);
begin
  if not Assigned(Tasks) then
  begin
    Tasks := TTaskList.Create(Self);
    Tasks.PaymoInstance := Paymo;
    btnMenu.Enabled := True;
    btnAddTask.Enabled := True;
    Tasks.Align := alClient;
    ListTasks();
    Tasks.Parent := Self;
  end
  else
    ListTasks();
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
end;

procedure TfrmMain.lblStopClick(Sender: TObject);
begin
  if (Paymo.RunningTimerData <> nil) then
  begin
    case Paymo.StopRunningTimer(start_time, now, '') of
      prOK:
      begin
        stop_ok := True;
        pnlTime.Visible := False;
        Application.ProcessMessages;
        // Sync for now, ToDo: change to async with tasks
        Paymo.GetRunningTimer();
        DownloadRunningTimerFinish(nil, 0, 0);
        Application.ProcessMessages;
        // Sync for now, ToDo: change to async with tasks
        Paymo.GetTasks();
        DownloadTasksFinish(nil, 0, 0);
      end;
      prTRYAGAIN, prERROR:
      begin
        stop_ok := False;
        ShowMessage(rsErrorCantStopTimer);
      end;
    end;
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
  if not Sender.Terminated then
    Paymo.GetTaskLists();
end;

procedure TfrmMain.timerEntryTimer(Sender: TObject);
begin
  RefreshTimeEntry();
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
  pnlTime.Enabled := True;
  if Assigned(Tasks) then
    Tasks.Enabled := True;
end;

procedure TfrmMain.WMMove(var Message: TLMMove);
var
  l, t: integer;
begin
  inherited WMMove(Message);
  {$IFDEF LINUX}
  // does not works on linux
  {$ELSE}
  l := Self.Left - frmTimeEntry.Width;
  t := Self.Top;
  if frmTimeEntry.Left <> l then
    frmTimeEntry.Left := l;
  if frmTimeEntry.Top <> t then
    frmTimeEntry.Top := t;
  {$ENDIF}
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
  Tasks.Visible := False;
  Tasks.RefreshItems;
  //SetFonts(Tasks);
  Tasks.Visible := True;
end;

procedure TfrmMain.ListTimeEntry();
begin
  if (Paymo.RunningTimerData <> nil) then
  begin
    lblProject.Caption := Paymo.GetProjectName(
      Paymo.RunningTimerData.GetPath('project_id').AsInteger);
    lblTask.Caption := Paymo.GetTaskName(Paymo.RunningTimerData.GetPath(
      'task_id').AsInteger);
    start_time := TTaskList.StringToDateTime(
      Paymo.RunningTimerData.GetPath('start_time').AsString);
    pnlTime.Visible := True;
  end
  else
    pnlTime.Visible := False;
end;

procedure TfrmMain.ChangeIcon(const AIndex: integer);   //0: normal //1:start //2:offline
var
  Image1: TImage;
  iIndex: integer;
  //bmp: TBGRABitmap;
begin
  iIndex := AIndex;
  if IconIndex = 1 then
    iIndex := 3;//another start icon
  if IconIndex = 3 then
    iIndex := 1;//another start icon
  if AIndex = 0 then
    iIndex := 0;
  if AIndex = 2 then
    iIndex := 2;
  if IconIndex = iIndex then
    exit;
  try
    IconIndex := iIndex;
    Image1 := TImage.Create(self);
    case iIndex of
      0:
      begin
        ilApplication.GetBitmap(0, Image1.Picture.BitMap);
        tiTray.Icons := ilTrayNormalWin;
        IconIndex := 0;
      end;
      1:
      begin
        ilApplication.GetBitmap(1, Image1.Picture.BitMap);
        //Image1.Canvas.Font.Color := clBlack;
        //Image1.Canvas.Font.Size := 20;
        //Image1.Canvas.Brush.Style := bsClear;
        //Image1.Canvas.TextOut(20, 20, 'HELLO WORLD');
        tiTray.Icons := ilTrayAnimWin;
      end;
      2:
      begin
        ilApplication.GetBitmap(2, Image1.Picture.BitMap);
        tiTray.Icons := ilTrayOfflineWin;
        IconIndex := 2;
      end;
      3:
      begin
        ilApplication.GetBitmap(3, Image1.Picture.BitMap);
        //Image1.Canvas.Font.Color := clBlack;
        //Image1.Canvas.Font.Size := 20;
        //Image1.Canvas.Brush.Style := bsClear;
        //Image1.Canvas.TextOut(20, 20, 'HELLO WORLD');
      end;
      else
      begin
        ilTrayNormalWin.GetBitmap(0, Image1.Picture.BitMap);
        tiTray.Icons := ilTrayNormalWin;
      end;
    end;
    {bmp := TBGRABitmap.Create();
    bmp.Assign(Image1.Picture.Graphic);
    bmp.FontHeight := 11;
    bmp.FontName := FONTNAMEARROW;
    bmp.Rectangle(0, bmp.Height-15, bmp.Width, bmp.Height, BGRABlack, BGRABlack, dmSet);
    bmp.TextOut(0, bmp.Height-15, TTaskList.SecondsToHHMMSS(SecondsBetween(start_time, now)), BGRAWhite);
    Application.Icon.Assign(bmp.Bitmap);}
    Application.Icon.Assign(Image1.Picture.Graphic);
  finally
    //bmp.Free;
    Image1.Free;
  end;
end;

procedure TfrmMain.RefreshTimeEntry();
begin
  if (Paymo.RunningTimerData <> nil) then
  begin
    lblTime.Caption := TTaskList.SecondsToHHMMSS(SecondsBetween(start_time, now));
    Application.Title := lblTime.Caption + ' - ' + 'FPC Paymo Widget';
    {$IFDEF DARWIN}
    // tray icon hint doesn't works
    {$ELSE}
    tiTray.Hint := lblTime.Caption + ' - ' + 'FPC Paymo Widget';
    {$ENDIF}
    ChangeIcon(1);
  end
  else
  begin
    ChangeIcon(0);
    Application.Title := 'FPC Paymo Widget';
    {$IFDEF DARWIN}
    // tray icon hint doesn't works
    {$ELSE}
    tiTray.Hint := 'FPC Paymo Widget';
    {$ENDIF}
  end;
end;

function TfrmMain.StopTimeEntry(): boolean;
begin
  Result := True;
  lblStopClick(nil);
  Result := stop_ok;
end;

procedure TfrmMain.SetFonts(Control: TControl);
var
  n: integer;
  WinControl: TWinControl;
begin
  // Font Size
  case Control.Tag of
    -12:
    begin
      Control.Font.Size := FONTSIZESMALL;
    end;
    -14:
    begin
      Control.Font.Size := FONTSIZEMEDIUM;
    end;
    -16:
    begin
      Control.Font.Size := FONTSIZEBIG;
    end;
    -23:
    begin
      Control.Font.Size := FONTSIZEBIG2;
    end;
    -24:
    begin
      Control.Font.Size := FONTSIZESTOP;
    end;
    -30:
    begin
      Control.Font.Size := FONTSIZEBACK;
    end;
    -40:
    begin
      Control.Font.Size := FONTSIZETIME;
    end;
    0:
    begin
      Control.Font.Size := FONTSIZEMEDIUM;
    end;
    {else
      Control.Font.Size := FONTSIZEMEDIUM;}
  end;

  // Font Name
  case Control.Font.Name of
    'Nunito Sans':
    begin
      Control.Font.Name := FONTNAME;
    end;
    'Nunito Sans Light':
    begin
      Control.Font.Name := FONTNAMELIGHT2;
    end;
    'Nunito Sans ExtraLight':
    begin
      Control.Font.Name := FONTNAMELIGHT;
    end;
    'Nunito Sans ExtraBold':
    begin
      Control.Font.Name := FONTNAMEBOLD;
    end;
    'Courier New':
    begin
      Control.Font.Name := FONTNAMEFIXED;
    end;
    'default':
    begin
      Control.Font.Name := FONTNAME;
    end;
    else
      Control.Font.Name := FONTNAME;
  end;

  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount > 0 then
    begin
      for n := 0 to WinControl.ControlCount - 1 do
      begin
        if WinControl.Controls[n] is TControl then
        begin
          SetFonts(WinControl.Controls[n]);
        end;
      end;
    end;
  end;
end;

end.
