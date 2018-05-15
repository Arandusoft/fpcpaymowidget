unit umain;

{$mode objfpc}{$H+}
{ $define debugoffline}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, upaymo, fpjson, uresourcestring, utasklist, AnimatedPanel,
  ColorSpeedButton, DefaultTranslator, LCLIntF, wcthread, LMessages,
  JSONPropStorage, IDEWindowIntf, DateUtils, BGRABitmap,
  BGRABitmapTypes, PropertyStorage, ComCtrls, Spin, LazUTF8, LCLType, udebug
  , fileinfo
  , winpeimagereader {need this for reading exe info}
  , elfreader {needed for reading ELF executables}
  , machoreader {needed for reading MACH-O executables}  ;

const
  IDLETIMECHECK = 10; // seconds

{$ifdef win32}
type
  PLASTINPUTINFO = ^LASTINPUTINFO;
  tagLASTINPUTINFO = record
    cbSize: UINT;
    dwTime: DWORD;
  end;
  LASTINPUTINFO = tagLASTINPUTINFO;
  TLastInputInfo = LASTINPUTINFO;

 function GetLastInputInfo(var plii: TLastInputInfo): BOOL;stdcall; external 'user32' name 'GetLastInputInfo';
 {$endif}

type
  { TfrmMain }

  TfrmMain = class(TForm)
    btnSettings: TColorSpeedButton;
    btnAbout: TColorSpeedButton;
    btnMenu: TColorSpeedButton;
    btnAddTask: TColorSpeedButton;
    btnRefresh: TColorSpeedButton;
    btnViewUsers: TColorSpeedButton;
    btnSettingsExit: TColorSpeedButton;
    btnOpenPaymoApp: TColorSpeedButton;
    btnMenuExit: TColorSpeedButton;
    btnQuit: TColorSpeedButton;
    btnReset: TButton;
    btnOpenSettingsFolder: TButton;
    cbShowTimeInAppIcon: TCheckBox;
    edSearch: TEdit;
    ilTrayAnimWin: TImageList;
    ilTrayAnimMac: TImageList;
    ilTrayOfflineMac: TImageList;
    ilTrayNormalWin: TImageList;
    ilTrayNormalMac: TImageList;
    ilTrayOfflineWin: TImageList;
    ilApplication: TImageList;
    JSONPropStorage1: TJSONPropStorage;
    lblIdle: TLabel;
    lblProject: TLabel;
    lblRefreshInterval: TLabel;
    lblStop: TLabel;
    lblTask: TLabel;
    lblTime: TLabel;
    leAPIURL: TLabeledEdit;
    miViewUsers: TMenuItem;
    miOpenPaymoApp: TMenuItem;
    miRefresh: TMenuItem;
    miSettings: TMenuItem;
    miShow: TMenuItem;
    miAbout: TMenuItem;
    miQuit: TMenuItem;
    pbRefresh: TProgressBar;
    pnlSettings: TPanel;
    pnlMenu: TAnimatedPanel;
    pnlMenuCompany: TLabel;
    pnlMenuUser: TLabel;
    pnlSpacer1: TPanel;
    pnlSpacer2: TPanel;
    pnlTime: TPanel;
    pnlTop: TPanel;
    pmTray: TPopupMenu;
    DownloadCompany: TTask;
    DownloadProjects: TTask;
    DownloadTasks: TTask;
    DownloadTaskLists: TTask;
    DownloadRunningTimer: TTask;
    seRefreshInterval: TSpinEdit;
    TabControl1: TTabControl;
    tmrIdle: TTimer;
    timerEntry: TTimer;
    timerRefresh: TTimer;
    tiTray: TTrayIcon;
    wcThreadDownloader: TWCThread;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnAddTaskClick(Sender: TObject);
    procedure btnAddTaskPaint(Sender: TObject);
    procedure btnMenuClick(Sender: TObject);
    procedure btnMenuPaint(Sender: TObject);
    procedure btnOpenPaymoAppClick(Sender: TObject);
    procedure btnOpenPaymoAppMouseEnter(Sender: TObject);
    procedure btnOpenPaymoAppMouseLeave(Sender: TObject);
    procedure btnSettingsExitClick(Sender: TObject);
    procedure btnOpenSettingsFolderClick(Sender: TObject);
    procedure btnViewUsersClick(Sender: TObject);
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
    procedure edSearchChange(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrIdleTimer(Sender: TObject);
    procedure JSONPropStorage1RestoreProperties(Sender: TObject);
    procedure lblStopClick(Sender: TObject);
    procedure leAPIURLChange(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miQuitClick(Sender: TObject);
    procedure DownloadTaskListsExecute(const Sender: TTask; const Msg: word;
      var Param: variant);
    procedure seRefreshIntervalEditingDone(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure timerEntryTimer(Sender: TObject);
    procedure timerRefreshTimer(Sender: TObject);
    procedure tiTrayClick(Sender: TObject);
    procedure hideMenu(Sender: TObject);
    procedure wcThreadDownloaderAllTasksFinished(const Sender: TWCthread);
  private
    Tasks: TTaskList;
    //procedure WMMove(var Message: TLMMove); message LM_MOVE;
  public
    Paymo: TPaymo;
    start_time: TDateTime;
    stop_ok: boolean;
    IconIndex: integer;
    firstSync: boolean;
    procedure Login;
    procedure Sync;
    procedure ListProjects();
    procedure ListTasks();
    procedure ListTimeEntry();
    procedure ChangeIcon(const AIndex: integer);
    procedure RefreshTimeEntry();
    function StopTimeEntry(): boolean;
    function StopTimer(end_time: TDateTime): boolean;
    procedure SetFonts(Control: TControl);
    procedure RefreshTabs;
    procedure AssignTask;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  utimeentry, ulogin, uuserlist, uidletime;

{$R *.lfm}

{ TfrmMain }


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  // prevent flickering
  {$ifdef windows}
  DoubleBuffered := True;
  pnlMenu.DoubleBuffered := True;
  pnlTop.DoubleBuffered := True;
  pnlSettings.DoubleBuffered := True;
  TabControl1.DoubleBuffered := True;
  {$endif}
  // To grab data from disk when offline
  firstSync := True;
  SetFonts(Self);
  pnlMenu.AnimWidth := ScaleX(pnlMenu.AnimWidth, 96);
  DoubleBuffered := True;
  pnlMenu.Left := 0;
  pnlMenu.Top := 0;
  // Default position (center of the screen)
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  // Restore position (only works with Position = poDesigned)
  {$IFNDEF DARWIN}
  ForceDirectories(GetAppConfigDir(False));
  JSONPropStorage1.JSONFileName := GetAppConfigDir(False) + 'settings.json';
  {$ENDIF}
  Paymo := TPaymo.Create;
  Paymo.SettingsFolder := GetAppConfigDir(False);
  Paymo.SettingsFile := GetAppConfigFile(False);
  {$ifdef debugoffline}
  Paymo.Offline := True;
  pnlTop.Caption := pnlTop.Caption + ' - Offline';
  {$endif}
  Paymo.LoadSettings;
  //try
  Login;
  //except
  //ShowMessage(rsErrorCantLogin);
  //Application.Terminate;
  //end;
  if Paymo.LoggedIn then
  begin
    try
      timerRefresh.Enabled := True;
      //timerRefreshTimer(self);
    except
    end;
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
  edSearch.Enabled := False;
  pnlTime.Enabled := False;
  TabControl1.Visible := False;
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
  frmTimeEntry.selected_project := 0;
  frmTimeEntry.selected_task := 0;
  frmTimeEntry.selected_tasklist := 0;
  frmTimeEntry.selected_data := 0;
  frmTimeEntry.ShowData;
  frmTimeEntry.Show;
end;

procedure TfrmMain.btnSettingsClick(Sender: TObject);
begin
  Self.ShowInTaskBar := stDefault;
  Self.Show;
  hideMenu(nil);
  pnlSettings.BringToFront;
  pnlSettings.Left := 0;
  pnlSettings.Top := 0;
  pnlSettings.Visible := True;
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  leAPIURL.Text := PAYMOAPIBASEURL;
end;

procedure TfrmMain.btnRefreshClick(Sender: TObject);
begin
  Self.ShowInTaskBar := stDefault;
  Self.Show;
  hideMenu(nil);
  frmMain.timerRefreshTimer(nil);
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

procedure TfrmMain.btnSettingsExitClick(Sender: TObject);
begin
  pnlSettings.Visible := False;
end;

procedure TfrmMain.btnOpenSettingsFolderClick(Sender: TObject);
begin
  OpenDocument(ExtractFilePath(JSONPropStorage1.JSONFileName));
end;

procedure TfrmMain.btnViewUsersClick(Sender: TObject);
begin
  uuserlist.frmUserList.Show;
end;

procedure TfrmMain.DownloadCompanyExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadCompany', 'Start');
  if not Sender.Terminated then
    Paymo.GetCompany();
end;

procedure TfrmMain.DownloadCompanyFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadCompany', 'Finish');
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
  DebugLog('FPC Paymo Widget', 'DownloadProjects', 'Start');
  if not Sender.Terminated then
    Paymo.GetProjects();
end;

procedure TfrmMain.DownloadProjectsFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadProjects', 'Finish');
  //ShowMessage('Projects OK');
end;

procedure TfrmMain.DownloadRunningTimerExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadRunningTimer', 'Start');
  if not Sender.Terminated then
    Paymo.GetRunningTimer();
end;

procedure TfrmMain.DownloadRunningTimerFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadRunningTimer', 'Finish');
  ListTimeEntry();
end;

procedure TfrmMain.DownloadTaskListsFinish(const Sender: TTask;
  const Msg: word; const Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadTaskLists', 'Finish');
  //ShowMessage('TaskLists OK');
end;

procedure TfrmMain.DownloadTasksExecute(const Sender: TTask; const Msg: word;
  var Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadTasks', 'Start');
  if not Sender.Terminated then
    Paymo.GetTasks();
end;

procedure TfrmMain.DownloadTasksFinish(const Sender: TTask; const Msg: word;
  const Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadTasks', 'Finish');
  //ShowMessage(Paymo.TasksArray.formatJSON());
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

procedure TfrmMain.edSearchChange(Sender: TObject);
var
  i, j: integer;
  comp, comp2, comp3: TComponent;
  s, search: string;
  _hide: boolean;
begin
  if not Assigned(Tasks) then
    exit;
  search := UTF8LowerCase(edSearch.Text);
  if search = '' then
  begin
    ListTasks();
    exit;
  end;
  //ShowMessage(Tasks.ComponentCount.ToString());
  for i := 0 to Tasks.ComponentCount - 1 do
  begin
    _hide := True;
    comp := nil;
    // day
    comp := Tasks.Components[i];
    for j := 0 to comp.ComponentCount - 1 do
    begin
      comp2 := nil;
      // task container
      comp2 := comp.FindComponent('taskc' + j.ToString());
      if Assigned(comp2) then
      begin
        comp3 := nil;
        // task description
        comp3 := comp2.FindComponent('taskl');
        if Assigned(comp3) then
        begin
          s := UTF8LowerCase(TLabel(comp3).Caption);
          if (UTF8Pos(search, s) <> 0) then
          begin
            _hide := False;
            TControl(comp2).Visible := True;
          end
          else
            TControl(comp2).Visible := False;
        end;
      end;
    end;
    if _hide then
      TControl(comp).Visible := False
    else
    begin
      TControl(comp).Visible := True;
      TControl(comp).AutoSize := True;
    end;
  end;
end;

procedure TfrmMain.FormClick(Sender: TObject);
begin
  hideMenu(Self);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  timerRefresh.Enabled := False;
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
  pnlSettings.Width := Width;
  pnlSettings.Height := Height;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if Paymo.MyData.GetPath('type').AsString = 'Admin' then
  begin
    miViewUsers.Visible := True;
    btnViewUsers.Visible := True;
  end;
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
  timerEntry.Enabled := True;
  timerRefresh.Enabled := True;
  timerRefreshTimer(self);
end;

procedure TfrmMain.tmrIdleTimer(Sender: TObject);
{$IFDEF WIN32}
function IdleTime: DWord;
var
  LastInput: TLastInputInfo;
begin
  LastInput.cbSize := SizeOf(TLastInputInfo);
  GetLastInputInfo(LastInput);
  Result := (GetTickCount - LastInput.dwTime) DIV 1000;
end;
var
  task_id: int64;
  iSecondsIdle: integer;
begin
  // always update display
  frmIdleTime.updateDisplay;
  try
     iSecondsIdle:=IdleTime;
  except
     iSecondsIdle:=0;
     DebugLog('FPC Paymo Widget', 'Idle detection', 'Error');
  end;
  if (iSecondsIdle>=60*IDLETIMECHECK) and (Paymo.RunningTimerData <> nil) then
  begin
    if (not frmIdleTime.IsVisible) then
    case frmIdleTime.ShowModal of
      // Discard Idle Time
      mrOK: begin
        task_id := Paymo.RunningTimerData.GetPath('task_id').AsInt64;
        StopTimer(frmIdleTime.first_idle);
        case Paymo.StartRunningTimer(task_id, now) of
          prOK, prNOInternet:
          begin
            case Paymo.GetRunningTimer() of
              prOK, prNOInternet:
              begin
                frmMain.DownloadRunningTimerFinish(nil, 0, 0);
              end;
              prTRYAGAIN, prERROR:
              begin
                ShowMessage(rsErrorCantStartTimer);
              end;
              {prNOInternet:
              begin
                ShowMessage(rsWorkingOfflineTheDataWillBeSavedTheNextTimeYouAreOnline);
              end;}
            end;
          end;
          prTRYAGAIN, prERROR:
          begin
            ShowMessage(rsErrorCantStartTimerTryStoppingCurrentTimerFirst);
          end;
          {prNOInternet:
          begin
            ShowMessage(rsWorkingOfflineTheDataWillBeSavedTheNextTimeYouAreOnline);
          end;}
        end;
      end;
    end;
  end;
end;
{$ELSE}
 Begin
   //todo: detect idle time in another plataforms /macos/linux
 End;
{$ENDIF}

procedure TfrmMain.JSONPropStorage1RestoreProperties(Sender: TObject);
begin
  if leAPIURL.Text = '' then
    leAPIURL.Text := PAYMOAPIBASEURL;
  TimerRefresh.Interval := seRefreshInterval.Value * 1000;
  Width := MulDiv(Width, 96, Screen.PixelsPerInch);
  Height := MulDiv(Height, 96, Screen.PixelsPerInch);
end;

procedure TfrmMain.lblStopClick(Sender: TObject);
begin
  StopTimer(now);
end;

procedure TfrmMain.leAPIURLChange(Sender: TObject);
begin
  Paymo.APIURL := leAPIURL.Text;
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
var
  FileVerInfo: TFileVersionInfo;
  s: string = '';
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    s :=
    //'Company: ' + FileVerInfo.VersionStrings.Values['CompanyName'] + LineEnding +
    //'File description: ' + FileVerInfo.VersionStrings.Values['FileDescription'] + LineEnding +
    'File version: ' + FileVerInfo.VersionStrings.Values['FileVersion'] + LineEnding // +
    //'Internal name: ' + FileVerInfo.VersionStrings.Values['InternalName'] + LineEnding +
    //'Legal copyright: ' + FileVerInfo.VersionStrings.Values['LegalCopyright'] + LineEnding +
    //'Original filename: ' + FileVerInfo.VersionStrings.Values['OriginalFilename'] + LineEnding +
    //'Product name: ' + FileVerInfo.VersionStrings.Values['ProductName'] + LineEnding +
    //'Product version: ' + FileVerInfo.VersionStrings.Values['ProductVersion'] + LineEnding
    ;
  finally
    FileVerInfo.Free;
  end;
  ShowMessage(s + 'Copyright © 2018 Arandú Software');
end;

procedure TfrmMain.miQuitClick(Sender: TObject);
begin
  Self.OnCloseQuery := nil;
  Close;
end;

procedure TfrmMain.DownloadTaskListsExecute(const Sender: TTask;
  const Msg: word; var Param: variant);
begin
  DebugLog('FPC Paymo Widget', 'DownloadTaskLists', 'Start');
  if not Sender.Terminated then
    Paymo.GetTaskLists();
end;

procedure TfrmMain.seRefreshIntervalEditingDone(Sender: TObject);
begin
  if (seRefreshInterval.Value >= seRefreshInterval.MinValue) and
    (seRefreshInterval.Value <= seRefreshInterval.MaxValue) then
    timerRefresh.Interval := seRefreshInterval.Value * 1000
  else
  begin
    // default value if data is invalid
    seRefreshInterval.Value := 60;
    timerRefresh.Interval := 60000;
  end;
end;

procedure TfrmMain.TabControl1Change(Sender: TObject);
begin
  AssignTask;
end;

procedure TfrmMain.timerEntryTimer(Sender: TObject);
begin
  RefreshTimeEntry();
end;

procedure TfrmMain.timerRefreshTimer(Sender: TObject);
begin
  if not Paymo.Offline or firstSync then
  begin
    DebugLog('FPC Paymo Widget', 'Refresh', 'Start');
    timerRefresh.Enabled := False;
    pbRefresh.Visible := True;
    Application.ProcessMessages;
    if Assigned(frmTimeEntry) and (frmTimeEntry.Visible) then
      frmTimeEntry.Enabled := False;
    if frmMain.Visible then
      frmMain.Enabled := False;
    if not Assigned(Paymo.CompanyData) then
      DownloadCompany.Start;
    DownloadProjects.Start;
    DownloadTasks.Start;
    DownloadRunningTimer.Start;
    DownloadTaskLists.Start;
    firstSync := False;
  end;
end;

procedure TfrmMain.tiTrayClick(Sender: TObject);
begin
  Self.ShowInTaskBar := stDefault;
  Self.Show;
  // if application is minimized
  Application.Restore;
end;

procedure TfrmMain.hideMenu(Sender: TObject);
begin
  pnlMenu.Animate();
  // change style back to enabled
  pnlTop.Enabled := True;
  edSearch.Enabled := True;
  pnlTime.Enabled := True;
  if TabControl1.Tabs.Count > 0 then
     TabControl1.Visible := True;
  if Assigned(Tasks) then
    Tasks.Enabled := True;
end;

procedure TfrmMain.wcThreadDownloaderAllTasksFinished(const Sender: TWCthread);
begin
  DebugLog('FPC Paymo Widget', 'Refresh', 'Finish');
  if Assigned(frmTimeEntry) then
  begin
    if (frmTimeEntry.Visible) then
      frmTimeEntry.ShowData(True);
    frmTimeEntry.Enabled := True;
  end;
  frmMain.Enabled := True;
  pbRefresh.Visible := False;
  timerRefresh.Enabled := True;
  Sync;
end;

{procedure TfrmMain.WMMove(var Message: TLMMove);
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
end;}

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
    //NO Internet
    prNOInternet:
    begin
      Paymo.Offline := True;
      Paymo.Login;
      pnlTop.Caption := pnlTop.Caption + ' - Offline';
      ShowMessage(rsWorkingOfflineTheDataWillBeSavedTheNextTimeYouAreOnline);
    end;
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

procedure TfrmMain.Sync;
var
  i: integer;
begin
  if not Paymo.Offline and Paymo.HasOfflineData then
  begin
    DebugLog('FPC Paymo Widget', 'Sync', 'Start');
    i := Paymo.SYNC_OfflineData;
    if i <> 0 then
    begin
      ShowMessage(rsSyncCompleteWithError);
      DebugLog('FPC Paymo Widget', 'Sync', 'Finish: Errors ' + IntToStr(i));
    end
    else
    begin
      ShowMessage(rsSyncCompleteSuccessfully);
      DebugLog('FPC Paymo Widget', 'Sync', 'Finish: OK');
    end;
    timerRefreshTimer(nil);
  end;
end;

procedure TfrmMain.ListProjects();
begin

end;

procedure TfrmMain.ListTasks();
begin
  Tasks.Visible := False;
  Tasks.RefreshItems;
  if edSearch.Text <> '' then
    edSearchChange(nil);
  //SetFonts(Tasks);
  Tasks.Visible := True;
end;

procedure TfrmMain.ListTimeEntry();
begin
  RefreshTabs;
end;

procedure TfrmMain.ChangeIcon(const AIndex: integer);   //0: normal //1:start //2:offline
var
  Image1: TImage;
  iIndex: integer;
  bmp: TBGRABitmap;
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
      end;
      else
      begin
        ilTrayNormalWin.GetBitmap(0, Image1.Picture.BitMap);
        tiTray.Icons := ilTrayNormalWin;
      end;
    end;
    if cbShowTimeInAppIcon.Checked and ((iIndex = 1) or (iIndex = 3)) then
    begin
      bmp := TBGRABitmap.Create();
      try
        bmp.Assign(Image1.Picture.Graphic);
        bmp.FontHeight := 11;
        //bmp.FontName := FONTNAMEARROW;
        bmp.Rectangle(0, bmp.Height - 11, bmp.Width, bmp.Height, BGRABlack,
          BGRABlack, dmSet);
        bmp.TextOut(0, bmp.Height - 11, TTaskList.SecondsToHHMMSS(
          SecondsBetween(start_time, now)), BGRAWhite);
        Application.Icon.Assign(bmp.Bitmap);
      finally
        bmp.Free;
      end;
    end
    else
      Application.Icon.Assign(Image1.Picture.Graphic);
  finally
    Image1.Free;
  end;
end;

procedure TfrmMain.RefreshTimeEntry();
begin
  if (TabControl1.Visible) then
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

function TfrmMain.StopTimer(end_time: TDateTime): boolean;
var
  index: integer;
begin
  Index := TabControl1.TabIndex;
  if (Paymo.RunningTimerData = nil) and (TabControl1.TabIndex = 0) then
    Index := 1;
  case Index of
    // main timer
    0:
    begin
      if (Paymo.RunningTimerData <> nil) then
      begin
        case Paymo.StopRunningTimer(start_time, end_time, '') of
          prOK:
          begin
            stop_ok := True;
            //pnlTime.Visible := False;
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
          prNOInternet:
          begin
            ShowMessage(rsMainTimerCantBeStoppedWithoutInternet);
          end;
        end;
      end;
    end;
    // additional timers
    else
    begin
      index := TabControl1.TabIndex;
      if Paymo.RunningTimerData <> nil then
        Dec(index);
      case Paymo.StopAdditionalTimer(index, end_time) of
        prOK, prNOInternet:
        begin
          stop_ok := True;
          //pnlTime.Visible := False;
          Application.ProcessMessages;
          // Sync for now, ToDo: change to async with tasks
          if not Paymo.Offline then
          begin
            Paymo.GetTasks();
            DownloadTasksFinish(nil, 0, 0);
          end;
          ListTasks;
        end;
        prTRYAGAIN, prERROR:
        begin
          stop_ok := False;
          ShowMessage(rsErrorCantStopTimer);
        end;
        {prNOInternet:
        begin
          ShowMessage(rsWorkingOfflineTheDataWillBeSavedTheNextTimeYouAreOnline);
        end;}
      end;
    end;
  end;
  if Paymo.Offline then
    RefreshTabs;
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

procedure TfrmMain.RefreshTabs;
begin
  TabControl1.Tabs.Text := Paymo.GetTimerTabs;
  TabControl1.Visible := TabControl1.Tabs.Count <> 0;
  AssignTask;
end;

procedure TfrmMain.AssignTask;
var
  index: integer;
begin
  if not TabControl1.Visible then
    exit;
  Index := TabControl1.TabIndex;
  if (Paymo.RunningTimerData = nil) and (TabControl1.TabIndex = 0) then
    Index := 1;
  case Index of
    // Main Timer
    0:
    begin
      lblProject.Caption := Paymo.GetProjectName(
        Paymo.RunningTimerData.GetPath('project_id').AsInteger);
      lblTask.Caption := Paymo.GetTaskName(Paymo.RunningTimerData.GetPath(
        'task_id').AsInt64);
      start_time := TTaskList.StringToDateTime(
        Paymo.RunningTimerData.GetPath('start_time').AsString);
    end;
    // Additional
    else
    begin
      index := TabControl1.TabIndex;
      if Paymo.RunningTimerData <> nil then
        Dec(index);
      lblProject.Caption := Paymo.GetProjectName(
        Paymo.GetAdditionalRunningTimers[index].GetPath('project_id').AsInteger);
      lblTask.Caption := Paymo.GetTaskName(
        Paymo.GetAdditionalRunningTimers[index].GetPath('task_id').AsInt64);
      start_time := TTaskList.StringToDateTime(
        Paymo.GetAdditionalRunningTimers[index].GetPath('start_time').AsString);
    end;
  end;
  // refresh time now
  timerEntryTimer(nil);
end;

end.
