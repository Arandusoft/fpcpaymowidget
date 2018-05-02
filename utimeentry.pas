unit utimeentry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  DateUtils, upaymo, fpjson, utasklist, ColorSpeedButton, LMessages, ExtCtrls,
  uresourcestring, AutoCompletePanel, LazUTF8, LCLType, JSONPropStorage;

type

  { TfrmTimeEntry }

  TfrmTimeEntry = class(TForm)
    acProject: TAutoCompletePanel;
    acTaskList: TAutoCompletePanel;
    acTask: TAutoCompletePanel;
    btnExistingTask: TColorSpeedButton;
    btnSet15: TColorSpeedButton;
    btnSet1h: TColorSpeedButton;
    btnSet30: TColorSpeedButton;
    btnSet45: TColorSpeedButton;
    btnSetNow: TColorSpeedButton;
    chkCompletedTask: TCheckBox;
    JSONPropStorage1: TJSONPropStorage;
    dlgDate: TCalendarDialog;
    lblDescription: TLabel;
    lblDescription1: TLabel;
    lblDescription2: TLabel;
    lblDescription3: TLabel;
    lbl_date: TLabel;
    memoDescription: TMemo;
    pnlSetTime: TPanel;
    pnlGroup: TPanel;
    time_end_hh: TEdit;
    time_end_mm: TEdit;
    time_end_separator: TLabel;
    time_start_hh: TEdit;
    time_start_mm: TEdit;
    time_start_separator: TLabel;
    time_start_separator1: TLabel;
    btnDeleteEntry: TColorSpeedButton;
    btnSaveEntry: TColorSpeedButton;
    procedure acProjectSearch(Sender: TObject; SearchText: string; Items: TStrings);
    procedure acProjectSelectionChange(Sender: TObject);
    procedure acTaskListSearch(Sender: TObject; SearchText: string;
      Items: TStrings);
    procedure acTaskListSelectionChange(Sender: TObject);
    procedure acTaskSearch(Sender: TObject; SearchText: string; Items: TStrings);
    procedure acTaskSelectionChange(Sender: TObject);
    procedure btnDeleteEntryClick(Sender: TObject);
    procedure btnExistingTaskClick(Sender: TObject);
    procedure btnSaveEntryClick(Sender: TObject);
    procedure btnSet15Click(Sender: TObject);
    procedure btnSet1hClick(Sender: TObject);
    procedure btnSet30Click(Sender: TObject);
    procedure btnSet45Click(Sender: TObject);
    procedure btnSetNowClick(Sender: TObject);
    procedure btnStartTimer(Sender: TObject);
    procedure btnCreateTask(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbl_dateClick(Sender: TObject);
    procedure time_end_hhChange(Sender: TObject);
    procedure time_end_mmChange(Sender: TObject);
  private
    procedure FillDateAndTime;
    procedure FillStartTime;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
  public
    PaymoInstance: TPaymo;
    Data: TJSONData;
    selected_data: integer;
    selected_project: integer;
    selected_task: integer;
    selected_tasklist: integer;
    procedure ShowData(FromRefresh: boolean = False);
    procedure SelectItems(project_id: integer; task_id: integer; tasklist_id: integer);
  end;

var
  frmTimeEntry: TfrmTimeEntry;

implementation

uses
  umain;

{$R *.lfm}

{ TfrmTimeEntry }

procedure TfrmTimeEntry.lbl_dateClick(Sender: TObject);
begin
  if dlgDate.Execute then
  begin
    lbl_date.Caption := FormatDateTime('ddddd', dlgDate.Date);
  end;
end;

procedure TfrmTimeEntry.time_end_hhChange(Sender: TObject);
var
  e: TEdit;
  s: string;
begin
  e := TEdit(Sender);
  s := e.Text;
  if (Length(s) > 0) and (s.ToInteger > 23) then
    e.Text := '23';
end;

procedure TfrmTimeEntry.time_end_mmChange(Sender: TObject);
var
  e: TEdit;
  s: string;
begin
  e := TEdit(Sender);
  s := e.Text;
  if (Length(s) > 0) and (s.ToInteger > 59) then
    e.Text := '59';
end;

procedure TfrmTimeEntry.FillDateAndTime;
var
  tempTime: TDateTime;
begin
  if Data <> nil then
  begin
    tempTime := TTaskList.StringToDateTime(Data.GetPath('start_time').AsString);
    dlgDate.Date := tempTime;
    time_start_hh.Caption := FormatDateTime('hh', tempTime);
    time_start_mm.Caption := FormatDateTime('nn', tempTime);
    tempTime := TTaskList.StringToDateTime(Data.GetPath('end_time').AsString);
    time_end_hh.Caption := FormatDateTime('hh', tempTime);
    time_end_mm.Caption := FormatDateTime('nn', tempTime);
  end
  else
  begin
    tempTime := now;
    dlgDate.Date := tempTime;
    time_start_hh.Caption := FormatDateTime('hh', tempTime);
    time_start_mm.Caption := FormatDateTime('nn', tempTime);
    time_end_hh.Caption := FormatDateTime('hh', tempTime);
    time_end_mm.Caption := FormatDateTime('nn', tempTime);
  end;
  lbl_date.Caption := FormatDateTime('ddddd', dlgDate.Date);
end;

procedure TfrmTimeEntry.FillStartTime;
begin
  time_start_hh.Caption := FormatDateTime('hh', dlgDate.Date);
  time_start_mm.Caption := FormatDateTime('nn', dlgDate.Date);
end;

procedure TfrmTimeEntry.WMMove(var Message: TLMMove);
var
  l, t: integer;
begin
  inherited WMMove(Message);
  {$IFDEF LINUX}
  // does not works on linux
  {$ELSE}
  l := Self.Left + Self.Width;
  t := Self.Top;
  if frmMain.Left <> l then
    frmMain.Left := l;
  if frmMain.Top <> t then
    frmMain.Top := t;
  {$ENDIF}
end;

procedure TfrmTimeEntry.ShowData(FromRefresh: boolean);
begin
  acProject.SelectedObject := nil;
  acProject.SelectedObjectText := ' ';
  acTaskList.SelectedObject := nil;
  acTaskList.SelectedObjectText := ' ';
  acTask.SelectedObject := nil;
  acTask.SelectedObjectText := ' ';

  if FromRefresh then
  begin
    // show work in progress stuff of new task
    if selected_data = 0 then
    begin
      SelectItems(selected_project, selected_task, selected_tasklist);
      exit;
    end
    // reload task being edited, can be changed so reload entirely from synchro
    else
      Data := PaymoInstance.GetTimeEntry(selected_data);
  end;

  // show / hide depending if is a new task or edit time entry
  if Data = nil then
  begin
    acTask.Visible := False;
    btnDeleteEntry.Visible := True;
    btnDeleteEntry.Caption := rsCreateTask;
    btnDeleteEntry.OnClick := @btnCreateTask;
    lbl_date.Visible := False;
    time_start_hh.Visible := True;
    time_start_separator.Visible := True;
    time_start_mm.Visible := True;
    time_start_separator1.Visible := False;
    time_end_hh.Visible := False;
    time_end_separator.Visible := False;
    time_end_mm.Visible := False;
    btnSaveEntry.Caption := rsStartTimer;
    btnSaveEntry.OnClick := @btnStartTimer;
    memoDescription.Visible := True;
    memoDescription.Lines.Clear;
    lblDescription.Visible := True;
    if memoDescription.CanSetFocus then
      memoDescription.SetFocus;
    lblDescription3.Visible := False;
    btnExistingTask.Visible := True;
    pnlSetTime.Visible := True;
    chkCompletedTask.Visible := False;
    selected_data := 0;
  end
  else
  begin
    acTask.Visible := True;
    acTask.Width := acProject.Width;
    // Workaround to set size
    acTask.HideListBox;
    btnDeleteEntry.Visible := True;
    btnDeleteEntry.Caption := rsDeleteEntry;
    btnDeleteEntry.OnClick := @btnDeleteEntryClick;
    lbl_date.Visible := True;
    time_start_hh.Visible := True;
    time_start_separator.Visible := True;
    time_start_mm.Visible := True;
    time_start_separator1.Visible := True;
    time_end_hh.Visible := True;
    time_end_separator.Visible := True;
    time_end_mm.Visible := True;
    btnSaveEntry.Caption := rsSaveEntry;
    btnSaveEntry.OnClick := @btnSaveEntryClick;
    memoDescription.Visible := False;
    lblDescription.Visible := False;
    lblDescription3.Visible := True;
    btnExistingTask.Visible := False;
    pnlSetTime.Visible := False;
    chkCompletedTask.Visible := True;
    SelectItems(Data.GetPath('project_id').AsInteger,
      Data.GetPath('task_id').AsInteger, 0);
    selected_data := Data.GetPath('id').AsInteger;
  end;
  FillDateAndTime;
end;

procedure TfrmTimeEntry.SelectItems(project_id: integer; task_id: integer;
  tasklist_id: integer);
var
  t_id: integer;
  i: integer;
  project, task, tasklist: TJSONArray;
begin
  // Search Project
  project := PaymoInstance.ProjectsArray;
  for i := 0 to project.Count - 1 do
  begin
    if project[i].GetPath('id').AsInteger = project_id then
    begin
      acProject.SelectedObject := project[i];
      acProject.SelectedObjectText := project[i].GetPath('name').AsString;
      break;
    end;
  end;

  if acProject.SelectedObject <> nil then
  begin

    // Search Task
    task := PaymoInstance.TasksArray;
    for i := 0 to task.Count - 1 do
    begin
      if task[i].GetPath('id').AsInteger = task_id then
      begin
        acTask.SelectedObject := task[i];
        acTask.SelectedObjectText := task[i].GetPath('name').AsString;
        chkCompletedTask.Checked := task[i].GetPath('complete').AsBoolean;
        break;
      end;
    end;

    if acTask.SelectedObject <> nil then
    begin

      // Search Task List
      if tasklist_id = 0 then
        t_id := TJSONData(acTask.SelectedObject).GetPath('tasklist_id').AsInteger
      else
        t_id := tasklist_id;

      tasklist := PaymoInstance.TaskListsArray;
      for i := 0 to tasklist.Count - 1 do
      begin
        if tasklist[i].GetPath('id').AsInteger = t_id then
        begin
          acTaskList.SelectedObject := tasklist[i];
          acTaskList.SelectedObjectText := tasklist[i].GetPath('name').AsString;
        end;
      end;
    end
    else
    begin
      if tasklist_id <> 0 then
      begin
        tasklist := PaymoInstance.TaskListsArray;
        for i := 0 to tasklist.Count - 1 do
        begin
          if tasklist[i].GetPath('id').AsInteger = tasklist_id then
          begin
            acTaskList.SelectedObject := tasklist[i];
            acTaskList.SelectedObjectText := tasklist[i].GetPath('name').AsString;
          end;
        end;
      end;
    end;
  end;
  acProject.HideListBox;
  if acTask.Visible then
    acTask.HideListBox;
  acTaskList.HideListBox;
end;

procedure TfrmTimeEntry.FormClick(Sender: TObject);
begin
  acTask.HideListBox;
  acProject.HideListBox;
  acTaskList.HideListBox;
end;

procedure TfrmTimeEntry.FormCreate(Sender: TObject);
begin
  // prevent flickering
  {$ifdef windows}
  DoubleBuffered:=True;
  {$endif}
  frmMain.SetFonts(Self);
  // Restore position (only works with Position = poDesigned)
  {$IFNDEF DARWIN}
  JSONPropStorage1.JSONFileName := GetAppConfigDir(False) + 'settings.json';
  {$ENDIF}
  acProject.Width := Width - acProject.Left - ScaleX(20, 96);
  acTask.Width := Width - acTask.Left - ScaleX(20, 96);
  acTaskList.Width := Width - acTaskList.Left - ScaleX(20, 96);
end;

procedure TfrmTimeEntry.FormShow(Sender: TObject);
var
  l, t: integer;
begin
  l := frmMain.Left - Width;
  t := frmMain.Top;
  if Left <> l then
    Left := l;
  if Top <> t then
    Top := t;
  // Workaround to set right size
  acProject.HideListBox;
  acTask.HideListBox;
  acTaskList.HideListBox;
end;

procedure TfrmTimeEntry.btnSaveEntryClick(Sender: TObject);
var
  canSave: boolean;
  t_start, t_end: TDateTime;
  s_hh, s_mm: string;
  r, r2: TPaymoResponseStatus;
begin
  //ShowMessage(TJSONData(acProject.SelectedObject).GetPath('id').AsString);
  if (frmMain.pnlMenu.Width <> 0) and (not frmMain.pnlMenu.Timer.Enabled) then
    frmMain.hideMenu(nil);
  // required fields
  canSave := (time_start_hh.Text <> '') and (time_start_mm.Text <> '') and
    (time_end_hh.Text <> '') and (time_end_mm.Text <> '') and
    (acProject.SelectedObject <> nil) and (acTaskList.SelectedObject <> nil) and
    (acTask.SelectedObject <> nil);
  if not CanSave then
  begin
    ShowMessage(rsPleaseFillAllFields);
    exit();
  end;
  // start time
  s_hh := time_start_hh.Text;
  s_mm := time_start_mm.Text;
  t_start := dlgDate.Date;
  ReplaceTime(t_start, EncodeTime(s_hh.ToInteger, s_mm.ToInteger, 0, 0));
  // end time
  s_hh := time_end_hh.Text;
  s_mm := time_end_mm.Text;
  t_end := dlgDate.Date;
  ReplaceTime(t_end, EncodeTime(s_hh.ToInteger, s_mm.ToInteger, 0, 0));
  // required time between
  if SecondsBetween(t_start, t_end) < 59 then
  begin
    ShowMessage(rsThereMustBeAtLeastAMinuteOfDifferenceBetweenStartAndEndTime);
    exit();
  end;
  // time entry
  r := PaymoInstance.UpdateTimeEntry(Data.GetPath('id').AsInteger,
    t_start, t_end, TJSONData(acProject.SelectedObject).GetPath('id').AsInteger,
    TJSONData(acTask.SelectedObject).GetPath('id').AsInteger,
    TJSONData(acTaskList.SelectedObject).GetPath('id').AsInteger);
  case r of
    prTRYAGAIN, prERROR:
    begin
      ShowMessage(rsErrorCantUpdateTimeEntry);
    end;
  end;
  // task completion
  if TJSONData(acTask.SelectedObject).GetPath('complete').AsBoolean <>
    chkCompletedTask.Checked then
    r2 := PaymoInstance.UpdateTaskCompletion(chkCompletedTask.Checked,
      TJSONData(acTask.SelectedObject));
  case r2 of
    prTRYAGAIN, prERROR:
    begin
      ShowMessage(rsErrorCantUpdateTask);
      exit;
    end;
  end;
  if (r = prOK) or (r2 = prOK) then
  begin
    Self.Close;
    Application.ProcessMessages;
    // Sync for now, ToDo: change to async with tasks
    PaymoInstance.GetTasks();
    frmMain.DownloadTasksFinish(nil, 0, 0);
  end;
end;

procedure TfrmTimeEntry.btnSet15Click(Sender: TObject);
begin
  dlgDate.Date := IncMinute(dlgDate.Date, -15);
  FillStartTime;
end;

procedure TfrmTimeEntry.btnSet1hClick(Sender: TObject);
begin
  dlgDate.Date := IncHour(dlgDate.Date, -1);
  FillStartTime;
end;

procedure TfrmTimeEntry.btnSet30Click(Sender: TObject);
begin
  dlgDate.Date := IncMinute(dlgDate.Date, -30);
  FillStartTime;
end;

procedure TfrmTimeEntry.btnSet45Click(Sender: TObject);
begin
  dlgDate.Date := IncMinute(dlgDate.Date, -45);
  FillStartTime;
end;

procedure TfrmTimeEntry.btnSetNowClick(Sender: TObject);
begin
  dlgDate.Date := now;
  FillStartTime;
end;

procedure TfrmTimeEntry.btnStartTimer(Sender: TObject);
var
  task: TJSONData;
  r: TPaymoResponseStatus;
  canSave: boolean;
  s_hh, s_mm: string;
  t_start: TDateTime;
begin
  if (frmMain.pnlMenu.Width <> 0) and (not frmMain.pnlMenu.Timer.Enabled) then
    frmMain.hideMenu(nil);
  if memoDescription.Visible then
  begin
    // required fields
    canSave := (acProject.SelectedObject <> nil) and (acTaskList.SelectedObject <> nil);
    if not CanSave then
    begin
      ShowMessage(rsPleaseFillAllFields);
      exit();
    end;
    if memoDescription.Lines.Text = '' then
    begin
      ShowMessage(rsPleaseEnterTaskDescription);
      exit();
    end;
  end
  else
  begin
    // required fields
    canSave := (acProject.SelectedObject <> nil) and (acTask.SelectedObject <> nil) and
      (acTaskList.SelectedObject <> nil);
    if not CanSave then
    begin
      ShowMessage(rsPleaseFillAllFields);
      exit();
    end;
  end;

  if memoDescription.Visible then
  begin
    r := PaymoInstance.CreateTask(memoDescription.Lines.Text, '',
      TJSONData(acTaskList.SelectedObject).GetPath('id').AsInteger, task);
  end
  else
  begin
    r := prOK;
    task := TJSONData(acTask.SelectedObject);
  end;

  if (r = prOK) then
  begin
    // start time
    s_hh := time_start_hh.Text;
    s_mm := time_start_mm.Text;
    t_start := now;
    ReplaceTime(t_start, EncodeTime(s_hh.ToInteger, s_mm.ToInteger, 0, 0));
    case PaymoInstance.StartRunningTimer(task.GetPath('id').AsInteger, t_start) of
      prOK:
      begin
        case PaymoInstance.GetRunningTimer() of
          prOK:
          begin
            Self.Close();
            frmMain.DownloadRunningTimerFinish(nil, 0, 0);
          end;
          prTRYAGAIN, prERROR:
          begin
            ShowMessage(rsErrorCantStartTimer);
          end;
        end;
      end;
      prTRYAGAIN, prERROR:
      begin
        ShowMessage(rsErrorCantStartTimerTryStoppingCurrentTimerFirst);
      end;
    end;
  end
  else
    ShowMessage(rsErrorCantCreateTask);
end;

procedure TfrmTimeEntry.btnCreateTask(Sender: TObject);
var
  task: TJSONData;
  r: TPaymoResponseStatus;
  canSave: boolean;
  s_hh, s_mm: string;
  t_start: TDateTime;
begin
  if (frmMain.pnlMenu.Width <> 0) and (not frmMain.pnlMenu.Timer.Enabled) then
    frmMain.hideMenu(nil);
  // required fields
  canSave := (acProject.SelectedObject <> nil) and (acTaskList.SelectedObject <> nil);
  if not CanSave then
  begin
    ShowMessage(rsPleaseFillAllFields);
    exit();
  end;
  if memoDescription.Lines.Text = '' then
  begin
    ShowMessage(rsPleaseEnterTaskDescription);
    exit();
  end;
  case PaymoInstance.CreateTask(memoDescription.Lines.Text, '',
      TJSONData(acTaskList.SelectedObject).GetPath('id').AsInteger, task) of
    prOK:
    begin
      Self.Close;
    end;
    prERROR, prTRYAGAIN:
    begin
      ShowMessage(rsErrorCantCreateTask);
    end;
  end;
end;

procedure TfrmTimeEntry.btnDeleteEntryClick(Sender: TObject);
begin
  case PaymoInstance.DeleteTimeEntry(Data.GetPath('id').AsString) of
    prOK:
    begin
      Self.Close;
      Application.ProcessMessages;
      // Sync for now, ToDo: change to async with tasks
      PaymoInstance.GetTasks();
      frmMain.DownloadTasksFinish(nil, 0, 0);
    end;
    prTRYAGAIN, prERROR:
    begin
      ShowMessage(rsErrorCantDeleteTimeEntry);
    end;
  end;
end;

procedure TfrmTimeEntry.acProjectSearch(Sender: TObject; SearchText: string;
  Items: TStrings);
var
  i, id: integer;
  projects: TJSONArray;
  pass1: boolean;
  search: string;
begin
  projects := PaymoInstance.ProjectsArray;

  pass1 := UTF8Length(SearchText) = 0;
  search := UTF8LowerCase(SearchText);
  for i := 0 to projects.Count - 1 do
  begin
    if (pass1) or (UTF8Pos(search,
      UTF8LowerCase(projects[i].GetPath('name').AsString)) <> 0) then
      Items.AddObject(projects[i].GetPath('name').AsString, projects[i]);
  end;
end;

procedure TfrmTimeEntry.acProjectSelectionChange(Sender: TObject);
begin
  if acProject.SelectedObject <> nil then
    selected_project := TJSONData(acProject.SelectedObject).GetPath('id').AsInteger
  else
    selected_project := 0;

  selected_task := 0;
  selected_tasklist := 0;

  acTask.SelectedObject := nil;
  acTask.SelectedObjectText := ' ';
  acTaskList.SelectedObject := nil;
  acTaskList.SelectedObjectText := ' ';
end;

procedure TfrmTimeEntry.acTaskListSearch(Sender: TObject; SearchText: string;
  Items: TStrings);
var
  i, proj_id: integer;
  tasklists: TJSONArray;
  pass1: boolean;
  search: string;
begin
  if acProject.SelectedObject = nil then
    exit;
  tasklists := PaymoInstance.TaskListsArray;
  proj_id := TJSONData(acProject.SelectedObject).GetPath('id').AsInteger;
  pass1 := UTF8Length(SearchText) = 0;
  search := UTF8LowerCase(SearchText);
  for i := 0 to tasklists.Count - 1 do
  begin
    if (proj_id = tasklists[i].GetPath('project_id').AsInteger) and
      ((pass1) or (UTF8Pos(search,
      UTF8LowerCase(tasklists[i].GetPath('name').AsString)) <> 0)) then
      Items.AddObject(tasklists[i].GetPath('name').AsString, tasklists[i]);
  end;
end;

procedure TfrmTimeEntry.acTaskListSelectionChange(Sender: TObject);
begin
  if acTaskList.SelectedObject <> nil then
    selected_tasklist := TJSONData(acTaskList.SelectedObject).GetPath('id').AsInteger
  else
    selected_tasklist := 0;
end;

procedure TfrmTimeEntry.acTaskSearch(Sender: TObject; SearchText: string;
  Items: TStrings);
var
  i, proj_id: integer;
  tasks: TJSONArray;
  pass1: boolean;
  search: string;
begin
  if acProject.SelectedObject = nil then
    exit;
  tasks := PaymoInstance.TasksArray;
  proj_id := TJSONData(acProject.SelectedObject).GetPath('id').AsInteger;
  pass1 := UTF8Length(SearchText) = 0;
  search := UTF8LowerCase(SearchText);
  for i := 0 to tasks.Count - 1 do
  begin
    if (proj_id = tasks[i].GetPath('project_id').AsInteger) and
      ((pass1) or (UTF8Pos(search, UTF8LowerCase(tasks[i].GetPath('name').AsString)) <>
      0)) and (not tasks[i].GetPath('complete').AsBoolean) then
      Items.AddObject(tasks[i].GetPath('name').AsString, tasks[i]);
  end;
end;

procedure TfrmTimeEntry.acTaskSelectionChange(Sender: TObject);
begin
  if acTask.SelectedObject <> nil then
    selected_task := TJSONData(acTask.SelectedObject).GetPath('id').AsInteger
  else
    selected_task := 0;
end;

procedure TfrmTimeEntry.btnExistingTaskClick(Sender: TObject);
begin
  btnExistingTask.Visible := False;
  acTask.Visible := True;
  acTask.Width := acProject.Width;
  // Workaround to set size
  acTask.HideListBox;
  if acTask.CanSetFocus then
    acTask.SetFocus;
  lblDescription3.Visible := True;
  lblDescription.Visible := False;
  memoDescription.Visible := False;
  btnDeleteEntry.Visible := False;
end;

end.
