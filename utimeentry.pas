unit utimeentry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  DateUtils, upaymo, fpjson, utasklist, ColorSpeedButton, LMessages, ExtCtrls,
  uresourcestring, LazUTF8;

type

  { TfrmTimeEntry }

  TfrmTimeEntry = class(TForm)
    btnExistingTask: TColorSpeedButton;
    editSearchProject: TEdit;
    cbProjectTasks: TComboBox;
    cbProjects: TComboBox;
    cbProjectTaskLists: TComboBox;
    dlgDate: TCalendarDialog;
    editSearchTaskLists: TEdit;
    editSearchTasks: TEdit;
    lblDescription: TLabel;
    lblDescription1: TLabel;
    lblDescription2: TLabel;
    lblDescription3: TLabel;
    lbl_date: TLabel;
    memoDescription: TMemo;
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
    procedure btnDeleteEntryClick(Sender: TObject);
    procedure btnExistingTaskClick(Sender: TObject);
    procedure btnSaveEntryClick(Sender: TObject);
    procedure btnStartTimer(Sender: TObject);
    procedure cbProjectsChange(Sender: TObject);
    procedure editSearchTaskListsChange(Sender: TObject);
    procedure editSearchTasksChange(Sender: TObject);
    procedure editSearchProjectChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbl_dateClick(Sender: TObject);
    procedure time_end_hhChange(Sender: TObject);
    procedure time_end_mmChange(Sender: TObject);
  private
    Data_TaskListID: integer;
    procedure FillProjectsCombo;
    procedure FillProjectTaskLists(TaskListID: integer = 0);
    procedure FillProjectTasks(FromData: boolean);
    procedure FillDateAndTime;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
  public
    PaymoInstance: TPaymo;
    Data: TJSONData;
    procedure ShowData;
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

procedure TfrmTimeEntry.FillProjectsCombo;
var
  i, id: integer;
  projects: TJSONArray;
  pass1: boolean;
  search: string;
begin
  cbProjects.Clear;
  projects := PaymoInstance.ProjectsArray;

  if Data <> nil then
    id := Data.GetPath('project_id').AsInteger
  else
    id := 0;

  pass1 := UTF8Length(editSearchProject.Text) = 0;
  search := UTF8LowerCase(editSearchProject.Text);
  for i := 0 to projects.Count - 1 do
  begin
    if (pass1) or (UTF8Pos(search,
      UTF8LowerCase(projects[i].GetPath('name').AsString)) <>
      0) then
      cbProjects.AddItem(projects[i].GetPath('name').AsString, projects[i]);
    if id = projects[i].GetPath('id').AsInteger then
      cbProjects.ItemIndex := i;
  end;

  if cbProjects.ItemIndex = -1 then
    cbProjects.ItemIndex := 0;
end;

procedure TfrmTimeEntry.FillProjectTaskLists(TaskListID: integer = 0);
var
  i: integer;
  tasklists: TJSONArray;
  proj_id: integer;
  pass1: boolean;
  search: string;
begin
  cbProjectTaskLists.Clear;
  if cbProjects.Items.Count = 0 then
    exit;
  tasklists := PaymoInstance.TaskListsArray;

  proj_id := TJSONData(cbProjects.Items.Objects[cbProjects.ItemIndex]).GetPath('id').AsInteger;
  pass1 := UTF8Length(editSearchTaskLists.Text) = 0;
  search := UTF8LowerCase(editSearchTaskLists.Text);
  for i := 0 to tasklists.Count - 1 do
  begin
    if (proj_id = tasklists[i].GetPath('project_id').AsInteger) and
      ((pass1) or (UTF8Pos(search, UTF8LowerCase(tasklists[i].GetPath('name').AsString)) <>
      0)) then
    begin
      cbProjectTaskLists.AddItem(tasklists[i].GetPath('name').AsString, tasklists[i]);
      if TaskListID = tasklists[i].GetPath('id').AsInteger then
        cbProjectTaskLists.ItemIndex := cbProjectTaskLists.Items.Count - 1;
    end;
  end;

  if TaskListID = 0 then
    cbProjectTaskLists.ItemIndex := 0;
end;

procedure TfrmTimeEntry.FillProjectTasks(FromData: boolean);
var
  i, proj_id, task_id: integer;
  tasks: TJSONArray;
  pass1: boolean;
  search: string;
begin
  cbProjectTasks.Clear;
  if cbProjects.Items.Count = 0 then
    exit;
  tasks := PaymoInstance.TasksArray;
  proj_id := TJSONData(cbProjects.Items.Objects[cbProjects.ItemIndex]).GetPath('id').AsInteger;

  if (Data <> nil) and (FromData) then
  begin
    task_id := Data.GetPath('task_id').AsInteger;
    proj_id := Data.GetPath('project_id').AsInteger;
  end
  else
    task_id := 0;

  pass1 := UTF8Length(editSearchTasks.Text) = 0;
  search := UTF8LowerCase(editSearchTasks.Text);
  for i := 0 to tasks.Count - 1 do
  begin
    if (proj_id = tasks[i].GetPath('project_id').AsInteger) and
      ((pass1) or (UTF8Pos(search, UTF8LowerCase(tasks[i].GetPath('name').AsString)) <>
      0))
    {and (not tasks[i].GetPath('complete').AsBoolean)} then
      cbProjectTasks.AddItem(tasks[i].GetPath('name').AsString, tasks[i]);
    if task_id = tasks[i].GetPath('id').AsInteger then
    begin
      cbProjectTasks.ItemIndex := cbProjectTasks.Items.Count - 1;
      Data_TaskListID := tasks[i].GetPath('tasklist_id').AsInteger;
    end;
  end;

  if not FromData then
    cbProjectTasks.ItemIndex := 0;
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

procedure TfrmTimeEntry.WMMove(var Message: TLMMove);
var
  l, t: integer;
begin
  inherited WMMove(Message);
  l := Self.Left + Self.Width;
  t := Self.Top;
  if frmMain.Left <> l then
    frmMain.Left := l;
  if frmMain.Top <> t then
    frmMain.Top := t;
end;

procedure TfrmTimeEntry.ShowData;
begin
  Data_TaskListID := 0;

  editSearchProject.Text := '';
  editSearchTaskLists.Text := '';
  editSearchTasks.Text := '';

  // show / hide depending if is a new task or edit time entry
  if Data = nil then
  begin
    cbProjectTasks.Visible := False;
    btnDeleteEntry.Visible := False;
    pnlGroup.Visible := False;
    btnSaveEntry.Caption := rsStartTimer;
    btnSaveEntry.OnClick := @btnStartTimer;
    memoDescription.Visible := True;
    memoDescription.Lines.Clear;
    lblDescription.Visible := True;
    if memoDescription.CanSetFocus then
      memoDescription.SetFocus;
    lblDescription3.Visible := False;
    editSearchTasks.Visible := False;
    btnExistingTask.Visible := True;
  end
  else
  begin
    cbProjectTasks.Visible := True;
    btnDeleteEntry.Visible := True;
    pnlGroup.Visible := True;
    btnSaveEntry.Caption := rsSaveEntry;
    btnSaveEntry.OnClick := @btnSaveEntryClick;
    memoDescription.Visible := False;
    lblDescription.Visible := False;
    lblDescription3.Visible := True;
    editSearchTasks.Visible := True;
    if editSearchProject.CanSetFocus then
      editSearchProject.SetFocus;
    btnExistingTask.Visible := False;
  end;

  FillProjectsCombo;
  FillProjectTasks(True);
  FillProjectTaskLists(Data_TaskListID);
  FillDateAndTime;
end;

procedure TfrmTimeEntry.FormShow(Sender: TObject);
begin

end;

procedure TfrmTimeEntry.cbProjectsChange(Sender: TObject);
begin
  FillProjectTasks(False);
  FillProjectTaskLists();
end;

procedure TfrmTimeEntry.editSearchTaskListsChange(Sender: TObject);
begin
  FillProjectTaskLists();
  FillProjectTasks(False);
end;

procedure TfrmTimeEntry.editSearchTasksChange(Sender: TObject);
begin
  FillProjectTasks(False);
end;

procedure TfrmTimeEntry.editSearchProjectChange(Sender: TObject);
begin
  FillProjectsCombo;
  FillProjectTasks(False);
  FillProjectTaskLists();
end;

procedure TfrmTimeEntry.btnSaveEntryClick(Sender: TObject);
var
  canSave: boolean;
  t_start, t_end: TDateTime;
  s_hh, s_mm: string;
begin
  // required fields
  canSave := (time_start_hh.Text <> '') and (time_start_mm.Text <> '') and
    (time_end_hh.Text <> '') and (time_end_mm.Text <> '') and
    (cbProjects.ItemIndex >= 0) and (cbProjectTaskLists.ItemIndex >= 0) and
    (cbProjectTasks.ItemIndex >= 0);
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
  case PaymoInstance.UpdateTimeEntry(Data.GetPath('id').AsString,
      t_end, TJSONData(cbProjects.Items.Objects[cbProjects.ItemIndex]).GetPath('id').AsString, TJSONData(
      cbProjectTasks.Items.Objects[cbProjectTasks.ItemIndex]).GetPath('id').AsString,
      TJSONData(cbProjectTaskLists.Items.Objects[cbProjectTaskLists.ItemIndex]).GetPath(
      'id').AsString) of
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
      ShowMessage(rsErrorCantUpdateTimeEntry);
    end;
  end;
end;

procedure TfrmTimeEntry.btnStartTimer(Sender: TObject);
var
  task: TJSONData;
  r: TPaymoResponseStatus;
  canSave: boolean;
begin
  if memoDescription.Visible then
  begin
    // required fields
    canSave := (cbProjects.ItemIndex >= 0) and (cbProjectTaskLists.ItemIndex >= 0);
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
    canSave := (cbProjects.ItemIndex >= 0) and (cbProjectTaskLists.ItemIndex >= 0) and
      (cbProjectTasks.ItemIndex >= 0);
    if not CanSave then
    begin
      ShowMessage(rsPleaseFillAllFields);
      exit();
    end;
  end;

  if memoDescription.Visible then
  begin
    r := PaymoInstance.CreateTask(memoDescription.Lines.Text, '',
      TJSONData(cbProjectTaskLists.Items.Objects[cbProjectTaskLists.ItemIndex]).GetPath('id').AsInteger, task);
  end
  else
  begin
    r := prOK;
    task := TJSONData(cbProjectTasks.Items.Objects[cbProjectTasks.ItemIndex]);
  end;

  if (r = prOK) then
  begin
    case PaymoInstance.StartRunningTimer(task.GetPath('id').AsInteger) of
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

procedure TfrmTimeEntry.btnExistingTaskClick(Sender: TObject);
begin
  btnExistingTask.Visible := False;
  cbProjectTasks.Visible := True;
  editSearchTasks.Visible := True;
  lblDescription3.Visible := True;
  lblDescription.Visible := False;
  memoDescription.Visible := False;
end;

end.
