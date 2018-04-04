unit utimeentry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  DateUtils, upaymo, fpjson, utasklist, ColorSpeedButton, LMessages, ExtCtrls,
  uresourcestring, LazUTF8, LCLType;

type

  { TfrmTimeEntry }

  TfrmTimeEntry = class(TForm)
    btnExistingTask: TColorSpeedButton;
    lbProjectTaskLists: TListBox;
    lbProjectTasks: TListBox;
    lbProjects: TListBox;
    editSearchProject: TEdit;
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
    procedure editSearchProjectClick(Sender: TObject);
    procedure editSearchProjectEnter(Sender: TObject);
    procedure editSearchProjectExit(Sender: TObject);
    procedure editSearchProjectKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure editSearchTaskListsChange(Sender: TObject);
    procedure editSearchTaskListsClick(Sender: TObject);
    procedure editSearchTaskListsEnter(Sender: TObject);
    procedure editSearchTaskListsExit(Sender: TObject);
    procedure editSearchProjectChange(Sender: TObject);
    procedure editSearchTaskListsKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure editSearchTasksChange(Sender: TObject);
    procedure editSearchTasksClick(Sender: TObject);
    procedure editSearchTasksEnter(Sender: TObject);
    procedure editSearchTasksExit(Sender: TObject);
    procedure editSearchTasksKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbl_dateClick(Sender: TObject);
    procedure lbProjectsClick(Sender: TObject);
    procedure lbProjectsSelectionChange(Sender: TObject; User: boolean);
    procedure lbProjectTaskListsClick(Sender: TObject);
    procedure lbProjectTaskListsSelectionChange(Sender: TObject; User: boolean);
    procedure lbProjectTasksClick(Sender: TObject);
    procedure time_end_hhChange(Sender: TObject);
    procedure time_end_mmChange(Sender: TObject);
  private
    Data_TaskListID: integer;
    procedure FillProjectsCombo(Select: boolean = False);
    procedure FillProjectTaskLists(Select: boolean = False; TaskListID: integer = 0);
    procedure FillProjectTasks(Select: boolean = False; FromData: boolean = False);
    procedure FillDateAndTime;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure CloseListBox(Keep: TListBox);
  public
    PaymoInstance: TPaymo;
    Data: TJSONData;
    ProjectExit: boolean;
    TaskExit: boolean;
    TaskListExit: boolean;
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

procedure TfrmTimeEntry.lbProjectsClick(Sender: TObject);
begin
  editSearchProjectExit(nil);
end;

procedure TfrmTimeEntry.lbProjectsSelectionChange(Sender: TObject; User: boolean);
begin
  if (editSearchProject.Focused) or (lbProjects.Focused) then
  begin
    FillProjectTasks(False);
    FillProjectTaskLists();
  end;
end;

procedure TfrmTimeEntry.lbProjectTaskListsClick(Sender: TObject);
begin
  editSearchTaskListsExit(nil);
end;

procedure TfrmTimeEntry.lbProjectTaskListsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if (editSearchTaskLists.Focused) or (lbProjectTaskLists.Focused) then
  begin
    try
      lbProjectTaskLists.Enabled := False;
      //FillProjectTaskLists();
      FillProjectTasks(False);
    finally
      lbProjectTaskLists.Enabled := True;
    end;
  end;
end;

procedure TfrmTimeEntry.lbProjectTasksClick(Sender: TObject);
begin
  editSearchTasksExit(nil);
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

procedure TfrmTimeEntry.FillProjectsCombo(Select: boolean);
var
  i, id: integer;
  projects: TJSONArray;
  pass1: boolean;
  search: string;
begin
  lbProjects.Clear;
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
      UTF8LowerCase(projects[i].GetPath('name').AsString)) <> 0) then
      lbProjects.AddItem(projects[i].GetPath('name').AsString, projects[i]);
    if (Select) and (id = projects[i].GetPath('id').AsInteger) then
      lbProjects.ItemIndex := lbProjects.Items.Count - 1;
  end;

  if (lbProjects.ItemIndex = -1) and (lbProjects.Items.Count > 0) then
    lbProjects.ItemIndex := 0;

  if Select then
    if lbProjects.ItemIndex > -1 then
      editSearchProject.Text := lbProjects.Items[lbProjects.ItemIndex]
    else
      editSearchProject.Text := '';
end;

procedure TfrmTimeEntry.FillProjectTaskLists(Select: boolean; TaskListID: integer);
var
  i: integer;
  tasklists: TJSONArray;
  proj_id: integer;
  pass1: boolean;
  search: string;
begin
  lbProjectTaskLists.Clear;
  if lbProjects.Items.Count = 0 then
    exit;
  tasklists := PaymoInstance.TaskListsArray;

  proj_id := TJSONData(lbProjects.Items.Objects[lbProjects.ItemIndex]).GetPath('id').AsInteger;
  pass1 := UTF8Length(editSearchTaskLists.Text) = 0;
  search := UTF8LowerCase(editSearchTaskLists.Text);
  for i := 0 to tasklists.Count - 1 do
  begin
    if (proj_id = tasklists[i].GetPath('project_id').AsInteger) and
      ((pass1) or (UTF8Pos(search,
      UTF8LowerCase(tasklists[i].GetPath('name').AsString)) <> 0)) then
    begin
      lbProjectTaskLists.AddItem(tasklists[i].GetPath('name').AsString, tasklists[i]);
      if TaskListID = tasklists[i].GetPath('id').AsInteger then
        lbProjectTaskLists.ItemIndex := lbProjectTaskLists.Items.Count - 1;
    end;
  end;

  if (TaskListID = 0) and (lbProjectTaskLists.Items.Count > 0) then
    lbProjectTaskLists.ItemIndex := 0;

  if Select then
    if lbProjectTaskLists.ItemIndex > -1 then
      editSearchTaskLists.Text := lbProjectTaskLists.Items[lbProjectTaskLists.ItemIndex]
    else
      editSearchTaskLists.Text := '';
end;

procedure TfrmTimeEntry.FillProjectTasks(Select: boolean; FromData: boolean);
var
  i, proj_id, task_id: integer;
  tasks: TJSONArray;
  pass1: boolean;
  search: string;
begin
  lbProjectTasks.Clear;
  if lbProjects.Items.Count = 0 then
    exit;
  tasks := PaymoInstance.TasksArray;
  proj_id := TJSONData(lbProjects.Items.Objects[lbProjects.ItemIndex]).GetPath('id').AsInteger;

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
      lbProjectTasks.AddItem(tasks[i].GetPath('name').AsString, tasks[i]);
    if task_id = tasks[i].GetPath('id').AsInteger then
    begin
      lbProjectTasks.ItemIndex := lbProjectTasks.Items.Count - 1;
      Data_TaskListID := tasks[i].GetPath('tasklist_id').AsInteger;
    end;
  end;

  if (not FromData) and (lbProjectTasks.Items.Count > 0) then
    lbProjectTasks.ItemIndex := 0;

  if Select then
    if lbProjectTasks.ItemIndex > -1 then
      editSearchTasks.Text := lbProjectTasks.Items[lbProjectTasks.ItemIndex]
    else
      editSearchTasks.Text := '';
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

procedure TfrmTimeEntry.CloseListBox(Keep: TListBox);
var
  n: string;
begin
  if Assigned(Keep) then
    n := Keep.Name
  else
    n := '';

  if lbProjects.Name = n then
    lbProjects.Visible := True
  else
    lbProjects.Visible := False;

  if lbProjectTasks.Name = n then
    lbProjectTasks.Visible := True
  else
    lbProjectTasks.Visible := False;

  if lbProjectTaskLists.Name = n then
    lbProjectTaskLists.Visible := True
  else
    lbProjectTaskLists.Visible := False;
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
    lbProjectTasks.Visible := False;
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
    lbProjectTasks.Visible := True;
    btnDeleteEntry.Visible := True;
    pnlGroup.Visible := True;
    btnSaveEntry.Caption := rsSaveEntry;
    btnSaveEntry.OnClick := @btnSaveEntryClick;
    memoDescription.Visible := False;
    lblDescription.Visible := False;
    lblDescription3.Visible := True;
    editSearchTasks.Visible := True;
    {if editSearchProject.CanSetFocus then
      editSearchProject.SetFocus;}
    btnExistingTask.Visible := False;
  end;

  ProjectExit := True;
  TaskListExit := True;
  TaskExit := True;
  FillProjectsCombo(True);
  FillProjectTaskLists(True, Data_TaskListID);
  FillProjectTasks(True, True);
  FillDateAndTime;

  lbProjects.Visible := False;
  lbProjectTasks.Visible := False;
  lbProjectTaskLists.Visible := False;
end;

procedure TfrmTimeEntry.editSearchProjectEnter(Sender: TObject);
begin
  ProjectExit := False;
  CloseListBox(lbProjects);
  lbProjects.BringToFront;
  lbProjects.Height := editSearchProject.Height * 4;
end;

procedure TfrmTimeEntry.editSearchProjectExit(Sender: TObject);
begin
  ProjectExit := True;
  lbProjects.Visible := False;
  if lbProjects.ItemIndex > -1 then
    editSearchProject.Text := lbProjects.Items[lbProjects.ItemIndex];
end;

procedure TfrmTimeEntry.editSearchProjectKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  iPos: integer;
begin
  if (key = VK_DOWN) then
  begin
    key := 0;
    if (lbProjects.Items.Count > 0) then
    begin
      iPos := lbProjects.ItemIndex;
      Inc(iPos);
      if (iPos > lbProjects.Items.Count - 1) then
        iPos := 0;
      lbProjects.ItemIndex := iPos;
    end;
    exit;
  end;
  if (key = VK_UP) then
  begin
    key := 0;
    if (lbProjects.Items.Count > 0) then
    begin
      iPos := lbProjects.ItemIndex;
      Dec(iPos);
      if (iPos < 0) then
        iPos := lbProjects.Items.Count - 1;
      lbProjects.ItemIndex := iPos;
    end;
    exit;
  end;
  if Key = VK_RETURN then
  begin
    editSearchProjectExit(nil);
    editSearchTaskLists.SetFocus;
    exit;
  end;
end;

procedure TfrmTimeEntry.editSearchTaskListsChange(Sender: TObject);
begin
  FillProjectTasks(False);
  if not TaskListExit then
    FillProjectTaskLists();
end;

procedure TfrmTimeEntry.editSearchTaskListsClick(Sender: TObject);
begin
  TaskListExit := False;
  CloseListBox(lbProjectTaskLists);
end;

procedure TfrmTimeEntry.editSearchTaskListsEnter(Sender: TObject);
begin
  TaskListExit := False;
  CloseListBox(lbProjectTaskLists);
  lbProjectTaskLists.BringToFront;
  lbProjectTaskLists.Height := editSearchTaskLists.Height * 4;
end;

procedure TfrmTimeEntry.editSearchTaskListsExit(Sender: TObject);
begin
  TaskListExit := True;
  lbProjectTaskLists.Visible := False;
  if lbProjectTaskLists.ItemIndex > -1 then
    editSearchTaskLists.Text := lbProjectTaskLists.Items[lbProjectTaskLists.ItemIndex];
end;

procedure TfrmTimeEntry.editSearchProjectChange(Sender: TObject);
begin
  if not ProjectExit then
    FillProjectsCombo;
  FillProjectTasks(False);
  FillProjectTaskLists();
end;

procedure TfrmTimeEntry.editSearchTaskListsKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  iPos: integer;
begin
  if (key = VK_DOWN) then
  begin
    key := 0;
    if (lbProjectTaskLists.Items.Count > 0) then
    begin
      iPos := lbProjectTaskLists.ItemIndex;
      Inc(iPos);
      if (iPos > lbProjectTaskLists.Items.Count - 1) then
        iPos := 0;
      lbProjectTaskLists.ItemIndex := iPos;
    end;
    exit;
  end;
  if (key = VK_UP) then
  begin
    key := 0;
    if (lbProjectTaskLists.Items.Count > 0) then
    begin
      iPos := lbProjectTaskLists.ItemIndex;
      Dec(iPos);
      if (iPos < 0) then
        iPos := lbProjectTaskLists.Items.Count - 1;
      lbProjectTaskLists.ItemIndex := iPos;
    end;
    exit;
  end;
  if Key = VK_RETURN then
  begin
    editSearchTaskListsExit(nil);
    if editSearchTasks.Visible then
      editSearchTasks.SetFocus
    else if memoDescription.Visible then
      memoDescription.SetFocus;
    exit;
  end;
end;

procedure TfrmTimeEntry.editSearchTasksChange(Sender: TObject);
begin
  if not TaskExit then
    FillProjectTasks(False);
end;

procedure TfrmTimeEntry.editSearchTasksClick(Sender: TObject);
begin
  TaskExit := False;
  CloseListBox(lbProjectTasks);
end;

procedure TfrmTimeEntry.editSearchTasksEnter(Sender: TObject);
begin
  TaskExit := False;
  CloseListBox(lbProjectTasks);
  lbProjectTasks.BringToFront;
  lbProjectTasks.Height := editSearchTasks.Height * 4;
end;

procedure TfrmTimeEntry.editSearchTasksExit(Sender: TObject);
begin
  TaskExit := True;
  lbProjectTasks.Visible := False;
  if lbProjectTasks.ItemIndex > -1 then
    editSearchTasks.Text := lbProjectTasks.Items[lbProjectTasks.ItemIndex];
end;

procedure TfrmTimeEntry.editSearchTasksKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
var
  iPOs: integer;
begin
  if (key = VK_DOWN) then
  begin
    key := 0;
    if (lbProjectTasks.Items.Count > 0) then
    begin
      iPos := lbProjectTasks.ItemIndex;
      Inc(iPos);
      if (iPos > lbProjectTasks.Items.Count - 1) then
        iPos := 0;
      lbProjectTasks.ItemIndex := iPos;
    end;
    exit;
  end;
  if (key = VK_UP) then
  begin
    key := 0;
    if (lbProjectTasks.Items.Count > 0) then
    begin
      iPos := lbProjectTasks.ItemIndex;
      Dec(iPos);
      if (iPos < 0) then
        iPos := lbProjectTasks.Items.Count - 1;
      lbProjectTasks.ItemIndex := iPos;
    end;
    exit;
  end;
  if Key = VK_RETURN then
  begin
    editSearchTasksExit(nil);
    exit;
  end;
end;

procedure TfrmTimeEntry.FormClick(Sender: TObject);
begin
  CloseListBox(nil);
end;

procedure TfrmTimeEntry.FormCreate(Sender: TObject);
begin
  frmMain.SetFonts(Self);
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
    (lbProjects.ItemIndex >= 0) and (lbProjectTaskLists.ItemIndex >= 0) and
    (lbProjectTasks.ItemIndex >= 0);
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
      t_end, TJSONData(lbProjects.Items.Objects[lbProjects.ItemIndex]).GetPath('id').AsString, TJSONData(
      lbProjectTasks.Items.Objects[lbProjectTasks.ItemIndex]).GetPath('id').AsString,
      TJSONData(lbProjectTaskLists.Items.Objects[lbProjectTaskLists.ItemIndex]).GetPath(
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
    canSave := (lbProjects.ItemIndex >= 0) and (lbProjectTaskLists.ItemIndex >= 0);
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
    canSave := (lbProjects.ItemIndex >= 0) and (lbProjectTaskLists.ItemIndex >= 0) and
      (lbProjectTasks.ItemIndex >= 0);
    if not CanSave then
    begin
      ShowMessage(rsPleaseFillAllFields);
      exit();
    end;
  end;

  if memoDescription.Visible then
  begin
    r := PaymoInstance.CreateTask(memoDescription.Lines.Text, '',
      TJSONData(lbProjectTaskLists.Items.Objects[lbProjectTaskLists.ItemIndex]).GetPath('id').AsInteger, task);
  end
  else
  begin
    r := prOK;
    task := TJSONData(lbProjectTasks.Items.Objects[lbProjectTasks.ItemIndex]);
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

procedure TfrmTimeEntry.editSearchProjectClick(Sender: TObject);
begin
  ProjectExit := False;
  CloseListBox(lbProjects);
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
  //lbProjectTasks.Visible := True;
  editSearchTasks.Visible := True;
  lblDescription3.Visible := True;
  lblDescription.Visible := False;
  memoDescription.Visible := False;
  editSearchTasks.Clear;
  if editSearchTasks.CanFocus then
    editSearchTasks.SetFocus;
  editSearchTasks.SendToBack;
end;

end.
