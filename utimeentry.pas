unit utimeentry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  DateUtils, upaymo, fpjson, utasklist, ColorSpeedButton, LMessages,
  uresourcestring;

type

  { TfrmTimeEntry }

  TfrmTimeEntry = class(TForm)
    btnDeleteEntry: TColorSpeedButton;
    cbProjectTasks: TComboBox;
    cbProjects: TComboBox;
    cbProjectTaskLists: TComboBox;
    btnSaveEntry: TColorSpeedButton;
    dlgDate: TCalendarDialog;
    time_start_hh: TEdit;
    time_end_hh: TEdit;
    time_start_mm: TEdit;
    time_end_mm: TEdit;
    time_start_separator: TLabel;
    time_end_separator: TLabel;
    time_start_separator1: TLabel;
    lbl_date: TLabel;
    procedure btnDeleteEntryClick(Sender: TObject);
    procedure btnSaveEntryClick(Sender: TObject);
    procedure cbProjectsChange(Sender: TObject);
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
begin
  cbProjects.Clear;
  projects := PaymoInstance.ProjectsArray;

  if data <> nil then
    id := data.GetPath('project_id').AsInteger
  else
    id := 0;

  for i:=0 to projects.Count-1 do
  begin
    cbProjects.AddItem(projects[i].GetPath('name').AsString, projects[i]);
    if id = projects[i].GetPath('id').AsInteger then
      cbProjects.ItemIndex := i;
  end;
end;

procedure TfrmTimeEntry.FillProjectTaskLists(TaskListID: integer = 0);
var
  i: integer;
  tasklists: TJSONArray;
  proj_id: integer;
begin
  cbProjectTaskLists.Clear;
  tasklists := PaymoInstance.TaskListsArray;

  proj_id := TJSONData(cbProjects.Items.Objects[cbProjects.ItemIndex]).GetPath('id').AsInteger;

  for i:=0 to tasklists.Count-1 do
  begin
    if (proj_id = tasklists[i].GetPath('project_id').AsInteger) then
    begin
    cbProjectTaskLists.AddItem(tasklists[i].GetPath('name').AsString, tasklists[i]);
    if TaskListID = tasklists[i].GetPath('id').AsInteger then
      cbProjectTaskLists.ItemIndex := cbProjectTaskLists.Items.Count-1;
    end;
  end;

  if TaskListID = 0 then
    cbProjectTaskLists.ItemIndex := 0;
end;

procedure TfrmTimeEntry.FillProjectTasks(FromData: boolean);
var
  i, proj_id, task_id: integer;
  tasks: TJSONArray;
begin
  cbProjectTasks.Clear;
  tasks := PaymoInstance.TasksArray;
  proj_id := TJSONData(cbProjects.Items.Objects[cbProjects.ItemIndex]).GetPath('id').AsInteger;

  if (data <> nil) and (FromData) then
  begin
    task_id := data.GetPath('task_id').AsInteger;
    proj_id := data.GetPath('project_id').AsInteger;
  end
  else
    task_id := 0;

  for i:=0 to tasks.Count-1 do
  begin
    if (proj_id = tasks[i].GetPath('project_id').AsInteger) {and (not tasks[i].GetPath('complete').AsBoolean)} then
      cbProjectTasks.AddItem(tasks[i].GetPath('name').AsString, tasks[i]);
    if task_id = tasks[i].GetPath('id').AsInteger then
    begin
      cbProjectTasks.ItemIndex := cbProjectTasks.Items.Count-1;
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
  if data <> nil then
  begin
    tempTime := TTaskList.StringToDateTime(data.GetPath('start_time').AsString);
    dlgDate.Date := tempTime;
    time_start_hh.Caption := FormatDateTime('hh', tempTime);
    time_start_mm.Caption := FormatDateTime('nn', tempTime);
    tempTime := TTaskList.StringToDateTime(data.GetPath('end_time').AsString);
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

procedure TfrmTimeEntry.btnSaveEntryClick(Sender: TObject);
var
  canSave: boolean;
  t_start, t_end: TDateTime;
  s_hh, s_mm: string;
begin
  // required fields
  canSave := (time_start_hh.Text <> '') and (time_start_mm.Text <> '') and (time_end_hh.Text <> '') and (time_end_mm.Text <> '');
  if not CanSave then
  begin
    ShowMessage(rsPleaseFillAllTimeFields);
    exit();
  end;
  // start time
  s_hh := time_start_hh.Text;
  s_mm := time_start_mm.Text;
  t_start := dlgDate.Date;
  ReplaceTime(t_start, EncodeTime(s_hh.ToInteger,s_mm.ToInteger,0,0));
  // end time
  s_hh := time_end_hh.Text;
  s_mm := time_end_mm.Text;
  t_end := dlgDate.Date;
  ReplaceTime(t_end, EncodeTime(s_hh.ToInteger,s_mm.ToInteger,0,0));
  // required time between
  if SecondsBetween(t_start, t_end) < 60 then
  begin
    ShowMessage(rsThereMustBeAtLeastAMinuteOfDifferenceBetweenStartAndEndTime);
    exit();
  end;
end;

procedure TfrmTimeEntry.btnDeleteEntryClick(Sender: TObject);
begin
  case PaymoInstance.DeleteTimeEntry(data.GetPath('id').AsString) of
    prOK: begin
      Self.Close;
      Application.ProcessMessages;
      // Sync for now, ToDo: change to async with tasks
      PaymoInstance.GetTasks();
      frmMain.DownloadTasksFinish(nil, 0, 0);
    end;
    prTRYAGAIN, prERROR: begin
      ShowMessage(rsErrorCantDeleteTimeEntry);
    end;
  end;
end;

end.

