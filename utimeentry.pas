unit utimeentry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  DateUtils, upaymo, fpjson, utasklist, LMessages;

type

  { TfrmTimeEntry }

  TfrmTimeEntry = class(TForm)
    cbProjectTasks: TComboBox;
    cbProjects: TComboBox;
    dlgDate: TCalendarDialog;
    time_start_hh: TEdit;
    time_end_hh: TEdit;
    time_start_mm: TEdit;
    time_end_mm: TEdit;
    time_start_separator: TLabel;
    time_end_separator: TLabel;
    time_start_separator1: TLabel;
    lbl_date: TLabel;
    procedure FormShow(Sender: TObject);
    procedure lbl_dateClick(Sender: TObject);
  private
    procedure FillProjectsCombo;
    procedure FillProjectTasks;
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

procedure TfrmTimeEntry.FillProjectTasks;
var
  i, proj_id, task_id: integer;
  tasks: TJSONArray;
begin
  cbProjectTasks.Clear;
  tasks := PaymoInstance.TasksArray;
  proj_id := TJSONData(cbProjects.Items.Objects[cbProjects.ItemIndex]).GetPath('id').AsInteger;

  if data <> nil then
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
      cbProjectTasks.ItemIndex := cbProjectTasks.Items.Count-1;
  end;
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
  FillProjectsCombo;
  FillProjectTasks;
  FillDateAndTime;
end;

procedure TfrmTimeEntry.FormShow(Sender: TObject);
begin

end;

end.

