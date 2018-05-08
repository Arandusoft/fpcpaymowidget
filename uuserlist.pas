unit uuserlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, fpjson, upaymo, utasklist, uresourcestring;

type

  { TfrmUserList }

  TfrmUserList = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private

  public
    procedure ListUsers;
  end;

var
  frmUserList: TfrmUserList;

implementation
uses
  umain;

{$R *.lfm}

{ TfrmUserList }

procedure TfrmUserList.ToolButton1Click(Sender: TObject);
begin
  if frmMain.Paymo.GetUsers() = prOK then
  begin
    if frmMain.Paymo.GetAllUsersRunningTimer() = prOK then
    begin
      Panel1.Caption := rsLastSincro + ' ' + FormatDateTime('hh:nn', now);
      ListUsers;
    end;
  end;
end;

procedure TfrmUserList.FormShow(Sender: TObject);
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

procedure TfrmUserList.ListUsers;
var
  users: TJSONData;
  timers: TJSONArray;
  i: integer;
  s: string;
  task: TJSONData;
  item: TListItem;
begin
  ListView1.Clear;
  users := frmMain.Paymo.Users;
  timers := frmMain.Paymo.UsersRunningTimer;
  for i:=0 to users.Count-1 do
  begin
    s := users.Items[i].GetPath('name').AsString;
    item := ListView1.Items.Add;
    item.Caption := s;
    if (timers.Items[i].GetPath('entries').Count = 1) then
    begin
      task := frmMain.Paymo.GetSingleTask(timers.Items[i].GetPath('entries').Items[0].GetPath('task_id').AsInteger);
      item.SubItems.Add(rsSince + ' ' + FormatDateTime('hh:nn', TTaskList.StringToDateTime(timers[i].GetPath('entries').Items[0].GetPath('start_time').AsString)) + ' ' + rsWorkingOn + ' ' + frmMain.Paymo.GetProjectName(task.GetPath('tasks').Items[0].GetPath('project_id').AsInt64) + ' [' + task.GetPath('tasks').Items[0].GetPath('name').AsString + ']');
      task.Free;
    end;
  end;
end;

end.

