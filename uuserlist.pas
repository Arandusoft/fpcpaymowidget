unit uuserlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, fpjson, upaymo, utasklist, uresourcestring;

type

  { TfrmUserList }

  TfrmUserList = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
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

procedure TfrmUserList.ListUsers;
var
  users: TJSONData;
  timers: TJSONArray;
  i: integer;
  s: string;
  task: TJSONData;
begin
  ListBox1.Clear;
  users := frmMain.Paymo.Users;
  timers := frmMain.Paymo.UsersRunningTimer;
  for i:=0 to users.Count-1 do
  begin
    s := users.Items[i].GetPath('name').AsString;
    if (timers.Items[i].GetPath('entries').Count = 1) then
    begin
      task := frmMain.Paymo.GetSingleTask(timers.Items[i].GetPath('entries').Items[0].GetPath('task_id').AsInteger);
      ListBox1.Items.Add(s + ' - ' + rsSince + ' ' + FormatDateTime('hh:nn', TTaskList.StringToDateTime(timers[i].GetPath('entries').Items[0].GetPath('start_time').AsString)) + ' ' + rsWorkingOn + ' ' + frmMain.Paymo.GetProjectName(task.GetPath('tasks').Items[0].GetPath('project_id').AsInt64) + ' [' + task.GetPath('tasks').Items[0].GetPath('name').AsString + ']');
      task.Free;
    end
    else
      ListBox1.Items.Add(s);
  end;
end;

end.

