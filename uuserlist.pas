unit uuserlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, fpjson, upaymo, utasklist, uresourcestring;

type

  { TfrmUserList }

  TfrmUserList = class(TForm)
    chkAutoUpdate: TCheckBox;
    ListView1: TListView;
    Panel1: TPanel;
    tmrRefresh: TTimer;
    ToolBar1: TToolBar;
    btnRefresh: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure tmrRefreshTimer(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
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

procedure TfrmUserList.btnRefreshClick(Sender: TObject);
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

procedure TfrmUserList.ListView1AdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
   if Item.SubItems.Count>0 then
   begin
       Sender.Canvas.Brush.Color := $F6ECB7;
       Sender.Canvas.Font.Style:=[fsBold];
   end
   else
   begin
       Sender.Canvas.Brush.Color := $CDCDCD;
       Sender.Canvas.Font.Style:=[];
   end;
end;

procedure TfrmUserList.tmrRefreshTimer(Sender: TObject);
begin
  if (chkAutoUpdate.Checked) and (btnRefresh.Enabled) then
     btnRefresh.Click;
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

