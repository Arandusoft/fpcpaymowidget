unit uuserlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  fpjson, upaymo;

type

  { TfrmUserList }

  TfrmUserList = class(TForm)
    ListBox1: TListBox;
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
    //ShowMessage('ok');
    //ShowMessage(frmMain.Paymo.Users.FormatJSON());
    ListUsers;
  end;
end;

procedure TfrmUserList.ListUsers;
var
  users: TJSONData;
  i: integer;
begin
  ListBox1.Clear;
  users := frmMain.Paymo.Users;
  for i:=0 to users.Count-1 do
  begin
    ListBox1.Items.Add(users.Items[i].GetPath('name').AsString);
  end;
end;

end.

