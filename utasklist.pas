unit utasklist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, fpjson, StdCtrls, Graphics, Forms,
  upaymo, Dialogs, DateUtils;

type

  { TTaskList }

  TTaskList = class(TScrollBox)
  private
    FItems: TJSONArray;
    FPaymo: TPaymo;
    procedure SetFItems(AValue: TJSONArray);
    procedure SetFPaymo(AValue: TPaymo);
  protected
    function SecondsToString(Seconds: integer): string;
    procedure OnClickItem(Sender: TObject);
    procedure OnClickItemTime(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  public
    property PaymoInstance: TPaymo read FPaymo write SetFPaymo;
    procedure RefreshItems;
  end;

implementation

{ TTaskList }

procedure TTaskList.SetFItems(AValue: TJSONArray);
begin
  if FItems = AValue then
    Exit;
  FItems := AValue;
  RefreshItems;
end;

procedure TTaskList.OnClickItemTime(Sender: TObject);
var
  p: TControl;
begin
  p := TControl(TControl(Sender).Parent.Parent.FindComponent('entries'));
  p.Visible := not p.Visible;
end;

procedure TTaskList.SetFPaymo(AValue: TPaymo);
begin
  if FPaymo = AValue then
    Exit;
  FPaymo := AValue;
end;

function TTaskList.SecondsToString(Seconds: integer): string;
var
  t, t2: string;
begin
  t := trunc(Seconds / 3600).ToString();
  t2 := trunc(Frac(Seconds / 3600) * 60).ToString();

  if t2.Length = 1 then
    Result := t + ':0' + t2
  else
    Result := t + ':' + t2;
end;

procedure TTaskList.OnClickItem(Sender: TObject);
var
  p: TControl;
begin
  p := TControl(TControl(Sender).Parent.FindComponent('entries'));
  p.Visible := not p.Visible;
end;

procedure TTaskList.RefreshItems;
var
  i, j, sum: integer;
  p, pc, e: TPanel;
  l, lt: TLabel;
  arr, arrEntries: TJSONArray;
begin
  for i := Self.ControlCount - 1 downto 0 do
    Self.Controls[i].Free;

  arr := PaymoInstance.TasksArray;

  for i := 0 to arr.Count - 1 do
  begin
    // container
    p := TPanel.Create(Self);
    p.BevelOuter := bvNone;
    p.BorderSpacing.Left := 40;
    p.BorderSpacing.Top := 20;
    p.BorderSpacing.Right := 40;
    p.BorderSpacing.Bottom := 20;
    p.Align := alTop;
    p.AutoSize := True;
    p.Parent := Self;
    // title
    l := TLabel.Create(p);
    l.Cursor := crHandPoint;
    l.Font.Color := clGray;
    l.Font.Size := -12;
    l.Align := alTop;
    l.Caption := FPaymo.GetProjectName(arr[i].GetPath('project_id').AsInteger);
    l.OnClick := @OnClickItem;
    l.Parent := p;
    // name
    l := TLabel.Create(p);
    l.Cursor := crHandPoint;
    l.Font.Color := clBlack;
    l.Font.Size := -14;
    l.Align := alClient;
    l.WordWrap := True;
    if arr[i].GetPath('complete').AsBoolean then
      l.Font.Style := [fsStrikeOut];
    l.Caption := arr[i].GetPath('name').AsString;
    l.OnClick := @OnClickItem;
    l.Parent := p;
    // sum of time entries container
    pc := TPanel.Create(p);
    pc.BevelOuter := bvNone;
    pc.Align := alRight;
    pc.AutoSize := True;
    pc.Parent := p;
    // sum of time entries
    lt := TLabel.Create(pc);
    lt.Cursor := crHandPoint;
    lt.Font.Color := clGray;
    lt.Font.Size := -12;
    lt.Alignment := taRightJustify;
    lt.Align := alBottom;
    lt.OnClick:=@OnClickItemTime;
    lt.Parent := pc;
    // time entries container
    e := TPanel.Create(p);
    e.BevelOuter := bvNone;
    e.Align := alBottom;
    e.AutoSize := True;
    e.Visible := False;
    e.Name := 'entries';
    e.Caption := '';
    e.Parent := p;
    // time entries
    arrEntries := TJSONArray(arr[i].GetPath('entries'));
    sum := 0;
    for j := 0 to arrEntries.Count - 1 do
    begin
      l := TLabel.Create(e);
      l.Font.Color := clGray;
      l.Font.Size := -12;
      l.Alignment := taRightJustify;
      l.Caption := SecondsToString(arrEntries[j].GetPath('duration').AsInteger);
      l.Align := alTop;
      l.Parent := e;
      sum := sum + arrEntries[j].GetPath('duration').AsInteger;
    end;
    lt.Caption := SecondsToString(sum);
  end;
end;

constructor TTaskList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BorderStyle := bsNone;
  Self.HorzScrollBar.Visible := False;
  Self.VertScrollBar.Smooth := True;
  Self.VertScrollBar.Tracking := True;
end;

end.
