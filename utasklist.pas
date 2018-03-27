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
    procedure OnMouseEnterTimeLabel(Sender: TObject);
    procedure OnMouseLeaveTimeLabel(Sender: TObject);
    procedure SetFItems(AValue: TJSONArray);
    procedure SetFPaymo(AValue: TPaymo);
  protected
    function SecondsToString(Seconds: integer): string;
    function StringToDateTime(DateTime: string): TDateTime;
    procedure OnClickItem(Sender: TObject);
    procedure OnClickItemParent(Sender: TObject);
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

procedure TTaskList.OnMouseEnterTimeLabel(Sender: TObject);
begin
  TLabel(Sender).Font.Color := RGBToColor(57, 202, 84);
end;

procedure TTaskList.OnMouseLeaveTimeLabel(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clGray;
end;

procedure TTaskList.OnClickItemParent(Sender: TObject);
begin
  OnClickItem(TControl(Sender).Parent);
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

function TTaskList.StringToDateTime(DateTime: string): TDateTime;
begin
 Result := ScanDateTime('yyyy-mm-dd"T"hh:nn:ss', Copy(DateTime, 1, 19));
 Result := UniversalTimeToLocal(Result);
end;

procedure TTaskList.OnClickItem(Sender: TObject);
var
  p: TControl;
begin
  p := TControl(TControl(Sender).Parent.FindComponent('entries'));
  p.Visible := not p.Visible;
  p := TControl(TControl(Sender).Parent.FindComponent('arrow_container').FindComponent('arrow'));
  if p.Caption = '˅' then
    p.Caption := '˄'
  else
    p.Caption := '˅';
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
    // title and arrow container
    pc := TPanel.Create(p);
    pc.BevelOuter := bvNone;
    pc.Align := alTop;
    pc.AutoSize := True;
    pc.Name := 'arrow_container';
    pc.Caption := '';
    pc.Parent := p;
    // title
    l := TLabel.Create(pc);
    l.Cursor := crHandPoint;
    l.Font.Color := clGray;
    l.Font.Size := -12;
    l.Align := alLeft;
    l.Caption := FPaymo.GetProjectName(arr[i].GetPath('project_id').AsInteger);
    l.OnClick := @OnClickItemParent;
    l.Parent := pc;
    // arrow
    l := TLabel.Create(pc);
    l.Cursor := crHandPoint;
    l.Font.Color := clGray;
    l.Font.Size := -12;
    l.Font.Name := 'Courier New';
    l.Align := alRight;
    l.Alignment := taRightJustify;
    l.Name := 'arrow';
    l.Caption := '˅';
    l.OnClick := @OnClickItemParent;
    l.Parent := pc;
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
    lt.OnClick:=@OnClickItemParent;
    lt.Parent := pc;
    // time entries container
    e := TPanel.Create(p);
    e.BevelOuter := bvNone;
    e.Align := alBottom;
    e.AutoSize := True;
    e.Visible := False;
    e.Name := 'entries';
    e.Caption := '';
    e.ChildSizing.ControlsPerLine := 2;
    e.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    e.ChildSizing.EnlargeHorizontal := crsScaleChilds;
    e.ChildSizing.VerticalSpacing := 20;
    e.ChildSizing.TopBottomSpacing := 10;
    e.Parent := p;
    // time entries
    arrEntries := TJSONArray(arr[i].GetPath('entries'));
    sum := 0;
    for j := 0 to arrEntries.Count - 1 do
    begin
      // start - end time label
      l := TLabel.Create(e);
      l.Cursor := crHandPoint;
      l.Font.Color := clGray;
      l.Font.Size := -12;
      l.Alignment := taLeftJustify;
      l.Caption := FormatDateTime('t', StringToDateTime(arrEntries[j].GetPath('start_time').AsString)) + ' ‒ ' + FormatDateTime('t', StringToDateTime(arrEntries[j].GetPath('end_time').AsString));;
      l.OnMouseEnter:=@OnMouseEnterTimeLabel;
      l.OnMouseLeave:=@OnMouseLeaveTimeLabel;
      l.Parent := e;
      // entry time label
      l := TLabel.Create(e);
      l.Font.Color := clGray;
      l.Font.Size := -12;
      l.Alignment := taRightJustify;
      l.Caption := SecondsToString(arrEntries[j].GetPath('duration').AsInteger);
      l.Parent := e;
      // sum of all time entries
      sum := sum + arrEntries[j].GetPath('duration').AsInteger;
    end;
    // sum of all time entries
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
