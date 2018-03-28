unit utasklist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, fpjson, StdCtrls, Graphics, Forms,
  upaymo, Dialogs, DateUtils, uanimatedpanel, LazUTF8, Math;

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
    function IsSameDate(Date1, Date2: TDateTime): boolean;
    procedure OnClickItem(Sender: TObject);
    procedure OnClickItemParent(Sender: TObject);
    procedure DayClick(Sender: TObject);
    procedure DayClickParent(Sender: TObject);
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

procedure TTaskList.DayClickParent(Sender: TObject);
var
  p: TAnimatedPanel;
begin
  p := TAnimatedPanel(TControl(Sender).Parent.Parent);
  p.Animate();
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
  hh, mins, ss: DWord;
begin
  DivMod(Seconds, SecsPerMin, mins, ss);
  DivMod(mins, MinsPerHour, hh, mins);
  Result := Format('%.2d:%.2d', [hh, mins]);
end;

function TTaskList.StringToDateTime(DateTime: string): TDateTime;
begin
  Result := ScanDateTime('yyyy-mm-dd"T"hh:nn:ss', Copy(DateTime, 1, 19));
  Result := UniversalTimeToLocal(Result);
end;

function TTaskList.IsSameDate(Date1, Date2: TDateTime): boolean;
var
  y1, m1, d1, y2, m2, d2: word;
begin
  DecodeDate(Date1, y1, m1, d1);
  DecodeDate(Date2, y2, m2, d2);
  Result := (y1 = y2) and (m1 = m2) and (d1 = d2);
end;

procedure TTaskList.OnClickItem(Sender: TObject);
var
  p: TAnimatedPanel;
  c: TControl;
begin
  p := TAnimatedPanel(TControl(Sender).Parent.FindComponent('entries'));
  p.Animate();

  c := TControl(TControl(Sender).Parent.FindComponent(
    'arrow_container').FindComponent('arrow'));
  if c.Caption = '˅' then
    c.Caption := '˄'
  else
    c.Caption := '˅';
end;

procedure TTaskList.RefreshItems;
var
  i, j, k, sum, sec: integer;
  d, p, pc, e: TPanel;
  l, lt: TLabel;
  arr, arrEntries, arrFilteredEntries: TJSONArray;
  sl: TStringList;
  tempstr: string;
  t: TDateTime;
begin
  for i := Self.ControlCount - 1 downto 0 do
    Self.Controls[i].Free;

  arr := PaymoInstance.TasksArray;

  // list of days
  sl := TStringList.Create;
  for i := 0 to arr.Count - 1 do
  begin
    // all time entries
    arrEntries := TJSONArray(arr[i].GetPath('entries'));
    for j := 0 to arrEntries.Count - 1 do
    begin
      // hide time entries of other users
      if arrEntries[j].GetPath('user_id').AsInteger <>
        PaymoInstance.MyData[0].GetPath('id').AsInteger then
        Continue;
      // add only new items
      tempstr := FormatDateTime('dd mm yyyy',
        StringToDateTime(arrEntries[j].GetPath('start_time').AsString));
      if (sl.IndexOf(tempstr) = -1) then
        sl.Add(tempstr);
    end;
  end;

  for k := 0 to sl.Count - 1 do
  begin
    // time to compare
    t := ScanDateTime('dd mm yyyy', sl[k]);

    // day panel
    d := TAnimatedPanel.Create(Self);
    d.BevelOuter := bvNone;
    d.Align := alTop;
    d.BorderSpacing.Top := 20;
    d.AutoSize := True;
    //d.OnClick := @DayClick;
    d.Parent := Self;

    for i := 0 to arr.Count - 1 do
    begin
      // all time entries
      arrEntries := TJSONArray(arr[i].GetPath('entries'));
      // filtered time entries
      arrFilteredEntries := TJSONArray.Create;
      for j := 0 to arrEntries.Count - 1 do
      begin
        // hide time entries of other users
        if arrEntries[j].GetPath('user_id').AsInteger <>
          PaymoInstance.MyData[0].GetPath('id').AsInteger then
          Continue;
        // show only items that match time
        if not IsSameDate(StringToDateTime(arrEntries[j].GetPath('start_time').AsString),
          t) then
          Continue;
        // add entries to filtered list
        arrFilteredEntries.Add(arrEntries[j].Clone);
      end;
      // hide "empty time" entries
      if (arrFilteredEntries.Count = 0) then
      begin
        arrFilteredEntries.Clear;
        arrFilteredEntries.Free;
        Continue;
      end;
      // container
      p := TPanel.Create(d);
      p.BevelOuter := bvNone;
      p.BorderSpacing.Left := 40;
      p.BorderSpacing.Right := 40;
      p.BorderSpacing.Bottom := 20;
      p.Align := alTop;
      p.AutoSize := True;
      p.Parent := d;
      // title and arrow container
      pc := TPanel.Create(p);
      pc.Cursor := crHandPoint;
      pc.BevelOuter := bvNone;
      pc.Align := alTop;
      pc.AutoSize := True;
      pc.Name := 'arrow_container';
      pc.Caption := '';
      pc.OnClick := @OnClickItem;
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
      pc.Cursor := crHandPoint;
      pc.BevelOuter := bvNone;
      pc.Align := alRight;
      pc.AutoSize := True;
      pc.OnClick := @OnClickItem;
      pc.Parent := p;
      // sum of time entries
      lt := TLabel.Create(pc);
      lt.Cursor := crHandPoint;
      lt.Font.Color := clGray;
      lt.Font.Size := -12;
      lt.Alignment := taRightJustify;
      lt.Align := alBottom;
      lt.OnClick := @OnClickItemParent;
      lt.Parent := pc;
      // time entries container
      e := TAnimatedPanel.Create(p);
      e.BevelOuter := bvNone;
      e.Align := alBottom;
      e.Height := 0;
      e.Name := 'entries';
      e.Caption := '';
      e.ChildSizing.ControlsPerLine := 2;
      e.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
      e.ChildSizing.EnlargeHorizontal := crsScaleChilds;
      e.ChildSizing.VerticalSpacing := 20;
      e.ChildSizing.TopBottomSpacing := 10;
      e.Parent := p;
      // time entries
      sum := 0;
      for j := arrFilteredEntries.Count - 1 downto 0 do
      begin
        // start - end time label
        l := TLabel.Create(e);
        l.Cursor := crHandPoint;
        l.Font.Color := clGray;
        l.Font.Size := -12;
        l.Alignment := taLeftJustify;
        l.Caption := FormatDateTime('t', StringToDateTime(
          arrFilteredEntries[j].GetPath('start_time').AsString)) +
          ' ‒ ' + FormatDateTime('t', StringToDateTime(
          arrFilteredEntries[j].GetPath('end_time').AsString));
        l.OnMouseEnter := @OnMouseEnterTimeLabel;
        l.OnMouseLeave := @OnMouseLeaveTimeLabel;
        l.Parent := e;
        // entry time label
        l := TLabel.Create(e);
        l.Font.Color := clGray;
        l.Font.Size := -12;
        l.Alignment := taRightJustify;
        sec := SecondsBetween(StringToDateTime(
          arrFilteredEntries[j].GetPath('start_time').AsString),
          StringToDateTime(arrFilteredEntries[j].GetPath('end_time').AsString));
        l.Caption := SecondsToString(sec);
        l.Parent := e;
        // sum of all time entries
        sum := sum + sec;
      end;
      // sum of all time entries
      lt.Caption := SecondsToString(sum);
      // free filtered entries
      arrFilteredEntries.Clear;
      arrFilteredEntries.Free;
    end;

    // day label
    l := TLabel.Create(p);
    l.Cursor := crHandPoint;
    l.OnClick := @DayClickParent;
    if (IsSameDate(t, now)) then
    begin
      l.Caption := 'TODAY';
      l.Font.Color := RGBToColor(255, 152, 0);
    end
    else if (IsSameDate(t, yesterday)) then
    begin
      l.Caption := 'YESTERDAY';
      l.Font.Color := RGBToColor(99, 213, 120);
    end
    else
    begin
      l.Caption := UTF8UpperCase(LongDayNames[dayofweek(t)]);
      l.Font.Color := RGBToColor(99, 213, 120);
    end;
    l.Font.Height := -12;
    l.Font.Style := [fsBold];
    l.Align := alTop;
    l.Parent := p;
    l.BorderSpacing.Top := 10;
    l.BorderSpacing.Bottom := 10;
    // min height of container (label height + border spacing)
    d.Constraints.MinHeight := l.Height + 20;
  end;
  sl.Free;
end;

constructor TTaskList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.BorderStyle := bsNone;
  Self.HorzScrollBar.Visible := False;
  Self.VertScrollBar.Smooth := True;
  Self.VertScrollBar.Tracking := True;
end;

procedure TTaskList.DayClick(Sender: TObject);
var
  p: TAnimatedPanel;
begin
  p := TAnimatedPanel(TControl(Sender));
  p.Animate();
end;

end.
