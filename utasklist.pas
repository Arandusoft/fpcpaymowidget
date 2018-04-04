unit utasklist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, fpjson, StdCtrls, Graphics, Forms,
  upaymo, Dialogs, DateUtils, AnimatedPanel, LazUTF8, Math, uresourcestring;

const
  {$IFDEF WINDOWS}
  FONTNAME = 'Nunito Sans';
  FONTNAMELIGHT = 'Nunito Sans ExtraLight';
  FONTNAMELIGHT2 = 'Nunito Sans Light';
  FONTNAMEBOLD = 'Nunito Sans ExtraBold';
  FONTNAMEFIXED = 'Courier New';
  FONTSIZETIME = -40; // timer
  FONTSIZEBACK = -30; // <- back (menu)
  FONTSIZESTOP = -24; // [] stop button
  FONTSIZEBIG2 = -23; // total time of day
  FONTSIZEBIG = -16; // big
  FONTSIZEMEDIUM = -14; // medium
  FONTSIZESMALL = -12; // small
  {$ENDIF}
  {$IFDEF LINUX}
  FONTNAME = 'Roboto';
  FONTNAMELIGHT = 'Roboto';
  FONTNAMELIGHT2 = 'Roboto';
  FONTNAMEBOLD = 'Roboto';
  FONTNAMEFIXED = 'Courier New';
  FONTSIZETIME = -40; // timer
  FONTSIZEBACK = -30; // <- back (menu)
  FONTSIZESTOP = -24; // [] stop button
  FONTSIZEBIG2 = -18; // total time of day
  FONTSIZEBIG = -14; // big
  FONTSIZEMEDIUM = -1; // medium
  FONTSIZESMALL = -10; // small
  {$ENDIF}
  {$IFDEF DARWIN}
  FONTNAME = 'Helvetica Neue';
  FONTNAMELIGHT = 'Helvetica Neue';
  FONTNAMELIGHT2 = 'Helvetica Neue';
  FONTNAMEBOLD = 'Helvetica Neue';
  FONTNAMEFIXED = 'Monaco';
  FONTSIZETIME = -40; // timer
  FONTSIZEBACK = -30; // <- back (menu)
  FONTSIZESTOP = -24; // [] stop button
  FONTSIZEBIG2 = -18; // total time of day
  FONTSIZEBIG = -14; // big
  FONTSIZEMEDIUM = -1; // medium
  FONTSIZESMALL = -10; // small
  {$ENDIF}

type

  { TTaskList }

  TTaskList = class(TScrollBox)
  private
    FItems: TJSONArray;
    FPaymo: TPaymo;
    procedure OnClickPlay(Sender: TObject);
    procedure OnClickTimeEntry(Sender: TObject);
    procedure OnEnterPlay(Sender: TObject);
    procedure OnLeavePlay(Sender: TObject);
    procedure OnMouseEnterTimeLabel(Sender: TObject);
    procedure OnMouseLeaveTimeLabel(Sender: TObject);
    procedure SetFItems(AValue: TJSONArray);
    procedure SetFPaymo(AValue: TPaymo);
  protected
    procedure OnClickItem(Sender: TObject);
    procedure OnClickItemParent(Sender: TObject);
    procedure DayClick(Sender: TObject);
    procedure DayClickParent(Sender: TObject);
  public
    class function SecondsToString(Seconds: integer): string;
    class function SecondsToHHMMSS(Seconds: integer): string;
    class function StringToDateTime(DateTime: string): TDateTime;
    class function IsSameDate(Date1, Date2: TDateTime): boolean;
    constructor Create(AOwner: TComponent); override;
  public
    property PaymoInstance: TPaymo read FPaymo write SetFPaymo;
    procedure RefreshItems;
  end;

implementation

uses
  utimeentry, umain;

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

procedure TTaskList.OnClickPlay(Sender: TObject);
begin
  // task id
  if TControl(Sender).Tag <> 0 then
  begin
    // ToDo: stop current entry, then start new one (don't show error)
    //if frmMain.StopTimeEntry() then
    case PaymoInstance.StartRunningTimer(TControl(Sender).Tag) of
      prOK:
      begin
        case PaymoInstance.GetRunningTimer() of
          prOK:
          begin
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
    OnClickItem(Sender);
end;

procedure TTaskList.OnClickTimeEntry(Sender: TObject);
var
  Data: TJSONData;
begin
  // show edit time entry
  Data := PaymoInstance.GetTimeEntry(TControl(Sender).Tag);
  if Data <> nil then
  begin
    frmTimeEntry.PaymoInstance := PaymoInstance;
    frmTimeEntry.Data := Data;
    //frmTimeEntry.Caption := 'Entry ' + TControl(Sender).Tag.ToString;
    frmTimeEntry.ShowData;
    frmTimeEntry.Show;
  end;
end;

procedure TTaskList.OnEnterPlay(Sender: TObject);
begin
  TControl(Sender).Font.Color := RGBToColor(57, 202, 84);
end;

procedure TTaskList.OnLeavePlay(Sender: TObject);
begin
  TControl(Sender).Font.Color := RGBToColor(221, 221, 221);
end;

procedure TTaskList.DayClickParent(Sender: TObject);
var
  p: TAnimatedPanel;
  c: TControl;
begin
  p := TAnimatedPanel(TControl(Sender).Parent.Parent.Parent);
  p.Animate();

  c := TControl(TControl(Sender).Parent.FindComponent('arrow'));
  if c.Caption = '˅' then
    c.Caption := '˄'
  else
    c.Caption := '˅';
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

class function TTaskList.SecondsToString(Seconds: integer): string;
var
  hh, mins, ss: DWord;
begin
  DivMod(Seconds, SecsPerMin, mins, ss);
  DivMod(mins, MinsPerHour, hh, mins);
  Result := Format('%.2d:%.2d', [hh, mins]);
end;

class function TTaskList.SecondsToHHMMSS(Seconds: integer): string;
var
  hh, mins, ss: DWord;
begin
  DivMod(Seconds, SecsPerMin, mins, ss);
  DivMod(mins, MinsPerHour, hh, mins);
  if hh > 0 then
    Result := Format('%.2d:%.2d:%.2d', [hh, mins, ss])
  else
    Result := Format('%.2d:%.2d', [mins, ss]);
end;

class function TTaskList.StringToDateTime(DateTime: string): TDateTime;
begin
  Result := ScanDateTime('yyyy-mm-dd"T"hh:nn:ss', Copy(DateTime, 1, 19));
  Result := UniversalTimeToLocal(Result);
end;

class function TTaskList.IsSameDate(Date1, Date2: TDateTime): boolean;
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
  i, j, k, sum, sec, sumDay: integer;
  d, e: TAnimatedPanel;
  p, pc, play: TPanel;
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
        PaymoInstance.MyData.GetPath('id').AsInteger then
        Continue;
      // ToDo: only start_time format for now
      if arrEntries[j].FindPath('start_time') <> nil then
      begin
        // add only new items
        tempstr := FormatDateTime('yyyy mm dd',
          StringToDateTime(arrEntries[j].GetPath('start_time').AsString));
        if (sl.IndexOf(tempstr) = -1) then
          sl.Add(tempstr);
      end;
    end;
  end;

  sl.Sort;

  for k := 0 to sl.Count - 1 do
  begin
    sumDay := 0;

    // time to compare
    t := ScanDateTime('yyyy mm dd', sl[k]);

    // day panel
    d := TAnimatedPanel.Create(Self);
    d.Style := apsTopBottom;
    d.UseAutoSize := True;
    d.Font.Name := FONTNAME;
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
          PaymoInstance.MyData.GetPath('id').AsInteger then
          Continue;
        // ToDo: only start_time format for now
        if arrEntries[j].FindPath('start_time') = nil then
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
      // task container
      p := TPanel.Create(d);
      p.Font.Name := FONTNAME;
      p.BevelOuter := bvNone;
      p.BorderSpacing.Left := 30;
      p.BorderSpacing.Right := 30;
      p.BorderSpacing.Bottom := 20;
      p.Align := alTop;
      p.AutoSize := True;
      p.Parent := d;
      // title and arrow container
      pc := TPanel.Create(p);
      pc.Font.Name := FONTNAME;
      pc.Cursor := crHandPoint;
      pc.BevelOuter := bvNone;
      pc.Align := alTop;
      pc.AutoSize := True;
      pc.Name := 'arrow_container';
      pc.Caption := '';
      pc.OnClick := @OnClickItem;
      pc.Parent := p;
      // project title
      l := TLabel.Create(pc);
      l.Font.Name := FONTNAME;
      l.BorderSpacing.Left := 30;
      l.Cursor := crHandPoint;
      l.Font.Color := clGray;
      l.Font.Size := FONTSIZESMALL;
      l.Align := alLeft;
      l.Caption := FPaymo.GetProjectName(arr[i].GetPath('project_id').AsInteger);
      l.OnClick := @OnClickItemParent;
      l.Parent := pc;
      // task right arrow
      l := TLabel.Create(pc);
      l.Font.Name := FONTNAMEFIXED;
      l.Cursor := crHandPoint;
      l.Font.Color := clGray;
      l.Font.Size := FONTSIZESMALL;
      l.Align := alRight;
      l.Alignment := taRightJustify;
      l.Name := 'arrow';
      l.Caption := '˅';
      l.OnClick := @OnClickItemParent;
      l.Parent := pc;
      // play button
      play := TPanel.Create(p);
      play.Font.Name := FONTNAME;
      play.BevelOuter := bvNone;
      play.Font.Color := RGBToColor(221, 221, 221);
      play.Align := alLeft;
      play.Width := 30;
      play.Font.Size := FONTSIZEBIG;
      play.Caption := '▶';
      play.Cursor := crHandPoint;
      play.OnClick := @OnClickPlay;
      if not arr[i].GetPath('complete').AsBoolean then
      begin
        play.OnMouseEnter := @OnEnterPlay;
        play.OnMouseLeave := @OnLeavePlay;
        // task id
        play.Tag := arr[i].GetPath('id').AsInteger;
      end
      else
      begin
        play.ShowHint := True;
        play.Hint := 'Task is complete';
        play.Tag := 0;
      end;
      play.Parent := p;
      // task name
      l := TLabel.Create(p);
      l.Font.Name := FONTNAME;
      l.Cursor := crHandPoint;
      l.Font.Color := clBlack;
      l.Font.Size := FONTSIZEMEDIUM;
      l.Align := alClient;
      l.WordWrap := True;
      if arr[i].GetPath('complete').AsBoolean then
        l.Font.Style := [fsStrikeOut];
      l.Caption := arr[i].GetPath('name').AsString;
      l.OnClick := @OnClickItem;
      l.Parent := p;
      // sum of time entries container
      pc := TPanel.Create(p);
      pc.Font.Name := FONTNAME;
      pc.Cursor := crHandPoint;
      pc.BevelOuter := bvNone;
      pc.Align := alRight;
      pc.AutoSize := True;
      pc.OnClick := @OnClickItem;
      pc.Parent := p;
      // sum of time entries
      lt := TLabel.Create(pc);
      lt.Font.Name := FONTNAME;
      lt.Cursor := crHandPoint;
      lt.Font.Color := clGray;
      lt.Font.Size := FONTSIZESMALL;
      lt.Alignment := taRightJustify;
      lt.Align := alBottom;
      lt.OnClick := @OnClickItemParent;
      lt.Parent := pc;
      // time entries container
      e := TAnimatedPanel.Create(p);
      e.Style := apsTopBottom;
      e.UseAutoSize := True;
      e.Font.Name := FONTNAME;
      e.BorderSpacing.Left := 30;
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
        l.Font.Name := FONTNAME;
        l.Cursor := crHandPoint;
        l.Font.Color := clGray;
        l.Font.Size := FONTSIZESMALL;
        l.Alignment := taLeftJustify;
        l.Caption := FormatDateTime('t', StringToDateTime(
          arrFilteredEntries[j].GetPath('start_time').AsString)) +
          ' ‒ ' + FormatDateTime('t', StringToDateTime(
          arrFilteredEntries[j].GetPath('end_time').AsString));
        l.OnMouseEnter := @OnMouseEnterTimeLabel;
        l.OnMouseLeave := @OnMouseLeaveTimeLabel;
        l.Tag := arrFilteredEntries[j].GetPath('id').AsInteger;
        l.OnClick := @OnClickTimeEntry;
        l.Parent := e;
        // entry time label
        l := TLabel.Create(e);
        l.Font.Name := FONTNAME;
        l.Font.Color := clGray;
        l.Font.Size := FONTSIZESMALL;
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
      sumDay := sumDay + sum;
      // free filtered entries
      arrFilteredEntries.Clear;
      arrFilteredEntries.Free;
    end;

    // day and date container
    pc := TPanel.Create(p);
    pc.Font.Name := FONTNAME;
    pc.BevelOuter := bvNone;
    pc.Align := alTop;
    pc.AutoSize := True;
    pc.ChildSizing.ControlsPerLine := 4;
    pc.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    pc.ChildSizing.HorizontalSpacing := 5;
    pc.Parent := p;
    // day label
    l := TLabel.Create(pc);
    l.Font.Name := FONTNAMEBOLD;
    l.Cursor := crHandPoint;
    l.OnClick := @DayClickParent;
    l.Font.Size := FONTSIZESMALL;
    l.BorderSpacing.Top := 10;
    l.BorderSpacing.Bottom := 10;
    l.Parent := pc;
    if (IsSameDate(t, now)) then
    begin
      l.Caption := rsToday;
      l.Font.Color := RGBToColor(255, 152, 0);
    end
    else if (IsSameDate(t, yesterday)) then
    begin
      l.Caption := rsYesterday;
      l.Font.Color := RGBToColor(99, 213, 120);
    end
    else
    begin
      l.Caption := UTF8UpperCase(LongDayNames[dayofweek(t)]);
      l.Font.Color := RGBToColor(99, 213, 120);
      // show date
      lt := TLabel.Create(pc);
      lt.Font.Name := FONTNAME;
      lt.Cursor := crHandPoint;
      lt.OnClick := @DayClickParent;
      lt.Font.Size := FONTSIZESMALL;
      lt.Font.Color := clGray;
      lt.Caption := UTF8UpperCase(FormatDateTime('ddddd', t));
      lt.Parent := pc;
    end;
    // min height of container (label height + border spacing)
    d.Constraints.MinHeight := l.Height + 20;
    // arrow
    l := TLabel.Create(pc);
    l.Font.Name := FONTNAMEFIXED;
    l.Cursor := crHandPoint;
    l.Font.Color := clGray;
    l.Font.Size := FONTSIZESMALL;
    l.Name := 'arrow';
    l.Caption := '˄';
    l.OnClick := @DayClickParent;
    l.Parent := pc;
    // total time container
    p := TPanel.Create(pc);
    p.Font.Name := FONTNAME;
    p.BevelOuter := bvNone;
    p.Align := alRight;
    p.AutoSize := True;
    p.Parent := pc;
    // total time of day
    lt := TLabel.Create(p);
    lt.Font.Name := FONTNAMELIGHT;
    lt.Font.Size := FONTSIZEBIG2;
    lt.Align := alTop;
    lt.Font.Color := clBlack;
    lt.Caption := SecondsToString(sumDay);
    lt.Parent := p;
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
