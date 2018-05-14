unit AnimatedPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type

  TAnimatedPanelStyle = (apsTopBottom, apsLeftRight);

  { TAnimatedPanel }

  TAnimatedPanel = class(TPanel)
  private
    FShow: boolean;
    FStyle: TAnimatedPanelStyle;
    FUseAutoSize: boolean;
    FWidth, FHeight: integer;
    FStep, FAcum: double;
    FTimer: TTimer;
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure SetFHeight(AValue: integer);
    procedure SetFStyle(AValue: TAnimatedPanelStyle);
    procedure SetFTimer(AValue: TTimer);
    procedure SetFUseAutoSize(AValue: boolean);
    procedure SetFWidth(AValue: integer);
  protected
    function easeInOutQuad(t: double): double;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Animate();
    property Timer: TTimer read FTimer write SetFTimer;
  published
    property Style: TAnimatedPanelStyle read FStyle write SetFStyle;
    property UseAutoSize: boolean read FUseAutoSize write SetFUseAutoSize;
    property AnimWidth: integer read FWidth write SetFWidth;
    property AnimHeight: integer read FHeight write SetFHeight;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FPC Paymo Widget', [TAnimatedPanel]);
end;

{ TAnimatedPanel }

procedure TAnimatedPanel.SetFTimer(AValue: TTimer);
begin
  if FTimer = AValue then
    Exit;
  FTimer := AValue;
end;

procedure TAnimatedPanel.SetFUseAutoSize(AValue: boolean);
begin
  if FUseAutoSize = AValue then
    Exit;
  FUseAutoSize := AValue;
end;

procedure TAnimatedPanel.SetFWidth(AValue: integer);
begin
  if FWidth = AValue then
    Exit;
  FWidth := AValue;
end;

function TAnimatedPanel.easeInOutQuad(t: double): double;
begin
  {$ifndef darwin}
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
  {$else}
    Result := 1;
  {$endif}
end;

procedure TAnimatedPanel.Animate();
begin
  Timer.Enabled := True;
end;

procedure TAnimatedPanel.OnTimer(Sender: TObject);
var
  temp: double;
begin
  FAcum := FAcum + FStep;
  if FStep > 1 then
    FStep := 1;
  temp := easeInOutQuad(FAcum);

  if Style = apsTopBottom then
  begin
    if FShow then
    begin
      Height := round(FHeight * temp);
      if Height >= FHeight then
      begin
        Height := FHeight;
        Timer.Enabled := False;
      end;
    end
    else
    begin
      temp := FHeight - round(FHeight * temp);
      if temp <= Constraints.MinHeight then
      begin
        Height := Constraints.MinHeight;
        Timer.Enabled := False;
      end
      else
        Height := trunc(temp);
    end;
  end;

  if Style = apsLeftRight then
  begin
    if FShow then
    begin
      Width := round(FWidth * temp);
      if Width >= FWidth then
      begin
        Width := FWidth;
        Timer.Enabled := False;
      end;
    end
    else
    begin
      temp := FWidth - round(FWidth * temp);
      if temp <= Constraints.MinWidth then
      begin
        Width := Constraints.MinWidth;
        Timer.Enabled := False;
      end
      else
        Width := trunc(temp);
    end;
  end;
end;

procedure TAnimatedPanel.SetFHeight(AValue: integer);
begin
  if FHeight = AValue then
    Exit;
  FHeight := AValue;
end;

procedure TAnimatedPanel.SetFStyle(AValue: TAnimatedPanelStyle);
begin
  if FStyle = AValue then
    Exit;
  FStyle := AValue;
end;

procedure TAnimatedPanel.OnStartTimer(Sender: TObject);
begin
  if FUseAutoSize then
  begin
    CalculatePreferredSize(FWidth, FHeight, False);
    AutoSize := False;
  end;
  if Style = apsTopBottom then
    FShow := FHeight <> Height;
  if Style = apsLeftRight then
    FShow := FWidth <> Width;
  FAcum := 0;
end;

procedure TAnimatedPanel.OnStopTimer(Sender: TObject);
begin
  if FShow and FUseAutoSize then
    AutoSize := True;
end;

constructor TAnimatedPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  FTimer := TTimer.Create(Self);
  FStep := 0.1;
  FTimer.Interval := 15;
  FTimer.Enabled := False;
  FTimer.OnTimer := @OnTimer;
  FTimer.OnStartTimer := @OnStartTimer;
  FTimer.OnStopTimer := @OnStopTimer;
end;

destructor TAnimatedPanel.Destroy;
begin
  if Assigned(FTimer) then
  begin
    FShow := False;
    FTimer.Enabled := False;
    FTimer.Free;
  end;
  inherited Destroy;
end;

end.
