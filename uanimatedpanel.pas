unit uanimatedpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type

  { TAnimatedPanel }

  TAnimatedPanel = class(TPanel)
  private
    FShow: boolean;
    FWidth, FHeight: integer;
    FTimer: TTimer;
    procedure OnStartTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure SetFTimer(AValue: TTimer);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Animate();
    property Timer: TTimer read FTimer write SetFTimer;
  end;

implementation

{ TAnimatedPanel }

procedure TAnimatedPanel.SetFTimer(AValue: TTimer);
begin
  if FTimer = AValue then
    Exit;
  FTimer := AValue;
end;

procedure TAnimatedPanel.Animate();
begin
  Timer.Enabled := True;
end;

procedure TAnimatedPanel.OnTimer(Sender: TObject);
var
  temp: integer;
begin
  if FShow then
  begin
    temp := Height + 5;
    if temp <= FHeight then
      Height := temp
    else
    begin
      Height := FHeight;
      Timer.Enabled := False;
    end;
  end
  else
  begin
    temp := Height - 5;
    if temp > 0 then
      Height := temp
    else
    begin
      Height := 0;
      Timer.Enabled := False;
    end;
  end;
end;

procedure TAnimatedPanel.OnStartTimer(Sender: TObject);
begin
  CalculatePreferredSize(FWidth, FHeight, False);
  FShow := FHeight <> Height;
end;

procedure TAnimatedPanel.OnStopTimer(Sender: TObject);
begin

end;

constructor TAnimatedPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 15;
  FTimer.Enabled := False;
  FTimer.OnTimer := @OnTimer;
  FTimer.OnStartTimer := @OnStartTimer;
  FTimer.OnStopTimer := @OnStopTimer;
end;

end.
