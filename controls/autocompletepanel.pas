unit AutoCompletePanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLType;

type

  TAutocompleteEvent = procedure(Sender: TObject; SearchText: string; Items: TStrings) of object;

  { TAutoCompletePanel }

  TAutoCompletePanel = class(TCustomPanel)
  private
    FOnSearch: TAutocompleteEvent;
    FOnSelectionChange: TNotifyEvent;
    FSelectedObject: TObject;
    FSelectedObjectText: string;
    FLabel: TLabel;
    FEdit: TEdit;
    FListBox: TListBox;
    procedure OnEditChange(Sender: TObject);
    procedure OnEditEnter(Sender: TObject);
    procedure OnEditExit(Sender: TObject);
    procedure OnEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure OnListBoxClick(Sender: TObject);
    procedure SetFSelectedObject(AValue: TObject);
    procedure SetFSelectedObjectText(AValue: string);
  protected

  public
    procedure ShowListBox;
    procedure HideListBox;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedObject: TObject read FSelectedObject write SetFSelectedObject;
    property SelectedObjectText: string read FSelectedObjectText
      write SetFSelectedObjectText;
  published
    property OnSearch: TAutocompleteEvent read FOnSearch write FOnSearch;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property TabOrder;
    property Anchors;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FPC Paymo Widget', [TAutoCompletePanel]);
end;

{ TAutoCompletePanel }

procedure TAutoCompletePanel.OnEditEnter(Sender: TObject);
begin
  FListBox.Items.Clear;
  FOnSearch(Self, FEdit.Caption, FListBox.Items);
  ShowListBox;
end;

procedure TAutoCompletePanel.OnEditChange(Sender: TObject);
begin
  if Assigned(FOnSearch) then
  begin
    FListBox.Items.Clear;
    FOnSearch(Self, FEdit.Caption, FListBox.Items);
  end;
end;

procedure TAutoCompletePanel.OnEditExit(Sender: TObject);
begin
  HideListBox;
end;

procedure TAutoCompletePanel.OnEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  iPos: integer;
begin
  if (key = VK_TAB) then
    exit;
  if (key = VK_ESCAPE) then
  begin
    HideListBox;
    exit;
  end;
  ShowListBox;
  if (key = VK_DOWN) then
  begin
    key := 0;
    if (FListBox.Items.Count > 0) then
    begin
      iPos := FListBox.ItemIndex;
      Inc(iPos);
      if (iPos > FListBox.Items.Count - 1) then
        iPos := 0;
      FListBox.ItemIndex := iPos;
    end;
    exit;
  end;
  if (key = VK_UP) then
  begin
    key := 0;
    if (FListBox.Items.Count > 0) then
    begin
      iPos := FListBox.ItemIndex;
      Dec(iPos);
      if (iPos < 0) then
        iPos := FListBox.Items.Count - 1;
      FListBox.ItemIndex := iPos;
    end;
    exit;
  end;
  if Key = VK_RETURN then
  begin
    OnListBoxClick(nil);
  end;
end;

procedure TAutoCompletePanel.OnListBoxClick(Sender: TObject);
begin
  if FListBox.ItemIndex > -1 then
  begin
    SelectedObjectText := FListBox.Items[FListBox.ItemIndex];
    FSelectedObject := FListBox.Items.Objects[FListBox.ItemIndex];
    HideListBox;
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self);
  end;
end;

procedure TAutoCompletePanel.SetFSelectedObject(AValue: TObject);
begin
  if FSelectedObject = AValue then
    Exit;
  FSelectedObject := AValue;
end;

procedure TAutoCompletePanel.SetFSelectedObjectText(AValue: string);
begin
  if FSelectedObjectText = AValue then Exit;
  FSelectedObjectText := AValue;
  FLabel.Caption := FSelectedObjectText;
end;

procedure TAutoCompletePanel.ShowListBox;
begin
  Height := FLabel.Height + FEdit.Height + FListBox.Height;
  Self.BringToFront;
end;

procedure TAutoCompletePanel.HideListBox;
begin
  Height := FLabel.Height + FEdit.Height;
  FEdit.Caption := '';
end;

constructor TAutoCompletePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  // ListBox
  FListBox := TListBox.Create(Self);
  FListBox.Align := alTop;
  FListBox.OnClick := @OnListBoxClick;
  FListBox.TabStop := False;
  FListBox.Parent := Self;
  // Edit
  FEdit := TEdit.Create(Self);
  FEdit.OnEnter := @OnEditEnter;
  FEdit.OnKeyDown := @OnEditKeyDown;
  FEdit.OnChange := @OnEditChange;
  FEdit.OnClick := @OnEditEnter;
  FEdit.Align := alTop;
  FEdit.Parent := Self;
  // Label
  FLabel := TLabel.Create(Self);
  FLabel.Align := alTop;
  FLabel.Caption := ' ';
  FLabel.Parent := Self;
  OnExit := @OnEditExit;
end;

destructor TAutoCompletePanel.Destroy;
begin
  FEdit.Free;
  FListBox.Free;
  inherited Destroy;
end;

end.
