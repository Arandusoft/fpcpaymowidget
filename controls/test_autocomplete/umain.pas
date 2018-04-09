unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AutoCompletePanel, LazUTF8;

type

  { TForm1 }

  { TPais }

  TPais = class(TObject)
  private
    FCodigo: string;
    FNombre: string;
    procedure SetFCodigo(AValue: string);
    procedure SetFNombre(AValue: string);
    public
      constructor Create(Nombre, Codigo: string);
      property Nombre: string read FNombre write SetFNombre;
      property Codigo: string read FCodigo write SetFCodigo;
  end;

  TForm1 = class(TForm)
    AutoCompletePanel1: TAutoCompletePanel;
    Button1: TButton;
    procedure AutoCompletePanel1Search(Sender: TObject; SearchText: string; Items: TStrings);
    procedure Button1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Data: TStringList;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TPais }

procedure TPais.SetFCodigo(AValue: string);
begin
  if FCodigo=AValue then Exit;
  FCodigo:=AValue;
end;

procedure TPais.SetFNombre(AValue: string);
begin
  if FNombre=AValue then Exit;
  FNombre:=AValue;
end;

constructor TPais.Create(Nombre, Codigo: string);
begin
  Self.Nombre := Nombre;
  Self.Codigo := Codigo;
end;

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
begin
  AutoCompletePanel1.HideListBox;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Data := TStringList.Create;
  Data.OwnsObjects := True;
  Data.AddObject('Argentina', TPais.Create('Argentina', 'AR'));
  Data.AddObject('Brasil', TPais.Create('Brasil', 'BR'));
  Data.AddObject('Paraguay', TPais.Create('Paraguay', 'PY'));
  Data.Add('Ninguno');

  AutoCompletePanel1.SelectedObject := Data.Objects[0];
  AutoCompletePanel1.SelectedObjectText := Data[0];
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Workaround to set size until size of label is fixed on Create event of AutoCompletePanel
  AutoCompletePanel1.HideListBox;
end;

procedure TForm1.AutoCompletePanel1Search(Sender: TObject; SearchText: string; Items: TStrings);
var
  i: integer;
  s: string;
begin
  s := UTF8LowerCase(SearchText);
  for i:=0 to Data.Count-1 do
  begin
    if (UTF8Pos(s, UTF8LowerCase(Data[i])) <> 0) or (s = '') then
      Items.AddObject(Data[i], Data.Objects[i]);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if AutoCompletePanel1.SelectedObject <> nil then
    ShowMessage(TPais(AutoCompletePanel1.SelectedObject).Codigo)
  else
    ShowMessage('Ninguno');
end;

end.

