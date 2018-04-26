unit ulimitedstringlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLimitedStringList = class(TObject)
  private
    FIndexOfInsert: integer;
    FList: TStringList;
    function Get(Index: integer): string;
    function GetCount: integer;
  public
    constructor Create(AMaxCount: integer);
    destructor Destroy; override;
    procedure Append(const S: string);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property Count: integer read GetCount;
    property Strings[Index: integer]: string read Get; default;
  end;

implementation

constructor TLimitedStringList.Create(AMaxCount: integer);
begin
  inherited Create;
  if AMaxCount <= 0 then
    AMaxCount := 1;
  FList := TStringList.Create;
  FList.Capacity := AMaxCount;
  //  FIndexOfInsert := 0;
end;

procedure TLimitedStringList.Append(const S: string);
begin
  if FList.Count < FList.Capacity then
    FList.Append(S)
  else
  begin
    FList[FIndexOfInsert] := S;
    Inc(FIndexOfInsert);
    if FIndexOfInsert >= FList.Count then
      FIndexOfInsert := 0;
  end;
end;

destructor TLimitedStringList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TLimitedStringList.Get(Index: integer): string;
begin
  Inc(Index, FIndexOfInsert);
  if Index >= FList.Count then
    Dec(Index, FList.Count);
  Result := FList[Index];
end;

function TLimitedStringList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TLimitedStringList.LoadFromFile(const FileName: string);
var
  TempList: TStringList;
  i: integer;
  Start: integer;
begin
  TempList := TStringList.Create;
  try
    TempList.LoadFromFile(FileName, TEncoding.UTF8);
    FList.Clear;
    FIndexOfInsert := 0;
    Start := TempList.Count - FList.Capacity;
    if Start < 0 then
      Start := 0;
    for i := Start to TempList.Count - 1 do
      Append(TempList[i]);
  finally
    TempList.Free;
  end;
end;

procedure TLimitedStringList.SaveToFile(const FileName: string);
var
  TempList: TStringList;
  i: integer;
begin
  TempList := TStringList.Create;
  try
    TempList.Capacity := FList.Capacity;
    for i := 0 to FList.Count - 1 do
      TempList.Append(Get(i));
    try
      TempList.SaveToFile(FileName);
    except
      on E: EFCreateError do
      begin
        // no hacer nada, no se pudo guardar el log
      end;
    end;
  finally
    TempList.Free;
  end;
end;

end.

