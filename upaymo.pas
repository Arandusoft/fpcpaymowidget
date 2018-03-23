unit upaymo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, jsonConf, fpjson, jsonparser,
  Dialogs;

const
  PAYMOAPIBASEURL = 'https://app.paymoapp.com/api/';
  PAYMOAPIKEYURL = 'https://app.paymoapp.com/#Paymo.module.myaccount/';

type
  TPaymoResponseStatus = (prOK, prERROR, prTRYAGAIN);

  { TPaymo }

  TPaymo = class(TObject)
  private
    FAPIKey: string;
    FProjects: TJSONObject;
    procedure SetFAPIKey(AValue: string);
    procedure SetFProjects(AValue: TJSONObject);
  protected
    property Projects: TJSONObject read FProjects write SetFProjects;
  public
    function ProjectsArray: TJSONArray;
  public
    property APIKey: string read FAPIKey write SetFAPIKey;
    function Login: TPaymoResponseStatus;
    destructor Destroy; override;
  public
    procedure LoadSettings;
    procedure SaveSettings;
  end;

implementation

{ TPaymo }

procedure TPaymo.SetFAPIKey(AValue: string);
begin
  if FAPIKey = AValue then
    Exit;
  FAPIKey := AValue;
end;

procedure TPaymo.SetFProjects(AValue: TJSONObject);
begin
  if FProjects = AValue then
    Exit;
  FProjects := AValue;
end;

function TPaymo.ProjectsArray: TJSONArray;
begin
  FProjects.Find('projects', result);
end;

destructor TPaymo.Destroy;
begin
  if Assigned(FProjects) then
    FProjects.Free;
  inherited Destroy;
end;

function TPaymo.Login: TPaymoResponseStatus;
var
  client: TFPHTTPClient;
  response: string;
begin
  Result := prERROR;
  try
    client := TFPHttpClient.Create(nil);
    client.AddHeader('Accept', 'application/json');
    client.UserName := APIKey;
    client.Password := '';
    try
      response := client.Get(PAYMOAPIBASEURL + 'projects');
      if (client.ResponseStatusCode >= 200) and (client.ResponseStatusCode <= 300) then
      begin
        FProjects := TJSONObject(GetJSON(response));
        Result := prOK;
      end
      else if (client.ResponseStatusCode = 429) then
        Result := prTRYAGAIN
      else
        Result := prERROR;
    except
      Result := prERROR;
    end;
  finally
    client.Free;
  end;
end;

procedure TPaymo.LoadSettings;
var
  c: TJSONConfig;
begin
  c := TJSONConfig.Create(nil);
  try
    if ForceDirectories(GetAppConfigDir(False)) then
    begin
      c.Filename := GetAppConfigFile(False);
      APIKey := c.GetValue('apikey', '');
    end;
  finally
    c.Free;
  end;
end;

procedure TPaymo.SaveSettings;
var
  c: TJSONConfig;
begin
  c := TJSONConfig.Create(nil);
  try
    if ForceDirectories(GetAppConfigDir(False)) then
    begin
      c.Filename := GetAppConfigFile(False);
      c.SetValue('apikey', APIKey);
    end;
  finally
    c.Free;
  end;
end;

end.
