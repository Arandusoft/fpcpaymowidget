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
    FLoggedIn: boolean;
    FMe: TJSONObject;
    FProjects: TJSONObject;
    FTaskLists: TJSONObject;
    FTasks: TJSONObject;
    procedure SetFAPIKey(AValue: string);
    procedure SetFLoggedIn(AValue: boolean);
    procedure SetFMe(AValue: TJSONObject);
    procedure SetFProjects(AValue: TJSONObject);
    procedure SetFTaskLists(AValue: TJSONObject);
    procedure SetFTasks(AValue: TJSONObject);
  protected
    property Projects: TJSONObject read FProjects write SetFProjects;
    property Tasks: TJSONObject read FTasks write SetFTasks;
    property TaskLists: TJSONObject read FTaskLists write SetFTaskLists;
    property Me: TJSONObject read FMe write SetFMe;
  public
    function ProjectsArray: TJSONArray;
    function TasksArray: TJSONArray;
    function TaskListsArray: TJSONArray;
    function MyData: TJSONArray;
    function GetProjectName(ProjectID: integer): string;
  public
    destructor Destroy; override;
    property APIKey: string read FAPIKey write SetFAPIKey;
    property LoggedIn: boolean read FLoggedIn write SetFLoggedIn;
    function Login: TPaymoResponseStatus;
    function Get(Endpoint: string; var Response: string): TPaymoResponseStatus;
    function GetTasks(): TPaymoResponseStatus;
    function GetProjects(): TPaymoResponseStatus;
    function GetTaskLists(): TPaymoResponseStatus;
    function GetMe(): TPaymoResponseStatus;
    function Post(Endpoint: string; sJSON: TJSONStringType;
      var Response: string): TPaymoResponseStatus;
    function CreateTask(Name, Description: string; TaskListID: integer): TPaymoResponseStatus;
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

procedure TPaymo.SetFLoggedIn(AValue: boolean);
begin
  if FLoggedIn = AValue then
    Exit;
  FLoggedIn := AValue;
end;

procedure TPaymo.SetFMe(AValue: TJSONObject);
begin
  if FMe = AValue then
    Exit;
  FMe := AValue;
end;

procedure TPaymo.SetFProjects(AValue: TJSONObject);
begin
  if FProjects = AValue then
    Exit;
  FProjects := AValue;
end;

procedure TPaymo.SetFTaskLists(AValue: TJSONObject);
begin
  if FTaskLists=AValue then Exit;
  FTaskLists:=AValue;
end;

procedure TPaymo.SetFTasks(AValue: TJSONObject);
begin
  if FTasks = AValue then
    Exit;
  FTasks := AValue;
end;

function TPaymo.ProjectsArray: TJSONArray;
begin
  FProjects.Find('projects', Result);
end;

function TPaymo.TasksArray: TJSONArray;
begin
  FTasks.Find('tasks', Result);
end;

function TPaymo.TaskListsArray: TJSONArray;
begin
  FTaskLists.Find('tasklists', Result);
end;

function TPaymo.MyData: TJSONArray;
begin
  FMe.Find('users', Result);
end;

function TPaymo.GetProjectName(ProjectID: integer): string;
var
  i: integer;
  arr: TJSONArray;
begin
  Result := '';
  arr := ProjectsArray;
  for i := 0 to arr.Count - 1 do
  begin
    if ProjectID = arr[i].GetPath('id').AsInteger then
      exit(arr[i].GetPath('name').AsString);
  end;
end;

destructor TPaymo.Destroy;
begin
  if Assigned(FProjects) then
    FProjects.Free;
  if Assigned(FTasks) then
    FTasks.Free;
  if Assigned(FMe) then
    FMe.Free;
  if Assigned(FTaskLists) then
    FTaskLists.Free;
  inherited Destroy;
end;

function TPaymo.Login: TPaymoResponseStatus;
begin
  Result := GetProjects();
  case Result of
    prOK: FLoggedIn := True;
    prTRYAGAIN: FLoggedIn := False;
    prERROR: FLoggedIn := False;
  end;
end;

function TPaymo.Get(Endpoint: string; var Response: string): TPaymoResponseStatus;
var
  client: TFPHTTPClient;
begin
  Result := prERROR;
  try
    client := TFPHttpClient.Create(nil);
    client.AddHeader('Accept', 'application/json');
    client.UserName := FAPIKey;
    client.Password := '';
    try
      Response := client.Get(PAYMOAPIBASEURL + Endpoint);
      if (client.ResponseStatusCode >= 200) and (client.ResponseStatusCode <= 300) then
        Result := prOK
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

function TPaymo.GetTasks(): TPaymoResponseStatus;
var
  response: string;
begin
  //Result := Get('tasks?where=users=' + MyData[0].GetPath('id').AsString + '&include=entries', response);
  Result := Get('tasks?where=mytasks=true&include=entries', response);
  case Result of
    prOK:
    begin
      if Assigned(FTasks) then
        FTasks.Free;
      FTasks := TJSONObject(GetJSON(response));
    end;
  end;
end;

function TPaymo.GetProjects(): TPaymoResponseStatus;
var
  response: string;
begin
  Result := Get('projects', response);
  case Result of
    prOK:
    begin
      if Assigned(FProjects) then
        FProjects.Free;
      FProjects := TJSONObject(GetJSON(response));
    end;
  end;
end;

function TPaymo.GetTaskLists(): TPaymoResponseStatus;
var
  response: string;
begin
  Result := Get('tasklists', response);
  case Result of
    prOK:
    begin
      if Assigned(FTaskLists) then
        FTaskLists.Free;
      FTaskLists := TJSONObject(GetJSON(response));
    end;
  end;
end;

function TPaymo.GetMe(): TPaymoResponseStatus;
var
  response: string;
begin
  Result := Get('me', response);
  case Result of
    prOK:
    begin
      if Assigned(FMe) then
        FMe.Free;
      FMe := TJSONObject(GetJSON(response));
    end;
  end;
end;

function TPaymo.Post(Endpoint: string; sJSON: TJSONStringType;
  var Response: string): TPaymoResponseStatus;
var
  client: TFPHTTPClient;
  ss: TMemoryStream;
begin
  Result := prERROR;
  try
    client := TFPHttpClient.Create(nil);
    client.AddHeader('Content-type', 'application/json');
    client.AddHeader('Accept', 'application/json');
    client.UserName := FAPIKey;
    client.Password := '';
    ss := TMemoryStream.Create();
    ss.Write(Pointer(sJSON)^, length(sJSON));
    ss.Position := 0;
    client.RequestBody := ss;
    try
      Response := client.Post(PAYMOAPIBASEURL + Endpoint);
      if (client.ResponseStatusCode >= 200) and (client.ResponseStatusCode <= 300) then
        Result := prOK
      else if (client.ResponseStatusCode = 429) then
        Result := prTRYAGAIN
      else
        Result := prERROR;
    except
      Result := prERROR;
    end;
  finally
    ss.Free;
    client.Free;
  end;
end;

function TPaymo.CreateTask(Name, Description: string; TaskListID: integer): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
  jArr: TJSONArray;
begin
  // logged in user
  jArr := TJSONArray.Create([MyData[0].GetPath('id').AsInteger]);
  jObj := TJSONObject.Create;
  jObj.Add('name', Name);
  jObj.Add('description', Description);
  jObj.Add('tasklist_id', TaskListID);
  jObj.Add('users', jArr);
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('tasks', sJSON, response);
  case Result of
    prOK:
    begin
      ShowMessage(response);
    end;
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
