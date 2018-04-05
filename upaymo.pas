unit upaymo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, jsonConf, fpjson, jsonparser,
  Dialogs, DateUtils;

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
    FRunningTimer: TJSONObject;
    FTaskLists: TJSONObject;
    FTasks: TJSONObject;
    FCompany: TJSONObject;
    procedure SetFAPIKey(AValue: string);
    procedure SetFLoggedIn(AValue: boolean);
  public
    function ProjectsArray: TJSONArray;
    function TasksArray: TJSONArray;
    function TaskListsArray: TJSONArray;
    function MyData: TJSONData;
    function CompanyData: TJSONData;
    function RunningTimerData: TJSONData;
    function GetProjectName(ProjectID: integer): string;
    function GetTaskName(TaskID: integer): string;
    function GetTimeEntry(EntryID: integer): TJSONData;
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
    function GetRunningTimer(): TPaymoResponseStatus;
    function GetCompany(): TPaymoResponseStatus;
    function Post(Endpoint: string; sJSON: TJSONStringType;
      var Response: string): TPaymoResponseStatus;
    function Delete(Endpoint: string; var Response: string): TPaymoResponseStatus;
    function CreateTask(Name, Description: string;
      TaskListID: integer; var task: TJSONData): TPaymoResponseStatus;
    function UpdateTaskCompletion(Complete: boolean; task: TJSONData): TPaymoResponseStatus;
    function StopRunningTimer(start_time, end_time: TDateTime;
      Description: string): TPaymoResponseStatus;
    function StartRunningTimer(task_id: integer; start_time: TDateTime): TPaymoResponseStatus;
    function DeleteTimeEntry(TimeEntryID: string): TPaymoResponseStatus;
    function UpdateTimeEntry(TimeEntryID: string; end_time: TDateTime;
      project_id, task_id, tasklist_id: string): TPaymoResponseStatus;
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

function TPaymo.MyData: TJSONData;
var
  arr: TJSONArray;
begin
  FMe.Find('users', arr);
  Result := arr[0];
end;

function TPaymo.CompanyData: TJSONData;
begin
  FCompany.Find('company', Result);
end;

function TPaymo.RunningTimerData: TJSONData;
var
  arr: TJSONArray;
begin
  if not Assigned(FRunningTimer) then
    exit(nil);
  FRunningTimer.Find('entries', arr);
  if (arr <> nil) and (arr.Count > 0) then
    Result := arr[0]
  else
    Result := nil;
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

function TPaymo.GetTaskName(TaskID: integer): string;
var
  i: integer;
  arr: TJSONArray;
begin
  Result := '';
  arr := TasksArray;
  for i := 0 to arr.Count - 1 do
  begin
    if TaskID = arr[i].GetPath('id').AsInteger then
      exit(arr[i].GetPath('name').AsString);
  end;
end;

function TPaymo.GetTimeEntry(EntryID: integer): TJSONData;
var
  arr, arrEntries: TJSONArray;
  i, j: integer;
begin
  Result := nil;
  arr := TasksArray;
  for i := 0 to arr.Count - 1 do
  begin
    arrEntries := TJSONArray(arr[i].GetPath('entries'));
    for j := 0 to arrEntries.Count - 1 do
    begin
      if arrEntries[j].GetPath('id').AsInteger = EntryID then
        exit(arrEntries[j]);
    end;
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
  if Assigned(FRunningTimer) then
    FRunningTimer.Free;
  if Assigned(FCompany) then
    FCompany.Free;
  inherited Destroy;
end;

function TPaymo.Login: TPaymoResponseStatus;
begin
  Result := GetMe();
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

function TPaymo.GetRunningTimer(): TPaymoResponseStatus;
var
  response: string;
begin
  Result := Get('entries?where=user_id=' + MyData.GetPath('id').AsString +
    '%20and%20end_time=null', response);
  case Result of
    prOK:
    begin
      if Assigned(FRunningTimer) then
        FRunningTimer.Free;
      FRunningTimer := TJSONObject(GetJSON(response));
    end;
  end;
end;

function TPaymo.GetCompany(): TPaymoResponseStatus;
var
  response: string;
begin
  Result := Get('company', response);
  case Result of
    prOK:
    begin
      if Assigned(FCompany) then
        FCompany.Free;
      FCompany := TJSONObject(GetJSON(response));
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

function TPaymo.Delete(Endpoint: string; var Response: string): TPaymoResponseStatus;
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
      Response := client.Delete(PAYMOAPIBASEURL + Endpoint);
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

function TPaymo.CreateTask(Name, Description: string;
  TaskListID: integer; var task: TJSONData): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
  jArr: TJSONArray;
begin
  // logged in user
  jArr := TJSONArray.Create([MyData.GetPath('id').AsInteger]);
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
      task := GetJSON(response).GetPath('tasks').Items[0];
      TasksArray.Add(task);
    end;
  end;
end;

function TPaymo.UpdateTaskCompletion(Complete: boolean; task: TJSONData): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
begin
  jObj := TJSONObject.Create;
  jObj.Add('complete', Complete);
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('tasks/' + task.GetPath('id').AsString, sJSON, response);
  case Result of
    prOK: begin
      task := GetJSON(response).GetPath('tasks').Items[0];
    end;
  end;
end;

function TPaymo.StopRunningTimer(start_time, end_time: TDateTime;
  Description: string): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
begin
  // https://github.com/paymoapp/api/blob/master/sections/entries.md#stopping-a-timer
  // more than a minute = POST
  if SecondsBetween(start_time, end_time) >= 60 then
  begin
    jObj := TJSONObject.Create;
    jObj.Add('end_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
      LocalTimeToUniversal(end_time)));
    jObj.Add('description', Description);
    sJSON := jObj.FormatJSON();
    jObj.Free;
    Result := Post('entries/' + RunningTimerData.GetPath('id').AsString,
      sJSON, response);
  end
  // less than a minute = DELETE
  else
  begin
    Result := Delete('entries/' + RunningTimerData.GetPath('id').AsString, response);
  end;
end;

function TPaymo.StartRunningTimer(task_id: integer; start_time: TDateTime): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
begin
  jObj := TJSONObject.Create;
  jObj.Add('start_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
    LocalTimeToUniversal(start_time)));
  jObj.Add('user_id', MyData.GetPath('id').AsInteger);
  jObj.Add('task_id', task_id);
  //jObj.Add('description', '');
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('entries', sJSON, response);
end;

function TPaymo.DeleteTimeEntry(TimeEntryID: string): TPaymoResponseStatus;
var
  response: string;
begin
  Result := Delete('entries/' + TimeEntryID, response);
end;

function TPaymo.UpdateTimeEntry(TimeEntryID: string; end_time: TDateTime;
  project_id, task_id, tasklist_id: string): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
begin
  jObj := TJSONObject.Create;
  jObj.Add('end_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
    LocalTimeToUniversal(end_time)));
  jObj.Add('project_id', project_id);
  jObj.Add('task_id', task_id);
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('entries/' + TimeEntryID, sJSON, response);

  jObj := TJSONObject.Create;
  jObj.Add('tasklist_id', tasklist_id);
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('tasks/' + task_id, sJSON, response);
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
