unit upaymo;

{< This is the core of the FPC Paymo Widget.
   This unit contains the main class to interact with
   the Paymo API.
   @author(Leandro Diaz (http://lainz.github.io))
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, jsonConf, fpjson, jsonparser,
  Dialogs, DateUtils, LazUTF8, udebug;

const
  { URL used for GET, POST and DELETE }
  PAYMOAPIBASEURL = 'https://app.paymoapp.com/api/';
  { URL used to get the API Key to start using the application }
  PAYMOAPIKEYURL = 'https://app.paymoapp.com/#Paymo.module.myaccount/';

{ Sort function by name property, case insensitive }
function NameSort(Item1, Item2: Pointer): integer;
{ Sort function by 'seq' property, it allows to sort like in the Paymo website }
function SeqSort(Item1, Item2: Pointer): integer;
{ Sort function by 'seq' property plus sort by project name, case insensitive }
function SeqSortProjectName(Item1, Item2: Pointer): integer;
{ Sort function by name property, case insensitive, from bottom to top }
function InverseNameSort(Item1, Item2: Pointer): integer;

type
  { Status for any GET, POST, DELETE operation }
  TPaymoResponseStatus = (
    prOK, //< operation went well
    prERROR, //< operation went bad
    prTRYAGAIN, //< server is bussy
    prNOInternet //< offline
    );

  { TPaymo -- Main class to interact with Paymo API }

  TPaymo = class(TObject)
  private
    FAPIKey: string;
    FAPIKeyURL: string;
    FAPIURL: string;
    FLoggedIn: boolean;
    FMe: TJSONObject;
    FOffline: boolean;
    FProjects: TJSONObject;
    FRunningTimer: TJSONObject;
    FSettingsFile: string;
    FSettingsFolder: string;
    FTaskLists: TJSONObject;
    FTasks: TJSONObject;
    FCompany: TJSONObject;
    FOfflineData: TJSONArray;
    function GetFHasOfflineData: boolean;
    procedure SetFAPIKey(AValue: string);
    procedure SetFAPIKeyURL(AValue: string);
    procedure SetFAPIURL(AValue: string);
    procedure SetFLoggedIn(AValue: boolean);
    procedure SetFOffline(AValue: boolean);
    procedure SetFSettingsFile(AValue: string);
    procedure SetFSettingsFolder(AValue: string);
  public
    { List of Projects }
    function ProjectsArray: TJSONArray;
    { List of Tasks }
    function TasksArray: TJSONArray;
    { List of Task Lists}
    function TaskListsArray: TJSONArray;
    { User information }
    function MyData: TJSONData;
    { Company information }
    function CompanyData: TJSONData;
    { Current running timer }
    function RunningTimerData: TJSONData;
    { Returns the name of the Project given the ID }
    function GetProjectName(ProjectID: integer): string;
    { Returns the name of the Task given the ID}
    function GetTaskName(TaskID: integer): string;
    { Returns the task data given the ID}
    function GetTask(TaskID: integer): TJSONData;
    { Returns the time entry data given the ID}
    function GetTimeEntry(EntryID: integer): TJSONData;
    { Returns the task list data given the ID}
    function GetTaskList(TaskListID: integer): TJSONData;
  public
    { Constructor }
    constructor Create;
    { Destructor }
    destructor Destroy; override;
    { The key to connect to the API }
    property APIKey: string read FAPIKey write SetFAPIKey;
    { The base URL used to connect to the API}
    property APIURL: string read FAPIURL write SetFAPIURL;
    { The URL used to obtain an API Key}
    property APIKeyURL: string read FAPIKeyURL write SetFAPIKeyURL;
    { Check if the user is Logged In }
    property LoggedIn: boolean read FLoggedIn write SetFLoggedIn;
    { Calls GetMe() and test the response }
    function Login: TPaymoResponseStatus;
    { Get data from an endpoint, stores the server response in the response variable }
    function Get(Endpoint: string; var Response: string): TPaymoResponseStatus;
    { Retrieve tasks online or offline }
    function GetTasks(): TPaymoResponseStatus;
    { Retrieve projects online or offline }
    function GetProjects(): TPaymoResponseStatus;
    { Retrieve task lists online or offline }
    function GetTaskLists(): TPaymoResponseStatus;
    { Retrieve user information }
    function GetMe(): TPaymoResponseStatus;
    { Retrieve current running timer }
    function GetRunningTimer(): TPaymoResponseStatus;
    { Retrieve company information }
    function GetCompany(): TPaymoResponseStatus;
    { Creates or updates data to an endpoint, providing a JSON string, stores the server response in the response variable }
    function Post(Endpoint: string; sJSON: TJSONStringType;
      var Response: string): TPaymoResponseStatus;
    { Delete the data of an endpoint, stores the server response in the response variable }
    function Delete(Endpoint: string; var Response: string): TPaymoResponseStatus;
    { Post a new task, if went OK it adds it to the list of tasks }
    function CreateTask(Name, Description: string; TaskListID: integer;
      var task: TJSONData): TPaymoResponseStatus;
    { Updates task 'complete' status }
    function UpdateTaskCompletion(Complete: boolean;
      task: TJSONData): TPaymoResponseStatus;
    { Persists the running timer, if time interval is less than a second the data is discarded }
    function StopRunningTimer(start_time, end_time: TDateTime;
      Description: string): TPaymoResponseStatus;
    { Set the running timer }
    function StartRunningTimer(task_id: integer;
      start_time: TDateTime): TPaymoResponseStatus;
    { Delete a time entry, given the ID }
    function DeleteTimeEntry(TimeEntryID: string): TPaymoResponseStatus;
    { Updates a time entry start and end time, and also project, task and tasklist related }
    function UpdateTimeEntry(TimeEntryID: integer; start_time, end_time: TDateTime;
      project_id, task_id, tasklist_id: integer): TPaymoResponseStatus;
  public
    { Persists a JSON to file, used to save offline data }
    function SaveJSON(FileName: string; sJSON: string): TPaymoResponseStatus;
    { Loads into memory a JSON file, used to retrieve previously saved offline data }
    function LoadJSON(FileName: string; var response: string): TPaymoResponseStatus;
  public
    { Retrieves the API Key and offline data previously saved }
    procedure LoadSettings;
    { Stores the API Key}
    procedure SaveSettings;
    { Stores into offline.json the data send to Post method, if the user is offline }
    procedure POST_Offline(Endpoint: string; sJSON: TJSONStringType;
      var Response: string);
    { Stores into offline.json the data send to Delete method, if the user is offline }
    procedure DELETE_Offline(Endpoint: string; var Response: string);
    { Creates a Post or Delete for each element stored in offline.json }
    function SYNC_OfflineData: integer;
    { Get / Set the Offline status }
    property Offline: boolean read FOffline write SetFOffline;
    { Check if there is offline data loaded into memory, used to see if SYNC_OfflineData must be called }
    property HasOfflineData: boolean read GetFHasOfflineData;
    { The folder where the API Key and json files must be saved }
    property SettingsFolder: string read FSettingsFolder write SetFSettingsFolder;
    { The API Key file name }
    property SettingsFile: string read FSettingsFile write SetFSettingsFile;
  end;

var
  { Used internally by the sort function SeqSortProjectName because it requires to retrieve the names of the projects }
  PAYMO_SORT_INSTANCE: TPaymo;

implementation

function NameSort(Item1, Item2: Pointer): integer;
begin
  if UTF8LowerCase(TJSONData(Item1).GetPath('name').AsString) >
    UTF8LowerCase(TJSONData(Item2).GetPath('name').AsString) then
    exit(1);
  if UTF8LowerCase(TJSONData(Item1).GetPath('name').AsString) <
    UTF8LowerCase(TJSONData(Item2).GetPath('name').AsString) then
    exit(-1);
  exit(0);
end;

function SeqSort(Item1, Item2: Pointer): integer;
begin
  if TJSONData(Item1).GetPath('project_id').AsInteger >
    TJSONData(Item2).GetPath('project_id').AsInteger then
    exit(-1);
  if TJSONData(Item1).GetPath('project_id').AsInteger <
    TJSONData(Item2).GetPath('project_id').AsInteger then
    exit(1);
  if TJSONData(Item1).GetPath('seq').AsInteger >
    TJSONData(Item2).GetPath('seq').AsInteger then
    exit(-1);
  if TJSONData(Item1).GetPath('seq').AsInteger <
    TJSONData(Item2).GetPath('seq').AsInteger then
    exit(1);
  exit(0);
end;

function SeqSortProjectName(Item1, Item2: Pointer): integer;
begin
  if UTF8LowerCase(PAYMO_SORT_INSTANCE.GetProjectName(
    TJSONData(Item1).GetPath('project_id').AsInteger)) >
    UTF8LowerCase(PAYMO_SORT_INSTANCE.GetProjectName(
    TJSONData(Item2).GetPath('project_id').AsInteger)) then
    exit(-1);
  if UTF8LowerCase(PAYMO_SORT_INSTANCE.GetProjectName(
    TJSONData(Item1).GetPath('project_id').AsInteger)) <
    UTF8LowerCase(PAYMO_SORT_INSTANCE.GetProjectName(
    TJSONData(Item2).GetPath('project_id').AsInteger)) then
    exit(1);
  if TJSONData(Item1).GetPath('seq').AsInteger >
    TJSONData(Item2).GetPath('seq').AsInteger then
    exit(-1);
  if TJSONData(Item1).GetPath('seq').AsInteger <
    TJSONData(Item2).GetPath('seq').AsInteger then
    exit(1);
  exit(0);
end;

function InverseNameSort(Item1, Item2: Pointer): integer;
begin
  if UTF8LowerCase(TJSONData(Item1).GetPath('name').AsString) <
    UTF8LowerCase(TJSONData(Item2).GetPath('name').AsString) then
    exit(1);
  if UTF8LowerCase(TJSONData(Item1).GetPath('name').AsString) >
    UTF8LowerCase(TJSONData(Item2).GetPath('name').AsString) then
    exit(-1);
  exit(0);
end;

{ TPaymo }

procedure TPaymo.SetFAPIKey(AValue: string);
begin
  if FAPIKey = AValue then
    Exit;
  FAPIKey := AValue;
end;

function TPaymo.GetFHasOfflineData: boolean;
begin
  Result := FOfflineData.Count > 0;
end;

procedure TPaymo.SetFAPIKeyURL(AValue: string);
begin
  if FAPIKeyURL = AValue then
    Exit;
  FAPIKeyURL := AValue;
end;

procedure TPaymo.SetFAPIURL(AValue: string);
begin
  if FAPIURL = AValue then
    Exit;
  FAPIURL := AValue;
end;

procedure TPaymo.SetFLoggedIn(AValue: boolean);
begin
  if FLoggedIn = AValue then
    Exit;
  FLoggedIn := AValue;
end;

procedure TPaymo.SetFOffline(AValue: boolean);
begin
  if FOffline = AValue then
    Exit;
  FOffline := AValue;
end;

procedure TPaymo.SetFSettingsFile(AValue: string);
begin
  if FSettingsFile = AValue then
    Exit;
  FSettingsFile := AValue;
end;

procedure TPaymo.SetFSettingsFolder(AValue: string);
begin
  if FSettingsFolder = AValue then
    Exit;
  FSettingsFolder := AValue;
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
  if not Assigned(FCompany) then
    exit(nil);
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

function TPaymo.GetTask(TaskID: integer): TJSONData;
var
  i: integer;
  arr: TJSONArray;
begin
  Result := nil;
  arr := TasksArray;
  for i := 0 to arr.Count - 1 do
  begin
    if TaskID = arr[i].GetPath('id').AsInteger then
      exit(arr[i]);
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

function TPaymo.GetTaskList(TaskListID: integer): TJSONData;
var
  i: integer;
  arr: TJSONArray;
begin
  Result := nil;
  arr := TaskListsArray;
  for i := 0 to arr.Count - 1 do
  begin
    if TaskListID = arr[i].GetPath('id').AsInteger then
      exit(arr[i]);
  end;
end;

constructor TPaymo.Create;
begin
  inherited Create;
  APIKeyURL := PAYMOAPIKEYURL;
  APIURL := PAYMOAPIBASEURL;
  FOffline := False;
  FOfflineData := TJSONArray.Create;
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
  if Assigned(FOfflineData) then
    FOfflineData.Free;
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
      Response := client.Get(APIURL + Endpoint);
      if (client.ResponseStatusCode >= 200) and (client.ResponseStatusCode <= 300) then
        Result := prOK
      else if (client.ResponseStatusCode = 429) then
      begin
        DebugLog('', 'Get/' + Endpoint, 'prTRYAGAIN');
        Result := prTRYAGAIN;
      end
      else
      begin
        DebugLog('', 'Get/' + Endpoint, 'prERROR');
        Result := prERROR;
      end;
    except
      on e: Exception do
      begin
        DebugLog('', 'Get/' + Endpoint, 'Exception: ' + e.message);
        if (Pos('HOST NAME RESOLUTION', UpperCase(e.message)) > 0) then
          Result := prNOInternet
        else
          Result := prERROR;
      end;
    end;
  finally
    client.Free;
  end;
end;

function TPaymo.GetTasks(): TPaymoResponseStatus;
var
  response: string;
begin
  if not FOffline then
    Result := Get('tasks?where=mytasks=true&include=entries', response)
  else
    Result := LoadJSON('tasks.json', response);
  case Result of
    prOK:
    begin
      if Assigned(FTasks) then
        FTasks.Free;
      FTasks := TJSONObject(GetJSON(response));
      TasksArray.Sort(@NameSort);
      SaveJSON('tasks.json', FTasks.FormatJSON());
    end;
  end;
end;

function TPaymo.GetProjects(): TPaymoResponseStatus;
var
  response: string;
begin
  if not FOffline then
    Result := Get('projects', response)
  else
    Result := LoadJSON('projects.json', response);
  case Result of
    prOK:
    begin
      if Assigned(FProjects) then
        FProjects.Free;
      FProjects := TJSONObject(GetJSON(response));
      ProjectsArray.Sort(@NameSort);
      SaveJSON('projects.json', FProjects.FormatJSON());
    end;
  end;
end;

function TPaymo.GetTaskLists(): TPaymoResponseStatus;
var
  response: string;
begin
  if not FOffline then
    Result := Get('tasklists', response)
  else
    Result := LoadJSON('tasklists.json', response);
  case Result of
    prOK:
    begin
      if Assigned(FTaskLists) then
        FTaskLists.Free;
      FTaskLists := TJSONObject(GetJSON(response));
      TaskListsArray.Sort(@NameSort);
      SaveJSON('tasklists.json', FTaskLists.FormatJSON());
    end;
  end;
end;

function TPaymo.GetMe(): TPaymoResponseStatus;
var
  response: string;
begin
  if not FOffline then
    Result := Get('me', response)
  else
    Result := LoadJSON('me.json', response);
  case Result of
    prOK:
    begin
      if Assigned(FMe) then
        FMe.Free;
      FMe := TJSONObject(GetJSON(response));
      SaveJSON('me.json', FMe.FormatJSON());
    end;
  end;
end;

function TPaymo.GetRunningTimer(): TPaymoResponseStatus;
var
  response: string;
begin
  if not FOffline then
    Result := Get('entries?where=user_id=' + MyData.GetPath('id').AsString +
      '%20and%20end_time=null', response)
  else
    Result := LoadJSON('runningtimer.json', response);
  case Result of
    prOK:
    begin
      if Assigned(FRunningTimer) then
        FRunningTimer.Free;
      FRunningTimer := TJSONObject(GetJSON(response));
      SaveJSON('runningtimer.json', FRunningTimer.FormatJSON());
    end;
  end;
end;

function TPaymo.GetCompany(): TPaymoResponseStatus;
var
  response: string;
begin
  if not FOffline then
    Result := Get('company', response)
  else
    Result := LoadJSON('company.json', response);
  case Result of
    prOK:
    begin
      if Assigned(FCompany) then
        FCompany.Free;
      FCompany := TJSONObject(GetJSON(response));
      SaveJSON('company.json', FCompany.FormatJSON());
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
    if not FOffline then
    begin
      try
        Response := client.Post(APIURL + Endpoint);
        if (client.ResponseStatusCode >= 200) and (client.ResponseStatusCode <= 300) then
          Result := prOK
        else if (client.ResponseStatusCode = 429) then
        begin
          DebugLog('', 'Post/' + Endpoint, 'prTRYAGAIN');
          Result := prTRYAGAIN;
        end
        else
        begin
          DebugLog('', 'Post/' + Endpoint, 'prERROR');
          Result := prERROR;
        end;
      except
        on e: Exception do
        begin
          DebugLog('', 'Post/' + Endpoint, 'Exception: ' + e.message);
          Result := prERROR;
        end;
      end;
    end
    else
    begin
      POST_Offline(Endpoint, sJSON, response);
      Result := prNOInternet;
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
    if not FOffline then
    begin
      try
        Response := client.Delete(APIURL + Endpoint);
        if (client.ResponseStatusCode >= 200) and (client.ResponseStatusCode <= 300) then
          Result := prOK
        else if (client.ResponseStatusCode = 429) then
        begin
          DebugLog('', 'Delete/' + Endpoint, 'prTRYAGAIN');
          Result := prTRYAGAIN;
        end
        else
        begin
          DebugLog('', 'Delete/' + Endpoint, 'prERROR');
          Result := prERROR;
        end;
      except
        on e: Exception do
        begin
          DebugLog('', 'Delete/' + Endpoint, 'Exception: ' + e.message);
          Result := prERROR;
        end;
      end;
    end
    else
    begin
      DELETE_Offline(Endpoint, response);
      Result := prNOInternet;
    end;
  finally
    client.Free;
  end;
end;

function TPaymo.CreateTask(Name, Description: string; TaskListID: integer;
  var task: TJSONData): TPaymoResponseStatus;
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
  if FOffline then
  begin
    // values used internally, needed even if offline
    jObj.Add('id', 9999999); // ToDo: generate unique number
    jObj.Add('complete', False);
    jObj.Add('seq', 0);
    jObj.Add('project_id', GetTaskList(TaskListID).GetPath('project_id').AsInteger);
    // detect that this is an offline created task
    jObj.Add('offline', True);
  end;
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('tasks', sJSON, response);
  case Result of
    prOK:
    begin
      task := GetJSON(response).GetPath('tasks').Items[0];
      TasksArray.Add(task);
    end;
    prNOInternet:
    begin
      task := GetJSON(response);
      TasksArray.Add(task);
    end;
  end;
end;

function TPaymo.UpdateTaskCompletion(Complete: boolean;
  task: TJSONData): TPaymoResponseStatus;
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
  {case Result of
    prOK: begin
      task := GetJSON(response).GetPath('tasks').Items[0];
    end;
  end;}
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

function TPaymo.StartRunningTimer(task_id: integer;
  start_time: TDateTime): TPaymoResponseStatus;
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

function TPaymo.UpdateTimeEntry(TimeEntryID: integer; start_time, end_time: TDateTime;
  project_id, task_id, tasklist_id: integer): TPaymoResponseStatus;
var
  response: string;
  sJSON: TJSONStringType;
  jObj: TJSONObject;
  r: TPaymoResponseStatus;
begin
  jObj := TJSONObject.Create;
  jObj.Add('start_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
    LocalTimeToUniversal(start_time)));
  jObj.Add('end_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"',
    LocalTimeToUniversal(end_time)));
  jObj.Add('project_id', project_id);
  jObj.Add('task_id', task_id);
  sJSON := jObj.FormatJSON();
  jObj.Free;
  Result := Post('entries/' + TimeEntryID.ToString, sJSON, response);

  jObj := TJSONObject.Create;
  jObj.Add('tasklist_id', tasklist_id);
  sJSON := jObj.FormatJSON();
  jObj.Free;
  r := Post('tasks/' + task_id.ToString, sJSON, response);
end;

function TPaymo.SaveJSON(FileName: string; sJSON: string): TPaymoResponseStatus;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.DefaultEncoding := TEncoding.UTF8;
  try
    try
      s.Add(sJSON);
      s.SaveToFile(SettingsFolder + FileName);
      Result := prOK;
    except
      Result := prERROR;
    end;
  finally
    s.Free;
  end;
end;

function TPaymo.LoadJSON(FileName: string; var response: string): TPaymoResponseStatus;
var
  s: TStringList;
begin
  s := TStringList.Create;
  s.DefaultEncoding := TEncoding.UTF8;
  try
    try
      s.LoadFromFile(SettingsFolder + FileName);
      response := s.Text;
      Result := prOK;
    except
      response := '[]';
      Result := prERROR;
    end;
  finally
    s.Free;
  end;
end;

procedure TPaymo.LoadSettings;
var
  c: TJSONConfig;
  response: string;
begin
  c := TJSONConfig.Create(nil);
  try
    if ForceDirectories(SettingsFolder) then
    begin
      c.Filename := SettingsFile;
      APIKey := c.GetValue('apikey', '');
    end;
  finally
    c.Free;
  end;
  if FileExists(SettingsFolder + 'offline.json') then
  begin
    case LoadJSON('offline.json', response) of
      prOK:
      begin
        if Assigned(FOfflineData) then
          FOfflineData.Free;
        FOfflineData := TJSONArray(GetJSON(response));
      end;
    end;
  end;
end;

procedure TPaymo.SaveSettings;
var
  c: TJSONConfig;
begin
  c := TJSONConfig.Create(nil);
  try
    if ForceDirectories(SettingsFolder) then
    begin
      c.Filename := SettingsFile;
      c.SetValue('apikey', APIKey);
    end;
  finally
    c.Free;
  end;
end;

procedure TPaymo.POST_Offline(Endpoint: string; sJSON: TJSONStringType;
  var Response: string);
var
  i: integer;
  obj: TJSONObject;
  s, t: TJSONStringType;
  found: boolean;
begin
  obj := TJSONObject.Create;
  obj.add('Type', 'POST');
  obj.add('Endpoint', Endpoint);
  obj.add('Data', GetJSON(sJSON));
  s := obj.FormatJSON();
  Response := sJSON;

  found := False;
  for i := 0 to FOfflineData.Count - 1 do
  begin
    t := FOfflineData.Items[i].FormatJSON();
    if s = t then
    begin
      found := True;
      break;
    end;
  end;

  if not found then
  begin
    FOfflineData.Add(obj);
    SaveJSON('offline.json', FOfflineData.FormatJSON());
  end;
end;

procedure TPaymo.DELETE_Offline(Endpoint: string; var Response: string);
var
  i: integer;
  obj: TJSONObject;
  s, t: TJSONStringType;
  found: boolean;
begin
  obj := TJSONObject.Create;
  obj.add('Type', 'DELETE');
  obj.add('Endpoint', Endpoint);
  s := obj.FormatJSON();
  Response := '{}';

  found := False;
  for i := 0 to FOfflineData.Count - 1 do
  begin
    t := FOfflineData.Items[i].FormatJSON();
    if s = t then
    begin
      found := True;
      break;
    end;
  end;

  if not found then
  begin
    FOfflineData.Add(obj);
    SaveJSON('offline.json', FOfflineData.FormatJSON());
  end;
end;

function TPaymo.SYNC_OfflineData: integer;
var
  i: integer;
  items_err: integer = 0;
  obj: TJSONObject;
  response: string;
begin
  for i := 0 to FOfflineData.Count - 1 do
  begin
    obj := TJSONObject(FOfflineData.Items[i]);
    if obj.GetPath('Type').AsString = 'POST' then
    begin
      case POST(obj.GetPath('Endpoint').AsString, obj.GetPath('Data').AsJSON,
          response) of
        prERROR, prTRYAGAIN:
        begin
          obj.Add('SyncError', 'True');
          DebugLog('Error', 'SYNC_OfflineData', obj.FormatJSON());
          Inc(items_err);
        end;
      end;
    end;
    if obj.GetPath('Type').AsString = 'DELETE' then
    begin
      case Delete(obj.GetPath('Endpoint').AsString, response) of
        prERROR, prTRYAGAIN:
        begin
          obj.Add('SyncError', 'True');
          DebugLog('Error', 'SYNC_OfflineData', obj.FormatJSON());
          Inc(items_err);
        end;
      end;
    end;
  end;
  FOfflineData.Clear;
  SaveJSON('offline.json', FOfflineData.FormatJSON());
  Result := items_err;
end;

end.
