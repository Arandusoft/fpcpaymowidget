unit uresourcestring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rsTooManyRequestsTryAgainSoon = 'Too many Requests. Try again soon.';
  rsErrorCantLoginTryGeneratingANewAPIKey =
    'Error: Can''t login. Try generating a new API Key.';
  rsErrorCantLogin = 'Can''t login.';
  rsToday = 'TODAY';
  rsYesterday = 'YESTERDAY';
  rsErrorCantStopTimer = 'Error: Can''t stop timer.';
  rsPleaseFillAllFields = 'Please fill all fields.';
  rsThereMustBeAtLeastAMinuteOfDifferenceBetweenStartAndEndTime =
    'There must be at least a minute of difference between start and end time.';
  rsErrorCantDeleteTimeEntry = 'Error: Can''t delete time entry.';
  rsErrorCantUpdateTimeEntry = 'Error: Can''t update time entry.';
  rsErrorCantStartTimer = 'Error: Can''t start timer.';
  rsErrorCantStartTimerTryStoppingCurrentTimerFirst =
    'Error: Can''t start timer. Try stopping current timer first.';
  rsStartTimer = 'Start Timer';
  rsSaveEntry = 'Save Entry';
  rsErrorCantCreateTask = 'Error: Can''t create task.';
  rsPleaseEnterTaskDescription = 'Please enter task description.';
  rsTaskIsComplete = 'Task is complete';
  rsErrorCantUpdateTask = 'Error: Can''t update task.';

implementation

end.
