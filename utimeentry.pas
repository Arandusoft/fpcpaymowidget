unit utimeentry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  DateUtils;

type

  { TfrmTimeEntry }

  TfrmTimeEntry = class(TForm)
    dlgDate: TCalendarDialog;
    time_start_hh: TEdit;
    time_end_hh: TEdit;
    time_start_mm: TEdit;
    time_end_mm: TEdit;
    time_start_separator: TLabel;
    time_end_separator: TLabel;
    time_start_separator1: TLabel;
    lbl_date: TLabel;
    procedure lbl_dateClick(Sender: TObject);
  private

  public

  end;

var
  frmTimeEntry: TfrmTimeEntry;

implementation

{$R *.lfm}

{ TfrmTimeEntry }

procedure TfrmTimeEntry.lbl_dateClick(Sender: TObject);
begin
  if dlgDate.Execute then
  begin
    lbl_date.Caption := FormatDateTime('ddddd', dlgDate.Date);
  end;
end;

end.

