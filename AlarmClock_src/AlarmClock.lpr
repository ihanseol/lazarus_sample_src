{*****************}
{*  Alarm Clock  *}
{*****************}

program AlarmClock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, datetimectrls,
  alarmclock_u1, alarmclock_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TAlarmClockForm, AlarmClockForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

