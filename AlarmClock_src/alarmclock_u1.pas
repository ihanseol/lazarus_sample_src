{****************************}
{*  Alarm Clock (main unit) *}
{****************************}

unit alarmclock_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus, PopupNotifier, ExtCtrls, LCLIntf, IniFiles, alarmclock_u2;

type
  { TAlarmClockForm }
  TAlarmClockForm = class(TForm)
    AlarmMenu: TMainMenu;
    MenuAlarm: TMenuItem;
    MenuAlarmReset: TMenuItem;
    MenuAlarmHide: TMenuItem;
    MenuAlarmExit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    SetDates: TRadioButton;
    SetWeekdays: TRadioButton;
    AlarmWeekdays1: TCheckBox;
    AlarmWeekdays2: TCheckBox;
    AlarmWeekdays3: TCheckBox;
    AlarmWeekdays4: TCheckBox;
    AlarmWeekdays5: TCheckBox;
    AlarmWeekdays6: TCheckBox;
    AlarmWeekdays7: TCheckBox;
    TimeWeekdays1: TDateTimePicker;
    TimeWeekdays2: TDateTimePicker;
    TimeWeekdays3: TDateTimePicker;
    TimeWeekdays4: TDateTimePicker;
    TimeWeekdays5: TDateTimePicker;
    TimeWeekdays6: TDateTimePicker;
    TimeWeekdays7: TDateTimePicker;
    ButtonAudio1: TButton;
    ButtonAudio2: TButton;
    ButtonAudio3: TButton;
    ButtonAudio4: TButton;
    ButtonAudio5: TButton;
    ButtonAudio6: TButton;
    ButtonAudio7: TButton;
    Memo1: TMemo;
    ButtonCopyTime: TButton;
    Memo2: TMemo;
    ButtonCopyAudio: TButton;
    AlarmDatesA1: TCheckBox;
    AlarmDatesA2: TCheckBox;
    AlarmDatesA3: TCheckBox;
    TimeDatesA1: TDateTimePicker;
    TimeDatesA2: TDateTimePicker;
    TimeDatesA3: TDateTimePicker;
    ButtonAudioA1: TButton;
    ButtonAudioA2: TButton;
    ButtonAudioA3: TButton;
    ButtonSet: TButton;
    AudioSelect: TOpenDialog;
    AlarmTimer: TTimer;
    ACTrayIcon: TTrayIcon;
    AboutNotifier: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure MenuAlarmResetClick(Sender: TObject);
    procedure MenuAlarmHideClick(Sender: TObject);
    procedure MenuAlarmExitClick(Sender: TObject);
    procedure MenuHelpHelpClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure SetDatesChange(Sender: TObject);
    procedure SetWeekdaysChange(Sender: TObject);
    procedure AlarmWeekdays1Change(Sender: TObject);
    procedure AlarmWeekdays2Change(Sender: TObject);
    procedure AlarmWeekdays3Change(Sender: TObject);
    procedure AlarmWeekdays4Change(Sender: TObject);
    procedure AlarmWeekdays5Change(Sender: TObject);
    procedure AlarmWeekdays6Change(Sender: TObject);
    procedure AlarmWeekdays7Change(Sender: TObject);
    procedure AlarmDatesA1Change(Sender: TObject);
    procedure AlarmDatesA2Change(Sender: TObject);
    procedure AlarmDatesA3Change(Sender: TObject);
    procedure TimeWeekdays1Exit(Sender: TObject);
    procedure TimeWeekdays2Exit(Sender: TObject);
    procedure TimeWeekdays3Exit(Sender: TObject);
    procedure TimeWeekdays4Exit(Sender: TObject);
    procedure TimeWeekdays5Exit(Sender: TObject);
    procedure TimeWeekdays6Exit(Sender: TObject);
    procedure TimeWeekdays7Exit(Sender: TObject);
    procedure TimeDatesA1Exit(Sender: TObject);
    procedure TimeDatesA2Exit(Sender: TObject);
    procedure TimeDatesA3Exit(Sender: TObject);
    procedure ButtonCopyTimeClick(Sender: TObject);
    procedure ButtonAudio1Click(Sender: TObject);
    procedure ButtonAudio2Click(Sender: TObject);
    procedure ButtonAudio3Click(Sender: TObject);
    procedure ButtonAudio4Click(Sender: TObject);
    procedure ButtonAudio5Click(Sender: TObject);
    procedure ButtonAudio6Click(Sender: TObject);
    procedure ButtonAudio7Click(Sender: TObject);
    procedure ButtonAudioA1Click(Sender: TObject);
    procedure ButtonAudioA2Click(Sender: TObject);
    procedure ButtonAudioA3Click(Sender: TObject);
    procedure ButtonCopyAudioClick(Sender: TObject);
    procedure ButtonSetClick(Sender: TObject);
    procedure AlarmTimerTimer(Sender: TObject);
    procedure ACTrayIconClick(Sender: TObject);
  end;

const
  DayNames: array[1..7] of string[2] = ('MO', 'TU', 'WE', 'TH', 'FR', 'SA', 'SU');

var
  AlarmClockForm: TAlarmClockForm;
  AlarmWeekdays: array[1..7] of TCheckBox;
  TimeWeekdays: array[1..7] of TDateTimePicker;
  AlarmDates: array[1..3] of TCheckBox;
  TimeDates: array[1..3] of TDateTimePicker;
  ButtonsAudio: array[1..10] of TButton;
  AudioFiles: array[1..10] of string;
  LastTime: TDateTime;
  LastAudioFile, DefaultDir0, InitialDir0: string;
  IniFile: TINIFile;

implementation

{$R *.lfm}

{ Check if date/time has already passed }

function PassedDate(DT: TDateTime): Boolean;

var
  Y, M, D, H, N, S, MS: Word;
  NowDate, NowTime, GivenDate, GivenTime: Integer;
  Passed: Boolean;

begin

  Passed := False;
  DecodeDate(Now, Y, M, D);                                                              // actual date decoded into separate variables
  NowDate := 10000 * Y + 100 * M + D;                                                    // actual date transformed to integer number
  DecodeDate(DT, Y, M, D);                                                               // given date decoded into separate variables
  GivenDate := 10000 * Y + 100 * M + D;                                                  // given date transformed to integer number
  if GivenDate < NowDate then
    Passed := True
  else begin
    if GivenDate = NowDate then begin
      DecodeTime(Now, H, N, S, MS);                                                      // actual time decoded into separate variables
      NowTime := 10000 * H + 100 * N + S;                                                // actual time transformed to integer number
      DecodeTime(DT, H, N, S, MS);                                                       // given time decoded into separate variables
      GivenTime := 10000 * H + 100 * N + S;                                              // given time transformed to integer number
      if GivenTime <= NowTime then
        Passed := True;
    end;
  end;
  PassedDate := Passed;
end;

{ Initialize alarm clock settings (from .INI file) }

procedure InitSettings(Filename: string);

var
  DefaultDir, AudioDir, Message, S, S1, A: string;
  I: Integer;

begin
  IniFile := TINIFile.Create(Filename);
  try
    // Current alarm settings
    S := IniFile.ReadString('General', 'Alarm', '');
    if (S = 'WEEKDAYS') or (S = '') then
      AlarmClockForm.SetWeekdays.Checked := True
    else if S = 'DATES' then
      AlarmClockForm.SetDates.Checked := True;
    // Audio file directory
    DefaultDir0 := IniFile.ReadString('General', 'DefaultDir', ''); DefaultDir := DefaultDir0;
    if (DefaultDir = 'HOME') or (DefaultDir = 'MUSIC') then begin
      S := GetUserDir;
      if DefaultDir = 'MUSIC' then begin
        S += 'Music';
        if not DirectoryExists(S) then
          S := GetUserDir;
      end;
      DefaultDir := S;
    end;
    InitialDir0 := IniFile.ReadString('General', 'InitialDir', ''); AudioDir := InitialDir0;
    if AudioDir = '' then
      AudioDir := DefaultDir;
    if not DirectoryExists(AudioDir) then
      AudioDir := '';
    AlarmClockForm.AudioSelect.InitialDir := AudioDir;                                   // set initial directory in Open dialog box
    // Weekdays alarm settings
    for I := 1 to 7 do begin
      // Alarm set or not
      S := IniFile.ReadString('WeekdaysAlarms', DayNames[I], '');
      if S = 'True' then
        AlarmWeekdays[I].Checked := True                                                 // select alarm for this weekday
      else
        AlarmWeekdays[I].Checked := False;                                               // unselect alarm for this weekday
      if AlarmClockForm.SetWeekdays.Checked then                                         // enable alarm only if week days alarm is selected
        AlarmWeekdays[I].Enabled := True
      else
        AlarmWeekdays[I].Enabled := False;
      // Alarm time
      S := IniFile.ReadString('WeekdaysTimes', DayNames[I], '');
      if S = '' then
        S := '00:00';
      TimeWeekdays[I].Time := StrToTime(S);                                              // set alarm time for this week day
      // Alarm audio file
      AudioFiles[I] := IniFile.ReadString('WeekdaysAudio', DayNames[I], '');             // save audio file name for this week day
      if AudioFiles[I] = '' then
        ButtonsAudio[I].Caption := 'Choose audio file'                                   // no audio file selected yet
      else begin
        S := ExtractFileName(AudioFiles[I]);
        ButtonsAudio[I].Caption := Copy(S, 1, Length(S) - 4);                            // audio file name displayed on file selection button
      end;
    end;
    // Dates alarm settings
    for I := 1 to 3 do begin
      A := 'Alarm' + IntToStr(I);
      // Alarm set or not
      S := IniFile.ReadString('DatesAlarms', A, '');
      S1 := IniFile.ReadString('DatesDateTimes', A, '');
      if (S = 'False') or (S1 = '' ) or PassedDate(StrToDateTime(S1)) then
        AlarmDates[I].Checked := False                                                   // select alarm for this date
      else begin
        AlarmDates[I].Checked := True;                                                   // unselect alarm for this date
        IniFile.WriteString('DatesAlarms', A, 'False');                                  // be sure to update .INI file
      end;
      if AlarmClockForm.SetDates.Checked then                                            // enable alarm only if dates alarm is selected
        AlarmDates[I].Enabled := True
      else
        AlarmDates[I].Enabled := False;
      // Alarm date and time
      if (S1 = '') or PassedDate(StrToDateTime(S1)) then begin
        TimeDates[I].DateTime := Now;                                                    // set alarm date/time to actual values
        IniFile.WriteString('DatesDateTimes', A, '');                                    // be sure to update .INI file
      end
      else
        TimeDates[I].DateTime := StrToDateTime(S1);                                      // set alarm time for this date
      // Alarm audio file
      AudioFiles[I + 7] := IniFile.ReadString('DatesAudio', A, '');                      // save audio file name for this date
      if AudioFiles[I + 7] = '' then
        ButtonsAudio[I + 7].Caption := 'Choose audio file'                               // no audio file selected yet
      else begin
        S := ExtractFileName(AudioFiles[I + 7]);
        ButtonsAudio[I + 7].Caption := Copy(S, 1, Length(S) - 4);                        // audio file name displayed on file selection button
      end;
    end;
  except
    // Inexisting/invalid .INI file: exit application
    Message := 'Configuration file AlarmClock.INI not found or invalid!';
    MessageDlg('File error',Message,mtError,[mbOK],0);
    Application.Terminate;
  end;
  LastAudioFile := 'Choose audio file'; LastTime := StrToTime('00:00');
  // Stop alarm timer
  AlarmClockForm.AlarmTimer.Enabled := False;
  AlarmClockForm.ButtonSet.Caption := 'Start';
  AlarmClockForm.Caption := 'Alarm Clock (All alarms stopped)';
  // Be sure to use the correct .INI file for write operations
  IniFile.Free; IniFile := TINIFile.Create('AlarmClock.INI');
end;

{ Check if alarm mode is actually set to "week days" }

function WeekdaysAlarmSet: Boolean;

var
  I: Integer;
  A: Boolean;

begin
  A := False;
  if AlarmClockForm.SetWeekdays.Checked then begin
    for I := 1 to 7 do
      if AlarmWeekdays[I].Checked then
        A := True;
  end;
  WeekdaysAlarmSet := A;
end;

{ Check if alarm mode is actually set to "dates" }

function DatesAlarmSet: Boolean;

var
  I: Integer;
  A: Boolean;

begin
  A := False;
  if AlarmClockForm.SetDates.Checked then begin
    for I := 1 to 3 do
      if AlarmDates[I].Checked then
        A := True;
  end;
  DatesAlarmSet := A;
end;

{ Set weekdays alarm on/off (in .INI file) }

procedure SetWeekdaysAlarmOnOff(I: Integer);

begin
  if AlarmWeekDays[I].Checked then
    IniFile.WriteString('WeekdaysAlarms', DayNames[I], 'True')
  else
    IniFile.WriteString('WeekdaysAlarms', DayNames[I], 'False');
end;

{ Set dates alarm on/off (in .INI file) }

procedure SetDatesAlarmOnOff(I: Integer);

var
  A: string;

begin
  A := 'Alarm' + IntToStr(I);
  if AlarmDates[I].Checked then
    IniFile.WriteString('DatesAlarms', A, 'True')
  else
    IniFile.WriteString('DatesAlarms', A, 'False');
end;

{ Set alarm time for given weekday (in .INI file) }

procedure SetAlarmTime(I: Integer);

begin
  LastTime := TimeWeekdays[I].Time;                                                      // remember time for "time copy" function
  IniFile.WriteString('WeekdaysTimes', DayNames[I], TimeToStr(LastTime));
end;

{ Set alarm date/time for given date (in .INI file) }

procedure SetAlarmDate(I: Integer);

var
  A: string;

begin
  A := 'Alarm' + IntToStr(I);
  IniFile.WriteString('DatesDateTimes', A, DateTimeToStr(TimeDates[I].DateTime));
end;

{ Select audio file for given weekday/dates alarm }

procedure GetAudioFile(I: Integer);

var
  S: string;

begin
  if AlarmClockForm.AudioSelect.Execute then begin
    AudioFiles[I] := AlarmClockForm.AudioSelect.Filename;
    S := ExtractFileName(AudioFiles[I]);
    ButtonsAudio[I].Caption := Copy(S, 1, Length(S) - 4);
    if I <= 7 then begin                                                                 // index 1-7 = weekdays alarm
      LastAudioFile := AudioFiles[I];                                                    // remeber file for "copy audio" function
      IniFile.WriteString('WeekdaysAudio', DayNames[I], LastAudioFile);
    end
    else                                                                                 // index 8-10 = days alarm
      IniFile.WriteString('DatesAudio', 'Alarm' + IntToStr(I - 7), AudioFiles[I]);
    IniFile.WriteString('General', 'InitialDir', ExtractFilePath(AudioFiles[I]));        // remember directory for next "open audio file" action
  end;
end;

{ Play audio file using the operating systems default music player }

procedure StartAudioPlayer(AudioFile: string);

begin
  if not FileExists(AudioFile) then
    AudioFile := 'AlarmClock.mp3';
  OpenDocument(AudioFile);
end;

{ Write alarm clock settings to .INI file }

procedure WriteSettings;

var
  I: Integer;
  A: string;

begin
  IniFile.WriteString('General', 'DefaultDir', DefaultDir0);
  IniFile.WriteString('General', 'InitialDir', InitialDir0);
  if AlarmClockForm.SetDates.Checked then
    IniFile.WriteString('General', 'Alarm', 'DATES')
  else
    IniFile.WriteString('General', 'Alarm', 'WEEKDAYS');
  for I := 1 to 7 do begin
    if AlarmWeekDays[I].Checked then
      IniFile.WriteString('WeekdaysAlarms', DayNames[I], 'True')
    else
      IniFile.WriteString('WeekdaysAlarms', DayNames[I], 'False');
    IniFile.WriteString('WeekdaysTimes', DayNames[I], TimeToStr(TimeWeekdays[I].Time));
    IniFile.WriteString('WeekdaysAudio', DayNames[I], AudioFiles[I]);
  end;
  for I := 1 to 3 do begin
    A := 'Alarm' + IntToStr(I);
    if AlarmDates[I].Checked then
      IniFile.WriteString('DatesAlarms', A, 'True')
    else
      IniFile.WriteString('DatesAlarms', A, 'False');
    IniFile.WriteString('DatesDateTimes', A, '');
    IniFile.WriteString('DatesAudio', A, AudioFiles[7 + I]);
  end;
end;

{ Restore form window (from system tray )}

procedure FormRestore;

begin
  if AlarmClockForm.WindowState = wsMinimized then begin
    AlarmClockForm.WindowState:=wsNormal;                                                // window from 'minimized' to 'normal'
    AlarmClockForm.Show;                                                                 // window from 'hidden' to 'displayed'
    AlarmClockForm.ACTrayIcon.Visible := False;                                          // remove application from system tray
  end;
end;

{ TAlarmClockForm }

{ Application start: Program initialisation}

procedure TAlarmClockForm.FormCreate(Sender: TObject);

begin
  // Create arrays containing the different form objects
  AlarmWeekdays[1] := AlarmWeekdays1; AlarmWeekdays[2] := AlarmWeekdays2; AlarmWeekdays[3] := AlarmWeekdays3; AlarmWeekdays[4] := AlarmWeekdays4;
  AlarmWeekdays[5] := AlarmWeekdays5; AlarmWeekdays[6] := AlarmWeekdays6; AlarmWeekdays[7] := AlarmWeekdays7;
  AlarmDates[1]    := AlarmDatesA1;   AlarmDates[2]    := AlarmDatesA2;   AlarmDates[3]    := AlarmDatesA3;
  TimeWeekdays[1]  := TimeWeekdays1;  TimeWeekdays[2]  := TimeWeekdays2;  TimeWeekdays[3]  := TimeWeekdays3;  TimeWeekdays[4]  := TimeWeekdays4;
  TimeWeekdays[5]  := TimeWeekdays5;  TimeWeekdays[6]  := TimeWeekdays6;  TimeWeekdays[7]  := TimeWeekdays7;
  TimeDates[1]     := TimeDatesA1;    TimeDates[2]     := TimeDatesA2;    TimeDates[3]     := TimeDatesA3;
  ButtonsAudio[1]  := ButtonAudio1;   ButtonsAudio[2]  := ButtonAudio2;   ButtonsAudio[3]  := ButtonAudio3;   ButtonsAudio[4]  := ButtonAudio4;
  ButtonsAudio[5]  := ButtonAudio5;   ButtonsAudio[6]  := ButtonAudio6;   ButtonsAudio[7]  := ButtonAudio7;   ButtonsAudio[8]  := ButtonAudioA1;
  ButtonsAudio[9]  := ButtonAudioA2;  ButtonsAudio[10] := ButtonAudioA3;
  // Initialize alarm settings (from .INI file)
  InitSettings('AlarmClock.INI');
end;

{ Menu "Alarm > Reset": Reset alarm clock configuration }

procedure TAlarmClockForm.MenuAlarmResetClick(Sender: TObject);

begin
  IniFile.Free; InitSettings('AlarmClock_default.INI');                                            // read default alarm settings
  WriteSettings;                                                                                   // write default settings to .INI file
end;

{ Menu "Alarm > Hide": Hide application into system tray }

procedure TAlarmClockForm.MenuAlarmHideClick(Sender: TObject);

begin
  ACTrayIcon.Visible := True;                                                                      // show application in system tray
  WindowState:=wsMinimized;                                                                        // minimize the application window
  Hide;                                                                                            // hide the window
end;

{ Menu "Alarm > Exit": Exit application }

procedure TAlarmClockForm.MenuAlarmExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu "Help > Help": Show application Help content}

procedure TAlarmClockForm.MenuHelpHelpClick(Sender: TObject);

begin
  if HelpForm.Visible then
    HelpForm.Close
  else
    HelpForm.Show;
end;

{ Menu "Help > About": Show application About info }

procedure TAlarmClockForm.MenuHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Music alarm clock with individual settings for each week day ';
  S += 'or alternatively individual alarms for any 3 given dates.' + Chr(13) + Chr(13);
  S += '© allu, December 2017';
  AboutNotifier.Text := S;
  if not AboutNotifier.Visible then
    AboutNotifier.Visible := True                                                                  // show "About" popup
  else
    AboutNotifier.Visible := False;
end;

{ Alter settings if "dates" alarm is (un)selected }

procedure TAlarmClockForm.SetDatesChange(Sender: TObject);

var
  I: Integer;

begin
  if SetDates.Checked then begin
    IniFile.WriteString('General', 'Alarm', 'DATES');
    for I := 1 to 3 do
      AlarmDates[I].Enabled := True;
    for I := 1 to 7 do
      AlarmWeekdays[I].Enabled := False;
  end;
end;

{ Alter settings if "weekdays" alarm is (un)selected }

procedure TAlarmClockForm.SetWeekdaysChange(Sender: TObject);

var
  I: Integer;

begin
  if SetWeekDays.Checked then begin
    IniFile.WriteString('General', 'Alarm', 'WEEKDAYS');
    for I := 1 to 7 do
      AlarmWeekdays[I].Enabled := True;
    for I := 1 to 3 do
      AlarmDates[I].Enabled := False;
  end;
end;

{ Alter settings if "MO" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays1Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(1);
end;

{ Alter settings if "TU" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays2Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(2);
end;

{ Alter settings if "WE" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays3Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(3);
end;

{ Alter settings if "TH" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays4Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(4);
end;

{ Alter settings if "FR" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays5Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(5);
end;

{ Alter settings if "SA" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays6Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(6);
end;

{ Alter settings if "SO" alarm has been (un)selected }

procedure TAlarmClockForm.AlarmWeekdays7Change(Sender: TObject);

begin
  SetWeekdaysAlarmOnOff(7);
end;

{ Alter settings if "MO" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays1Exit(Sender: TObject);

begin
  SetAlarmTime(1);
end;

{ Alter settings if "TU" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays2Exit(Sender: TObject);

begin
  SetAlarmTime(2);
end;

{ Alter settings if "WE" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays3Exit(Sender: TObject);

begin
  SetAlarmTime(3);
end;

{ Alter settings if "TH" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays4Exit(Sender: TObject);

begin
  SetAlarmTime(4);
end;

{ Alter settings if "FR" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays5Exit(Sender: TObject);

begin
  SetAlarmTime(5);
end;

{ Alter settings if "SA" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays6Exit(Sender: TObject);

begin
  SetAlarmTime(6);
end;

{ Alter settings if "SO" alarm time has been changed }

procedure TAlarmClockForm.TimeWeekdays7Exit(Sender: TObject);

begin
  SetAlarmTime(7);
end;

{ Copy last time changed to all selected week days }

procedure TAlarmClockForm.ButtonCopyTimeClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 7 do begin
    if AlarmWeekdays[I].Checked then begin
      TimeWeekdays[I].Time := LastTime;
      IniFile.WriteString('WeekdaysTimes', DayNames[I], TimeToStr(LastTime));
    end;
  end;
end;

{ Alter settings if "MO" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio1Click(Sender: TObject);

begin
  GetAudioFile(1);
end;

{ Alter settings if "TU" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio2Click(Sender: TObject);

begin
  GetAudioFile(2);
end;

{ Alter settings if "WE" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio3Click(Sender: TObject);

begin
  GetAudioFile(3);
end;

{ Alter settings if "TH" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio4Click(Sender: TObject);

begin
  GetAudioFile(4);
end;

{ Alter settings if "FR" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio5Click(Sender: TObject);

begin
  GetAudioFile(5);
end;

{ Alter settings if "SA" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio6Click(Sender: TObject);

begin
  GetAudioFile(6);
end;

{ Alter settings if "SO" audio file has been changed }

procedure TAlarmClockForm.ButtonAudio7Click(Sender: TObject);

begin
  GetAudioFile(7);
end;

{ Copy last audio file changed to all selected week days }

procedure TAlarmClockForm.ButtonCopyAudioClick(Sender: TObject);

var
  I: Integer;
  S: string;

begin
  for I := 1 to 7 do begin
    if AlarmWeekdays[I].Checked then begin
      AudioFiles[I] := LastAudioFile;
      S := ExtractFileName(LastAudioFile);
      ButtonsAudio[I].Caption := Copy(S, 1, Length(S) - 4);
      IniFile.WriteString('WeekdaysAudio', DayNames[I], LastAudioFile);
    end;
  end;
end;

{ Alter settings if 1st date alarm has been (un)selected }

procedure TAlarmClockForm.AlarmDatesA1Change(Sender: TObject);

begin
  SetDatesAlarmOnOff(1);
end;

{ Alter settings if 2nd date alarm has been (un)selected }

procedure TAlarmClockForm.AlarmDatesA2Change(Sender: TObject);

begin
  SetDatesAlarmOnOff(2);
end;

{ Alter settings if 3rd date alarm has been (un)selected }

procedure TAlarmClockForm.AlarmDatesA3Change(Sender: TObject);

begin
  SetDatesAlarmOnOff(3);
end;

{ Alter settings 1st date alarm date/time has been changed }

procedure TAlarmClockForm.TimeDatesA1Exit(Sender: TObject);

begin
  SetAlarmDate(1);
end;

{ Alter settings 2nd date alarm date/time has been changed }

procedure TAlarmClockForm.TimeDatesA2Exit(Sender: TObject);

begin
  SetAlarmDate(2);
end;

{ Alter settings 3rd date alarm date/time has been changed }

procedure TAlarmClockForm.TimeDatesA3Exit(Sender: TObject);

begin
  SetAlarmDate(3);
end;

{ Alter settings if 1st date audio file has been changed }

procedure TAlarmClockForm.ButtonAudioA1Click(Sender: TObject);

begin
  GetAudioFile(8);
end;

{ Alter settings if 2nd date audio file has been changed }

procedure TAlarmClockForm.ButtonAudioA2Click(Sender: TObject);

begin
  GetAudioFile(9);
end;

{ Alter settings if 3rd date audio file has been changed }

procedure TAlarmClockForm.ButtonAudioA3Click(Sender: TObject);

begin
  GetAudioFile(10);
end;

{ Button "Start/Stop": Activate/Deactivate alarm }

procedure TAlarmClockForm.ButtonSetClick(Sender: TObject);

var
  I: Integer;
  ValidAlarms: Boolean;

begin
  if ButtonSet.Caption = 'Start' then begin
    // Activate alarm
    if WeekdaysAlarmSet or DatesAlarmSet then begin
      // Check if there are (valid) alarm times selected
      ValidAlarms := True;
      if DatesAlarmSet then begin
        ValidAlarms := False;
        for I := 1 to 3 do
          if AlarmDates[I].Checked and not PassedDate(TimeDates[I].DateTime) then
            ValidAlarms := True;
      end;
      if ValidAlarms then begin
        // Activate alarm
        AlarmTimer.Interval := 30000;                                                      // timer interval = 30 sec
        AlarmTimer.Enabled := True;                                                        // start timer
        SetWeekdays.Enabled := False; SetDates.Enabled := False;                           // disable weekdays/dates alarm radiobuttons
        if SetWeekdays.Checked then begin
          AlarmClockForm.Caption := 'Alarm Clock  (Weekdays alarms active)';
          for I := 1 to 7 do
            AlarmWeekdays[I].Enabled := False;                                             // disable week days selection
        end
        else begin
          AlarmClockForm.Caption := 'Alarm Clock  (Dates alarms active)';
          for I := 1 to 3 do
            AlarmDates[I].Enabled := False;                                                // disable dates selection
        end;
        ButtonSet.Caption := 'Stop';
      end;
    end;
  end
  else begin
    // Deactivate alarm
    AlarmTimer.Enabled := False;                                                         // stop timer
    AlarmClockForm.Caption := 'Alarm Clock (All alarms stopped)';
    SetWeekdays.Enabled := True; SetDates.Enabled := True;                               // enable weekdays/dates alarm radiobuttons
    if SetWeekdays.Checked then begin
        for I := 1 to 7 do
          AlarmWeekdays[I].Enabled := True;                                              // enable week days selection
      end
      else begin
        for I := 1 to 3 do
          AlarmDates[I].Enabled := True;                                                 // enable dates selection
      end;
    ButtonSet.Caption := 'Start';
  end;
end;

{ Alarm clock timer }

procedure TAlarmClockForm.AlarmTimerTimer(Sender: TObject);

var
  Today: TDateTime;
  TY, TM, TD, TH, TN, AY, AM, AD, AH, AN, S, MS: Word;
  I, N, TodayDay: Integer;
  A: string;
  PlayerStarted: Boolean;

begin
  Today := Now;                                                                          // today's date and time
  DecodeDate(Today, TY, TM, TD); DecodeTime(Today, TH, TN, S, MS);                       // date and time decoded into separate variables
  // "Weekday" alarm clock selected
  if SetWeekDays.Checked then begin
    if AlarmTimer.Interval = 120000 then                                                 // reset normal timer interval (see below)
      AlarmTimer.Interval := 30000;
    TodayDay := DayOfWeek(Today);                                                        // day of the week as integer from 1 (SO) to 7 (SA)
    if TodayDay = 1 then                                                                 // adjust this integer to array index from 1 (MO) to 7 (SO)
      TodayDay := 7
    else
      TodayDay -= 1;
    // Alarm is set for actual day of the week
    if AlarmWeekdays[TodayDay].Checked then begin
      // Ring alarm if actual time = alarm time
      DecodeTime(TimeWeekdays[TodayDay].DateTime, AH, AN, S, MS);
      if (TH = AH) and (TN = AN) then begin
        AlarmTimer.Interval := 120000;                                                   // set timer interval to 2 mins to be sure alarm is rung twice
        StartAudioPlayer(AudioFiles[TodayDay]);                                          // ring alarm (start audio player)
      end;
    end;
  end
  // "Dates" alarm clock selected
  else begin
    N := 0; PlayerStarted := False;
    // For each of the 3 alarm dates
    for I := 1 to 3 do begin
      if AlarmDates[I].Checked then begin
        // Alarm date is actually selected
        Inc(N);                                                                          // increment number of alarms remaining to be rung
        A := 'Alarm' + IntToStr(I);
        DecodeDate(TimeDates[I].DateTime, AY, AM, AD);                                   // date decoded into separate variables
        DecodeTime(TimeDates[I].DateTime, AH, AN, S, MS);                                // time decoded into separate variables
        if (TY = AY) and (TM = AM) and (TD = AD) and (TH = AH) and (TN = AN) then begin  // if actual date/time = date/time set on this alarm
          AlarmDates[I].Checked := False;                                                // reset this alarm (as has being done)
          TimeDates[I].DateTime := Now;
          IniFile.WriteString('DatesAlarms', A, 'False');
          IniFile.WriteString('DatesDateTimes', A, '');
          if not PlayerStarted then                                                      // to be sure to start audio player only once
            StartAudioPlayer(AudioFiles[I + 7]);                                         // ring alarm (start audio player)
          PlayerStarted := True;
          Dec(N);                                                                        // decrement number of alarms remaining to be rung
        end;
      end;
    end;
    // If all alarms have been rung, stop the timer (deactivate the alarm clock)
    if N = 0 then begin
      ButtonSet.Click;                                                                   // stop alarm clock
      FormRestore;                                                                       // restore form window (from tray)
    end;
  end;
end;

{ Click on tray icon: show the AlarmClock window}

procedure TAlarmClockForm.ACTrayIconClick(Sender: TObject);

begin
  FormRestore;                                                                           // restore form window (from tray)
end;

end.

