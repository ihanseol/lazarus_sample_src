{ ***************************************** }
{ Main unit for Time Calculator application }
{ ***************************************** }

unit calculator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, PopupNotifier, help;

type

  TTimeRecord = record
    Hours, Mins, Secs: Integer;
  end;
  { TCalculatorForm }
  TCalculatorForm = class(TForm)
    CalculatorMenu: TMainMenu;
    MenuItemFileExit: TMenuItem;
    MenuItemHelpHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    OperationDisplay: TEdit;
    AboutNotifier: TPopupNotifier;
    TimeDisplay: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button0: TButton;
    ButtonSeparatorHM: TButton;
    ButtonSeparatorMS: TButton;
    ButtonPlus: TButton;
    ButtonMinus: TButton;
    ButtonCalculate: TButton;
    ButtonClearAll: TButton;
    ButtonClearEntry: TButton;
    ButtonClearDigit: TButton;
    ButtonConvertHH: TButton;
    ButtonConvertMM: TButton;
    ButtonConvertDec: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemFileExitClick(Sender: TObject);
    procedure MenuItemHelpAboutClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button0Click(Sender: TObject);
    procedure ButtonSeparatorHMClick(Sender: TObject);
    procedure ButtonSeparatorMSClick(Sender: TObject);
    procedure ButtonPlusClick(Sender: TObject);
    procedure ButtonMinusClick(Sender: TObject);
    procedure ButtonCalculateClick(Sender: TObject);
    procedure ButtonClearAllClick(Sender: TObject);
    procedure ButtonClearEntryClick(Sender: TObject);
    procedure ButtonClearDigitClick(Sender: TObject);
    procedure ButtonConvertHHClick(Sender: TObject);
    procedure ButtonConvertMMClick(Sender: TObject);
    procedure ButtonConvertDecClick(Sender: TObject);
    procedure MenuItemHelpHelpClick(Sender: TObject);
  end;

var
  CalculatorForm: TCalculatorForm;
  TimeRecord, ResultRecord: TTimeRecord;
  HMAllowed, MSAllowed, ResetEntry, CalculationDone, DecValue: Boolean;
  TimeValue, TimeOperation: string;
  DigitCount: Integer;
  LastOperation: Char;

implementation

{$R *.lfm}

// Procedure: Clear last time value entered

procedure ClearEntry;

begin
  TimeValue := '';
  HMAllowed := True; MSAllowed := True;
  DigitCount := 0;
  CalculatorForm.TimeDisplay.Text := '';
  with TimeRecord do begin
    Hours := 0;
    Mins  := 0;
    Secs  := 0;
  end;
  DecValue := False;
end;

// Procedure: Clear all (reset calculation)

procedure ClearAll;

begin
  ClearEntry;
  CalculationDone := False;
  TimeOperation := '';
  CalculatorForm.OperationDisplay.Text := '';
  with ResultRecord do begin
    Hours := 0;
    Mins  := 0;
    Secs  := 0;
  end;
end;

// Procedure: Digits buttons pressed handling

procedure DigitPressed(Digit: Char);

var
  DigitAllowed: Boolean;
  S: string;

begin
  if CalculationDone then
    ClearAll;
  if ResetEntry then begin
    ClearEntry;
    ResetEntry := False;
  end;
  DigitAllowed := False; Inc(DigitCount);
  if HMAllowed and MSAllowed then
    DigitAllowed := True
  else begin
    if DigitCount <= 2 then begin
      S := TimeValue + Digit;
      if (DigitCount = 2) or ((DigitCount = 1) and (StrToInt(Copy(S, Length(S), 1)) < 6)) then
        DigitAllowed := True;
    end
  end;
  if DigitAllowed then begin
    TimeValue += Digit;
    CalculatorForm.TimeDisplay.Text := TimeValue;
  end
  else
    Dec(DigitCount);
end;

// Procedure: Extract time record from time value string

procedure TimeRecordExtract(var TimeRecord: TTimeRecord; TimeString: string);

var
  P1, P2: Integer;

begin
  with TimeRecord do begin
    Hours := 0; Mins := 0; Secs := 0;
    P1 := Pos('h', TimeString);
    P2 := Pos(':', TimeString);
    if P1 = 0 then begin
      // Time value doesn't include hours
      Mins := StrToInt(Copy(Timestring, 1, P2 - 1));
      Secs := StrToInt(Copy(Timestring, P2 + 1, 2));
    end
    else begin
      // Time value does include hours
      Hours := StrToInt(Copy(Timestring, 1, P1 - 1));
      if P2 = 0 then begin
        // Time value doesn't include minutes
        Mins := StrToInt(Copy(Timestring, P1 + 1, 2));
      end
      else begin
        // Time value does include minutes
        Mins := StrToInt(Copy(Timestring, P1 + 1, 2));
        Secs := StrToInt(Copy(Timestring, P2 + 1, 2));
      end;
    end;
  end;
end;

// Procedure: Time calculation (addition or subtraction)

procedure TimeCalculation(var ResultRecord: TTimeRecord; TimeValue: string; Operation: Char);

  var
    TimeRecord: TTimeRecord;
    T, R: Integer;

begin
  TimeRecordExtract(TimeRecord, TimeValue);
  if Operation = '+' then begin
    // Addition
    with ResultRecord do begin
      Secs += TimeRecord.Secs;
      if Secs >= 60 then begin
        Secs -= 60; Mins += 1;
      end;
      Mins += TimeRecord.Mins;
      while Mins >= 60 do begin
        Mins -= 60; Hours += 1;
      end;
      Hours += TimeRecord.Hours;
    end;
  end
  else begin
    // Subtraction
    with ResultRecord do begin
      T := 3600 * TimeRecord.Hours + 60 * TimeRecord.Mins + TimeRecord.Secs;
      R := 3600 * Hours + 60 * Mins + Secs;
      if R >= T then begin                                                     // do calculation only if result is positive
        if Secs >= TimeRecord.Secs then
          Secs -= TimeRecord.Secs
        else begin
          Secs := Secs + 60 - TimeRecord.Secs; Mins -= 1;
        end;
        if Mins >= TimeRecord.Mins then
          Mins -= TimeRecord.Mins
        else begin
          repeat
            Mins := Mins + 60; Hours -= 1;
          until Mins >= TimeRecord.Mins;
          Mins -= TimeRecord.Mins;
        end;
        Hours -= TimeRecord.Hours;
      end
      else begin
        Hours := 0; Mins := 0; Secs := 0; R := 0;
        MessageDlg('Invalid operation', 'Negative time values not permitted!', mtError, [mbOK], 0);
      end;
    end;
  end;
end;

// Function: Extract time value string fom time record

function TimeValueExtract(TimeRecord: TTimerecord): string;

var
  S: string;

begin
  S := '';
  with TimeRecord do begin
    if Hours <> 0 then                                                          // hours only if not 0
      S += IntToStr(Hours) + 'h';
    if Mins < 10 then
      S += '0';
    S += IntToStr(Mins) + ':';
    if Secs < 10 then
      S += '0';
    S += IntToStr(Secs);
  end;
  TimeValueExtract := S;
end;

{ --------------- }
{ TCalculatorForm }
{ --------------- }

// Application start: Initialize variables

procedure TCalculatorForm.FormCreate(Sender: TObject);

begin
  ClearAll;
  ResetEntry := False;
  CalculationDone := False;
end;

// Menu item "Exit": Exit application

procedure TCalculatorForm.MenuItemFileExitClick(Sender: TObject);

begin
  Close();
end;

// Menu item "Help": Display help text

procedure TCalculatorForm.MenuItemHelpHelpClick(Sender: TObject);

begin
  HelpForm.Show;
end;

// Menu item "About": Display about text

procedure TCalculatorForm.MenuItemHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if AboutNotifier.Visible then
    AboutNotifier.Hide
  else begin
    S := 'Simple calculator to add and subtract time values.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, July, 2017.' + Chr(13);
    S += 'Version 1.1, © allu, July, 2018.';
    AboutNotifier.Text := S;
    AboutNotifier.Show;
  end;
end;

// Digits buttons (1 .. 0): call DigitPressed procedure to create time value string

procedure TCalculatorForm.Button1Click(Sender: TObject);

begin
  DigitPressed('1');
end;

procedure TCalculatorForm.Button2Click(Sender: TObject);

begin
  DigitPressed('2');
end;

procedure TCalculatorForm.Button3Click(Sender: TObject);

begin
  DigitPressed('3');
end;

procedure TCalculatorForm.Button4Click(Sender: TObject);

begin
  DigitPressed('4');
end;

procedure TCalculatorForm.Button5Click(Sender: TObject);

begin
  DigitPressed('5');
end;

procedure TCalculatorForm.Button6Click(Sender: TObject);

begin
  DigitPressed('6');
end;

procedure TCalculatorForm.Button7Click(Sender: TObject);

begin
  DigitPressed('7');
end;

procedure TCalculatorForm.Button8Click(Sender: TObject);

begin
  DigitPressed('8');
end;

procedure TCalculatorForm.Button9Click(Sender: TObject);

begin
  DigitPressed('9');
end;

procedure TCalculatorForm.Button0Click(Sender: TObject);

begin
  DigitPressed('0');
end;

// Hours-minutes separator button (h): insert 'h' into time value string

procedure TCalculatorForm.ButtonSeparatorHMClick(Sender: TObject);

begin
  if HMAllowed then begin
    if TimeValue <> '' then begin
      TimeValue += 'h';
      TimeDisplay.Text := TimeValue;
      HMAllowed := False;
      DigitCount := 0;
    end;
  end;
end;

// Minutes-seconds separator button (:): insert ':' into time value string

procedure TCalculatorForm.ButtonSeparatorMSClick(Sender: TObject);

begin
  if MSAllowed then begin
    if TimeValue <> '' then begin
      if HMAllowed or (not HMAllowed and (DigitCount = 2)) then begin
        TimeValue += ':';
        TimeDisplay.Text := TimeValue;
        MSAllowed := False; HMAllowed := False;
        DigitCount := 0;
      end;
    end;
  end;
end;

// Plus button (+): Time values addition
// TimeCalculation procedure called to do it

procedure TCalculatorForm.ButtonPlusClick(Sender: TObject);

var
  L: Integer;

begin
  if not (HMAllowed and MSAllowed) and (DigitCount = 2) and not DecValue then begin
    if CalculationDone then begin
      TimeValue := TimeValueExtract(ResultRecord);
      TimeOperation := TimeValue + ' + ';
      OperationDisplay.Text := TimeOperation;
      CalculationDone := False;
    end
    else begin
      if TimeOperation = '' then                                             // first time value put into result record
        TimeRecordExtract(ResultRecord, TimeValue)
      else                                                                   // all following time values added or subtracted
        TimeCalculation(ResultRecord, TimeValue, LastOperation);
      TimeOperation += TimeValue + ' + ';                                    // update operation string (for display)
      L := Length(TimeOperation);
      if L <= 70 then
        OperationDisplay.Text := TimeOperation
      else
        OperationDisplay.Text := '... ' + Copy(TimeOperation, L - 69, 70);   // cut off left part of operation string to fit display
    end;
    ResetEntry := True;
    LastOperation := '+';                                                    // this operation will be used for next calculation
  end;
end;

// Minus button (-): Subtraction
// TimeCalculation procedure called to do it

procedure TCalculatorForm.ButtonMinusClick(Sender: TObject);

var
  L: Integer;

begin
  if not (HMAllowed and MSAllowed) and (DigitCount = 2) and not DecValue then begin
    if CalculationDone then begin
      TimeValue := TimeValueExtract(ResultRecord);
      TimeOperation := TimeValue + ' - ';
      OperationDisplay.Text := TimeOperation;
      CalculationDone := False;
    end
    else begin
      if TimeOperation = '' then                                               // first time value put into result record
        TimeRecordExtract(ResultRecord, TimeValue)
      else                                                                     // all following time values added or subtracted
        TimeCalculation(ResultRecord, TimeValue, LastOperation);
      TimeOperation += TimeValue + ' - ';                                      // update operation string (for display)
      L := Length(TimeOperation);
      if L <= 70 then
        OperationDisplay.Text := TimeOperation
      else
        OperationDisplay.Text := '... ' + Copy(TimeOperation, L - 69, 70);     // cut off left part of operation string to fit display
    end;
    ResetEntry := True;
    LastOperation := '-';                                                      // this operation will be used for next calculation
  end;
end;

// Calculate button (=): Calculate final result and display it
// TimeCalculation procedure called to do it

procedure TCalculatorForm.ButtonCalculateClick(Sender: TObject);

var
  L : Integer;

begin
  if (TimeOperation <> '') and not CalculationDone and (DigitCount = 2) and not DecValue then begin
    TimeOperation += TimeValue;                                                // update operation string (for display)
    L := Length(TimeOperation);
    if L <= 70 then
      OperationDisplay.Text := TimeOperation
    else
      OperationDisplay.Text := '... ' + Copy(TimeOperation, L - 69, 70);       // cut off left part of operation string to fit display
    TimeCalculation(ResultRecord, TimeValue, LastOperation);                   // do addition/subtraction for last time value entered
    TimeValue := TimeValueExtract(ResultRecord);
    TimeDisplay.Text := TimeValue;                                             // display calculation result
    CalculationDone := True;
    ResetEntry := True;
  end;
end;

// Clear all button (C): Reset calculation

procedure TCalculatorForm.ButtonClearAllClick(Sender: TObject);

begin
  ClearAll;
end;

// Clear last entry button (CE): Clear current time value

procedure TCalculatorForm.ButtonClearEntryClick(Sender: TObject);

begin
  ClearEntry;
  CalculationDone := False;
  DecValue := False;
end;

// Backspace button (<-): Clear last character of current time value

procedure TCalculatorForm.ButtonClearDigitClick(Sender: TObject);

var
  L: Integer;

begin
  if TimeValue <> '' then begin
    L := Length(TimeValue);
    if Copy(TimeValue, L, 1) = 'h' then                                        // if 'h' separator will be removed...
      HMAllowed := True
    else if Copy(TimeValue, L, 1) = ':' then begin                             // if ':' separator will be removed...
      MSAllowed := True;
      if (L <= 4) or ((L > 4) and (Copy(TimeValue, L - 3, 1) <> 'h')) then
        HMAllowed := True
    end;
    TimeValue := Copy(TimeValue, 1, L - 1);
    TimeDisplay.Text := TimeValue;
    CalculationDone := False;
  end;
end;

// Convert to hours button (-> hh): Display time as HhMM:SS

procedure TCalculatorForm.ButtonConvertHHClick(Sender: TObject);

var
  TimeRecord: TTimeRecord;

begin
  if (TimeValue <> '') and not DecValue and (DigitCount = 2) then begin
    TimeRecordExtract(TimeRecord, TimeValue);
    with TimeRecord do begin
      if Mins >= 60 then begin
        Hours += Mins div 60;                                                  // all 60 minutes displayed as hours
        Mins := Mins mod 60;
      end;
    end;
    TimeValue := TimeValueExtract(TimeRecord);
    if TimeRecord.Hours = 0 then
      TimeValue := '0h' + TimeValue;                                           // include 'h' separator, even if hours = 0
    TimeDisplay.Text := TimeValue;
    ResetEntry := True;
  end;
end;

// Convert to minutes button (-> mm): Display time as MM:SS

procedure TCalculatorForm.ButtonConvertMMClick(Sender: TObject);

var
  TimeRecord: TTimeRecord;

begin
  if (TimeValue <> '') and not DecValue and (DigitCount = 2) then begin
    TimeRecordExtract(TimeRecord, TimeValue);
    with TimeRecord do begin
      if Hours > 0 then begin
        Mins += Hours * 60;                                                    // all hours as 60 minutes
        Hours := 0;
      end;
    end;
    TimeValue := TimeValueExtract(TimeRecord);
    TimeDisplay.Text := TimeValue;
    ResetEntry := True;
  end;
end;

// Convert to decimal button (-> dec): display time as decimal number

procedure TCalculatorForm.ButtonConvertDecClick(Sender: TObject);

var
  DecTime: Real;
  DecUnit: string;

begin
  if (TimeValue <> '') and not DecValue and (DigitCount = 2) then begin
    TimeRecordExtract(TimeRecord, TimeValue);
    with TimeRecord do begin
      if Hours > 0 then begin                                                  // display decimal value as hours
        DecTime := Hours + Mins / 60 + Secs / 3600;
        DecUnit := 'h';
      end
      else begin                                                               // display decimal value as minutes
        DecTime := Hours * 60 + Mins + Secs / 60;
        DecUnit := 'min';
      end;
    end;
    DecValue := True;
    TimeValue := FloatToStr(DecTime) + ' ' + DecUnit;
    TimeDisplay.Text := TimeValue;
    ResetEntry := True;
  end;
end;

end.

