{***************************************}
{* Main unit for Feierdeeg application *}
{***************************************}

unit holidays;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, Grids, LazUTF8;

type
  TDate = record
    YY, MM, DD: Word;
  end;

  {*************}
  { TfFeierdeeg }
  {*************}
  TfFeierdeeg = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edYear: TEdit;
    sgHolidays: TStringGrid;
    btCalc: TButton;
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
  private
    EDate, HDate: TDate;
    DTDate: TDateTime;
  end;

const
  MonthNames: array[1..12] of string = (
    'Januar', 'Februar', 'Mäerz', 'Abrël', 'Mee', 'Juni',
    'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'
  );
  DayNames: array[1..7] of string = (
    'Méindeg', 'Dënschdeg', 'Mëttwoch', 'Donneschdeg', 'Freideg', 'Samschdeg', 'Sonndeg'
  );
  // Holidays with dates depending on Easter
  HolidayNames: array[0..10] of string = (
    'Äschermëttwoch', 'Pällemsonndeg', 'Gréngen Donneschdeg', 'Karfreideg', 'Karsamschdeg',
    'Ouschteren', 'Ouschterméindeg', 'Christi Himmelfaart', 'Péngschten', 'Péngschtméindeg', 'Erläichendag'
  );
  // Difference in days between Easter and given holiday
  HolidayValues: array[0..10] of Integer = (
    -46, -7, -3, -2, -1, 0, +1, +39, +49, +50, +60
  );

var
  fFeierdeeg: TfFeierdeeg;

implementation

{$R *.lfm}

{ Number of month with given name }

function MonthNumber(Month: string): Word;

var
  N, I: Word;

begin
  N := 0;
  for I := 1 to 12 do begin
    if Month = MonthNames[I] then
      N := I;
  end;
  Result := N;
end;

{ Calculation of date of Easter for a given year }

function DateOfEaster(Year: Integer): TDate;

// The date of Easter Sunday is calculated using the improved Gaussian algorithm described at
// https://www.calendarbede.com/book/gauss-easter-algorithm

var
  A, BC, K, P, Q, M, N, D, E: Word;
  EasterDate: TDate;

begin
  A  := Year mod 19;
  BC := (Year + Year div 4 ) mod 7;
  K  := Year div 100;
  P  := (13 + 8 * K) div 25;
  Q  := K div 4;
  M  := 15 - P + K - Q;
  N  := 4 + K - Q;
  D  := (19 * A + M) mod 30;
  if ((D = 28) and (A > 10)) or (D = 29) then
    Dec(D);
  E  := (35 + N - BC - D) mod 7;
  if (D + E + 22) < 32 then begin
    EasterDate.DD := D + E + 22;
    EasterDate.MM := 3;
  end
  else begin
    EasterDate.DD := D + E - 9;
    EasterDate.MM := 4;
  end;
  EasterDate.YY := Year;
  Result := EasterDate;
end;

{ Cacluation of date of a given variable date holiday for the actual date of Easter }

function DateOfHoliday(Holiday: string; EasterDate: TDate): TDate;

var
  I: Integer;
  Found: Boolean;
  Date: TDateTime;
  HolidayDate: TDate;

begin
  Found := False; I := -1;
  while (I < Length(HolidayNames) - 1) and (not Found) do begin
    Inc(I);
    if Holiday = HolidayNames[I] then
      Found := True;
  end;
  if Found then begin
    // Holiday is in list: Calculate and return its date
    Date := EncodeDate(EasterDate.YY, EasterDate.MM, EasterDate.DD);
    Date := IncDay(Date, HolidayValues[I]);
    DecodeDate(Date, HolidayDate.YY, HolidayDate.MM, HolidayDate.DD);
  end
  else begin
    // Holiday is not in list: Return date with day and month set to 0
    HolidayDate.YY := EasterDate.YY;
    HolidayDate.MM := 0;
    HolidayDate.DD := 0;
  end;
  Result := HolidayDate;
end;

{*************}
{ TfFeierdeeg }
{*************}

{ Menu item "Fichier > Verloossen": Exit application }

procedure TfFeierdeeg.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Hëllef > Iwwer": Display application about }

procedure TfFeierdeeg.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := '"Feierdeeg" ass eng Applikatioun, déi ee benotze ka fir den Datum vun Ouschteren an anere ';
  S += 'katoulesche Feierdeeg ze berechnen.' + LineEnding + LineEnding;
  S += 'Versioun 1.0, © allu, Septemeber 2024.';
  MessageDlg('Iwwer "Feierdeeg"', S, mtInformation, [mbOK], 0);
end;

{ Button "Berechnen" pushed: Calculate and display holidays (fill-in the string grid) }

procedure TfFeierdeeg.btCalcClick(Sender: TObject);

var
  D, M, Y, DN, I: Word;
  SDate, Mess: string;

begin
  Mess := '';
  if Length(edYear.Text) = 4 then begin
    // Edit field contains a 4 digit number
    Y := StrToInt(edYear.Text);
    // Calculate date of Easter for actual year
    EDate := DateOfEaster(Y);
    // Fill-in the string grid line by line
    for I := 1 to 18 do begin
      if LeftStr(sgHolidays.Cells[3, I], 4) = 'fest' then begin
        // For holidays with fixed date, determine the day of the week for that date
        M := MonthNumber(RightStr(sgHolidays.Cells[1, I], Length(sgHolidays.Cells[1, I]) - 4));
        D := StrToInt(LeftStr(sgHolidays.Cells[1, I], 2));
        DTDate := EncodeDate(Y, M, D);
        DN := DayOfTheWeek(DTDate);
        sgHolidays.Cells[2, I] := DayNames[DN];                                // fill-in the name of the day
      end
      else begin
        // For holidays with variable date, determine the date for the actual year
        HDate := DateOfHoliday(sgHolidays.Cells[0, I], EDate);
        if HDate.DD = 0 then begin
          // If holiday is not in "variable date holidays" array, display a warning (continue without filling in the date)
          Mess := 'Onbekannte Feierdag " ' + sgHolidays.Cells[0, I] + '"!';
          MessageDlg('Fehler', Mess, mtWarning, [mbOK], 0);
          Mess := '';
        end
        else begin
          // Date has been successfully determined: Fill it into the string grid
          SDate := IntToStr(HDate.DD);
          if Length(SDate) = 1 then
            SDate := '0' + SDate;
          SDate += ' ' + MonthNames[HDate.MM];
          sgHolidays.Cells[1, I] := SDate;
        end;
      end;
    end;
  end
  else begin
    // Year not entered or not a 4-digit number
    Mess := 'Joer net aginn, oder ongeltegt Joer!';
  end;
  if Mess <> '' then
    MessageDlg('Error', Mess, mtError, [mbOK], 0);
end;

end.

