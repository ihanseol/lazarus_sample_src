{************************************}
{* 3-months calendar in 5 languages *}
{* (Version 2.1, © allu, 2017-2020) *}
{************************************}

program Calendar;

// Change log:
// Version 1.0 (June, 2017): Original program
// Version 2.0 (April 2020):
//   - Addition of UTF-8 support for non-English letters
//   - Review of code (with usage of IsLeapYear of SysUtils unit) and addition of suppl. comments
// Version 2.1 (May 2020):
//   - Addition of Spanish language
//   - Reset of language in case of bad input (instead of setting it to English)
//   - Correction of spelling errors

// Not fully usable on Linux: Non-ASCII characters in the day names Luxembourgish
// and Spanish) result in an improper display of the whole calendar!

uses
  Crt, SysUtils, LazUTF8;

const
  Languages : array[1 .. 5] of string = (
    'English', 'French', 'German', 'Luxembourgish', 'Spanish'
  );
  Titles : array[1 .. 5] of string = (
    'Calendar', 'Calendrier', 'Kalender', 'Kalenner', 'Calendario'
  );
  Months : array[1 .. 5, 1 .. 12] of string = (
    ('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'),
    ('Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre'),
    ('Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'),
    ('Januar', 'Februar', 'Mäerz', 'Abrëll', 'Mee', 'Juni', 'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'),
    ('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
  );
  Days : array[1 .. 5, 1 .. 7] of string = (
    ('MO', 'TU', 'WE', 'TH', 'FR', 'SA', 'SO'),
    ('LU', 'MA', 'ME', 'JE', 'VE', 'SA', 'DI'),
    ('MO', 'DI', 'MI', 'DO', 'FR', 'SA', 'SO'),
    ('MÉ', 'DË', 'MË', 'DO', 'FR', 'SA', 'SO'),
    ('LU', 'MA', 'MI', 'JU', 'VI', 'SÁ', 'DO')
  );
  DaysPerMonth : array[1 .. 12] of Byte = (
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  );
  Key_ESC  = 27;
  Key_Left = 75;  Key_Right  = 77;  Key_Up = 72;  Key_Down = 80;
  Key_PgUp = 73;  Key_PgDown = 81;
  Key_F2   = 60;  Key_F3     = 61;

var
  Today : TDateTime;
  DD, MM, YY, MMOld: Word;
  ActualDay, ActualMonth, ActualYear : Word;
  Language, OldLanguage, I : Byte;
  Key : Char;
  Key_Null : Boolean;

{ Display calendar with values selected }

procedure DisplayCalendar(YY, MM, AYear, AMonth, ADay : Word; Lang : Byte);

var
  X0, Y0, X, Y : TCrtCoord;
  DD, WD, D, I, J : Byte;
  YearSpan : Boolean;

begin
  Window(4, 3, 77, 17);
  TextBackground(White); ClrScr;
  X := (77 - (Length(Titles[Lang]) + 6)) div 2;
  TextColor(Blue);
  GotoXY(X, 2); Write(Titles[Lang], '  ', YY);                                 // print calendar title
  GotoXY(X, 3);
  for I := 1 to Length(Titles[Lang]) + 6 do
    Write('=');
  TextColor(Black);
  for I := 0 to 2 do begin                                                     // print (2-letters) names of days
    X := 25 * I + 3; GotoXY(X, 7);
    for J := 1 to 7 do
      Write(Days[Lang, J], ' ');
  end;
  GotoXY(3, 8);
  for I := 1 to 70 do
    Write('-');
  DD := 1; Dec(MM);                                                            // start with first day, one month back
  if MM = 0 then begin
    MM := 12;
    Dec(YY);
  end;
  WD := DayOfWeek(EncodeDate(YY, MM, DD)) - 1;                                 // get and adapt day of the week
  if WD = 0 then
    WD := 7;
  YearSpan := False;
  if (MM = 11) or (MM = 12) then begin
    // Calendar spans over 2 years
    YearSpan := True;
  end;
  // Display calendar for 3 months
  for I := 0 to 2 do begin
    X0 := 25 * I + 3; Y0 := 9;
    Y := Y0;
    D := DaysPerMonth[MM];
    if (MM = 2) and IsLeapYear(YY) then begin
      // If year is a leap year, February is 29 days
      D := D + 1;
    end;
    TextBackground(White); TextColor(Blue);
    GotoXY(X0, 5);
    if (MM = AMonth) and (YY = AYear) then begin
      // Highlight month name if it is actual one
      TextBackground(Blue);
      TextColor(Yellow);
    end;
    Write(Months[Lang, MM]);                                                   // print month name
    if YearSpan then begin
      // If calendar spans over 2 years, add year after month name
      Write(' ', YY);
    end;
    TextBackground(White); TextColor(Black);
    // Print day (at day of week position)
    for J := 1 to D do begin
      X := X0 + 3 * (WD - 1);
      GotoXY(X, Y);
      if J < 10 then
        Write(' ');
      if (YY = AYear) and (MM = AMonth) and (J = ADay) then begin
        // Highlight day (date) if it is actual one
        TextBackground(Blue);
        TextColor(Yellow);
      end;
      Write(J);
      TextBackground(White); TextColor(Black);
      Write(' ');
      Inc(WD);
      if WD = 8 then begin
        WD := 1;
        X := X0; Y := Y + 1;
      end;
    end;
    // Next month
    Inc(MM);
    if MM > 12 then begin
      MM := 1;
      Inc(YY);
    end;
  end;
end;

{****************}
{* Main program *}
{****************}

begin
  ClrScr; CursorOff;
  // Display key help in bottom window
  Window(1, 20, 80, 50);
  TextBackground(Black); TextColor(White);
  GotoXY(6, 1); Write('Left/Down = previous month');
  GotoXY(6, 2); Write('Right/Up  = next month');
  GotoXY(6, 3); Write('PgDown    = previous year');
  GotoXY(6, 4); Write('PgUp      = next year');
  GotoXY(47, 1); Write('F2  = change year and month');
  GotoXY(47, 2); Write('F3  = change calendar language');
  GotoXY(47, 3); Write('ESC = quit program');
  // Initialisation at program start: English calendar with actual date
  Language := 1; Today := Date;
  DeCodeDate(Today, YY, MM, DD);
  ActualDay := DD; ActualMonth := MM; ActualYear := YY;
  // Display calendar as selected by the user (until user terminates program)
  repeat
    DisplayCalendar(YY, MM, ActualYear, ActualMonth, ActualDay, Language);
    TextBackground(Black); TextColor(White);
    Key_Null := False; Key := ' ';
    // Wait for valid key pressed
    repeat
      Key := ReadKey;
      if Ord(Key) = 0 then begin
        Key_Null := True;
        Key := ReadKey;                                                        // read second byte for #0#c keys
      end;
    until (Ord(Key) = Key_ESC) or (Key_Null and (Ord(Key) in [Key_Left, Key_Right, Key_Up, Key_Down, Key_PgUp, Key_PgDown, Key_F2, Key_F3]));
    // Do action, depending on key pressed
    case Ord(Key) of
      // 1 month back
      Key_Left, Key_Down : begin
        Dec(MM);
        if MM = 0 then begin
          MM := 12; Dec(YY);
        end;
      end;
      // 1 month forward
      Key_Right, Key_Up : begin
        Inc(MM);
        if MM = 13 then begin
          MM := 1; Inc(YY);
        end;
      end;
      // 1 year back
      Key_PgDown :
        Dec(YY);
      // 1 year forward
      Key_PgUp :
        Inc(YY);
      // Change year/month
      Key_F2 : begin
        Window(1, 20, 80, 50); CursorOn;
        MMOLd := MM;
        GotoXY(6, 6); Write('Year  ? '); Readln(YY);
        GotoXY(6, 7); Write('Month ? '); Readln(MM);
        if not (MM in [1 .. 12]) then
          MM := MMOld;                                                         // reset month to old value, if user entered invalid month
        GotoXY(1, 6); ClrEoL;                                                  // clear year/month entry text
        GotoXY(1, 7); ClrEoL;
        CursorOff;
      end;
      // Change language
      Key_F3 : begin
        Window(1, 20, 80, 50); CursorOn;
        GotoXY(6, 7); Write('  (');
        for I := 1 to 5 do begin
          Write(I, ' = ');
          if I = Language then
            TextColor(Yellow);                                                 // highlight actual language
          Write(Languages[I]);
          TextColor(White);
          if I < 5 then
            Write(', ');
        end;
        Write(')');
        OldLanguage := Language;
        GotoXY(6, 6); Write('New language [1 - 5] ? '); Readln(Language);
        if not (Language in [1 .. 5]) then
          Language := OldLanguage;                                             // reset language, if user entered invalid language
        GotoXY(1, 6); ClrEoL;                                                  // clear language entry text
        GotoXY(1, 7); ClrEoL;
        CursorOff;
      end;
    end;
  until Ord(Key) = Key_ESC;                                                    // ESC key terminates the program
  // End of program
  Window(1, 1, 80, 50);
  TextBackground(Black); TextColor(White); ClrScr;
end.

