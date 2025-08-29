{***********************************}
{* Main unit for Clock application *}
{***********************************}

unit clock_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, LazUTF8, IniFiles, DateUtils, clock_u2, clock_u3;

type
  TDST = record
    DST_Month, DST_DayInMonth, DST_DayOfWeek: Integer;
    DST_Time: TTime;
  end;
  TCity = record
    Name, LName: string;
    UsesDST: Boolean;
    OffSetUTCSign, OffSetDSTSign: Char;
    OffsetUTC, OffsetDST: string;
    DSTStart, DSTEnd: string;
    DSTStartDetails, DSTEndDetails: TDST;
  end;
  TCities = array[0..8] of TCity;
  {*********}
  { TfClock }
  {*********}
  TfClock = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsDate, mSettingsDate1, mSettingsDate2, mSettingsDate3: TMenuItem;
    mSettingsTime, mSettingsTime1, mSettingsTime2: TMenuItem;
    mSettingsSeconds, mSettingsClock24, MenuItem1, mSettingsConfig: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    imDraw: TImage;
    laDate: TLabel;
    Label1, Label2: TLabel;
    laCity0, laCity1, laCity2, laCity3, laCity4, laCity5, laCity6, laCity7, laCity8: TLabel;
    laDate0, laDateUTC, laDate1, laDate2, laDate3, laDate4, laDate5, laDate6, laDate7, laDate8: TLabel;
    edTime0, edTimeUTC, edTime1, edTime2, edTime3, edTime4, edTime5, edTime6, edTime7, edTime8: TEdit;
    tiClock: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsDate1Click(Sender: TObject);
    procedure mSettingsDate2Click(Sender: TObject);
    procedure mSettingsDate3Click(Sender: TObject);
    procedure mSettingsTime1Click(Sender: TObject);
    procedure mSettingsTime2Click(Sender: TObject);
    procedure mSettingsSecondsClick(Sender: TObject);
    procedure mSettingsClock24Click(Sender: TObject);
    procedure mSettingsConfigClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure tiClockTimer(Sender: TObject);
  private
    iClockRadius, iFaceRadius: Integer;
    sDateFormat, sTimeFormat: string;
    bStart, bMissingData: Boolean;
    aCities: TCities;
    Bitmap : TBitmap;
    laCities, laDates: array[0..9] of TLabel;
    edTimes: array[0..9] of TEdit;
  end;

const
  iClockBorder = 50;
  // English week days and months names (because functions return them in the Windows local settings language)
  aDayNames: array[1..7] of string = (
    'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'
  );
  aMonthNames: array[1..12] of string = (
    'January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'
  );

var
  fClock: TfClock;
  CitiesFile: TINIFile;

implementation

{$R *.lfm}

{ Read configuration file clock.ini }

procedure ReadCities(out Cities: TCities; out Mess: string);

// Data is read without validity check here; however, the procedure checks if "something" is specified for
// the local city data (and "launch configuration" message will be shown, if some information is missing)

var
  I, P: Integer;
  Section, S: string;

begin
  Mess := '';
  for I := 0 to 8 do begin
    Section := 'City' + IntToStr(I);
    Cities[I].Name := ''; Cities[I].LName := '';
    // Read city name (or names, for home local)
    S := UTF8Trim(CitiesFile.ReadString(Section, 'City', ''));
    if I = 0 then begin
      // Home local
      if S <> '' then begin
        // Name must be set
        P := UTF8Pos(',', S);
        // Split name into English and local city name
        if P = 0 then begin
          Cities[I].Name  := S;
          Cities[I].LName := S;
        end
        else begin
          Cities[I].Name  := UTF8Copy(S, 1, P - 1);
          Cities[I].LName := UTF8Copy(S, P + 1, UTF8Length(S));
        end;
      end
      else begin
        Mess := 'Name of local city not set!';
      end;
    end
    // Other locals
    else begin
      Cities[I].Name := S;
      Cities[I].LName := '';
    end;
    Cities[I].OffsetUTCSign := ' '; Cities[I].OffsetUTC := '00:00';
    Cities[I].UsesDST := False;
    Cities[I].OffsetDSTSign := ' '; Cities[I].OffsetDST := '00:00';
    Cities[I].DSTStart := ''; Cities[I].DSTEnd := '';
    // Read UTC time offset
    S := Trim(CitiesFile.ReadString(Section, 'UTC_Offset', ''));
    if S <> '' then begin
      Cities[I].OffsetUTCSign := LeftStr(S, 1)[1];
      Cities[I].OffsetUTC := RightStr(S, 5);
    end
    else begin
      // Local city: UTC offset must be set
      if I = 0 then begin
        if Mess = '' then
          Mess := 'UTC offset for local city not set!'
        else
          Mess := StringReplace(Mess, 'Name', 'Name and UTC offset', []);
      end;
    end;
    // Read DST time offset
    S := Trim(CitiesFile.ReadString(Section, 'DST_Offset', ''));
    if S <> '' then begin
      // If DST offset is set, proceed reading DST information
      if (I <> 0) or ((I = 0) and (S <> Trim(CitiesFile.ReadString(Section, 'UTC_Offset', '')))) then begin
        // For home local: if DST offset = UTC offset, there is no daylight saving and DST info may be ignored
        Cities[I].UsesDST := True;                                                       // this city uses DST time
        Cities[I].OffsetDSTSign := LeftStr(S, 1)[1]; Cities[I].OffsetDST := RightStr(S, 5);
        Cities[I].DSTStart := Trim(CitiesFile.ReadString(Section, 'DST_Start', ''));
        // For home local: If DST is used, start and end date information must be set
        if Cities[I].DSTStart = '' then begin
          if (I = 0) and (Mess = '') then
            Mess := 'DST start for local city not set!';
        end;
        Cities[I].DSTEnd := Trim(CitiesFile.ReadString(Section, 'DST_End', ''));
        if Cities[I].DSTEnd = '' then begin
          if (I = 0) and (Mess = '') then
            Mess := 'DST end for local city not set!';
        end;
      end;
    end
    else begin
      // DST offset must always have a value. If there is no daylight saving, DST offset must be set
      // equal to UTC offset; just a precaution to not forget about it...
      if (I = 0) and (Mess = '') then
        Mess := 'DST offset for local city not set!';
    end;
  end;
end;

{ Get values of DST start/end data from corresponding string }

procedure GetDSTData(SDST: string; out DSTData: TDST; out Mess: string);

const
  DaysInMonth: array[1..5] of string = (
    'First', 'Second', 'Third', 'Fourth', 'Last'
  );

var
  I, P: Integer;

begin
  Mess := '';
  P := Pos(',', SDST);
  if P > 0 then begin
    // Time
    DSTData.DST_Time := StrToTime(Copy(SDST, P + 2, Length(SDST)), ':');
    // Nth day in month
    SDST := LeftStr(SDST, P - 1);
    P := Pos(' ', SDST);
    if P > 0 then begin
      DSTData.DST_DayInMonth := 0;
      for I := 1 to 5 do begin
        if LeftStr(SDST, P - 1) = DaysInMonth[I] then
          DSTData.DST_DayInMonth := I;
      end;
    end;
    if (P > 0) and (DSTData.DST_DayInMonth > 0) then begin
      // Day of the week
      SDST := Copy(SDST, P + 1, Length(SDST));
      P := Pos(' ', SDST);
      if P > 0 then begin
        DSTData.DST_DayOfWeek := 0;
        for I := 1 to 7 do begin
          if LeftStr(SDST, 3) = aDayNames[I] then
            DSTData.DST_DayOfWeek := I;
        end;
        if (P > 0) and (DSTData.DST_DayOfWeek > 0) then begin
          // Month
          SDST := Copy(SDST, P + 1, Length(SDST));
          SDST := StringReplace(SDST, 'in ', '', []);
          DSTData.DST_Month := 0;
          for I := 1 to 12 do begin
            if SDST = aMonthNames[I] then
              DSTData.DST_Month := I;
          end;
          if DSTData.DST_Month = 0 then
            Mess := 'Invalid month name';
        end
        else
          Mess := 'Invalid day of week name'
      end;
    end
    else
      Mess := 'Invalid day in month';
  end
  else
    Mess := 'Invalid format';
end;

{ Check validity of city information }

function CheckCities(var Cities: TCities; LCities, LDates: array of TLabel; ETimes: array of TEdit): string;

// The function also hides cities without name and fills in the city's DST details record!

var
  I: Integer;
  Mess, MessDST: string;

begin
  Mess := '';
  for I := 8 downto 0 do begin                                                           // stepping backward will give message for first city with bad data
    LCities[I].Visible := True; LDates[I].Visible := True; ETimes[I].Visible := True;
    if Cities[I].Name = '' then begin
      // Hide cities without name
      LCities[I].Visible := False; LDates[I].Visible := False; ETimes[I].Visible := False;
    end
    else begin
      if not IsCorrectOffset(Cities[I].OffSetUTCSign, Cities[I].OffsetUTC) then
        // Check UTC offset
        Mess := Cities[I].Name + ': Invalid UTC offset!'
      else if Cities[I].UsesDST then begin
        // If DST is used, check all DST data
        if not IsCorrectOffset(Cities[I].OffSetDSTSign, Cities[I].OffsetDST) then
          // Check DST offset
          Mess := Cities[I].Name + ': Invalid DST offset!'
        else if ((Cities[I].OffSetUTCSign = '+') and (CompareTime(StrToTime(Cities[I].OffsetDST), StrToTime(Cities[I].OffsetUTC)) <> 1)) or
          ((Cities[I].OffSetUTCSign = '-') and (CompareTime(StrToTime(Cities[I].OffsetDST), StrToTime(Cities[I].OffsetUTC)) <> -1)) then
          // Check if DST offset > UTC offset
          Mess := Cities[I].Name + ': Incompatible UTC/DST offsets!'
        else begin
          // Get and check DST start details (this data has now been filled into the DST details record!)
          GetDSTData(Cities[I].DSTStart, Cities[I].DSTStartDetails, MessDST);
          if MessDST = '' then begin
            // Get and check DST end details (this data has now been filled into the DST details record!)
            GetDSTData(Cities[I].DSTEnd, Cities[I].DSTEndDetails, MessDST);
            if MessDST <> '' then
              Mess := Cities[I].Name + ': DST end details - ' + MessDST;
          end
          else
            Mess := Cities[I].Name + ': DST start details - ' + MessDST;
        end;
      end;
    end;
  end;
  Result := Mess;
end;

{ Check if actual date/time is included in daylight saving period }

function IsDSTDate(Thisday: TDateTime; DSTStart, DSTEnd: TDST): Boolean;

var
  DSTStart_Year, DSTEnd_Year, DSTStart_DayInMonth, DSTEnd_DayInMonth, DD, MM, YY, TH, TM, TS, TMS: Word;
  ItIs: Boolean;
  DSTStart_Date, DSTEnd_Date: TDateTime;

begin
  ItIs := False;
  DecodeDate(ThisDay, YY, MM, DD);
  DSTStart_Year := YY; DSTEnd_Year := YY;
  if DSTEnd.DST_Month < DSTStart.DST_Month then
    // End of daylight saving period is in next year
    Inc(DSTEnd_Year);
  DSTStart_DayInMonth := DSTStart.DST_DayInMonth; DSTEnd_DayInMonth := DSTEnd.DST_DayInMonth;
  // Last weekday in month is originally set to 5th weekday in month. Now checking if this 5th weekday actually exists
  // and if not setting last weekday in month to 4th weekday in month
  if not TryEncodeDayOfWeekInMonth(DSTStart_Year, DSTStart.DST_Month, DSTStart_DayInMonth, DSTStart.DST_DayOfWeek, DSTStart_Date) then
    DSTStart_DayInMonth := 4;
  if not TryEncodeDayOfWeekInMonth(DSTEnd_Year, DSTEnd.DST_Month, DSTEnd_DayInMonth, DSTEnd.DST_DayOfWeek, DSTEnd_Date) then
    DSTEnd_DayInMonth := 4;
  // Determination of DST start date (including time)
  DSTStart_Date := EncodeDayOfWeekInMonth(DSTStart_Year, DSTStart.DST_Month, DSTStart_DayInMonth, DSTStart.DST_DayOfWeek);
  DecodeTime(DSTStart.DST_Time, TH, TM, TS, TMS);
  DSTStart_Date := RecodeTime(DSTStart_Date,  TH, TM, 0, 0);                             // adding DST start time (to Nth day in start month date)
  // Determination of DST end date (including time)
  DSTEnd_Date := EncodeDayOfWeekInMonth(DSTEnd_Year, DSTEnd.DST_Month, DSTEnd_DayInMonth, DSTEnd.DST_DayOfWeek);
  DecodeTime(DSTEnd.DST_Time, TH, TM, TS, TMS);
  DSTEnd_Date := RecodeTime(DSTEnd_Date,  TH, TM, 0, 0);                                 // adding DST end time (to Nth day in end month date)
  // Checking if actual date is within period limited by DST start and DST end dates (times included)
  if (CompareDateTime(ThisDay, DSTStart_Date) = 0) or (CompareDateTime(ThisDay, DSTEnd_Date) = 0) then
    ItIs := True
  else if (CompareDateTime(ThisDay, DSTStart_Date) = 1) and (CompareDateTime(ThisDay, DSTEnd_Date) = -1) then
    ItIs := True;
  Result := ItIs;
end;

{ Reset the clock display }

procedure ClockReset(RC, RF: Integer; HH: Word);

var
  LT, X1, X2, Y1, Y2, I: Integer;
  A: Real;

begin
  // Outer circle
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fClock.imDraw.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  fClock.imDraw.Picture.Bitmap.Canvas.EllipseC(RC, RC, RC, RC);
  // Time (hours) indication numbers
  fClock.imDraw.Picture.Bitmap.Canvas.Font.Color := clYellow;
  fClock.imDraw.Picture.Bitmap.Canvas.Font.Size := 25;
  if fClock.mSettingsClock24.Checked and fClock.mSettingsTime1.Checked and (HH > 12) then begin
    // 24 hours clock display
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(RC - 19, 2, '24');
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(2 * RC - 40, RC - 25, '15');
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(RC - 19, 2 * RC - 49, '18');
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(5, RC - 25, '21');
  end
  else begin
    // 12 hours clock display
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(RC - 17, 2, '12');
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(2 * RC - 30, RC - 25, '3');
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(RC - 10, 2 * RC - 49, '6');
    fClock.imDraw.Picture.Bitmap.Canvas.TextOut(10, RC - 25, '9');
  end;
  // Inner circle
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fClock.imDraw.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fClock.imDraw.Picture.Bitmap.Canvas.EllipseC(RC, RC, RF, RF);
  // Time indication ticks
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Width := 3;
  LT := 25;
  for I := 0 to 11 do begin
    A := I * (2 * Pi / 12);
    X1 := Round(Int(RF * Sin(A))); Y1 := Round(Int(RF * Cos(A)));
    X2 := Round(Int((RF - LT) * Sin(A))); Y2 := Round(Int((RF - LT) * Cos(A)));
    fClock.imDraw.Picture.Bitmap.Canvas.Line(RC + X1 - 1, RC - Y1 - 1, RC + X2 - 1, RC - Y2 - 1);
  end;
  // Clock center (where the hands are fixed)
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clRed;
  fClock.imDraw.Picture.Bitmap.Canvas.Brush.Color := clRed;
  fClock.imDraw.Picture.Bitmap.Canvas.EllipseC(RC, RC, 15, 15);
end;

{ Set the clock (clock display) }

procedure ClockSet(RC, RF: Integer; Thisday: TDateTime; TimeFormat: string);

var
  LH, LM, LS, X, Y: Integer;
  DY, DM, DD, HH, MM, SS, MS: Word;
  A: Real;

begin
  DecodeDate(Thisday, DY, DM, DD); DecodeTime(Thisday, HH, MM, SS, MS);
  ClockReset(RC, RF, HH);                                                                // this removes the hands as they were before
  LH := RF div 2 + 30; LM := RF - 30; LS := LM;                                          // length of the 3 hands
  // Hours hand
  if HH > 12 then
    HH -= 12;
  A := (HH + MM / 60) * (2 * Pi / 12);                                                   // angle, depending on hours and minutes
  X := Round(Int(LH * Sin(A))); Y := Round(Int(LH * Cos(A)));                            // x- and y- component of hours hand line
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Width := 5;
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clRed;
  fClock.imDraw.Picture.Bitmap.Canvas.Line(RC - 2, RC - 2, RC + X - 2, RC - Y - 2);      // draw hours hand
  // Minutes hand
  A := MM * (2 * Pi / 60);                                                               // angle depending on minutes (seconds ignored)
  X := Round(Int(LM * Sin(A))); Y := Round(Int(LM * Cos(A)));                            // x- and y- component of minutes hand line
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Width := 3;
  fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clRed;
  fClock.imDraw.Picture.Bitmap.Canvas.Line(RC - 1, RC - 1, RC + X - 1, RC - Y - 1);      // draw minutes hand
  if LeftStr(TimeFormat, 8) = 'hh:mm:ss' then begin
    // Seconds hand (if selected to display it)
    A := SS * (2 * Pi / 60);                                                             // angle, depending on seconds
    X := Round(Int(LS * Sin(A))); Y := Round(Int(LS * Cos(A)));                          // x- and y- component of seconds hand line
    fClock.imDraw.Picture.Bitmap.Canvas.Pen.Width := 1;
    fClock.imDraw.Picture.Bitmap.Canvas.Pen.Color := clRed;
    fClock.imDraw.Picture.Bitmap.Canvas.Line(RC, RC, RC + X, RC - Y);                    // draw seconds hand
  end;
end;

{ Date/time set for UTC, home local and world cities }

procedure WorldClockSet(var Cities: TCities; LCities, LDates: array of TLabel; ETimes: array of TEdit; Thisday: TDateTime; DateFormat, TimeFormat: string);

var
  HHI, MMI, I: Integer;
  HH, MM, SS, MS: Word;
  UTCToday, CityToday: TDateTime;

begin
  for I := 0 to 8 do begin
    // Home local
    if I = 0 then begin
      // Set date and time as taken from PC system time
      LCities[0].Caption := Cities[0].Name;
      ETimes[0].Text := FormatDateTime(TimeFormat, Thisday);
      LDates[0].Caption := aDayNames[DayOfTheWeek(Thisday)] + ', ' + FormatDateTime(DateFormat, Thisday);
      // Use actual system time as base for UTC time calculation (using UTC/DST info as given in clock.ini for City0)
      if Cities[0].UsesDST and IsDSTDate(Thisday, Cities[0].DSTStartDetails, Cities[0].DSTEndDetails) then
        DecodeTime(StrToTime(Cities[0].OffsetDST), HH, MM, SS, MS)
      else
        DecodeTime(StrToTime(Cities[0].OffsetUTC), HH, MM, SS, MS);
      HHI := HH; MMI := MM;
      if Cities[0].OffSetUTCSign = '+' then begin
        HHI := -HHI; MMI := -MMI;
      end;
      UTCToday := IncMinute(Thisday, MMI); UTCToday := IncHour(UTCToday, HHI);
      // Set UTC date and time
      ETimes[9].Text := FormatDateTime(TimeFormat, UTCToday);
      LDates[9].Caption := aDayNames[DayOfTheWeek(UTCToday)] + ', ' + FormatDateTime(DateFormat, UTCToday);
    end
    // World cities
    else begin
      LCities[I].Caption := Cities[I].Name;
      // Use calculated UTC time as base for city time calculation (using UTC/DST info as given in clock.ini for this city)
      if Cities[I].UsesDST and IsDSTDate(Thisday, Cities[I].DSTStartDetails, Cities[I].DSTEndDetails) then
        DecodeTime(StrToTime(Cities[I].OffsetDST), HH, MM, SS, MS)
      else
        DecodeTime(StrToTime(Cities[I].OffsetUTC), HH, MM, SS, MS);
      HHI := HH; MMI := MM;
      if Cities[I].OffSetUTCSign = '-' then begin
        HHI := -HHI; MMI := -MMI;
      end;
      CityToday := IncMinute(UTCToday, MMI); CityToday := IncHour(CityToday, HHI);
      // Set date and time for this city
      ETimes[I].Text := FormatDateTime(TimeFormat, CityToday);
      LDates[I].Caption := aDayNames[DayOfTheWeek(CityToday)] + ', ' + FormatDateTime(DateFormat, CityToday);
    end;
  end;
end;

{*********}
{ TfClock }
{*********}

{ Application start: Initialisations }

procedure TfClock.FormCreate(Sender: TObject);

begin
  // Create arrays with cities controls
  laCities[0] := laCity0; laDates[0]  := laDate0; edTimes[0]  := edTime0;
  laCities[9] := nil; laDates[9]  := laDateUTC; edTimes[9]  := edTimeUTC;
  laCities[1] := laCity1; laCities[2] := laCity2; laCities[3] := laCity3; laCities[4] := laCity4;
  laCities[5] := laCity5; laCities[6] := laCity6; laCities[7] := laCity7; laCities[8] := laCity8;
  laDates[1]  := laDate1; laDates[2]  := laDate2; laDates[3]  := laDate3; laDates[4]  := laDate4;
  laDates[5]  := laDate5; laDates[6]  := laDate6; laDates[7]  := laDate7; laDates[8]  := laDate8;
  edTimes[1]  := edTime1; edTimes[2]  := edTime2; edTimes[3]  := edTime3; edTimes[4]  := edTime4;
  edTimes[5]  := edTime5; edTimes[6]  := edTime6; edTimes[7]  := edTime7; edTimes[8]  := edTime8;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := imDraw.Width;
  Bitmap.Height := imDraw.Height;
  //Assign the bitmap to the image component (the drawing surface)
  imDraw.Picture.Graphic := Bitmap;
  // Use bStart variable to tell the TfClock.FormActivate method if or not it is executed at application start
  bStart := True;
end;

{ Window activation: At application start, read and check config file and (if all ok) start the clock }

procedure TfClock.FormActivate(Sender: TObject);

// Except for application start, this method does nothing. In fact, I transfered the code here would normally be part of the
// TfClock.FormCreate method. However, transfering it to here, ensures that, in the case the clock can't be started, the GUI
// shows up (and not only a message box, telling to launch configuration or that the application will abort because of bad file data)

var
  Mess: string;

begin
  if bStart then begin
    bStart := False;
    // Read cities data from clock.ini file
    CitiesFile := TINIFile.Create('clock.ini');
    ReadCities(aCities, Mess);
    // All info for home local is present (but not necessarily correct)
    if Mess = '' then begin
      bMissingData := False;
      // Check data for all cities (incl. home local)
      Mess := CheckCities(aCities, laCities, laDates, edTimes);
      // All data correct
      if Mess = '' then begin
        laCity0.Caption := aCities[0].Name;
        // Start the clock
        iClockRadius := imDraw.Width div 2; iFaceRadius := iClockRadius - iClockBorder;
        sDateFormat := 'dd"."mm"."yy'; sTimeFormat := 'hh:mm';
        tiClock.Enabled := True;
      end
      // Corrupted clock.ini file
      else begin
        Mess += ' Application aborted!';
        MessageDlg('File error', Mess, mtError, [mbOK], 0);
        Halt;
      end;
    end
    // Incomplete data for home local. Is the case at the very first run of the application (should not happen otherwise)
    else begin
      bMissingData := True;
      Mess += ' Please, launch configuration!';
      MessageDlg('Missing data', Mess, mtWarning, [mbOK], 0);
    end;
  end;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfClock.mFileExitClick(Sender: TObject);

begin
  CitiesFile.Free;
  tiClock.Enabled := False;
  Close;
end;

{ Menu items "Settings > Date format > ...": Choose a format for display of dates }

procedure TfClock.mSettingsDate1Click(Sender: TObject);

begin
  mSettingsDate1.Checked := True; mSettingsDate2.Checked := False; mSettingsDate3.Checked := False;
  sDateFormat := 'dd"."mm"."yy';
end;

procedure TfClock.mSettingsDate2Click(Sender: TObject);

begin
  mSettingsDate1.Checked := False; mSettingsDate2.Checked := True; mSettingsDate3.Checked := False;
  sDateFormat := 'dd"/"mm"/"yy';
end;

procedure TfClock.mSettingsDate3Click(Sender: TObject);

begin
  mSettingsDate1.Checked := False; mSettingsDate2.Checked := False; mSettingsDate3.Checked := True;
  sDateFormat := 'mm"/"dd"/"yy';
end;

{ Menu items "Settings > Time format > ...": Choose a format for display of time }

procedure TfClock.mSettingsTime1Click(Sender: TObject);

begin
  mSettingsTime1.Checked := True; mSettingsTime2.Checked := False;
  sTimeFormat := StringReplace(sTimeFormat, ' am/pm', '', []);
end;

procedure TfClock.mSettingsTime2Click(Sender: TObject);

begin
  mSettingsTime1.Checked := False; mSettingsTime2.Checked := True;
  if mSettingsTime1.Checked then
    sTimeFormat += ' am/pm';
end;

{ Menu item "Settings > Display seconds": Toggle to display or not seconds and the seconds hand }

procedure TfClock.mSettingsSecondsClick(Sender: TObject);

begin
  if mSettingsSeconds.Checked then begin
    mSettingsSeconds.Checked := False;
    sTimeFormat := StringReplace(sTimeFormat, 'hh:mm:ss', 'hh:mm', []);
  end
  else begin
    mSettingsSeconds.Checked := True;
    sTimeFormat := StringReplace(sTimeFormat, 'hh:mm', 'hh:mm:ss', []);
  end;
end;

{ Menu item "Settings > 24 hours clock": Toggle 12h / 24h clock display }

procedure TfClock.mSettingsClock24Click(Sender: TObject);

begin
  if mSettingsClock24.Checked then
    mSettingsClock24.Checked := False
  else
    mSettingsClock24.Checked := True;
end;

{ Menu items "Settings > Configuration ...": Launch home local configurtion }

procedure TfClock.mSettingsConfigClick(Sender: TObject);

var
  Mess: string;

begin
  tiClock.Enabled := False;
  // Fill the actual data into the fields of the configuration form
  fConfig.edCity.Text := aCities[0].Name; fConfig.edCity2.Text := aCities[0].LName;
  if aCities[0].OffSetUTCSign = ' ' then
    fConfig.edUTC.Text := '±hh:mm'
  else
    fConfig.edUTC.Text := aCities[0].OffSetUTCSign + aCities[0].OffsetUTC;
  if bMissingData then
    fConfig.cbDST.Checked := True                                                        // set DST usage by default to True (just a precaution not to forget about it)
  else
    fConfig.cbDST.Checked := aCities[0].UsesDST;
  if aCities[0].OffSetDSTSign = ' ' then
    fConfig.edDST.Text := '±hh:mm'
  else
    fConfig.edDST.Text := aCities[0].OffSetDSTSign + aCities[0].OffsetDST;
  fConfig.cobDSTStartSeq.ItemIndex := aCities[0].DSTStartDetails.DST_DayInMonth - 1;
  fConfig.cobDSTStartDay.ItemIndex := aCities[0].DSTStartDetails.DST_DayOfWeek - 1;
  fConfig.cobDSTStartMonth.ItemIndex := aCities[0].DSTStartDetails.DST_Month - 1;
  if FormatDateTime('hh:mm', aCities[0].DSTStartDetails.DST_Time) = '00:00' then
    fConfig.edDSTStartTime.Text := 'hh:mm'
  else
    fConfig.edDSTStartTime.Text := TimeToStr(aCities[0].DSTStartDetails.DST_Time);
  fConfig.cobDSTEndSeq.ItemIndex := aCities[0].DSTEndDetails.DST_DayInMonth - 1;
  fConfig.cobDSTEndDay.ItemIndex := aCities[0].DSTEndDetails.DST_DayOfWeek - 1;
  fConfig.cobDSTEndMonth.ItemIndex := aCities[0].DSTEndDetails.DST_Month - 1;
  if FormatDateTime('hh:mm', aCities[0].DSTEndDetails.DST_Time) = '00:00' then
    fConfig.edDSTEndTime.Text := 'hh:mm'
  else
    fConfig.edDSTEndTime.Text := TimeToStr(aCities[0].DSTEndDetails.DST_Time);
  // Show the configuration window and wait until user closes it
  fConfig.ShowModal;
  if fConfig.sButton = 'save' then begin
    // If user pushed "Save" button, retrieve data from configuration form
    aCities[0].Name := fConfig.edCity.Text;
    aCities[0].LName := fConfig.edCity2.Text;
    aCities[0].OffSetUTCSign := LeftStr(fConfig.edUTC.Text, 1)[1];
    aCities[0].OffsetUTC := RightStr(fConfig.edUTC.Text, 5);
    aCities[0].UsesDST := fConfig.cbDST.Checked;
    aCities[0].OffSetDSTSign := LeftStr(fConfig.edDST.Text, 1)[1];
    aCities[0].OffsetDST := RightStr(fConfig.edDST.Text, 5);
    aCities[0].DSTStart := ''; aCities[0].DSTEnd := '';
    if aCities[0].UsesDST then begin
      aCities[0].DSTStart := fConfig.cobDSTStartSeq.Text + ' ' + fConfig.cobDSTStartDay.Text;
      aCities[0].DSTStart += ' in ' + fConfig.cobDSTStartMonth.Text + ', ' + fConfig.edDSTStartTime.Text;
      aCities[0].DSTEnd := fConfig.cobDSTEndSeq.Text + ' ' + fConfig.cobDSTEndDay.Text;
      aCities[0].DSTEnd += ' in ' + fConfig.cobDSTEndMonth.Text + ', ' + fConfig.edDSTEndTime.Text;
    end;
    // Check configuration, write clock.ini and start the clock
    Mess := CheckCities(aCities, laCities, laDates, edTimes);
    // Proceed if all data ok
    if Mess = '' then begin
      bMissingData := False;
      // Write configuration file
      if aCities[0].LName = aCities[0].Name then
        CitiesFile.WriteString('City0', 'City', aCities[0].Name)
      else
        CitiesFile.WriteString('City0', 'City', aCities[0].Name + ',' + aCities[0].LName);
      CitiesFile.WriteString('City0', 'UTC_Offset', aCities[0].OffSetUTCSign + aCities[0].OffsetUTC);
      CitiesFile.WriteString('City0', 'DST_Offset', StringReplace(aCities[0].OffSetDSTSign, ' ', '', []) + aCities[0].OffsetDST);
      CitiesFile.WriteString('City0', 'DST_start', aCities[0].DSTStart);
      CitiesFile.WriteString('City0', 'DST_end', aCities[0].DSTEnd);
      // Start clock
      iClockRadius := imDraw.Width div 2; iFaceRadius := iClockRadius - iClockBorder;
      sDateFormat := 'dd"."mm"."yy'; sTimeFormat := 'hh:mm';
      tiClock.Enabled := True;
    end
    else begin
      // Error in configuration file: Application abort
      Mess += ' Application aborted!';
      MessageDlg('File error', Mess, mtError, [mbOK], 0);
      Halt;
    end;
  end;
  // Also abort the application if (home local) data not complete (may happen if user pushed the "Cancel" button)
  if bMissingData then begin
    MessageDlg('Missing data', 'Invalid configuration. Application aborted!', mtError, [mbOK], 0);
    Halt;
  end;
end;

{ Menu item "Help > Help": Display application help }

procedure TfClock.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfClock.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Multiple locals clock.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April-May 2020.';
  MessageDlg('About "Clock"', S, mtInformation, [mbOK], 0);
end;

{ Timer routine: Read actual system time and update all dates/times accordingly }

procedure TfClock.tiClockTimer(Sender: TObject);

var
  Thisday: TDateTime;

begin
  Thisday := Now;
  laDate.Caption := aCities[0].LName + '  -  ' + FormatDateTime('dddd, dd. mmmm yyyy', Thisday) + ', ' + FormatDateTime(sTimeFormat, Thisday);
  ClockSet(iClockRadius, iFaceRadius, Thisday, sTimeFormat);
  WorldClockSet(aCities, laCities, laDates, edTimes, Thisday, sDateFormat, sTimeFormat);
  tiClock.Interval := 1000;
end;

end.

