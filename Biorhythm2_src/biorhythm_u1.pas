{****************************************}
{* Main unit for Biorhythm2 application *}
{****************************************}

unit biorhythm_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus, LCLIntf;

type
  {*************}
  { TfBiorhythm }
  {*************}
  TfBiorhythm = class(TForm)
    mMenu: TMainMenu;
    mBiorhythm, mBiorhythmExit: TMenuItem;
    mHelp, mHelpBiorhythm, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle, Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    imCharts: TImage;
    rbBiorhythmYour, rbBiorhythmTheir, rbPhysical, rbIntellectual, rbEmotional: TRadioButton;
    edBirthDateYour, edBirthDateTheir: TEdit;
    laPhysical, laIntellectual, laEmotional, laAverage: TLabel;
    Shape1, Shape2, Shape3, Shape4, shPhysical, shIntellectual, shEmotional, shAverage: TShape;
    Memo1: TMemo;
    btCalculate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mBiorhythmExitClick(Sender: TObject);
    procedure mHelpBiorhythmClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbPhysicalChange(Sender: TObject);
    procedure rbIntellectualChange(Sender: TObject);
    procedure rbEmotionalChange(Sender: TObject);
    procedure btCalculateClick(Sender: TObject);
  private
    Bitmap : TBitmap;
    iImageLeft, iImageTop, iImageWidth, iImageHeight, iCycle : Integer;
  end;

var
  fBiorhythm : TfBiorhythm;

implementation

{$R *.lfm}

{ Determine days per month }

function GetDaysPerMonth(M, Y : Integer) : Integer;

const
  DaysPerMonth : array[1 .. 12] of Integer = (
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  );

var
  D : Integer;

begin
  // Get days per month from array, but consider leapyear for February
  if (M = 2) and IsLeapYear(Y) then
    D := 29
  else
    D := DaysPerMonth[M];
  Result := D;
end;

{ Check birthdate validity }

function IsValidDate(D: string): Boolean;

var
  V, C: Integer;
  Day0, Month0, Year0, Day1, Month1, Year1, DaysM: Word;
  DateOK: Boolean;

begin
  DateOK := True;
  if Length(D) <> 10 then
    DateOK := False
  else begin
    D := StringReplace(D, '/', '', [rfReplaceAll]);
    Val(D, V, C);
    if C <> 0 then
      DateOK := False;
  end;
  if DateOK then begin
    DeCodeDate (Date, Year0, Month0, Day0) ;
    Day1   := StrToInt(Copy(D, 1, 2));
    Month1 := StrToInt(Copy(D, 3, 2));
    Year1  := StrToInt(Copy(D, 5, 4));
    if (Year1 < Year0 - 100) or (Year1 > Year0) or (Month1 < 1) or (Month1 > 12) or ((Year1 = Year0) and (Month1 >= Month0))then
      DateOK := False
    else begin
      DaysM := GetDaysPerMonth(Month1, Year1);
      if (Day1 < 1) or (Day1 > DaysM) then
        DateOK := False;
    end;
  end;
  Result := DateOK;
end;

{ Calculate biorhythm value (for a given time past since birthday) }

function BiorhythmValue(Cycle: string; T: Integer) : Real;

const
  PI = 3.14159265;

var
  Value : Real;

begin
  // Determine biorhythm value depending on current cycle
  Value := 0;
  if Cycle = 'Physical' then
    Value := Sin(2 * PI * T / 23)
  else if Cycle = 'Emotional' then
    Value := Sin(2 * PI * T / 28)
  else if Cycle = 'Intellectual' then
    Value := Sin(2 * PI * T / 33);
  Result := Value;
end;

{ Clean the drawing surface by displaying a white rectangle }

procedure DrawingSurfaceClean(W, H: Integer);

begin
  fBiorhythm.imCharts.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fBiorhythm.imCharts.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fBiorhythm.imCharts.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw biorhythm graph (for a given time past since birthday) }

procedure BiorhythmDraw(Cycle: string; I, T, X0, XF, Y0, YM, Colour: Integer; var XL, YL: Integer);

var
  X, Y, YH : Integer;

begin
  X := X0 + XF * I;                                                            // current x-value on graph
  YH := YM - 2 * Y0;                                                           // maximum y-value on graph
  // Get biorhythm value and determine current y-value on graph
  Y := 2 * Y0 + (YH - Round(YH * BiorhythmValue(Cycle, T)));
  // Set pen colour depending on what is actually drawn
  fBiorhythm.imCharts.Picture.Bitmap.Canvas.Pen.Color := Colour;
  // Draw a line from previous to actual graph coordinates
  if I = 1 then
    fBiorhythm.imCharts.Picture.Bitmap.Canvas.MoveTo(X, Y)
  else begin
    fBiorhythm.imCharts.Picture.Bitmap.Canvas.MoveTo(XL, YL);
    fBiorhythm.imCharts.Picture.Bitmap.Canvas.LineTo(X, Y);
  end;
  // Remember actual coordinates for next line to draw
  XL := X; YL := Y;
end;

{*************}
{ TfBiorhythm }
{*************}

{ Application start: Initialization }

procedure TfBiorhythm.FormCreate(Sender: TObject);

begin
  iImageLeft  := 10;  iImageTop    := 40;
  iImageWidth := 800; iImageHeight := 350;
  imCharts.Left   := iImageLeft;
  imCharts.Top    := iImageTop;
  imCharts.Width  := iImageWidth;
  imCharts.Height := iImageHeight;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imCharts.Picture.Graphic := Bitmap;
  // Clean the drawing surface
  DrawingSurfaceClean(iImageWidth, iImageHeight);
end;

{ Menu item "Biorhythm > Exit": Exit the application }

procedure TfBiorhythm.mBiorhythmExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Biorhythm": Display biorhythms help (open PDF document) }

procedure TfBiorhythm.mHelpBiorhythmClick(Sender: TObject);

begin
  OpenDocument('biorhythms.pdf');
end;

{ Menu item "Help > Help": Display application help }

procedure TfBiorhythm.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Enter your birth date, and the one of your partner. Select the graph that you want to display ';
  S += 'by checking one of the 5 radio buttons. Push "Calculate" to draw the graph and to determine the ';
  S += 'biorhythm compatibility between you and your partner.' + LineEnding;
  S += 'For details about how to interpret the calculations, use "Help > Biorhythm" from the menu bar.';
  MessageDlg('"Biorhythm2" Help.', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfBiorhythm.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Biorhythm partner compatibility.' + LineEnding;
  S += 'Graph of the physical, intellectual, and emotional biorhythm cycles for you, or ';
  S += 'your partner. Comparison of your and their graph for a given cycle. Determination ';
  S += 'of the biorhythm compatibility between you and your partner.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2025.';
  MessageDlg('About "Biorhythm2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculate" pushed: Do the calculations and draw the graphs }

procedure TfBiorhythm.btCalculateClick(Sender: TObject);

const
  X0 = 35; XF = 8; Y0 = 10;
  Months: array[1..12] of string = (
    'January', 'February', 'March', 'April', 'Mai', 'June', 'July',
    'August', 'September', 'October', 'November', 'December'
  );
  Cycles1: array[1..2] of string = (
    'You', 'Them'
  );
  Cycles2: array[1..3] of string = (
    'Physical', 'Intellectual', 'Emotional'
  );
  Colors1: array[1..2] of TColor = (
    clBlue, clGreen
  );
  Colors2: array[1..3] of TColor = (
    clGreen, clBlue, clRed
  );
  LegendX1: array[1..2] of Integer = (
    40, 68
  );
  LegendX2: array[1..3] of Integer = (
    45, 96, 163
  );

var
  Day1, Month1, Year1, Day2, Month2, Year2, Day3, Month3, Year3, DayF1, DayF2, DayF3: Word;
  N, N1, N3, T, X1, X2, Y1, Y2, YM, XL, YL, I: Integer;
  CompPhysical, CompIntellectual, CompEmotional, CompAverage: Integer;
  Date1, Date2, S: string;
  DiffDays, DBirth, DBirth1, DBirth2, DBio, DBio3: TDateTime;
  DateOK: Boolean;

begin
  // Check if birth dates are valid
  Date1 := edBirthDateYour.Text; Date2 := edBirthDateTheir.Text;
  DateOK := IsValidDate(Date1);
  if not DateOK then
    edBirthDateYour.SetFocus
  else begin
    DateOK := IsValidDate(Date2);
    if not DateOK then
      edBirthDateTheir.SetFocus
  end;
  if not DateOk then
    MessageDlg('Date error', 'Birthdate specified is not valid!', mtError, [mbOK], 0)
  // Biorhythm chart for 3 months
  else begin
    // Get birthday values
    Day1 := StrToInt(Copy(Date1, 1, 2)); Month1 := StrToInt(Copy(Date1, 4, 2)); Year1 := StrToInt(Copy(Date1, 7, 4));
    DBirth1 := EncodeDate(Year1, Month1, Day1);
    Day1 := StrToInt(Copy(Date2, 1, 2)); Month1 := StrToInt(Copy(Date2, 4, 2)); Year1 := StrToInt(Copy(Date2, 7, 4));
    DBirth2 := EncodeDate(Year1, Month1, Day1);
    // Determine graph period (this month plus the 2 following)
    DecodeDate(Date, Year2, Month2, Day2);
    DayF1 := 1; DayF2 := GetDaysPerMonth(Month2, Year2) + 1;
    if Month2 > 10 then begin
      Year3 := Year2 + 1;
      if Month2 = 11 then begin
        Month3 := 1;
        DayF3 := DayF2 + GetDaysPerMonth(12, Year2);
      end
      else begin
        Month3 := 2;
        DayF3 := DayF2 + GetDaysPerMonth(1, Year3);
      end;
    end
    else begin
      Month3 := Month2 + 2;
      Year3 := Year2;
      DayF3 := DayF2 + GetDaysPerMonth(Month3 - 1, Year3);
    end;
    Day3 := GetDaysPerMonth(Month3, Year3);
    DBio := EncodeDate(Year2, Month2, 1);
    DBio3 := EncodeDate(Year3, Month3, Day3);
    S := 'Biorhythm chart from ' + Months[Month2] + ' ';
    if Month2 > 10 then
      S := S + IntToStr(Year2) + ' ';
    S := S + 'to ' + Months[Month3] + ' ' + IntToStr(Year3);
    // Graph title
    if rbBiorhythmYour.Checked or rbBiorhythmTheir.Checked then begin
      if rbBiorhythmYour.Checked then
        S += ': Your cycles.'
      else
        S += ': Their cycles.';
    end
    else
      S += ': ' + Cycles2[iCycle] + ' cycle.';
    stTitle.Caption := S;
    // Coordinates of graph area on drawing surface
    X1 := 25; X2 := iImageWidth - 25;
    Y1 := 0; Y2 := iImageHeight - 50;
    YM := (Y2 - Y1) div 2;                                                     // vertical middle of graph area
    N1 := 1; N3 := Round(DBio3 - DBio);
    // Draw x-axis (at vertical middle of graph area)
    DrawingSurfaceClean(iImageWidth, iImageHeight);                            // clean the drawing surface
    imCharts.Picture.Bitmap.Canvas.Pen.Width := 1;                             // pen width for x-axis
    imCharts.Picture.Bitmap.Canvas.Pen.Color := clBlack;
    imCharts.Picture.Bitmap.Canvas.Line(X1, YM, X2, YM);
    for N := N1 to N3 do begin
      if (N = DayF1) or (N = DayF2) or (N = DayF3) then begin
        // First day of month
        I := 1;
        imCharts.Picture.Bitmap.Canvas.Pen.Width := 2;
        imCharts.Picture.Bitmap.Canvas.Line(X0 + XF * N, YM - 10, X0 + XF * N, YM + 10);
      end
      else begin
        // Other days of month
        imCharts.Picture.Bitmap.Canvas.Pen.Width := 1;
        Inc(I);
        if (I mod 10 = 0) and (I <> 30) then                                   // every 10th day (except end of month)
          imCharts.Picture.Bitmap.Canvas.Line(X0 + XF * N, YM - 6, X0 + XF * N, YM + 6)
        else
          imCharts.Picture.Bitmap.Canvas.Line(X0 + XF * N, YM - 3, X0 + XF * N, YM + 3);
      end;
      // Draw a vertical line for today's date
      if N = Day2 then begin
        imCharts.Picture.Bitmap.Canvas.Pen.Width := 1;
        imCharts.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
        imCharts.Picture.Bitmap.Canvas.Line(X0 + XF * N, Y1 + 10, X0 + XF * N, Y2 - 10);
        imCharts.Picture.Bitmap.Canvas.Pen.Color := clBlack;
      end;
    end;
    // Graph legend
    if rbBiorhythmYour.Checked or rbBiorhythmTheir.Checked then begin
      // 3-cycles biorhythm (for you, or your partner)
      imCharts.Picture.Bitmap.Canvas.Font.Color := clBlack;
      imCharts.Picture.Bitmap.Canvas.TextOut(X1, iImageHeight - 30, 'Cycles: ');
      for I := 1 to 3 do begin
        imCharts.Picture.Bitmap.Canvas.Font.Color := Colors2[I];
        imCharts.Picture.Bitmap.Canvas.TextOut(X1 + LegendX2[I], iImageHeight - 30, Cycles2[I]);
      end;
    end
    else begin
      // Biorhythm comparison (for given cycle)
      imCharts.Picture.Bitmap.Canvas.Font.Color := clBlack;
      imCharts.Picture.Bitmap.Canvas.TextOut(X1, iImageHeight - 30, 'Cycle: ');
      for I := 1 to 2 do begin
        imCharts.Picture.Bitmap.Canvas.Font.Color := Colors1[I];
        imCharts.Picture.Bitmap.Canvas.TextOut(X1 + LegendX1[I], iImageHeight - 30, Cycles1[I]);
      end;
    end;
    // Draw the graphs
    imCharts.Picture.Bitmap.Canvas.Pen.Width := 2;                                  // pen width for graphs
    if rbBiorhythmYour.Checked or rbBiorhythmTheir.Checked then begin
      // 3-cycles biorhythm (for you, or your partner)
      if rbBiorhythmYour.Checked then
        DBirth := DBirth1
      else
        DBirth := DBirth2;
      XL := 0; YL := 0;                                                             // previous coordinates
      for I := 1 to 3 do begin                                                      // 3 graphs (the 3 cycles)
        for N := N1 to N3 do begin                                                  // 3 months (expressed as number of days)
          T := Round(DBio - DBirth) + (N - 1);                                      // actual time (days) passed since birth day
          BiorhythmDraw(Cycles2[I], N, T, X0, XF, Y0, YM, Colors2[I], XL, YL);      // put actual value onto biorhythm graph
        end;
      end;
    end
    else begin
      // Biorhythm comparison (for given cycle)
      XL := 0; YL := 0;                                                             // previous coordinates
      for N := N1 to N3 do begin                                                    // 3 months (expressed as number of days)
        T := Round(DBio - DBirth1) + (N - 1);                                       // actual time (days) passed since birth day
        BiorhythmDraw(Cycles2[iCycle], N, T, X0, XF, Y0, YM, Colors1[1], XL, YL);   // put actual value (your biorhythm) onto graph
      end;
      for N := N1 to N3 do begin                                                     // 3 months (expressed as number of days)
        T := Round(DBio - DBirth2) + (N - 1);                                        // actual time (days) passed since birth day
        BiorhythmDraw(Cycles2[iCycle], N, T, X0, XF, Y0, YM, Colors1[2], XL, YL);    // put actual value (their biorhythm) onto graph
      end;
    end;
    // Partner compatibility determination
    DiffDays := (DBio - DBirth1) - (DBio - DBirth2);
    CompPhysical := Round(100 * Abs(Cos(Pi * DiffDays / 23)));
    CompIntellectual := Round(100 * Abs(Cos(Pi * DiffDays / 33)));
    CompEmotional := Round(100 * Abs(Cos(Pi * DiffDays / 28)));
    CompAverage := (CompPhysical + CompIntellectual + CompEmotional) div 3;
    laPhysical.Caption := IntToStr(CompPhysical) + '%'; laPhysical.Visible := True;
    laIntellectual.Caption := IntToStr(CompIntellectual) + '%'; laIntellectual.Visible := True;
    laEmotional.Caption := IntToStr(CompEmotional) + '%'; laEmotional.Visible := True;
    laAverage.Caption := IntToStr(CompAverage) + '%'; laAverage.Visible := True;
    shPhysical.Width := Round(CompPhysical / 100 * 250); shPhysical.Visible := True;
    shIntellectual.Width := Round(CompIntellectual / 100 * 250); shIntellectual.Visible := True;
    shEmotional.Width := Round(CompEmotional / 100 * 250); shEmotional.Visible := True;
    shAverage.Width := Round(CompAverage / 100 * 250); shAverage.Visible := True;
  end;
end;

{ Save cycle if user chooses to display one of the cpmparison graphs (selects one of the corr. radio buttons) }

procedure TfBiorhythm.rbPhysicalChange(Sender: TObject);

begin
  if rbPhysical.Checked then
    iCycle := 1;
end;

procedure TfBiorhythm.rbIntellectualChange(Sender: TObject);

begin
  if rbIntellectual.Checked then
    iCycle := 2;
end;

procedure TfBiorhythm.rbEmotionalChange(Sender: TObject);

begin
  if rbEmotional.Checked then
    iCycle := 3;
end;

end.

