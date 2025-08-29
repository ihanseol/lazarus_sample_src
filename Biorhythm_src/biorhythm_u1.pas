{***************************************}
{* Main unit for Biorhythm application *}
{***************************************}

unit biorhythm_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  {*************}
  { TfBiorhythm }
  {*************}
  TfBiorhythm = class(TForm)
    Title: TLabel;
    DrawingSurface: TImage;
    Label1, Label2: TLabel;
    EBirthDate, EBioDate: TEdit;
    BDraw: TButton;
    BClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BDrawClick(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
  private
    Bitmap : TBitmap;
    iImageLeft, iImageTop, iImageWidth, iImageHeight : Integer;
  end;

var
  fBiorhythm : TfBiorhythm;

implementation

{$R *.lfm}

{ Function: Check if a year is a leapyear}

function IsLeapYear (Y : Word) : Boolean;

var
  LeapYear : Boolean;

begin
  LeapYear := False;
  if Y mod 4 = 0 then begin
    if Y mod 100 = 0 then begin
      if Y mod 400 = 0 then                                                    // leapyear, if divisible by 400
        LeapYear := True
      else
        LeapYear := False                                                      // no leapyear, if divisible by 100
    end
    else
      LeapYear := True;                                                        // leapyear, if divisible by 4
  end;
  IsLeapYear := LeapYear;                                                      // no leap year for other years
end;

{ Function: Determine days per month }

function GetDaysPerMonth(M, Y : Integer) : Integer;

const
  DaysPerMonth : array[1 .. 12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

var
  D : Integer;

begin
  // Get days per month from array, but consider leapyear for February
  if (M = 2) and IsLeapYear(Y) then
    D := 29
  else
    D := DaysPerMonth[M];
  GetDaysPerMonth := D;
end;

{ Function: Calculate biorhythm value (for a given time past since birthday) }

function BiorhythmValue(Cycle: string; T: Integer) : Real;

const
  PI = 3.14159265;

var
  Value : Real;

begin
  // Determine biorhythm value depending on current cycle
  if Cycle = 'Physical' then
    Value := Sin(2 * PI * T / 23)
  else if Cycle = 'Emotional' then
    Value := Sin(2 * PI * T / 28)
  else
	  Value := Sin(2 * PI * T / 33);
  BiorhythmValue := Value;
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
  // Set pen colour depending on current biorhythm cycle
  fBiorhythm.DrawingSurface.Picture.Bitmap.Canvas.Pen.Color := Colour;
  // Draw a line from previous to actual graph coordinates
  if I = 1 then
    fBiorhythm.DrawingSurface.Picture.Bitmap.Canvas.MoveTo(X, Y)
  else begin
    fBiorhythm.DrawingSurface.Picture.Bitmap.Canvas.MoveTo(XL, YL);
    fBiorhythm.DrawingSurface.Picture.Bitmap.Canvas.LineTo(X, Y);
  end;
  // Remember actual coordinates for next line to draw
  XL := X; YL := Y;
end;

{ Clean the drawing surface by displaying a white rectangle }

procedure DrawingSurfaceClean(W, H: Integer);

begin
  fBiorhythm.DrawingSurface.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fBiorhythm.DrawingSurface.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{*************}
{ TfBiorhythm }
{*************}

{ Application start: Initialization }

procedure TfBiorhythm.FormCreate(Sender: TObject);

begin
  iImageLeft := 10;   iImageTop := 40;
  iImageWidth := 800; iImageHeight := 350;
  DrawingSurface.Left := iImageLeft;
  DrawingSurface.Top := iImageTop;
  DrawingSurface.Width := iImageWidth;
  DrawingSurface.Height := iImageHeight;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  DrawingSurface.Picture.Graphic := Bitmap;
  // Clean the drawing surface
  DrawingSurfaceClean(iImageWidth, iImageHeight);
end;

{ Button "Draw It!": Create the biorhythm chart }

procedure TfBiorhythm.BDrawClick(Sender: TObject);

const
  X0 = 35; XF = 8; Y0 = 10;
  Months : array[1 .. 12] of string =
    ('January', 'February', 'March', 'April', 'Mai', 'June', 'July',
     'August', 'September', 'October', 'November', 'December');
  Cycles  : array[1 .. 3] of string = ('Physical', 'Emotional', 'Intellectual');
  Colors  : array[1 .. 3] of TColor = (clGreen, clRed, clBlue);
  LegendX : array[1 .. 3] of Integer = (45, 95, 155);

var
  Day1, Month1, Year1, Day2, Month2, Year2, Day3, Month3, Year3 : Integer;
  DaysM, DayF1, DayF2, DayF3, V, C : Integer;
  Year0, Month0, Day0: Word;
  I, N, N1, N3, T, X1, X2, Y1, Y2, YM, XL, YL : Integer;
  S : string;
  DBirth, DBio, DBio3 : TDateTime;
  DatesOK : Boolean;

begin
  DatesOK := True;
  // Read birth date from form
  S := EBirthDate.Text;
  // Check if birth date is correct format
  if Length(S) <> 10 then
    DatesOK := False
  else begin
    S := StringReplace(S, '/', '', [rfReplaceAll]);
    Val(S, V, C);
    if C <> 0 then
      DatesOK := False;
  end;
  if DatesOK then begin
    DeCodeDate (Date, Year0, Month0, Day0) ;
    Day1 := StrToInt(Copy(S, 1, 2));
    Month1 := StrToInt(Copy(S, 3, 2));
    Year1 := StrToInt(Copy(S, 5, 4));
    // Check birth date validity
    if (Year1 < Year0 - 100) or (Year1 > Year0) or (Month1 < 1) or (Month1 > 12) or ((Year1 = Year0) and (Month1 >= Month0))then
      DatesOK := False
    else begin
      DaysM := GetDaysPerMonth(Month1, Year1);
      if (Day1 < 1) or (Day1 > DaysM) then
        DatesOK := False;
    end;
  end;
  if not DatesOk then begin
    MessageDlg('Date error', 'Birthdate specified is not valid!', mtError, [mbOK], 0);
    EBirthDate.SetFocus;
  end
  else begin
    DBirth := EncodeDate(Year1, Month1, Day1);
    // Read biorhythm date from form
    S := EBioDate.Text;
    // Check if biorhythm date is correct format
    if Length(S) <> 7 then
      DatesOK := False
    else begin
      S := StringReplace(S, '/', '', [rfReplaceAll]);
      Val(S, V, C);
      if C <> 0 then
        DatesOK := False;
    end;
    if DatesOK then begin
      Day2 := 1;
      Month2 := StrToInt(Copy(S, 1, 2));
      Year2 := StrToInt(Copy(S, 3, 4));
      // Check biorhythm date validity
      if (Month2 < 1) or (Month2 > 12) then
        DatesOK := False
      else if (Year2 < Year1) or ((Year2 = Year1) and (Month2 <= Month1)) then
        DatesOK := False;
    end;
    if not DatesOk then begin
      MessageDlg('Date error', 'Biorhythm date specified is not valid!', mtError, [mbOK], 0);
      EBioDate.SetFocus;
    end;
  end;
  if DatesOk then begin
    // Biorhythm chart for 3 months
    DBio := EncodeDate(Year2, Month2, Day2);
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
    S := 'Biorythm chart from ' + Months[Month2] + ' ';
    if Month2 > 10 then
      S := S + IntToStr(Year2) + ' ';
    S := S + 'to ' + Months[Month3] + ' ' + IntToStr(Year3) + '.';
    Title.Caption := S;
    Day3 := GetDaysPerMonth(Month3, Year3);
    DBio3 := EncodeDate(Year3, Month3, Day3);
    // Coordinates of graph area on drawing surface
    X1 := 25; X2 := iImageWidth - 25;
    Y1 := 0; Y2 := iImageHeight - 50;
    YM := (Y2 - Y1) div 2;                                                     // vertical middle of graph area
    N1 := 1; N3 := Round(DBio3 - DBio);
    // Draw x-axis (at vertical middle of graph area)
    DrawingSurfaceClean(iImageWidth, iImageHeight);                            // clean the drawing surface
    DrawingSurface.Picture.Bitmap.Canvas.Pen.Width := 1;                       // pen width for x-axis
    DrawingSurface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
    DrawingSurface.Picture.Bitmap.Canvas.Line(X1, YM, X2, YM);
    for N := N1 to N3 do begin
      if (N = DayF1) or (N = DayF2) or (N = DayF3) then begin                  // first day of month
        I := 1;
        DrawingSurface.Picture.Bitmap.Canvas.Pen.Width := 2;
        DrawingSurface.Picture.Bitmap.Canvas.Line(X0 + XF * N, YM - 10, X0 + XF * N, YM + 10);
      end
      else begin                                                               // 'normal' days of month
        DrawingSurface.Picture.Bitmap.Canvas.Pen.Width := 1;
        Inc(I);
        if (I mod 10 = 0) and (I <> 30) then                                   // every 10th day (except end of month)
          DrawingSurface.Picture.Bitmap.Canvas.Line(X0 + XF * N, YM - 6, X0 + XF * N, YM + 6)
        else
          DrawingSurface.Picture.Bitmap.Canvas.Line(X0 + XF * N, YM - 3, X0 + XF * N, YM + 3);
      end;
    end;
    // Color legend
    DrawingSurface.Picture.Bitmap.Canvas.Font.Color := clBlack;
    DrawingSurface.Picture.Bitmap.Canvas.TextOut(X1, iImageHeight - 30, 'Cycles: ');
    for I := 1 to 3 do begin
      DrawingSurface.Picture.Bitmap.Canvas.Font.Color := Colors[I];
      DrawingSurface.Picture.Bitmap.Canvas.TextOut(X1 + LegendX[I], iImageHeight - 30, Cycles[I]);
    end;
    // Draw the graphs
    DrawingSurface.Picture.Bitmap.Canvas.Pen.Width := 2;                       // pen width for graphs
    XL := 0; YL := 0;                                                          // previous coordinates
    for I := 1 to 3 do begin                                                   // 3 graphs (the 3 cycles)
      for N := N1 to N3 do begin                                               // 3 months (expressed as number of days)
        T := Round(DBio - DBirth) + (N - 1);                                   // actual time (days) passed since birth day
        BiorhythmDraw(Cycles[I], N, T, X0, XF, Y0, YM, Colors[I], XL, YL);     // put actual value onto biorhythm graph
      end;
    end;
  end;
end;

{ Button "Exit": Close the form (exit the application) }

procedure TfBiorhythm.BCloseClick(Sender: TObject);

begin
  close;
end;

end.

