{*************************************}
{* Main unit for Springs application *}
{*************************************}

unit spring;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, LCLIntf;

type
  TArray = array of Real;
  {***********}
  { TfSprings }
  {***********}
  TfSprings = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imDraw: TImage;
    Label1, Label11, Label12, Label13, Label16, Label17, Label19: TLabel;
    Label2, Label21, Label15, Label18, Label10, Label20, Label22: TLabel;
    Label23, Label4, Label14, Label8, Label9: TLabel;
    edSpringStretch0, edBlocMass, edSpringConstant: TEdit;
    edSpringStretch, edTimeConstant, edPeriod: TEdit;
    edCalcTime, edGraphTimeStart, edGraphTimeEnd: TEdit;
    btCalc: TButton;
    btDraw: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure edBlocMassChange(Sender: TObject);
    procedure edSpringConstantChange(Sender: TObject);
    procedure edSpringStretch0Change(Sender: TObject);
    procedure edCalcTimeChange(Sender: TObject);
  private
    iCurve: Integer;
    rMass, rConstant, rStretch0, rCalcTime, rGraphTimeStart, rGraphTimeEnd, rMax: Real;
    aTimes, aStretches: TArray;
    bmDraw: TBitmap;
  end;

const
  clOrange = $00A5FF; clPurple = $DB7093;
  Colours: array[0..7] of TColor = (
    clRed, clBlue, clGreen, clLime, clOrange, clFuchsia, clAqua, clPurple
  );

var
  fSprings: TfSprings;

implementation

{$R *.lfm}

{ Integer power of a real number }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Format real number (with given number of decimal digits) }

function RFormat(R: Real; F: Integer): string;

var
  R0: Real;
  SR: string;

begin
  SR := '';
  if R = 0 then
    SR := '0'
  else begin
    if F >= 0 then begin
      R0 := Round(R * Power(10, F)) / Power(10, F);
      if Abs(R0) < Power(10, -F) then
        SR := FloatToStrF(R, ffExponent, F, 0)
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Maximum value of an array }

function ArrayMax(A: TArray): Real;

var
  I: Integer;
  Max: Real;

begin
  Max := Abs(A[0]);
  for I := 1 to Length(A) - 1 do begin
    if Abs(A[I]) > Max then
      Max := Abs(A[I]);
  end;
  Result := Max;
end;

{ Reset drawing surface }

procedure DrawingSurfaceReset(Surface: TImage);

var
  W, H, I: Integer;

begin
  // Drawing surface as white rectancle
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
  W := Surface.Width; H := Surface.Height - 40;
  // X-axis
  Surface.Picture.Bitmap.Canvas.Line(10, H div 2, W - 10, H div 2);
  // Y-axis
  Surface.Picture.Bitmap.Canvas.Line(70, 10, 70, H - 10);
  // X-ticks
  Surface.Picture.Bitmap.Canvas.Line(70, H + 5, W - 10, H + 5);
  for I := 0 to 10 do
    Surface.Picture.Bitmap.Canvas.Line(100 * I + 70, H, 100 * I + 70, H + 10);
  // Y-ticks
  for I := -5 to 5 do
    Surface.Picture.Bitmap.Canvas.Line(65, H div 2 + 62 * I, 75, H div 2 + 62 * I);
end;

{ Read user input from the form }

procedure ReadParams(out M, K, X0, TC, TGStart, TGEnd: Real; IsCalc: Boolean; out Mess: string);

begin
  Mess := '';
  if fSprings.edBlocMass.Text = '' then
    M := 0
  else
    M := StrToFloat(fSprings.edBlocMass.Text);
  if fSprings.edSpringConstant.Text = '' then
    K := 0
  else
    K := StrToFloat(fSprings.edSpringConstant.Text);
  if fSprings.edSpringStretch0 .Text = '' then
    X0 := 0
  else
    X0 := StrToFloat(fSprings.edSpringStretch0.Text);
  if IsCalc then begin
    if fSprings.edCalcTime.Text = '' then
      TC := -1
    else
      TC := StrToFloat(fSprings.edCalcTime.Text);
  end;
  if not IsCalc then begin
    if fSprings.edGraphTimeStart.Text = '' then
      TGStart := -1
    else
      TGStart := StrToFloat(fSprings.edGraphTimeStart.Text);
  end;
  if not IsCalc then begin
    if fSprings.edGraphTimeEnd.Text = '' then
      TGEnd := -1
    else
      TGEnd := StrToFloat(fSprings.edGraphTimeEnd.Text);
  end;
  if M <= 0 then begin
    Mess := 'Bloc mass missing or invalid';
    fSprings.edBlocMass.SetFocus;
  end
  else if K <= 0 then begin
    Mess := 'Spring constant missing or invalid';
    fSprings.edSpringConstant.SetFocus;
  end
  else if IsCalc and (TC < 0) then begin
    Mess := 'Calculation time missing or invalid';
    fSprings.edCalcTime.SetFocus;
  end
  else if (not IsCalc) and (TGStart < 0) then begin
    Mess := 'Graph start time missing or invalid';
    fSprings.edGraphTimeStart.SetFocus;
  end
  else if (not IsCalc) and (TGEnd < 0) then begin
    Mess := 'Graph end time missing or invalid';
    fSprings.edGraphTimeEnd.SetFocus;
  end
  else if (not IsCalc) and (TGEnd <= TGStart) then begin
    Mess := 'The graph end time must be greater than the graph start time';
    fSprings.edGraphTimeStart.SetFocus;
  end;
  if Mess <> '' then
    MessageDlg('Invalid parameters', Mess + '!', mtError, [mbOK], 0);
end;

{ Spring calculations }

procedure SpringCalc(M, K, X0, CalcTime, StartTime, EndTime: Real; IsCalc: Boolean; out Times, Stretches: TArray);

var
  I: Integer;
  T, X, DT, E, Period, TimeCnst: Real;

begin
  E := (-20 * PI * Sqrt(M / K)) / (Ln(0.37));
  // Period and time constant
  Period := 2 * Pi * Sqrt(M / K);
  fSprings.edPeriod.Text := RFormat(Period, 3);
  TimeCnst := -10 * Period / Ln(0.37);
  fSprings.edTimeConstant.Text := RFormat(TimeCnst, 3);
  // Stretch for given time value
  if IsCalc then begin
    X := X0 * Exp(-CalcTime / E) * Sin((Sqrt(K / M) * CalcTime));
    fSprings.edSpringStretch.Text := RFormat(X, 3);
  end;
  // Stretch over time (1000 values)
  T := StartTime; DT := (EndTime - StartTime) / 1000;
  SetLength(Times, 1000); SetLength(Stretches, 1000);
  I := 0;
  repeat
    X := X0 * Exp(-T / E) * Sin((Sqrt(K / M) * T));
    Times[I] := T;
    Stretches[I] := X;
    T += DT; Inc(I);
  until (T >= EndTime);
end;

{ Spring stretch vs. time graph }

procedure DrawGraph(Surface: TImage; StartTime, EndTime: Real; var Curve: Integer; var Max0: Real; var Times, Stretches: TArray);

const
  Colors: array[0..5] of TColor = (
    clBlue, clRed, clLime, clFuchsia, clAqua, clGreen
  );

var
  W, H, HM, X, Y, I: Integer;
  Max: Real;

begin
  W := Surface.Width; H:= Surface.Height - 40; HM := H div 2;
  // Maximum y-value (spring stretch)
  // If multiple curves are drawn on the same graph, the maximum of the first curve has to be used
  // If the actual maximum is greater than the one of the first curve, the curve wouldn't fit on the graph
  // and thus will not be drawn
  Max := ArrayMax(Stretches);
  if Curve = -1 then
    Max0 := Max;
  if Max - Max0 > 1E-3 then
    MessageDlg('Drawing error', 'Curve will not fit on drawing area!', mtError, [mbOK], 0)
  else begin
    Max := Max0;
    // Color of the curve
    Inc(Curve);
    if Curve = 6 then
      Curve := 0;
    // Curve drawing settings
    Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
    Surface.Picture.Bitmap.Canvas.Pen.Color := Colors[Curve];
    // Drawing the curve
    for I := 0 to Length(Times) - 1 do begin
      // X-value (time) on graph
      X := Round(((Times[I] - StartTime) / (EndTime - StartTime) * (W - 80)) + 70);
      // Y-value (stretch) on graph
      Y := (HM + 20) - (Round((Stretches[I] / (2 * Max)) * (H - 40)) + 20);
      if I = 0 then                                                            // for the first point of the curve:
        Surface.Picture.Bitmap.Canvas.MoveTo(X, Y)                             // position the pen
      else                                                                     // for all other points of the curve:
        Surface.Picture.Bitmap.Canvas.LineTo(X, Y);                            // draw line from previous point to here
    end;
  end;
end;

{***********}
{ TfSprings }
{***********}

{ Application start: Initialization }

procedure TfSprings.FormCreate(Sender: TObject);

begin
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  DrawingSurfaceReset(imDraw);
  iCurve := -1;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfSprings.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display application help (PDF) }

procedure TfSprings.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('SimpleSpring.pdf');                                            // open file in default PDF viewer
end;

{ Menu item "Help > About": Display application about }

procedure TfSprings.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics of springs:' + LineEnding;
  S += 'Damped motion of a vertical spring (Simple calculation of the spring stretch vs. time).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March-April 2025.';
  MessageDlg('About "SimpleSpring"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculation" clicked: Calculate stretch of the spring for user defined time }

procedure TfSprings.btCalcClick(Sender: TObject);

var
  Mess: string;

begin
  ReadParams(rMass, rConstant, rStretch0, rCalcTime, rGraphTimeStart, rGraphTimeEnd, True, Mess);
  if Mess = '' then
    SpringCalc(rMass, rConstant, rStretch0, rCalcTime, rGraphTimeStart, rGraphTimeEnd, True, aTimes, aStretches);
end;

{ Button "Draw" clicked: Draw the spring stretch vs. time graph (for user defined time interval) }

procedure TfSprings.btDrawClick(Sender: TObject);

var
  Mess: string;

begin
  ReadParams(rMass, rConstant, rStretch0, rCalcTime, rGraphTimeStart, rGraphTimeEnd, False, Mess);
  if Mess = '' then begin
    SpringCalc(rMass, rConstant, rStretch0, rCalcTime, rGraphTimeStart, rGraphTimeEnd, False, aTimes, aStretches);
    DrawGraph(imDraw, rGraphTimeStart, rGraphTimeEnd, iCurve, rMax, aTimes, aStretches);
  end;
end;

{ Button "Clear" clicked: Clear the grapg drawing area }

procedure TfSprings.btClearClick(Sender: TObject);

begin
  DrawingSurfaceReset(imDraw);
  iCurve := -1;
end;

{ Clear calculation values when user input changes }

procedure TfSprings.edBlocMassChange(Sender: TObject);

begin
  edPeriod.Text := ''; edTimeConstant.Text := ''; edSpringStretch.Text := '';
end;

procedure TfSprings.edSpringConstantChange(Sender: TObject);

begin
  edPeriod.Text := ''; edTimeConstant.Text := ''; edSpringStretch.Text := '';
end;

procedure TfSprings.edSpringStretch0Change(Sender: TObject);

begin
  edPeriod.Text := ''; edTimeConstant.Text := ''; edSpringStretch.Text := '';
end;

procedure TfSprings.edCalcTimeChange(Sender: TObject);

begin
  edPeriod.Text := ''; edTimeConstant.Text := ''; edSpringStretch.Text := '';
end;

end.

