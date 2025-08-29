{*****************************************}
{* Main unit for Equilibrium application *}
{*****************************************}

unit balance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, help;

type
  // Usual vector representation with x and y coordinates
  TCoordinates = record
    X, Y: Integer;
  end;
  // Drawing vector with origin and endpoint
  TCoordinates2 = record
    VStart, VEnd: TCoordinates;
  end;
  {***********}
  { TfBalance }
  {***********}
  TfBalance = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsLevel, mOptionsLevel5, mOptionsLevel4, mOptionsLevel3, mOptionsLevel2, mOptionsLevel1: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label2, Label3, Label4: TLabel;
    StaticText1: TStaticText;
    imDraw: TImage;
    Shape1, shColorA, shColorB, shColorC, shColorRN, shColorRB, shColorX, shColorY: TShape;
    stVectorA, stVectorB, stVectorC, stVectorRN, stVectorRB, stVectorX, stVectorY: TStaticText;
    edVectorA, edVectorB, edVectorC, edVectorRN, edVectorRB, edVectorX, edVectorY: TEdit;
    edAngleA, edAngleB, edAngleC, edAngleRN, edAngleRB, edAngleX, edAngleY: TEdit;
    imEval: TImage;
    btQuestion: TButton;
    btShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsLevel1Click(Sender: TObject);
    procedure mOptionsLevel2Click(Sender: TObject);
    procedure mOptionsLevel3Click(Sender: TObject);
    procedure mOptionsLevel4Click(Sender: TObject);
    procedure mOptionsLevel5Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    iLevel, iLevel0: Integer;
    rMagnitudeRN, rAngleRN, rMagnitudeRB, rAngleRB, rMagnitudeX, rAngleX, rMagnitudeY, rAngleY: Real;
    vRN1, vRN2, vRN, vRB, vX, vY: TCoordinates;
    aVectors: array[0..2] of TCoordinates;
    aDrawVectors: array[0..6] of TCoordinates2;
    edMagnitudes, edAngles: array[0..2] of TEdit;
    bmDraw: TBitmap;
  end;

const
  WidthFields  = 30;                                                           // drawing surface width = 30 fields
  HeightFields = 16;                                                           // drawing surface height = 16 fields
  FieldSize    = 40;                                                           // pixelsize of 1 field
  clOrange = $00A5FF;
  Colors: array[0..6] of TColor = (                                            // colors of the vectors
    clNavy, clBlue, clPurple, clRed, clOrange, clLime, clAqua
  );
  LevelVectors : array[0..4] of Integer = (                                    // number of vectors per level
    1, 2, 2, 3, 3
  );
  LevelVectorsAxis : array[0..4] of Integer = (                                // number of vectors lying along an axis per level
    0, 1, 0, 1, 0
  );

var
  fBalance: TfBalance;

implementation

{$R *.lfm}

{ Calculate nth power of a real number }

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

{ Format real number with f decimal digits (dropping insignificant zeros) }

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

{ Draw an arrowhead at the end of a given line segment }

procedure DrawArrow(Surface: TImage; X1, Y1, X2, Y2: Integer; Colour: TColor);

const
  HeadLength = 8;

var
  xBase, yBase: Integer;
  xLineDelta, yLineDelta, xLineUnitDelta, yLineUnitDelta: Real;
  xNormalDelta, yNormalDelta, xNormalUnitDelta, yNormalUnitDelta: Real;
  PColour, BColour: TColor;

begin
  // Save current colors
  PColour := Surface.Picture.Bitmap.Canvas.Pen.Color;
  BColour := Surface.Picture.Bitmap.Canvas.Brush.Color;
  // Arrow color (as given as argument)
  Surface.Picture.Bitmap.Canvas.Pen.Color := Colour;
  Surface.Picture.Bitmap.Canvas.Brush.Color := Colour;
  // Calculate the arrow's values
  xLineDelta := x2 - x1;
  yLineDelta := y2 - y1;
  xLineUnitDelta := xLineDelta / SQRT( SQR(xLineDelta) + SQR(yLineDelta) );
  yLineUnitDelta := yLineDelta / SQRt( SQR(xLineDelta) + SQR(yLineDelta) );
  xBase := x2 - ROUND(HeadLength * xLineUnitDelta);                            // (xBase,yBase) is where arrow line is perpendicular to base of triangle
  yBase := y2 - ROUND(HeadLength * yLineUnitDelta);
  xNormalDelta :=  yLineDelta;
  yNormalDelta := -xLineDelta;
  xNormalUnitDelta := xNormalDelta / SQRT( SQR(xNormalDelta) + SQR(yNormalDelta) );
  yNormalUnitDelta := yNormalDelta / SQRt( SQR(xNormalDelta) + SQR(yNormalDelta) );
  // Draw the arrow tip
  Surface.Picture.Bitmap.Canvas.Polygon([Point(x2,y2),
  Point(xBase + ROUND(HeadLength * xNormalUnitDelta), yBase + ROUND(HeadLength * yNormalUnitDelta)),
  Point(xBase - ROUND(HeadLength * xNormalUnitDelta), yBase - ROUND(HeadLength * yNormalUnitDelta)) ]);
  // Restore the colours
  Surface.Picture.Bitmap.Canvas.Pen.Color := PColour;
  Surface.Picture.Bitmap.Canvas.Brush.Color := BColour;
end;

{ Clean the drawing surface }

procedure DrawingSurfaceClean(Surface: TImage; W, H, L: Integer);

var
  OX1, OY1, OX2, OY2, I: Integer;

begin
  // Drawing surface as white rectancle
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
  // Draw the object
  Surface.Picture.Bitmap.Canvas.Pen.Color := clSilver;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clSilver;
  OX1 := Round((W / 2 - 0.5) * L); OY1 := Round((H / 2 - 0.5) * L);
  OX2 := Round((W / 2 + 0.5) * L); OY2 := Round((H / 2 + 0.5) * L);
  Surface.Picture.Bitmap.Canvas.Rectangle(OX1, OY1, OX2, OY2);
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  // Horizontal gridline and x-axis
  for I := 1 to H - 1 do begin
    if I = H div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(1, I * L, W * L, I * L);
  end;
  DrawArrow(Surface, (W div 2) * L, (H div 2) * L, W * L, (H div 2) * L, clBlack);  // x-axis arrow
  // Vertical gridline and y-axis
  for I := 1 to W - 1 do begin
    if I = W div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(I * L, 1, I * L, H * L);
  end;
  DrawArrow(Surface, (W div 2) * L, (H div 2) * L, (W div 2) * L, 0, clBlack);      // y-axis arrow
end;

{ Reset all form controls (depending on level = number of forces) }

procedure ResetForm(Surface: TImage; W, H, L, Level: Integer);

var
  N: Integer;

begin
  fBalance.edVectorA.Text := ''; fBalance.edVectorB.Text := ''; fBalance.edVectorC.Text := '';
  fBalance.edAngleA.Text := ''; fBalance.edAngleB.Text := ''; fBalance.edAngleC.Text := '';
  fBalance.edVectorRN.Text := ''; fBalance.edVectorRB.Text := '';
  fBalance.edAngleRN.Text := ''; fBalance.edAngleRB.Text := '';
  fBalance.edVectorX.Text := ''; fBalance.edVectorY.Text := '';
  fBalance.edAngleX.Text := ''; fBalance.edAngleY.Text := '';
  fBalance.imEval.Picture.Clear;
  N := LevelVectors[Level];
  if N = 1 then begin
    // 1 vector
    fBalance.shColorB.Visible := False; fBalance.shColorC.Visible := False;
    fBalance.stVectorB.Visible := False; fBalance.stVectorC.Visible := False;
    fBalance.edVectorB.Visible := False; fBalance.edVectorC.Visible := False;
    fBalance.edAngleB.Visible := False; fBalance.edAngleC.Visible := False;
  end
  else if N = 2 then begin
    // 2 vectors
    fBalance.shColorB.Visible := True; fBalance.shColorC.Visible := False;
    fBalance.stVectorB.Visible := True; fBalance.stVectorC.Visible := False;
    fBalance.edVectorB.Visible := True; fBalance.edVectorC.Visible := False;
    fBalance.edAngleB.Visible := True; fBalance.edAngleC.Visible := False;
  end
  else begin
    // 3 vectors
    fBalance.shColorB.Visible := True; fBalance.shColorC.Visible := True;
    fBalance.stVectorB.Visible := True; fBalance.stVectorC.Visible := True;
    fBalance.edVectorB.Visible := True; fBalance.edVectorC.Visible := True;
    fBalance.edAngleB.Visible := True; fBalance.edAngleC.Visible := True;
  end;
  DrawingSurfaceClean(Surface, W, H, L);                                       // clean drawing surface
end;

{ Draw given vector (using given color) }

procedure DrawVector(Surface: TImage; W, H, L, X1, Y1, X2, Y2: Integer; Colour: TColor);

begin
  // xy-coordinates on the 30x16 drawing surface
  X1 := (W div 2 + X1) * L - 1; X2 := (W div 2 + X2) * L - 1;
  Y1 := (H div 2 - Y1) * L; Y2 := (H div 2 - Y2) * L;
  // Draw the vector
  Surface.Picture.Bitmap.Canvas.Pen.Width := 3;
  Surface.Picture.Bitmap.Canvas.Pen.Color := Colour;
  Surface.Picture.Bitmap.Canvas.Brush.Color := Colour;
  Surface.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
  // Add an arrowhead at the end of the vector line segment
  DrawArrow(Surface, X1, Y1 - 1, X2, Y2 - 1, Colour);
end;

{ Generate a random force }

function RandomVector(N: Integer; Axis: Boolean): TCoordinates;

var
  XMax, YMax: Integer;
  V: TCoordinates;

begin
  case N of
    // Arbitrary max. magnitude values for drawing surface size = 30x16
    1: begin XMax := 15; YMax := 8; end;
    2: begin XMax := 12; YMax := 6; end;
    3: begin XMax := 10; YMax := 5; end;
  end;
  repeat
    if Axis then begin
      // Vector lying on x- or y-axis
      if Random(2) = 0 then begin
        V.X := Random(2 * XMax + 1) - XMax; V.Y := 0;
      end
      else begin
        V.X := 0; V.Y := Random(2 * YMax + 1) - YMax;
      end;
    end
    else begin
      // Angled vector
      repeat
        V.X := Random(2 * XMax + 1) - XMax; V.Y := Random(2 * YMax + 1) - YMax;
      until (V.X <> 0) and (V.Y <> 0);
    end;
  until (V.X <> 0) or (V.Y <> 0);                                              // this would be a null vector
  Result := V;
end;

{ Calculate the sum of 2 vectors }

function AddVectors(V1, V2: TCoordinates): TCoordinates;

var
  V: TCoordinates;

begin
  V.X := V1.X + V2.X;
  V.Y := V1.Y + V2.Y;
  Result := V;
end;

{ Calculate the opposite of a vector }

function OppositeVector(V1: TCoordinates): TCoordinates;

var
  V: TCoordinates;

begin
  V.X := -V1.X;
  V.Y := -V1.Y;
  Result := V;
end;

{ Calculate vector magnitude and angle }

procedure GetVectorValues(Vector: TCoordinates; out Magnitude, Angle: Real);

begin
  Magnitude := Sqrt(Sqr(Vector.X) + Sqr(Vector.Y));
  // All angles as positive values between 0 and 360 degrees
  if Vector.X = 0 then begin
    if Vector.Y > 0 then
      Angle := 90
    else
      Angle := 270;
  end
  else begin
    Angle := ArcTan(Vector.Y/Vector.X) * (360 / (2 * Pi));                     // angle as calculated by formula θ = Arctan(b/a)
    Angle := Abs(Angle);                                                       // use absolute value for positive angle calculation
    if Vector.X > 0 then begin
      if Vector.Y <= 0 then
        // 4th quadrant angle
        Angle := 360 - Angle;
    end
    else begin
      if Vector.Y >= 0 then
        // 2nd quadrant angle
        Angle := 180 - Angle
      else
        // 3rd quadrant angle
        Angle := 180 + Angle;
    end;
  end;
  if Angle = 360 then
    Angle := 0;
end;

{***********}
{ TfBalance }
{***********}

{ Application start: Initialisation }

procedure TfBalance.FormCreate(Sender: TObject);

const
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  SUB_3 = #$E2#$82#$83;

begin
  // Create arrays with vector magnitude and angle edit fields
  edMagnitudes[0] := edVectorA; edMagnitudes[1] := edVectorB; edMagnitudes[2] := edVectorC;
  edAngles[0] := edAngleA; edAngles[1] := edAngleB; edAngles[2] := edAngleC;
  // Create bitmap for canvas drawing
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  // Apply subscripts
  stVectorA.Caption := StringReplace(stVectorA.Caption, '1', SUB_1, []);
  stVectorB.Caption := StringReplace(stVectorB.Caption, '2', SUB_2, []);
  stVectorC.Caption := StringReplace(stVectorC.Caption, '3', SUB_3, []);
  // Exercise startup value
  iLevel0 := 0;
  // Start random generator
  Randomize;
  // Reset form controls (for 1 force exercise)
  ResetForm(imDraw, WidthFields, HeightFields, FieldSize, iLevel0);
end;

{ Menu item "Exercise > New": Start new exercise }

procedure TfBalance.mExerciseNewClick(Sender: TObject);

begin
  iLevel := iLevel0;                                                           // level becomes active now
  ResetForm(imDraw, WidthFields, HeightFields, FieldSize, iLevel);             // reset form controls
  imEval.Visible := False;
  // First button click will be to generate a question
  btQuestion.Caption := 'Question';
  btQuestion.Enabled := True;
  btShow.Enabled := False;
end;

{ Menu item "Exercise > Exit": Exit application }

procedure TfBalance.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Level > ...": Exercise level selection }

procedure TfBalance.mOptionsLevel1Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := True; mOptionsLevel2.Checked := False; mOptionsLevel3.Checked := False;
  mOptionsLevel4.Checked := False; mOptionsLevel5.Checked := False;
  iLevel0 := 0;
end;

procedure TfBalance.mOptionsLevel2Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False; mOptionsLevel2.Checked := True; mOptionsLevel3.Checked := False;
  mOptionsLevel4.Checked := False; mOptionsLevel5.Checked := False;
  iLevel0 := 1;
end;

procedure TfBalance.mOptionsLevel3Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False; mOptionsLevel2.Checked := False; mOptionsLevel3.Checked := True;
  mOptionsLevel4.Checked := False; mOptionsLevel5.Checked := False;
  iLevel0 := 2;
end;

procedure TfBalance.mOptionsLevel4Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False; mOptionsLevel2.Checked := False; mOptionsLevel3.Checked := False;
  mOptionsLevel4.Checked := True; mOptionsLevel5.Checked := False;
  iLevel0 := 3;
end;

procedure TfBalance.mOptionsLevel5Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False; mOptionsLevel2.Checked := False; mOptionsLevel3.Checked := False;
  mOptionsLevel4.Checked := False; mOptionsLevel5.Checked := True;
  iLevel0 := 4;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfBalance.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfBalance.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics trainer: Static equilibrium exercises.' + LineEnding;
  S += 'Balancing the forces that act upon an object.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April 2023.';
  MessageDlg('About "Vectors"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new question resp. check user answer }

procedure TfBalance.btQuestionClick(Sender: TObject);

var
  I: Integer;
  Magnitude, Angle: Real;
  OK: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    ResetForm(imDraw, WidthFields, HeightFields, FieldSize, iLevel);           // reset form controls
    imEval.Visible := False;
    // Random forces (with some constraints, see below...)
    repeat
      OK := True;
      for I := 0 to LevelVectors[iLevel] - 1 do begin
        if (LevelVectorsAxis[iLevel] = 1) and (I = 0) then begin
          // Vector lying on one of the axes
          aVectors[I] := RandomVector(LevelVectors[iLevel], True);
        end
        else begin
          // Angled vector
          aVectors[I] := RandomVector(LevelVectors[iLevel], False);
        end;
      end;
      aDrawVectors[0].VStart.X := 0; aDrawVectors[0].VStart.Y := 0;
      aDrawVectors[0].VEnd := aVectors[0];
      if LevelVectors[iLevel] = 1 then begin
        vRN1 := aVectors[0]; vRN := vRN1;
      end
      else begin
        vRN1 := AddVectors(aVectors[0], aVectors[1]);
        aDrawVectors[1].VStart := aVectors[0]; aDrawVectors[1].VEnd := vRN1;
        if LevelVectors[iLevel] = 2 then begin
          vRN2 := vRN1; vRN := vRN2;
        end
        else begin
          vRN2 := AddVectors(vRN1, aVectors[2]);
          aDrawVectors[2].VStart := vRN1; aDrawVectors[2].VEnd := vRN2;
          vRN := vRN2
        end;
      end;
      vRB := OppositeVector(vRN);
      // Check if forces generated obey to contraints
      if (vRN.X = 0) or (vRN.Y = 0) then
        // Avoid horizontal and vertical net forces
        OK := False
      else begin
        // All vectors have to fit onto the drawing surface
        if (vRN1.X < -WidthFields div 2) or (vRN2.X < -WidthFields div 2) or (vRN.X < -WidthFields div 2) then
          OK := False
        else if (vRN1.X > WidthFields div 2) or (vRN2.X > WidthFields div 2) or (vRN.X > WidthFields div 2) then
          OK := False
        else if (vRN1.Y < -HeightFields div 2) or (vRN2.Y < -HeightFields div 2) or (vRN.Y < -HeightFields div 2) then
          OK := False
        else if (vRN1.Y > HeightFields div 2) or (vRN2.Y > HeightFields div 2) or (vRN.Y > HeightFields div 2) then
          OK := False;
      end;
    until OK;
    // Calculate and display the given forces magnitude and angle; draw the force vectors
    for I := 0 to LevelVectors[iLevel] - 1 do begin
      GetVectorValues(aVectors[I], Magnitude, Angle);
      edMagnitudes[I].Text := RFormat(Magnitude, 3);
      edAngles[I].Text := RFormat(Angle, 3) + '°';
      DrawVector(imDraw, WidthFields, HeightFields, FieldSize, 0, 0, aVectors[I].X, aVectors[I].Y, Colors[I]);
    end;
    // Calculate net force and balancing forces magnitude and angle
    GetVectorValues(vRN, rMagnitudeRN, rAngleRN);
    GetVectorValues(vRB, rMagnitudeRB, rAngleRB);
    vX.X := vRB.X; vX.Y := 0;
    GetVectorValues(vX, rMagnitudeX, rAngleX);
    vY.X := 0; vY.Y := vRB.Y;
    GetVectorValues(vY, rMagnitudeY, rAngleY);
    // Store these forces with the given ones
    aDrawVectors[LevelVectors[iLevel]].VStart.X := 0; aDrawVectors[LevelVectors[iLevel]].VStart.Y := 0;
    aDrawVectors[LevelVectors[iLevel]].VEnd := vRN;
    aDrawVectors[LevelVectors[iLevel] + 1].VStart.X := 0; aDrawVectors[LevelVectors[iLevel] + 1].VStart.Y := 0;
    aDrawVectors[LevelVectors[iLevel] + 1].VEnd := vRB;
    aDrawVectors[LevelVectors[iLevel] + 2].VStart.X := 0; aDrawVectors[LevelVectors[iLevel] + 2].VStart.Y := 0;
    aDrawVectors[LevelVectors[iLevel] + 2].VEnd := vX;
    aDrawVectors[LevelVectors[iLevel] + 3].VStart.X := 0; aDrawVectors[LevelVectors[iLevel] + 3].VStart.Y := 0;
    aDrawVectors[LevelVectors[iLevel] + 3].VEnd := vY;
    // Next button push will be to check user answer
    btShow.Enabled := False;
    btQuestion.Caption := 'Answer';
  end
  // Button "Answer": Check user answer
  else begin
    OK := True;
    // If one of the answer values is false, the global user answer is false...
    if (edVectorRN.Text = '') or (edAngleRN.Text = '') or (edVectorRB.Text = '') or (edAngleRB.Text = '') then
      OK := False
    else if (edVectorX.Text = '') or (edAngleX.Text = '') or (edVectorY.Text = '') or (edAngleY.Text = '') then
      OK := False
    else if (RFormat(StrToFloat(edVectorRN.Text), 3) <> RFormat(rMagnitudeRN, 3)) or (RFormat(StrToFloat(edAngleRN.Text), 3) <> RFormat(rAngleRN, 3)) then
      OK := False
    else if (RFormat(StrToFloat(edVectorRB.Text), 3) <> RFormat(rMagnitudeRB, 3)) or (RFormat(StrToFloat(edAngleRB.Text), 3) <> RFormat(rAngleRB, 3)) then
      OK := False
    else if (RFormat(StrToFloat(edVectorX.Text), 3) <> RFormat(rMagnitudeX, 3)) or (RFormat(StrToFloat(edAngleX.Text), 3) <> RFormat(rAngleX, 3)) then
      OK := False
    else if (RFormat(StrToFloat(edVectorY.Text), 3) <> RFormat(rMagnitudeY, 3)) or (RFormat(StrToFloat(edAngleY.Text), 3) <> RFormat(rAngleY, 3)) then
      OK := False;
    // Display "correct" resp. "false" picture
    if OK then
      imEval.Picture.LoadFromFile('correct.png')
    else
      imEval.Picture.LoadFromFile('false.png');
    imEval.Visible := True;
    // Next button push will be to generate a new question
    btShow.Enabled := True;
    btQuestion.Caption := 'Question';
  end;
end;

{ Button "Show": Show forces drawing and values }

procedure TfBalance.btShowClick(Sender: TObject);

var
  X1, Y1, X2, Y2, I: Integer;
  Colour: TColor;

begin
  // Fill in force values
  edVectorRN.Text := RFormat(rMagnitudeRN, 3); edAngleRN.Text := RFormat(rAngleRN, 3) + '°';
  edVectorRB.Text := RFormat(rMagnitudeRB, 3); edAngleRB.Text := RFormat(rAngleRB, 3) + '°';
  edVectorX.Text := RFormat(rMagnitudeX, 3); edAngleX.Text := RFormat(rAngleX, 3) + '°';
  edVectorY.Text := RFormat(rMagnitudeY, 3); edAngleY.Text := RFormat(rAngleY, 3) + '°';
  DrawingSurfaceClean(imDraw, WidthFields, HeightFields, FieldSize);           // clean drawing surface
  // Display all vectors
  for I := 0 to LevelVectors[iLevel] + 3 do begin
    if I = LevelVectors[iLevel] then
      // Net force
      Colour := Colors[3]
    else if I = LevelVectors[iLevel] + 1 then
      // Balance force
      Colour := Colors[4]
    else if I = LevelVectors[iLevel] + 2 then
      // Balance force (x-axis)
      Colour := Colors[5]
    else if I = LevelVectors[iLevel] + 3 then
      // Balance force (y-axis)
      Colour := Colors[6]
    else
      // Other vectors
      Colour := Colors[I];
    X1 := aDrawVectors[I].VStart.X; Y1 := aDrawVectors[I].VStart.Y;
    X2 := aDrawVectors[I].VEnd.X; Y2 := aDrawVectors[I].VEnd.Y;
    DrawVector(imDraw, WidthFields, HeightFields, FieldSize, X1, Y1, X2, Y2, Colour);
  end;
end;

end.

