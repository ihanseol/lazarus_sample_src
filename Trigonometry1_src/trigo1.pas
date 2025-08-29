{*******************************************}
{* Main unit for Trigonometry1 application *}
{*******************************************}

unit trigo1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, Math;

type
  TTriangles  = array[0..1, 0..5] of Real;
  TDisplayTriangles  = array[0..1, 0..5] of Boolean;
  TEditFields = array[0..1, 0..5] of TEdit;
  TLabels = array[0..1, 0..5] of TLabel;
  {*********}
  { TfTrigo }
  {*********}
  TfTrigo = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mExercises, mExercises1, mExercises2, mExercises3, mExercises4, mExercises5: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laQuestion: TLabel;
    imExercise: TImage;
    Label1, Label2, laT1, laT2: TLabel;
    laT1ac, laT1bc, laT1ab, laT2ac, laT2bc, laT2ab: TLabel;
    laFunc1, laFunc2, laFunc3: TLabel;
    edT1ac, edT1bc, edT1ab, edT2ac, edT2bc, edT2ab: TEdit;
    edFunc1, edFunc2, edFunc3: TEdit;
    sgEval: TStringGrid;
    imEval: TImage;
    btTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mExercises1Click(Sender: TObject);
    procedure mExercises2Click(Sender: TObject);
    procedure mExercises3Click(Sender: TObject);
    procedure mExercises4Click(Sender: TObject);
    procedure mExercises5Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btTestClick(Sender: TObject);
  private
    iExercise, iQuestion, iCorrect: Integer;
    aTestExercises: array[0..4] of Boolean;
    aTriangles: TTriangles;
    laTriangles: TLabels;
    edTriangles: TEditFields;
    bmDraw: TBitmap;
  end;

const
  TrigoFunctions: array[0..5] of string = (
    'cos', 'sin', 'tan', 'cot', 'sec', 'csc'
  );

var
  fTrigo: TfTrigo;

implementation

{$R *.lfm}

{ Format real number (drop non significant decimal digits )}

function RFormat(R: Real): string;

var
  F: Integer;

begin
  if R = Int(R) then
    F := 0
  else if 10 * R = Int(10 * R) then
    F := 1
  else if 100 * R = Int(100 * R) then
    F := 2
  else
    F := 3;                                                                    // default number of decimal digits displayed = 3
  Result := FloatToStrF(R, ffFixed, 0, F);
end;

{ Format numbers for grid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN += S;                                                                   // this is for the '%' sign
  Result := SN;
end;

{ Clear the drawing surface }

procedure DrawingSurfaceClear(Surface: TImage);

begin
  // Drawing surface as white rectancle
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
end;

{ Init form and variables for actual exercise }

procedure ResetExercise(Exercise: Integer; var LTriangles: TLabels; var ETriangles: TEditFields; out DTriangles: TDisplayTriangles);

const
  TriangleSides: array[0..1, 0..2] of string = (
    ('AC', 'BC', 'AB'),
    ('A''C''', 'B''C''', 'A''B''')
  );
  TriangleAngles: array[0..1, 0..2] of string = (
    ('α', 'β', 'γ'),
    ('α''', 'β''', 'γ''')
  );

var
  I, J: Integer;

begin
  if Exercise = 1 then begin
    fTrigo.laT1.Caption := 'Triangle 1';
    fTrigo.laT2.Visible := True;
    fTrigo.laT2.Caption := 'Triangle 2';
  end
  else begin
    fTrigo.laT1.Caption := 'Triangle';
    fTrigo.laT2.Visible := False;
  end;
  // Show or hide triangle(s) side/angle input fields as needed
  for I := 0 to 2 do begin
    ETriangles[0, I].Text := '';
    ETriangles[0, I].ReadOnly := False; ETriangles[0, I].TabStop := True;
    LTriangles[1, I].Visible  := False; ETriangles[1, I].Visible := False;
    if Exercise in [1, 3, 4] then begin
      LTriangles[1, I].Visible  := True;
      ETriangles[1, I].Visible  := True; ETriangles[1, I].Text := '';
      ETriangles[1, I].ReadOnly := False; ETriangles[1, I].TabStop := True;
      if Exercise = 1 then
        LTriangles[1, I].Caption := 'Side ' + TriangleSides[1, I]
      else
        LTriangles[1, I].Caption := 'Angle ' + TriangleAngles[0, I];
    end;
  end;
  // Show or hide other input fields as needed
  for I := 3 to 5 do begin
    LTriangles[0, I].Visible := False; ETriangles[0, I].Visible := False;
    if Exercise = 2 then begin
      LTriangles[0, I].Visible := True; LTriangles[0, I].Caption := 'cos(α)';
      ETriangles[0, I].Visible := True; ETriangles[0, I].Text := '';
    end
    else if (Exercise = 3) and (I = 3) then begin
      LTriangles[0, I].Visible := True; LTriangles[0, I].Caption := 'Length AD';
      ETriangles[0, I].Visible := True; ETriangles[0, I].Text := '';
    end;
  end;
  // Reset all input fields to "value not displayed by program" (= to be entered by user)
  for I := 0 to 1 do begin
    for J := 0 to 5 do begin
      DTriangles[I, J] := False;
    end;
  end;
end;

{ Generate random right triangle }

procedure GetRightTriangle(Exercise, ExType, T: Integer; out Triangles: TTriangles; out DTriangles: TDisplayTriangles);

begin
  // Get random size for AC and BC
  repeat
    if Exercise = 1 then begin
      // For "similar triangles" exercises there are two right triangles; the routine is called twice
      // and the second one is not entirely random, because it has to be similar to the first one
      repeat
        Triangles[T, 0] := Random(6) + 10;
        if T = 0 then
          // First triangle
          Triangles[T, 1] := Random(16) + 5
        else
          // Second triangle
          Triangles[T, 1] := Triangles[0, 1] * (Triangles[1, 0] / Triangles[0, 0]);
      until Triangles[T, 1] <= 20;
    end
    else begin
      // All exercise type, except "similar triangles"
      Triangles[T, 0] := Random(21) + 20;
      Triangles[T, 1] := Random(16) + 5;
    end;
  until Abs(Triangles[T, 0] - Triangles[T, 1]) >= 2;
  // Calculate size of AB (hypothenuse)
  Triangles[T, 2] := Sqrt(Sqr(Triangles[T, 0]) + Sqr(Triangles[T, 1]));
  if Exercise = 0 then begin
    // Pythagore: Let the program randomly display 2 of the sides (user has to find other one)
    // Adapt side length, in order to always display integers
    case ExType of
      0: begin
           DTriangles[T, 0] := True; DTriangles[T, 1] := True;
         end;
      1: begin
           Triangles[T, 2] := Round(Triangles[T, 2]);
           Triangles[T, 1] := Sqrt(Sqr(Triangles[T, 2]) - Sqr(Triangles[T, 0]));
           DTriangles[T, 0] := True; DTriangles[T, 2] := True;
         end;
      2: begin
           Triangles[T, 2] := Round(Triangles[T, 2]);
           Triangles[T, 0] := Sqrt(Sqr(Triangles[T, 2]) - Sqr(Triangles[T, 1]));
           DTriangles[T, 1] := True; DTriangles[T, 2] := True;
         end;
    end;
  end
  else if Exercise = 1 then begin
    // Similar right triangles: Display all sides of first triangle and one random side of the second
    DTriangles[0, 0] := True; DTriangles[0, 1] := True; DTriangles[0, 2] := True;
    case ExType of
      0: DTriangles[1, 0] := True;
      1: DTriangles[1, 1] := True;
      2: DTriangles[1, 2] := True;
    end;
  end
  else if Exercise = 2 then begin
    // Trigonometric ratios: Display all sides of the triangle
    DTriangles[0, 0] := True; DTriangles[0, 1] := True; DTriangles[0, 2] := True;
  end;
end;

{ Generate random oblique triangle }

procedure GetObliqueTriangle(Exercise, TriangleType, ExType: Integer; out Triangles: TTriangles; out DTriangles: TDisplayTriangles);

var
  AD, DC, AC, BD, AB, Alpha, Gamma: Real;

begin
  // Triangle with angle at C < 90 (AD < AC)
  if TriangleType = 0 then begin
    // 2 random sides (of the right triangle)
    repeat
      AD := Random(16) + 10;
      BD := Random(16) + 5;
    until Abs(AD - BD) >= 2;
    // Hypothenuse and angle at A (of the right triangle)
    AB := Sqrt(Sqr(AD) + Sqr(BD));
    Alpha := RadToDeg(ArcTan(BD / AD));
    // Adapt values in order to always display AB and angle as integers
    AB := Round(AB);
    Alpha := Round(Alpha);
    BD := AB * Sin(DegToRad(Alpha)); AD := AB * Cos(DegToRad(Alpha));
    // Random angle at C (< 90)
    repeat
      Gamma := Random(71) + 10;
      DC := BD / Tan(DegToRad(Gamma));
    until (DC >= 5) and (DC <= 15);
    // D situated on segment [AC]
    AC := AD + DC;
  end
  // Triangle with angle at C > 90 (AC < AD)
  else begin
    // 2 random sides (of the right triangle)
    repeat
      AD := Random(21) + 20;
      BD := Random(11) + 10;
    until Abs(AD - BD) >= 2;
    // Hypothenuse and angle at A (of the right triangle)
    AB := Sqrt(Sqr(AD) + Sqr(BD));
    Alpha := RadToDeg(ArcTan(BD / AD));
    // Adapt values in order to always display AB and angle as integers
    AB := Round(AB);
    Alpha := Round(Alpha);
    BD := AB * Sin(DegToRad(Alpha)); AD := AB * Cos(DegToRad(Alpha));
    // Random angle at C (> 90)
    repeat
      Gamma := Random(41) + 120;
      DC := BD / Tan(Pi - DegToRad(Gamma));
    until (DC >= 5) and (DC <= 15);
    // D situated outside of segment [AC]
    AC := AD - DC;
  end;
  // Fill in triangle values
  Triangles[0, 0] := AC;
  Triangles[0, 1] := Sqrt(Sqr(DC) + Sqr(BD));
  Triangles[0, 2] := AB;
  Triangles[0, 3] := AD;
  Triangles[1, 0] := Alpha;
  Triangles[1, 1] := 180 - Alpha - Gamma;
  Triangles[1, 2] := Gamma;
  // Set edit fields, that will be filled in by programs (the others have to be filled in by user)
  if Exercise = 3 then begin
    // For "oblique triangles", show AB and randomly 2 of the angles
    DTriangles[0, 2] := True;
    if ExType = 0 then begin
      DTriangles[1, 0] := True; DTriangles[1, 1] := True
    end
    else if ExType = 1 then begin
      DTriangles[1, 1] := True; DTriangles[1, 2] := True
    end
    else begin
      DTriangles[1, 0] := True; DTriangles[1, 2] := True;
    end;
  end
  else begin
    // For "law of sines", randomly show 2 sides and one of the 2 "appropriate" angles OR 1 side and any 2 angles
    case ExType of
      0, 15: begin
           DTriangles[0, 0] := True; DTriangles[0, 1] := True; DTriangles[1, 0] := True;
         end;
      1, 16: begin
           DTriangles[0, 0] := True; DTriangles[0, 1] := True; DTriangles[1, 1] := True;
         end;
      2, 17: begin
           DTriangles[0, 0] := True; DTriangles[0, 2] := True; DTriangles[1, 1] := True;
         end;
      3, 18: begin
           DTriangles[0, 0] := True; DTriangles[0, 2] := True; DTriangles[1, 2] := True;
         end;
      4, 19: begin
           DTriangles[0, 1] := True; DTriangles[0, 2] := True; DTriangles[1, 0] := True;
         end;
      5, 20: begin
           DTriangles[0, 1] := True; DTriangles[0, 2] := True; DTriangles[1, 2] := True;
         end;
      6: begin
           DTriangles[0, 0] := True; DTriangles[1, 1] := True; DTriangles[1, 0] := True;
         end;
      7: begin
           DTriangles[0, 0] := True; DTriangles[1, 1] := True; DTriangles[1, 2] := True;
         end;
      8: begin
           DTriangles[0, 0] := True; DTriangles[1, 0] := True; DTriangles[1, 2] := True;
         end;
      9: begin
           DTriangles[0, 1] := True; DTriangles[1, 0] := True; DTriangles[1, 1] := True;
         end;
     10: begin
           DTriangles[0, 1] := True; DTriangles[1, 0] := True; DTriangles[1, 2] := True;
         end;
     11: begin
           DTriangles[0, 1] := True; DTriangles[1, 1] := True; DTriangles[1, 2] := True;
         end;
     12: begin
           DTriangles[0, 2] := True; DTriangles[1, 2] := True; DTriangles[1, 0] := True;
         end;
     13: begin
           DTriangles[0, 2] := True; DTriangles[1, 2] := True; DTriangles[1, 1] := True;
         end;
     14: begin
           DTriangles[0, 2] := True; DTriangles[1, 0] := True; DTriangles[1, 1] := True;
         end;
    end;
  end;
end;

{ Fill the form with the values given for actual exercise (other values have to be calculated by user) }

procedure FillExerciseForm(var Triangles: TTriangles; var DTriangles: TDisplayTriangles; var ETriangles: TEditFields);

var
  I, J: Integer;

begin
   for I := 0 to 1 do begin
     for J := 0 to 5 do begin
       if DTriangles[I, J] then begin
         if ETriangles[I, J] <> nil then begin
           ETriangles[I, J].Text := RFormat(Triangles[I, J]);
           ETriangles[I, J].ReadOnly := True; ETriangles[I, J].TabStop := False;
         end;
       end;
     end;
   end;
end;

{ Draw (right or oblique) triangle }

procedure DrawTriangle(Exercise, TriangleType, T: Integer; var Triangles: TTriangles; Clear: Boolean);

const
  Mult = 20;
  TrianglePoints: array[0..1, 0..2] of string = (
    ('A', 'B', 'C'),
    ('A''', 'B''', 'C''')
  );

var
  X0, Y0, X1, Y1, X2, Y2, X3, Y3: Integer;

begin
  if Clear then
    // Clear the drawing surface before drawing the triangle
    DrawingSurfaceClear(fTrigo.imExercise);
  fTrigo.imExercise.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fTrigo.imExercise.Picture.Bitmap.Canvas.Pen.Width := 3;
  // X- and Y-coordinates of drawing start (point A)
  X0 := fTrigo.imExercise.Left + 25;
  Y0 := fTrigo.imExercise.Height - 50;
  if (Exercise = 1) and (T = 1) then
    // For "similar triangles", the second triangle is drawn to the right of the first one
    X0 += 425;
  // Point C coordinates
  X1 := X0 + Round(Triangles[T, 0] * Mult); Y1 := Y0;
  // Point B coordinates
  if Exercise <= 2 then begin
    // Right triangle
    X2 := X1; Y2 := Round(Y0 - Triangles[T, 1] * Mult);
  end
  else begin
    // Oblique triangle
    X2 := X0 + Round(Triangles[T, 3] * Mult);
    Y2 := Round(Y0 - Sqrt(Sqr(Triangles[T, 2]) - Sqr(Triangles[T, 3])) * Mult);
  end;
  // Draw the triangle
  fTrigo.imExercise.Picture.Bitmap.Canvas.Line(X0, Y0, X1, Y1);
  fTrigo.imExercise.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
  fTrigo.imExercise.Picture.Bitmap.Canvas.Line(X0, Y0, X2, Y2);
  // For "oblique triangles" exercise, also draw point D and (eventually line CD)
  if Exercise = 3 then begin
    X3 := X2; Y3 := Y0;
    fTrigo.imExercise.Picture.Bitmap.Canvas.Pen.Color := clBlue;
    fTrigo.imExercise.Picture.Bitmap.Canvas.Line(X2, Y2, X3, Y3);
    if TriangleType = 1 then
      // Triangle with angle at C > 90 (AD > AC; CD has to be drawn)
      fTrigo.imExercise.Picture.Bitmap.Canvas.Line(X1, Y1, X3, Y3);
  end;
  // Write the 3 (4) points labels (different positions for right and oblique triangles)
  fTrigo.imExercise.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fTrigo.imExercise.Picture.Bitmap.Canvas.Pen.Width := 1;
  fTrigo.imExercise.Picture.Bitmap.Canvas.Font.Size := 15;
  fTrigo.imExercise.Picture.Bitmap.Canvas.Font.Style := [fsBold];
  if Exercise <= 2 then begin
    // Right triangle
    fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X0 - 20, Y0 - 17, TrianglePoints[T, 0]);
    fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X2 + 10, Y2 - 17, TrianglePoints[T, 1]);
    fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X1 + 10, Y1 - 17, TrianglePoints[T, 2]);
  end
  else begin
    // Oblique triangle
    fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X0 - 5, Y0 + 17, TrianglePoints[T, 0]);
    fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X2 - 5, Y2 - 34, TrianglePoints[T, 1]);
    fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X1 - 5, Y1 + 17, TrianglePoints[T, 2]);
    if Exercise = 3 then
      // Oblique triangle with angle at C > 90 (point D having to be displayed)
      fTrigo.imExercise.Picture.Bitmap.Canvas.TextOut(X3 - 5, Y3 + 17, 'D');
  end;
end;

{ Exercise type 1: Pythagorean theorem }

procedure ExPythagorean(out Triangles: TTriangles; var LTriangles: TLabels; var ETriangles: TEditFields);

var
  ExType: Integer;
  DTriangles: TDisplayTriangles;

begin
  ResetExercise(0, LTriangles, ETriangles, DTriangles);
  ExType := Random(3);
  GetRightTriangle(0, ExType, 0, Triangles, DTriangles);
  FillExerciseForm(Triangles, DTriangles, ETriangles);
  DrawTriangle(0, 0, 0, Triangles, True);
end;

{ Exercise type 2: Similar right triangles }

procedure ExSimilarRight(out Triangles: TTriangles; var LTriangles: TLabels; var ETriangles: TEditFields);

var
  ExType: Integer;
  DTriangles: TDisplayTriangles;

begin
  ResetExercise(1, LTriangles, ETriangles, DTriangles);
  ExType := Random(3);
  repeat
    GetRightTriangle(1, ExType, 0, Triangles, DTriangles);
    GetRightTriangle(1, ExType, 1, Triangles, DTriangles);
  until Abs(Triangles[0, 0] - Triangles[1, 0]) >= 2;
  FillExerciseForm(Triangles, DTriangles, ETriangles);
  DrawTriangle(1, 0, 0, Triangles, True);
  DrawTriangle(1, 0, 1, Triangles, False);
end;

{ Exercise type 3: Trigonometric ratios }

procedure ExTrigRatios(out Triangles: TTriangles; var LTriangles: TLabels; var ETriangles: TEditFields);

var
  FX, I, J: Integer;
  OK: Boolean;
  DTriangles: TDisplayTriangles;

begin
  ResetExercise(2, LTriangles, ETriangles, DTriangles);
  GetRightTriangle(2, 0, 0, Triangles, DTriangles);
  // Randomly choose 3 trigonometric functions
  for I := 3 to 5 do begin
    repeat
      OK := True;
      FX := Random(6);
      if I > 0 then begin
        // Each function only once
        for J := 0 to I - 1 do begin
          if TrigoFunctions[FX] = LeftStr(LTriangles[0, J].Caption, 3) then
            OK := False;
        end;
      end;
    until OK;
    LTriangles[0, I].Caption := StringReplace(LTriangles[0, I].Caption, 'cos', TrigoFunctions[FX], []);
    // Calculate the functions value (as trigonometric ratio)
    case FX of
       0: Triangles[0, I] := Triangles[0, 0] / Triangles[0, 2];
       1: Triangles[0, I] := Triangles[0, 1] / Triangles[0, 2];
       2: Triangles[0, I] := Triangles[0, 1] / Triangles[0, 0];
       3: Triangles[0, I] := Triangles[0, 0] / Triangles[0, 1];
       4: Triangles[0, I] := Triangles[0, 2] / Triangles[0, 0];
       5: Triangles[0, I] := Triangles[0, 2] / Triangles[0, 1];
    end;
  end;
  FillExerciseForm(Triangles, DTriangles, ETriangles);
  DrawTriangle(2, 0, 0, Triangles, True);
end;

{ Exercise type 4: Oblique triangles }

procedure ExOblique(out Triangles: TTriangles; var LTriangles: TLabels; var ETriangles: TEditFields);

var
  TriangleType, ExType: Integer;
  DTriangles: TDisplayTriangles;

begin
  ResetExercise(3, LTriangles, ETriangles, DTriangles);
  ExType := Random(5);
  TriangleType := Random(2);
  GetObliqueTriangle(3, TriangleType, ExType, Triangles, DTriangles);
  FillExerciseForm(Triangles, DTriangles, ETriangles);
  DrawTriangle(3, TriangleType, 0, Triangles, True);
end;

{ Exercise type 5: Law of sines }

procedure ExLawSines(out Triangles: TTriangles; var LTriangles: TLabels; var ETriangles: TEditFields);

var
  TriangleType, ExType: Integer;
  DTriangles: TDisplayTriangles;

begin
  ResetExercise(4, LTriangles, ETriangles, DTriangles);
  TriangleType := Random(2); ExType := Random(21);
  GetObliqueTriangle(4, TriangleType, ExType, Triangles, DTriangles);
  FillExerciseForm(Triangles, DTriangles, ETriangles);
  DrawTriangle(4, TriangleType, 0, Triangles, True);
end;

{*********}
{ TfTrigo }
{*********}

procedure TfTrigo.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create bitmap for canvas drawing
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imExercise.Width;
  bmDraw.Height := imExercise.Height;
  imExercise.Picture.Graphic := bmDraw;
  // Create array with labels and edit fields
  laTriangles[0, 0] := laT1ac; laTriangles[0, 1] := laT1bc; laTriangles[0, 2] := laT1ab;
  laTriangles[0, 3] := laFunc1;  laTriangles[0, 4] := laFunc2;  laTriangles[0, 5] := laFunc3;
  laTriangles[1, 0] := laT2ac; laTriangles[1, 1] := laT2bc; laTriangles[1, 2] := laT2ab;
  laTriangles[1, 3] := nil;    laTriangles[1, 4] := nil;    laTriangles[1, 5] := nil;
  edTriangles[0, 0] := edT1ac; edTriangles[0, 1] := edT1bc; edTriangles[0, 2] := edT1ab;
  edTriangles[0, 3] := edFunc1;  edTriangles[0, 4] := edFunc2;  edTriangles[0, 5] := edFunc3;
  edTriangles[1, 0] := edT2ac; edTriangles[1, 1] := edT2bc; edTriangles[1, 2] := edT2ab;
  edTriangles[1, 3] := nil;    edTriangles[1, 4] := nil;    edTriangles[1, 5] := nil;
  // Default test exercises
  aTestExercises[0] := True;
  for I := 1 to 4 do
    aTestExercises[I] := False;
  // Start random number generator
  Randomize;
  // Start a new test
  mTestNew.Click;
end;

{ Menu item "Test > New": Start a new test }

procedure TfTrigo.mTestNewClick(Sender: TObject);

var
  I, J: Integer;

begin
  laQuestion.Caption := 'Exercise.';
  DrawingSurfaceClear(imExercise);
  // Clear edit fields
  for I := 0 to 1 do begin
    for J := 0 to 5 do begin
      if edTriangles[I, J] <> nil then
        edTriangles[I, J].Text := '';
    end;
  end;
  // Cleat the drawing surface
  imEval.Picture.Clear;
  // Clear the evaluation grid
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  // Reset evalauation counters
  iQuestion := 0; iCorrect := 0;
  // Set button capture
  btTest.Caption := 'Start';
end;

{ Menu item "Test > Exit": Exit application }

procedure TfTrigo.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Exercise > ...": Select/unselect this type of exercise to be part of test questions }

procedure TfTrigo.mExercises1Click(Sender: TObject);

begin
  if mExercises1.Checked then
    mExercises1.Checked := False
  else
    mExercises1.Checked := True;
  aTestExercises[0] := mExercises1.Checked;
end;

procedure TfTrigo.mExercises2Click(Sender: TObject);

begin
  if mExercises2.Checked then
    mExercises2.Checked := False
  else
    mExercises2.Checked := True;
  aTestExercises[1] := mExercises2.Checked;
end;

procedure TfTrigo.mExercises3Click(Sender: TObject);

begin
  if mExercises3.Checked then
    mExercises3.Checked := False
  else
    mExercises3.Checked := True;
  aTestExercises[2] := mExercises3.Checked;
end;

procedure TfTrigo.mExercises4Click(Sender: TObject);

begin
  if mExercises4.Checked then
    mExercises4.Checked := False
  else
    mExercises4.Checked := True;
  aTestExercises[3] := mExercises4.Checked;
end;

procedure TfTrigo.mExercises5Click(Sender: TObject);

begin
  if mExercises5.Checked then
    mExercises5.Checked := False
  else
    mExercises5.Checked := True;
  aTestExercises[4] := mExercises5.Checked;
end;

{ Menu item "Help > About": Display application about }

procedure TfTrigo.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Numerical trigonometry: Triangles.' + LineEnding;
  S += 'Freeware mathematics exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June-July 2020.';
  MessageDlg('About "Trigonometry1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next/Check": Generate question resp. check user answer }

procedure TfTrigo.btTestClick(Sender: TObject);

const
  Exercises: array[0..4] of string = (
    'Pythagorean theorem', 'Similar right triangles', 'Trigonometric ratios', 'Oblique triangles', 'Law of sines'
  );

var
  I, J: Integer;
  Answer: Real;
  OK: Boolean;

begin
  // Button "Start/Next": Generate new question
  if (btTest.Caption = 'Start') or (btTest.Caption = 'Next') then begin
    OK := False;
    // Check if at least one exercise type is selected
    for I := 0 to 4 do begin
      if aTestExercises[I] then
        OK := True;
    end;
    // Proceed if all ok
    if OK then begin
      Inc(iQuestion);
      // Random exercise type (among those actually selected)
      repeat
        iExercise := Random(5);
      until aTestExercises[iExercise];
      laQuestion.Caption := 'Exercice ' + IntToStr(iQuestion) + ': ' + Exercises[iExercise] + '.';
      // Generate the exercise (depending on actual exercise type)
      case iExercise of
        0: ExPythagorean(aTriangles, laTriangles, edTriangles);
        1: ExSimilarRight(aTriangles, laTriangles, edTriangles);
        2: ExTrigRatios(aTriangles, laTriangles, edTriangles);
        3: ExOblique(aTriangles, laTriangles, edTriangles);
        4: ExLawSines(aTriangles, laTriangles, edTriangles);
      end;
      // Set cursor into first user entry field
      for I := 1 downto 0 do begin
        for J := 5 downto 0 do begin
          if (edTriangles[I, J] <> nil) and edTriangles[I, J].Visible and not edTriangles[I, J].ReadOnly then
            edTriangles[I, J].SetFocus;
        end;
      end;
      // Clear the evaluation picture
      imEval.Picture.Clear;
      // Next button push will be to check user answer
      btTest.Caption := 'Check';
    end
    else
      // No exercise type selected: Error message
      MessageDlg('Trigonometry test', 'You must select at least one exercise type!', mtError, [mbOK], 0);
  end
  // Button "Check": Check user answer
  else begin
    OK := True;
    // Check all user entry fields: If one of them contains a value different from the calculated one, the global answer is false
    for I := 0 to 1 do begin
      for J := 0 to 5 do begin
        if (edTriangles[I, J] <> nil) and edTriangles[I, J].Visible then begin
          if edTriangles[I, J].Text = '' then
            Answer := 0
          else
            Answer := StrToFloat(edTriangles[I, J].Text);
          if Round(100 * Answer) <> Round(100 * StrToFloat(RFormat(aTriangles[I, J]))) then
            OK := False;
        end;
      end;
    end;
    // Display evaluation picture and update evaluation counters
    if OK then begin
      // Correct answer
      imEval.Picture.LoadFromFile('correct.png');
      Inc(iCorrect);
    end
    else begin
      // Wrong answer
      imEval.Picture.LoadFromFile('false.png');
    end;
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * iCorrect / iQuestion), '%');
    // Next button push will be to generate a new question
    btTest.Caption := 'Next';
  end;
end;

end.

