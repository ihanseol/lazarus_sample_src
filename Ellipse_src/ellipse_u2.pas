{*************************************}
{* Graph unit of Ellipse application *}
{*************************************}

unit ellipse_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  {*********}
  { TfGraph }
  {*********}
  TfGraph = class(TForm)
    stTitle: TStaticText;
    imDraw: TImage;
    Label1, Label10, Label11, Label12, Label13: TLabel;
    laFormula, laEquation, laEquation2, laIntersect1, laIntersect2: TLabel;
    edEllipseA, edEllipseB, edCalc: TEdit;
    edFormula, edEquation, edEquation2, edIntersect1, edIntersect2: TEdit;
    tbScale: TTrackBar;
    edScale: TEdit;
    btRedraw: TButton;
    btClose: TButton;
    procedure btRedrawClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure tbScaleChange(Sender: TObject);
  private
    rScale: Real;
    bmDraw: TBitmap;
  public
    rA, rB, rPX, rPY, rPA, rM: Real;
    sLine, sForm: string;
  end;

const
  // Sub- and superscripts
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  SUP_2 = #$C2#$B2;

var
  fGraph: TfGraph;

// Public function (also used by main unit)
function UCFirst(S: string): string;

implementation

{$R *.lfm}

const
  // Graph region: 40x20 fields, 25px each
  FieldsX = 40; FieldsY = 20; FieldSize = 25;

{ First character uppercase }

function UCFirst(S: string): string;

// All ANSI characters supposed

begin
  S[1] := UpperCase(S[1])[1];
  Result := S;
end;

{ Degrees to radiants conversion }

function DegToRad(Angle: Real): Real;

begin
  Result := 2 * Pi * Angle / 360;
end;

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

{ Real number formatting }

function RFormat(R: Real; F: Integer): string;

// If possible, the number is rounded to F decimal digits (with unsignificant zeros being dropped)
// If not possible (because number is to small), display will be in exponential form

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

{ Get formula of tangent/normal with sub- and superscripts applied }

function GetFormula(Equation: string): string;

begin
  Equation := StringReplace(Equation, 'a2', 'a' + SUP_2, [rfReplaceAll]);
  Equation := StringReplace(Equation, 'b2', 'b' + SUP_2, [rfReplaceAll]);
  Equation := StringReplace(Equation, 'm2', 'm' + SUP_2, [rfReplaceAll]);
  Equation := StringReplace(Equation, 'x1', 'x' + SUB_1, [rfReplaceAll]);
  Equation := StringReplace(Equation, 'y1', 'y' + SUB_1, [rfReplaceAll]);
  Equation := StringReplace(Equation, 'x2', 'x' + SUB_2, [rfReplaceAll]);
  Equation := StringReplace(Equation, 'y2', 'y' + SUB_2, [rfReplaceAll]);
  Result := Equation;
end;

{ Get equation of ellipse with a and b replaced by actual values and superscripts applied }

function EllipseEquation(A, B: Real): string;

var
  Equation: string;

begin
  Equation := 'x2/a2 + y2/b2 = 1';
  Equation := StringReplace(Equation, 'x2', 'x' + SUP_2, []);
  Equation := StringReplace(Equation, 'y2', 'y' + SUP_2, []);
  Equation := StringReplace(Equation, 'a2', RFormat(Sqr(A), 3), []);
  Equation := StringReplace(Equation, 'b2', RFormat(Sqr(B), 3), []);
  Result := Equation;
end;

{ Get equation of line with m and c replaced by actual values }

function LineEquation(M, C: Real; XLine: Boolean; F: Integer): string;

var
  Equ: string;

begin
  if XLine then begin
    // Line parallel to Oy
    Equ := 'x = ' + RFormat(C, F);
  end
  else begin
    // Other lines
    if M = 0 then begin
      Equ := 'y = ' + RFormat(C, F);
    end
    else begin
      Equ := 'y = ' + RFormat(M, F) + 'x';
      if C > 0 then
        Equ += ' + ' + RFormat(C, F)
      else if C < 0 then
        Equ += ' - ' + RFormat(-C, F);
    end;
  end;
  Result := Equ;
end;

{ Reset drawing surface (clear all, then draw gridlines and axes) }

procedure DrawingSurfaceReset(Surface: TImage; W, H, L: Integer);

var
  I: Integer;

begin
  // Drawing surface as white rectancle
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
  // Horizontal gridline and x-axis
  for I := 1 to H - 1 do begin
    if I = H div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(1, I * L, W * L, I * L);
  end;
  // Vertical gridline and y-axis
  for I := 1 to W - 1 do begin
    if I = W div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(I * L, 1, I * L, H * L);
  end;
end;

{ Draw the ellipse }

procedure EllipseDraw(Surface: TImage; W, H, L: Integer; A, B, Scale: Real);

var
  CX1, CX2, CY1, CY2, CX1Old, CX2Old, CY1Old, CY2Old: Integer;
  X, Y1, Y2: Real;

begin
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlue; Surface.Picture.Bitmap.Canvas.Pen.Width := 3;
  CX1Old := 0; CX2Old := 0; CY1Old := 0; CY2Old := 0;
  X := -A;
  repeat
    Y1 := Sqrt(Sqr(B) * (1 - (Sqr(X) / Sqr(A)))); Y2 := -Sqrt(Sqr(B) * (1 - (Sqr(X) / Sqr(A))));
    CX1 := (W div 2) * L + Round(X * L / Scale);   CX2 := (W div 2) * L - Round(X * L / Scale);
    CY1 := (H div 2) * L - Round(Y1 * L / Scale);  CY2 := (H div 2) * L - Round(Y2 * L / Scale);
    if X > -A then begin
      Surface.Picture.Bitmap.Canvas.Line(CX1Old, CY1Old, CX1, CY1);
      Surface.Picture.Bitmap.Canvas.Line(CX1Old, CY2Old, CX1, CY2);
      Surface.Picture.Bitmap.Canvas.Line(CX2Old, CY1Old, CX2, CY1);
      Surface.Picture.Bitmap.Canvas.Line(CX2Old, CY2Old, CX2, CY2);
    end;
    CX1Old := CX1; CX2Old := CX2; CY1Old := CY1; CY2Old := CY2;
    X += 0.25 * Scale;
  until X > A;
end;

{ Draw the tangent or normal line }

procedure LineDraw(Surface: TImage; W, H, L: Integer; Scale, A, B, PX, PY, M, C: Real; XLine, YLine: Boolean; Colour: TColor);

var
  GX, GY, GX1, GX2, GY1, GY2: Integer;
  X, Y: Real;

begin
  // Draw tangent point
  GX := (W div 2) * L + Round(PX * L / Scale); GY1 := (H div 2) * L - Round(PY * L / Scale);
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clBlue;
  Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY1, 4, 4);
  // Draw the tangent/normal line
  Surface.Picture.Bitmap.Canvas.Pen.Color := Colour;
  Surface.Picture.Bitmap.Canvas.Brush.Color := Colour;
  if XLine then begin
    // Special case: Line // Oy
    GX := (W div 2) * L + Round(PX * L / Scale);
    GY1 := (H div 2) * L - Round(PY * L / Scale) - Round((-1.5 * B) * L / Scale);
    GY2 := (H div 2) * L - Round(PY * L / Scale) - Round((1.5 * B) * L / Scale);
    Surface.Picture.Bitmap.Canvas.Line(GX, GY1, GX, GY2);
  end
  else if YLine then begin
    // Special case: Line // Ox
    GX1 := (W div 2) * L + Round(PX * L / Scale) + Round((-1.5 * A) * L / Scale);
    GX2 := (W div 2) * L + Round(PX * L / Scale) + Round((1.5 * A) * L / Scale);
    GY := (H div 2) * L - Round(PY * L / Scale);
    Surface.Picture.Bitmap.Canvas.Line(GX1, GY, GX2, GY);
  end
  else begin
    // "Normal" case: Line with equation y = mx + c
    // Draw the line, trying to limit its length, just displayed
    // horizontaly and verticaly "nearby" the tangent point
    X := PX - 1.5 * A;
    repeat
      Y := M * X + C;
      GX := (W div 2) * L + Round(X * L / Scale); GY := (H div 2) * L - Round(Y * L / Scale);
      if X = PX - 1.5 * A then
        Surface.Picture.Bitmap.Canvas.MoveTo(GX, GY)
      else if Abs(Y - PY) <= 1.5 * B then
        Surface.Picture.Bitmap.Canvas.LineTo(GX, GY);
      X += A / 100;
    until X >= PX + 1.5 * A;
  end;
end;

{*********}
{ TfGraph }
{*********}

{ Application start: Create bitmap for canvas drawing }

procedure TfGraph.FormCreate(Sender: TObject);

begin
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
end;

{ Graph window, becoming active: Do calculations and draw the ellipse and the tangent/normal }

procedure TfGraph.FormActivate(Sender: TObject);

begin
  stTitle.Caption := UCFirst(sLine) + ' to the ellipse: ' + EllipseEquation(rA, rB);
  tbScale.Position := 12; edScale.Text := '1'; rScale := StrToFloat(edScale.Text);
  edEllipseA.Text := RFormat(rA, 3); edEllipseB.Text := RFormat(rB, 3);
  btRedraw.Click;                                                              // do the drawings
end;

{ Button "Redraw": Redo the drawings (this button has to be pushed, after changing the drawing zoom)}

procedure TfGraph.btRedrawClick(Sender: TObject);

var
  N, I: Integer;
  M, C: Real;
  XLine, YLine: Boolean;
  Colour: TColor;

begin
  // Draw the ellipse
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
  EllipseDraw(imDraw, FieldsX, FieldsY, FieldSize, rA, rB, rScale);
  // Display formula for actual calculation and adapt label captions
  if sForm = 'slope' then begin
    edCalc.Text := UCFirst(sLine) + 's with a slope of m = ' + RFormat(rM, 3);
    laFormula.Caption := UCFirst(sLine) + 's formula';
    laEquation.Caption := UCFirst(sLine) + ' 1 equation';
    laEquation2.Visible := True; edEquation2.Visible := True;
    laIntersect1.Visible := True; edIntersect1.Visible := True;
    laIntersect2.Visible := True; edIntersect2.Visible := True;
    laEquation2.Caption := UCFirst(sLine) + ' 2 equation';
    if sLine = 'tangent' then begin
      edFormula.Text := GetFormula('y = m·x ± √(a2·m2+b2)');
      laIntersect1.Caption := 'Intersection point 1';
      laIntersect2.Caption := 'Intersection point 2';
    end
    else begin
      edFormula.Text := GetFormula('y = m·x ± m·(a2-b2)/√(a2·m2+b2)');
      laIntersect1.Caption := 'Tangent point 1';
      laIntersect2.Caption := 'Tangent point 2';
    end;
  end
  else begin
    edCalc.Text := UCFirst(sLine) + ' at the point P(';
    if sForm = 'point' then
      edCalc.Text := edCalc.Text + RFormat(rPX, 3) + ' ; ' + RFormat(rPY, 3) + ')'
    else
      edCalc.Text := edCalc.Text + RFormat(rA, 3) + '·cos' + RFormat(rPA, 3) + '° ; ' + RFormat(rB, 3) + '·sin' + RFormat(rPA, 3) + '°)';
    laFormula.Caption := UCFirst(sLine) + ' formula';
    laEquation.Caption := UCFirst(sLine) + ' equation';
    laEquation2.Visible := False; edEquation2.Visible := False;
    laIntersect1.Visible := False; edIntersect1.Visible := False;
    laIntersect2.Visible := False; edIntersect2.Visible := False;
    if sLine = 'tangent' then begin
      if sForm = 'point' then
        edFormula.Text := GetFormula('(x·x1/a2) + (y·y1/b2) = 1')
      else
        edFormula.Text := 'b·x·cosθ + a·y·sinθ – a·b = 0';
    end
    else begin
      if sForm = 'point' then
        edFormula.Text := GetFormula('a2·y1·(x-x1) = b2·x1·(y-y1)')
      else
        edFormula.Text := GetFormula('a·x·secθ – b·y·cscθ = a2–b2');
    end;
  end;
  // Determine and draw the tangent (and, if selected, the normal)
  if sForm = 'slope' then
    N := 2                                                                     // slope form: 2 lines to draw
  else
    N := 1;                                                                    // other forms: 1 line to draw
  for I := 1 to N do begin
    // If the tangent/normal is given in slope form, determine the tangents points and then do
    // the calculations for the tangent/normal equation just the same way as for the other cases
    if sForm = 'slope' then begin
      if sLine = 'tangent' then begin
        if I = 1 then begin
          // First tangent
          rPX := -Sqr(rA) * rM / Sqrt(Sqr(rA) * Sqr(rM) + Sqr(rB));
          rPY := Sqr(rB) / Sqrt(Sqr(rA) * Sqr(rM) + Sqr(rB));
          edIntersect1.Text := 'P' + SUB_1 + '(' + RFormat(rPX, 3) + ' ; ' + RFormat(rPY, 3) + ')';
        end
        else begin
          // Second tangent
          rPX := Sqr(rA) * rM / Sqrt(Sqr(rA) * Sqr(rM) + Sqr(rB));
          rPY := -Sqr(rB) / Sqrt(Sqr(rA) * Sqr(rM) + Sqr(rB));
          edIntersect2.Text := 'P' + SUB_2 + '(' + RFormat(rPX, 3) + ' ; ' + RFormat(rPY, 3) + ')';
        end;
      end
      else begin
        if I = 1 then begin
          // First normal
          rPX := Sqr(rA) / Sqrt(Sqr(rA) + Sqr(rB) * Sqr(rM));
          rPY := Sqr(rB) * rM / Sqrt(Sqr(rA) + Sqr(rB) * Sqr(rM));
          edIntersect1.Text := 'P' + SUB_1 + '(' + RFormat(rPX, 3) + ' ; ' + RFormat(rPY, 3) + ')';
        end
        else begin
          // Second normal
          rPX := -Sqr(rA) / Sqrt(Sqr(rA) + Sqr(rB) * Sqr(rM));
          rPY := -Sqr(rB) * rM / Sqrt(Sqr(rA) + Sqr(rB) * Sqr(rM));
          edIntersect2.Text := 'P' + SUB_2 + '(' + RFormat(rPX, 3) + ' ; ' + RFormat(rPY, 3) + ')';
        end;
      end;
    end;
    // Determine and draw the tangent
    M := 0; C := 0; XLine := False; YLine := False;
    if sForm = 'parametric' then begin
      // Calculate point coordinates from ellipse semi-axes and point angle
      if rPA = 90 then begin
        rPX := 0; rPY := rB;
      end
      else if rPA = 270 then begin
        rPX := 0; rPY := -rB;
      end
      else begin
        rPX := rA * Cos(DegToRad(rPA)); rPY := rB * Sin(DegToRad(rPA));
      end;
    end;
    if Abs(rPX) = rA then begin
      // Tangent // Oy
      XLine := True;
      C := rPX;
    end
    else if rPX = 0 then begin
      // Tangent // Ox
      YLine := True;
      C := rPY;
    end
    else begin
      // "Normal" case: Tangent is line with equation y = mx + c
      M := (-rPX / Sqr(rA)) * (Sqr(rB) / rPY); C := Sqr(rB) / rPY;
    end;
    if sLine = 'tangent' then begin
      // If drawing the tangent is selected, fill in the tangent(s) equation
      if I = 2 then
        edEquation2.Text := LineEquation(M, C, XLine, 3)
      else
        edEquation.Text := LineEquation(M, C, XLine, 3);
    end;
    Colour := clRed;
    if sLine = 'normal' then
      // Draw tangent in green, if drawing the normal is selected
      Colour := clGreen;
    LineDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rA, rB, rPX, rPY, M, C, XLine, YLine, Colour);
    // Determine and draw the normal (only if this is selected)
    if sLine = 'normal' then begin
      M := 0; C := 0; XLine := False; YLine := False;
      if Abs(rPX) = rA then begin
        // Normal // Ox
        YLine := True;
      end
      else if rPX = 0 then begin
        // Normal // Oy
        XLine := True;
      end
      else begin
        // "Normal case": Normal is line with equation y = mx + c
        M := (Sqr(rA) * rPY) / (Sqr(rB)* rPX); C := rPX * rPY * (Sqr(rB) - Sqr(rA)) / (Sqr(rB) * rPX);
      end;
      // Fill in the normal(s) equation
      if I = 2 then
        edEquation2.Text := LineEquation(M, C, XLine, 3)
      else
        edEquation.Text := LineEquation(M, C, XLine, 3);
      // Draw the normal
      Colour := clRed;
      LineDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rA, rB, rPX, rPY, M, C, XLine, YLine, Colour);
    end;
  end;
end;

{ Button "Close": Close the graph window }

procedure TfGraph.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Trackbar changes: Set new drawing zoom value }

procedure TfGraph.tbScaleChange(Sender: TObject);

const
  Scales: array[0..24] of string = (
    '1/50', '1/25', '1/20', '1/10', '1/9', '1/8', '1/7', '1/6', '1/5', '1/4', '1/3', '1/2',
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '20', '25', '50'
  );

begin
  edScale.Text := Scales[tbScale.Position];
  if tbScale.Position >= 12 then                                               // trackbar position 12 is 1:1 zoome
    // Values greater than 1 will give smaller drawings (zoom out)
    rScale := StrToFloat(Scales[tbScale.Position])
  else
    // Values less than 1 will give larger drawings (zoom in)
    rScale := 1 / StrToFloat(Copy(Scales[tbScale.Position], 3, Length(Scales[tbScale.Position])));
end;

end.

