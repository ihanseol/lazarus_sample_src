{*************************************}
{* Graph unit of Circles application *}
{*************************************}

unit circles_graph;

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
    Label1, Label6, Label7, Label8, Label9, Label10: TLabel;
    imDraw: TImage;
    laCircle, laCirclePoints, laPointA, laPointB: TLabel;
    laTangent, laTangentPoint, laTangentEqu: TLabel;
    laIntersection, laIntersectionI1, laIntersectionI2: TLabel;
    laSquareInner, laSquareInnerS1, laSquareInnerS2, laSquareInnerS3, laSquareInnerS4, laSquareInnerSide: TLabel;
    laSquareOuter, laSquareOuterS1, laSquareOuterS2, laSquareOuterS3, laSquareOuterS4, laSquareOuterSide: TLabel;
    edCenter, edRadius, edDiameter, edPerimeter, edArea: TEdit;
    edPointA, edPointB, edTangentPoint, edTangentEqu, edIntersectionI1, edIntersectionI2: TEdit;
    edIntersection: TMemo;
    edSquareInnerS1, edSquareInnerS2, edSquareInnerS3, edSquareInnerS4, edSquareInnerSide: TEdit;
    edSquareOuterS1, edSquareOuterS2, edSquareOuterS3, edSquareOuterS4, edSquareOuterSide: TEdit;
    tbScale: TTrackBar;
    edScale: TEdit;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure tbScaleChange(Sender: TObject);
  private
    rScale: Real;
    bmDraw: TBitmap;
  public
    iFormat: Integer;
    rCX, rCY, rR, rAX, rAY, rBX, rBY, rFX, rFY, rLM, rLC, rCCX, rCCY, rCR: Real;
    sCalculation, sEquation, sEquationInt: string;
    bCirclePoints, bLineX, bSqIn, bSqOut: Boolean;

  end;

const
  FieldsX = 46; FieldsY = 26; FieldSize = 20;
  SUB_1 = #$E2#$82#$81;  SUB_2 = #$E2#$82#$82;
  SUB_3 = #$E2#$82#$83;  SUB_4 = #$E2#$82#$84;

var
  fGraph: TfGraph;

// -------------------------Public functions--------------------------

function RFormat(R: Real; F: Integer): string;
function LineEquation(M, C: Real; XLine: Boolean; F: Integer): string;
function CircleEquation(CX, CY, R: Real; F: Integer): string;

// -------------------------------------------------------------------

implementation

{$R *.lfm}

{ Convert fraction string to float number }

function FractionToFloat(Fraction: string): Real;

var
  P: Integer;
  R: Real;

begin
  P := Pos('/', Fraction);
  if P = 0 then
    R := StrToFloat(Fraction)
  else
    R := StrToFloat(LeftStr(Fraction, P - 1)) / StrToFloat(RightStr(Fraction, Length(Fraction) - P));
  Result := R;
end;

{ Format real number, depending on how many significant decimal digits it has }

function RFormat(R: Real; F: Integer): string;

var
  I, P: Integer;
  SR1, SR2: string;

begin
  SR1 := FloatToStrF(R, ffFixed, 0, F); SR2 := SR1;                            // F = max. number of decimal digits to be considered
  P := Pos('.', SR1);
  if P = 0 then
    P := Pos(',', SR1);                                                        // testing on '.' and ',' makes routine independent of regional decimal separator
  if P <> 0 then begin
    I := Length(SR1) + 1;
    repeat
      Dec(I);
      if SR1[I] = '0' then
        Delete(SR2, Length(SR2), 1);                                           // remove unsignificant zeros
    until SR1[I] <> '0';
    if (RightStr(SR2, 1) = '.') or (RightStr(SR2, 1) = ',') then
      Delete(SR2, Length(SR2), 1);                                             // remove decimal separator if there are no decimal digits
  end;
  Result := SR2;
end;

{ Format a point with coordinates x and y to "(x ; y)" }

function PFormat(X, Y: Real; F: Integer): string;

begin
  Result := '(' + RFormat(X, F) + ' ; ' + RFormat(Y, F) + ')';                 // coordinates will have max. of 2 or 3 decimal digits
end;

{ Determine equation of a line: x=c or y=mx+c }

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

{ Determine equation of a circle: (x-a)² + (y-b)² = r² }

function CircleEquation(CX, CY, R: Real; F: Integer): string;

var
  Equ: string;

begin
  if CX = 0 then
    Equ := 'x²'
  else if CX > 0 then
    Equ := '(x - ' + RFormat(CX, F) + ')²'
  else
    Equ := '(x + ' + RFormat(-CX, F) + ')²';
  Equ += ' + ';
  if CY = 0 then
    Equ += 'y²'
  else if CY > 0 then
    Equ += '(y - ' + RFormat(CY, F) + ')²'
  else
    Equ += '(y + ' + RFormat(-CY, F) + ')²';
  Equ += ' = ' + RFormat(Sqr(R), F);
  Result := Equ;
end;

{ Solve quadratic equation in 1 variable (ax² + bx + c = 0) }

procedure EquQuadSolve(A, B, C: Real; out X1, X2: Real; out NSol: Integer);

var
  Delta: Real;

begin
  Delta := Sqr(B) - 4 * A * C;
  if Delta >= 0 then begin
    // Real solution(s) if delta is positive
    if Delta > 0 then
      NSol := 2
    else
      NSol := 1;
    X1 := (-B - Sqrt(Delta)) / (2 * A); X2 := (-B + Sqrt(Delta)) / (2 * A);
  end
  else begin
    // No real solutions, if delta is negative
    NSol := 0;
  end;
end;

{ Reset drawing surface (clear all, draw gridlines and axes) }

procedure DrawingSurfaceReset(Surface: TImage; W, H, L: Integer);

var
  I: Integer;

begin
  // Drawing surface as white rectancle
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
  // Horizontal gridline and x-axis
  for I := 1 to H - 1 do begin
    if I = H div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack                       // black for the x-axis gridline
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;                    // gray for the other horiz. gridlines
    Surface.Picture.Bitmap.Canvas.Line(1, I * L, W * L, I * L);
  end;
  // Vertical gridline and y-axis
  for I := 1 to W - 1 do begin
    if I = W div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack                       // black for the y-axis gridline
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;                    // gray for the other vert. gridlines
    Surface.Picture.Bitmap.Canvas.Line(I * L, 1, I * L, H * L);
  end;
end;

{ Draw a circle with given center and radius }

procedure CircleDraw(Surface: TImage; W, H, L: Integer; Scale, CX, CY, R: Real; CPoints: Boolean; AX, AY, BX, BY: Real; Colour: TColor; F: Integer);

// The procedure also draws the center point and 2 diameter points (if these are given) and also displays the circle parameters

var
  GX, GY, GR: Integer;

begin
  // Determine graph values for Cx, Cy and R
  GX := (W div 2) * L + Round(CX * L / Scale);
  GY := (H div 2) * L - Round(CY * L / Scale);
  GR := Round(R * L / Scale);
  // Draw the circle
  Surface.Picture.Bitmap.Canvas.Pen.Color := Colour;
  Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
  Surface.Picture.Bitmap.Canvas.Brush.Style := bsClear;                        // this will draw a circle without filling it (with brush color)
  Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, GR, GR);
  // Draw the circle center
  Surface.Picture.Bitmap.Canvas.Brush.Color := Colour;
  Surface.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
  // Display of circle parameters (do it for blue circle only -> nothing done if routine is called to draw the intersection circle)
  if Colour = clBlue then begin
    fGraph.edCenter.Text := PFormat(CX, CY, F);
    fGraph.edRadius.Text := RFormat(R, F); fGraph.edDiameter.Text := RFormat(2 * R, F);
    fGraph.edPerimeter.Text := RFormat(2 * Pi * R, F); fGraph.edArea.Text := RFormat(Pi * Sqr(R), F);
    if CPoints then begin
      // If circle has been defined by 2 diameter points, draw them and display their values
      fGraph.edPointA.Enabled := True; fGraph.edPointB.Enabled := True;
      fGraph.edPointA.Text := PFormat(AX, AY, F);
      GX := (W div 2) * L + Round(AX * L / Scale); GY := (H div 2) * L - Round(AY * L / Scale);
      Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
      fGraph.edPointB.Text := PFormat(BX, BY, F);
      GX := (W div 2) * L + Round(BX * L / Scale); GY := (H div 2) * L - Round(BY * L / Scale);
      Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
    end
    else begin
      // Disable the diameter points edit fields if not used
      fGraph.edPointA.Enabled := False; fGraph.edPointB.Enabled := False;
    end;
  end;
end;

{ Determine and draw the tangent line at a given circle point }

procedure TangentDraw(Surface: TImage; W, H, L: Integer; Scale, CX, CY, R, FX, FY: Real; F: Integer);

var
  GX, GY, GX1, GX2, GY1, GY2: Integer;
  M, C, MR, X, Y: Real;
  XLine: Boolean;

begin
  // Display tangent point
  fGraph.edTangentPoint.Text := PFormat(FX, FY, F);
  // Draw tangent point
  GX := (W div 2) * L + Round(FX * L / Scale); GY1 := (H div 2) * L - Round(FY * L / Scale);
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clBlue;
  Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY1, 4, 4);
  // Determine the tangent line and draw it
  M := 0; C := 0; XLine := False;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clRed;
  if (FX = CX - R) or (FX = CX + R) then begin
    // Special case for points (-r, Cy) and (+r, Cy): Tangent // Oy
    XLine := True; C := FX;
    GX := (W div 2) * L + Round(FX * L / Scale);
    GY1 := (H div 2) * L - Round((CY - 1.5 * R) * L / Scale); GY2 := (H div 2) * L - Round((CY + 1.5 * R) * L / Scale);
    Surface.Picture.Bitmap.Canvas.Line(GX, GY1, GX, GY2);
  end
  else if FX = CX then begin
    // Case for points (Cx, -r) and (Cx, +r): Tangent // Ox
    C := FY;
    GX1 := (W div 2) * L + Round((CX - 1.5 * R) * L / Scale); GX2 := (W div 2) * L + Round((CX + 1.5 * R) * L / Scale);
    GY := (H div 2) * L - Round(FY * L / Scale);
    Surface.Picture.Bitmap.Canvas.Line(GX1, GY, GX2, GY);
  end
  else begin
    // "Normal" case: Tangent is line with equation y = mx + c
    MR := (FY - CY) / (FX - CX);
    M := -1 / MR; C := FY - M * FX;
    // Draw tangent line, trying to limit its length, just displayed horizontaly and verticaly "nearby" the tangent point
    X := FX - 1.5 * R;
    repeat
      Y := M * X + C;
      GX := (W div 2) * L + Round(X * L / Scale); GY := (H div 2) * L - Round(Y * L / Scale);
      if X = FX - 1.5 * R then
        Surface.Picture.Bitmap.Canvas.MoveTo(GX, GY)
      else if Abs(Y - FY) <= 1.5 * R then
        Surface.Picture.Bitmap.Canvas.LineTo(GX, GY);
      X += R / 100;
    until X >= FX + 1.5 * R;
  end;
  // Determine and diaplay tangent line equation
  fGraph.edTangentEqu.Text := LineEquation(M, C, XLine, F);
end;

{ Determine and draw intersection of circle and given line }

procedure LineIntersectionDraw(Surface: TImage; W, H, L: Integer; Scale, CX, CY, R, LM, LC: Real; LineX: Boolean; F: Integer);

var
  NSol, GX, GY, GY1, GY2: Integer;
  A, B, C, X1, X2, Y1, Y2, X, Y, XStart, XEnd: Real;

begin
  fGraph.edIntersection.Lines.Clear; fGraph.edIntersectionI1.Text := ''; fGraph.edIntersectionI2.Text := '';
  // Special case: Line // Oy (equation: x = 0)
  if LineX then begin
    // With x-coordinates given from line parameters, calculate y-coordnates by solving quadratic equation
    A := 1;
    B := -2 * CY;
    C := Sqr(CY) - Sqr(R) + Sqr(LC - CX);
    X1 := LC; X2 := LC;
    EquQuadSolve(A, B, C, Y1, Y2, NSol);
    GX := (W div 2) * L + Round(LC * L / Scale);
    GY1 := (H div 2) * L - Round((CY - 1.5 * R) * L / Scale); GY2 := (H div 2) * L - Round((CY + 1.5 * R) * L / Scale);
    Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
    Surface.Picture.Bitmap.Canvas.Line(GX, GY1, GX, GY2);
  end
  // "Normal" case: Any line not // Oy (equation: y = mx + c)
  else begin
    // With x-coordinates determined by solving quadratic equation, calculate y-coordinates by filling in x in line equation
    A := Sqr(LM) + 1;
    B := 2 * LM * LC - 2 * CX - 2 * LM * CY;
    C := Sqr(CX) + Sqr(CY) + Sqr(LC) - Sqr(R) - 2 * CY * LC;
    EquQuadSolve(A, B, C, X1, X2, NSol);
    if NSol = 2 then begin
      Y1 := LM * X1 + LC; Y2 := LM * X2 + LC;
    end
    else if NSol = 1 then begin
      Y1 := LM * X1 + LC;
    end;
    // Draw the intersection line, trying to limit its length, just displayed "nearby" the intersection point(s)
    // or the circle center (if there isn't any). In some cases, this does not give, what I wanted to get...
    Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
    if NSol = 0 then begin
      XStart := CX; XEnd := CX;
    end
    else if NSol = 1 then begin
      XStart := X1; XEnd := X1;
    end
    else begin
      if X1 <= X2 then begin
        XStart := X1; XEnd := X2;
      end
      else begin
        XStart := X2; XEnd := X1;
      end;
    end;
    X := XStart - 1.5 * R;
    repeat
      Y := LM * X + LC;
      GX := (W div 2) * L + Round(X * L / Scale); GY := (H div 2) * L - Round(Y * L / Scale);
      if X = XStart - 1.5 * R then
        Surface.Picture.Bitmap.Canvas.MoveTo(GX, GY)
      else
        Surface.Picture.Bitmap.Canvas.LineTo(GX, GY);
      X += R / 100;
    until X >= XEnd + 1.5 * R;
  end;
  // If there is intersection, draw intersection point(s)
  if NSol = 0 then begin
    fGraph.edIntersection.Lines.AddText('There are no intersection points');
  end
  else begin
    fGraph.edIntersectionI1.Text := PFormat(X1, Y1, F);
    Surface.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    Surface.Picture.Bitmap.Canvas.Brush.Color := clFuchsia;
    GX := (W div 2) * L + Round(X1 * L / Scale); GY := (H div 2) * L - Round(Y1 * L / Scale);
    Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
    if NSol = 2 then begin
      fGraph.edIntersectionI2.Text := PFormat(X2, Y2, F);
      GX := (W div 2) * L + Round(X2 * L / Scale); GY := (H div 2) * L - Round(Y2 * L / Scale);
      Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
      fGraph.edIntersection.Lines.AddText('There are two intersection points');
    end
    else begin
      fGraph.edIntersection.Lines.AddText('There is one intersection point; the line is a tangent');
    end;
  end;
end;

{ Determine and draw intersection of circle and a second given circle }

procedure CircleIntersectionDraw(Surface: TImage; W, H, L: Integer; Scale, CX, CY, R, CCX, CCY, CR: Real; F: Integer);

var
  NSol, GX, GY: Integer;
  D, K1, K2, K3, A, B, C, X1, X2, Y1, Y2: Real;

begin
  fGraph.edIntersection.Lines.Clear; fGraph.edIntersectionI1.Text := ''; fGraph.edIntersectionI2.Text := '';
  // The distance between the circle centers tells us if the circles intersect or not
  D := Sqrt(Sqr(CX - CCX) + Sqr(CY - CCY));
  if D = 0 then begin
    // If the centers are the same, the circles are concentric (no intersection or identical circles if the radiuses are equal)
    if R = CR then begin
      fGraph.edIntersection.Lines.AddText('The two circles are identical');
      CircleDraw(Surface, W, H, L, Scale, CCX, CCY, CR, false, 0, 0, 0, 0, clFuchsia, F);
    end
    else begin
      fGraph.edIntersection.Lines.AddText('There are no intersection points (concentric circles)');
      CircleDraw(Surface, W, H, L, Scale, CCX, CCY, CR, false, 0, 0, 0, 0, clRed, F);
    end;
  end
  else begin
    // Two circles intersect, if |r1 - r2| <= d <= r1 + r2
    // If |r1 - r2| < d < r1 + r2, there are two intersection points (otherwise only one)
    if (Abs(R - CR) <= D) and (R + CR >= D) then begin
      // "Normal" case: The y-coordinates of the two circle centers are different)
      if CY <> CCY then begin
        // Calculate x-coordinates by solving quadratic equation and get y-coordinates by using the calculated x-values
        K1 := -(Sqr(CX) + Sqr(CY) - Sqr(R) - Sqr(CCX) - Sqr(CCY) + Sqr(CR));
        K2 := -((CCX - CX) / (CCY - CY));
        K3 := K1 / (2 * CCY - 2 * CY);
        A := 1 + Sqr(K2);
        B := -2 * CX + 2 * K2 * K3 - 2 * CY * K2;
        C := Sqr(CX) + Sqr(K3) - 2 * CY * K3 + Sqr(CY) - Sqr(R);
        EquQuadSolve(A, B, C, X1, X2, NSol);
        if NSol = 2 then begin
          Y1 := K2 * X1 + K3; Y2 := K2 * X2 + K3;
        end
        else if NSol = 1 then begin
          Y1 := K2 * X1 + K3;
        end;
      end
      // Special case: The y-coordinates of the two circle centers are the same
      // This needs a separate calculation, as doing as above would lead to a division by 0 when calculating K2
      else begin
        // Calculate y-coordinates by solving quadratic equation and get x-coordinates by using the calculated y-values
        K1 := -(Sqr(CY) + Sqr(CX) - Sqr(R) - Sqr(CCY) - Sqr(CCX) + Sqr(CR));
        K2 := -((CCY - CY) / (CCX - CX));
        K3 := K1 / (2 * CCX - 2 * CX);
        A := 1 + Sqr(K2);
        B := -2 * CY + 2 * K2 * K3 - 2 * CX * K2;
        C := Sqr(CY) + Sqr(K3) - 2 * CX * K3 + Sqr(CX) - Sqr(R);
        EquQuadSolve(A, B, C, Y1, Y2, NSol);
        if NSol = 2 then begin
          X1 := K2 * Y1 + K3; X2 := K2 * Y2 + K3;
        end
        else if NSol = 1 then begin
          X1 := K2 * Y1 + K3;
        end;
      end;
      // Display and draw intersection points
      fGraph.edIntersectionI1.Text := PFormat(X1, Y1, F);
      Surface.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
      Surface.Picture.Bitmap.Canvas.Brush.Color := clFuchsia;
      GX := (W div 2) * L + Round(X1 * L / Scale); GY := (H div 2) * L - Round(Y1 * L / Scale);
      Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
      if NSol = 2 then begin
        fGraph.edIntersectionI2.Text := PFormat(X2, Y2, F);
        GX := (W div 2) * L + Round(X2 * L / Scale); GY := (H div 2) * L - Round(Y2 * L / Scale);
        Surface.Picture.Bitmap.Canvas.EllipseC(GX, GY, 4, 4);
        fGraph.edIntersection.Lines.AddText('There are two intersection points');
      end
      else begin
        fGraph.edIntersection.Lines.AddText('There is one intersection point');
      end;
    end
    else begin
      // Two possible cases, if the circles haven't any intersection points:
      // 1. One circle is located "within" the other (I call these "internal circles")
      // 2. The intersection circle is located "somewhere outside" the original circle (I call these "external circles")
      if Abs(R - CR) <= D then
        fGraph.edIntersection.Lines.AddText('There are no intersection points (external circle)')
      else
        fGraph.edIntersection.Lines.AddText('There are no intersection points (internal circle)');
    end;
    // Draw the intersection circle
    CircleDraw(Surface, W, H, L, Scale, CCX, CCY, CR, false, 0, 0, 0, 0, clRed, F);
  end;
end;

{ Determine and draw the outer and inner squares of a given circle }

procedure ShapesDraw(Surface: TImage; W, H, L: Integer; Scale, CX, CY, R: Real; SqIn, SqOut: Boolean; F: Integer);

// For both squares, is considered the one with sides // Ox resp. Oy

var
  GX1, GY1, GX2, GY2: Integer;
  X1, Y1, X2, Y2: Real;

begin
  Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
  Surface.Picture.Bitmap.Canvas.Brush.Style := bsClear;
  if SqIn then begin
    // Inner square
    X1 := CX - R * Cos(Pi / 4); Y1 := CY - R * Sin (Pi / 4);
    X2 := CX + R * Cos(Pi / 4); Y2 := CY + R * Sin (Pi / 4);
    GX1 := (W div 2) * L + Round(X1 * L / Scale); GY1 := (H div 2) * L - Round(Y1 * L / Scale);
    GX2 := (W div 2) * L + Round(X2 * L / Scale); GY2 := (H div 2) * L - Round(Y2 * L / Scale);
    // Draw the inner square
    Surface.Picture.Bitmap.Canvas.Rectangle(GX1, GY1, GX2, GY2);
    // Display points defining the square (and length of square side)
    fGraph.edSquareInnerS1.Text := PFormat(X1, Y2, F);
    fGraph.edSquareInnerS2.Text := PFormat(X2, Y2, F);
    fGraph.edSquareInnerS3.Text := PFormat(X2, Y1, F);
    fGraph.edSquareInnerS4.Text := PFormat(X1, Y1, F);
    fGraph.edSquareInnerSide.Text := RFormat(Abs(X1 - X2), F);
  end;
  if SqOut then begin
    // Outer square
    X1 := CX - R; Y1 := CY - R;
    X2 := CX + R; Y2 := CY + R;
    GX1 := (W div 2) * L + Round(X1 * L / Scale); GY1 := (H div 2) * L - Round(Y1 * L / Scale);
    GX2 := (W div 2) * L + Round(X2 * L / Scale); GY2 := (H div 2) * L - Round(Y2 * L / Scale);
    // Draw the outer square
    Surface.Picture.Bitmap.Canvas.Rectangle(GX1, GY1, GX2, GY2);
    // Display points defining the square (and length of square side)
    fGraph.edSquareOuterS1.Text := PFormat(X1, Y2, F);
    fGraph.edSquareOuterS2.Text := PFormat(X2, Y2, F);
    fGraph.edSquareOuterS3.Text := PFormat(X2, Y1, F);
    fGraph.edSquareOuterS4.Text := PFormat(X1, Y1, F);
    fGraph.edSquareOuterSide.Text := RFormat(Abs(X1 - X2), F);
  end;
end;

{*********}
{ TfGraph }
{*********}

{ Application start: Initialisation }

procedure TfGraph.FormCreate(Sender: TObject);

begin
  // Create bitmap object and assign it to the drawing surface picture object
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  // Clear graph (display gridlines and axes)
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
  // Apply subscripts for concerned labels
  laIntersectionI1.Caption := StringReplace(laIntersectionI1.Caption, '1', SUB_1, []);
  laIntersectionI2.Caption := StringReplace(laIntersectionI2.Caption, '2', SUB_2, []);
  laSquareInnerS1.Caption := StringReplace(laSquareInnerS1.Caption, '1', SUB_1, []);
  laSquareInnerS2.Caption := StringReplace(laSquareInnerS2.Caption, '2', SUB_2, []);
  laSquareInnerS3.Caption := StringReplace(laSquareInnerS3.Caption, '3', SUB_3, []);
  laSquareInnerS4.Caption := StringReplace(laSquareInnerS4.Caption, '4', SUB_4, []);
  laSquareOuterS1.Caption := StringReplace(laSquareOuterS1.Caption, '1', SUB_1, []);
  laSquareOuterS2.Caption := StringReplace(laSquareOuterS2.Caption, '2', SUB_2, []);
  laSquareOuterS3.Caption := StringReplace(laSquareOuterS3.Caption, '3', SUB_3, []);
  laSquareOuterS4.Caption := StringReplace(laSquareOuterS4.Caption, '4', SUB_4, []);
  // The position of the different labels and edit fields in Lazarus form design does not correspond to their positions on
  // the user GUI (in fact, they are located at "some place, where the programmer can easily access them"). Changing the
  // controls' Top and Left properties at application startup, allows to "move" them to their "real" position on the GUI.
  laSquareOuter.Left := laTangent.Left; laSquareOuterS1.Left := laTangent.Left; laSquareOuterS2.Left := laTangent.Left;
  laSquareOuterS3.Left := laTangent.Left; laSquareOuterS4.Left := laTangent.Left; laSquareOuterSide.Left := laTangent.Left;
  edSquareOuterS1.Left := edTangentPoint.Left; edSquareOuterS2.Left := edTangentPoint.Left;
  edSquareOuterS3.Left := edTangentPoint.Left; edSquareOuterS4.Left := edTangentPoint.Left; edSquareOuterSide.Left := edTangentPoint.Left;
  laSquareOuter.Top := laSquareInner.Top;
  laSquareOuterS1.Top := laSquareInnerS1.Top; edSquareOuterS1.Top := edSquareInnerS1.Top;
  laSquareOuterS2.Top := laSquareInnerS2.Top; edSquareOuterS2.Top := edSquareInnerS2.Top;
  laSquareOuterS3.Top := laSquareInnerS3.Top; edSquareOuterS3.Top := edSquareInnerS3.Top;
  laSquareOuterS4.Top := laSquareInnerS4.Top; edSquareOuterS4.Top := edSquareInnerS4.Top;
  laSquareOuterSide.Top := laSquareInnerSide.Top; edSquareOuterSide.Top := edSquareInnerSide.Top;
  laIntersection.Top := laTangent.Top;
  laIntersectionI1.Top := laTangentPoint.Top; edIntersectionI1.Top := edTangentPoint.Top;
  laIntersectionI2.Top := laTangentEqu.Top; edIntersectionI2.Top := edTangentEqu.Top;
  edIntersection.Top := edTangentPoint.Top;
end;

{ Graph window, becoming active: Do calculations and drawings }

procedure TfGraph.FormActivate(Sender: TObject);

var
  L: Integer;

begin
  // Hide all calculation dependent controls (and make them visible, as needed)
  laTangent.Visible := False; laTangentPoint.Visible := False; edTangentPoint.Visible := False;
  laTangentEqu.Visible := False; edTangentEqu.Visible := False;
  laIntersection.Visible := False; edIntersection.Visible := False;
  laIntersectionI1.Visible := False; edIntersectionI1.Visible := False;
  laIntersectionI2.Visible := False; edIntersectionI2.Visible := False;
  laSquareInner.Visible := False; laSquareInnerS1.Visible := False; laSquareInnerS2.Visible := False;
  laSquareInnerS3.Visible := False; laSquareInnerS4.Visible := False; laSquareInnerSide.Visible := False;
  edSquareInnerS1.Visible := False; edSquareInnerS2.Visible := False;
  edSquareInnerS3.Visible := False; edSquareInnerS4.Visible := False; edSquareInnerSide.Visible := False;
  laSquareOuter.Visible := False; laSquareOuterS1.Visible := False; laSquareOuterS2.Visible := False;
  laSquareOuterS3.Visible := False; laSquareOuterS4.Visible := False; laSquareOuterSide.Visible := False;
  edSquareOuterS1.Visible := False; edSquareOuterS2.Visible := False;
  edSquareOuterS3.Visible := False; edSquareOuterS4.Visible := False; edSquareOuterSide.Visible := False;
  if sCalculation = 'tangent' then begin
    // Tangent at a given point: Make tangent related controls visible
    laTangent.Visible := True; laTangentPoint.Visible := True; edTangentPoint.Visible := True;
    laTangentEqu.Visible := True; edTangentEqu.Visible := True;
  end
  else if (sCalculation = 'line') or (sCalculation = 'circle') then begin
    // Intersection with a line or a circle: Make intersection related controls visible
    laIntersection.Visible := True; edInterSection.Visible := True;
    if sCalculation = 'line' then
      laIntersection.Caption := 'Intersection with line ' + sEquationInt
    else
      laIntersection.Caption := 'Intersection with circle ' + sEquationInt;
    laIntersectionI1.Visible := True; edIntersectionI1.Visible := True;
    laIntersectionI2.Visible := True; edIntersectionI2.Visible := True;
  end
  else begin
    // Inner and/or outer squares: Make square related controls visible
    if bSqOut then begin
      // Make outer square related controls visible
      laSquareOuter.Visible := True; laSquareOuterS1.Visible := True; laSquareOuterS2.Visible := True;
      laSquareOuterS3.Visible := True; laSquareOuterS4.Visible := True; laSquareOuterSide.Visible := True;
      edSquareOuterS1.Visible := True; edSquareOuterS2.Visible := True;
      edSquareOuterS3.Visible := True; edSquareOuterS4.Visible := True; edSquareOuterSide.Visible := True;
    end;
    if bSqIn then begin
      // Make inner square related controls visible
      laSquareInner.Visible := True; laSquareInnerS1.Visible := True; laSquareInnerS2.Visible := True;
      laSquareInnerS3.Visible := True; laSquareInnerS4.Visible := True; laSquareInnerSide.Visible := True;
      edSquareInnerS1.Visible := True; edSquareInnerS2.Visible := True;
      edSquareInnerS3.Visible := True; edSquareInnerS4.Visible := True; edSquareInnerSide.Visible := True;
      // "Move" inner square related controls to the right if both squares will be drawn
      L := 0;
      if bSqOut then
        L := 255;
      laSquareInner.Left := laTangent.Left + L; laSquareInnerS1.Left := laTangent.Left + L; laSquareInnerS2.Left := laTangent.Left + L;
      laSquareInnerS3.Left := laTangent.Left + L; laSquareInnerS4.Left := laTangent.Left + L; laSquareInnerSide.Left := laTangent.Left + L;
      edSquareInnerS1.Left := edTangentPoint.Left + L; edSquareInnerS2.Left := edTangentPoint.Left + L;
      edSquareInnerS3.Left := edTangentPoint.Left + L; edSquareInnerS4.Left := edTangentPoint.Left + L;
      edSquareInnerSide.Left := edTangentPoint.Left + L;
    end;
  end;
  // Clear drawing surface (display axes and gridlines)
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
  // Reset the scaling trackbar (zoom 1:1)
  tbScale.Position := 12; edScale.Text := '1'; rScale := StrToFloat(edScale.Text);
  // Draw the circle
  laCircle.Caption := 'Circle: ' + sEquation;
  CircleDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, bCirclePoints, rAX, rAY, rBX, rBY, clBlue, iFormat);
  // Determine and draw tangent, line/circle intersection, or square(s)
  if sCalculation = 'tangent' then
    TangentDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, rFX, rFY, iFormat)
  else if sCalculation = 'line' then
    LineIntersectionDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, rLM, rLC, bLineX, iFormat)
  else if sCalculation = 'circle' then
    CircleIntersectionDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, rCCX, rCCY, rCR, iFormat)
  else
    ShapesDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, bSqIn, bSqOut, iFormat);
end;

{ Button "Close": Close the graph window }

procedure TfGraph.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Trackbar changes: Set new drawing scaling value }

procedure TfGraph.tbScaleChange(Sender: TObject);

const
  Scales: array[0..24] of string = (
    '1/25', '1/20', '1/15', '1/10', '1/5', '3/10', '2/5', '1/2', '3/5', '7/10', '4/5', '9/10', '1',
    '11/10', '6/5', '13/10', '7/5', '3/2', '8/5', '17/10', '9/5', '19/10', '2', '3', '4'
  );

begin
  edScale.Text := Scales[tbScale.Position];
  rScale := 1 / FractionToFloat(Scales[tbScale.Position]);
  // Redo actual drawings with new scaling value
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
  CircleDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, bCirclePoints, rAX, rAY, rBX, rBY, clBlue, iFormat);
  if sCalculation = 'tangent' then
    TangentDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, rFX, rFY, iFormat)
  else if sCalculation = 'line' then
    LineIntersectionDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, rLM, rLC, bLineX, iFormat)
  else if sCalculation = 'circle' then
    CircleIntersectionDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, rCCX, rCCY, rCR, iFormat)
  else
    ShapesDraw(imDraw, FieldsX, FieldsY, FieldSize, rScale, rCX, rCY, rR, bSqIn, bSqOut, iFormat);
end;

end.

