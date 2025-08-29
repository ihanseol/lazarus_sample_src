{***************************************}
{* Main unit for Triangles application *}
{***************************************}

unit triangles_u1;

// Note:
// The code of the graph (and some other) routines is rather messy and would need to be rewritten:
//   - using arrays instead of scalar values
//   - using subroutines instead of repetitive code

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, Math;

type
  {*************}
  { TfTriangles }
  {*************}
  TfTriangles = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imDraw: TImage;
    rbCentroid, rbOrthocenter: TRadioButton;
    Label1, Label4, Label5, Label6, Label7, Label8: TLabel;
    Label9, Label10, Label11, Label2, Label3: TLabel;
    laPoint1, laPoint1X, laPoint1Y: TLabel;
    laPoint2, laPoint2X, laPoint2Y: TLabel;
    laPoint3, laPoint3X, laPoint3Y: TLabel;
    laLines, laLine1, laLine2, laLine3: TLabel;
    laPoint0, laPoint, laPointX, laPointY: TLabel;
    edXA, edYA, edXB, edYB, edXC, edYC: TEdit;
    edXP, edYP, edXP1, edYP1, edXP2, edYP2, edXP3, edYP3: TEdit;
    edLine1, edLine2, edLine3, edZoom: TEdit;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure rbCentroidChange(Sender: TObject);
    procedure rbOrthocenterChange(Sender: TObject);
  private
    rAX, rAY, rBX, rBY, rCX, rCY, rZoom: Real;
    bmDraw: TBitmap;
  end;

const
  WidthFields  = 30;                                                           // drawing surface width = 30 fields
  HeightFields = 20;                                                           // drawing surface height = 16 fields
  FieldSize    = 30;                                                           // pixelsize of 1 field
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  SUB_3 = #$E2#$82#$83;

var
  fTriangles: TfTriangles;

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

{ Format of a real number to specified number of decimal digits }

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
        SR := FloatToStrF(R, ffExponent, F, 0)                                 // use exponential format if number is to small for wanted format
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Pretty display of "équation cartésienne réduite" defined by M and P }

function FormatEquation(POrd: Boolean; M, P, C: Real): string;

var
  Eq: string;

// "Equation cartésienne réduite" of a line: y = mx + p (except for lines parallel to y-axis: x = c)

begin
  if POrd then
    // Line parallel to y-axis
    Eq := 'x = ' + RFormat(C, 3)
  else begin
    // All other lines
    Eq := 'y = ';
    if (M = 0) and (P = 0) then
      Eq += '0'
    else begin
      if M <> 0 then begin
        if M = 1 then
          Eq += 'x'
        else if M = -1 then
          Eq += '-x'
        else
          Eq += RFormat(M, 3) + 'x';
      end;
      if P <> 0 then begin
        if M <> 0 then begin
          if P > 0 then
            Eq += ' + ' + RFormat(P, 3)
          else
            Eq += ' - ' + RFormat(-P, 3);
        end
        else
          Eq += RFormat(P, 3);
      end;
    end;
  end;
  Result := Eq;
end;

{ Get equation of line passing through two points }

procedure LineEquation(XA, YA, XB, YB: Real; out POrd: Boolean; out M, P, C: Real);

begin
  if XA = XB then begin
    // Line parallel to y-axis
    POrd := True;
    C := XA;
  end
  else begin
    // All other lines
    POrd := False;
    M := (YB - YA) / (XB - XA);
    P := YA - M * XA;
  end;
end;

{ Get coordinates of intersection point of two lines }

procedure LineIntersection(POrd1, POrd2: Boolean; M1, P1, C1, M2, P2, C2: Real; out XI, YI: Real);

// The routine presumes that the 2 lines have an intersection point!

begin
  if POrd1 then begin
    // First line parallel to y-axis
    XI := C1; YI := M2 * XI + P2;
  end
  else if POrd2 then begin
    // Second line parallel to y-axis
    XI := C2; YI := M1 * XI + P1;
  end
  else begin
    // All other lines
    XI := (P2 - P1) / (M1 - M2); YI := M1 * XI + P1;
  end;
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

procedure DrawingSurfaceClear(Surface: TImage; W, H, L: Integer);

var
  I: Integer;

begin
  // Drawing surface as white rectancle
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
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
  DrawArrow(Surface, (W div 2) * L, (H div 2) * L, W * L, (H div 2) * L, clBlack);
  // Vertical gridline and y-axis
  for I := 1 to W - 1 do begin
    if I = W div 2 then
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(I * L, 1, I * L, H * L);
  end;
  DrawArrow(Surface, (W div 2) * L, (H div 2) * L, (W div 2) * L, 0, clBlack);
end;

{ Clear the form (incl. the drawing surface) }

procedure ClearForm(Surface: TImage; W, H, L: Integer);

begin
  DrawingSurfaceClear(Surface, W, H, L);
  fTriangles.edXP1.Text := ''; fTriangles.edYP1.Text := '';
  fTriangles.edXP2.Text := ''; fTriangles.edYP2.Text := '';
  fTriangles.edXP3.Text := ''; fTriangles.edYP3.Text := '';
  fTriangles.edLine1.Text := ''; fTriangles.edLine2.Text := ''; fTriangles.edLine3.Text := '';
  fTriangles.edXP.Text := ''; fTriangles.edYP.Text := '';
end;

{ Read coordinates of the 3 points defining the triangle }

procedure ReadTriangle(out AX, AY, BX, BY, CX, CY, Zoom: Real; out Mess: string);

begin
  Mess := '';
  if fTriangles.edZoom.Text = '' then
    fTriangles.edZoom.Text := '1';
  if fTriangles.edYC.Text = '' then begin
    Mess := 'coordonnée y du point C';
    fTriangles.edYC.SetFocus;
  end
  else
    CY := StrToFloat(fTriangles.edYC.Text);
  if fTriangles.edXC.Text = '' then begin
    Mess := 'coordonnée x du point C';
    fTriangles.edXC.SetFocus;
  end
  else
    CX := StrToFloat(fTriangles.edXC.Text);
  if fTriangles.edYB.Text = '' then begin
    Mess := 'coordonnée y du point B';
    fTriangles.edYB.SetFocus;
  end
  else
    BY := StrToFloat(fTriangles.edYB.Text);
  if fTriangles.edXB.Text = '' then begin
    Mess := 'coordonnée x du point B';
    fTriangles.edXB.SetFocus;
  end
  else
    BX := StrToFloat(fTriangles.edXB.Text);
  if fTriangles.edYA.Text = '' then begin
    Mess := 'coordonnée y du point A';
    fTriangles.edYA.SetFocus;
  end
  else
    AY := StrToFloat(fTriangles.edYA.Text);
  if fTriangles.edXA.Text = '' then begin
    Mess := 'coordonnée x du point A';
    fTriangles.edXA.SetFocus;
  end
  else
    AX := StrToFloat(fTriangles.edXA.Text);
  if Mess <> '' then begin
    Mess := 'Il faut entrer une valeur pour la ' + Mess;
  end
  else begin
    if (AX = BX) and (AY = BY) then begin
      Mess := 'A et B';
      fTriangles.edXP1.SetFocus;
    end
    else if (AX = CX) and (AY = CY) then begin
      Mess := 'A et C';
      fTriangles.edXP1.SetFocus;
    end
    else if (BX = CX) and (BY = CY) then begin
      Mess := 'B et C';
      fTriangles.edXP2.SetFocus;
    end;
    if Mess <> '' then begin
      Mess := 'Les coordonnées des points ' + Mess + ' doivent être différentes';
    end;
  end;
  if Mess <> '' then
    MessageDlg('Données invalides', Mess + '!', mtError, [mbOK], 0)
  else begin
    Zoom := StrToFloat(fTriangles.edZoom.Text);
    if Zoom <= 0 then begin
      MessageDlg('Zoom invalide', 'La valeur de zoom doit être positive! Valeur mise à zoom = 1', mtWarning, [mbOK], 0);
      fTriangles.edZoom.Text := '1'; Zoom := 1;
    end;
  end;
end;

{ Draw the triangle (using actual zoom factor) }

procedure DrawTriangle(Surface: TImage; WidthFields, HeightFields, FieldSize: Integer; AX, AY, BX, BY, CX, CY, Zoom: Real);

var
  X1, Y1, X2, Y2, I: Integer;
  PX1, PY1, PX2, PY2: Real;
  PA, PB, PC, PP: Char;
  PLabel: string;

begin
  // Try to find the best position to pretty display the triangle points names (A, B, C)
  if (AX <= BX) and (AX <= CX) then
    PA := 'l'
  else if (AX >= BX) and (AX >= CX) then
    PA := 'r'
  else if (AY <= BY) and (AY <= CY) then
    PA := 'b'
  else if (AY >= BY) and (AY >= CY) then
    PA := 't'
  else
    PA := 'l';
  if (BX <= AX) and (BX <= CX) then
    PB := 'l'
  else if (BX >= AX) and (BX >= CX) then
    PB := 'r'
  else if (BY <= AY) and (BY <= CY) then
    PB := 'b'
  else if (BY >= AY) and (BY >= CY) then
    PB := 't'
  else
    PB := 'l';
  if (CX <= AX) and (CX <= BX) then
    PC := 'l'
  else if (CX >= AX) and (CX >= BX) then
    PC := 'r'
  else if (CY <= AY) and (CY <= BY) then
    PC := 'b'
  else if (CY >= AY) and (CY >= BY) then
    PC := 't'
  else
    PC := 'l';
  // Clear the drawing surface
  DrawingSurfaceClear(Surface, WidthFields, HeightFields, FieldSize);
  // Calculate graph values
  for I := 1 to 3 do begin
    case I of
      1: begin PX1 := AX; PY1 := AY; PX2 := BX; PY2 := BY; PP := PA; PLabel := 'A'; end;
      2: begin PX1 := BX; PY1 := BY; PX2 := CX; PY2 := CY; PP := PB; PLabel := 'B'; end;
      3: begin PX1 := CX; PY1 := CY; PX2 := AX; PY2 := AY; PP := PC; PLabel := 'C'; end;
    end;
    X1 := (WidthFields div 2) * FieldSize + Round(PX1 * FieldSize * Zoom);
    Y1 := (HeightFields div 2) * FieldSize - Round(PY1 * FieldSize * Zoom);
    X2 := (WidthFields div 2) * FieldSize + Round(PX2 * FieldSize * Zoom);
    Y2 := (HeightFields div 2) * FieldSize - Round(PY2 * FieldSize * Zoom);
    // Draw the triangle
    Surface.Picture.Bitmap.Canvas.Pen.Color := clBlue;
    Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
    Surface.Picture.Bitmap.Canvas.Brush.Color := clBlue;
    Surface.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
    Surface.Picture.Bitmap.Canvas.EllipseC(X1, Y1, 4, 4);
    Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
    Surface.Picture.Bitmap.Canvas.Font.Color := clBlue;
    Surface.Picture.Bitmap.Canvas.Font.Style := [fsBold];
    // Display the point name (at position determined above)
    case PP of
      'l': Surface.Picture.Bitmap.Canvas.TextOut(X1 - 22, Y1 - 10, PLabel);
      'r': Surface.Picture.Bitmap.Canvas.TextOut(X1 + 12, Y1 - 10, PLabel);
      't': Surface.Picture.Bitmap.Canvas.TextOut(X1 - 5, Y1 - 26, PLabel);
      'b': Surface.Picture.Bitmap.Canvas.TextOut(X1 - 5, Y1 + 8, PLabel);
    end;
  end;
end;

{ Centroid of a triangle = intersection of the 3 medians }

procedure Centroid(Surface: TImage; WidthFields, HeightFields, FieldSize: Integer; AX, AY, BX, BY, CX, CY, Zoom: Real);

var
  X1, Y1, X2, Y2, I: Integer;
  PX1, PY1, PX2, PY2, GX, GY, M, P, C: Real;
  PA, PB, PC, PP: Char;
  PLabel: string;
  POrd: Boolean;

begin
  // Try to find the best position to pretty display the median points names (M1, M2, M3)
  if (AX <= BX) and (AX <= CX) then
    PA := 'r'
  else if (AX >= BX) and (AX >= CX) then
    PA := 'l'
  else if (AY <= BY) and (AY <= CY) then
    PA := 't'
  else if (AY >= BY) and (AY >= CY) then
    PA := 'b'
  else
    PA := 'r';
  if (BX <= AX) and (BX <= CX) then
    PB := 'r'
  else if (BX >= AX) and (BX >= CX) then
    PB := 'l'
  else if (BY <= AY) and (BY <= CY) then
    PB := 't'
  else if (BY >= AY) and (BY >= CY) then
    PB := 'b'
  else
    PB := 'r';
  if (CX <= AX) and (CX <= BX) then
    PC := 'r'
  else if (CX >= AX) and (CX >= BX) then
    PC := 'l'
  else if (CY <= AY) and (CY <= BY) then
    PC := 't'
  else if (CY >= AY) and (CY >= BY) then
    PC := 'b'
  else
    PC := 'r';
  // Draw the triangle
  DrawTriangle(Surface, WidthFields, HeightFields, FieldSize, AX, AY, BX, BY, CX, CY, Zoom);
  for I := 1 to 3 do begin
    // Determine the 3 medians
    case I of
      1: begin
        PX1 := AX; PY1 := AY; PX2 := (BX + CX) / 2; PY2 := (BY + CY) / 2;
        PP := PA; PLabel := 'M' + SUB_1;
        fTriangles.edXP1.Text := RFormat(PX2, 3); fTriangles.edYP1.Text := RFormat(PY2, 3);
        LineEquation(PX1, PY1, PX2, PY2, POrd, M, P, C);
        fTriangles.edLine1.Text := FormatEquation(POrd, M, P, C);
      end;
      2: begin
        PX1 := BX; PY1 := BY; PX2 := (AX + CX) / 2; PY2 := (AY + CY) / 2;
        PP := PB; PLabel := 'M' + SUB_2;
        fTriangles.edXP2.Text := RFormat(PX2, 3); fTriangles.edYP2.Text := RFormat(PY2, 3);
        LineEquation(PX1, PY1, PX2, PY2, POrd, M, P, C);
        fTriangles.edLine2.Text := FormatEquation(POrd, M, P, C);
      end;
      3: begin
        PX1 := CX; PY1 := CY; PX2 := (AX + BX) / 2; PY2 := (AY + BY) / 2;
        PP := PC; PLabel := 'M' + SUB_3;
        fTriangles.edXP3.Text := RFormat(PX2, 3); fTriangles.edYP3.Text := RFormat(PY2, 3);
        LineEquation(PX1, PY1, PX2, PY2, POrd, M, P, C);
        fTriangles.edLine3.Text := FormatEquation(POrd, M, P, C);
      end;
    end;
    // Calculate graph values
    X1 := (WidthFields div 2) * FieldSize + Round(PX1 * FieldSize * Zoom);
    Y1 := (HeightFields div 2) * FieldSize - Round(PY1 * FieldSize * Zoom);
    X2 := (WidthFields div 2) * FieldSize + Round(PX2 * FieldSize * Zoom);
    Y2 := (HeightFields div 2) * FieldSize - Round(PY2 * FieldSize * Zoom);
    // Draw the medians
    Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
    Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
    Surface.Picture.Bitmap.Canvas.Brush.Color := clRed;
    Surface.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
    Surface.Picture.Bitmap.Canvas.EllipseC(X2, Y2, 4, 4);
    Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
    Surface.Picture.Bitmap.Canvas.Font.Color := clRed;
    Surface.Picture.Bitmap.Canvas.Font.Style := [fsBold];
    // Display the point name (at position determined above)
    case PP of
      'l': Surface.Picture.Bitmap.Canvas.TextOut(X2 - 30, Y2 - 10, PLabel);
      'r': Surface.Picture.Bitmap.Canvas.TextOut(X2 + 12, Y2 - 10, PLabel);
      't': Surface.Picture.Bitmap.Canvas.TextOut(X2 - 8, Y2 - 26, PLabel);
      'b': Surface.Picture.Bitmap.Canvas.TextOut(X2 - 8, Y2 + 8, PLabel);
    end;
  end;
  // Calculate the coordinates of the centroid
  GX := (AX + BX + CX) / 3; GY := (AY + BY + CY) / 3;
  fTriangles.edXP.Text := RFormat(GX, 3); fTriangles.edYP.Text := RFormat(GY, 3);
  // Draw the centroid
  X1 := (WidthFields div 2) * FieldSize + Round(GX * FieldSize * Zoom);
  Y1 := (HeightFields div 2) * FieldSize - Round(GY * FieldSize * Zoom);
  Surface.Picture.Bitmap.Canvas.Brush.Color := clRed;
  Surface.Picture.Bitmap.Canvas.EllipseC(X1, Y1, 4, 4);
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Font.Color := clRed;
  Surface.Picture.Bitmap.Canvas.Font.Style := [fsBold];
  Surface.Picture.Bitmap.Canvas.TextOut(X1 - 6, Y1 + 8, 'G');
end;

{ Orthocenter of a triangle = intersection of the 3 altitudes }

procedure Orthocenter(Surface: TImage; WidthFields, HeightFields, FieldSize: Integer; AX, AY, BX, BY, CX, CY, Zoom: Real);

var
  X1, Y1, X2, Y2, I: Integer;
  PX1, PY1, PX2, PY2, OX, OY, HX, HY, M, P, C, MH, PH, CH, M1H, P1H, C1H, M2H, P2H, C2H, A1, A2, A3: Real;
  PA, PB, PC, PP: Char;
  PLabel: string;
  POrd, POrdH, POrd1H, POrd2H, Obtuse: Boolean;

begin
  // Try to find the best position to pretty display the altitude points names (H1, H2, H3)
  if (AX <= BX) and (AX <= CX) then
    PA := 'r'
  else if (AX >= BX) and (AX >= CX) then
    PA := 'l'
  else if (AY <= BY) and (AY <= CY) then
    PA := 't'
  else if (AY >= BY) and (AY >= CY) then
    PA := 'b'
  else
    PA := 'r';
  if (BX <= AX) and (BX <= CX) then
    PB := 'r'
  else if (BX >= AX) and (BX >= CX) then
    PB := 'l'
  else if (BY <= AY) and (BY <= CY) then
    PB := 't'
  else if (BY >= AY) and (BY >= CY) then
    PB := 'b'
  else
    PB := 'r';
  if (CX <= AX) and (CX <= BX) then
    PC := 'r'
  else if (CX >= AX) and (CX >= BX) then
    PC := 'l'
  else if (CY <= AY) and (CY <= BY) then
    PC := 't'
  else if (CY >= AY) and (CY >= BY) then
    PC := 'b'
  else
    PC := 'r';
  // Draw triangle
  DrawTriangle(Surface, WidthFields, HeightFields, FieldSize, AX, AY, BX, BY, CX, CY, Zoom);
  // Check if the triangle is obtuse
  // This version of the application calculates, bit does NOT draw the orthocenter for obtuse triangles!
  Obtuse := False;
  A1 := ArcCos(((BX - AX) * (CX - AX) + (BY - AY) * (CY - AY)) / (Sqrt(Sqr(BX - AX) + Sqr(BY - AY)) * Sqrt(Sqr(CX - AX) + Sqr(CY - AY))));
  A2 := ArcCos(((AX - BX) * (CX - BX) + (AY - BY) * (CY - BY)) / (Sqrt(Sqr(AX - BX) + Sqr(AY - BY)) * Sqrt(Sqr(CX - BX) + Sqr(CY - BY))));
  A3 := Pi - (A1 + A2);
  if (A1 > Pi / 2) or (A2 > Pi / 2) or (A3 > Pi / 2) then begin
    Obtuse := True;
    MessageDlg('Triangle obtus', 'L''orthocentre est à l''extérieur du triangle.', mtInformation, [mbOK], 0);
  end;
  for I := 1 to 3 do begin
    // Determine the 3 medians
    case I of
      1: begin
        LineEquation(BX, BY, CX, CY, POrd, M, P, C);
        if POrd then begin
          POrdH := False; MH := 0; PH := AY;
        end
        else begin
          if M = 0 then begin
            POrdH := True; CH := AX;
          end
          else begin
            POrdH := False; MH := -1 / M; PH := AY - MH * AX;
          end;
        end;
        POrd1H := POrdH; M1H := MH; P1H := PH; C1H := CH;
        fTriangles.edLine1.Text := FormatEquation(POrdH, MH, PH, CH);
        LineIntersection(POrd, POrdH, M, P, C, MH, PH, CH, HX, HY);
        PX1 := AX; PY1 := AY; PX2 := HX; PY2 := HY;
        PP := PA; PLabel := 'H' + SUB_1;
        fTriangles.edXP1.Text := RFormat(PX2, 3); fTriangles.edYP1.Text := RFormat(PY2, 3);
      end;
      2: begin
        LineEquation(AX, AY, CX, CY, POrd, M, P, C);
        if POrd then begin
          POrdH := False; MH := 0; PH := BY;
        end
        else begin
          if M = 0 then begin
            POrdH := True; CH := BX;
          end
          else begin
            POrdH := False; MH := -1 / M; PH := BY - MH * BX;
          end;
        end;
        POrd2H := POrdH; M2H := MH; P2H := PH; C2H := CH;
        fTriangles.edLine2.Text := FormatEquation(POrdH, MH, PH, CH);
        LineIntersection(POrd, POrdH, M, P, C, MH, PH, CH, HX, HY);
        PP := PB; PLabel := 'H' + SUB_2;
        PX1 := BX; PY1 := BY; PX2 := HX; PY2 := HY;
        fTriangles.edXP2.Text := RFormat(PX2, 3); fTriangles.edYP2.Text := RFormat(PY2, 3);
      end;
      3: begin
        LineEquation(AX, AY, BX, BY, POrd, M, P, C);
        if POrd then begin
          POrdH := False; MH := 0; PH := CY;
        end
        else begin
          if M = 0 then begin
            POrdH := True; CH := CX;
          end
          else begin
            POrdH := False; MH := -1 / M; PH := CY - MH * CX;
          end;
        end;
        fTriangles.edLine3.Text := FormatEquation(POrdH, MH, PH, CH);
        LineIntersection(POrd, POrdH, M, P, C, MH, PH, CH, HX, HY);
        PX1 := CX; PY1 := CY; PX2 := HX; PY2 := HY;
        PP := PC; PLabel := 'H' + SUB_3;
        fTriangles.edXP3.Text := RFormat(PX2, 3); fTriangles.edYP3.Text := RFormat(PY2, 3);
      end;
    end;
    if not Obtuse then begin
      // Calculate graph values
      X1 := (WidthFields div 2) * FieldSize + Round(PX1 * FieldSize * Zoom);
      Y1 := (HeightFields div 2) * FieldSize - Round(PY1 * FieldSize * Zoom);
      X2 := (WidthFields div 2) * FieldSize + Round(PX2 * FieldSize * Zoom);
      Y2 := (HeightFields div 2) * FieldSize - Round(PY2 * FieldSize * Zoom);
      // Draw the altitudes
      Surface.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
      Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
      Surface.Picture.Bitmap.Canvas.Brush.Color := clFuchsia;
      Surface.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
      Surface.Picture.Bitmap.Canvas.EllipseC(X2, Y2, 4, 4);
      Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
      Surface.Picture.Bitmap.Canvas.Font.Color := clFuchsia;
      Surface.Picture.Bitmap.Canvas.Font.Style := [fsBold];
      // Display the point name (at position determined above)
      case PP of
        'l': Surface.Picture.Bitmap.Canvas.TextOut(X2 - 30, Y2 - 10, PLabel);
        'r': Surface.Picture.Bitmap.Canvas.TextOut(X2 + 12, Y2 - 10, PLabel);
        't': Surface.Picture.Bitmap.Canvas.TextOut(X2 - 8, Y2 - 26, PLabel);
        'b': Surface.Picture.Bitmap.Canvas.TextOut(X2 - 8, Y2 + 8, PLabel);
      end;
    end;
  end;
  // Calculate the coordinates of the orthocenter
  LineIntersection(POrd1H, POrd2H, M1H, P1H, C1H, M2H, P2H, C2H, OX, OY);
  fTriangles.edXP.Text := RFormat(OX, 3); fTriangles.edYP.Text := RFormat(OY, 3);
  if not Obtuse then begin
    // Draw the orthocenter
    X1 := (WidthFields div 2) * FieldSize + Round(OX * FieldSize * Zoom);
    Y1 := (HeightFields div 2) * FieldSize - Round(OY * FieldSize * Zoom);
    Surface.Picture.Bitmap.Canvas.Brush.Color := clFuchsia;
    Surface.Picture.Bitmap.Canvas.EllipseC(X1, Y1, 4, 4);
    Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
    Surface.Picture.Bitmap.Canvas.Font.Color := clFuchsia;
    Surface.Picture.Bitmap.Canvas.Font.Style := [fsBold];
    Surface.Picture.Bitmap.Canvas.TextOut(X1 - 6, Y1 + 8, 'O');
  end;
end;

{*************}
{ TfTriangles }
{*************}

{ Application start: Initialization }

procedure TfTriangles.FormCreate(Sender: TObject);

begin
  // Apply subscripts
  laPoint1.Caption := StringReplace(laPoint1.Caption, '1', SUB_1, [rfReplaceAll]);
  laPoint2.Caption := StringReplace(laPoint2.Caption, '2', SUB_2, [rfReplaceAll]);
  laPoint3.Caption := StringReplace(laPoint3.Caption, '3', SUB_3, [rfReplaceAll]);
  laPoint1X.Caption := StringReplace(laPoint1X.Caption, '1', SUB_1, []);
  laPoint1Y.Caption := StringReplace(laPoint1Y.Caption, '1', SUB_1, []);
  laPoint2X.Caption := StringReplace(laPoint2X.Caption, '2', SUB_2, []);
  laPoint2Y.Caption := StringReplace(laPoint2Y.Caption, '2', SUB_2, []);
  laPoint3X.Caption := StringReplace(laPoint3X.Caption, '3', SUB_3, []);
  laPoint3Y.Caption := StringReplace(laPoint3Y.Caption, '3', SUB_3, []);
  laLine1.Caption := StringReplace(laLine1.Caption, '1', SUB_1, []);
  laLine2.Caption := StringReplace(laLine2.Caption, '2', SUB_2, []);
  laLine3.Caption := StringReplace(laLine3.Caption, '3', SUB_3, []);
  // Create bitmap for canvas drawing
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  // Clear the drawing surface
  DrawingSurfaceClear(imDraw, WidthFields, HeightFields, FieldSize);
end;

{ Menu item: "Fichuer > Quitter: Exit application" }

procedure TfTriangles.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item: "Aide > Info: Display application about" }

procedure TfTriangles.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Géométrie analytique:' + LineEnding;
  S += 'Détermination du centre de gravité et de l''orthocentre d''un triangle.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, septembre - novembre 2023.';
  MessageDlg('Info "Triangles"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calcul" pushed: Do the centroid and orthocenter calculations }

procedure TfTriangles.btCalcClick(Sender: TObject);

var
  Mess: string;

begin
  ClearForm(imDraw, WidthFields, HeightFields, FieldSize);
  ReadTriangle(rAX, rAY, rBX, rBY, rCX, rCY, rZoom, Mess);
  if Mess = '' then begin
    if rbCentroid.Checked then begin
      // Do the calculations to draw the centroid
      Centroid(imDraw, WidthFields, HeightFields, FieldSize, rAX, rAY, rBX, rBY, rCX, rCY, rZoom);
    end
    else begin
      // Do the calculations to draw the orthocenter
      Orthocenter(imDraw, WidthFields, HeightFields, FieldSize, rAX, rAY, rBX, rBY, rCX, rCY, rZoom);
    end;
  end;
end;

{ Point selection: Centroid or orthocenter (user clicked corr. radio button) }

procedure TfTriangles.rbCentroidChange(Sender: TObject);

begin
  if rbCentroid.Checked then begin
    // Centroid = intersection of the 3 medians
    laLines.Caption := 'Médianes:';
    laPoint1.Caption := StringReplace(laPoint1.Caption, 'H', 'M', []);
    laPoint1.Caption := StringReplace(laPoint1.Caption, 'h', 'm', [rfReplaceAll]);
    laPoint2.Caption := StringReplace(laPoint2.Caption, 'H', 'M', []);
    laPoint2.Caption := StringReplace(laPoint2.Caption, 'h', 'm', [rfReplaceAll]);
    laPoint3.Caption := StringReplace(laPoint3.Caption, 'H', 'M', []);
    laPoint3.Caption := StringReplace(laPoint3.Caption, 'h', 'm', [rfReplaceAll]);
    laPoint1X.Caption := StringReplace(laPoint1X.Caption, 'h', 'm', []);
    laPoint2X.Caption := StringReplace(laPoint2X.Caption, 'h', 'm', []);
    laPoint3X.Caption := StringReplace(laPoint3X.Caption, 'h', 'm', []);
    laPoint1Y.Caption := StringReplace(laPoint1Y.Caption, 'h', 'm', []);
    laPoint2Y.Caption := StringReplace(laPoint2Y.Caption, 'h', 'm', []);
    laPoint3Y.Caption := StringReplace(laPoint3Y.Caption, 'h', 'm', []);
    laLine1.Caption := StringReplace(laLine1.Caption, 'H', 'M', []);
    laLine2.Caption := StringReplace(laLine2.Caption, 'H', 'M', []);
    laLine3.Caption := StringReplace(laLine3.Caption, 'H', 'M', []);
    laPoint0.Caption := 'Centre de gravité';
    laPoint.Caption := StringReplace(laPoint.Caption, 'O', 'G', []);
    laPoint.Caption := StringReplace(laPoint.Caption, 'o', 'g', [rfReplaceAll]);
    laPointX.Caption := StringReplace(laPointX.Caption, 'o', 'g', []);
    laPointY.Caption := StringReplace(laPointY.Caption, 'o', 'g', []);
  end;
end;

procedure TfTriangles.rbOrthocenterChange(Sender: TObject);

begin
  if rbOrthocenter.Checked then begin
    // Orthocenter = intersection of the 3 altitudes
    laLines.Caption := 'Hauteurs:';
    laPoint1.Caption := StringReplace(laPoint1.Caption, 'M', 'H', []);
    laPoint1.Caption := StringReplace(laPoint1.Caption, 'm', 'h', [rfReplaceAll]);
    laPoint2.Caption := StringReplace(laPoint2.Caption, 'M', 'H', []);
    laPoint2.Caption := StringReplace(laPoint2.Caption, 'm', 'h', [rfReplaceAll]);
    laPoint3.Caption := StringReplace(laPoint3.Caption, 'M', 'H', []);
    laPoint3.Caption := StringReplace(laPoint3.Caption, 'm', 'h', [rfReplaceAll]);
    laPoint1X.Caption := StringReplace(laPoint1X.Caption, 'm', 'h', []);
    laPoint2X.Caption := StringReplace(laPoint2X.Caption, 'm', 'h', []);
    laPoint3X.Caption := StringReplace(laPoint3X.Caption, 'm', 'h', []);
    laPoint1Y.Caption := StringReplace(laPoint1Y.Caption, 'm', 'h', []);
    laPoint2Y.Caption := StringReplace(laPoint2Y.Caption, 'm', 'h', []);
    laPoint3Y.Caption := StringReplace(laPoint3Y.Caption, 'm', 'h', []);
    laLine1.Caption := StringReplace(laLine1.Caption, 'M', 'H', []);
    laLine2.Caption := StringReplace(laLine2.Caption, 'M', 'H', []);
    laLine3.Caption := StringReplace(laLine3.Caption, 'M', 'H', []);
    laPoint0.Caption := 'Orthocentre';
    laPoint.Caption := StringReplace(laPoint.Caption, 'G', 'O', []);
    laPoint.Caption := StringReplace(laPoint.Caption, 'g', 'o', [rfReplaceAll]);
    laPointX.Caption := StringReplace(laPointX.Caption, 'g', 'o', []);
    laPointY.Caption := StringReplace(laPointY.Caption, 'g', 'o', []);
  end;
end;

end.

