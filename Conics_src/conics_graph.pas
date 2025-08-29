{************************************}
{* Graph unit of Conics application *}
{************************************}

unit conics_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TPoints2 = record
    Point1, Point2: string;
    Point1X, Point1Y, Point2X, Point2Y: Real;
  end;
  TLines2 = record
    Line1, Line2: string;
  end;
  {*********}
  { TfGraph }
  {*********}
  TfGraph = class(TForm)
    stTitle: TStaticText;
    imDraw: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    laConic, laConicA, laConicB, laVertices: TLabel;
    laFoci, laDirectrices, laAsymptotes: TLabel;
    edConicA, edConicB, edCenter, edVertex1, edVertex2, edFocus1, edFocus2: TEdit;
    edDirectrix1, edDirectrix2, edAsymptote1, edAsymptote2, edConicE, edConicC, edConicL, edConicP: TEdit;
    tbScale: TTrackBar;
    edScale: TEdit;
    btClose: TButton;
    rbRedraw: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure rbRedrawClick(Sender: TObject);
    procedure tbScaleChange(Sender: TObject);
  private
    rScale: Real;
    rcFoci, rcVertices: TPoints2;
    rcDirectrices, rcAsymptotes: TLines2;
    bmDraw: TBitmap;
  public
    rA, rB: Real;
    sConic, sEquation: string;
    bInverse: Boolean;
  end;

const
  SUP_2 = #$C2#$B2;

var
  fGraph: TfGraph;

implementation

{$R *.lfm}

const
  FieldsX = 40; FieldsY = 20; FieldSize = 25;

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

{ Swap 2 real variables }

procedure RSwap(var R1, R2: Real);

var
  R: Real;

begin
  R := R1; R1 := R2; R2 := R;
end;

{ Format real number, depending on how many significant decimal digits it has }

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
  else if 1000 * R = Int(1000 * R) then
    F := 3
  else
    F := 4;                                                                    // Sqr(a)/Sqr(b) supposed be greater than 0,001
  Result := FloatToStrF(R, ffFixed, 0, F);
end;

{ Format point string (Cartesian coordinates) }

function PFormat(X, Y: Real): string;

begin
  Result := '(' + RFormat(X) + ' ; ' + RFormat(Y) + ')';
end;

{ Get equation of actual conic with a and/or b replaced by actual values }

function ConicEquation(Conic, Equation: string; A, B: Real; Inverse: Boolean): string;

begin
  // Parabola
  if Conic = 'Parabola' then begin
    Equation := StringReplace(Equation, '4a', RFormat(4 * A), []);
    if Inverse then begin
      // "Vertically" turned parabola
      Equation := StringReplace(Equation, 'x', 'y', []);
      Equation := StringReplace(Equation, 'y', 'x', []);
    end;
  end
  // Other conics
  else begin
    if (Conic = 'Ellipse') or (Conic = 'Hyperbola') then begin
      // Ellipse or hyperbola
      if Inverse then begin
        // Conic with foci on y-axis
        Equation := StringReplace(Equation, 'b', 'a', []);
        Equation := StringReplace(Equation, 'a', 'b', []);
      end;
    end;
    Equation := StringReplace(Equation, 'a' + SUP_2, RFormat(Sqr(A)), []);
    Equation := StringReplace(Equation, 'b' + SUP_2, RFormat(Sqr(B)), []);
  end;
  Result := Equation;
end;

{ Calculate vertices of actual conic }

function ConicVertices(Conic: string; A: Real; Inverse: Boolean): TPoints2;

var
  Points: TPoints2;

begin
  Points.Point1Y := 0; Points.Point2Y := 0;
  // Parabola
  if Conic = 'Parabola' then begin
    Points.Point1X := 0;
    Points.Point2X := -1; Points.Point2Y := -1;                                // indicates, this point does not exits
  end
  // Other conics
  else begin
    Points.Point1X := A; Points.Point2X := -A;
  end;
  if Inverse then begin
    // For "vertically turned" conics, invert x- and y-axis
    RSwap(Points.Point1X, Points.Point1Y);
    RSwap(Points.Point2X, Points.Point2Y);
  end;
  Points.Point1 := PFormat(Points.Point1X, Points.Point1Y);
  if (Points.Point2X = -1) and (Points.Point2Y = -1) then
    Points.Point2 := ''                                                        // non-existing 2nd vertex
  else
    Points.Point2 := PFormat(Points.Point2X, Points.Point2Y);;
  Result := Points;
end;

{ Calculate foci for actual conic }

function ConicFoci(Conic: string; A, B: Real; Inverse: Boolean): TPoints2;

var
  C: Real;
  Points: TPoints2;

begin
  Points.Point1Y := 0; Points.Point2Y := 0;
  // Parabola
  if Conic = 'Parabola' then begin
    Points.Point1X := A;
    Points.Point2X := -1; Points.Point2Y := -1;                                // indicates, this point does not exits
  end
  // Circle
  else if Conic = 'Circle' then begin
    Points.Point1X := 0;
    Points.Point2X := -1; Points.Point2Y := -1;                                // indicates, this point does not exits
  end
  // Ellipse
  else if Conic = 'Ellipse' then begin
    C := Sqrt(Sqr(A) - Sqr(B));
    Points.Point1X := C;
    Points.Point2X := -C;
  end
  // Hyperbola
  else begin
    C := Sqrt(Sqr(A) + Sqr(B));
    Points.Point1X := C;
    Points.Point2X := -C;
  end;
  if Inverse then begin
    // For "vertically turned" conics, invert x- and y-axis
    RSwap(Points.Point1X, Points.Point1Y);
    RSwap(Points.Point2X, Points.Point2Y);
  end;
  Points.Point1 := PFormat(Points.Point1X, Points.Point1Y);
  if (Points.Point2X = -1) and (Points.Point2Y = -1) then
    Points.Point2 := ''                                                        // non-existing 2nd focus
  else
    Points.Point2 := PFormat(Points.Point2X, Points.Point2Y);
  Result := Points;
end;

{ Calculate eccentricity of actual conic }

function ConicEccentricity(Conic: string; A, B: Real): Real;

var
  E: Real;

begin
  if Conic = 'Parabola' then
    E := 1
  else if Conic = 'Circle' then
    E := 0
  else if Conic = 'Ellipse' then
    E := Sqrt(1 - Sqr(B) / Sqr(A))
  else
    E := Sqrt(1 + Sqr(B) / Sqr(A));
  Result := E;
end;

{ Calculate linear eccentricity of actual conic }

function ConicLinearEccentricity(Conic: string; A, B: Real): Real;

var
  C: Real;

begin
  if Conic = 'Parabola' then
    C := -1                                                                    // indicates, that linear eccentricity does not apply
  else if Conic = 'Circle' then
    C := 0
  else if Conic = 'Ellipse' then
    C := Sqrt(Sqr(a) - Sqr(B))
  else
    C := Sqrt(Sqr(a) + Sqr(B));
  Result := C;
end;

{ Calculate semi latus rectum of actual conic }

function ConicSemiLatusRectum(Conic: string; A, B: Real): Real;

var
  L: Real;

begin
  if Conic = 'Parabola' then
    L := Abs(2 * A)
  else if Conic = 'Circle' then
    L := A
  else
    L := Sqr(B) / A;
  Result := L;
end;

{ Calculate focal parameter of actual conic }

function ConicFocalParameter(Conic: string; A, B: Real): Real;

var
  P: Real;

begin
  if Conic = 'Parabola' then
    P := Abs(2 * A)
  else if Conic = 'Circle' then
    P := -1                                                                    // indicates, that focal parameter does not apply
  else if Conic = 'Ellipse' then
    P := Sqr(B) / Sqrt(Sqr(A) - Sqr(B))
  else
    P := Sqr(B) / Sqrt(Sqr(A) + Sqr(B));
  Result := P;
end;

{ Determine equations of actual conic's directrices }

function ConicDirectrices(Conic: string; A, B: Real; Inverse: Boolean): TLines2;

var
  C: Real;
  Directrices: TLines2;

begin
  // Parabola
  if Conic = 'Parabola' then begin
    Directrices.Line1 := 'x = ' + RFormat(-A); Directrices.Line2 := '';
  end
  // Circle
  else if Conic = 'Circle' then begin
    Directrices.Line1 := 'N/A'; Directrices.Line2 := '';
  end
  // Ellipse
  else if Conic = 'Ellipse' then begin
    C := Sqrt(Sqr(A) - Sqr(B));
    Directrices.Line1 := 'x = ' + RFormat(Sqr(A) / C); Directrices.Line2 := 'x = -' + RFormat(Sqr(A) / C);
  end
  // Hyperbola
  else begin
    C := Sqrt(Sqr(A) + Sqr(B));
    Directrices.Line1 := 'x = ' + RFormat(Sqr(A) / C); Directrices.Line2 := 'x = -' + RFormat(Sqr(A) / C);
  end;
  if Inverse then begin
    // For "vertically turned" conics, invert x- and y-axis
    Directrices.Line1 := StringReplace(Directrices.Line1, 'x', 'y', []);
    Directrices.Line2 := StringReplace(Directrices.Line2, 'x', 'y', []);
  end;
  Result := Directrices;
end;

{ Determine equation of actual hyperbola's asymptotes }

function HyperbolaAsymptotes(A, B: Real; Inverse: Boolean): TLines2;

var
  P: Real;
  Asymptotes: TLines2;

begin
  P := B / A;
  if Inverse then
    // "Vertically turned" hyperbola
    P := 1 / P;
  Asymptotes.Line1 := 'y = ' + RFormat(P) + 'x';
  Asymptotes.Line2 := 'y = -' + RFormat(P) + 'x';
  Result := Asymptotes;
end;

{ Draw actual conic }

procedure ConicDraw(Surface: TImage; W, H, L: Integer; Conic: string; A, B, Scale: Real; Vertices, Foci: TPoints2; Inverse: Boolean);

var
  CX, CY, CX1, CX2, CY1, CY2, CXOld, CYOld, CX1Old, CX2Old, CY1Old, CY2Old, CDX, CDY, I: Integer;
  X, Y, X1, X2, Y1, Y2, XMax: Real;
  Directrix1, Directrix2, Asymptote1, Asymptote2: string;

begin
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlue; Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
  CDX := 0;
  CXOld := 0; CYOld := 0; CX1Old := 0; CX2Old := 0; CY1Old := 0; CY2Old := 0;
  // Parabola
  // --------
  if Conic = 'Parabola' then begin
    if Inverse then begin
      // "Vertically turned" parabola
      Y := 0;
      repeat
        X1 := Sqrt(4 * Abs(A) * Y); X2 := -Sqrt(4 * Abs(A) * Y);
        if A > 0 then
          CY := (H div 2) * L - Round(Y * L / Scale)
        else
          CY := (H div 2) * L + Round(Y * L / Scale);
        CX1 := (W div 2) * L + Round(X1 * L / Scale); CX2 := (W div 2) * L + Round(X2 * L / Scale);
        if Y > 0 then begin
          Surface.Picture.Bitmap.Canvas.Line(CX1Old, CYOld, CX1, CY);
          Surface.Picture.Bitmap.Canvas.Line(CX2Old, CYOld, CX2, CY);
        end;
        CX1Old := CX1; CX2Old := CX2; CYOld := CY;
        Y += 0.25 * Scale;
      until (CX1 < 0) or (CX1 > W * L) or (CX2 < 0) or (CX2 > W * L);
    end
    else begin
      // "Horizontally turned" parabola
      X := 0;
      repeat
        Y1 := Sqrt(4 * Abs(A) * X); Y2 := -Sqrt(4 * Abs(A) * X);
        if A > 0 then
          CX := (W div 2) * L + Round(X * L / Scale)
        else
          CX := (W div 2) * L - Round(X * L / Scale);
        CY1 := (H div 2) * L - Round(Y1 * L / Scale); CY2 := (H div 2) * L - Round(Y2 * L / Scale);
        if X > 0 then begin
          Surface.Picture.Bitmap.Canvas.Line(CXOld, CY1Old, CX, CY1);
          Surface.Picture.Bitmap.Canvas.Line(CXOld, CY2Old, CX, CY2);
        end;
        CXOld := CX; CY1Old := CY1; CY2Old := CY2;
        X += 0.25 * Scale;
      until (CY1 < 0) or (CY1 > H * L) or (CY2 < 0) or (CY2 > H * L);
    end;
  end
  // Circle or ellipse
  // -----------------
  else if (Conic = 'Circle') or (Conic = 'Ellipse') then begin
    if Inverse then begin
      // "Vertically turned" ellipse
      Y := -A;
      repeat
        X1 := Sqrt(Sqr(B) * (1 - (Sqr(Y) / Sqr(A)))); X2 := -Sqrt(Sqr(B) * (1 - (Sqr(Y) / Sqr(A))));
        CX1 := (W div 2) * L + Round(X1 * L / Scale);
        CX2 := (W div 2) * L + Round(X2 * L / Scale);
        CY1 := (H div 2) * L - Round(Y * L / Scale);
        CY2 := (H div 2) * L - Round(Y * L / Scale);
        if Y <> -A then begin
          Surface.Picture.Bitmap.Canvas.Line(CX1Old, CY1Old, CX1, CY1);
          Surface.Picture.Bitmap.Canvas.Line(CX1Old, CY2Old, CX1, CY2);
          Surface.Picture.Bitmap.Canvas.Line(CX2Old, CY1Old, CX2, CY1);
          Surface.Picture.Bitmap.Canvas.Line(CX2Old, CY2Old, CX2, CY2);
        end;
        CX1Old := CX1; CX2Old := CX2; CY1Old := CY1; CY2Old := CY2;
        Y += 0.01 * Scale;
      until Y > A;
    end
    else begin
      // "Horizontally turned" ellipse (including circle)
      X := -A;
      repeat
        if Conic = 'Circle' then begin
          Y1 := Sqrt(Sqr(A) - Sqr(X)); Y2 := -(Sqrt(Sqr(A) - Sqr(X)));
        end
        else if Conic = 'Ellipse' then begin
          Y1 := Sqrt(Sqr(B) * (1 - (Sqr(X) / Sqr(A)))); Y2 := -Sqrt(Sqr(B) * (1 - (Sqr(X) / Sqr(A))));
        end;
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
  end
  // Hyperbola
  // ---------
  else begin
    if Inverse then begin
      // "Vertically turned" hyperbola
      for I := 1 to 2 do begin
        // Hyperbola has 2 branches
        if I = 1 then
          Y := -A
        else
          Y := A;
        repeat
          X1 := Sqrt(-Sqr(B) * (1 - (Sqr(Y) / Sqr(A)))); X2 := -Sqrt(-Sqr(B) * (1 - (Sqr(Y) / Sqr(A))));
          CX1 := (W div 2) * L + Round(X1 * L / Scale);
          CX2 := (W div 2) * L + Round(X2 * L / Scale);
          CY1 := (H div 2) * L + Round(Y * L / Scale);
          CY2 := (H div 2) * L - Round(Y * L / Scale);
          if Abs(Y) <> Abs(A) then begin
            Surface.Picture.Bitmap.Canvas.Line(CX1Old, CY1Old, CX1, CY1);
            Surface.Picture.Bitmap.Canvas.Line(CX2Old, CY2Old, CX2, CY2);
          end;
          CX1Old := CX1; CX2Old := CX2; CY1Old := CY1; CY2Old := CY2;
          if I = 1 then
            Y -= 0.25 * Scale
          else
            Y += 0.25 * Scale;
        until (CX1 < 0) or (CX1 > W * L) or (CX2 < 0) or (CX2 > W * L);
      end;
      XMax := X1;
    end
    else begin
      // "Horizontally turned" hyperbola
      for I := 1 to 2 do begin
        // Hyperbola has 2 branches
        if I = 1 then
          X := -A
        else
          X := A;
        repeat
          Y1 := Sqrt(-Sqr(B) * (1 - (Sqr(X) / Sqr(A)))); Y2 := -Sqrt(-Sqr(B) * (1 - (Sqr(X) / Sqr(A))));
          CX1 := (W div 2) * L + Round(X * L / Scale);
          CX2 := (W div 2) * L - Round(X * L / Scale);
          CY1 := (H div 2) * L - Round(Y1 * L / Scale); CY2 := (H div 2) * L - Round(Y2 * L / Scale);
          if Abs(X) <> Abs(A) then begin
            Surface.Picture.Bitmap.Canvas.Line(CX1Old, CY1Old, CX1, CY1);
            Surface.Picture.Bitmap.Canvas.Line(CX2Old, CY2Old, CX2, CY2);
          end;
          CX1Old := CX1; CX2Old := CX2; CY1Old := CY1; CY2Old := CY2;
          if I = 1 then
            X -= 0.25 * Scale
          else
            X += 0.25 * Scale;
        until (CY1 < 0) or (CY1 > H * L) or (CY2 < 0) or (CY2 > H * L);
      end;
      XMax := X;
    end;
  end;
  // Vertices
  // --------
  Surface.Picture.Bitmap.Canvas.Brush.Color := clBlue;
  CX := (W div 2) * L + Round(Vertices.Point1X * L / Scale);
  CY1 := (H div 2) * L - Round(Vertices.Point1Y * L / Scale);
  Surface.Picture.Bitmap.Canvas.EllipseC(CX, CY1, 5, 5);
  CX := (W div 2) * L + Round(Vertices.Point2X * L / Scale);
  CY2 := (H div 2) * L - Round(Vertices.Point2Y * L / Scale);
  if (Vertices.Point2X <> -1) or (Vertices.Point2Y <> -1) then                 // indicates that 2nd vertex does not exist
    Surface.Picture.Bitmap.Canvas.EllipseC(CX, CY2, 5, 5);
  // Foci
  // ----
  Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clRed;
  CX := (W div 2) * L + Round(Foci.Point1X * L / Scale);
  CY1 := (H div 2) * L - Round(Foci.Point1Y * L / Scale);
  Surface.Picture.Bitmap.Canvas.EllipseC(CX, CY1, 5, 5);
  CX := (W div 2) * L + Round(Foci.Point2X * L / Scale);
  CY2 := (H div 2) * L - Round(Foci.Point2Y * L / Scale);
  if (Foci.Point2X <> -1) or (Foci.Point2Y <> -1) then                         // indicates that 2nd focus does not exist
    Surface.Picture.Bitmap.Canvas.EllipseC(CX, CY2, 5, 5);
  // Directrices
  // -----------
  if Conic <> 'Circle' then begin
    // All conics, except circle
    Surface.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    Directrix1 := fGraph.edDirectrix1.Text; Directrix2 := fGraph.edDirectrix2.Text;
    if Inverse then begin
      // "Vertically turned" conic
      Directrix1 := StringReplace(Directrix1, 'y = ', '', []);
      CDY := (H div 2) * L - Round(StrToFloat(Directrix1) * L / Scale);
      Surface.Picture.Bitmap.Canvas.Line(0, CDY, W * L, CDY);
    end
    else begin
      // "Horizontally turned" conic
      Directrix1 := StringReplace(Directrix1, 'x = ', '', []);
      CDX := (W div 2) * L + Round(StrToFloat(Directrix1) * L / Scale);
      Surface.Picture.Bitmap.Canvas.Line(CDX, 0, CDX, H * L);
    end;
  end;
  if Directrix2 <> '' then begin
    // 2nd directrix only, if it exists
    if Inverse then begin
      // "Vertically turned" conic
      Directrix2 := StringReplace(Directrix2, 'y = ', '', []);
      CDY := (H div 2) * L - Round(StrToFloat(Directrix2) * L / Scale);
      Surface.Picture.Bitmap.Canvas.Line(0, CDY, W * L, CDY);
    end
    else begin
      // "Horizontally turned" conic
      Directrix2 := StringReplace(Directrix2, 'x = ', '', []);
      CDX := (W div 2) * L + Round(StrToFloat(Directrix2) * L / Scale);
      Surface.Picture.Bitmap.Canvas.Line(CDX, 0, CDX, H * L);
    end;
  end;
  // Asymptotes
  // ----------
  if Conic = 'Hyperbola' then begin
    // Hyperbola only
    Surface.Picture.Bitmap.Canvas.Pen.Color := clLime;
    Asymptote1 := fGraph.edAsymptote1.Text; Asymptote2 := fGraph.edAsymptote2.Text;
    Asymptote1 := StringReplace(Asymptote1, 'y = ', '', []); Asymptote2 := StringReplace(Asymptote2, 'y = ', '', []);
    Asymptote1 := StringReplace(Asymptote1, 'x', '', []); Asymptote2 := StringReplace(Asymptote2, 'x', '', []);
    X := -XMax;                                                                // XMax was calculated when drawing the hyperbola
    while X <= XMax do begin
      Y1 := StrToFloat(Asymptote1) * X; Y2 := StrToFloat(Asymptote2) * X;
      CX := (W div 2) * L + Round(X * L / Scale);
      CY1 := (H div 2) * L - Round(Y1 * L / Scale); CY2 := (H div 2) * L - Round(Y2 * L / Scale);
      if Abs(X) <> Abs(XMax) then begin
        Surface.Picture.Bitmap.Canvas.Line(CXOld, CY1Old, CX, CY1);
        Surface.Picture.Bitmap.Canvas.Line(CXOld, CY2Old, CX, CY2);
      end;
      CXOld := CX; CY1Old := CY1; CY2Old := CY2;
      X += 0.25 * Scale;
    end;
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

{ Graph window, becoming active: Do calculations and draw the actual conic }

procedure TfGraph.FormActivate(Sender: TObject);

begin
  stTitle.Caption := sConic + ': ' + ConicEquation(sConic, sEquation, rA, rB, bInverse);
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
  tbScale.Position := 12; edScale.Text := '1'; rScale := StrToFloat(edScale.Text);  // always resetting to 1:1 zoom avoids "problems" due to calculation time
  laConic.Caption := sConic + ' parameters:';
  // Adapt edit field labels (depending on actual conic)
  if sConic = 'Parabola' then
    laConicA.Caption := 'Leading coefficient'
  else if sConic = 'Circle' then
    laConicA.Caption := 'Radius'
  else begin
    laConicA.Caption := 'Semi-major axis'; laConicB.Caption := 'Semi-minor axis';
  end;
  edConicA.Text := RFormat(rA);
  if (sConic = 'Ellipse') or (sConic = 'Hyperbola') then begin
    laConicB.Visible := True; edConicB.Visible := True;
    edConicB.Text := RFormat(rB);
  end
  else begin
    laConicB.Visible := False; edConicB.Visible := False;
  end;
  // Conic center
  if sConic = 'Parabola' then
    edCenter.Text := 'N/A'
  else
    edCenter.Text := '(0 ; 0)';
  // Calculate and display conic parameters
  rcVertices := ConicVertices(sConic, rA, bInverse);
  rcFoci := ConicFoci(sConic, rA, rB, bInverse);
  rcDirectrices := ConicDirectrices(sConic, rA, rB, bInverse);
  edVertex1.Text := rcVertices.Point1;
  edFocus1.Text:=rcFoci.Point1;
  edDirectrix1.Text := rcDirectrices.Line1;
  if sConic = 'Parabola' then begin
    // 1 vertex and focus
    laFoci.Caption := 'Focus'; laVertices.Caption := 'Vertex'; laDirectrices.Caption := 'Directrix';
    edFocus2.Visible := False; edVertex2.Visible := False; edDirectrix2.Visible := False;
  end
  else begin
    // 2 vertices
    laVertices.Caption := 'Vertices';
    edVertex2.Visible := True;
    edVertex2.Text := rcVertices.Point2;
    if sConic = 'Circle' then begin
      // 1 focus, 1 directrix (in fact, none)
      laFoci.Caption := 'Focus';
      laDirectrices.Caption := 'Directrix';
      edFocus2.Visible := False;
      edDirectrix2.Visible := False;
    end
    else begin
      // 2 foci, 2 directrices
      laFoci.Caption := 'Foci';
      laDirectrices.Caption := 'Directrices';
      edFocus2.Visible := True;
      edFocus2.Text:=rcFoci.Point2;
      edDirectrix2.Visible := True;
      edDirectrix2.Text := rcDirectrices.Line2;
    end;
  end;
  // Asymptotes (for hyperbola)
  if sConic = 'Hyperbola' then begin
    rcAsymptotes := HyperbolaAsymptotes(rA, rB, bInverse);
    laAsymptotes.Visible := True; edAsymptote1.Visible := True; edAsymptote2.Visible := True;
    edAsymptote1.Text := rcAsymptotes.Line1; edAsymptote2.Text := rcAsymptotes.Line2;
  end
  else begin
    laAsymptotes.Visible := False; edAsymptote1.Visible := False; edAsymptote2.Visible := False;
  end;
  // Common parameters display
  edConicE.Text := RFormat(ConicEccentricity(sConic, rA, rB));
  edConicC.Text := RFormat(ConicLinearEccentricity(sConic, rA, rB));
  if edConicC.Text = '-1' then                                                           // -1 indicates, that center does not apply
    edConicC.Text := 'N/A';
  edConicL.Text := RFormat(ConicSemiLatusRectum(sConic, rA, rB));
  edConicP.Text := RFormat(ConicFocalParameter(sConic, rA, rB));
  if edConicP.Text = '-1' then                                                           // -1 indicates, that focal parameter is ∞
    edConicP.Text := '∞';
  // Drawing the actual conic
  ConicDraw(imDraw, FieldsX, FieldsY, FieldSize, sConic, rA, rB, rScale, rcVertices, rcFoci, bInverse);
end;

{ Button "Redraw": Redraw the actual conic (this button has to be pushed, after changing the drawing zoom)}

procedure TfGraph.rbRedrawClick(Sender: TObject);

begin
  DrawingSurfaceReset(imDraw, FieldsX, FieldsY, FieldSize);
  ConicDraw(imDraw, FieldsX, FieldsY, FieldSize, sConic, rA, rB, rScale, rcVertices, rcFoci, bInverse);
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
    '1/50', '1/25', '1/20', '1/10', '1/9', '1/8', '1/7', '1/6', '1/5', '1/4', '1/3', '1/2', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '20', '25', '50'
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

