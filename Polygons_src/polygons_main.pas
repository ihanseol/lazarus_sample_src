{**************************************}
{* Main unit for Polygons application *}
{**************************************}

unit polygons_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus;

type
  {************}
  { TfPolygons }
  {************}
  TfPolygons = class(TForm)
    mMenu: TMainMenu;
    mPolygon, mPolygonExit, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    Label9, Label10, Label11, Label12, Label13, Label14, Label15: TLabel;
    Label16, Label17, Label18, Label19, Label20, Label21: TLabel;
    imDisplay: TImage;
    laPolygon, laUAExt, laUAInt, laT1UAngle, laT2UAngle: TLabel;
    edN, edR, edAp, edAInt, edAExt, edA, edP, edS: TEdit;
    edCircleR, edCircleAp, edDiags: TEdit;
    edT1a, edT1b, edT1c, edT1Angle, edT1Area: TEdit;
    edT2a, edT2b, edT2c, edT2Angle, edT2Area: TEdit;
    cbR, cbAp, cbCercleR, cbCircleAp, cbDiagonals, cbT1, cbT2: TCheckBox;
    btCompute: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPolygonExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btComputeClick(Sender: TObject);
  private
    iDisplayWidth, iDisplayHeight, iNSides: Integer;
    rSideLength, rRadius, rApothem, rA: Real;
    bmBitmap: TBitmap;
  end;

var
  fPolygons: TfPolygons;

implementation

{$R *.lfm}

{ Radiants to degrees conversion }

function RadToDeg(A: Real): Real;

begin
  Result := 360 * A / (2 * Pi);
end;

{ Clear drawing display (by showing a white rectangle) }

procedure DisplayClear(W, H: Integer);

begin
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 1;
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw polygon and related properties (if selected so) }

procedure PolygonDraw(W, H, N: Integer; L, R, Ap: Real);

var
  LX, X0, Y0, X1, Y1, X2, Y2, I, J: Integer;
  Mult: Real;
  Coords: array [1..20] of record
    X, Y: Integer;
  end;

begin
  DisplayClear(W, H);                                                          // clear drawing surface
  Mult := (H - 50) / (2 * R);                                                  // drawing scaling multiplication factor
  LX := Round(Mult * L);                                                       // scaled polygon side length
  // Determine drawing starting point
  X0 := W div 2 + LX div 2;
  Y0 := H div 2 + Round(Mult * Ap);
  // Drawing pen and font size settings
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 2;
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Font.Size := 10;
  // Draw the circles first (filled circles would otherwise overlay on rest of drawing)
  if fPolygons.cbCercleR.Checked then begin
    // Circumcircle
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clRed;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.EllipseC(W div 2, H div 2, Round(R * Mult), Round(R * Mult));
  end;
  if fPolygons.cbCircleAp.Checked then begin
    // Incircle
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.EllipseC(W div 2, H div 2, Round(Ap * Mult), Round(Ap * Mult));
  end;
  // Draw the polygon
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 4;
  fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  fPolygons.imDisplay.Picture.Bitmap.Canvas.MoveTo(X0, Y0);                    // start drawing at this point
  X1 := X0; Y1 := Y0;                                                          // first point of line = starting point
  for I := 1 to N do begin
    // Draw one polygon side after the other
    Coords[I].X := X1; Coords[I].Y := Y1;                                      // save point coordinates (for diagonals drawing)
    X2 := Round(X1 + LX * Cos(I * 2 * Pi / N));                                // second point of line X coordinate
    Y2 := Round(Y1 - LX * Sin(I * 2 * Pi / N));                                // second point of line Y coordinate
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);            // draw the line from first to second point
    X1 := X2; Y1 := Y2;                                                        // second point will be first point of next line (= side)
  end;
  // Draw diagonals
  if fPolygons.cbDiagonals.Checked then begin
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 1;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clGreen;
    for I := 1 to N do begin
      // At each point N of the polygon, there are starting N - 3 diagonals
      for J := 1 to N - 3 do begin
        if I + J + 1 <= N then
          fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(Coords[I].X, Coords[I].Y, Coords[I + J + 1].X, Coords[I + J + 1].Y);
      end;
    end;
  end;
  // Draw polygon triangle
  if fPolygons.cbT1.Checked then begin
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 2;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Font.Color := clRed;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clRed;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(Coords[1].X, Y0, W div 2, Y0 - Round(Ap * Mult));
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(Coords[N].X, Y0, W div 2, Y0 - Round(Ap * Mult));
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(Coords[N].X, Y0, Coords[1].X, Y0);
  end;
  // Draw small triangle
  if fPolygons.cbT2.Checked then begin
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 2;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Font.Color := clFuchsia;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(W div 2, Y0, W div 2, Y0 - Round(Ap * Mult));
    if fPolygons.cbT1.Checked then
      fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clRed;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(X0, Y0, W div 2, Y0 - Round(Ap * Mult));
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(W div 2, Y0, X0, Y0);
  end;
  // Draw apothem
  if fPolygons.cbAp.Checked then begin
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 2;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Font.Color := clFuchsia;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(W div 2, Y0, W div 2, Y0 - Round(Ap * Mult));
    fPolygons.imDisplay.Picture.Bitmap.Canvas.TextOut(W div 2 - 65, Y0 - Round(Ap * Mult / 2), 'Apothem') ;
  end;
  // Draw radius
  if fPolygons.cbR.Checked then begin
    if fPolygons.cbT2.Checked and not fPolygons.cbT1.Checked then begin
      fPolygons.imDisplay.Picture.Bitmap.Canvas.Font.Color := clFuchsia;
      fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    end
    else begin
      fPolygons.imDisplay.Picture.Bitmap.Canvas.Font.Color := clRed;
      fPolygons.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clRed;
    end;
    fPolygons.imDisplay.Picture.Bitmap.Canvas.Line(X0, Y0, W div 2, Y0 - Round(Ap * Mult));
    fPolygons.imDisplay.Picture.Bitmap.Canvas.TextOut(W div 2 + LX div 3, Y0 - Round(Ap * Mult / 2), 'Radius') ;
  end;
end;

{************}
{ TfPolygons }
{************}

{ Application start: Initialisation }

procedure TfPolygons.FormCreate(Sender: TObject);

begin
  // Create a bitmap object and assign dimensions
    iDisplayWidth := imDisplay.Width; iDisplayHeight := imDisplay.Height;
    bmBitmap := TBitmap.Create;
    bmBitmap.Width := iDisplayWidth;
    bmBitmap.Height := iDisplayHeight;
    //Assign the bitmap to the image component (the drawing surface)
    imDisplay.Picture.Graphic := bmBitmap;
    // Clear the form (draw display area)
    DisplayClear(iDisplayWidth, iDisplayHeight);
end;

{ Menu item "Polygon > Exit": Exit application }

procedure TfPolygons.mPolygonExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfPolygons.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Geometry:' + LineEnding;
  S += 'Properties of regular polygons.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2020.';
  MessageDlg('About "Polygons"', S, mtInformation, [mbOK], 0);
end;

{ Button "Compute": Draw polygon and compute its properties }

procedure TfPolygons.btComputeClick(Sender: TObject);

const
  PolygonNames: array[3..20] of string = (
    'Triangle (Trigon)', 'Quadrilateral (Tetragon)', 'Pentagon', 'Hexagon', 'Heptagon (Septagon)', 'Octagon', 'Nonagon (Enneagon)',
    'Decagon', 'Hendecagon (Undecagon)', 'Dodecagon', 'Triskaidecagon', 'Tetrakaidecagon', 'Pentadecagon', 'Hexakaidecagon',
    'Heptadecagon', 'Octakaidecagon', 'Enneadecagon', 'Icosagon'
  );

var
  Mess: string;

begin
  Mess := '';
  // Get number of sides and side length from user
  if edN.Text = '' then
    iNSides := 0
  else
    iNSides := StrToInt(edN.Text);
  if edS.Text = '' then
    rSideLength := 0
  else
    rSideLength := StrToFloat(edS.Text);
  // Check user values
  if iNSides < 3 then begin
    Mess := 'Invalid number of polygon sides';
    edN.SetFocus;
  end
  else if iNSides > 20 then begin                                              // above 20 sides, drawing starts to become unreadable
    Mess := 'Please choose number of polygon sides between 3 and 20';
    edN.SetFocus;
  end
  else if rSideLength <= 0 then begin
    Mess := 'Invalid polygon side length';
    edS.SetFocus;
  end;
  // If user values ok, do the calculations
  if Mess = '' then begin
    // Polygon name
    laPolygon.Caption := PolygonNames[iNSides] + ':';
    // External and internal angle, radius and apothem
    rA := RadToDeg(2 * Pi / iNSides);
    edAExt.Text := FloatToStr(Round(1000 * rA) / 1000);
    edAInt.Text := FloatToStr(Round(1000 * (180 - rA)) / 1000);
    rRadius  := rSideLength / (2 * Sin(Pi / iNSides));
    edR.Text := FloatToStr(Round(1000 * rRadius) / 1000);
    rApothem := rRadius * Cos(Pi / iNSides);
    edAp.Text := FloatToStr(Round(1000 * rApothem) / 1000);
    // Perimeter and surface area
    edP.Text := FloatToStr(Round(1000 * iNSides * rSideLength) / 1000);
    edA.Text := FloatToStr(Round(1000 * iNSides * rSideLength * rApothem / 2) / 1000);
    // Equations of circumcircle and incercle, number of diagonals
    edCircleR.Text := 'x² + y² = ' + FloatToStr(Round(1000 * Sqr(rRadius)) / 1000);
    edCircleAp.Text := 'x² + y² = ' + FloatToStr(Round(1000 * Sqr(rApothem)) / 1000);
    edDiags.Text := IntToStr(iNSides * (iNSides - 3) div 2);
    // Polygon main triangles
    edT1a.Text := FloatToStr(Round(1000 * rRadius) / 1000);
    edT1b.Text := FloatToStr(Round(1000 * rSideLength) / 1000);
    edT1c.Text := FloatToStr(Round(1000 * rRadius) / 1000);
    edT1Angle.Text := FloatToStr(Round(1000 * 360 / iNSides) / 1000);
    edT1Area.Text  := FloatToStr(Round(1000 * rSideLength * rApothem / 2) / 1000);
    // Small triangles
    edT2c.Text := FloatToStr(Round(1000 * rRadius) / 1000);
    edT2b.Text := FloatToStr(Round(1000 * rSideLength / 2) / 1000);
    edT2a.Text := FloatToStr(Round(1000 * rApothem) / 1000);
    edT2Angle.Text := FloatToStr(Round(1000 * 180 / iNSides) / 1000);
    edT2Area.Text  := FloatToStr(Round(1000 * 0.5 * rApothem * rSideLength / 2) / 1000);;
    // Draw the polygon (and properties as selected)
    PolygonDraw(iDisplayWidth, iDisplayHeight, iNSides, rSideLength, rRadius, rApothem);
    // Focus "number of sides" field for next user entry
    edN.SetFocus;
  end
  else
    // Error message when user given parametesr are invalid
    MessageDlg('Parameter error', Mess + '!', mtError, [mbOK], 0);
end;

end.

