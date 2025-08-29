{***************************************}
{* Main unit for Geometry3 application *}
{***************************************}

unit geo3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, help;

type
  TPoint = record
    X, Y: Real;
  end;
  TPoints = array of TPoint;
  TPointEditFields = record
    FieldX, FieldY: TEdit;
  end;
  TPointsEditFields = array[0..3] of TPointEditFields;
  {********}
  { TfGeo3 }
  {********}
  TfGeo3 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsZoom, mOptionsZoom10, mOptionsZoom20, mOptionsZoom40: TMenuItem;
    mHelp, mHelpGeometry, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imDraw: TImage;
    rbTranslation, rbRotation, rbReflexion, rbHomothety: TRadioButton;
    cobRotTheta, cobReflAxis, cobObject: TComboBox;
    Label1, Label2, Label4, Label5, Label7, Label6, Label10: TLabel;
    laTransformation, laRotCentre, laHomCentre, laPtA, laPtB, laPtC, laPtD: TLabel;
    edTransA, edTransB, edRotX0, edRotY0, edHomX0, edHomY0, edHomK: TEdit;
    edX1, edX2, edX3, edX4, edY1, edY2, edY3, edY4: TEdit;
    edTX1, edTX2, edTX3, edTX4, edTY1, edTY2, edTY3, edTY4: TEdit;
    btCompute: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsZoom10Click(Sender: TObject);
    procedure mOptionsZoom20Click(Sender: TObject);
    procedure mOptionsZoom40Click(Sender: TObject);
    procedure mHelpGeometryClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btComputeClick(Sender: TObject);
    procedure rbTranslationChange(Sender: TObject);
    procedure rbRotationChange(Sender: TObject);
    procedure rbReflexionChange(Sender: TObject);
    procedure rbHomothetyChange(Sender: TObject);
    procedure cobObjectChange(Sender: TObject);
  private
    iPixles, iWidthFields, iHeightFields, iFieldSize: Integer;
    rTransA, rTransB, rRotTheta, rHomK: Real;
    sReflAxis: string;
    rRotCenter, rHomCenter: TPoint;
    aPoints, aPoints2: TPoints;
    edPoints, edPoints2: TPointsEditFields;
    bmDraw: TBitmap;
  end;

const
  WidthFields  = 80;                                                           // max. drawing surface width = 80 fields
  HeightFields = 64;                                                           // max. drawing surface height = 64 fields
  FieldSize    = 10;                                                           // corr. pixelsize of 1 field

var
  fGeo3: TfGeo3;

implementation

{$R *.lfm}

{ Get system (local) specific decimal separator }

function GetDecimalSeparator: Char;

var
  SR: string;

begin
  SR := FloatToStrF(1.2, ffFixed, 0, 1);
  Result := SR[2];
end;

{ Check if a given string is numeric }

function IsNumeric(SR: string): Boolean;

var
  I: Integer;
  DC: Char;
  ItIs: Boolean;

begin
  if Length(SR) = 0 then
    ItIs := False
  else begin
    ItIs := True;
    DC := GetDecimalSeparator;
    for I := 1 to Length(SR) do begin
      if not (SR[I] in ['0'..'9', '-', DC]) then
        ItIs := False;
    end;
  end;
  Result := ItIs;
end;

{ Format real number (cut non-significant decimal zeros) }

function RFormat(R: Real): string;

var
  F: Integer;

begin
  if R = Int(R) then
    F := 0
  else if R * 10 = Int(R * 10) then
    F := 1
  else if R * 100 = Int(R * 100) then
    F := 2
  else
    F := 3;
  Result := FloatToStrF(R, ffFixed, 0, F);
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
  // Restore the colors
  Surface.Picture.Bitmap.Canvas.Pen.Color := PColour;
  Surface.Picture.Bitmap.Canvas.Brush.Color := BColour;
end;

{ Reset drawing surface: Draw grid and axes }

procedure DrawingSurfaceReset(Surface: TImage; W, H, L: Integer);

var
  I: Integer;

begin
  // Drawing surface as white rectancle (with black border)
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
  // Horizontal gridline and x-axis
  for I := 1 to H - 1 do begin
    if I = H div 2 then                                                        // x-axis
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(1, I * L, W * L, I * L);
  end;
  DrawArrow(Surface, (W div 2) * L, (H div 2) * L, W * L, (H div 2) * L, clBlack);
  // Vertical gridline and y-axis
  for I := 1 to W - 1 do begin
    if I = W div 2 then                                                        // y-axis
      Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack
    else
      Surface.Picture.Bitmap.Canvas.Pen.Color := clMedGray;
    Surface.Picture.Bitmap.Canvas.Line(I * L, 1, I * L, H * L);
  end;
  DrawArrow(Surface, (W div 2) * L, (H div 2) * L, (W div 2) * L, 0, clBlack);
end;

{ Get translation parameters from form (user input) }

procedure ReadTranslation(AEdit, BEdit: TEdit; out A, B: Real; out Mess: string);

begin
  Mess := '';
  if IsNumeric(AEdit.Text) and IsNumeric(BEdit.Text) then begin
    A := StrToFloat(AEdit.Text);
    B := StrToFloat(BEdit.Text);
  end
  else begin
    if not IsNumeric(AEdit.Text) then begin
      Mess := 'Glissement horizontal de la translation absent ou non numérique';
      AEdit.SetFocus;
    end
    else begin
      Mess := 'Glissement vertical de la translation absent ou non numérique';;
      BEdit.SetFocus;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Données invalides"', Mess + '!', mtError, [mbOK], 0);
end;

{ Get rotation parameters from form (user input) }

procedure ReadRotation(X0Edit, Y0Edit: TEdit; ThetaCob: TComboBox; out Center: TPoint; out Theta: Real; out Mess: string);

begin
  Mess := '';
  if IsNumeric(X0Edit.Text) and IsNumeric(Y0Edit.Text) then begin
    Center.X := StrToFloat(X0Edit.Text);
    Center.Y := StrToFloat(Y0Edit.Text);
  end
  else begin
    if not IsNumeric(X0Edit.Text) then begin
      Mess := 'Coordonnée X du centre de rotation absente ou non numérique';
      X0Edit.SetFocus;
    end
    else begin
      Mess := 'Coordonnée Y du centre de rotation absente ou non numérique';
      Y0Edit.SetFocus;
    end;
  end;
  if Mess = '' then
    Theta := StrToFloat(Trim(StringReplace(ThetaCob.Text, '°', '', [])))
  else
    MessageDlg('Données invalides"', Mess + '!', mtError, [mbOK], 0);
end;

{ Get reflexion parameters from form (user input) }

procedure ReadReflexion(AxisCob: TComboBox; out Axis: string; out Mess: string);

begin
  Mess := '';
  Axis := AxisCob.Text;
  Axis := StringReplace(Axis, 'axe des ', '', []);                             // 'abscisses' and 'ordonnées'
  Axis := StringReplace(Axis, ' q1 et q3', '1', []);                           // 'bissectrice1'
  Axis := StringReplace(Axis, ' q2 et q4', '2', []);                           // 'bissectrice2'
end;

{ Get homothety parameters from form (user input) }

procedure ReadHomothety(X0Edit, Y0Edit, KEdit: TEdit; out Center: TPoint; out K: Real; out Mess: string);

begin
  Mess := '';
  if IsNumeric(X0Edit.Text) and IsNumeric(Y0Edit.Text) then begin
    Center.X := StrToFloat(X0Edit.Text);
    Center.Y := StrToFloat(Y0Edit.Text);
  end
  else begin
    if not IsNumeric(X0Edit.Text) then begin
      Mess := 'Coordonnée X du centre d''homothétie absente ou non numérique';
      X0Edit.SetFocus;
    end
    else begin
      Mess := 'Coordonnée Y du centre d''homothétie absente ou non numérique';
      Y0Edit.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if IsNumeric(KEdit.Text) and (StrToFloat(KEdit.Text) <> 0) then
      K := StrToFloat(KEdit.Text)
    else begin
      if not IsNumeric(KEdit.Text) then
        Mess := 'Rapport d''homothétie absent ou non numérique'
      else begin
        Mess := 'Rapport d''homothétie égal à zéro invalide';
        KEdit.SetFocus;
      end;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Données invalides"', Mess + '!', mtError, [mbOK], 0);
end;

{ Get object's point coordinates from form (user input) }

procedure ReadPoints(var PointsEdit: TPointsEditFields; out Points: TPoints; out Mess: string);

var
  I, J: Integer;

begin
  Mess := ''; SetLength(Points, 0);
  for I := 0 to 3 do begin
    if PointsEdit[I].FieldX.Visible then begin
      // Fields for not considered points are not visible
      if IsNumeric(PointsEdit[I].FieldX.Text) and IsNumeric(PointsEdit[I].FieldY.Text) then begin
        // Add point coordinates to array
        SetLength(Points, I + 1);
        Points[I].X := StrToFloat(PointsEdit[I].FieldX.Text);
        Points[I].Y := StrToFloat(PointsEdit[I].FieldY.Text);
      end
      else begin
        if Mess = '' then begin
           if not IsNumeric(PointsEdit[I].FieldX.Text) then begin
            Mess := 'Coordonnée X du point ' + Chr(Ord('A') + I) + ' absente ou non numérique';
            PointsEdit[I].FieldX.SetFocus;
          end
          else begin
            Mess := 'Coordonnée Y du point ' + Chr(Ord('A') + I) + ' absente ou non numérique';
            PointsEdit[I].FieldY.SetFocus;
          end;
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    // Check if all points are unique (have different coordinates)
    for I := 0 to Length(Points) - 2 do begin
      for J := I + 1 to Length(Points) - 1 do begin
        if (Points[J].X = Points[I].X) and (Points[J].Y = Points[I].Y) then begin
          if Mess = '' then begin
            Mess := 'Un ou plusieurs des points du ' + fGeo3.cobObject.Text + ' sont confondus';
          end;
        end;
      end;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Données invalides"', Mess + '!', mtError, [mbOK], 0);
end;

{ Draw object (point, segment, triangle, tetragon) defined by points passed as arguments }

procedure DrawObject(Surface: TImage; W, H, L: Integer; var Points: TPoints; Colour: TColor);

var
  CX, CY, DX, DY, DAX, DAY, DBX, DBY, I: Integer;
  AX, AY, BX, BY, M, P: Real;
  POrd: Boolean;

begin
  Surface.Canvas.Pen.Width := 2;
  Surface.Canvas.Brush.Color := Colour; Surface.Canvas.Pen.Color := Colour;
  CX := W * L div 2; CY := H * L div 2;                                        // position of x- and y-axis
  // For each point (defining the object), draw the point and a line from it to the next point
  for I := 1 to Length(Points) do begin
    AX := Points[I - 1].X; AY := Points[I - 1].Y;
    DAX := Round(CX + AX * L);                                                 // x-coordinate on the graph
    DAY := Round(CY - AY * L);                                                 // y-coordinate on the graph
    Surface.Canvas.EllipseC(DAX, DAY, 4, 4);                                   // draw point as colored circle
    if I < Length(Points) then begin
      // Next point is next element in array
      BX := Points[I].X; BY := Points[I].Y;
    end
    else begin
      // For last point, next point is first element in array
      BX := Points[0].X; BY := Points[0].Y;
    end;
    DBX := Round(CX + BX * L);                                                 // x-coordinate on the graph
    DBY := Round(CY - BY * L);                                                 // y-coordinate on the graph
    // Determine equation parameters of line passing through the 2 points
    if DAX = DBX then begin
      // Line parallel to y-axis
      POrd := True;
    end
    else begin
      // Normal case
      POrd := False;
      M := (DBY - DAY) / (DBX - DAX);
      P := DAY - M * DAX;
    end;
    // Draw line from actual to next point
    if POrd then begin
      // Line parallel to y-axis (equation: x = c)
      if DBY < DAY then begin
        DX := DAX; DAX := DBX; DBX := DX;
        DY := DAY; DAY := DBY; DBY := DY;
      end;
      Surface.Canvas.MoveTo(DAX, DAY);
      for DY := DAY to DBY do begin
        Surface.Canvas.LineTo(DAX, DY);                                        // draw the line (point after point)
      end;
    end
    else begin
      // "Normal case line" (equation: y = mx + p)
      if DBX < DAX then begin
        DX := DAX; DAX := DBX; DBX := DX;
        DY := DAY; DAY := DBY; DBY := DY;
      end;
      Surface.Canvas.MoveTo(DAX, DAY);
      for DX := DAX to DBX do begin
        DY := Round(M * DX + P);                                               // y-coordinate for actual x, given by equation
        Surface.Canvas.LineTo(DX, DY);                                         // draw the line (point after point)
      end;
    end;
  end;
end;

{ Determine image of an object (of its defining points) by a given translation }

function Translation(A, B: Real; Points: TPoints): TPoints;

var
  I: Integer;
  Points2: TPoints;

begin
  SetLength(Points2, Length(Points));
  for I := 0 to Length(Points) - 1 do begin
    Points2[I].X := Points[I].X + A;
    Points2[I].Y := Points[I].Y + B;
  end;
  Result := Points2;
end;

{ Determine image of an object (of its defining points) by a given rotation }

function Rotation(Center: TPoint; Theta: Real; Points: TPoints): TPoints;

// Only rotations with particular angles considered!

var
  I: Integer;
  A, B: Real;
  Points2: TPoints;

begin
  SetLength(Points2, Length(Points));
  // Center of rotation <> O(0,0): Do translation (to get rotation center at O) first
  if (Center.X <> 0) or (Center.Y <> 0) then begin
    A := -Center.X; B := -Center.Y;
    Points := Translation(A, B, Points);
  end;
  // Do the rotation
  for I := 0 to Length(Points) - 1 do begin
    if (Theta = 90) or (Theta = -270) then begin
      Points2[I].X := -Points[I].Y;
      Points2[I].Y := Points[I].X;
    end
    else if (Theta = 180) or (Theta = -180) then begin
      Points2[I].X := -Points[I].X;
      Points2[I].Y := -Points[I].Y;
    end
    else if (Theta = 270) or (Theta = -90) then begin
      Points2[I].X := Points[I].Y;
      Points2[I].Y := -Points[I].X;
    end;
  end;
  // Center of rotation <> O(0,0): Do translation (opposite directions as translation before)
  if (Center.X <> 0) or (Center.Y <> 0) then begin
    Points2 := Translation(-A, -B, Points2);
  end;
  Result := Points2;
end;

{ Determine image of an object (of its defining points) by a given reflexion }

function Reflexion(Axis: string; Points: TPoints): TPoints;

// Only reflexions with particular axes considered!

var
  I: Integer;
  Points2: TPoints;

begin
  SetLength(Points2, Length(Points));
  // Do the reflexion
  for I := 0 to Length(Points) - 1 do begin
    if (Axis = 'abscisses') then begin
      Points2[I].X := Points[I].X;
      Points2[I].Y := -Points[I].Y;
    end
    else if (Axis = 'ordonnées') then begin
      Points2[I].X := -Points[I].X;
      Points2[I].Y := Points[I].Y;
    end
    else if (Axis = 'bissectrice1') then begin
      Points2[I].X := Points[I].Y;
      Points2[I].Y := Points[I].X;
    end
    else if (Axis = 'bissectrice2') then begin
      Points2[I].X := -Points[I].Y;
      Points2[I].Y := -Points[I].X;
    end;
  end;
  Result := Points2;
end;

{ Determine image of an object (of its defining points) by a given homothety }

function Homothety(Center: TPoint; K: Real; Points: TPoints): TPoints;

// Only homothetys with center O(0,0) are considered!

var
  I: Integer;
  Points2: TPoints;

begin
  SetLength(Points2, Length(Points));
  for I := 0 to Length(Points) - 1 do begin
    Points2[I].X := K * Points[I].X;
    Points2[I].Y := K * Points[I].Y;
  end;
  Result := Points2;
end;

{********}
{ TfGeo3 }
{********}

{ Application start: Initialisation }

procedure TfGeo3.FormCreate(Sender: TObject);

const
  SUB_0 = #$E2#$82#$80; SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82;
  SUB_3 = #$E2#$82#$83; SUB_4 = #$E2#$82#$84;

begin
  // Create bitmap for canvas drawing
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  // Apply subscripts
  laRotCentre.Caption := StringReplace(laRotCentre.Caption, '0', SUB_0, [rfReplaceAll]);
  laHomCentre.Caption := StringReplace(laHomCentre.Caption, '0', SUB_0, [rfReplaceAll]);
  laPtA.Caption := StringReplace(laPtA.Caption, '1', SUB_1, [rfReplaceAll]);
  laPtB.Caption := StringReplace(laPtB.Caption, '2', SUB_2, [rfReplaceAll]);
  laPtC.Caption := StringReplace(laPtC.Caption, '3', SUB_3, [rfReplaceAll]);
  laPtD.Caption := StringReplace(laPtD.Caption, '4', SUB_4, [rfReplaceAll]);
  // Create arrays with points edit fields
  edPoints[0].FieldX := edX1; edPoints[0].FieldY := edY1;
  edPoints[1].FieldX := edX2; edPoints[1].FieldY := edY2;
  edPoints[2].FieldX := edX3; edPoints[2].FieldY := edY3;
  edPoints[3].FieldX := edX4; edPoints[3].FieldY := edY4;
  edPoints2[0].FieldX := edTX1; edPoints2[0].FieldY := edTY1;
  edPoints2[1].FieldX := edTX2; edPoints2[1].FieldY := edTY2;
  edPoints2[2].FieldX := edTX3; edPoints2[2].FieldY := edTY3;
  edPoints2[3].FieldX := edTX4; edPoints2[3].FieldY := edTY4;
  // Reset drawing surface
  iPixles := 20;
  iFieldSize := iPixles; iWidthFields := WidthFields div (iPixles div FieldSize); iHeightFields := HeightFields div (iPixles div FieldSize);
  DrawingSurfaceReset(imDraw, iWidthFields, iHeightFields, iFieldSize);
end;

{ Menu item "Fichier > Quitter": Exit application }

procedure TfGeo3.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Zoom > ...": Select field-size (number of pixels per unit) for drawing }

procedure TfGeo3.mOptionsZoom10Click(Sender: TObject);

begin
  mOptionsZoom10.Checked := True; mOptionsZoom20.Checked := False; mOptionsZoom40.Checked := False;
  iPixles := 10;
end;

procedure TfGeo3.mOptionsZoom20Click(Sender: TObject);

begin
  mOptionsZoom10.Checked := False; mOptionsZoom20.Checked := True; mOptionsZoom40.Checked := False;
  iPixles := 20;
end;

procedure TfGeo3.mOptionsZoom40Click(Sender: TObject);

begin
  mOptionsZoom10.Checked := False; mOptionsZoom20.Checked := False; mOptionsZoom40.Checked := True;
  iPixles := 40;
end;

{ Menu item "Aide > Aide géométrie": Display geometry help text }

procedure TfGeo3.mHelpGeometryClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else begin
    fHelp.edHelp.Lines.Clear;
    fHelp.edHelp.Lines.LoadFromFile('geometry.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Aide > Info programme": Display application about }

procedure TfGeo3.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathématiques.' + LineEnding;
  S += 'Transformations géométriques: translations, rotations simples, réflexions simples, homothéties simples.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, décembre 2020 - mars 2021.';
  MessageDlg('Info "Geometry3"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calcul": Do the geometric transformation of the object defined by the points entered by the user }

procedure TfGeo3.btComputeClick(Sender: TObject);

var
  DCX, DCY, I: Integer;
  Mess: string;

begin
  iFieldSize := iPixles;
  iWidthFields := WidthFields div (iPixles div FieldSize);
  iHeightFields := HeightFields div (iPixles div FieldSize);
  DrawingSurfaceReset(imDraw, iWidthFields, iHeightFields, iFieldSize);
  // Read transformation parameters from form (user input)
  if rbTranslation.Checked then
    ReadTranslation(edTransA, edTransB, rTransA, rTransB, Mess)
  else if rbRotation.Checked then
    ReadRotation(edRotX0, edRotY0, cobRotTheta, rRotCenter, rRotTheta, Mess)
  else if rbReflexion.Checked then
    ReadReflexion(cobReflAxis, sReflAxis, Mess)
  else
    ReadHomothety(edHomX0, edHomY0, edHomK, rHomCenter, rHomK, Mess);
  if Mess = '' then begin
    // Read the point coordinates
    ReadPoints(edPoints, aPoints, Mess);
    if Mess = '' then begin
      // Draw the object to be transformed (in green)
      DrawObject(imDraw, iWidthFields, iHeightFields, iFieldSize, aPoints, clGreen);
      // Determine the image of the points by the actual geometric transformation
      if rbTranslation.Checked then
        aPoints2 := Translation(rTransA, rTransB, aPoints)
      else if rbRotation.Checked then
        aPoints2 := Rotation(rRotCenter, rRotTheta, aPoints)
      else if rbReflexion.Checked then
        aPoints2 := Reflexion(sReflAxis, aPoints)
      else
        aPoints2 := Homothety(rHomCenter, rHomK, aPoints);
      // Display the image point coordinates
      for I := 0 to Length(aPoints2) - 1 do begin
        edPoints2[I].FieldX.Text := RFormat(aPoints2[I].X);
        edPoints2[I].FieldY.Text := RFormat(aPoints2[I].Y);
      end;
      // For a rotation or a homothety, draw the center of rotation/homothety point
      if rbRotation.Checked or rbHomothety.Checked then begin
        imDraw.Canvas.Pen.Width := 2;
        imDraw.Canvas.Brush.Color := clRed; imDraw.Canvas.Pen.Color := clRed;
        if rbRotation.Checked then begin
          DCX := Round((iWidthFields * iFieldSize div 2) + rRotCenter.X * iFieldSize);
          DCY := Round((iHeightFields * iFieldSize div 2) - rRotCenter.Y * iFieldSize);
        end
        else begin
          DCX := Round((iWidthFields * iFieldSize div 2) + rHomCenter.X * iFieldSize);
          DCY := Round((iHeightFields * iFieldSize div 2) - rHomCenter.Y * iFieldSize);
        end;
        imDraw.Canvas.EllipseC(DCX, DCY, 4, 4);
      end
      // For a reflexion, draw the reflexion axis
      else if rbReflexion.Checked then begin
        imDraw.Canvas.Pen.Width := 2;
        imDraw.Canvas.Pen.Color := clRed;
        if sReflAxis = 'abscisses' then
          imDraw.Canvas.Line(0, imDraw.Height div 2, imDraw.Width - 10, imDraw.Height div 2)
        else if sReflAxis = 'ordonnées' then
          imDraw.Canvas.Line(imDraw.Width div 2, 0, imDraw.Width div 2, imDraw.Height)
        else if sReflAxis = 'bissectrice1' then
          imDraw.Canvas.Line(imDraw.Width div 2 - imDraw.Height div 2, imDraw.Height, imDraw.Width div 2 + imDraw.Height div 2, 0)
        else if sReflAxis = 'bissectrice2' then
          imDraw.Canvas.Line(imDraw.Width div 2 + imDraw.Height div 2, imDraw.Height, imDraw.Width div 2 - imDraw.Height div 2, 0);
      end;
      // Draw the image of the object
      DrawObject(imDraw, iWidthFields, iHeightFields, iFieldSize, aPoints2, clBlue);
    end;
  end;
end;

{ Translation/rotation/reflexion/homothety selection (radiobuttons): Adapt form controls accordingly }

procedure TfGeo3.rbTranslationChange(Sender: TObject);

begin
  if rbTranslation.Checked then begin
    laTransformation.Caption := 'Translation';
    edTransA.TabStop := True; edTransB.TabStop := True;
    edRotX0.TabStop := False; edRotY0.TabStop := False;
    edHomK.TabStop := False;
    edTransA.SetFocus;
  end;
end;

procedure TfGeo3.rbRotationChange(Sender: TObject);

begin
  if rbRotation.Checked then begin
    laTransformation.Caption := 'Rotation';
    edTransA.TabStop := False; edTransB.TabStop := False;
    edRotX0.TabStop := True; edRotY0.TabStop := True;
    edHomK.TabStop := False;
    edRotX0.SetFocus;
  end;
end;

procedure TfGeo3.rbReflexionChange(Sender: TObject);

begin
  if rbReflexion.Checked then begin
    laTransformation.Caption := 'Réflexion';
    edTransA.TabStop := False; edTransB.TabStop := False;
    edRotX0.TabStop := False; edRotY0.TabStop := False;
    edHomK.TabStop := False;
    cobReflAxis.SetFocus;
  end;
end;

procedure TfGeo3.rbHomothetyChange(Sender: TObject);

begin
  if rbHomothety.Checked then begin
    laTransformation.Caption := 'Homothétie';
    edTransA.TabStop := False; edTransB.TabStop := False;
    edRotX0.TabStop := False; edRotY0.TabStop := False;
    edHomK.TabStop := True;
    edHomK.SetFocus;
  end;
end;

{ Object selection (combobox changes): Display/hide points labels/edit fields as appropriate }

procedure TfGeo3.cobObjectChange(Sender: TObject);

var
  I: Integer;

begin
  laPtB.Visible := False; laPtC.Visible := False; laPtD.Visible := False;
  for I := 0 to 3 do begin
    if I > 0 then begin
      edPoints[I].FieldX.Visible := False; edPoints[I].FieldY.Visible := False;
      edPoints2[I].FieldX.Visible := False; edPoints2[I].FieldY.Visible := False;
    end;
    edPoints2[I].FieldX.Text := ''; edPoints2[I].FieldY.Text := '';
  end;
  if cobObject.ItemIndex > 0 then
    laPtB.Visible := True;
  if cobObject.ItemIndex > 1 then
    laPtC.Visible := True;
  if cobObject.ItemIndex > 2 then
    laPtD.Visible := True;
  for I := 1 to 3 do begin
    if cobObject.ItemIndex >= I then begin
      edPoints[I].FieldX.Visible := True; edPoints[I].FieldY.Visible := True;
      edPoints2[I].FieldX.Visible := True; edPoints2[I].FieldY.Visible := True;
    end;
  end;
end;

end.

