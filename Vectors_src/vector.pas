{*************************************}
{* Main unit for Vectors application *}
{*************************************}

unit vector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, help;

type
  // Usual vector representation with x and y component
  TVector = record
    X, Y: Integer;
  end;
  // Drawing vector with origin and endpoint
  TVector2 = record
    VStart, VEnd: TVector;
  end;
  {***********}
  { TfVectors }
  {***********}
  TfVectors = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsOperations, mOptionsOperationsAdd, mOptionsOperationsSub: TMenuItem;
    mOptionsVectors, mOptionsVectors2, mOptionsVectors3, mOptionsVectors4, mOptionsDrawingOnly: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imDraw: TImage;
    Label2, Label3: TLabel;
    StaticText1, StaticText2, StaticText3, StaticText4, StaticText5, StaticText6: TStaticText;
    stVectorC, stVectorD, stOperation: TStaticText;
    edVectorA, edVectorB, edVectorC, edVectorD, edVectorR: TEdit;
    edOperation1, edOperation2, edOperation3: TEdit;
    edMagnitude, edAngle: TEdit;
    imEval: TImage;
    Shape1, Shape2, Shape3, Shape6: TShape;
    shVectorC, shVectorD: TShape;
    shVStart: TShape;
    btQuestion: TButton;
    btShow: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsOperationsAddClick(Sender: TObject);
    procedure mOptionsOperationsSubClick(Sender: TObject);
    procedure mOptionsVectors2Click(Sender: TObject);
    procedure mOptionsVectors3Click(Sender: TObject);
    procedure mOptionsVectors4Click(Sender: TObject);
    procedure mOptionsDrawingOnlyClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure imDrawMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    iVectors, iVector: Integer;
    bVectorStart: Boolean;
    bOperators: array[0..1] of Boolean;
    vA, vB, vC, vD, vR1, vR2, vR: TVector;
    aVectors, aUVectors, aPixelVectors: array[1..5] of TVector2;
    bmDraw: TBitmap;
  end;

const
  WidthFields  = 30;                                                           // drawing surface width = 30 fields
  HeightFields = 16;                                                           // drawing surface height = 16 fields
  FieldSize    = 40;                                                           // pixelsize of 1 field
  Colors: array[1..4] of TColor = (                                            // colors for the 4 vectors (resultant vector will be red)
    clNavy, clBlue, clGreen, clLime
  );

var
  fVectors: TfVectors;

implementation

{$R *.lfm}

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

{ Reset all form controls (depending on number of vectors to be used) }

procedure ResetForm(Surface: TImage; W, H, L, N: Integer; out V: Integer; out VStart: Boolean);

begin
  fVectors.edVectorA.Text := ''; fVectors.edVectorB.Text := ''; fVectors.edVectorC.Text := ''; fVectors.edVectorD.Text := '';
  fVectors.edOperation1.Text := ''; fVectors.edOperation2.Text := ''; fVectors.edOperation3.Text := '';
  fVectors.edVectorR.Text := ''; fVectors.edMagnitude.Text := ''; fVectors.edAngle.Text := '';
  fVectors.imEval.Picture.Clear;
  if N = 2 then begin
    // 2 vectors
    fVectors.shVectorC.Brush.Color := Colors[3]; fVectors.shVectorC.Brush.Style := bsBDiagonal;
    fVectors.shVectorD.Brush.Color := Colors[4]; fVectors.shVectorD.Brush.Style := bsBDiagonal;
    fVectors.stVectorC.Enabled := False; fVectors.edVectorC.Enabled := False;
    fVectors.stVectorD.Enabled := False; fVectors.edVectorD.Enabled := False;
    fVectors.edOperation2.Enabled := False; fVectors.edOperation3.Enabled := False;
  end
  else if N = 3 then begin
    // 3 vectors
    fVectors.shVectorC.Brush.Color := Colors[3]; fVectors.shVectorC.Brush.Style := bsSolid;
    fVectors.shVectorD.Brush.Color := Colors[4]; fVectors.shVectorD.Brush.Style := bsBDiagonal;
    fVectors.stVectorC.Enabled := True; fVectors.edVectorC.Enabled := True;
    fVectors.stVectorD.Enabled := False; fVectors.edVectorD.Enabled := False;
    fVectors.edOperation2.Enabled := True; fVectors.edOperation3.Enabled := False;
  end
  else begin
    // 4 vectors
    fVectors.shVectorC.Brush.Color := Colors[3]; fVectors.shVectorC.Brush.Style := bsSolid;
    fVectors.shVectorD.Brush.Color := Colors[4]; fVectors.shVectorD.Brush.Style := bsSolid;
    fVectors.stVectorC.Enabled := True; fVectors.edVectorC.Enabled := True;
    fVectors.stVectorD.Enabled := True; fVectors.edVectorD.Enabled := True;
    fVectors.edOperation2.Enabled := True; fVectors.edOperation3.Enabled := True;
  end;
  DrawingSurfaceClear(Surface, W, H, L);                                       // clean drawing surface
  V := 1; VStart := True;                                                      // first click on drawing surface will be origin of 1st vector
  fVectors.shVStart.Visible := False;
  // Display vector operation ('+' sign will be replaced later if operation = subtraction)
  fVectors.stOperation.Caption := '→     →     →';
  if N >= 3 then begin
    fVectors.stOperation.Caption := fVectors.stOperation.Caption + '     →';
    if N = 4 then
      fVectors.stOperation.Caption := fVectors.stOperation.Caption + '     →';
  end;
  fVectors.stOperation.Caption := fVectors.stOperation.Caption + LineEnding;
  fVectors.stOperation.Caption := fVectors.stOperation.Caption + 'R  =  A  +  B';
  if N >= 3 then begin
    fVectors.stOperation.Caption := fVectors.stOperation.Caption + '  +  C';
    if N = 4 then
      fVectors.stOperation.Caption := fVectors.stOperation.Caption + '  +  D';
  end;
  // Magnitude and angle edit fields properties (depending on user selection)
  if fVectors.mOptionsDrawingOnly.Checked then begin
    fVectors.edMagnitude.ReadOnly := True; fVectors.edMagnitude.TabStop := False;
    fVectors.edAngle.ReadOnly := True; fVectors.edAngle.TabStop := False;
  end
  else begin
    fVectors.edMagnitude.ReadOnly := False; fVectors.edMagnitude.TabStop := True;
    fVectors.edAngle.ReadOnly := False; fVectors.edAngle.TabStop := True;
  end;
  // First button click will be to generate a question
  fVectors.btQuestion.Caption := 'Question';
end;

{ Generate a random vector }

function RandomVector(N: Integer): TVector;

var
  XMax, YMax: Integer;
  V: TVector;

begin
  case N of
    // Arbitrary max. magnitude values for drawing surface size = 30x16
    2: begin XMax := 15; YMax := 8; end;
    3: begin XMax := 12; YMax := 6; end;
    4: begin XMax := 10; YMax := 5; end;
  end;
  repeat
    V.X := Random(2 * XMax + 1) - XMax; V.Y := Random(2 * YMax + 1) - YMax;
  until (V.X <> 0) or (V.Y <> 0);                                              // avoid null vector
  Result := V;
end;

{ Do vector addition or subtraction }

function VectorOperation(V1, V2: TVector; Operation: Char): TVector;

var
  V: TVector;

begin
  if Operation = '+' then begin
    V.X := V1.X + V2.X;
    V.Y := V1.Y + V2.Y;
  end
  else begin
    V.X := V1.X - V2.X;
    V.Y := V1.Y - V2.Y;
  end;
  Result := V;
end;

{ Display vector, using format: ( a , b ) }

function VectorDisplay(V: TVector): string;

begin
  Result := '( ' + IntToStr(V.X) + ', ' + IntToStr(V.Y) + ' )';
end;

{ Check if 2 display vectors are equal }

function VectorsEqual(V1, V2: TVector2): Boolean;

var
  VEqual: Boolean;

begin
  VEqual := True;
  if (V1.VStart.X <> V2.VStart.X) or (V1.VStart.Y <> V2.VStart.Y) then
    VEqual := False
  else if (V1.VEnd.X <> V2.VEnd.X) or (V1.VEnd.Y <> V2.VEnd.Y) then
    VEqual := False;
  Result := VEqual;
end;

{ Calculate vector magnitude and angle }

procedure GetVectorValues(Vector: TVector; out Magnitude, Angle: Real);

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
{ TfVectors }
{***********}

{ Application start: Initialisation }

procedure TfVectors.FormCreate(Sender: TObject);

begin
  // Create bitmap for canvas drawing
  bmDraw := TBitmap.Create;
  bmDraw.Width  := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  imDraw.Picture.Graphic := bmDraw;
  // Exercise startup values
  iVectors := 2; bOperators[0] := True; bOperators[1] := True;
  // Start random generator
  Randomize;
  // Reset form controls (for 2 vectors exercise)
  ResetForm(imDraw, WidthFields, HeightFields, FieldSize, iVectors, iVector, bVectorStart);
end;

{ Menu item "File > Exit": Exit application }

procedure TfVectors.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Vector operations > ...": Select vector operations to be used in exercises }

procedure TfVectors.mOptionsOperationsAddClick(Sender: TObject);

begin
  if mOptionsOperationsAdd.Checked then
    mOptionsOperationsAdd.Checked := False
  else
    mOptionsOperationsAdd.Checked := True;
  bOperators[0] := mOptionsOperationsAdd.Checked;
end;

procedure TfVectors.mOptionsOperationsSubClick(Sender: TObject);

begin
  if mOptionsOperationsSub.Checked then
    mOptionsOperationsSub.Checked := False
  else
    mOptionsOperationsSub.Checked := True;
  bOperators[1] := mOptionsOperationsSub.Checked;
end;

{ Menu items "Options > Number of vectors > ...": Select number of vectors to be used in exercises }

procedure TfVectors.mOptionsVectors2Click(Sender: TObject);

begin
  mOptionsVectors2.Checked := True;
  mOptionsVectors3.Checked := False;
  mOptionsVectors4.Checked := False;
  iVectors := 2;
end;

procedure TfVectors.mOptionsVectors3Click(Sender: TObject);

begin
  mOptionsVectors2.Checked := False;
  mOptionsVectors3.Checked := True;
  mOptionsVectors4.Checked := False;
  iVectors := 3;
end;

procedure TfVectors.mOptionsVectors4Click(Sender: TObject);

begin
  mOptionsVectors2.Checked := False;
  mOptionsVectors3.Checked := False;
  mOptionsVectors4.Checked := True;
  iVectors := 4;
end;

{ Menu item "Options > Don't evaluate magnitude and angle": Toggle exercise evaluation option }

procedure TfVectors.mOptionsDrawingOnlyClick(Sender: TObject);

begin
  if mOptionsDrawingOnly.Checked then
    mOptionsDrawingOnly.Checked := False
  else
    mOptionsDrawingOnly.Checked := True;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfVectors.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfVectors.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Geometry:' + LineEnding;
  S += 'Graphical addition and subtraction of vectors.' + LineEnding + LineEnding;
  S += 'Version 1.0.1, © allu, January 2020 - April 2023.';
  MessageDlg('About "Vectors"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new question resp. check user answer }

procedure TfVectors.btQuestionClick(Sender: TObject);

const
  AllOperators: array[0..1] of Char = (
    '+', '-'
  );

var
  I, J: Integer;
  R, UR, Theta, UTheta: Real;
  VectorR, UVectorR: string;
  OK: Boolean;
  Operators: array[0..2] of Char;

begin
  btShow.Enabled := True;                                                    // button was enabled at application start, as there is nothing to show then
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    // Proceed only if at leat one operation is selected
    if bOperators[0] or bOperators[1] then begin
      ResetForm(imDraw, WidthFields, HeightFields, FieldSize, iVectors, iVector, bVectorStart);
      // Random operators (among those selected by the user)
      for I := 0 to 2 do begin
        repeat
          J := Random(2);
        until bOperators[J] = True;
        Operators[I] := AllOperators[J];
      end;
      // Display correct vector operation (replacing defaut '+' by '-' if necessary)
      stOperation.Caption := StringReplace(stOperation.Caption, 'A  +  B', 'A  ' + Operators[0] + '  B', []);
      stOperation.Caption := StringReplace(stOperation.Caption, 'B  +  C', 'B  ' + Operators[1] + '  C', []);
      stOperation.Caption := StringReplace(stOperation.Caption, 'C  +  D', 'C  ' + Operators[2] + '  D', []);
      // Random vectors (with some constraints, see below...)
      repeat
        OK := True;
        // 1st and 2nd vector
        vA := RandomVector(iVectors); vB := RandomVector(iVectors);
        vR1 := VectorOperation(vA, vB, Operators[0]);
        aVectors[1].VStart.X := 0; aVectors[1].VStart.Y := 0; aVectors[1].VEnd := vA;
        aVectors[2].VStart := vA; aVectors[2].VEnd := vR1;
        if mOptionsVectors2.Checked then begin
          vR2.X := 0;
          vR2.Y := 0;
          vR := vR1;
        end
        else begin
          // 3rd vector
          vC := RandomVector(iVectors);
          vR2 := VectorOperation(vR1, vC, Operators[1]);
          aVectors[3].VStart := vR1; aVectors[3].VEnd := vR2;
          if mOptionsVectors3.Checked then
            vR := vR2
          else begin
            // 4th vector
            vD := RandomVector(iVectors);
            vR := VectorOperation(vR2, vD, Operators[2]);
            aVectors[4].VStart := vR2; aVectors[4].VEnd := vR;
          end;
        end;
        // Check if vectors generated obey to contraints
        if (vR.X = 0) and (vR.Y = 0) then
          // Avoid a null vector resultant
          OK := False
        else begin
          aVectors[iVectors + 1].VStart.X := 0; aVectors[iVectors + 1].VStart.Y := 0; aVectors[iVectors + 1].VEnd := vR;
          // All vectors have to fit onto the drawing surface
          if (vR1.X < -WidthFields div 2) or (vR2.X < -WidthFields div 2) or (vR.X < -WidthFields div 2) then
            OK := False
          else if (vR1.X > WidthFields div 2) or (vR2.X > WidthFields div 2) or (vR.X > WidthFields div 2) then
            OK := False
          else if (vR1.Y < -HeightFields div 2) or (vR2.Y < -HeightFields div 2) or (vR.Y < -HeightFields div 2) then
            OK := False
          else if (vR1.Y > HeightFields div 2) or (vR2.Y > HeightFields div 2) or (vR.Y > HeightFields div 2) then
            OK := False;
        end;
      until OK;
      // Display the vectors and the operators
      edVectorA.Text := VectorDisplay(vA); edVectorB.Text := VectorDisplay(vB);
      edOperation1.Text := Operators[0];
      if mOptionsVectors3.Checked or mOptionsVectors4.Checked then begin
        edVectorC.Text := VectorDisplay(vC);
        edOperation2.Text := Operators[1];
        if mOptionsVectors4.Checked then begin
          edVectorD.Text := VectorDisplay(vD);
          edOperation3.Text := Operators[2];
        end;
      end;
      // Next button push will be to check user answer
      btQuestion.Caption := 'Answer';
    end
    // Display error message if no operation selected
    else
      MessageDlg('Invalid options', 'You must at least select one vector operation!', mtError, [mbOK], 0);
  end
  // Button "Answer": Check user answer
  else begin
    OK := True;
    // Check display vectors A, B and R
    if (not VectorsEqual(aUVectors[1], aVectors[1])) or (not VectorsEqual(aUVectors[2], aVectors[2])) or (not VectorsEqual(aUVectors[iVectors + 1], aVectors[iVectors + 1])) then
      OK := False
    else if (mOptionsVectors3.Checked or mOptionsVectors4.Checked) then begin
      // Check display vector C (if 3 or 4 vectors are being used)
      if not VectorsEqual(aUVectors[3], aVectors[3]) then
        OK := False
      else if mOptionsVectors4.Checked then begin
        // Check display vector D (if 4 vectors are being used)
        if not VectorsEqual(aUVectors[4], aVectors[4]) then
          OK := False;
      end;
    end;
    // All vectors drawn are correct; now check vector values
    if OK then begin
      VectorR := VectorDisplay(vR);
      VectorR := StringReplace(VectorR, ' ', '', [rfReplaceAll]);              // eliminate spaces (for user convenience)
      UVectorR := StringReplace(edVectorR.Text, ' ', '', [rfReplaceAll]);
      // Check resultant vector value
      if UVectorR <> VectorR then
        OK := False
      else begin
        // Check magnitude and angle (if user selected to include them for evaluation)
        GetVectorValues(vR, R, Theta);
        R := Round(100 * R) / 100; Theta := Round(100 * Theta) / 100;
        if mOptionsDrawingOnly.Checked then begin
          // No evaluation: Display the values
          edMagnitude.Text := FloatToStr(R);
          edAngle.Text := FloatToStr(Theta) + '°';
        end
        else begin
          // Evaluation: Check user values
          if edMagnitude.Text = '' then
            UR := 0
          else
            UR := StrToFloat(edMagnitude.Text);
          if edAngle.Text = '' then
            UTheta := 0
          else
            UTheta := StrToFloat(edAngle.Text);
          UR := Round(100 * UR) / 100; UTheta := Round(100 * UTheta) / 100;    // Check magnitude and angle on 2 decimal digits
          if (UR <> R) or (UTheta <> Theta) then
            OK := False;
        end;
      end;
    end;
    if OK then
      // Vector drawing and all vector values entered by the user are correct
      imEval.Picture.LoadFromFile('correct.png')
    else
      // At least one error in user answers
      imEval.Picture.LoadFromFile('false.png');
    // Next button push will be to generate a new question
    btQuestion.Caption := 'Question';
  end;
end;

{ Button "Show": Show vector drawing and values }

procedure TfVectors.btShowClick(Sender: TObject);

var
  StartX, StartY, EndX, EndY, I: Integer;
  R, Theta: Real;
  Colour: TColor;

begin
  DrawingSurfaceClear(imDraw, WidthFields, HeightFields, FieldSize);           // clean drawing surface
  // Display all vectors
  for I := 1 to iVectors + 1 do begin
    if I = iVectors + 1 then
      // Resultant vector
      Colour := clRed
    else
      // Other vectors
      Colour := Colors[I];
    imDraw.Picture.Bitmap.Canvas.Pen.Color := Colour;
    imDraw.Picture.Bitmap.Canvas.Pen.Width := 3;
    imDraw.Picture.Bitmap.Canvas.Brush.Color := Colour;
    // Vector origin
    StartX := (aVectors[I].VStart.X + (WidthFields) div 2) * FieldSize;
    StartY := (HeightFields div 2 - aVectors[I].VStart.Y) * FieldSize;
    // Vector end point (with arrowhead)
    EndX := (aVectors[I].VEnd.X + (WidthFields) div 2) * FieldSize;
    EndY := (HeightFields div 2 - aVectors[I].VEnd.Y) * FieldSize;
    imDraw.Picture.Bitmap.Canvas.Line(StartX, StartY, EndX, EndY);
    DrawArrow(imDraw, StartX - 1, StartY - 1, EndX - 1, EndY - 1, Colour);
  end;
  // Display all resultant vector values
  edVectorR.Text := VectorDisplay(vR);
  GetVectorValues(vR, R, Theta);
  R := Round(100 * R) / 100; Theta := Round(100 * Theta) / 100;
  edMagnitude.Text := FloatToStr(R);
  edAngle.Text := FloatToStr(Theta) + '°';
  // Next (main) button push will be to generate a new question
  btQuestion.Caption := 'Question';
end;

{ Button "Clear": Clean drawing surface }

procedure TfVectors.btClearClick(Sender: TObject);

begin
  DrawingSurfaceClear(imDraw, WidthFields, HeightFields, FieldSize);
  // Vector values to be reset!
  shVStart.Visible := False;
  iVector := 1; bVectorStart := True;
end;

{ Mouse click on drawing surface: Mark vector origin resp. draw the vector }

procedure TfVectors.imDrawMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

// Ignore compiler hints on unused parameters; removing them may result in variable mixup (?)

// User clicks will only be accepted as valid if they are done 10 pixels around a grid line intersection!

var
  IX, IY: Integer;
  Colour: TColor;

begin
  // Proceed only if answer is awaited and if there are still vectors to be drawn
  if (btQuestion.Caption = 'Answer') and (iVector <= iVectors + 1) then begin
    // Determine the correct horizontal grid-pixelvalue for actual mouse click
    if X <= 10 then
      IX := 0
    else if X >= WidthFields * FieldSize - 10 then
      IX := WidthFields * FieldSize
    else begin
      IX := 0;
      repeat
        IX += FieldSize;
      until ((X >= IX - 10) and (X <= IX + 10)) or (IX > (WidthFields + 1) * FieldSize);
    end;
    // Determine the correct horizontal grid-pixelvalue for actual mouse click
    if IX <= WidthFields * FieldSize then begin
      if Y <= 10 then
        IY := 0
      else if Y >= WidthFields * FieldSize - 10 then
        IY := WidthFields * FieldSize
      else begin
        IY := 0;
        repeat
          IY += FieldSize;
        until ((Y >= IY - 10) and (Y <= IY + 10)) or (IY > (WidthFields + 1) * FieldSize);
      end;
    end;
    // Proceed only if the click is considered to be on a grid line intersection
    if (IX <= WidthFields * FieldSize) and (IY <= WidthFields * FieldSize) then begin
      if iVector = iVectors + 1 then
        // Resultant vector
        Colour := clRed
      else
        // Other vectors
        Colour := Colors[iVector];
      imDraw.Picture.Bitmap.Canvas.Pen.Color := Colour;
      imDraw.Picture.Bitmap.Canvas.Brush.Color := Colour;
      // First click for actual vector: Show colored shape at vector origin
      if bVectorStart then begin
        shVStart.Pen.Color := Colour; shVStart.Brush.Color := Colour;
        shVStart.Left := imDraw.Left + IX - 2; shVStart.Top := imDraw.Top + IY - 2;
        shVStart.Visible := True;
        aPixelVectors[iVector].VStart.X := IX; aPixelVectors[iVector].VStart.Y := IY;
        aUVectors[iVector].VStart.X := (IX div FieldSize) - (WidthFields div 2);
        aUVectors[iVector].VStart.Y := (HeightFields div 2) - (IY div FieldSize);
        bVectorStart := False;                                                 // next click will draw the vector
      end
      // Second click for actual vector: Display the vector (with arrowhead)
      else begin
        shVStart.Visible := False;
        aPixelVectors[iVector].VEnd.X := IX; aPixelVectors[iVector].VEnd.Y := IY;
        aUVectors[iVector].VEnd.X := (IX div FieldSize) - (WidthFields div 2);
        aUVectors[iVector].VEnd.Y := (HeightFields div 2) - (IY div FieldSize);
        imDraw.Picture.Bitmap.Canvas.Pen.Width := 3;
        imDraw.Picture.Bitmap.Canvas.Line(aPixelVectors[iVector].VStart.X, aPixelVectors[iVector].VStart.Y, IX, IY);
        DrawArrow(imDraw, aPixelVectors[iVector].VStart.X - 1, aPixelVectors[iVector].VStart.Y - 1, IX - 1, IY - 1, Colour);
        Inc(iVector);                                                          // point to next vector
        bVectorStart := True;                                                  // next click will set origin of this vector
      end;
      // All vectors drawn: Set focus for vector value entry
      if iVector = iVectors + 1 then
        edVectorR.SetFocus;
    end;
  end;
end;

end.

