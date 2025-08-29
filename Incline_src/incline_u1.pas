{*************************************}
{* Main unit for Incline application *}
{*************************************}

unit incline_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, PopupNotifier, Math, LCLIntf, incline_u2;

type
  {*************}
  {* TfIncline *}
  {*************}
  TfIncline = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsIncline, mSettingsIncline1, mSettingsIncline2, mSettingsIncline3: TMenuItem;
    mSettingsMassLbs, mSettingsSpeedKmh, mSettingsGravity980: TMenuItem;
    mHelp, mHelpPhysics, mHelpHelp, mHelpAbout: TMenuItem;
    imDSurface: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18, Label19, Label20, Label21: TLabel;
    edLength, edHeight, edDiagonal, edAngle: TEdit;
    edMass, edWeight: TEdit;
    cobFriction: TComboBox;
    edNFrictionStatic, edNFrictionKinetic: TEdit;
    edDownForce, edNormalForce, edFrictionStatic, edFrictionKinetic: TEdit;
    edAccForce, edAcceleration, edTime, edDistance, edSpeed: TEdit;
    btStart: TButton;
    btPause: TButton;
    btAutoPause: TButton;
    tiIncline: TTimer;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsIncline1Click(Sender: TObject);
    procedure mSettingsIncline2Click(Sender: TObject);
    procedure mSettingsIncline3Click(Sender: TObject);
    procedure mSettingsMassLbsClick(Sender: TObject);
    procedure mSettingsSpeedKmhClick(Sender: TObject);
    procedure mSettingsGravity980Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpPhysicsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btAutoPauseClick(Sender: TObject);
    procedure tiInclineTimer(Sender: TObject);
    procedure cobFrictionChange(Sender: TObject);
  private
    iWidth, iHeight, iLeft, iRight, iTop, iBottom, iDMax, iBoxLeft, iBoxLength, iBoxHeight: Integer;
    iDLength, iDHeight, iAutoPause, iSel: Integer;
    rLength, rHeight, rDiagonal, rAngle, rMass, rWeight, rDownForce, rNormalForce, rEqM, rEqP, rAutoPause, rAutoPause0: Real;
    rNFrictionStatic, rNFrictionKinetic, rFrictionStatic, rFrictionKinetic, rForce, rAcceleration, rTime, rDistance, rSpeed, rStep: Real;
    bAutoPauseActive, bAutoPauseDone: Boolean;
    Bitmap : TBitmap;
  end;

var
  fIncline: TfIncline;

implementation

{$R *.lfm}

{ Get equation of line passing through 2 given points }

procedure LineEquation(AX, AY, BX, BY: Real; out M, P: Real);

// Function is for lines with equation y = mx + p only!
// Please, treat lines with equation x = C in calling program!

begin
  if AX = BX then
    M := 0
  else
    M := (BY - AY) / (BX - AX);
  P := AY - M * AX;
end;

{ Get equation of line perpendicular to a given line and passing through a given point }

procedure PerpLineEquation(M, X0, Y0: Real; out MP, PP: Real);

// Function is for lines with equations y = mx + p only!
// Please, treat lines with equation x = C or y = C in calling program!

begin
  if M <> 0 then begin
    MP := -1 / M;
    PP := (X0 + M * Y0) / M;
  end;
end;

{ Return earth gravity (as selected by user) }

function Gravity: Real;

var
  G: Real;

begin
  if fIncline.mSettingsGravity980.Checked then
    G := 9.80
  else
    G := 9.81;
  Gravity := G;
end;

{ Enable/disable access to incline properties (as selected by user) }

procedure InclineFields(Sel: Integer);

begin
  fIncline.edLength.ReadOnly := False; fIncline.edLength.TabStop := True; fIncline.edLength.Color := clDefault;
  fIncline.edHeight.ReadOnly := False; fIncline.edHeight.TabStop := True; fIncline.edHeight.Color := clDefault;
  fIncline.edAngle.ReadOnly  := False; fIncline.edAngle.TabStop  := True; fIncline.edAngle.Color  := clDefault;
  case Sel of
    1: begin
         fIncline.edAngle.ReadOnly := True;
         fIncline.edAngle.TabStop  := False;
         fIncline.edAngle.Color    := clForm;
       end;
    2: begin
         fIncline.edHeight.ReadOnly := True;
         fIncline.edHeight.TabStop  := False;
         fIncline.edHeight.Color    := clForm;
       end;
    3: begin
         fIncline.edLength.ReadOnly := True;
         fIncline.edLength.TabStop  := False;
         fIncline.edLength.Color    := clForm;
       end;
  end;
end;

{ Get incline properties from form resp. calculate these values }

procedure InclineValues(var Sel: Integer; var L, H, S, A: Real);

var
  Mess: string;

// User selection determines which 2 of the incline properties are entered from the form
// and which one has to be calculated (diagonal is always calculated)

// Note that "Sel" is set to 0, if properties entered are invalid

begin
  Mess := '';
  case Sel of
    1: begin
         L := StrToFloat(fIncline.edLength.Text); H := StrToFloat(fIncline.edHeight.Text);
         if L <> 0 then begin
           A := Radtodeg(Arctan(H/L));
           fIncline.edAngle.Text := FloatToStrF(A, ffFixed, 2, 2);
         end;
         if L < 10 then
           Mess := 'Die Länge der schiefen Ebene sollte grösser als 10m sein.'
         else if (A < 5) or (A > 65) then
           Mess := 'Länge und Höhe der schiefen Ebene sollten so gewählt sein dass 5° ≤ Winkel ≤ 65°.';
       end;
    2: begin
         L := StrToFloat(fIncline.edLength.Text); A := StrToFloat(fIncline.edAngle.Text);
         if (A > 0) and (A < 90) then begin
           H := L * Tan(Degtorad(A));
           fIncline.edHeight.Text := FloatToStrF(H, ffFixed, 2, 2);
         end;
         if L < 10 then
           Mess := 'Die Länge der schiefen Ebene sollte grösser als 10m sein.'
         else if (A < 5) or (A > 65) then
           Mess := 'Der Winkel der schiefen Ebene sollte zwischen 5° und 65° sein.';
       end;
    3: begin
         H := StrToFloat(fIncline.edHeight.Text); A := StrToFloat(fIncline.edAngle.Text);
         if (A > 0) and (A < 90) then begin
           L := H / Tan(Degtorad(A));
           fIncline.edLength.Text := FloatToStrF(L, ffFixed, 2, 2);
         end;
         if (A < 5) or (A > 65) then
           Mess := 'Der Winkel der schiefen Ebene sollte zwischen 5° und 65° sein.'
         else if L < 10 then
           Mess := 'Höhe und Winkel der schiefen Ebene sollten so gewählt sein dass Länge ≥ 10m.';
       end;
  end;
  if Mess = '' then begin
    S := Sqrt(Sqr(L) + Sqr(H));
    fIncline.edDiagonal.Text := FloatToStrF(S, ffFixed, 2, 2);
  end
  else begin
    MessageDlg('Schiefe Ebene', Mess, mtError, [mbOK], 0);
    Sel := 0;
  end;
end;

{ Get friction values from predefined table }

procedure FrictionValues(out FStatic, FKinetic: Real);

// Sample static and kinetic friction values (corresponding to materials in combobox )

const
  FrictionVals: array[0..1, 0..7] of Real = (
    (-1, 0.5, 0.5, 0.6, 0.05, 0.04, 0.4, 0),
    (-1, 0.3, 0.3, 0.3, 0.03, 0.04, 0.02, 0)
  );

begin
  // Get table values
  FStatic  := FrictionVals[0, fIncline.cobFriction.ItemIndex];
  FKinetic := FrictionVals[1, fIncline.cobFriction.ItemIndex];
  if fIncline.cobFriction.ItemIndex = 0 then begin
    // User selected to enter custom values: give access to corresp. edit fields
    fIncline.edNFrictionStatic.ReadOnly := False;
    fIncline.edNFrictionStatic.TabStop := True;
    fIncline.edNFrictionStatic.Color := clDefault;
    fIncline.edNFrictionKinetic.ReadOnly := False;
    fIncline.edNFrictionKinetic.TabStop := True;
    fIncline.edNFrictionKinetic.Color := clDefault;
  end
  else begin
    // User selected to use predefined values: fill out the corresp. edit fields (set read-only)
    fIncline.edNFrictionStatic.ReadOnly := True;
    fIncline.edNFrictionStatic.TabStop := False;
    fIncline.edNFrictionStatic.Color := clForm;
    fIncline.edNFrictionKinetic.ReadOnly := True;
    fIncline.edNFrictionKinetic.TabStop := False;
    fIncline.edNFrictionKinetic.Color := clForm;
    if (FStatic = 0) and (FKinetic = 0) then begin
      // Display no values if "ignore friction" has been selected
      fIncline.edNFrictionStatic.Text := '';
      fIncline.edNFrictionKinetic.Text := '';
    end
    else begin
      fIncline.edNFrictionStatic.Text := FloatToStrF(FStatic, ffFixed, 2, 2);
      fIncline.edNFrictionKinetic.Text := FloatToStrF(FKinetic, ffFixed, 2, 2);
    end;
  end;
end;

{ Calculate and display down force and normal force }

procedure WeightForces(out Mass, Weight: Real; Angle: Real; out DownForce, NormalForce: Real);

// The routine reads the mass from the form, adapts it if it was given in lbs, calculates the weight
// and determines down force and normal force as X- and Y- components of the weight

begin
  // Read mass (m) from form (adapt if necessary)
  Mass := StrToFloat(fIncline.edMass.Text);
  if Mass > 0 then begin
    if fIncline.mSettingsMassLbs.Checked then
      Mass *= 0.453592;
    // Calculate weight: FG = m * g
    Weight := Mass * Gravity;
    // Calculate down force: FH = sin(α) * FG
    DownForce := Sin(Degtorad(Angle)) * Weight;
    // Calculate down force: FN = cos(α) * FG
    NormalForce := Cos(Degtorad(Angle)) * Weight;
    // Display weight related values
    fIncline.edWeight.Text := FloatToStrF(Weight, ffFixed, 2, 2);
    fIncline.edDownForce.Text := FloatToStrF(DownForce, ffFixed, 2, 2);
    fIncline.edNormalForce.Text := FloatToStrF(NormalForce, ffFixed, 2, 2);
  end
  else
    MessageDlg('Schiefe Ebene', 'Masse muss grösser als 0 sein!', mtError, [mbOK], 0);
end;

{ Calculate and display friction forces }

procedure FrictionForces(NormalForce: Real; out NFrictionStatic, NFrictionKinetic, FrictionStatic, FrictionKinetic: Real);

// Static and kinetic friction values are read from the form (resp. the predefined table values)

begin
  // Get friction values from table (according to combobox selection by user )
  FrictionValues(NFrictionStatic, NFrictionKinetic);
  if (NFrictionStatic = 0) and (NFrictionKinetic = 0) then begin
    // "Ignore friction" selected: friction values are 0
    FrictionStatic := 0;
    FrictionKinetic := 0;
  end
  else begin
    // Table value returned = -1 meaning: read friction value from form (as entered by user)
    if NFrictionStatic = -1 then begin
      if fIncline.edNFrictionStatic.Text = '' then
        NFrictionStatic := 0
      else
        NFrictionStatic := StrToFloat(fIncline.edNFrictionStatic.Text);
    end;
    if NFrictionKinetic = -1 then begin
        if fIncline.edNFrictionKinetic.Text = '' then
        NFrictionKinetic := 0
      else
        NFrictionKinetic := StrToFloat(fIncline.edNFrictionKinetic.Text);
    end;
    if (NFrictionStatic >= 0) and (NFrictionKinetic >= 0) and (NFrictionStatic <= 1) and (NFrictionKinetic <= 1) then begin
      if (NFrictionKinetic > NFrictionStatic) then begin
        MessageDlg('Schiefe Ebene', 'Die Gleitreibungszahl muss kleiner als die Haftreibungszahl sein!', mtError, [mbOK], 0);
        NFrictionKinetic := -1;
      end
      else begin
        // Calculate static friction: FR' = µ' * FN
        FrictionStatic := NFrictionStatic * NormalForce;
        // Calculate static friction: FR = µ * FN
        FrictionKinetic := NFrictionKinetic * NormalForce;
        // Display friction forces
        fIncline.edFrictionStatic.Text := FloatToStrF(FrictionStatic, ffFixed, 2, 2);
        fIncline.edFrictionKinetic.Text := FloatToStrF(FrictionKinetic, ffFixed, 2, 2);
      end;
    end
    else begin
      if (NFrictionStatic < 0) or (NFrictionStatic > 1) then
        MessageDlg('Schiefe Ebene', 'Die Haftreibungszahl muss zwischen 0 und 1 sein!', mtError, [mbOK], 0)
      else
        MessageDlg('Schiefe Ebene', 'Die Gleitreibungszahl muss zwischen 0 und 1 sein!', mtError, [mbOK], 0)
    end;
  end;
end;

{ Clear the drawing surface by displaying a white rectangle }

procedure DSurfaceClear(W, H: Integer);

begin
  fIncline.imDSurface.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fIncline.imDSurface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fIncline.imDSurface.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw line with equation y = mx + p, with x from x1 to x2 }

procedure DrawLine(X1, X2: Integer; M, P: Real);

var
  X, Y: Integer;

begin
  for X := X1 to X2 do begin
    Y := Round(M * X + P);
    if X = X1 then
      fIncline.imDSurface.Picture.Bitmap.Canvas.MoveTo(X, Y)
    else
      fIncline.imDSurface.Picture.Bitmap.Canvas.LineTo(X, Y);
  end;
end;

{ Draw the incline }

procedure DrawIncline(Len, Angle: Real; W, H, L, R, T, B: Integer; out X, Y: Integer; out M, P: Real);

var
  XMax, YMax: Integer;

begin
  // Determine display length X and height Y of the incline in order that the whole fits into the drawing surface
  XMax := W - L - R;
  YMax := H - T - B;
  if 10 * Len <= XMax then
    X := Round(10 * Len)
  else
    X := XMax;
  Y := Round(Tan(Degtorad(Angle)) * X);
  if Y > YMax then begin
    Y := YMax;
    X := Round(Y / Tan(Degtorad(Angle)));
  end;
  // Clear the drawing surface
  DSurfaceClear(W, H);
  // Draw the incline
  fIncline.imDSurface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fIncline.imDSurface.Picture.Bitmap.Canvas.Pen.Width := 2;
  fIncline.imDSurface.Picture.Bitmap.Canvas.Line(L, (H - B), L + X, (H - B));
  fIncline.imDSurface.Picture.Bitmap.Canvas.Line(L, (H - B) - Y, L, (H - B));
  LineEquation(L, (H - B) - Y, L + X, (H - B), M, P);                          // Determine line equation of incline's diagonal
  DrawLine(L, L + X, M, P);                                                    // Draw this line
end;

{ Draw the moving object (box) }

procedure DrawBox(BX1, BY1, BX2, BY2, BH: Integer; M, P: Real);

var
  X1, Y1, X2, Y2, I, J, J0: Integer;
  P1M, P1P, P2M, P2P: Real;

begin
  fIncline.imDSurface.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  fIncline.imDSurface.Picture.Bitmap.Canvas.Pen.Width := 1;
  // Draw bottom side of box
  DrawLine(BX1, BX2, M, P - 5);                                                // - 5: draw some pixels above diagonal of incline
  // Draw left side of the box
  PerpLineEquation(M, BX1, BY1, P1M, P1P);                                     // determine equation of perpendicular passing by bottom-left box corner
  I := BX1;                                                                    // start drawing with X-coordinate = bottom-left corner of box
  J0 := Round(P1M * I + P1P) - BH;                                             // Y-coordinate of top-left corner of box
  repeat
    J := Round(P1M * I + P1P) - 5;                                             // Y-coordinate as given by the perpendicular equation
    if I = BX1 then
      fIncline.imDSurface.Picture.Bitmap.Canvas.MoveTo(I, J)
    else
      fIncline.imDSurface.Picture.Bitmap.Canvas.LineTo(I, J);
    Inc(I);
  until J < J0;                                                                // draw line until Y-ccordinate < coordinate of top left corner of box
  X1 := I; Y1 := J;
  // Draw right side of the box
  PerpLineEquation(M, BX2, BY2, P2M, P2P);                                     // determine equation of perpendicular passing by bottom-right box corner
  I := BX2;                                                                    // start drawing with X-coordinate = bottom-right corner of box
  J0 := Round(P2M * I + P2P) - BH;                                             // Y-coordinate of top-right corner of box
  repeat
    J := Round(P2M * I + P2P) - 5;                                             // Y-coordinate as given by the perpendicular equation
    if I = BX2 then
      fIncline.imDSurface.Picture.Bitmap.Canvas.MoveTo(I, J)
    else
      fIncline.imDSurface.Picture.Bitmap.Canvas.LineTo(I, J);
    Inc(I);
  until J < J0;                                                                // draw line until Y-ccordinate < coordinate of top right corner of box
  X2 := I; Y2 := J;
  // Draw top side of box
  LineEquation(X1, Y1, X2, Y2, M, P);                                          // determine line equation of top side of box
  DrawLine(X1, X2, M, P);                                                      // draw the line
end;

{*************}
{* TfIncline *}
{*************}

{ Application start: Initialisation }

procedure TfIncline.FormCreate(Sender: TObject);

var
  X1, Y1, X2, Y2: Integer;

begin
  // Drawing surface
  iWidth := imDSurface.Width; iHeight := imDSurface.Height;
  iLeft  := 20; iRight  := 20;
  iTop   := 20; iBottom := 20;
  iDMax  := iWidth - iLeft - iRight;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iWidth;
  Bitmap.Height := iHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imDSurface.Picture.Graphic := Bitmap;
  // Clear the drawing surface
  DSurfaceClear(iWidth, iHeight);
  // Enable/disable user access to incline properties
  iSel := 2; InclineFields(iSel); InclineValues(iSel, rLength, rHeight, rDiagonal, rAngle);
  // Draw the incline
  DrawIncline(rLength, rAngle, iWidth, iHeight, iLeft, iRight, iTop, iBottom, iDLength, iDHeight, rEqM, rEqP); // equation parameters are returned by sub
  // Box values
  iBoxLeft := iLeft + 20;                                                      // distance of box from the left edge of drawing surface
  iBoxLength := 50;                                                            // box length
  iBoxHeight := 25;                                                            // box height
  // Box position: position and size has to be adapted accordingly to the actual display length of the incline (iDLength / iDMax)!
  X1 := iLeft + Round(iBoxLeft * (iDLength / iDMax));                          // X-position of box bottom-left corner
  Y1 := Round(rEqM * X1 + rEqP);                                               // Y-position of box bottom-left corner (determined by the equation params)
  X2 := X1 + Round(iBoxLength * (iDLength / iDMax));                           // X-position of box bottom-right corner
  Y2 := Round(rEqM * X2 + rEqP);                                               // Y-position of box bottom-left corner (determined by the equation params)
  // Draw the box
  DrawBox(X1, Y1, X2, Y2, Round(iBoxHeight * (iDLength / iDMax)), rEqM, rEqP);
  // Calculate and display the forces
  WeightForces(rMass, rWeight, rAngle, rDownForce, rNormalForce);
  FrictionForces(rNormalForce, rNFrictionStatic, rNFrictionKinetic, rFrictionStatic, rFrictionKinetic);
  // Automatic pause is off
  bAutoPauseActive := False;
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfIncline.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Schiefe Ebene Auswahl > ...": Select which incline proprties may be edited by user }

procedure TfIncline.mSettingsIncline1Click(Sender: TObject);

begin
  mSettingsIncline2.Checked := False;
  mSettingsIncline1.Checked := True;
  mSettingsIncline3.Checked := False;
  InclineFields(1);
end;

procedure TfIncline.mSettingsIncline2Click(Sender: TObject);

begin
  mSettingsIncline2.Checked := True;
  mSettingsIncline1.Checked := False;
  mSettingsIncline3.Checked := False;
  InclineFields(2);
end;

procedure TfIncline.mSettingsIncline3Click(Sender: TObject);

begin
  mSettingsIncline2.Checked := False;
  mSettingsIncline1.Checked := False;
  mSettingsIncline3.Checked := True;
  InclineFields(3);
end;

{ Menu item "Einstellungen > Masse in Pounds (lbs)": Select if mass unit is pound or kg }

procedure TfIncline.mSettingsMassLbsClick(Sender: TObject);

begin
  if mSettingsMassLbs.Checked then
    mSettingsMassLbs.Checked := False
  else
    mSettingsMassLbs.Checked := True;
end;

{ Menu item "Einstellungen > Geschwindigkeit in km/h": Select if speed unit is km/h or m/sec }

procedure TfIncline.mSettingsSpeedKmhClick(Sender: TObject);

begin
  if mSettingsSpeedKmh.Checked then
    mSettingsSpeedKmh.Checked := False
  else
    mSettingsSpeedKmh.Checked := True;
end;

{ Menu item "Erdbeschleunigung 9,80": Select if gravity constant used should be 9.80 or 9.81 }

procedure TfIncline.mSettingsGravity980Click(Sender: TObject);

begin
  if mSettingsGravity980.Checked then
    mSettingsGravity980.Checked := False
  else
    mSettingsGravity980.Checked := True;
end;

{ Menu item "Hilfe > Physik Hilfe": Display physics help (as PDF) }

procedure TfIncline.mHelpPhysicsClick(Sender: TObject);

begin
  OpenDocument('Incline_physics.pdf');
end;

{ Menu item "Hilfe > Programm Hilfe": Display program help (as PDF) }

procedure TfIncline.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('Incline_help.pdf');
end;

{ Menu item "Hilfe > Programm Info": Display program about }

procedure TfIncline.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := '"Incline" ist ein Physikprogramm, das die Gleitbewegung eines Körpers auf einer ';
    S += 'schiefen Ebene simuliert.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, November, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Start/Stop": Start resp. stop the simulation }

procedure TfIncline.btStartClick(Sender: TObject);

var
  X1, Y1, X2, Y2: Integer;

begin
  // Button "Start": Start the simulation
  if btStart.Caption = 'Start' then begin
    // Get incline properties from user
    if mSettingsIncline1.Checked then
      iSel := 1
    else if mSettingsIncline2.Checked then
      iSel := 2
    else
      iSel := 3;
    InclineValues(iSel, rLength, rHeight, rDiagonal, rAngle);
    // Proceed if valid user data
    if iSel <> 0 then begin
      // Draw incline
      DrawIncline(rLength, rAngle, iWidth, iHeight, iLeft, iRight, iTop, iBottom, iDLength, iDHeight, rEqM, rEqP);
      // Calculate coordinates of bottom corners of box
      X1 := iLeft + Round(iBoxLeft * (iDLength / iDMax)); Y1 := Round(rEqM * X1 + rEqP);
      X2 := iLeft + Round(iBoxLeft * (iDLength / iDMax)) + Round(iBoxLength * (iDLength / iDMax)); Y2 := Round(rEqM * X2 + rEqP);
      // Draw box
      DrawBox(X1, Y1, X2, Y2, Round(iBoxHeight * (iDLength / iDMax)), rEqM, rEqP);
      // Get weight forces (mass from user, others calculated)
      WeightForces(rMass, rWeight, rAngle, rDownForce, rNormalForce);
      // Proceed if mass entered is valid
      if rMass > 0 then begin
        // Get friction values (from user or from predefined table values) and friction forces (calculated)
        FrictionForces(rNormalForce, rNFrictionStatic, rNFrictionKinetic, rFrictionStatic, rFrictionKinetic);
        // Proceed if friction values entered are valid
        if (rNFrictionStatic >= 0) and (rNFrictionKinetic >= 0) then begin
          if rFrictionStatic >= rDownForce then begin
            // Static friction force >= down force: box remains at its position (will not move)
            edFrictionStatic.Color := clRed;
            MessageDlg('Schiefe Ebene', 'Haftreibungskraft > Hangabtriebskraft:' + Chr(13) + 'Die Kiste bleibt in Ruhestellung (gleitet nicht)!', mtWarning, [mbOK], 0);
          end
          else begin
            // Static friction force < down force: box moves along the incline
            if (rFrictionStatic <> 0) and (rFrictionKinetic <> 0) then
              edFrictionStatic.Color := clLime
            else
              edFrictionStatic.Color := clForm;
            // Calculate acceleration force (F = FH - FR) and acceleration (F / m)
            rForce := rDownForce - rFrictionKinetic; edAccForce.Text := FloatToStrF(rForce, ffFixed, 2, 2);
            rAcceleration := rForce / rMass; edAcceleration.Text := FloatToStrF(rAcceleration, ffFixed, 2, 2);
            // Reset simulation parameters
            rTime := 0; rDistance := 0; rSpeed := 0;
            rStep := 0.1;                                                      // simulation time step = 0.1 sec
            edTime.Text := '0'; edDistance.Text := '0'; edSpeed.Text := '0';
            // Set correct button captions
            btStart.Caption := 'Stop';
            btPause.Caption := 'Pause';
            // Start the simulation (by enabling the timer)
            tiIncline.Enabled := True;
          end;
        end;
      end;
    end;
  end
  // Button "Stop": Stop the simulation
  else begin
    // Stop the simulation (by disabling the timer)
    tiIncline.Enabled := False;
    // Reset button captions
    btStart.Caption := 'Start';
    btPause.Caption := 'Pause';
    // If automatic pause is active, reset auto-pause variables
    if bAutoPauseActive then begin
      iAutoPause := fPSelect.iSelect;
      rAutoPause0 := StrToFloat(fPSelect.edPauseValue.Text); rAutoPause := rAutoPause0;
      bAutoPauseDone := False;
    end;
  end;
end;

{ Button "Pause/Weiter": Pause resp. resume the simulation }

procedure TfIncline.btPauseClick(Sender: TObject);

begin
  if btStart.Caption = 'Stop' then begin
    if btPause.Caption = 'Pause' then begin
      tiIncline.Enabled := False;
      btPause.Caption := 'Weiter';
    end
    else begin
      tiIncline.Enabled := True;
      btPause.Caption := 'Pause';
    end;
  end;
end;

{ Button "Pause (auto)": Get auto-pause values and activate automatic pause }

procedure TfIncline.btAutoPauseClick(Sender: TObject);

begin
  fPSelect.ShowModal;                                                          // get values from user via the fPSelect form
  // Proceed only if user pushed "OK" button (ignore if she pushed "Cancel")
  if fPSelect.sButton = 'ok' then begin
    bAutoPauseActive := fPSelect.cbAutoPause.Checked;                          // auto-pause active/inactive status
    // Auto-pause has been activated
    if bAutoPauseActive then begin
      iAutoPause := fPSelect.iSelect;                                          // auto-pause type (corr. to radiobuttons on fPSelect form )
      rAutoPause0 := StrToFloat(fPSelect.edPauseValue.Text);                   // initial auto-pause value
      rAutoPause := rAutoPause0;                                               // actual auto-pause value
      bAutoPauseDone := False;                                                 // used to control single/multi pause behaviour
      btAutoPause.Font.Style := [fsBold];                                      // auto-pause indicated on GUI by bold button caption
    end
    else
      btAutoPause.Font.Style := [];
  end;
end;

{ Main simulation routine (timer routine) }

procedure TfIncline.tiInclineTimer(Sender: TObject);

var
  X1, Y1, X2, Y2: Integer;
  L, PauseValue, TimeSave: Real;
  Pause: Boolean;

begin
  // With time advancing 1 unit, calculate new distance and speed
  rTime += rStep;
  rDistance := 0.5 * rAcceleration * Sqr(rTime);
  rSpeed := rAcceleration * rTime;
  // Determine new position of the box (coordinates of bottom corners)
  L := Cos(Degtorad(rAngle)) * rDistance;                                      // horizontal component of the distance along the diagonal
  X1 := iLeft + iBoxLeft + Round((L / rLength) * iDLength);
  Y1 := Round(rEqM * X1 + rEqP);
  X2 := X1 + Round(iBoxLength * (iDLength / iDMax)); Y2 := Round(rEqM * X2 + rEqP);
  // If the box has reached the end of the incline, stop the simulation
  if Y2 > iHeight - iBottom then begin
    // Disabling the timer
    tiIncline.Enabled := False;
    // Reset button captions
    btStart.Caption := 'Start';
    btPause.Caption := 'Pause';
    // If automatic pause is active, reset auto-pause variables
    if bAutoPauseActive then begin
      iAutoPause := fPSelect.iSelect;
      rAutoPause0 := StrToFloat(fPSelect.edPauseValue.Text); rAutoPause := rAutoPause0;
      bAutoPauseDone := False;
    end;
  end
  // Box has to be moved...
  else begin
    Pause := False;
    // If auto-pause is activated and still is active, pause simulation if auto-pause value has been reached
    if bAutoPauseActive and not bAutoPauseDone then begin
      case iAutoPause of
        1, 2: PauseValue := rTime;
        3, 4: PauseValue := rDistance;
           5: PauseValue := rSpeed;
      end;
      // If actual value >= auto-pause value, the simulation has to be paused
      if Round(1000 * PauseValue) >= Round(1000 * rAutoPause) then begin
        Pause := True;
        TimeSave := rTime;                                                     // save actual simulation time
        // If actual value > auto-pause value, the moment where the simulation had to be stopped has already passed
        // Using the actual auto-pause value as reference, recalculate time, distance and speed; this allows to stop
        // the simulation at a time corresponding to the exact user entered value of time, distance or speed
        if Round(1000 * PauseValue) > Round(1000 * rAutoPause) then begin
          // Determine simulation-stop time based on chosen auto-pause value
          case iAutoPause of
            1, 2: rTime := rAutoPause;
            3, 4: rTime := Sqrt((2 * rAutoPause) / rAcceleration);
               5: rTime := rAutoPause / rAcceleration;
          end;
          // Calculate distance and speed for the so determined time
          rDistance := 0.5 * rAcceleration * Sqr(rTime);
          rSpeed := rAcceleration * rTime;
          // Redo the calculation of the box position
          L := Cos(Degtorad(rAngle)) * rDistance;
          X1 := iLeft + iBoxLeft + Round((L / rLength) * iDLength);
          Y1 := Round(rEqM * X1 + rEqP);
          X2 := X1 + Round(iBoxLength * (iDLength / iDMax)); Y2 := Round(rEqM * X2 + rEqP);
        end;
      end;
    end;
    // Display actual values of time, distance and speed
    edTime.Text := FloatToStrF(rTime, ffFixed, 2, 2);
    edDistance.Text := FloatToStrF(rDistance, ffFixed, 2, 2);
    if mSettingsSpeedKmh.Checked then
      edSpeed.Text := FloatToStrF(3.6 * rSpeed, ffFixed, 2, 2)                 // speed in km/h
    else
      edSpeed.Text := FloatToStrF(rSpeed, ffFixed, 2, 2);                      // speed in m/sec
    // Draw the box at actual position (redrawing the incline, with clearing drawing area, wipes out old box position)
    DrawIncline(rLength, rAngle, iWidth, iHeight, iLeft, iRight, iTop, iBottom, iDLength, iDHeight, rEqM, rEqP);
    DrawBox(X1, Y1, X2, Y2, Round(iBoxHeight * (iDLength / iDMax)), rEqM, rEqP);
    // If simulation has to be paused (auto-pause value reached), do so now
    if Pause then begin
      tiIncline.Enabled := False;                                              // disabling timer pauses the simulation
      btPause.Caption := 'Weiter';                                             // use "Pause" button to resume
      if iAutoPause in [1, 3, 5] then                                          // for "single-pause" auto-pause types, no more pause during this run
        bAutoPauseDone := True
      else
        rAutoPause += rAutoPause0;                                             // for "multi-pause" auto-pause types, update auto-pause value
      rTime := TimeSave;                                                       // restore time; this allows to continue with same time value format as before
    end;
  end;
end;

{ Changement of materials combobox selection: Get corresponding friction values }

procedure TfIncline.cobFrictionChange(Sender: TObject);

begin
  FrictionValues(rNFrictionStatic, rNFrictionKinetic);
end;

end.

