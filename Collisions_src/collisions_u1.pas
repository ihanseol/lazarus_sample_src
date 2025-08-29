{****************************************}
{* Main unit for Collisions application *}
{****************************************}

unit collisions_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, ComCtrls, Math, collisions_u2;

type
  {**************}
  { TfCollisions }
  {**************}
  TfCollisions = class(TForm)
    mMenu: TMainMenu;
    mCollision, mCollision1el, mCollision1inel, mCollision2el, mCollisionExit: TMenuItem;
    mOptions, mOptionsNoWarnings: TMenuItem;
    mHelp, mHelpPhysics, mHelpHelp, mHelpAbout: TMenuItem;
    Shape1: TShape;
    edTitle: TEdit;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    Label9, Label10, Label11, Label12, Label13, Label14, Label15, Label16: TLabel;
    shBall1, shBall2: TShape;
    shMarker1, shMarker2: TShape;
    tbBall: TTrackBar;
    btTrackbar: TButton;
    laImpact: TLabel;
    edTime, edPosition1, edPosition2, edImpactTime, edImpactPosition: TEdit;
    la2dTitle, la2dRadius, la2dURadius, la2dImpact, la2dUImpact: TLabel;
    laSituation: TLabel;
    laSpeedF1, laSpeedF2, laUSpeedF1, laUSpeedF2, laAngleF1, laAngleF2, laUAngleF1, laUAngleF2: TLabel;
    edMass1, edMass2, edSpeed1, edSpeed2, edRadius, edImpact: TEdit;
    edSpeedF1, edSpeedF2, edAngleF1, edAngleF2: TEdit;
    btStart: TButton;
    btPause: TButton;
    btSetRadius: TButton;
    btSetImpact: TButton;
    tiCollisions: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mCollision1elClick(Sender: TObject);
    procedure mCollision1inelClick(Sender: TObject);
    procedure mCollision2elClick(Sender: TObject);
    procedure mCollisionExitClick(Sender: TObject);
    procedure mOptionsNoWarningsClick(Sender: TObject);
    procedure mHelpPhysicsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btSetRadiusClick(Sender: TObject);
    procedure btSetImpactClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tbBallChange(Sender: TObject);
    procedure btTrackbarClick(Sender: TObject);
    procedure tiCollisionsTimer(Sender: TObject);
    procedure edRadiusEditingDone(Sender: TObject);
    procedure edImpactEditingDone(Sender: TObject);
    procedure edMass1Change(Sender: TObject);
  private
    iImpactBall1L, iImpactBall2L, iImpactBall1T, iImpactBall2T, iTBOldPosition, iTraj1, iTraj2: Integer;
    rM1, rM2, rV1, rV2, rR, rB, rV1F, rV2F, rA1F, rA2F: Real;
    rTime, rSpeed1, rSpeed2, rDistance1, rDistance2, rImpactTime, rImpactPosition, rBallDistance: Real;
    sCollision: string;
    bRadiusChanged, bImpactChanged, bImpact: Boolean;
    shTrajectory1, shTrajectory2: array[0..200] of TShape;
  end;

const
  Ball1Left = 100; Ball2Left = 350; BallMiddle = 300; BallRadius = 10;

var
  fCollisions: TfCollisions;

implementation

{$R *.lfm}

{ Prepare for a new collision simulation }

procedure NewCollision(Collision: string; Radius, Impact: Real; Traj1, Traj2: array of TShape);

var
  BRadius, BMiddle, I: Integer;

begin
  fCollisions.tiCollisions.Enabled := False;                                   // stop timer (if possibly still active)
  BRadius := Round(2 * Radius);                                                // drawn ball size = double of "real" ball size
  fCollisions.laSituation.Caption := 'Situation après l''impact:';
  // Clear simulation controls
  fCollisions.laSituation.Font.Color := clDefault;
  fCollisions.edSpeedF1.Text := ''; fCollisions.edSpeedF2.Text := ''; fCollisions.edAngleF1.Text := ''; fCollisions.edAngleF2.Text := '';
  fCollisions.edTime.Text := ''; fCollisions.edPosition1.Text := ''; fCollisions.edPosition2.Text := '';
  fCollisions.edImpactTime.Text := ''; fCollisions.edImpactPosition.Text := '';
  fCollisions.edImpactTime.Enabled := True; fCollisions.edImpactPosition.Enabled := True;
  fCollisions.edImpactTime.Color := clBlack; fCollisions.edImpactPosition.Color := clBlack;
  fCollisions.laImpact.Font.Style := [];
  for I := 0 to Length(Traj1) - 1 do begin
    Traj1[I].Visible := False; Traj2[I].Visible := False;
  end;
  // Show and make initial values controls accessible (depends on type of collision)
  fCollisions.edMass1.ReadOnly := False; fCollisions.edMass1.TabStop := True;
  fCollisions.edSpeed1.ReadOnly := False; fCollisions.edSpeed1.TabStop := True;
  if Collision = 'el2' then begin
    // 2D collision
    fCollisions.edTitle.Text := 'Collision élastique dans 2 dimensions.';
    fCollisions.edMass2.ReadOnly := True; fCollisions.edMass2.TabStop := False; fCollisions.edMass2.Color := clCream;
    fCollisions.edSpeed2.ReadOnly := True; fCollisions.edSpeed2.TabStop := False;
    fCollisions.edSpeed2.Color := clCream; fCollisions.edSpeed2.Text := '0';
    fCollisions.la2dTitle.Visible := True; fCollisions.la2dRadius.Visible := True; fCollisions.la2dImpact.Visible := True;
    fCollisions.la2dURadius.Visible := True; fCollisions.la2dUImpact.Visible := True;
    fCollisions.edRadius.Visible := True; fCollisions.edImpact.Visible := True;
    fCollisions.edRadius.ReadOnly := False; fCollisions.edRadius.TabStop := True;
    fCollisions.edImpact.ReadOnly := False; fCollisions.edImpact.TabStop := True;
    fCollisions.btSetRadius.Visible := True; fCollisions.btSetImpact.Visible := True;
    fCollisions.laAngleF1.Visible := True; fCollisions.laUAngleF1.Visible := True;
    fCollisions.laAngleF2.Visible := True; fCollisions.laUAngleF2.Visible := True;
    fCollisions.edAngleF1.Visible := True; fCollisions.edAngleF2.Visible := True;
    if not fCollisions.btTrackbar.Enabled then begin
      fCollisions.tbBall.Enabled := True;                                      // enable trackbar (except if impact parameter has been manually modified)
    end;
  end
  else begin
    // 1D collision
    if Collision = 'el1' then begin
      fCollisions.edTitle.Text := 'Collision élastique dans 1 dimension.';
      fCollisions.laSpeedF1.Caption := 'Vitesse bille 1';
      fCollisions.laSpeedF2.Visible := True; fCollisions.edSpeedF2.Visible := True; fCollisions.laUSpeedF2.Visible := True;
    end
    else begin
      fCollisions.edTitle.Text := 'Collision inélastique dans 1 dimension.';
      fCollisions.laSpeedF1.Caption := 'Vitesse des billes';
      fCollisions.laSpeedF2.Visible := False; fCollisions.edSpeedF2.Visible := False; fCollisions.laUSpeedF2.Visible := False;
    end;
    fCollisions.edMass2.ReadOnly := False; fCollisions.edMass2.TabStop := True; fCollisions.edMass2.Color := clDefault;
    fCollisions.edSpeed2.ReadOnly := False; fCollisions.edSpeed2.TabStop := True; fCollisions.edSpeed2.Color := clDefault;
    fCollisions.la2dTitle.Visible := False; fCollisions.la2dRadius.Visible := False; fCollisions.la2dImpact.Visible := False;
    fCollisions.la2dURadius.Visible := False; fCollisions.la2dUImpact.Visible := False;
    fCollisions.edRadius.Visible := False; fCollisions.edImpact.Visible := False;
    fCollisions.btSetRadius.Visible := False; fCollisions.btSetImpact.Visible := False;
    fCollisions.laAngleF1.Visible := False; fCollisions.laUAngleF1.Visible := False;
    fCollisions.laAngleF2.Visible := False; fCollisions.laUAngleF2.Visible := False;
    fCollisions.edAngleF1.Visible := False; fCollisions.edAngleF2.Visible := False;
    fCollisions.tbBall.Enabled := False; fCollisions.btTrackbar.Enabled := False;
  end;
  // Draw the balls (and the markers for 2D collisions)
  if Collision = 'el2' then
    BMiddle := Round(BallMiddle - 2 * Impact)
  else
    BMiddle := BallMiddle;
  fCollisions.shBall1.Width := 2 * BRadius; fCollisions.shBall1.Height := fCollisions.shBall1.Width;
  fCollisions.shBall2.Width := fCollisions.shBall1.Width; fCollisions.shBall2.Height := fCollisions.shBall1.Height;
  fCollisions.shBall1.Left := Ball1Left; fCollisions.shBall2.Left := Ball2Left;
  fCollisions.shBall1.Top := BMiddle - BRadius; fCollisions.shBall2.Top := BallMiddle - BRadius;
  if Collision = 'el2' then begin
    fCollisions.shMarker1.Top := fCollisions.shBall1.Top; fCollisions.shMarker1.Visible := True;
    fCollisions.shMarker2.Top := fCollisions.shBall1.Top + 2 * BRadius; fCollisions.shMarker2.Visible := True;
  end
  else begin
    fCollisions.shMarker1.Visible := False; fCollisions.shMarker2.Visible := False;
  end;
  // Enable buttons
  fCollisions.btStart.Caption := 'Start'; fCollisions.btPause.Caption := 'Pause';
  fCollisions.btStart.Enabled := True; fCollisions.btPause.Enabled := True;
end;

{ Read initial values from form }

procedure ReadData(out M1, M2, V1, V2, R, B: Real; out Mess: string);

var
  BallDistance, ImpactPosition: Real;

begin
  Mess := '';
  fCollisions.edImpact.Font.Color := clDefault;
  if fCollisions.edMass1.Text = '' then
    M1 := 0
  else
    M1 := StrToFloat(fCollisions.edMass1.Text);
  if fCollisions.edSpeed1.Text = '' then
    V1 := 0
  else
    V1 := StrToFloat(fCollisions.edSpeed1.Text);
  if fCollisions.edMass2.Text = '' then
    M2 := 0
  else
    M2 := StrToFloat(fCollisions.edMass2.Text);
  if fCollisions.edSpeed2.Text = '' then
    V2 := 0
  else
    V2 := StrToFloat(fCollisions.edSpeed2.Text);
  if fCollisions.edRadius.Text = '' then
    R := 0
  else
    R := StrToFloat(fCollisions.edRadius.Text);
  if fCollisions.edImpact.Text = '' then
    B := 0
  else
    B := StrToFloat(fCollisions.edImpact.Text);
  // Check values and set error message if invalid
  if (M1 <= 0) or (M2 <= 0) then begin
    if fCollisions.edMass2.ReadOnly and (fCollisions.edMass2.Text = '') and (fCollisions.edMass1.Text <> '') then
      // For 2D collisions, set m2 = m1
      fCollisions.edMass2.Text := fCollisions.edMass1.Text
    else begin
      Mess := 'La masse des billes doit être supérieure à 0';
      if M1 <= 0 then
        fCollisions.edMass1.SetFocus
      else
        fCollisions.edMass2.SetFocus;
    end;
  end
  else if (V1 < 0) or (V2 < 0) then begin
    Mess := 'La vitesse des billes doit être supérieure ou égale à 0';
    if V1 < 0 then
      fCollisions.edSpeed1.SetFocus
    else
      fCollisions.edSpeed2.SetFocus;
  end
  else if V1 = 0 then begin
    // The first ball must move, of course...
    Mess := 'La vitesse de la bille 1 doit être supérieure à 0';
    fCollisions.edSpeed1.SetFocus;
  end
  else if fCollisions.edRadius.Visible and (R <= 0) then begin
    Mess := 'Le rayon des billes doit être supérieur à 0';
    fCollisions.edRadius.SetFocus;
  end
  // Display warning messages, if no collision or impact outside display area
  else if V1 <= V2 then begin
    if not fCollisions.mOptionsNoWarnings.Checked then
      MessageDlg('Collisions 1D', 'La vitesse de la bille 1 doit être > celle de la bille 2 pour qu''il y ait collision!', mtWarning, [mbOK], 0);
  end
  else if fCollisions.edImpact.Visible and (Abs(B) > 2 * R) then begin
    if not fCollisions.mOptionsNoWarnings.Checked then
      MessageDlg('Billes de billard', 'Le paramètre d''impact doit être ≤ le diamètre des billes pour qu''il y ait collision!', mtWarning, [mbOK], 0);
  end
  else begin
    // Calculate impact position to see if it's within display area
    BallDistance := ((fCollisions.shBall2.Left - (fCollisions.shBall1.Left + fCollisions.shBall1.Width)) / 2) / 100;
    ImpactPosition := (BallDistance / (V1 - V2)) * V1 + (fCollisions.shBall1.Width / 2) / 100;
    if ImpactPosition > 4.30 then begin
      if not fCollisions.mOptionsNoWarnings.Checked then
        MessageDlg('Collisions', 'La position d''impact sera en dehors de la zone d''affichage!', mtWarning, [mbOK], 0);
    end;
  end;
end;

{**************}
{ TfCollisions }
{**************}

{ Application start: Initialisation }

procedure TfCollisions.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create 2 x 101 small red and blue ellipses being placed and made
  // visible as needed to draw the trajectories of the 2 balls
  for I := 0 to 200 do begin
    shTrajectory1[I] := TShape.Create(shTrajectory1[I]);
    shTrajectory1[I].Parent := Self;
    shTrajectory1[I].Shape := stEllipse;
    shTrajectory1[I].Width := 4; shTrajectory1[I].Height := 4;
    shTrajectory1[I].Brush.Color := clRed; shTrajectory1[I].Pen.Color := clRed;
    shTrajectory1[I].Visible := False;
    shTrajectory2[I] := TShape.Create(shTrajectory1[I]);
    shTrajectory2[I].Parent := Self;
    shTrajectory2[I].Shape := stEllipse;
    shTrajectory2[I].Width := 4; shTrajectory2[I].Height := 4;
    shTrajectory2[I].Brush.Color := clBlue; shTrajectory2[I].Pen.Color := clBlue;
    shTrajectory2[I].Visible := False;
  end;
  // Start up with 1D elastic collision simulation
  sCollision := 'el1'; rR := BallRadius / 2; rB := 0;
  iTBOldPosition := 0; bRadiusChanged := False; bImpactChanged := False;
  NewCollision(sCollision, rR, rB, shTrajectory1, shTrajectory2);
end;

{ Menu item "Collision > Collision élastique (1d)": Prepare 1D elastic collision simulation }

procedure TfCollisions.mCollision1elClick(Sender: TObject);

begin
  sCollision := 'el1'; rR := BallRadius / 2;  rB := 0;
  NewCollision(sCollision, rR, rB, shTrajectory1, shTrajectory2);
end;

{ Menu item "Collision > Collision inélastique (1d)": Prepare 1D inelastic collision simulation }

procedure TfCollisions.mCollision1inelClick(Sender: TObject);

begin
  sCollision := 'inel1'; rR := BallRadius / 2;  rB := 0;
  NewCollision(sCollision, rR, rB, shTrajectory1, shTrajectory2);
end;

{ Menu item "Collision > Collision élastique (2d)": Prepare 2D elastic collision simulation }

procedure TfCollisions.mCollision2elClick(Sender: TObject);

begin
  sCollision := 'el2'; rR := StrToFloat(edRadius.Text); rB := StrToFloat(edImpact.Text);
  NewCollision(sCollision, rR, rB, shTrajectory1, shTrajectory2);
end;

{ Menu item "Collision > Quitter": Exit the application }

procedure TfCollisions.mCollisionExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Ne pas afficher les avertissements": Select to display/not display warning messages }

procedure TfCollisions.mOptionsNoWarningsClick(Sender: TObject);

begin
  if mOptionsNoWarnings.Checked then
    mOptionsNoWarnings.Checked := False
  else
    mOptionsNoWarnings.Checked := True;
end;

{ Menu item "Aide > Aide physique": Display physics help text }

procedure TfCollisions.mHelpPhysicsClick(Sender: TObject);

begin
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Aide programme": Display application help text }

procedure TfCollisions.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Info programme": Display application about }

procedure TfCollisions.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Simulation de physique:' + LineEnding;
  S += 'Collisions élastiques et inélastiques.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Décembre 2018 - Mars 2019.';
  MessageDlg('Info "Collisions"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Stop": Start resp. stop collision simulation }

procedure TfCollisions.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Button "Start": Start collision simulation
  if btStart.Caption = 'Start' then begin
    if bRadiusChanged then
      // Be sure that the ball radius has been set
      Mess := 'Le rayon des billes n''est pas fixé'
    else if bImpactChanged then
      // Be sure that the (manually entered) impact parameter has been set
      Mess := 'Le paramètre d''impact n''est pas fixé'
    else
      // Get initial values from form
      ReadData(rM1, rM2, rV1, rV2, rR, rB, Mess);
    // Proceed only if all ok (otherwise display error message)
    if Mess <> '' then
      MessageDlg('Collisions: Erreur', Mess + '!', mtError, [mbOK], 0)
    else begin
      // Do not allow to change values during simulation runtime
      edMass1.ReadOnly := True; edMass1.TabStop := False; edMass2.ReadOnly := True; edMass2.TabStop := False;
      edSpeed1.ReadOnly := True; edSpeed1.TabStop := False; edSpeed2.ReadOnly := True; edSpeed2.TabStop := False;
      edRadius.ReadOnly := True; edRadius.TabStop := False; edImpact.ReadOnly := True; edImpact.TabStop := False;
      if (rV1 <= rV2) or ((sCollision = 'el2') and (Abs(rB) > 2 * rR)) then begin
        // Initial values are such that there is no collision
        bImpact := False;
        laSituation.Caption := 'Il n'' y a pas de collision dans ce cas!';
        laSituation.Font.Color := clRed;
        rV1F := rV1; rV2F := rV2;
        laAngleF1.Visible := False; laUAngleF1.Visible := False; laAngleF2.Visible := False; laUAngleF2.Visible := False;
        edAngleF1.Visible := False; edAngleF2.Visible := False; edImpactTime.Enabled := False; edImpactPosition.Enabled := False;
        edImpactTime.Color := clDefault; edImpactPosition.Color := clDefault; fCollisions.laImpact.Font.Style := [fsStrikeOut];
      end
      else begin
        // Normal case (there will be a collision)
        bImpact := True;
        laSituation.Caption := 'Situation après l''impact:';
        laSituation.Font.Color := clDefault;
        // After collision values calculations for 1D elastic collision
        if sCollision = 'el1' then begin
          rV1F := ((rM1 - rM2) / (rM1 + rM2)) * rV1 + ( 2 * rM2 / (rM1 + rM2)) * rV2;
          rV2F := (2 * rM1 / (rM1 + rM2)) * rV1 + ((rM2 - rM1) / (rM1 + rM2)) * rV2;
        end
        // After collision value calculations for 1D inelastic collision
        else if sCollision = 'inel1' then begin
          rV1F := (rM1 * rV1  + rM2 * rV2) / (rM1 + rM2); rV2F := rV1F;
        end
        // After collision value calculations for 2D elastic collision
        else begin
          if Abs(rB) = 2 * rR then
            rA1F := ArcCos((Abs(rB) - 1E-9) / (2 * rR))
          else
            rA1F := ArcCos(Abs(rB) / (2 * rR));
          rA2F := rA1F - (Pi / 2);
          if rB < 0 then begin
            rA1F := -rA1F; rA2F := -rA2F;
          end;
          rV1F := Cos(rA1F) * rV1; rV2F := Cos(rA2F) * rV1;
          if rV1F < 1E-4 then
            edAngleF1.Text := ''
          else
            edAngleF1.Text := FloatToStrF(RadToDeg(rA1F), ffFixed, 0, 2);
        end;
        edAngleF2.Text := FloatToStrF(RadToDeg(rA2F), ffFixed, 0, 2);
      end;
      edSpeedF1.Text := FloatToStrF(rV1F, ffFixed, 0, 2);
      if (sCollision = 'el2') and (rB = 2 * rR) then
        edSpeedF2.Text := FloatToStrF(rV2F, ffExponent, 3, 1)
      else
        edSpeedF2.Text := FloatToStrF(rV2F, ffFixed, 0, 2);
      // Remove the markers (2D collision)
      fCollisions.shMarker1.Visible := False;
      fCollisions.shMarker2.Visible := False;
      // Simuation start up values
      rTime := 0; rDistance1 := 0; rDistance2 := 0;
      rSpeed1 := rV1; rSpeed2 := rV2;
      rBallDistance := ((shBall2.Left - (shBall1.Left + shBall1.Width)) / 2) / 100;
      iImpactBall1T := shBall1.Top; iImpactBall2T := shBall2.Top;
      // Fill in impact values
      if bImpact then begin
        rImpactTime := rBallDistance / (rSpeed1 - rSpeed2);
        edImpactTime.Text := FloatToStrF(rImpactTime, ffFixed, 0, 2);
        rImpactPosition := rImpactTime * rV1 + (shBall1.Width / 2) / 100;
        edImpactPosition.Text := FloatToStrF(rImpactPosition, ffFixed, 0, 2);
      end;
      // Next button push will be 'stop'
      btStart.Caption := 'Stop';
      // Start using the first (of the 101) trajectory shapes
      iTraj1 := 0; iTraj2 := 0;
      // Set timer interval and start the timer (all simulation code within the timer routine)
      tiCollisions.Interval := Round(100 / rV1);
      tiCollisions.Enabled := True;
    end;
  end
  // Button "Stop": Stop the simulation
  else begin
    tiCollisions.Enabled := False;                                             // stop the timer
    btStart.Caption := 'Start';                                                // next button push will be 'start'
    btStart.Enabled := False; btPause.Enabled := False;                        // disable buttons (user has to use "collision" menu item)
  end;
end;

{ Button "Pause/Continuer": Pause resp. resume the simulation }

procedure TfCollisions.btPauseClick(Sender: TObject);

begin
  if btStart.Caption = 'Stop' then begin
    // Action button only if simulation is actually running
    if btPause.Caption = 'Pause' then begin
      // "Pause" button
      tiCollisions.Enabled := False;
      btPause.Caption := 'Continuer';
    end
    else begin
      // "Continuer" button
      tiCollisions.Enabled := True;
      btPause.Caption := 'Pause';
    end;
  end;
end;

{ Trackbar position changed: Position balls (and markers) }

procedure TfCollisions.tbBallChange(Sender: TObject);

var
  BMiddle, BRadius: Integer;

begin
  if btStart.Enabled and (btStart.Caption = 'Start') then begin
    // Allow trackbar changes only when simulation not running
    if (tbBall.Position <> 0) and (edRadius.Text = '') then begin
      // Ball positionning depending on ball radius...
      MessageDlg('Billes de billard', 'Entrez le rayon des billes avant de les positionner, svp!', mtError, [mbOK], 0);
      tbBall.Position := iTBOldPosition;
    end
    else begin
      // Position balls (and markers)
      BRadius := Round(2 * rR);
      BMiddle := Round(BallMiddle + tbBall.Position * (2 * (2 * BRadius) / 200));
      sHBall1.Top := BMiddle - BRadius;
      shMarker1.Top := sHBall1.Top; shMarker2.Top := sHBall1.Top + 2 * BRadius;
      edImpact.Text := FloatToStrF(-(sHBall1.Top - sHball2.Top) / 2, ffFixed, 0, 2);
      iTBOldPosition := tbBall.Position;
    end;
  end
  else
    // Reset trackbar to old position if user tried to change position during simulation
    tbBall.Position := iTBOldPosition;
end;

{ Button "TB": Re-enable the trackbar and reset impact parameter to 0 }

procedure TfCollisions.btTrackbarClick(Sender: TObject);

begin
  tbBall.Enabled := True;                                                      // re-enable trackbar
  tbBall.Position := 0;                                                        // set to '0' position
  edImpact.Text := '0';                                                        // what corresponds to b = 0
  // Re-position ball 1 and markers
  shBall1.Top := shBall2.Top;
  shMarker1.Top := sHBall1.Top; shMarker2.Top := sHBall1.Top + shBall1.Height;
  // Disable "TB" button
  btTrackbar.Enabled := False;
  btTrackbar.ShowHint := False;
end;

{ Button "Fixer" (for radius): Set new radius }

procedure TfCollisions.btSetRadiusClick(Sender: TObject);

// Setting the new radius includes a redrawing of the balls with impact parameter reset to 0

begin
  // Do this only if the radius value has been modified
  if bRadiusChanged then begin
    if fCollisions.edRadius.Text = '' then
      rR := 0
    else
      rR := StrToFloat(fCollisions.edRadius.Text);
    // Ball radius supposed to be between 1 and 20 (ball sizes between 4 and 80 pixles)
    if ((rR < 1) or (rR > 20)) then begin
      MessageDlg('Collisions: Erreur', 'Le rayon des billes est supposé être entre 1 et 20!', mtError, [mbOK], 0);
      edRadius.SetFocus;
    end
    // Redraw the balls with impact parameter reset to 0
    else begin
      NewCollision(sCollision, rR, rB, shTrajectory1, shTrajectory2);
      rB := 0; edImpact.Text := '0';
      tbBall.Position := 0;
      edRadius.Color := clDefault;
      bRadiusChanged := False;
    end;
  end;
end;

{ Button "Fixer" (for impact parameter): Set new impact parameter }

procedure TfCollisions.btSetImpactClick(Sender: TObject);

var
  S: string;

begin
  // Do this only if the impact parameter value has been manually modified
  if bImpactChanged then begin
    if edImpact.Text = '' then
      rB := 0
    else
      rB := StrToFloat(edImpact.Text);
    // Impact parameter supposed to be between arbitrarily fixed values)
    if Abs(rB) > 4 * rR then begin
      S := 'Le paramètre d''impact est supposé être entre ' + FloatToStrF(-4 * rR, ffFixed, 0, 0) + ' et ' + FloatToStrF(4 * rR, ffFixed, 0, 0);
      MessageDlg('Collisions: Erreur', S + '!', mtError, [mbOK], 0);
      edImpact.SetFocus;
    end
    // Redraw the balls with new impact parameter
    else begin
      NewCollision(sCollision, rR, rB, shTrajectory1, shTrajectory2);
      tbBall.Enabled := False;                                                 // disable trackbar
      btTrackbar.Enabled := True; btTrackbar.ShowHint := True;                 // enable "TB" button (to re-enable trackbar)
      edImpact.Color := clDefault;
      bImpactChanged := False;
    end;
  end;
end;

{ Timer routine: Collision simulation code }

procedure TfCollisions.tiCollisionsTimer(Sender: TObject);

var
  SDistance1, SDistance2, SDistance1x, SDistance1y, SDistance2x, SDistance2y, Ball1Top, Ball2Top: Integer;
  DistanceI1, DistanceI2, Distance1x, Distance1y, Distance2x, Distance2y: Real;
  Stop: Boolean;

begin
  Stop := False;
  // Calculate duration and distance done during this time
  rTime += 0.01 / rV1;
  rDistance1 += (0.01 / rV1) * rSpeed1; rDistance2 += (0.01 / rV1) * rSpeed2;
  // Determine distance done on display area
  SDistance1 := Round(2 * rDistance1 * 100); SDistance2 := Round(2 * rDistance2 * 100);
  // Continue simulation until balls move outside display area
  if (Ball1Left + SDistance1 > Shape1.Left + 28) and (Ball1Left + SDistance1 < Shape1.Left + Shape1.Width - shBall1.Width - 28) and (Ball2Left + SDistance2 < Shape1.Left + Shape1.Width - shBall1.Width - 28) then begin
    // 1D collisions: Just move the balls to new position (= distance done on display area)
    if RightStr(sCollision, 1) = '1' then begin
      shBall1.Left := Ball1Left + SDistance1;
      shBall2.Left := Ball2Left + SDistance2;
    end
    // 2D collision
    else begin
      // If no impact or impact moment not yet reached: Move ball 1 to new position (= distance done on display area)
      if not bImpact or (rTime <= rImpactTime) then begin
        // Display ball 1 trajectory
        if (shBall1.Left - Ball1Left) mod 10 = 0 then begin
          shTrajectory1[iTraj1].Left := shBall1.Left;
          shTrajectory1[iTraj1].Top := shBall1.Top + (shBall1.Width div 2) - 2;
          shTrajectory1[iTraj1].Visible := True;
          if iTraj1 < 200 then
            Inc(iTraj1);
        end;
        // Move ball 1
        shBall1.Left := Ball1Left + SDistance1;
        shBall2.Left := Ball2Left + SDistance2;
        // Save ball positions (last one will be needed for calculations after the impact)
        iImpactBall1L := shBall1.Left; iImpactBall2L := shBall2.Left;
      end
      // Balls 1 and 2 movements after the impact
      else begin
        // Calculate the balls' distances done after the impact and their x- and y-components
        DistanceI1 := (rTime - rImpactTime) * rSpeed1; DistanceI2 := (rTime - rImpactTime) * rSpeed2;
        Distance1x := Cos(rA1F) * DistanceI1; Distance1y := Sin(rA1F) * DistanceI1;
        Distance2x := Cos(rA2F) * DistanceI2; Distance2y := Sin(rA2F) * DistanceI2;
        // Determine distances done on display area
        SDistance1x := Round(2 * Distance1x * 100); SDistance1y := Round(2 * Distance1y * 100);
        SDistance2x := Round(2 * Distance2x * 100); SDistance2y := Round(2 * Distance2y * 100);
        // Display ball 1 trajectory
        if (shBall1.Left - Round(2 * rImpactPosition * 100)) mod 10 = 0 then begin
          shTrajectory1[iTraj1].Left := shBall1.Left;
          shTrajectory1[iTraj1].Top := shBall1.Top + (shBall1.Width div 2) - 2;
          shTrajectory1[iTraj1].Visible := True;
          if iTraj1 < 200 then
            Inc(iTraj1);
        end;
        // Display ball 2 trajectory
        if (shBall2.Left - Round(2 * rImpactPosition * 100)) mod 10 = 0 then begin
          shTrajectory2[iTraj2].Left := shBall2.Left;
          shTrajectory2[iTraj2].Top := shBall2.Top + (shBall2.Width div 2) - 2;
          shTrajectory2[iTraj2].Visible := True;
          if iTraj2 < 200 then
            Inc(iTraj2);
        end;
        // Set balls' y-position on display area
        Ball1Top := iImpactBall1T - SDistance1y;
        Ball2Top := iImpactBall2T - SDistance2y;
        if (Ball1Top > Shape1.Top + 15) and (Ball1Top < Shape1.Top + Shape1.Height - shBall1.Height - 15) and
           (Ball1Top > Shape1.Top + 15) and (Ball1Top < Shape1.Top + Shape1.Height - shBall1.Height - 15) then begin
          // If balls remain in display area, set their x-position
          shBall1.Left := iImpactBall1L + SDistance1x; shBall1.Top := Ball1Top;
          shBall2.Left := iImpactBall2L + SDistance2x; shBall2.Top := Ball2Top;
        end
        else
          // If balls leave display area, set flag to stop the simulation
          Stop := True;
      end;
    end;
    // If the balls are still within the display area, display their new position
    if not Stop then begin
      edTime.Text := FloatToStrF(rTime, ffFixed, 0, 2);
      edPosition1.Text := FloatToStrF(rDistance1, ffFixed, 0, 2);
      edPosition2.Text := FloatToStrF(rDistance2 + rBallDistance + (shBall1.Width / 2) / 100, ffFixed, 0, 2);
      if bImpact and (rTime >= rImpactTime) then begin
        rSpeed1 := rV1F; rSpeed2 := rV2F;
      end;
    end;
  end
  else
    Stop := True;
  // If balls are outside display area, stop the simulation (by stopping the timer)
  if Stop then begin
    tiCollisions.Enabled := False;
    btStart.Caption := 'Start';                                                // next button push will be 'start'                                                                               // disable buttons (user has to use "Collision" menu items)
    btStart.Enabled := False; btPause.Enabled := False;
  end;
end;

{ For 2D collisions, automatically set m2 = m1 }

procedure TfCollisions.edMass1Change(Sender: TObject);

begin
  if sCollision = 'el2' then
    edMass2.Text := edMass1.Text;
end;

{ Note that radius value has been changed (and needs to be set by "Fixer" button) }

procedure TfCollisions.edRadiusEditingDone(Sender: TObject);

begin
  edRadius.Color := clYellow;
  bRadiusChanged := True;
end;

{ Note that impact parameter value has been manually changed (and needs to be set by "Fixer" button) }

procedure TfCollisions.edImpactEditingDone(Sender: TObject);

begin
  edImpact.Color := clYellow;
  bImpactChanged := True;
end;


end.

