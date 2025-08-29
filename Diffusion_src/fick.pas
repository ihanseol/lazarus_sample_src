{***************************************}
{* Main unit for Diffusion application *}
{***************************************}

unit fick;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

const
  SoluteMax = 1000;

type
  {*************}
  { TfDiffusion }
  {*************}
  TfDiffusion = class(TForm)
    mMenu: TMainMenu;
    mSimul, mSimulExit: TMenuItem;
    mSettings, mSettingsTime, mSettingsDisplay, mSettingsSameVols: TMenuItem;
    mSettingsDisplay3, mSettingsDisplay6, mSettingsDisplay9: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    edDetails: TMemo;
    imDisplay: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label16, Label17, Label20, Label21, Label22, Label23: TLabel;
    laV1, laQ10, laV2, laTUnit, laQ1, laQ2, laC1, laC2: TLabel;
    edT, edQ1, edQ2, edC1, edC2: TEdit;
    edV1, edQ10, edV2, edA, edX, edD, edC: TEdit;
    stD: TStaticText;
    btStart: TButton;
    btPause: TButton;
    tiDiffusion: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mSimulExitClick(Sender: TObject);
    procedure mSettingsTimeClick(Sender: TObject);
    procedure mSettingsDisplay3Click(Sender: TObject);
    procedure mSettingsDisplay6Click(Sender: TObject);
    procedure mSettingsDisplay9Click(Sender: TObject);
    procedure mSettingsSameVolsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tiDiffusionTimer(Sender: TObject);
  private
    iDisplayWidth, iDisplayHeight, iCW1, iCW2, iCH, iSolute2, iSolute2Old, iStep, iSimulTime, iPrecision: Integer;
    rV1, rV2, rQ10, rA, rX, rT: Real;
    rC10, rC1, rC2, rQ1, rQ2, rC, rD, rK: Double;
    bmBitmap : TBitmap;
    shSolute: array[0 .. SoluteMax - 1] of TShape;
  end;

var
  fDiffusion: TfDiffusion;

implementation

{$R *.lfm}

{ Format real number (at given number of significant decimal digits) }

function RFormat(R: Real; Precision: Integer): string;

var
  I: Integer;
  SR: string;

begin
  SR := FloatToStrF(R, ffFixed, Precision, Precision);
  I := Length(SR);
  while SR[I] = '0' do
    Dec(I);
  SR := LeftStr(SR, I);
  if (RightStr(SR, 1) = ',') or (RightStr(SR, 1) = '.') then
    Delete(SR, Length(SR), 1);
  if SR = '' then
    SR := '0';
  Result := SR;
end;

{ Clear the display (by showing a white rectangle) }

procedure DisplayClear(W, H: Integer);

begin
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 1;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Clear the form }

procedure FormClear(W, H: Integer);

begin
  DisplayClear(W, H);
  fDiffusion.edC.Text := '';
  fDiffusion.edT.Text := '';
  fDiffusion.edQ1.Text := ''; fDiffusion.edQ2.Text := '';
  fDiffusion.edC1.Text := ''; fDiffusion.edC2.Text := '';
end;

{ Reset the simulation (clear all) }

procedure SimulReset(W, H: Integer; var Solute: array of TShape);

var
  I: Integer;

begin
  fDiffusion.tiDiffusion.Enabled := False;
  FormClear(W, H);
  for I := 0 to SoluteMax - 1 do
    Solute[I].Visible := False;
  fDiffusion.btStart.Caption := 'Start';
  fDiffusion.btPause.Caption := 'Pause';
end;

{ Draw the compartments (rectangles with dimensions proportional to compartment volume) }

procedure CompartmentsDraw(W, H: Integer; V1, V2: Real; var Solute: array of TShape; out CW1, CW2, CH: Integer);

var
  I: Integer;
  RW1, RW2: Real;

begin
  CH := H - 50;
  // Rectangles width (if volumes are equal)
  CW1 := W div 2 - 50; CW2 := CW1;
  // Adapt rectangles width (if volumes are different)
  if V1 <> V2 then begin
    if V1 > V2 then begin
      RW1 := CW1; RW2 := RW1 * (V2 / V1);                                           // use Real variables here, to avoid rounding twice!
    end
    else begin
      RW2 := CW2; RW1 := RW2 * (V1 / V2);
    end;
    // Extend the proportinal width, in order to use whole display area width
    CW1 := Round(RW1 * ((W - 100) / (RW1 + RW2)));
    CW2 := Round(RW2 * ((W - 100) / (RW1 + RW2)));
  end;
  // Draw the 2 rectangles
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Pen.Width := 3;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Rectangle(50, 25, 50 + CW1, 25 + CH);
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Rectangle(50 + CW1, 25, 50 + CW1 + CW2, 25 + CH);
  // Whipe-out the internal compartment borders, just letting a small line (= membrane)
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fDiffusion.imDisplay.Picture.Bitmap.Canvas.Line(50 + CW1, 28, 50 + CW1, 22 + CH);
  // "Move" all solute shapes into the left compartment
  for I := 0 to SoluteMax - 1 do begin
    Solute[I].Left := fDiffusion.imDisplay.Left + 50 + Random(CW1 - 25) + 10;
    Solute[I].Top  := fDiffusion.imDisplay.Top + 25 + Random(CH - 25) + 10;
    Solute[I].Visible := True;                                                      // make solute shapes visible
  end;
end;

{*************}
{ TfDiffusion }
{*************}

{ Application start: Initialisation }

procedure TfDiffusion.FormCreate(Sender: TObject);

const
  SUB_0 = #$E2#$82#$80;
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  SUP_MINUS = #$E2#$81#$BB;
  SUP_5 = #$E2#$81#$B5;
  SUP_6 = #$E2#$81#$B6;

var
  I: Integer;

begin
  // Create a bitmap object and assign dimensions
  iDisplayWidth := imDisplay.Width; iDisplayHeight := imDisplay.Height;
  bmBitmap := TBitmap.Create;
  bmBitmap.Width := iDisplayWidth;
  bmBitmap.Height := iDisplayHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imDisplay.Picture.Graphic := bmBitmap;
  // Clear the form (draw display area)
  FormClear(iDisplayWidth, iDisplayHeight);
  // Create solute shapes
  for I := 0 to SoluteMax - 1 do begin
    shSolute[I] := TShape.Create(shSolute[I]);
    shSolute[I].Parent := Self;
    shSolute[I].Shape := stCircle;
    shSolute[I].Width := 10; shSolute[I].Height := 10;
    shSolute[I].Brush.Color := clBlue; shSolute[I].Pen.Color := clBlue;
    shSolute[I].Visible := False;
  end;
  // Applay subscripts to description text and labels
  edDetails.Text := StringReplace(edDetails.Text, 'V1', 'V' + SUB_1, []);
  edDetails.Text := StringReplace(edDetails.Text, 'V2', 'V' + SUB_2, []);
  edDetails.Text := StringReplace(edDetails.Text, 'q10', 'q' + SUB_1 + SUB_0, []);
  edDetails.Text := StringReplace(edDetails.Text, 'c1', 'c' + SUB_1, []);
  edDetails.Text := StringReplace(edDetails.Text, 'c2', 'c' + SUB_2, []);
  laQ1.Caption := StringReplace(laQ1.Caption, 'q1', 'q' + SUB_1, []);
  laQ2.Caption := StringReplace(laQ2.Caption, 'q2', 'q' + SUB_2, []);
  laC1.Caption := StringReplace(laC1.Caption, 'c1', 'c' + SUB_1, []);
  laC2.Caption := StringReplace(laC2.Caption, 'c2', 'c' + SUB_2, []);
  stD.Caption := StringReplace(stD.Caption, '1E-5', '10' + SUP_MINUS + SUP_5, []);
  stD.Caption := StringReplace(stD.Caption, '1E-6', '10' + SUP_MINUS + SUP_6, []);
  laV1.Caption := StringReplace(laV1.Caption, 'V1', 'V' + SUB_1, []);
  laV2.Caption := StringReplace(laV2.Caption, 'V2', 'V' + SUB_2, []);
  laQ10.Caption := StringReplace(laQ10.Caption, 'q10', 'q' + SUB_1 + SUB_0, []);
  // Set start-up values
  iSimulTime := 10;                                                                 // simulation time: 1sec corr. to 10h real time
  iPrecision := 6;                                                                  // amounts and concentrations display with 6 decimal digits
end;

{ Menu item "Simulation > Exit": Exit application }

procedure TfDiffusion.mSimulExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Simulation time ...": User entry of simulation time }

procedure TfDiffusion.mSettingsTimeClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Simulation time', 'Enter simulation time (h) for 1s of real time', IntToStr(iSimulTime));
  if S <> '' then begin
    // Apply if user pushed OK button
    iSimulTime := StrToInt(S);
    if (iSimulTime >= 24) and (iSimulTime mod 24 = 0) then
      // For times > 24h, display as days (if time is multiple of 24)
      laTUnit.Caption := 'days'
    else
      laTUnit.Caption := 'h';
  end;
end;

{ Menu items "Settings > Concentrations display > ...": Choose number of decimal disgits for amounts and concentrations display }

procedure TfDiffusion.mSettingsDisplay3Click(Sender: TObject);

begin
  mSettingsDisplay3.Checked := True; mSettingsDisplay6.Checked := False; mSettingsDisplay9.Checked := False;
  iPrecision := 3;
end;

procedure TfDiffusion.mSettingsDisplay6Click(Sender: TObject);

begin
  mSettingsDisplay3.Checked := False; mSettingsDisplay6.Checked := True; mSettingsDisplay9.Checked := False;
  iPrecision := 6;
end;

procedure TfDiffusion.mSettingsDisplay9Click(Sender: TObject);

begin
  mSettingsDisplay3.Checked := False; mSettingsDisplay6.Checked := False; mSettingsDisplay9.Checked := True;
  iPrecision := 9;
end;

{ Menu item "Settings > Use same volumes": Toggle V2 = V1 or V2 = entered by user }

procedure TfDiffusion.mSettingsSameVolsClick(Sender: TObject);

begin
  if mSettingsSameVols.Checked then begin
    mSettingsSameVols.Checked := False;
    edV2.Enabled := True;
  end
  else begin
    mSettingsSameVols.Checked := True;
    edV2.Text := edV1.Text; edV2.Enabled := False;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfDiffusion.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry simulation:' + LineEnding;
  S += 'Compartmental modeling: Fick''s Law of diffusion.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2020.';
  MessageDlg('About "Diffusion"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Stop": Run resp. stop the simulation }

procedure TfDiffusion.btStartClick(Sender: TObject);

var
  Ret: Cardinal;
  Mess, Mess2: string;

begin
  Mess := ''; Mess2 := '';
  // Button "Start": Run the simulation
  if btStart.Caption = 'Start' then begin
    // Read simulation parameters from form
    if edV1.Text = '' then
      rV1 := 0
    else
      rV1 := StrToFloat(edV1.Text);
    if edQ10.Text = '' then
      rQ10 := 0
    else
      rQ10 := StrToFloat(edQ10.Text);
    if mSettingsSameVols.Checked then
      edV2.Text := edV1.Text;                                                       // set V2 = V1 (if this option is selected)
    if edV2.Text = '' then
      rV2 := 0
    else
      rV2 := StrToFloat(edV2.Text);
    if edA.Text = '' then
      rA := 0
    else
      rA := StrToFloat(edA.Text);
    if edX.Text = '' then
      rX := 0
    else
      rX := StrToFloat(edX.Text);
    if edD.Text = '' then
      rD := 0
    else
      rD := StrToFloat(edD.Text);
    // (Elementary) validity check of user entered parameters
    if rV1 <= 0 then begin
      Mess := 'Volume of compartment 1 must be greater than 0!';
      edV1.SetFocus;
    end
    else if rQ10 <= 0 then begin
      Mess := 'Initial solute amount must be greater than 0!';
      edQ10.SetFocus;
    end
    else if rV2 <= 0 then begin
      Mess := 'Volume of compartment 2 must be greater than 0!';
      edV2.SetFocus;
    end
    else if rA <= 0 then begin
      Mess := 'Membrane surface area must be greater than 0!';
      edA.SetFocus;
    end
    else if rX <= 0 then begin
      Mess := 'Membrane thickness be greater than 0!';
      edX.SetFocus;
    end
    else if rD <= 0 then begin
      Mess := 'Diffusion coefficient must be greater than 0!';
      edD.SetFocus;
    end
    else if (rD < 1E-7) or (rD > 1E-4) then begin
      // Consider diffusion coefficients to far out of 10e-6 ... 10e-5 range as invalid
      Mess := 'Invalid diffusion coefficient!';
      edD.SetFocus;
    end
    else if (rD < 5E-7) or (rD > 5E-5) then begin
      // Consider diffusion coefficients somewhat out of 10e-6 ... 10e-5 range as uncommon
      // and ask user, if she wants to use them anyway
      Mess2 := 'Uncommon diffusion coefficient!';
      edD.SetFocus;
    end;
    if Mess = '' then begin
      // No erroneous parameters entered
      if Mess2 <> '' then begin
        // Ask user if she wants to use "uncommon" diffusion coefficients
        Mess2 += ' Continue anyway?';
        Ret := MessageDlg('Data check', Mess2, mtWarning, [mbYes, mbNo], 0);
        if Ret = mrYes then begin
          // If yes, consider ok to start simulation with these values
          btStart.SetFocus;
          Mess2 := '';
        end;
      end;
    end
    else begin
      // Some parameter entered is erroneous
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
    end;
    if (Mess = '') and (Mess2 = '') then begin
      // If all ok, run the simulation (= start the timer)
      mSettingsTime.Enabled := False;                                               // block user access to simulation time settings
      SimulReset(iDisplayWidth, iDisplayHeight, shSolute);                          // clear all
      rD *= 1E-4;                                                                   // from cm² to m²
      rA *= 1E-4;                                                                   // from cm² to m²
      rX *= 1E-6;                                                                   // from µm to m
      rC10 := rQ10 / rV1; rK := rD * rA / rX;                                       // initial concentration
      // Calculate steady state concentration
      if rV1 = rV2 then
        rC := rC10 / 2
      else
        rC := rQ10 / (rV1 + rV2);
      edC.Text := RFormat(rC, iPrecision);                                          // display steady state concentration
      DisplayClear(iDisplayWidth, iDisplayHeight);
      // Draw the compartments
      CompartmentsDraw(iDisplayWidth, iDisplayHeight, rV1, rV2, shSolute, iCW1, iCW2, iCH);
      // Enable the simulation timer (all simulation code in the timer routine)
      iStep := 0; iSolute2Old := 0;
      tiDiffusion.Enabled := True;
      btStart.Caption := 'Stop';                                                    // next button push will be to stop the simulation
    end;
  end
  // Button "Stop": Stop the simulation
  else begin
    tiDiffusion.Enabled := False;                                                   // disable the timer
    mSettingsTime.Enabled := True;                                                  // give user access to simulation time setting
    btStart.Caption := 'Start';
  end;
end;

{ Button "Pause/Resume": Pause resp. resume the simulation }

procedure TfDiffusion.btPauseClick(Sender: TObject);

begin
  if btStart.Caption = 'Stop' then begin
    if btPause.Caption = 'Pause' then begin
      tiDiffusion.Enabled := False;
      btPause.Caption := 'Resume';
    end
    else begin
      tiDiffusion.Enabled := True;
      btPause.Caption := 'Pause';
    end;
  end;
end;

{ Simulation timer routine (all simulation code in this routine) }

procedure TfDiffusion.tiDiffusionTimer(Sender: TObject);

var
  SoluteDiff, IX, I: Integer;

begin
  rT := iStep * iSimulTime * 3600;                                                  // real time corr. to iStep seconds simulation time
  // Calculate concentration in both compartments
  if rV1 = rV2 then begin
    // Equal compartment volumes
    rC1 := (rC10 / 2) * (Exp(-2 * rK * rT / rV1)  + 1);
    rC2 := (rC10 / 2) * (1 - Exp(-2 * rK * rT / rV1));
  end
  else begin
    // Different compartment volumes
    rC1 := (rC10 / (rV1 + rV2)) * (rV2 * Exp((-rK * (rV1 + rV2) * rT) / (rV1 * rV2)) + rV1);
    rC2 := (rV1 * rC10 / (rV1 + rV2)) * (1 - Exp((-rK * (rV1 + rV2) * rT) / (rV1 * rV2)));
  end;
  // Calculate corr. solute amounts
  rQ1 := rC1 * rV1; rQ2 := rC2 * rV2;
  // Display time, amounts and concentrations
  if laTUnit.Caption = 'h' then
    edT.Text  := FloatToStr(rT / 3600)                                              // time displayed in hours
  else
    edT.Text  := FloatToStr(rT / (24 * 3600));                                      // time displayed in days
  edQ1.Text := RFormat(rQ1, iPrecision); edC1.Text := RFormat(rC1, iPrecision);
  edQ2.Text := RFormat(rQ2, iPrecision); edC2.Text := RFormat(rC2, iPrecision);
  // Determine number of solute shapes to display in each compartment
  iSolute2 := Round((rQ2 / rQ10) * SoluteMax);
  // Determine, how many solute shapes have to be "moved" from compartment 1 to compartment 2
  SoluteDiff := iSolute2 - iSolute2Old;
  iSolute2Old := iSolute2;
  // "Move" solute shapes from compartment 1 to compartment 2
  for I := 1 to SoluteDiff do begin
    repeat
      IX := Random(SoluteMax);                                                      // choose a random shape...
    until shSolute[IX].Left < imDisplay.Left + 50 + iCW1;                           // that must actually be in compartment 1, of course
    shSolute[IX].Left := fDiffusion.imDisplay.Left + 50 + iCW1 + Random(iCW2 - 25) + 10;
    shSolute[IX].Top  := fDiffusion.imDisplay.Top + 25 + Random(iCH - 25) + 10;     // "move" the shape by changing its "Left" property value
  end;
  // Increment simulation time by 1 second (timer interval is set 1000 ms)
  Inc(iStep);
  // If concentration in compartments equal the steady state concentration, stop the simulation
  if (RFormat(rC1, iPrecision) = RFormat(rC, iPrecision)) and (RFormat(rC2, iPrecision) = RFormat(rC, iPrecision)) then begin
    tiDiffusion.Enabled := False;
    btStart.Caption := 'Start'; btPause.Caption := 'Pause';
    mSettingsTime.Enabled := True;                                                  // give user access to simultion time setting
  end;
end;

end.

