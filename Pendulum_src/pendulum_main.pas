{**************************************}
{* Main unit for Pendulum application *}
{**************************************}

unit pendulum_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, math, pendulum_help;

type
  {************}
  { TfPendulum }
  {************}
  TfPendulum = class(TForm)
    mMenu: TMainMenu;
    mSimulation, mSimulationExit: TMenuItem;
    mSettings, mSettingsPendulum, mSettingsTime, mSettingsRefLines, mSettingsAutoStop: TMenuItem;
    mSettingsDecimals, mSettingsDecimals2, mSettingsDecimals3, mSettingsDecimals4, mSettingsDecimals7: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imDraw: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18, Label19, Label20: TLabel;
    Label21, Label22, Label23, Label24, Label25, Label26, Label27, Label28, Label29, Label30: TLabel;
    Label31, Label32, Label33, Label34, Label35, Label36, Label37, Label38, laUG: TLabel;
    Shape1, Shape2, Shape3: TShape;
    edM, edL, edTheta, edG: TEdit;
    edS, edK, edF, edT, edOmegaMax, edVMax, edEnergy: TEdit;
    edMovX, edMovY, edMovAngle, edMovT, edMovOmega, edMovV: TEdit;
    edMovPE, edMovKE, edMovEq: TEdit;
    btCalc: TButton;
    btStart: TButton;
    btPause: TButton;
    tiPendulum: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mSimulationExitClick(Sender: TObject);
    procedure mSettingsPendulumClick(Sender: TObject);
    procedure mSettingsTimeClick(Sender: TObject);
    procedure mSettingsDecimals2Click(Sender: TObject);
    procedure mSettingsDecimals3Click(Sender: TObject);
    procedure mSettingsDecimals4Click(Sender: TObject);
    procedure mSettingsDecimals7Click(Sender: TObject);
    procedure mSettingsRefLinesClick(Sender: TObject);
    procedure mSettingsAutoStopClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tiPendulumTimer(Sender: TObject);
  private
    iDecimals, iStep, iPause: Integer;
    rM, rL, rTheta, rG, rS, rF, rK, rT, rOmegaMax, rVMax, rEnergy, rYMax, rLMult, rTMult: Real;
    bPause: Boolean;
    Bitmap : TBitmap;
  end;

var
  fPendulum: TfPendulum;

implementation

{$R *.lfm}

{ Format reals with given number of decimal digits and removing non-significant zeros }

function RFormat(R: Real; F: Integer): string;

var
  RF: Integer;
  RR: Real;

begin
  RR := Abs(R);
  if RR - Int(RR) < Power(10, -(F + 1)) then
    RF := 0
  else if RR * 10 - Int(RR * 10) < Power(10, -(F + 1)) then
    RF := 1
  else if RR * 100 - Int(RR * 100) < Power(10, -(F + 1)) then
    RF := 2
  else
    RF := F;
  Result := FloatToStrF(R, ffFixed, 0, RF);
end;

{ Clean the graph by displaying a white rectangle }

procedure DrawingSurfaceClean(W, H: Integer);

begin
  fPendulum.imDraw.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fPendulum.imDraw.Picture.Bitmap.Canvas.Pen.Width := 1;
  fPendulum.imDraw.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fPendulum.imDraw.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Clean the form: Reset edit fields and clean the graph }

procedure FormClean(W, H: Integer);

begin
  // Reset the form fields
  fPendulum.edS.Text := ''; fPendulum.edF.Text := ''; fPendulum.edK.Text := ''; fPendulum.edT.Text := '';
  fPendulum.edOmegaMax.Text := ''; fPendulum.edVMax.Text := ''; fPendulum.edEnergy.Text := '';
  fPendulum.edMovT.Text := ''; fPendulum.edMovAngle.Text := ''; fPendulum.edMovX.Text := ''; fPendulum.edMovY.Text := '';
  fPendulum.edMovOmega.Text := ''; fPendulum.edMovV.Text := ''; fPendulum.edMovPE.Text := '';
  fPendulum.edMovKE.Text := ''; fPendulum.edMovEq.Text := '';
  // Clean the drawing surface
  DrawingSurfaceClean(W, H);
end;

{ Draw the pendulum at given XY-position }

procedure DrawPendulum(W, H, T, M: Integer; L, Theta, X, Y: Real);

var
  GX, GY: Integer;

begin
  DrawingSurfaceClean(W, H);
  // Draw the reference lines (if so selected)
  if fPendulum.mSettingsRefLines.Checked then begin
    fPendulum.imDraw.Picture.Bitmap.Canvas.Pen.Color := clRed;
    fPendulum.imDraw.Picture.Bitmap.Canvas.Pen.Width := 1;
    fPendulum.imDraw.Picture.Bitmap.Canvas.Line(M, T, M, Round(T + L + 30));
    fPendulum.imDraw.Picture.Bitmap.Canvas.Line(M, T, M + Round((T + H - (T + 20)) * Sin(Theta)), T + H - (T + 20));
    fPendulum.imDraw.Picture.Bitmap.Canvas.Line(M, T, M - Round((T + H - (T + 20)) * Sin(Theta)), T + H - (T + 20));
  end;
  // Draw the pendulum
  fPendulum.imDraw.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  fPendulum.imDraw.Picture.Bitmap.Canvas.Brush.Color := clBlue;
  fPendulum.imDraw.Picture.Bitmap.Canvas.Pen.Width := 3;
  GX := Round(M - 1 + X); GY := Round(T + L - Y);
  fPendulum.imDraw.Picture.Bitmap.Canvas.Line(M - 1, T, GX, GY);
  fPendulum.imDraw.Picture.Bitmap.Canvas.EllipseC(GX, GY + 15, 15, 15);
end;

{************}
{ TfPendulum }
{************}

procedure TfPendulum.FormCreate(Sender: TObject);

const
  SUP_2 = #$C2#$B2;

begin
  laUG.Caption := StringReplace(laUG.Caption, '2', SUP_2, []);
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := imDraw.Width;
  Bitmap.Height := imDraw.Height;
  //Assign the bitmap to the image component (the drawing surface)
  imDraw.Picture.Graphic := Bitmap;
  // Clear the form
  FormClean(imDraw.Width, imDraw.Height);
  rLMult := 5; rTMult := 1; iDecimals := 2;                                    // default pendulum length, simulation time and decimal digits parameters
end;

{ Menu item "Simulation > Exit": Exit application }

procedure TfPendulum.mSimulationExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Pendulum display ...": User entry of pendulum length display parameter }

procedure TfPendulum.mSettingsPendulumClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Pendulum simulation', 'Number of display pixles for 1cm pendulum length', FloatToStr(rLMult));
  if (S <> '') and (StrToFloat(S) > 0) then begin
    rLMult := StrToFloat(S);
  end;
end;

{ Menu item "Settings > Simulation time ...": User entry of simulation time parameter }

procedure TfPendulum.mSettingsTimeClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Pendulum simulation', 'Number of simulation seconds for 1s real time', FloatToStr(rTMult));
  if (S <> '') and (StrToFloat(S) > 0) then begin
    rTMult := StrToFloat(S);
  end;
end;

{ Menu items "Settings > Decimal digits > ": User selection of number of decimal digits to be displayed }

procedure TfPendulum.mSettingsDecimals2Click(Sender: TObject);

begin
  mSettingsDecimals2.Checked := True;  mSettingsDecimals3.Checked := False;
  mSettingsDecimals4.Checked := False; mSettingsDecimals7.Checked := False;
  iDecimals := 2;
end;

procedure TfPendulum.mSettingsDecimals3Click(Sender: TObject);

begin
  mSettingsDecimals2.Checked := False; mSettingsDecimals3.Checked := True;
  mSettingsDecimals4.Checked := False; mSettingsDecimals7.Checked := False;
  iDecimals := 3;
end;

procedure TfPendulum.mSettingsDecimals4Click(Sender: TObject);

begin
  mSettingsDecimals2.Checked := False; mSettingsDecimals3.Checked := False;
  mSettingsDecimals4.Checked := True; mSettingsDecimals7.Checked := False;
  iDecimals := 4;
end;

procedure TfPendulum.mSettingsDecimals7Click(Sender: TObject);

begin
  mSettingsDecimals2.Checked := False; mSettingsDecimals3.Checked := False;
  mSettingsDecimals4.Checked := False; mSettingsDecimals7.Checked := True;
  iDecimals := 7;
end;

{ Menu item "Settings > Display reference lines": Toggle display of reference lines or not }

procedure TfPendulum.mSettingsRefLinesClick(Sender: TObject);

begin
  if mSettingsRefLines.Checked then
    mSettingsRefLines.Checked := False
  else
    mSettingsRefLines.Checked := True;
end;

{ Menu item "Settings > Pause at reference lines": Toggle automatically pausing at reference lines or not }

procedure TfPendulum.mSettingsAutoStopClick(Sender: TObject);

begin
  if mSettingsAutoStop.Checked then
    mSettingsAutoStop.Checked := False
  else
    mSettingsAutoStop.Checked := True;
end;

{ Menu item "Help > Help": Display of application help }

procedure TfPendulum.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display of application about }

procedure TfPendulum.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics:' + LineEnding;
  S += 'Simulation of a simple pendulum' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April-December 2020.';
  MessageDlg('About "Pendulum"', S, mtInformation, [mbOK], 0);
end;

{ Button "Compute": Caculate values for actual pendulum }

procedure TfPendulum.btCalcClick(Sender: TObject);

var
  Ret: Cardinal;
  Mess: string;

begin
  // Get mass, length, angle and g from form (user entries)
  Mess := '';
  if edM.Text = '' then
    rM := 0
  else
    rM := StrToFloat(edM.Text);
  if edL.Text = '' then
    rL := 0
  else
    rL := StrToFloat(edL.Text);
  if edTheta.Text = '' then
    rTheta := 0
  else
    rTheta := StrToFloat(edTheta.Text);
  if edG.Text = '' then
    rG := 0
  else
    rG := StrToFloat(edG.Text);
  // Check values, entered by user
  FormClean(imDraw.Width, imDraw.Height);
  if rM <= 0 then begin
    Mess := 'The pendulum mass must be greater than 0';
    edM.SetFocus;
  end
  else if rL <= 0 then begin
    Mess := 'The pendulum length must be greater than 0';
    edL.SetFocus;
  end
  else if rTheta <= 0 then begin
    Mess := 'The displacement angle must be greater than 0';
    edTheta.SetFocus;
  end
  else if rTheta > 15 then begin
    // Formulas used are a simplification, only valid for small angles
    Mess := 'The displacement angle should be less than 15°';
    edTheta.SetFocus;
  end
  else if rG <= 0 then begin
    Mess := 'The acceleration due to gravity must be greater than 0';
    edG.SetFocus;
  end
  else if (rG < 9.78) or (rG > 9.83) then begin
    // Let user the possibility to use any g (simulation of pendulum on the moon...)
    Ret := MessageDlg('Pendulum simulation', 'Sure to use this acceleration due to gravity ?', mtWarning, [mbYes, mbNo], 0);
    if Ret = mrNo then begin
      Mess := 'Error';
      edG.SetFocus;
    end;
  end
  else if rL * rLMult > imDraw.Height - 75 then begin
    // Display warning, if, with the actual pendulum display settings, the pendulum does not fit on the page. Give user the possibility
    // to change the settings or to do the calculations without simulation
    Ret := MessageDlg('Pendulum simulation', 'Pendulum to long for simulation display. Continue anyway ?', mtWarning, [mbYes, mbNo], 0);
    if Ret = mrYes then begin
      Mess := 'NoSimul';
    end
    else begin
      Mess := 'Error';
      edL.SetFocus;
    end;
  end;
  if (Mess = '') or (Mess = 'NoSimul') then begin
    // Calculate pendulum values (for formulae, cf. a Physics book or website)
    rTheta := DegToRad(rTheta); rL := rL / 100;
    rS := rL * rTheta; edS.Text := RFormat(100 * rS, iDecimals);
    rF := -((rM * rG / rL) * rS); edF.Text := RFormat(rF, iDecimals);
    rK := rM * rG / rL; edK.Text := RFormat(rK, iDecimals);
    rT := 2 * Pi * Sqrt(rM / rK); edT.Text := RFormat(rT, iDecimals);
    rOmegaMax := Sqrt(rG / rL) * rTheta; edOmegaMax.Text := RFormat(rOmegaMax, iDecimals);
    rVMax := Sqrt(rK / rM) * rL * rTheta; edVMax.Text := RFormat(rVMax, iDecimals);
    rEnergy := 0.5 * rM * Sqr(rL) * Sqr(rOmegaMax) + 0.5 * rM * rG * rL * Sqr(0); edEnergy.Text := RFormat(rEnergy, iDecimals);
    rYmax := rL * (1 - Cos(rTheta)); edMovEq.Text := 'y(t) = ' + RFormat(1000 * rYmax, iDecimals) + '·sin(' + RFormat(2 * Pi / rT, iDecimals) + 't)';
    if Mess = '' then begin
      // Prepare for the simulation (done if user pushes the "Start" button)
      DrawPendulum(imDraw.Width, imDraw.Height, 25, imDraw.Width div 2, rL * 100 * rLMult, rTheta, 0, 0);
      btStart.Caption := 'Start'; btStart.Enabled := True; btStart.SetFocus;
    end;
  end
  else begin
    // Display error message if user values not valid
    if Mess <> 'Error' then
      // Mess was set to 'Error' for unusual values, with message, asking user to continue or not, already displayed
      MessageDlg('Pendulum simulation', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Button "Start/Stop": Start resp. stop simple pendulum simulation }

procedure TfPendulum.btStartClick(Sender: TObject);

// The code for the simulaion itself is within the timer routine

begin
  // Button "Start": Start simulation
  if btStart.Caption = 'Start' then begin
    btPause.Enabled := True; btPause.Caption := 'Pause';
    btStart.Caption := 'Stop'; btPause.SetFocus;
    // Simulation start values
    iStep := 0;                                                                // simulation step (used for time calculation)
    bPause := False; iPause := Round(10 * rTMult);                             // variables used for auto-pause at reference lines
    // Start the simulation timer
    tiPendulum.Enabled := True;
  end
  else begin
    // Button "Stop": Stop simulation
    tiPendulum.Enabled := False;                                               // stop simulaion timer
    btStart.Caption := 'Start'; btStart.Enabled := False;
    btPause.Caption := 'Pause'; btStart.Enabled := False;
    edM.SetFocus;
  end;
end;

{ Button "Pause/Resume": Pause resp. resume simple pendulum simulation }

procedure TfPendulum.btPauseClick(Sender: TObject);

begin
  if btPause.Caption = 'Pause' then begin
    // Button "Pause": Stop simulation timer
    tiPendulum.Enabled := False;
    btPause.Caption := 'Resume';
  end
  else begin
    // Button "Resume": Restart simulation timer
    btPause.Caption := 'Pause';
    tiPendulum.Enabled := True;
  end;
  btPause.SetFocus;
end;

{ Timer routine: Do the simple pendulum simulation }

procedure TfPendulum.tiPendulumTimer(Sender: TObject);

var
  Dir: Integer;
  SimulTime, X, Y, Alpha, V, Omega, PE, KE: Real;

begin
  // In the case of a real time simulation, simulation time increases by 10msec (timer interval) each time the routine is entered
  // (rTMult = 1). If the user choose to run the simulation in slow motion (rTMult > 1), the increase of simulation time is only a
  // fraction of these 10msec
  SimulTime := iStep * 0.01 / rTMult;
  // The pendulum movement may be considered as an oscillation; the Y-displacement may be calculated by a sine function
  Y := rYmax * Sin(2 * Pi * SimulTime / rT);
  // The real Y value is of course always positive. Instead, the pendulum moves to the right or to the left. Using the lowest pendulum
  // position as origin, the X-displacement, the actual angle and the velocity may be signed. These adaptions are done with the help
  // of the variable Dir.
  Dir := 1;
  if Y < 0 then
    Dir := -1;
  edMovT.Text := FloatToStrF(SimulTime, ffFixed, 0, 2);
  edMovY.Text := RFormat(Abs(Y * 1000), iDecimals);
  // Calculation of actual angle and X-displacement (with consideration of Dir variable!)
  Alpha := ArcCos((rL - Dir * Y) / rL);
  X := rL * Sin(Dir * Alpha);
  edMovAngle.Text := RFormat(RadToDeg(Dir * Alpha), iDecimals);
  edMovX.Text := RFormat(X * 1000, iDecimals);
  // Calculation of actual velocity (linear velocity may be considered as a cosine function, derived from the y displacement sine function)
  V := rVMax * Cos((2 * Pi * SimulTime) / rT);
  Omega := V / rL;
  edMovV.Text := RFormat(V, iDecimals);
  edMovOmega.Text := RFormat(Omega, iDecimals);
  // Calculation of actual potential and kinetic energy
  PE := 0.5 * rM * rG * rL * Sqr(Alpha);
  KE := 0.5 * rM * Sqr(rL) * Sqr(Omega);
  edMovPE.Text := RFormat(PE, iDecimals);
  edMovKE.Text := RFormat(KE, iDecimals);
  // Draw the pendulum at actual XY-position
  DrawPendulum(imDraw.Width, imDraw.Height, 25, imDraw.Width div 2, rL * 100 * rLMult, rTheta, X * 100 * rLMult, Abs(Y * 100 * rLMult));
  // Autopause at refernce lines, if so selected
  // The autopause code has to be reviwed. Basing the detection of the reference lines on the Y-displacement works well for y = Ymax
  // (angle = theta), but the lowest pendulum position (y = 0, angle = 0) is mostly missed. This is due to the fact, that the velocity
  // tends to its maximum, when the pendulum approaches this position and during 10msec the Y-displacement changes quite a lot. The
  // 0.002 used below pauses the simulation at least from time to time and will be ok, if the simulation is run in slow-motion. Increasing
  // this value is not an option, as this would pause the simulaion with the pendulum to far away from its lowest position...
  // Pausing the simulation for angle = theta also causes a problem. As the velocity tends to zero, just checking the actual Y-displacement
  // would action the "Pause" button several times. I solved this issue by using the Boolean bPause, that must be True to actually pause.
  // If pause has been done, the variable is set to False and a counter is set. This counter is decremented each time, the routine is entered
  // and when it is zero (enough time passed that y is reasonably less than Ymax), bPause is set True again and pausing is enabled again.
  if mSettingsAutoStop.Checked then begin
    if bPause and ((rYMax - Abs(Y) < 0.0001 * rYMax) or (Abs(Y) < 0.002 * rYMax)) then begin
      btPause.Click;
      bPause := False;
      iPause := Round(10 * rTMult);
    end;
    if not bPause then begin
      Dec(iPause);
      if iPause = 0 then
        bPause := True;
    end;
  end;
  // Increment the Step value (=> advance of time by 10msec resp. a fraction of it, depending on rTMult)
  Inc(iStep);
end;

end.

