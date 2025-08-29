{*********************************************}
{* Main unit for PlanetaryOrbits application *}
{*********************************************}

unit orbits_simul;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Menus, ExtCtrls, orbits_draw, orbits_classes, orbits_help;

type
  {**********}
  { TfOrbits }
  {**********}
  TfOrbits = class(TForm)
    mMenu: TMainMenu;
    mSimulation, mSimulationExit, mSettings, MenuItem1, MenuItem2: TMenuItem;
    mSettingsTimeStep, mSettingsTimeStepDay, mSettingsTimeStepWeek, mSettingsTimeStepMonth: TMenuItem;
    mSettingsTimer, mSettingsTimer1000, mSettingsTimer100, mSettingsTimer10, mSettingsTimer1: TMenuItem;
    mSettingsOrbits, mSettingsPlanets, mSettingsImages, mSettingsScaling, mSettingsRevolutionOff, mSettingsODrawing: TMenuItem;
    mHelp, mHelpHelp, mHelpTechnical, mHelpAbout: TMenuItem;
    laSimulation, Label1: TLabel;
    cbMercury, cbVenus, cbEarth, cbMars: TCheckBox;
    cbJupiter, cbSaturn, cbUranus, cbNeptune, cbPluto: TCheckBox;
    grOrbits: TStringGrid;
    btAll: TButton;
    btNone: TButton;
    btStart: TButton;
    btPause: TButton;
    btOrbits: TButton;
    tiOrbits: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure mSimulationExitClick(Sender: TObject);
    procedure mSettingsTimeStepDayClick(Sender: TObject);
    procedure mSettingsTimeStepWeekClick(Sender: TObject);
    procedure mSettingsTimeStepMonthClick(Sender: TObject);
    procedure mSettingsTimer1000Click(Sender: TObject);
    procedure mSettingsTimer100Click(Sender: TObject);
    procedure mSettingsTimer10Click(Sender: TObject);
    procedure mSettingsTimer1Click(Sender: TObject);
    procedure mSettingsOrbitsClick(Sender: TObject);
    procedure mSettingsPlanetsClick(Sender: TObject);
    procedure mSettingsImagesClick(Sender: TObject);
    procedure mSettingsScalingClick(Sender: TObject);
    procedure mSettingsRevolutionOffClick(Sender: TObject);
    procedure mSettingsODrawingClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpTechnicalClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btAllClick(Sender: TObject);
    procedure btNoneClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btOrbitsClick(Sender: TObject);
    procedure tiOrbitsTimer(Sender: TObject);
  private
    iPlanets, iBodies, iStep, iTimeStep, iTimeStepSecs, iSunDrawSize: Cardinal;
    sTimeStep: string;
    rScaleX, rScaleY: Double;
    bStarted, bUpdateInfo: Boolean;
    aPlanets: array of Cardinal;
    Bodies: array of TBody;
    BodiesInfo: array of TOrbitInfo;
    cbPlanets: array[1..9] of TCheckBox;
  end;

const
  AU = 149.6E+6 * 1000;                                                        // 149.6 million km, in meters
  SunMass = 1.98892E+30; SunVolume = 1412E+15;
  clOrange = $000080FF; clMauve = $00FF8080; clHotPink = $00B469FF;
  PlanetNames: array[1..9] of string = (
    'Mercury', 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Pluto'
  );
  PlanetMasses: array[1..9] of Double = (
    0.33011E+24, 4.8675E+24, 5.9724E+24, 0.64171E+24, 1898.19E+24, 568.34E+24, 86.813E+24, 102.413E+24, 0.01303E+24
  );
  PlanetVolumes: array[1..9] of Double = (
    6.083E+10, 92.843E+10, 108.321E+10, 16.318E+10, 143128E+10, 82713E+10, 6833E+10, 6254E+10, 0.697E+10
  );
  PlanetOPeriods: array[1..9] of Double = (
    87.969, 224.701, 365.256, 686.980, 4332.589, 10759.22, 30685.4, 60189, 90560
  );
  PlanetPosX: array[1..9] of Double = (
    0.387, 0.723, 1, 1.524, 5.204, 9.582, 19.201, 30.047, 39.482
  );
  PlanetVelY: array[1..9] of Double = (
    47.36, -35.02, 29.78, 24.07, 13.06, 9.68, -6.80, 5.43, 4.67
  );
  PlanetDrawColors: array[1..9] of TColor = (
    clHotPink, clLime, clBlue, clRed, clOrange, clFuchsia, clAqua, clMauve, clWhite
  );

var
  fOrbits: TfOrbits;

implementation

{$R *.lfm}

procedure PlanetsSelect(Select: Boolean);

var
  I: Cardinal;

begin
  for I := 1 to 9 do
    fOrbits.cbPlanets[I].Checked := Select;
end;

procedure DisplayBodiesInfo(var BodiesInfo: array of TOrbitInfo; Step: Cardinal; STimeStep: string);

var
  I: Cardinal;

begin
  fOrbits.laSimulation.Caption := 'Simulation - ' + STimeStep + ' = ' + IntToStr(Step) + ':';
  for I := 0 to Length(BodiesInfo) - 1 do begin
    if BodiesInfo[I].Position.X > 0 then
      fOrbits.grOrbits.Cells[I + 1, 4] := ' ' + FloatToStrF(BodiesInfo[I].Position.X, ffExponent, 6, 2)
    else
      fOrbits.grOrbits.Cells[I + 1, 4] := FloatToStrF(BodiesInfo[I].Position.X, ffExponent, 6, 2);
    if BodiesInfo[I].Position.Y > 0 then
      fOrbits.grOrbits.Cells[I + 1, 5] := ' ' + FloatToStrF(BodiesInfo[I].Position.Y, ffExponent, 6, 2)
    else
      fOrbits.grOrbits.Cells[I + 1, 5] := FloatToStrF(BodiesInfo[I].Position.Y, ffExponent, 6, 2);
    if BodiesInfo[I].Velocity.X > 0 then
      fOrbits.grOrbits.Cells[I + 1, 6] := ' ' + FloatToStrF(BodiesInfo[I].Velocity.X, ffExponent, 6, 2)
    else
      fOrbits.grOrbits.Cells[I + 1, 6] := FloatToStrF(BodiesInfo[I].Velocity.X, ffExponent, 6, 2);
    if BodiesInfo[I].Velocity.Y > 0 then
      fOrbits.grOrbits.Cells[I + 1, 7] := ' ' + FloatToStrF(BodiesInfo[I].Velocity.Y, ffExponent, 6, 2)
    else
      fOrbits.grOrbits.Cells[I + 1, 7] := FloatToStrF(BodiesInfo[I].Velocity.Y, ffExponent, 6, 2);
  end;
end;

procedure DrawOrbits(var BodiesInfo: array of TOrbitInfo; Step, TimeStep: Cardinal; STimeStep: string;
  ScaleX, ScaleY: Double; SunDrawSize: Cardinal; var Planets: array of Cardinal);

const
  PlanetDrawRadiusX: array[1..9] of Cardinal = (
    6, 9, 10, 7, 25, 30, 15, 15, 4
  );
  PlanetDrawRadiusY: array[1..9] of Cardinal = (
    6, 9, 10, 7, 25, 21, 15, 15, 4
  );

var
  NPlanets, X, Y, StepDays, I: Cardinal;
  ODrawDays: Double;
  ODraw: Boolean;

begin
  NPlanets := Length(BodiesInfo) - 1; StepDays := Step * TimeStep;
  fDraw.edStep.Text := STimeStep + ' = ' + IntToStr(Step);
  for I := 0 to NPlanets do begin
    if BodiesInfo[I].Body = 'Sun' then begin
      if fOrbits.mSettingsPlanets.Checked then begin
        X := 773 + Round((BodiesInfo[I].Position.X) * ScaleX);
        Y := 400 + Round((BodiesInfo[I].Position.Y) * ScaleY);
      end
      else begin
        X := 773; Y := 400;
      end;
      if SunDrawSize = 10 then begin
        fDraw.shSun10.Left := X - 5; fDraw.shSun10.Top := Y - 5; fDraw.shSun10.Visible := True;
      end
      else begin
        fDraw.shSun5.Left := X - 2; fDraw.shSun5.Top := Y - 2; fDraw.shSun5.Visible := True;
      end;
    end
    else begin
      X := 773 + Round((BodiesInfo[I].Position.X) * ScaleX);
      Y := 400 + Round((BodiesInfo[I].Position.Y) * ScaleY);
      if not fOrbits.mSettingsOrbits.Checked then
        ODraw := False
      else begin
        ODraw := True;
        if fOrbits.mSettingsPlanets.Checked then begin
          if BodiesInfo[NPlanets].OrbitPeriod >= PlanetOPeriods[7] then begin
            if (BodiesInfo[I].Body = 'Mercury') or (BodiesInfo[I].Body = 'Venus') or (BodiesInfo[I].Body = 'Earth') or (BodiesInfo[I].Body = 'Mars') then
              ODraw := False;
          end
          else if BodiesInfo[NPlanets].OrbitPeriod >= PlanetOPeriods[5] then begin
            if (BodiesInfo[I].Body = 'Mercury') or (BodiesInfo[I].Body = 'Venus') then
              ODraw := False;
          end
        end;
      end;
      if ODraw then begin
        if BodiesInfo[I].Body = 'Pluto' then
          ODrawDays := 0.97 * BodiesInfo[I].OrbitPeriod
        else
          ODrawDays := 1.025 * BodiesInfo[I].OrbitPeriod;
        if StepDays < ODrawDays then
          fDraw.imOrbits.Picture.Bitmap.Canvas.Pixels[X, Y] := BodiesInfo[I].DrawColor;
      end;
      if fOrbits.mSettingsPlanets.Checked then begin
        if fOrbits.mSettingsImages.Checked then begin
          fDraw.imPlanets[Planets[I]].Left := X - PlanetDrawRadiusX[Planets[I]];
          fDraw.imPlanets[Planets[I]].Top  := Y - PlanetDrawRadiusY[Planets[I]];
          fDraw.imPlanets[Planets[I]].Visible := True;
        end
        else begin
          fDraw.shPlanets10[Planets[I]].Left := X - 5; fDraw.shPlanets10[Planets[I]].Top := Y - 5; fDraw.shPlanets10[Planets[I]].Visible := True;
        end;
      end;
      if (StepDays >= BodiesInfo[I].OrbitPeriod) and (StepDays < BodiesInfo[I].OrbitPeriod + TimeStep) then begin
        if not fOrbits.mSettingsRevolutionOff.Checked then
          ShowMessage('Sun revolution accomplished for ' + BodiesInfo[I].Body + ' after ' + FloatToStr(BodiesInfo[I].OrbitPeriod) + ' days');
      end;
    end;
  end;
end;

{**********}
{ TfOrbits }
{**********}

{ Application start: Initialisation }

procedure TfOrbits.FormCreate(Sender: TObject);

begin
  SetLength(Bodies, 0); SetLength(BodiesInfo, 0);
  SetLength(aPlanets, 0);
  cbPlanets[1] := cbMercury; cbPlanets[2] := cbVenus;   cbPlanets[3] := cbEarth;
  cbPlanets[4] := cbMars;    cbPlanets[5] := cbJupiter; cbPlanets[6] := cbsaturn;
  cbPlanets[7] := cbUranus;  cbPlanets[8] := cbNeptune; cbPlanets[9] := cbPluto;
  iTimeStep := 1;
  tiOrbits.Enabled := False;
  tiOrbits.Interval := 100;
  bStarted := False; bUpdateInfo := False;
end;

{ Main form activation: Enable info grid update }

procedure TfOrbits.FormActivate(Sender: TObject);

begin
  bUpdateInfo := True;
end;

{ Main form deactivation: Disable info grid update }

procedure TfOrbits.FormDeactivate(Sender: TObject);

begin
  bUpdateInfo := False;
end;

{ Menu item "Simulation > Exit": Exit application }

procedure TfOrbits.mSimulationExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Simulation time step": Select simulation time step: day, week or month }

procedure TfOrbits.mSettingsTimeStepDayClick(Sender: TObject);

begin
  if not mSettingsTimeStepDay.Checked then begin
    mSettingsTimeStepDay.Checked := True;
    mSettingsTimeStepWeek.Checked := False;
    mSettingsTimeStepMonth.Checked := False;
    iTimeStep := 1;
  end;
end;

procedure TfOrbits.mSettingsTimeStepWeekClick(Sender: TObject);

begin
  if not mSettingsTimeStepWeek.Checked then begin
    mSettingsTimeStepDay.Checked := False;
    mSettingsTimeStepWeek.Checked := True;
    mSettingsTimeStepMonth.Checked := False;
    iTimeStep := 7;
  end;
end;

procedure TfOrbits.mSettingsTimeStepMonthClick(Sender: TObject);

begin
  if not mSettingsTimeStepMonth.Checked then begin
    mSettingsTimeStepDay.Checked := False;
    mSettingsTimeStepWeek.Checked := False;
    mSettingsTimeStepMonth.Checked := True;
    iTimeStep := 30;
  end;
end;

{ Menu items "Settings > Timer interval": Select simulation timer interval (cf. Help text) }

procedure TfOrbits.mSettingsTimer1000Click(Sender: TObject);

begin
  if not mSettingsTimer1000.Checked then begin
    mSettingsTimer1000.Checked := True;
    mSettingsTimer100.Checked := False;
    mSettingsTimer10.Checked := False;
    mSettingsTimer1.Checked := False;
    tiOrbits.Interval := 1000;
  end;
end;

procedure TfOrbits.mSettingsTimer100Click(Sender: TObject);

begin
  if not mSettingsTimer100.Checked then begin
    mSettingsTimer1000.Checked := False;
    mSettingsTimer100.Checked := True;
    mSettingsTimer10.Checked := False;
    mSettingsTimer1.Checked := False;
    tiOrbits.Interval := 100;
  end;
end;

procedure TfOrbits.mSettingsTimer10Click(Sender: TObject);

begin
  if not mSettingsTimer10.Checked then begin
    mSettingsTimer1000.Checked := False;
    mSettingsTimer100.Checked := False;
    mSettingsTimer10.Checked := True;
    mSettingsTimer1.Checked := False;
    tiOrbits.Interval := 10;
  end;
end;

procedure TfOrbits.mSettingsTimer1Click(Sender: TObject);

begin
  if not mSettingsTimer1.Checked then begin
    mSettingsTimer1000.Checked := False;
    mSettingsTimer100.Checked := False;
    mSettingsTimer10.Checked := False;
    mSettingsTimer1.Checked := True;
    tiOrbits.Interval := 1;
  end;
end;

{ Menu item "Settings > Display planet orbits": Toggle display orbits on/off }

procedure TfOrbits.mSettingsOrbitsClick(Sender: TObject);
begin
  if mSettingsOrbits.Checked then
    mSettingsOrbits.Checked := False
  else
    mSettingsOrbits.Checked := True;
end;

{ Menu item "Settings > Display planet movement": Toggle display planets on/off }

procedure TfOrbits.mSettingsPlanetsClick(Sender: TObject);

begin
  if mSettingsPlanets.Checked then begin
    mSettingsPlanets.Checked := False;
    mSettingsImages.Enabled := False;
  end
  else begin
    mSettingsPlanets.Checked := True;
    mSettingsImages.Enabled := True;
  end;
end;

{ Menu item "Settings > Use sized planet images": Toggle planet display: sized images/colored circles }

procedure TfOrbits.mSettingsImagesClick(Sender: TObject);

begin
  if mSettingsImages.Checked then
    mSettingsImages.Checked := False
  else
    mSettingsImages.Checked := True;
end;

{ Menu item "Settings > Use same horiz. and vert. scaling": Toggle orbits display: circle/ellipse }

procedure TfOrbits.mSettingsScalingClick(Sender: TObject);

begin
  if mSettingsScaling.Checked then
    mSettingsScaling.Checked := False
  else
    mSettingsScaling.Checked := True;
end;

{ Menu item "Settings > Turn revolution messages of": Toggle revolution messages display on/off }

procedure TfOrbits.mSettingsRevolutionOffClick(Sender: TObject);

begin
  if mSettingsRevolutionOff.Checked then
    mSettingsRevolutionOff.Checked := False
  else
    mSettingsRevolutionOff.Checked := True;
end;

{ Menu item "Settings > Turn orbit drawing warnings of": Toggle orbit drawing messages display on/off }

procedure TfOrbits.mSettingsODrawingClick(Sender: TObject);

begin
  if mSettingsODrawing.Checked then
    mSettingsODrawing.Checked := False
  else
    mSettingsODrawing.Checked := True;
end;

{ Menu item "Help > Application help": Display application usage help }

procedure TfOrbits.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.memoHelp.Clear;
    fHelp.memoHelp.Lines.LoadFromFile('help.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > Technical info": Display technical information for programmers }

procedure TfOrbits.mHelpTechnicalClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.memoHelp.Clear;
    fHelp.memoHelp.Lines.LoadFromFile('info.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfOrbits.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Animated simulation of the motion of the 9 planets ';
  S += 'of our solar system around the sun.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, June 2018 - September 2021.';
  MessageDlg('About "PlanetaryOrbits"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Stop": Start resp. stop the simulation }

procedure TfOrbits.btStartClick(Sender: TObject);

const
  Scales1X: array[1..9] of Integer = (
    400, 400, 400, 400, 120, 75, 36, 20, 18
  );
  Scales2X: array[1..9] of Integer = (
    360, 360, 360, 220, 75, 36, 20, 18, 18
  );

var
  SPeriod, ErrMess: string;
  I, J: Cardinal;

begin
  // Button "Start": Start the simulation
  btPause.Caption := 'Pause';
  if btStart.Caption = 'Start' then begin
    // Reset all
    ErrMess := '';
    fDraw.imOrbits.Picture.Bitmap.Canvas.Brush.Color := clBlack;
    fDraw.imOrbits.Picture.Bitmap.Canvas.Clear;
    for I := 1 to 9 do begin
      fDraw.shPlanets10[I].Visible := False;
      fDraw.imPlanets[I].Visible := False;
    end;
    iBodies := 1; iPlanets := 0; bStarted := False;
    // Create the sun and the selected planet objects
    SetLength(Bodies, iBodies); SetLength(aPlanets, iPlanets + 1);
    Bodies[0] := TBody.Create('Sun', SunMass, SunVolume, 0, 0, 0, clYellow);;
    for I := 1 to 9 do begin
      if cbPlanets[I].Checked then begin
        // Create object only for the planets selected by the user
        Inc(iPlanets); iBodies := iPlanets + 1;
        SetLength(Bodies, iBodies); SetLength(aPlanets, iPlanets + 1);
        Bodies[iPlanets] := TBody.Create(PlanetNames[I], PlanetMasses[I], PlanetVolumes[I], PlanetOPeriods[I],
          PlanetPosX[I] * AU, PlanetVelY[I] * 1000, PlanetDrawColors[I]);
        aPlanets[iPlanets] := I;
      end;
    end;
    SetLength(BodiesInfo, iBodies);
    if iPlanets > 0 then begin
      // Mercury and Venus have to small revolution period to use 1 month step: Display error message
      if mSettingsTimeStepMonth.Checked and (cbMercury.Checked or cbVenus.Checked) then
        ErrMess := 'Can''t calculate Mercury/Venus for 1 month time step'
      // Simulation is done, but display may be improper: Display warning message in this case
      else begin
        iStep := 1; bStarted := True; bUpdateInfo := True;
        grOrbits.ColCount := iBodies + 1;
        for  I := 0 to iBodies - 1 do
          BodiesInfo[I] := Bodies[I].GetOrbitData;
        if mSettingsOrbits.Checked and not mSettingsODrawing.Checked then begin
          if mSettingsTimeStepWeek.Checked and (BodiesInfo[iPlanets].OrbitPeriod <= PlanetOPeriods[4]) then
            MessageDlg('Improper orbit display', '1 week simulation time may be to high for planets selected!', mtwarning, [mbOK], 0)
          else if mSettingsTimeStepMonth.Checked and (BodiesInfo[iPlanets].OrbitPeriod <= PlanetOPeriods[6]) then
            MessageDlg('Improper orbit display', '1 month simulation time may be to high for planets selected!', mtwarning, [mbOK], 0);
        end;
        // Fill the string grid with the info of the actual planets selected
        for I := 0 to iBodies - 1 do begin
          grOrbits.Cells[I + 1, 0] := BodiesInfo[I].Body;
          grOrbits.Cells[I + 1, 1] := ' ' + FloatToStrF(BodiesInfo[I].Mass, ffExponent, 6, 2);
          grOrbits.Cells[I + 1, 2] := ' ' + FloatToStrF(BodiesInfo[I].Volume, ffExponent, 6, 2);
          if BodiesInfo[I].OrbitPeriod <> 0 then begin
            SPeriod := FloatToStrF(BodiesInfo[I].OrbitPeriod, ffNumber, 6, 3);
            for J := 1 to 11 - Length(SPeriod) do
              SPeriod := ' ' + SPeriod;
            grOrbits.Cells[I + 1, 3] := ' ' + SPeriod;
          end;
        end;
        // Set scaling factors (depending on circle/ellipse orbit selection)
        if mSettingsScaling.Checked then begin
          rScaleX := Scales2X[aPlanets[iPlanets]]; rScaleY := rScaleX;
        end
        else begin
          rScaleX := Scales1X[aPlanets[iPlanets]]; rScaleY := rScaleX / 2;
        end;
        rScaleX /= AU; rScaleY /= AU;
        // Reduce size of sun if one of the farest away planets is part of the simulation
        fDraw.shSun10.Visible := False; fDraw.shSun5.Visible := False;
        if (BodiesInfo[iPlanets].Body = 'Neptune') or (BodiesInfo[iPlanets].Body = 'Pluto') then
          iSunDrawSize := 5
        else
          iSunDrawSize := 10;
        // Determine real time corresponding to simultion step
        case iTimeStep of
           1: sTimeStep := 'Day';
           7: sTimeStep := 'Week';
          30: sTimeStep := 'Month';
        end;
        iTimeStepSecs := iTimeStep * 24 * 3600;
        // Display the (static) planet info
        DisplayBodiesInfo(BodiesInfo, iStep, sTimeStep);
        // Disable planet selection
        cbMercury.Enabled := False; cbVenus.Enabled   := False; cbEarth.Enabled  := False;
        cbMars.Enabled    := False; cbJupiter.Enabled := False; cbSaturn.Enabled := False;
        cbUranus.Enabled  := False; cbNeptune.Enabled := False; cbPluto.Enabled  := False;
        // Disable Settings menu items (except the revolution message toggle)
        mSettingsTimeStep.Enabled := False; mSettingsOrbits.Enabled := False;
        mSettingsPlanets.Enabled := False;  mSettingsImages.Enabled := False;
        mSettingsScaling.Enabled:= False;   mSettingsODrawing.Enabled := False;
        // Draw the planetary system starting position
        DrawOrbits(BodiesInfo, iStep, iTimeStep, sTimeStep, rScaleX, rScaleY, iSunDrawSize, aPlanets);
        btStart.Caption := 'Stop';
        // Start the timer = Do the simulation (all code in the timer routine)
        tiOrbits.Enabled := True;
      end;
    end
    else
      ErrMess := 'No planet(s) selected';
    if ErrMess <> '' then
      MessageDlg('Invalid selection', ErrMess + '!', mtError, [mbOK], 0);
  end
  // Button "Stop": Stop the simulation
  else begin
    tiOrbits.Enabled := False;
    bStarted := False; bUpdateInfo := False;
    // Re-enable planet selection
    cbMercury.Enabled := True; cbVenus.Enabled   := True; cbEarth.Enabled  := True;
    cbMars.Enabled    := True; cbJupiter.Enabled := True; cbSaturn.Enabled := True;
    cbUranus.Enabled  := True; cbNeptune.Enabled := True; cbPluto.Enabled  := True;
    // Re-enable the Settings menu items
    fOrbits.mSettingsTimeStep.Enabled := True; fOrbits.mSettingsOrbits.Enabled := True; mSettingsPlanets.Enabled := True;
    if mSettingsPlanets.Checked then
      mSettingsImages.Enabled := True;
    fOrbits.mSettingsScaling.Enabled:= True; fOrbits.mSettingsODrawing.Enabled := True;
    btStart.Caption := 'Start';
  end;
  // Destroy the planet objects if actual simulation is done
  if not bStarted then begin
    for I := 0 to iPlanets do
      Bodies[I].Destroy;
  end;
end;

{ Button "Pause/Resume": Pause resp. resume the simulation }

procedure TfOrbits.btPauseClick(Sender: TObject);

begin
  if bStarted then begin
    if btPause.Caption = 'Pause' then begin
      tiOrbits.Enabled := False;
      btPause.Caption := 'Resume';
    end
    else begin
      tiOrbits.Enabled := True;
      btPause.Caption := 'Pause';
    end;
  end;
end;

{ Button "Show orbits": Open the window with the graphical simulation }

procedure TfOrbits.btOrbitsClick(Sender: TObject);

begin
  if not fDraw.Visible then
    fDraw.ShowModal;
end;

{ Timer routine: Run the simulation }

procedure TfOrbits.tiOrbitsTimer(Sender: TObject);

// This procedure contains the complete code of the simulation:
//   - Physics calculations (actually done in the orbits_classes unit)
//   - Info grid update, planets movement on fDraw form

var
  I: Cardinal;
  Forces: array of TVector;

begin
  Inc(iStep); SetLength(Forces, iBodies);
  // Calculate total force exerted on each planet
  for I := 0 to iBodies - 1 do begin
    Forces[I].X := 0; Forces[I].Y := 0;
    Forces[I] := Bodies[I].TotalAttraction(Bodies);
  end;
  // Calculate new position for each planet, depending on the total force exerted on it
  for I := 0 to iBodies - 1 do begin
    //Update velocities based upon on the force
    Bodies[I].NewVelocity(Forces[I], iTimeStepSecs);
    // Update positions
    Bodies[I].NewPosition(iTimeStepSecs);
  end;
  // Get actual orbit data for each planet
  for  I := 0 to iBodies - 1 do
    BodiesInfo[I] := Bodies[I].GetOrbitData;
  // If the main window is active, update the info string grid
  if bUpdateInfo then
    DisplayBodiesInfo(BodiesInfo, iStep, sTimeStep);
  // Draw the planets at their actual orbit position
  DrawOrbits(BodiesInfo, iStep, iTimeStep, sTimeStep, rScaleX, rScaleY, iSunDrawSize, aPlanets);
end;

{ Button "All": Select all planets for the simulation }

procedure TfOrbits.btAllClick(Sender: TObject);

begin
  PlanetsSelect(True);
end;

{ Button "None": Deselect all planets for the simulation }

procedure TfOrbits.btNoneClick(Sender: TObject);

begin
  PlanetsSelect(False);
end;

end.

