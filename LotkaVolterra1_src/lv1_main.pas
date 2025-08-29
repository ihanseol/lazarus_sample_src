{********************************************}
{* Main unit for LotkaVolterra1 application *}
{********************************************}

unit lv1_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, lv1_graph, lv1_help;

const
  MaxAllAnimals = 500;

type
  TSimulationValues = array of Real;
  TAnimalImages = array of TImage;
  {*******}
  { TfLV1 }
  {*******}
  TfLV1 = class(TForm)
    edPreyCarryingCapacity: TEdit;
    laPreyCarryingCapacity: TLabel;
    laPreyCarryingCapacityUnit: TLabel;
    mSettingsLogistic: TMenuItem;
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSample, mFileExit: TMenuItem;
    mSettings, mSettingsTime, mSettingsAltFormulae: TMenuItem;
    mHelp, mHelpLV, mHelpHelp, mHelpAbout: TMenuItem;
    shSimulation: TShape;
    imgPrey, imgPredator: TImage;
    edSimTime, edSimPrey, edSimPreyDiff, edSimPredator, edSimPredatorDiff: TEdit;
    Label2, Label3, Label4, Label6, Label7, Label12, Label13, Label14, Label15, Label16, Label17: TLabel;
    laPreyGrowth, laPredatorGrowth, laPredatorDeath, laPredation: TLabel;
    laPreyUnit, laPredatorUnit, laPreyGrowthRateUnit, laPredatorGrowthRateUnit, laPredatorDeathRateUnit, laPredationRateUnit, laEndTimeUnit: TLabel;
    edPredatorName, edPredator, edPreyName, edPrey: TEdit;
    edPreyGrowthRate, edPredatorGrowthRate, edPredatorDeathRate, edPredationRate, edEndTime: TEdit;
    btStart: TButton;
    btPause: TButton;
    btGraph: TButton;
    tiLV: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSampleClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsAltFormulaeClick(Sender: TObject);
    procedure mSettingsLogisticClick(Sender: TObject);
    procedure mSettingsTimeClick(Sender: TObject);
    procedure mHelpLVClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btGraphClick(Sender: TObject);
    procedure tiBacteriaTimer(Sender: TObject);
  private
    iMaxPrey, iMaxPredator, iDisplayedPrey, iDisplayedPredator, iEndTime, iValues, iInterval, iValue: Integer;
    rP0, rN0, rK, rA, rB, rC, rD, rE, rMaxPredator, rMaxPrey, rOldPrey, rOldPredator: Real;
    sPredator, sPrey: string;
    aSimulationTime, aPredator, aPrey: TSimulationValues;
    imPrey, imPredator: TAnimalImages;
  end;

var
  fLV1: TfLV1;

implementation

{$R *.lfm}

{ Get user data for this simulation from form }

procedure ReadData(out Prey, Predator: string; out N0, P0, K, A, B, C, D, E: Real; out EndTime: Integer; out Mess: string);

// There is no 'real' validity check of the values that the user enters; thus up to her not to choose 'senseless' data

var
  S: string;

begin
  Mess := ''; N0 := 0; P0 := 0; K := 0; A := 0; B := 0; C := 0; D := 0; E := 0; EndTime := 0;
  if fLV1.edPreyName.Text = '' then
    Prey := 'prey'
  else
    Prey := fLV1.edPreyName.Text;
  if fLV1.edPrey.Text <> '' then
    N0 := StrToFloat(fLV1.edPrey.Text);
  if fLV1.edPreyGrowthRate.Text <> '' then
    A := StrToFloat(fLV1.edPreyGrowthRate.Text);
  if fLV1.edPredatorName.Text = '' then
    Predator := 'predator'
  else
    Predator := fLV1.edPredatorName.Text;
  if fLV1.edPredator.Text <> '' then
    P0 := StrToFloat(fLV1.edPredator.Text);
  if fLV1.mSettingsLogistic.Checked and (fLV1.edPreyCarryingCapacity.Text <> '') then
    K := StrToFloat(fLV1.edPreyCarryingCapacity.Text);
  if fLV1.edPredatorDeathRate.Text <> '' then
    C := StrToFloat(fLV1.edPredatorDeathRate.Text);
  if fLV1.edPredationRate.Text <> '' then
    B := StrToFloat(fLV1.edPredationRate.Text);
  if fLV1.edPredatorGrowthRate.Text <> '' then begin
    if fLV1.mSettingsAltFormulae.Checked then
      E := StrToFloat(fLV1.edPredatorGrowthRate.Text)
    else
      D := StrToFloat(fLV1.edPredatorGrowthRate.Text);
  end;
  if fLV1.edEndTime.Text <> '' then
    EndTime := StrToInt(fLV1.edEndTime.Text);
  if N0 < 0 then begin
    Mess := 'Initial prey must be greater than 0';
    fLV1.edPrey.SetFocus;
  end
  else if P0 < 0 then begin
    Mess := 'Initial predator population must be greater than 0';
    fLV1.edPredator.SetFocus;
  end
  else if (N0 = 0) and (P0 = 0) then begin
    Mess := 'At least one of prey or predator population must be greater than 0';
    fLV1.edPrey.SetFocus;
  end
  else if fLV1.mSettingsLogistic.Checked and (K <= 0) then begin
    Mess := 'Carrying capacity must be greater than 0';
    fLV1.edPreyCarryingCapacity.SetFocus;
  end;
  if Mess = '' then begin
    if (N0 > 0) and (A <= 0) then begin
      Mess := 'Prey growth rate must be greater than 0';
      fLV1.edPreyGrowthRate.SetFocus;
    end
    else if (P0 > 0) and (C <= 0) then begin
      Mess := 'Predator death rate must be greater than 0';
      fLV1.edPredatorDeathRate.SetFocus;
    end
    else if (N0 > 0) and (P0 > 0) and (B <= 0) then begin
      Mess := 'Predation death rate must be greater than 0';
      fLV1.edPredationRate.SetFocus;
    end
    else if not fLV1.mSettingsAltFormulae.Checked and (N0 > 0) and (P0 > 0) and (D <= 0) then begin
      Mess := 'Predation growth rate must be greater than 0';
      fLV1.edPredatorGrowthRate.SetFocus;
    end
    else if fLV1.mSettingsAltFormulae.Checked and (N0 > 0) and (P0 > 0) and (E <= 0) then begin
      Mess := 'Predation efficiency rate must be greater than 0';
      fLV1.edPredatorGrowthRate.SetFocus;
    end
    else if EndTime < 5 then begin
      Mess := 'Time period must be greater than or equal to 5 years';
      fLV1.edEndTime.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if P0 = 0 then begin
      S := 'No predators: Prey population will ';
      if fLV1.mSettingsLogistic.Checked then begin
        if K >= N0 then
          S += 'grow'
        else
          S += 'decrease';
        S += ' until carrying capacity is reached.';
      end
      else
        S += 'grow infinitely.';
      MessageDlg('Single species model', S, mtWarning, [mbOK], 0);
      Predator := ''; B := 0; C := 0; D := 0; E := 0;
      fLV1.edPredatorName.Text := ''; fLV1.edPredatorDeathRate.Text := ''; fLV1.edPredatorGrowthRate.Text := ''; fLV1.edPredationRate.Text := '';
    end
    else if N0 = 0 then begin
      MessageDlg('Single species model', 'No prey: Predator population will die out by starving.', mtWarning, [mbOK], 0);
      Prey := ''; A := 0; B := 0; D := 0; E := 0;
      fLV1.edPreyName.Text := ''; fLV1.edPreyGrowthRate.Text := ''; fLV1.edPredatorGrowthRate.Text := ''; fLV1.edPredationRate.Text := '';
    end;
  end;
end;

{ Clean graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fLV1G.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis and write labels }

procedure DrawAxis(XL, XR, YT, YB: Integer; LPrey, LPredator: string);

var
  YAX, YLX: Integer;

begin
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);                  // draw X-axis
  fLV1G.imGraph.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);                    // draw Y-axis
  fLV1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
  fLV1G.imGraph.Picture.Bitmap.Canvas.TextOut(XR - 15, YLX, '[Years]');
  if LPrey <> '' then begin
    fLV1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlue;                            // color (blue) for prey
    fLV1G.imGraph.Picture.Bitmap.Canvas.TextOut(6, 3, LPrey);
  end;
  if LPredator <> '' then begin
    fLV1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clRed;                             // color (red) for predator
    fLV1G.imGraph.Picture.Bitmap.Canvas.TextOut(6, 18, LPredator);
  end;
end;

{ Format values for proper output on the graph }

function GFormat(N: Real): string;

var
  I: Integer;
  SN: string;

begin
  SN := IntToStr(Round(N));
  // Add spaces to the left to rigth-justify the values
  for I := 1 to 6 - Length(SN) do
    SN := '  ' + SN;
  GFormat := SN;
end;

{ Calculate maximum value of population array }

function MaxPopulation(var Population: TSimulationValues): Real;

var
  I: Integer;
  Max: Real;

begin
  Max := Population[0];
  for I := 1 to Length(Population) - 1 do
    if Population[I] > Max then
      Max := Population[I];
  MaxPopulation := Max;
end;

{ Calculate maximum value of population array }

function MinPopulation(var Population: TSimulationValues): Real;

var
  I: Integer;
  Min: Real;

begin
  Min := Population[0];
  for I := 1 to Length(Population) - 1 do
    if Population[I] < Min then
      Min := Population[I];
  MinPopulation := Min;
end;

{ Lotka–Volterra model }

procedure Lotka_Volterra(K: Real; var N, P: Real; A, B, C, D, E, dT: Real);

var
  dN, dP: Real;

begin
  if fLV1.mSettingsLogistic.Checked then begin
    // Prey logistic growth
    dN := (N * (A * (1 - N / K) - B * P)) * dT;
  end
  else begin
    // Prey exponential growth
    dN := (N * (A - B * P)) * dT;
  end;
  if E = 0 then
    dP := (P * (D * N - C)) * dT
  else
    dP := (P * (E * B * N - C)) * dT;
  N += dN;
  if N < 0 then
    N := 0;
  P += dP;
  if P < 0 then
    P := 0;
end;

{ Compute T, N and P values for Lotka–Volterra model }

procedure Lotka_Volterra_XY(N0, P0, K, A, B, C, D, E: Real; Values, Endtime: Integer; var SimulationTime, Prey, Predator: TSimulationValues);

var
  I, J: Integer;
  T, N, P, dT: Real;

begin
  SetLength(SimulationTime, 1); SetLength(Prey, 1); SetLength(Predator, 1);
  // Initial values: for T = 0, N = N0, P = P0
  SimulationTime[0] := 0; Prey[0] := N0; Predator[0] := P0;
  N := N0; P := P0;
  // Compute numerical approxiation of  differential equations using the Euler method
  dT := Endtime / Values;
  for I := 1 to Values do begin
    T := I * dT;
    // The smaller the time interval, the more accurate the numerical approxiation will be (and the longer the calculations take)
    // The values chosen (100 * 240 steps / year) suit well in most cases; for very long time periods, however, the oscillations
    // increase in amplitude as time progresses (use higher end value in "for" statement below to prevent this)
    for J := 1 to 100 do
      Lotka_Volterra(K, N, P, A, B, C, D, E, dT / 100);                                  // Lotka–Volterra model values for T = (T - 1) + dT
    SetLength(SimulationTime, Length(SimulationTime) + 1);
    SetLength(Prey, Length(Prey) + 1);
    SetLength(Predator, Length(Predator) + 1);
    SimulationTime[I] := T;
    Prey[I] := N;
    Predator[I] := P;
  end;
end;

{ Draw the prey/predator population vs time graph (as calculated by the Lotka–Volterra model) }

procedure DrawGraph(Values, EndTime, XL, XR, YT, YB: Integer; var SimulationTime, Prey, Predator: TSimulationValues);

var
  X, Y, XDiv, YDiv, Mult, I: Integer;
  Max: Real;
  SN: string;

begin
  // Maximum population value
  if Prey[0] = 0 then
    Max := MaxPopulation(Predator)
  else begin
    Max := MaxPopulation(Prey);
    if Predator[0] <> 0 then begin
      if MaxPopulation(Predator) > Max then
        Max := MaxPopulation(Predator);
    end;
  end;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 2;
  // Draw prey population graph (in blue)
  if Prey[0] <> 0 then begin
    fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlue;
    for I := 0 to Values do begin
      // X-value (time) on graph
      X := Round((SimulationTime[I] / EndTime) * (XR - XL)) + XL;
      // Y-value (prey) on graph
      Y := YB - Round((Prey[I] / Max) * (YB - YT));
      if I = 0 then
        fLV1G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)                                 // first point of the curve: position the pen
      else
        fLV1G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);                                // other points: draw line from previous point to here
    end;
  end;
  // Draw predator population graph (in red)
  if Predator[0] <> 0 then begin
    fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clRed;
    for I := 0 to Values do begin
      // X-value (time) on graph
      X := Round((SimulationTime[I] / EndTime) * (XR - XL)) + XL;
      // Y-value (predator) on graph
      Y := YB - Round((Predator[I] / Max) * (YB - YT));
      if I = 0 then
        fLV1G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)                                 // first point of the curve: position the pen
      else
        fLV1G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);                                // other points: draw line from previous point to here
    end;
  end;
  // Draw ticks and write values of X-axis
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  XDiv := (EndTime div 25) + 1;
  if XDiv = 3 then                                                                       // use x-tick values of 1, 2, 5, 10 or 20 years (depending on simulation time)
    XDiv := 2
  else if XDiv = 4 then
    XDiv := 5
  else if (XDiv > 5) and (XDiv <= 10) then
    XDiv := 10
  else if XDiv > 10 then
    XDiv := 20;
  for I := 1 to EndTime - XDiv do begin
    if I mod XDiv = 0 then begin                                                         // limiting the number of ticks
      X := Round(I * (XR - XL) / EndTime) + XL;
      fLV1G.imGraph.Picture.Bitmap.Canvas.Line(X, YB - 5, X, YB + 5);
      SN := IntToStr(I);
      if Length(SN) = 1 then                                                             // adapt position of time value, depending on it's 1 or 2 digits
        X -= 3
      else
        X -= 6;
      fLV1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
      fLV1G.imGraph.Picture.Bitmap.Canvas.TextOut(X, YB + 7, SN);
    end;
  end;
  // Draw ticks and write values of Y-axis
  YDiv := 1; Mult := 10;
  while 20 * Mult * YDiv < Max do begin                                                  // use round y-tick values
    if YDiv = 1 then
      YDiv := 5
    else begin
      YDiv := 1; Mult *= 10;
    end;
  end;
  YDiv *= Mult;
  for I := 1 to Round(Max + YDiv) do begin
    if I mod YDiv = 0 then begin
      Y := YB - Round(I * (YB - YT) / Round(Max));
      if Y > 40 then begin
        fLV1G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
        fLV1G.imGraph.Picture.Bitmap.Canvas.TextOut(20, Y - 8, GFormat(I));
      end;
    end;
  end;
  // Reset pen width and pen and font color to default values
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clDefault;
  fLV1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clDefault;
end;

{*******}
{ TfLV1 }
{*******}

{ Application start: Load sample data }

procedure TfLV1.FormCreate(Sender: TObject);

begin
  SetLength(imPrey, 0); SetLength(imPredator, 0);
  SetLength(aSimulationTime, 0); SetLength(aPrey, 0); SetLength(aPredator, 0);
  iInterval := 1000;
  mFileSample.Click;                                                                     // simulate selection of "File > Sample" menu item
end;

{ Menu item "File > New": Clear simulation form }

procedure TfLV1.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  edSimTime.Text := ''; edSimPrey.Text := ''; edSimPredator.Text := '';
  edSimPreyDiff.Text := ''; edSimPreyDiff.Color := clDefault;
  edSimPredatorDiff.Text := ''; edSimPredatorDiff.Color := clDefault;
  if Length(imPrey) > 0 then begin
    for I := 0 to iMaxPrey - 1 do
      imPrey[I].Visible := False;                                                        // remove all prey by hiding them
  end;
  if Length(imPredator) > 0 then begin
    for I := 0 to iMaxPredator - 1 do
      imPredator[I].Visible := False;                                                    // remove all predator by hiding them
  end;
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Enabled := False;
end;

{ Menu item "File > Sample": Load sample data }

procedure TfLV1.mFileSampleClick(Sender: TObject);

begin
  if mSettingsLogistic.Checked then
    mSettingsLogistic.Click;
  mFileNew.Click;                                                                        // clear simulation form
  edPreyName.Text := 'hare'; edPredatorName.Text := 'lynx';
  edPrey.Text := '35'; edPreyGrowthRate.Text := '0,55';
  edPredator.Text := '5'; edPredatorDeathRate.Text := '0,84';
  laPredatorGrowth.Caption := 'Predation growth rate (d)';
  laPredatorGrowth.Font.Color := clDefault;
  edPredatorGrowthRate.Text := '0,028'; edPredationRate.Text := '0,026';
  mSettingsAltFormulae.Checked := False;
  edEndTime.Text := '20';
end;

{ Menu item "File > Exit": Exit the application }

procedure TfLV1.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Use logistic growth for prey": Use exponential or logistic growth for prey }

procedure TfLV1.mSettingsLogisticClick(Sender: TObject);

begin
  if mSettingsLogistic.Checked then begin
    mSettingsLogistic.Checked := False;
    laPreyCarryingCapacity.Visible := False;
    edPreyCarryingCapacity.Visible := False;
    laPreyCarryingCapacityUnit.Visible := False;
  end
  else begin
    mSettingsLogistic.Checked := True;
    laPreyCarryingCapacity.Visible := True;
    edPreyCarryingCapacity.Visible := True;
    laPreyCarryingCapacityUnit.Visible := True;
  end;
end;

{ Menu item "Settings > Use alternate formulae": Use or not of the "predation efficiency rate" formulae }

procedure TfLV1.mSettingsAltFormulaeClick(Sender: TObject);

begin
  laPreyGrowth.Caption := 'Prey growth rate ';
  laPredatorDeath.Caption := 'Predator death rate ';
  laPredation.Caption := 'Predation death rate ';
  if mSettingsAltFormulae.Checked then begin
    mSettingsAltFormulae.Checked := False;
    laPreyGrowth.Caption := laPreyGrowth.Caption + '(a)';
    laPredatorDeath.Caption := laPredatorDeath.Caption + '(c)';
    laPredation.Caption := laPredation.Caption + '(b)';
    laPredatorGrowth.Caption := 'Predation growth rate (d)';
    laPredatorGrowth.Font.Color := clDefault;
    edPredatorGrowthRate.Text := '';
  end
  else begin
    mSettingsAltFormulae.Checked := True;
    laPreyGrowth.Caption := laPreyGrowth.Caption + '(α)';
    laPredatorDeath.Caption := laPredatorDeath.Caption + '(γ)';
    laPredation.Caption := laPredation.Caption + '(β)';
    laPredatorGrowth.Caption := 'Predation efficiency rate (ε)';
    laPredatorGrowth.Font.Color := clBlue;
    edPredatorGrowthRate.Text := '';
  end;
end;

{ Menu item "Settings > Simulation time interval": Ask user for simulation time interval }

procedure TfLV1.mSettingsTimeClick(Sender: TObject);

var
  S: string;

// Simulation time interval = timer interval = how many secs between 2 display updates is used for 1 year population growth time

begin
  S := InputBox('Simulation time interval', 'Simulation time in sec for 1 year real time', FloatToStr(iInterval / 1000));
  if S <> '' then
    iInterval := Round(StrToFloat(S) * 1000);
end;

{ Menu item "Help > Biology help": Display predator-prey background help text }

procedure TfLV1.mHelpLVClick(Sender: TObject);

begin
  fLV1H.memoHelp.Lines.LoadFromFile('help1.txt');
  fLV1H.Show;
end;

{ Menu item "Help > Program help": Display program help text }

procedure TfLV1.mHelpHelpClick(Sender: TObject);

begin
  fLV1H.memoHelp.Lines.LoadFromFile('help2.txt');
  fLV1H.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfLV1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Biology simulation:' + LineEnding;
  S := 'Predator-prey (Lotka–Volterra) model.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, January-October 2019.';
  MessageDlg('About "LotkaVolterra1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Stop": Start resp. stop the simlation }

procedure TfLV1.btStartClick(Sender: TObject);

var
  I: Integer;
  MinPrey, MinPredator: Real;
  Mess: string;

begin
  // Button "Start": Start simulation with actual parameters read from form
  if btStart.Caption = 'Start' then begin
    ReadData(sPrey, sPredator, rN0, rP0, rK, rA, rB, rC, rD, rE, iEndTime, Mess);        // read user data from form
    if Mess = '' then begin                                                              // proceed if there were no error message
      if ((rN0 <> 0) and (rN0 < 1)) or ((rP0 <> 0) and (rP0 < 1)) then                   // do simulation only for initial population sizes >= 1
        Mess := 'Initial number of animals must be >= 1 for proper graphical simulation'
      else begin
        iValues := iEndTime * 240;                                                       // 240 values per year
        rOldPrey := 0; rOldPredator := 0;
        // Calculate simulation values using the Lotka-Volterra model
        Lotka_Volterra_XY(rN0, rP0, rK, rA, rB, rC, rD, rE, iValues, iEndTime, aSimulationTime, aPrey, aPredator);
        // Determine maximum of prey and predator images
        rMaxPrey := 0; rMaxPredator := 0;
        MinPrey := 0;  MinPredator := 0;
        if rN0 > 0 then begin
          rMaxPrey := MaxPopulation(aPrey);
          MinPrey := MinPopulation(aPrey);
        end;
        if rP0 > 0 then begin
          rMaxPredator := MaxPopulation(aPredator);
          MinPredator := MinPopulation(aPredator);
        end;
        if rMaxPrey + rMaxPredator < MaxAllAnimals then begin
          iMaxPrey := Round(Int(rMaxPrey));
          iMaxPredator := Round(Int(rMaxPredator));
        end
        else begin
          iMaxPrey := Round(Int(rMaxPrey * (MaxAllAnimals / (rMaxPrey + rMaxPredator))));
          iMaxPredator := Round(Int(rMaxPredator * (MaxAllAnimals / (rMaxPrey + rMaxPredator))));
        end;
        // Do the simulation only if at least 1 predator and 1 prey image will be displayed for initial/minimum values
        // (that will not be the case if the animal populations grow to big)
        if rN0 > 0 then begin
          if (Round((rN0 / rMaxPrey) * iMaxPrey) < 1) or ((MinPrey >= 0.1) and (Round((MinPrey / rMaxPrey) * iMaxPrey) < 1)) then
            Mess := 'To many animals for proper graphical simulation';
        end;
        if rP0 > 0 then begin
          if (Round((rP0 / rMaxPredator) * iMaxPredator) < 1) or ((MinPredator >= 0.1) and (Round((MinPredator / rMaxPredator) * iMaxPredator) < 1)) then
            Mess := 'To many animals for proper graphical simulation';
        end;
        if Mess = '' then begin
          // Create arrays with animal images (as much as needed, with a total of 500)
          if Length(imPrey) > 0 then begin                                     // destroy all prey images
            for I := 0 to Length(imPrey) - 1 do
              imPrey[I].Destroy;
            SetLength(imPrey, 0);
          end;
          if Length(imPredator) > 0 then begin                                 // destroy all predator images
            for I := 0 to Length(imPredator) -  1 do
              imPredator[I].Destroy;
            SetLength(imPredator, 0);
          end;
          for I := 0 to iMaxPrey - 1 do begin                                  // create prey images as needed
            SetLength(imPrey, I + 1);
            imPrey[I] := TImage.Create(fLV1.imPrey[I]);
            imPrey[I].Parent  := Self;
            imPrey[I].Width   := imgPrey.Width; imPrey[I].Height  := imgPrey.Height;
            imPrey[I].Picture := imgPrey.Picture; imPrey[I].Visible := False;
          end;
          for I := 0 to iMaxPredator - 1 do begin                                        // create predator images as needed
            SetLength(imPredator, I + 1);
            imPredator[I] := TImage.Create(fLV1.imPredator[I]);
            imPredator[I].Parent  := Self;
            imPredator[I].Width   := imgPredator.Width; imPredator[I].Height  := imgPredator.Height;
          imPredator[I].Picture := imgPredator.Picture; imPredator[I].Visible := False;
          end;
          // Start the timer (with simulation time = 0)
          iValue := 0; iDisplayedPrey := 0; iDisplayedPredator := 0;
          tiLV.Enabled := True;
          // Next push on button will be "Stop"
          btStart.Caption := 'Stop';
          // Enable "Pause" button
          btPause.Caption := 'Pause'; btPause.Enabled := True;
        end;
      end;
    end;
    // If there was an error, display message
    if Mess <> '' then
      MessageDlg('Graphics problem', Mess + '!', mtError, [mbOK], 0);
  end
  // Button "Stop": Stop simulation
  else begin
    // Disable the timer
    tiLV.Enabled := False;
    // Disable buttons (user must choose "File > New" to clear the form before running a new simulation)
    btStart.Caption := 'Start'; btStart.Enabled := False;
    btPause.Caption := 'Pause'; btPause.Enabled := False;
  end;
end;

{ Button "Pause/Resume": Pause resp. resume the simulation }

procedure TfLV1.btPauseClick(Sender: TObject);

begin
  // Button "Pause": Pause the simulation
  if btPause.Caption = 'Pause' then begin
    tiLV.Enabled := False;
    btPause.Caption := 'Resume';
  end
  // Button "Resume": Resume the simulation
  else begin
    tiLV.Enabled := True;
    btPause.Caption := 'Pause';
  end;
end;

{ Button "Graph": Draw the graph with parameters read from form (display will be done on form fLV1G) }

procedure TfLV1.btGraphClick(Sender: TObject);

var
  LPrey, LPredator, Mess, S: string;

begin
  ReadData(sPrey, sPredator, rN0, rP0, rK, rA, rB, rC, rD, rE, iEndTime, Mess);          // read user data from form
  iValues := iEndTime * 240;
  if Mess = '' then begin                                                                // proceed if there were no error
    // Write title and Lotka-Volterra model values
    LPrey := sPrey; LPredator := sPredator;
    if LPrey = 'prey' then
      LPrey := 'sample prey';
    if LPredator = 'predator' then
      LPredator := 'sample predator';
    fLV1G.stTitle.Caption := 'Predator-prey model: ';
    if rN0 = 0 then
      fLV1G.stTitle.Caption := fLV1G.stTitle.Caption + LPredator + ' (no prey).'
    else if rP0 = 0 then
      fLV1G.stTitle.Caption := fLV1G.stTitle.Caption + LPrey + ' (no predator).'
    else
    fLV1G.stTitle.Caption := fLV1G.stTitle.Caption + LPredator + ' and ' + LPrey + '.';
    S := 'Lotka-Volterra values:  ';
    if mSettingsAltFormulae.Checked then
      S += 'α='
    else
    S += 'a=';
    S += FloatToStr(rA);
    if mSettingsAltFormulae.Checked then
      S += '  β='
    else
      S += '  b=';
    S += FloatToStr(rB);
    if mSettingsAltFormulae.Checked then
      S += '  γ='
    else
      S += '  c=';
    S += FloatToStr(rC);
    if mSettingsAltFormulae.Checked then begin
      S += '  ε=' + FloatToStr(rE);
    end
    else begin
      S += '  d=' + FloatToStr(rD);
    end;
    if mSettingsLogistic.Checked then
      S += '  (prey carrying capacity K = ' + FloatToStr(rK) + ')';
    fLV1G.stLV.Caption := S;
    // Calculate Lotka-Volterra model values and create the graph
    Lotka_Volterra_XY(rN0, rP0, rK, rA, rB, rC, rD, rE, iValues, iEndTime, aSimulationTime, aPrey, aPredator);
    GraphClean(fLV1G.iImageWidth, fLV1G.iImageHeight);
    DrawAxis(fLV1G.iGraphLeft, fLV1G.iGraphRight, fLV1G.iGraphTop, fLV1G.iGraphBottom, sPrey, sPredator);
    DrawGraph(iValues, iEndTime, fLV1G.iGraphLeft, fLV1G.iGraphRight, fLV1G.iGraphTop, fLV1G.iGraphBottom, aSimulationTime, aPrey, aPredator);
    // Show the graph window
    fLV1G.Show;
  end;
end;

{ Simulation timer routine }

procedure TfLV1.tiBacteriaTimer(Sender: TObject);

var
  NPrey, NPredator, I: Integer;
  DPrey, DPredator: Real;

begin
  // If not yet all T / N and P values have been processed
  if iValue <= iValues then begin
    // Display actual prey and predator populations
    edSimPrey.Text  := IntToStr(Round(aPrey[iValue]));
    edSimPredator.Text  := IntToStr(Round(aPredator[iValue]));
    // Simulation start (time = 0)
    if iValue = 0 then begin
      // Set the correct timer interval
      tiLV.Interval := iInterval;
      // Previous step prey and predator imaages
      rOldPrey := aPrey[0]; rOldPredator := aPredator[0];
    end
    // Simulation running (t > 0)
    else begin
      // Simulation time elapsed (in years)
      edSimTime.Text := FloatToStrF(iValue / (iValues div iEndTime), ffFixed, 0, 0);
      // Set ΔN and ΔP to 0 (and clear the field) at display for last value
      if iValue = iValues then begin
        DPrey := 0;
        DPredator := 0;
      end
      // Calculate prey and predator differences (compared to previous simulation step)
      else begin
        DPrey := aPrey[iValue] - rOldPrey;
        DPredator := aPredator[iValue] - rOldPredator;
      end;
      // Display prey and predator differences
      if Abs(DPrey) < 1 then
        edSimPreyDiff.Text := ''
      else
        edSimPreyDiff.Text := IntToStr(Round(DPrey));
      if Abs(DPredator) < 1 then
        edSimPredatorDiff.Text := ''
      else
        edSimPredatorDiff.Text := IntToStr(Round(DPredator));
      // Use 'lime' to indicate that animal number increses, 'red' if it decreases
      if edSimPreyDiff.Text = '' then
        edSimPreyDiff.Color := clDefault
      else if DPrey > 0 then
        edSimPreyDiff.Color := clLime
      else if DPrey < 0 then
        edSimPreyDiff.Color := clRed;
      if edSimPredatorDiff.Text = '' then
        edSimPredatorDiff.Color := clDefault
      else if DPredator > 0 then
        edSimPredatorDiff.Color := clLime
      else if DPredator < 0 then
        edSimPredatorDiff.Color := clRed;
    end;
    // Calculate how many prey and predator images have to be displayed
    NPrey := 0; NPredator := 0;
    if rMaxPrey > 0 then
      NPrey := Round((aPrey[iValue] / rMaxPrey) * iMaxPrey);
    if rMaxPredator > 0 then
      NPredator := Round((aPredator[iValue] / rMaxPredator) * iMaxPredator);
    // If there are more prey than before, display further images (setting their "Visible" property to 'visible')
    if NPrey > iDisplayedPrey then begin
      for I := iDisplayedPrey + 1 to NPrey do begin
        // Prey images displayed at random positions
        imPrey[I - 1].Left := Random(shSimulation.Width - 50) + 20;
        imPrey[I - 1].Top := Random(shSimulation.Height - 60) + 80;
        imPrey[I - 1].Visible := True;
      end;
    end
    // If there are less prey than before, remove images (setting their "Visible" property to not 'visible')
    else if NPrey < iDisplayedPrey then begin
      for I := iDisplayedPrey downto NPrey  + 1 do begin
        imPrey[I - 1].Visible := False;
      end
    end;
    // If there are more predators than before, display further images (setting their "Visible" property to 'visible')
    if NPredator > iDisplayedPredator then begin
      for I := iDisplayedPredator + 1 to NPredator do begin
        // Predator images displayed at random positions
        imPredator[I - 1].Left := Random(shSimulation.Width - 70) + 20;
        imPredator[I - 1].Top := Random(shSimulation.Height - 50) + 80;
        imPredator[I - 1].Visible := True;
      end;
    end
    // If there are less predators than before, remove images (setting their "Visible" property to not 'visible')
    else if NPredator < iDisplayedPredator then begin
      for I := iDisplayedPredator downto NPredator  + 1 do begin
        imPredator[I - 1].Visible := False;
      end
    end;
    // Save number of prey and predators actually displayed
    iDisplayedPrey := NPrey; iDisplayedPredator := NPredator;
    // Next time value (simulation step = 1 year population dynamics)
    rOldPrey := aPrey[iValue]; rOldPredator := aPredator[iValue];
    iValue += iValues div iEndTime;
  end
  // If all T / N and P values have been processed
  else begin
    // Stop timer
    tiLV.Enabled := False;
    // Disable buttons (user has to choose "File > New" to clear the form before starting new simulation)
    btStart.Caption := 'Start'; btStart.Enabled := False;
    btPause.Caption := 'Pause'; btPause.Enabled := False;
  end;
end;

end.

