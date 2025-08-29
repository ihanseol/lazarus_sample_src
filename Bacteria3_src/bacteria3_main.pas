{***************************************}
{* Main unit for Bacteria3 application *}
{***************************************}

unit bacteria3_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, bacteria3_graph, bacteria3_analysis, bacteria3_help;

const
  MaxBacteria = 600;
  MaxNutrient = 1150;

type
  { TfBacteria3 }
  TfBacteria3 = class(TForm)
    mSettingsStep: TMenuItem;
    mSettingsStepHour: TMenuItem;
    mSettingsStepMin: TMenuItem;
    mMenu: TMainMenu;
    mFile, mFileNew, mFileReset, mFileExit: TMenuItem;
    mSettings, mSettingsTime, mSettingsTimeAuto, mSettingsTimeCustom: TMenuItem;
    mHelp, mHelpBacteria, mHelpHelp, mHelpAbout: TMenuItem;
    imBacteria: TImage;
    shCulture, shNutrient0, shNutrient: TShape;
    Label0, Label1, Label2, Label3, Label4, Label5, Label7, Label8: TLabel;
    Label9, Label10, Label11, Label12, Label13, Label14, Label15, Label16, Label17: TLabel;
    laVolumeUnit, laFlowingUni, laFreshUnit: TLabel;
    laBacteriaUnit, laNutrientUnit, laGrowthUnit, laHalfsatUnit: TLabel;
    laEndTimeUnit: TLabel;
    edVolume, edFlow, edFresh: TEdit;
    edBacteria, edNutrient: TEdit;
    edGrowth, edHalfSat, edYield: TEdit;
    edEndTime: TEdit;
    edSimTime, edSimBacteria, edSimBacteriaDiff, edSimNutrient: TEdit;
    btStart: TButton;
    btPause: TButton;
    btGraph: TButton;
    btAnalysis: TButton;
    tiBacteria: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileResetClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpBacteriaClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mSettingsStepHourClick(Sender: TObject);
    procedure mSettingsStepMinClick(Sender: TObject);
    procedure mSettingsTimeAutoClick(Sender: TObject);
    procedure mSettingsTimeCustomClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btGraphClick(Sender: TObject);
    procedure btAnalysisClick(Sender: TObject);
    procedure tiBacteriaTimer(Sender: TObject);
  private
    iMaxBacteria, iEndTime, iValues, iUpdate, iInterval, iValue, iBacteriaDisplayed: Integer;
    rN0, rS0, rR, rA, rY, rChV, rChR, rChF, rChD, rMaxBacteria: Double;
    rLambda, rSurvival, rWashoutN, rWashoutS, rSurvivalN, rSurvivalS: Double;
    sEquilibrium: string;
    aiBacteria: array[1..MaxBacteria] of TImage;
  end;

var
  fBacteria3: TfBacteria3;
  GrowthTime, Bacteria, Nutrient: array of Double;

implementation

{$R *.lfm}

{ Get max value of array of double }

function GetMax(var Arr: array of Double): Double;

var
  I: Integer;
  Max: Double;

begin
  Max := Arr[0];
  for I := 0 to Length(Arr) - 1 do begin
    if Arr[I] > Max then
      Max := Arr[I];
  end;
  GetMax := Max;
end;

{ Format values for proper output on the graph }

function GraphFormat(N: Double): string;

var
  SN: string;

begin
  SN := FloatToStrF(N, ffFixed, 0, 2);
  // Add spaces to the left to rigth-justify the values
  if Length(SN) = 4 then
    SN := '    ' + SN
  else if Length(SN) = 5 then
    SN := '  ' + SN;
  GraphFormat := SN;
end;

{ Format values for proper output on the grid }

function GridFormat(N: Double): string;

var
  I: Integer;
  SN: string;

begin
  SN := FloatToStrF(N, ffFixed, 0, 4);
  // Add spaces to the left to rigth-justify the values
  for I := 1 to 8 - Length(FloatToStrF(N, ffFixed, 0, 4)) do
    SN := ' ' + SN;
  GridFormat := SN;
end;

{ Get user data for this simulation from form }

procedure ReadData(out ChV, ChF, ChR, ChD, N0, S0, R, A, Y: Double; out Mess: string);

// There is no 'real' validity check of the values that the user enters; thus up to her not to
// choose 'senseless' data. The sub also calculates and returns the dilution rate D = F / V.

begin
  Mess := '';
  if fBacteria3.edVolume.Text = '' then
    ChV := 0
  else
    ChV := StrToFloat(fBacteria3.edVolume.Text);
  if fBacteria3.edFlow.Text = '' then
    ChF := 0
  else
    ChF := StrToFloat(fBacteria3.edFlow.Text);;
  if fBacteria3.edFresh.Text = '' then
    ChR := 0
  else
    ChR := StrToFloat(fBacteria3.edFresh.Text);;
  if fBacteria3.edBacteria.Text = '' then
    N0 := 0
  else
    N0 := StrToFloat(fBacteria3.edBacteria.Text);
  if fBacteria3.edNutrient.Text = '' then
    S0 := 0
  else
    S0 := StrToFloat(fBacteria3.edNutrient.Text);
  if fBacteria3.edGrowth.Text = '' then
    R := 0
  else
    R := StrToFloat(fBacteria3.edGrowth.Text);
  if fBacteria3.edHalfSat.Text = '' then
    A := 0
  else
    A := StrToFloat(fBacteria3.edHalfSat.Text);
  if fBacteria3.edYield.Text = '' then
    Y := 0
  else
    Y := StrToFloat(fBacteria3.edYield.Text);
  if ChV <= 0 then
    Mess := 'Chemostat volume must be greater than 0'
  else if ChF <= 0 then
    Mess := 'Fresh nutrient flow must be greater than 0'
  else if ChR <= 0 then
    Mess := 'Fresh nutrient concentration must be greater than 0'
  else if N0 <= 0 then
    Mess := 'Initial bacteria biomass must be greater than 0'
  else if S0 <= 0 then
    Mess := 'Initial nutrient concentration must be greater than 0'
  else if R <= 0 then
    Mess := 'Bacteria growth rate must be greater than 0'
  else if A <= 0 then
    Mess := 'Half-saturation constant must be greater than 0'
  else if Y <= 0 then
    Mess := 'Growth yield coefficient must be greater than 0';
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0)
  else
    ChD := ChF / ChV;                                                                    // dilution rate
end;

{ Clean the graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis and write labels }

procedure DrawAxis(XL, XR, YT, YB: Integer; LegendX, Legend1Y, Legend2Y: string);

var
  YAX, YLX: Integer;

begin
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);            // draw X-axis
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);              // draw Y-axis
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(XR - 15, YLX, LegendX);              // display X-axis legend
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Font.Color := clRed;                         // color (red) for bacteria
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(6, 5, Legend1Y);                     // display Y-axis legend for bacteria
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlue;                        // color (blue) for nutrient
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(6, 23, Legend2Y);                    // display Y-axis legend for nutrient
end;

{ Monod based chemstat growth model }

procedure Chemostat(var N, S: Double; ChD, ChR, R, A, Y, dT: Double);

var
  dN, dS: Double;

begin
  dN := (N * (((R * S) / (A + S)) - ChD)) * dT;
  dS := ((ChD * (ChR - S)) - (((1 / Y) * N) * ((R * S) / (A + S)))) * dT;
  N += dN;
  if N < 0 then
    N := 0;
  S += dS;
  if S < 0 then
    S := 0;
end;

{ Compute T, N and S values for bacteria growth using the Monod based chemostat growth model }

procedure Chemostat_XY(N0, S0, ChD, ChR, R, A, Y: Double; Values, Endtime: Integer; RealConc: Boolean);

var
  I: Integer;
  T, N, S, dT: Double;

begin
  SetLength(GrowthTime, 1); SetLength(Bacteria, 1); SetLength(Nutrient, 1);
  // Initial values: for T = 0: N = N0, S = S0
  GrowthTime[0] := 0; Bacteria[0] := N0; Nutrient[0] := S0;
  N := N0; S := S0;
  // Compute numerical approxiation of  differential equations using the Euler method
  dT := Endtime / Values;
  for I := 1 to Values do begin
    T := I * Endtime / Values;
    Chemostat(N, S, ChD, ChR, R, A, Y, dT);                                              // Monod based chemostat growth model values for T = (T - 1) + dT
    SetLength(GrowthTime, Length(GrowthTime) + 1);
    SetLength(Bacteria, Length(Bacteria) + 1);
    SetLength(Nutrient, Length(Nutrient) + 1);
    GrowthTime[I] := T;
    Bacteria[I] := N;
    Nutrient[I] := S;
  end;
  if not RealConc then begin
    // Return N / (a + y) for N and S / a for S (better graph)
    for I := 0 to Values do begin
      Bacteria[I] /= (A * Y);
      Nutrient[I] /= A;
    end;
  end;
end;

{ Chemostat bacteria growth analysis }

procedure Chemostat_Analyse(R, A, Y, ChR, ChD: Double; out Eq: string; out Lambda, Survival, WashoutN, WashoutS, SurvivalN, SurvivalS: Double);

begin
  Lambda := A * ChD / (R - ChD);
  Survival := R * ChR / (A + ChR);
  if Survival > ChD then
    Eq := 'survival'
  else
    Eq := 'washout';
  WashoutN := 0; WashoutS := ChR;
  SurvivalN := Y * (ChR - Lambda); SurvivalS := Lambda;
end;

{ Draw the bacteria/nutrient concentration vs time graph (as calculated by the Monod based chemostat model) }

procedure DrawGraph(Values, EndTime: Integer; Eq: string; EqBact, EqNutr, MaxBact: Double; XL, XR, YT, YB: Integer);

var
  X, Y, N0Y, NMaxY, NEqY, S0Y, SEqY, IDiv, I: Integer;
  MaxConc: Double;
  SN: string;

begin
  // Maximum concentrations
  if MaxBact > Nutrient[0] then
    MaxConc := MaxBact
  else
    MaxConc := Nutrient[0];
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 2;
  // Draw bacteria concentration graph (in red)
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clRed;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Bacteria[I] / MaxConc) * (YB - YT));
    if I = 0 then                                                                        // first point of the curve:
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)                             // position the pen
    else                                                                                 // other points of the curve:
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);                            // draw line from previous point to here
  end;
  // Draw nutrient concentration graph (in blue)
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Nutrient[I] / MaxConc) * (YB - YT));
    if I = 0 then
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)
    else
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);
  end;
  // Draw ticks and write values of X-axis
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  IDiv := (EndTime div 25) + 1;
  for I := 1 to EndTime - IDiv do begin
    if I mod IDiv = 0 then begin                                                         // limiting the number of ticks to 25
      X := Round(I * (XR - XL) / EndTime) + XL;
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(X, YB - 5, X, YB + 5);
      SN := IntToStr(I);
      if Length(SN) = 1 then                                                             // adapt position of time value, depending on it's 1 or 2 digits
        X -= 3
      else
        X -= 6;
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
      fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(X, YB + 7, SN);
    end;
  end;
  // Draw ticks and write values of Y-axis
  NMaxY := -1; NEqY := -1; SEqY := -1;
  N0Y := YB - Round((Bacteria[0] / MaxConc) * (YB - YT));
  if Eq = 'survival' then
    NMaxY := YB - Round((MaxBact / MaxConc) * (YB - YT));
  S0Y := YB - Round((Nutrient[0] / MaxConc) * (YB - YT));
  if Eq = 'survival' then
    NEqY := YB - Round((EqBact / MaxConc) * (YB - YT));
  SEqY := YB - Round((EqNutr / MaxConc) * (YB - YT));
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, N0Y, XL + 5, N0Y);
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(42, N0Y - 8, GraphFormat(Bacteria[0]));
  if NMaxY <> -1 then begin
    fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, NMaxY, XL + 5, NMaxY);
    fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(42, NMaxY - 8, GraphFormat(MaxBact));
  end;
  if NEqY <> -1 then begin
    fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, NEqY, XL + 5, NEqY);
    fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(42, NEqY - 8, GraphFormat(EqBact));
  end;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, S0Y, XL + 5, S0Y);
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(42, S0Y - 8, GraphFormat(Nutrient[0]));
  if SEqY <> -1 then begin
    fBacteria3G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, SEqY, XL + 5, SEqY);
    fBacteria3G.imGraph.Picture.Bitmap.Canvas.TextOut(42, SEqY - 8, GraphFormat(EqNutr));
  end;
  // Reset pen width and pen and font colors to default values
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clDefault;
  fBacteria3G.imGraph.Picture.Bitmap.Canvas.Font.Color := clDefault;
end;

{*************}
{ TfBacteria3 }
{*************}

{ Application start: Create bacteria images; initialisation }

procedure TfBacteria3.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to MaxBacteria do begin
    aiBacteria[I] := TImage.Create(fBacteria3.aiBacteria[I]);
    aiBacteria[I].Parent  := Self;
    aiBacteria[I].Width   := imBacteria.Width;
    aiBacteria[I].Height  := imBacteria.Height;
    aiBacteria[I].Picture := imBacteria.Picture;
    aiBacteria[I].Visible := False;
  end;
  SetLength(GrowthTime, 0); SetLength(Bacteria, 0); SetLength(Nutrient, 0);
  iUpdate := 60; iInterval := 100;                                                       // simulation update = 10 times / sec, each being 1 min of real growth time
end;

{ Menu item "File > New": Clear simulation form }

procedure TfBacteria3.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  edSimTime.Text := '';
  edSimBacteria.Text := '';
  edSimBacteriaDiff.Text := ''; edSimBacteriaDiff.Color := clDefault;
  edSimNutrient.Text := '';
  for I := 1 to MaxBacteria do
    aiBacteria[I].Visible := False;                                                      // remove all bacteria by hiding them
  shNutrient.Width := MaxNutrient;                                                       // set nutrient indicator bar to maximum
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Enabled := False;
  btStart.SetFocus;
end;

{ Menu item "File > Reset": Use default culture values; clear simulation form }

procedure TfBacteria3.mFileResetClick(Sender: TObject);

begin
  edVolume.Text := '5'; edFlow.Text := '5'; edFresh.Text := '0,06';
  edBacteria.Text := '0,023'; edNutrient.Text := '0,08';
  edGrowth.Text := '1,35'; edHalfSat.Text := '0,04'; edYield.Text := '0,23';
  edEndTime.Text := '10';
  mFileNew.Click;                                                                        // execute "File > New" to clear the form
end;

{ Menu item "File > Exit": Exit the application }

procedure TfBacteria3.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Simulation update interval > 1 hour": Set simulation update interval to 1 hour }

procedure TfBacteria3.mSettingsStepHourClick(Sender: TObject);

// This means that each update of the simulation window corresponds to 1 hour real growth time

begin
  mSettingsStepHour.Checked := True;
  mSettingsStepMin.Checked := False;
  iUpdate := 1;
end;

{ Menu item "Settings > Simulation update interval > 1 minute": Set simulation update interval to 1 min }

procedure TfBacteria3.mSettingsStepMinClick(Sender: TObject);

// This means that each update of the simulation window corresponds to 1 minute real growth time

begin
  mSettingsStepHour.Checked := False;
  mSettingsStepMin.Checked := True;
  iUpdate := 60;
end;

{ Menu item "Settings > Simulation time interval > Automatic": Set simulation time interval to default (1 sec) }

procedure TfBacteria3.mSettingsTimeAutoClick(Sender: TObject);

// This means that the simulation window is updated once a second

begin
  mSettingsTimeAuto.Checked := True;
  mSettingsTimeCustom.Checked := False;
  iInterval := 1000;
end;

{ Menu item "Settings > Simulation time interval > Custom": Ask user for simulation time interval }

procedure TfBacteria3.mSettingsTimeCustomClick(Sender: TObject);

// This means that the simulation window is updated every the value entered seconds (e.g. for 0.1 -> 10 updates per sec )

var
  TimeUnit, S: string;

begin
  mSettingsTimeAuto.Checked := False;
  mSettingsTimeCustom.Checked := True;
  if mSettingsStepHour.Checked then
    TimeUnit := 'hour'
  else
    TimeUnit := 'min';
  S := InputBox('Simulation time interval', 'Simulation time in sec for ' + TimeUnit + ' real time', FloatToStr(iInterval / 1000));
  if S <> '' then                                                                        // this is if user pushed "OK" (and not "Cancel")
    iInterval := Round(StrToFloat(S) * 1000);
end;

{ Menu item "Help > Bacteria growth": Display biology background help text }

procedure TfBacteria3.mHelpBacteriaClick(Sender: TObject);

begin
  if fBacteria3H.Visible then
    fBacteria3H.Hide;
  fBacteria3H.memoHelp.Lines.LoadFromFile('help1.txt');
  fBacteria3H.Show;
end;

{ Menu item "Help > Program help": Display program help text }

procedure TfBacteria3.mHelpHelpClick(Sender: TObject);

begin
  if fBacteria3H.Visible then
    fBacteria3H.Hide;
  fBacteria3H.memoHelp.Lines.LoadFromFile('help2.txt');
  fBacteria3H.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfBacteria3.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Biology simulation: Bacteria growth in the chemostat.' + LineEnding;
  S += 'Version 1.0, © allu, November-December 2018.';
  MessageDlg('About "Bacteria3"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/stop": Start resp. stop the simlation }

procedure TfBacteria3.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Starting simulation with actual parameters
  if btStart.Caption = 'Start' then begin
    ReadData(rChV, rChF, rChR, rChD, rN0, rS0, rR, rA, rY, Mess);                        // read user data from form
    if Mess = '' then begin                                                              // proceed if there were no error message
      if edEndTime.Text = '' then
        iEndTime := 0
      else
        iEndTime := StrToInt(edEndTime.Text);                                            // (real) time for simulation run
      if iEndTime < 1 then
        MessageDlg('Invalid data', 'Max. graph time must be at least 1h!', mtError, [mbOK], 0)
      else begin
        iValues := 360 * iEndTime;                                                       // number of time values to do calculation for
        // Get Monod based chemostat model values (incl. arrays with T / N and S values)
        Chemostat_XY(rN0, rS0, rChD, rChR, rR, rA, rY, iValues, iEndTime, True);
        Chemostat_Analyse(rR, rA, rY, rChR, rChD, sEquilibrium, rLambda, rSurvival, rWashoutN, rWashoutS, rSurvivalN, rSurvivalS);
        // Determine maximum bacteria concentration
        rMaxBacteria := GetMax(Bacteria);
        // Start the timer (with simulation time = 0)
        iValue := 0; iBacteriaDisplayed := 0;
        tiBacteria.Enabled := True;
        // Next push on button will be "Stop"
        btStart.Caption := 'Stop';
        btPause.Caption := 'Pause';
        btPause.Enabled := True;
      end;
    end;
  end
  // Stop simulation
  else begin
    // Disable the timer
    tiBacteria.Enabled := False;
    // Disable buttons (user must choose "File > New" to clear the form before running a new simulation)
    btStart.Caption := 'Start';
    btStart.Enabled := False;
    btPause.Caption := 'Pause';
    btPause.Enabled := False;
  end;
end;

{ Button "Pause/Resume": Pause resp. resume the simulation }

procedure TfBacteria3.btPauseClick(Sender: TObject);

begin
  if btStart.Caption = 'Stop' then begin
    if btPause.Caption = 'Pause' then begin
      tiBacteria.Enabled := False;
      btPause.Caption := 'Resume';
    end
    else begin
      tiBacteria.Enabled := True;
      btPause.Caption := 'Pause';
    end;
  end;
end;

{ Button "Graph": Draw the graph using actual parameters (will be done on form fBacteria3G) }

procedure TfBacteria3.btGraphClick(Sender: TObject);

var
  LegendX, Legend1Y, Legend2Y, Mess, S: string;

begin
  ReadData(rChV, rChF, rChR, rChD, rN0, rS0, rR, rA, rY, Mess);                          // read user data from form
  if Mess = '' then begin                                                                // proceed if there were no error
    if edEndTime.Text = '' then
      iEndTime := 0
    else
      iEndTime := StrToInt(edEndTime.Text);                                              // maximum time on graph
    if iEndTime < 1 then
      MessageDlg('Invalid data', 'Max. graph time must be at least 1h!', mtError, [mbOK], 0)
    else begin
      iValues := 360 * iEndTime;                                                  // number of time values to do the calculation for
      // Write title and chemostat and Monod model values
      fBacteria3G.stTitle.Caption := 'Bacteria growth in the chemostat.';
      S := 'Chemostat properties:  ';
      S += 'V=' + FloatToStr(rChV);
      S += '  F=' + FloatToStr(rChF);
      S += '  R=' + FloatToStr(rChR);
      S += '  D=' + FloatToStrF(rChD, ffFixed, 0, 2);
      fBacteria3G.stChemostat.Caption := S;
      S := 'Monod based growth model:  ';
      S += 'r=' + FloatToStr(rR);
      S += '  a=' + FloatToStr(rA);
      S += '  γ=' + FloatToStr(rY);
      fBacteria3G.stMonod.Caption := S;
      // Calculate Monod based chemostat growth model values and create the graph
      GraphClean(fBacteria3G.iImageWidth, fBacteria3G.iImageHeight);
      LegendX := 'Time [h]'; Legend1Y := 'Bacteria (*)'; Legend2Y := 'Nutrient (*)';
      DrawAxis(fBacteria3G.iGraphLeft, fBacteria3G.iGraphRight, fBacteria3G.iGraphTop, fBacteria3G.iGraphBottom, LegendX, Legend1Y, Legend2Y);
      Chemostat_XY(rN0, rS0, rChD, rChR, rR, rA, rY, iValues, iEndTime, False);
      rMaxBacteria := GetMax(Bacteria);
      Chemostat_Analyse(rR, rA, rY, rChR, rChD, sEquilibrium, rLambda, rSurvival, rWashoutN, rWashoutS, rSurvivalN, rSurvivalS);
      if sEquilibrium = 'survival' then
        DrawGraph(iValues, iEndTime, sEquilibrium, rSurvivalN / (rA * rY), rSurvivalS / rA, rMaxBacteria, fBacteria3G.iGraphLeft, fBacteria3G.iGraphRight, fBacteria3G.iGraphTop, fBacteria3G.iGraphBottom)
      else
        DrawGraph(iValues, iEndTime, sEquilibrium, 0, rWashoutS / rA, rMaxBacteria, fBacteria3G.iGraphLeft, fBacteria3G.iGraphRight, fBacteria3G.iGraphTop, fBacteria3G.iGraphBottom);
      fBacteria3G.Show;
    end;
  end;
end;

{ Button "Analyse": Analyse chemostat growth }

procedure TfBacteria3.btAnalysisClick(Sender: TObject);

var
  Mess: string;

begin
  ReadData(rChV, rChF, rChR, rChD, rN0, rS0, rR, rA, rY, Mess);                          // read user data from form
  if Mess = '' then begin                                                                // proceed if there were no error
    Chemostat_Analyse(rR, rA, rY, rChR, rChD, sEquilibrium, rLambda, rSurvival, rWashoutN, rWashoutS, rSurvivalN, rSurvivalS);
    fBacteria3A.sgAnalysis.Cells[1, 0] := GridFormat(rChD);
    fBacteria3A.sgAnalysis.Cells[1, 1] := GridFormat(1 / rChD);
    fBacteria3A.sgAnalysis.Cells[1, 2] := GridFormat(rSurvival);
    fBacteria3A.sgAnalysis.Cells[1, 3] := GridFormat(rLambda);
    fBacteria3A.sgAnalysis.Cells[2, 3] := GridFormat(rLambda / rA);
    fBacteria3A.sgAnalysis.Cells[1, 4] := GridFormat(rWashoutN);
    fBacteria3A.sgAnalysis.Cells[2, 4] := GridFormat(rWashoutN / (rA * rY));
    fBacteria3A.sgAnalysis.Cells[1, 5] := GridFormat(rWashoutS);
    fBacteria3A.sgAnalysis.Cells[2, 5] := GridFormat(rWashoutS / rA);
    if sEquilibrium = 'survival' then begin
      fBacteria3A.sgAnalysis.Cells[1, 6] := GridFormat(rSurvivalN);
      fBacteria3A.sgAnalysis.Cells[2, 6] := GridFormat(rSurvivalN / (rA * rY));
      fBacteria3A.sgAnalysis.Cells[1, 7] := GridFormat(rSurvivalS);
      fBacteria3A.sgAnalysis.Cells[2, 7] := GridFormat(rSurvivalS / rA);
      fBacteria3A.stEquilibrium.Font.Color := clDefault;
    end
    else begin
      fBacteria3A.sgAnalysis.Cells[1, 6] := '  ------';
      fBacteria3A.sgAnalysis.Cells[1, 7] := '  ------';
      fBacteria3A.sgAnalysis.Cells[2, 6] := '  ------';
      fBacteria3A.sgAnalysis.Cells[2, 7] := '  ------';
      fBacteria3A.stEquilibrium.Font.Color := clRed;
    end;
    fBacteria3A.stEquilibrium.Caption := 'N(t) will converge to the ' + sEquilibrium + ' equilibrium.';
    fBacteria3A.Show;
  end;
end;

{ Simulation timer routine }

procedure TfBacteria3.tiBacteriaTimer(Sender: TObject);

var
  NBacteria, I: Integer;
  DBacteria: Double;

begin
  // If not yet all T / N and S values have been processed
  if iValue <= iValues then begin
    // Display actual bacteria and nutrient concentrations
    edSimBacteria.Text  := FloatToStrF(Bacteria[iValue], ffFixed, 0, 8) + ' g/l';
    edSimNutrient.Text  := FloatToStrF(Nutrient[iValue], ffFixed, 0, 6) + ' g/l';
    // Simulation start (time = 0)
    if iValue = 0 then begin
      // Choose an appropriate number of bacteria to display for N = N0
      iMaxBacteria := Round(25 / (Bacteria[0] / rMaxBacteria));
      if iMaxBacteria < 2 * MaxBacteria / 3 then
        iMaxBacteria := Round(2 * MaxBacteria / 3)
      else if iMaxBacteria > MaxBacteria then
        iMaxBacteria := MaxBacteria;
      // Set the nutrient indicator to maximum for S = S0
      shNutrient.Width := MaxNutrient;
      // Set the correct timer interval
      tiBacteria.Interval := iInterval;
    end
    // Simulation running (t > 0)
    else begin
      edSimTime.Text := FloatToStrF(iValue / ((iValues div iEndTime) div iUpdate), ffFixed, 0, 0);
      if mSettingsStepHour.Checked then
        edSimTime.Text := edSimTime.Text + ' h'
      else
        edSimTime.Text := edSimTime.Text + ' min';
      // Set ΔN to 0 (and clear the field) at display for last value
      if iValue = iValues then
        DBacteria := 0
      else
        DBacteria := Bacteria[iValue] - Bacteria[iValue - 1];
      // Display number of bacteria
      if DBacteria = 0 then
        edSimBacteriaDiff.Text := ''
      else
        edSimBacteriaDiff.Text := FloatToStrF(DBacteria, ffFixed, 0, 8) + ' g/l';
      // Display bacteria difference; use 'lime' to indicate that bacteria number increses, 'red' if it decreases
      if DBacteria > 0 then
        edSimBacteriaDiff.Color := clLime
      else if DBacteria < 0 then
        edSimBacteriaDiff.Color := clRed
      else
        edSimBacteriaDiff.Color := clDefault;
    end;
    // Calculate how many bacteria images have to be displayed
    NBacteria := Round((Bacteria[iValue] / rMaxBacteria) * iMaxBacteria);
    // If there are more bacteria than before, display further images (setting their "Visible" property to 'visible')
    if NBacteria > iBacteriaDisplayed then begin
      for I := iBacteriaDisplayed + 1 to NBacteria do begin
        // Bacteria images displayed at random positions
        aiBacteria[I].Left := Random(shCulture.Width - 40) + 20;
        aiBacteria[I].Top := Random(shCulture.Height - 50) + 80;
        aiBacteria[I].Visible := True;
      end;
    end
    // If there are less bacteria than before, remove images (setting their "Visible" property to not 'visible')
    else if NBacteria < iBacteriaDisplayed then begin
      for I := iBacteriaDisplayed downto NBacteria  + 1 do begin
        aiBacteria[I].Visible := False;
      end
    end;
    // Set nutrient indicator bar to actual value
    shNutrient.Width := Round(Nutrient[iValue] / Nutrient[0] * MaxNutrient);
    // Save number of bacteria actually displayed
    iBacteriaDisplayed := NBacteria;
    // Next time value (simulation step = 1h/1min real time)
    iValue += (iValues div iEndTime) div iUpdate;
  end
  // If all T / N and S values have been processed
  else begin
    // Stop timer
    tiBacteria.Enabled := False;
    // Disable buttons (user has to choose "File > New" to clear the form before starting new simulation)
    btStart.Caption := 'Start';
    btStart.Enabled := False;
    btPause.Caption := 'Pause';
    btPause.Enabled := False;
  end;
end;

end.

