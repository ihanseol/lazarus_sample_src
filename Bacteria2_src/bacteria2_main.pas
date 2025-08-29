{***************************************}
{* Main unit for Bacteria2 application *}
{***************************************}

unit bacteria2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, PopupNotifier, bacteria2_graph, bacteria2_help;

const
  MaxBacteria = 600;
  MaxNutrient = 1150;

type
  { TfBacteria2 }
  TfBacteria2 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileExit: TMenuItem;
    mSettings, mSettingsTime, mSettingsTimeAuto, mSettingsTimeCustom: TMenuItem;
    mHelp, mHelpBacteria, mHelpHelp, mHelpAbout: TMenuItem;
    imBacteria: TImage;
    shCulture, shNutrient10, shNutrient1, shNutrient20, shNutrient2: TShape;
    Label3, Label4, Label5, Label6, Label7, Label8, Label9, Label10, Label11: TLabel;
    Label12, Label13, Label14, Label15, Label16: TLabel;
    Label17, Label18, Label19, Label20, Label21, Label22, Label23, Label24, Label25: TLabel;
    Label26, Label27, Label28, Label29, Label30, Label31, Label32, Label33, Label34: TLabel;
    edBacteria, edNutrient1, edNutrient2: TEdit;
    edGrowthRate1, edHalfSaturation1, edYieldCoeff1: TEdit;
    edGrowthRate2, edHalfSaturation2, edYieldCoeff2: TEdit;
    edEnzyme1, edSynthRate1, edDegradRate1: TEdit;
    edEnzyme2, edSynthRate2, edDegradRate2: TEdit;
    edSimTime, edSimBacteria, edSimBacteriaDiff, edSimNutrient1, edSimNutrient2: TEdit;
    btStart: TButton;
    btPause: TButton;
    btGraph: TButton;
    tiBacteria: TTimer;
    dlgOpen: TOpenDialog;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpBacteriaClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mSettingsTimeAutoClick(Sender: TObject);
    procedure mSettingsTimeCustomClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btGraphClick(Sender: TObject);
    procedure tiBacteriaTimer(Sender: TObject);
  private
    iMaxBacteria, iEndTime, iValues, iInterval, iValue, iBacteriaDisplayed: Integer;
    rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, rMaxBacteria: Double;
    aiBacteria: array[1..MaxBacteria] of TImage;
  end;

var
  fBacteria2: TfBacteria2;
  GrowthTime, Bacteria, Nutrient1, Nutrient2, Enzyme1, Enzyme2: array of Double;

implementation

{$R *.lfm}

{ Get user data for this simulation from form }

procedure ReadData(out N0, S10, S20, R1, R2, A1, A2, Y1, Y2, E10, E20, Alpha1, Alpha2, Beta1, Beta2: Double; out Mess: string);

// There is no 'real' validity check of the values that the user enters; thus up to her not to choose 'senseless' data
// If a field is left blank, it is either set to 0, either filled with a default value (growth yield coeff. and enzyme metabolism rates)

begin
  Mess := '';
  if fBacteria2.edBacteria.Text = '' then
    N0 := 0
  else
    N0 := StrToFloat(fBacteria2.edBacteria.Text);
  if fBacteria2.edNutrient1.Text = '' then
    S10 := 0
  else
    S10 := StrToFloat(fBacteria2.edNutrient1.Text);
  if fBacteria2.edNutrient2.Text = '' then
    S20 := 0
  else
    S20 := StrToFloat(fBacteria2.edNutrient2.Text);
  if fBacteria2.edGrowthRate1.Text = '' then
    R1 := 0
  else
    R1 := StrToFloat(fBacteria2.edGrowthRate1.Text);
  if fBacteria2.edGrowthRate2.Text = '' then
    R2 := 0
  else
    R2 := StrToFloat(fBacteria2.edGrowthRate2.Text);
  if fBacteria2.edHalfSaturation1.Text = '' then
    A1 := 0
  else
    A1 := StrToFloat(fBacteria2.edHalfSaturation1.Text);
  if fBacteria2.edHalfSaturation2.Text = '' then
    A2 := 0
  else
    A2 := StrToFloat(fBacteria2.edHalfSaturation2.Text);
  if fBacteria2.edYieldCoeff1.Text = '' then
    Y1 := 0.23                                                                 // growth yield coefficient 1 default value (0.23)
  else
    Y1 := StrToFloat(fBacteria2.edYieldCoeff1.Text);
  if fBacteria2.edYieldCoeff2.Text = '' then
    Y2 := 0.23                                                                 // growth yield coefficient 2 default value (0.23)
  else
    Y2 := StrToFloat(fBacteria2.edYieldCoeff2.Text);
  if fBacteria2.edEnzyme1.Text = '' then
    E10 := 0
  else
    E10 := StrToFloat(fBacteria2.edEnzyme1.Text);
  if fBacteria2.edEnzyme2.Text = '' then
    E20 := 0
  else
    E20 := StrToFloat(fBacteria2.edEnzyme2.Text);
  if fBacteria2.edSynthRate1.Text = '' then
    Alpha1 := 0.0001                                                           // enzyme 1 synthesis rate default value (0.001)
  else
    Alpha1 := StrToFloat(fBacteria2.edSynthRate1.Text);
  if fBacteria2.edSynthRate2.Text = '' then
    Alpha2 := Alpha1                                                           // enzyme 2 synthesis rate default value (equal to enzyme 1)
  else
    Alpha2 := StrToFloat(fBacteria2.edSynthRate2.Text);
  if fBacteria2.edDegradRate1.Text = '' then
    Beta1 := 0.05                                                              // enzyme 1 degradation rate default value (0.05)
  else
    Beta1 := StrToFloat(fBacteria2.edDegradRate1.Text);
  if fBacteria2.edDegradRate2.Text = '' then
    Beta2 := Beta1                                                             // enzyme 2 degradation rate default value (equal to enzyme 1)
  else
    Beta2 := StrToFloat(fBacteria2.edDegradRate2.Text);
  if N0 <= 0 then
    Mess := 'Initial bacteria biomass must be greater than 0'
  else if (S10 <= 0) or (S20 <= 0) then
    Mess := 'Initial nutrient concentrations must be greater than 0'
  else if (R1 <= 0) or (R2 <= 0) then
    Mess := 'Bacteria growth rates must be greater than 0'
  else if (A1 <= 0) or (A2 <= 0) then
    Mess := 'Half-saturation constants must be greater than 0'
  else if (Y1 <= 0) or (Y2 <= 0) then
    Mess := 'Growth yield coefficients must be greater than 0'
  else if (E10 <= 0) or (E20 <= 0) then
    Mess := 'Initial enzyme concentrations must be greater than 0'
  else if (Alpha1 < 0) or (Alpha2 < 0) or (Beta1 < 0) or (Beta2 < 0) then
    Mess := 'Enzyme metabolism rates must be greater than 0';
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Clean the graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis and write labels }

procedure DrawAxis(XL, XR, YT, YB: Integer; LegendX, Legend1Y, Legend2Y: string);

var
  YAX, YLX: Integer;

begin
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);  // draw X-axis
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);    // draw Y-axis
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(XR - 15, YLX, LegendX);    // display X-axis legend
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(10, 10, Legend1Y);         // display Y-axis legend for bacteria
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(10, 30, Legend2Y);         // display Y-axis legend for nutrient
end;

{ 2 substrates extended Monod growth model }

procedure Monod(var N, S1, S2, E1, E2: Double; R1, R2, A1, A2, Y1, Y2, Alpha1, Alpha2, Beta1, Beta2, E1Max, E2Max, dT: Double);

var
  dN, dS1, dS2, dE1, dE2, Er1, Er2, Mu1, Mu2, U1, U2, V1, V2: Double;

// For calculation details, cf. bacteria growth help text

begin
  Er1 := E1 / E1Max; Er2 := E2 / E2Max;
  Mu1 := (R1 * Er1 * S1) / (A1 + S1);
  Mu2 := (R2 * Er2 * S2) / (A2 + S2);
  if Mu1 + Mu2 = 0 then begin
    U1 := 0; U2 := 0;
  end
  else begin
    U1 := Mu1 / (Mu1 + Mu2);
    U2 := Mu2 / (Mu1 + Mu2);
  end;
  if Mu1 > Mu2 then begin
    if Mu1 <> 0 then begin
      V1 := Mu1 / Mu1;
      V2 := Mu2 / Mu1;
    end
    else begin
      V1 := 0; V2 := 0;
    end;
  end
  else begin
    if Mu2 <> 0 then begin
      V1 := Mu1 / Mu2;
      V2 := Mu2 / Mu2;
    end
    else begin
      V1 := 0; V2 := 0;
    end;
  end;
  dN  := ((Mu1 * V1 + Mu2 * V2) * N) * dT;
  dS1 := ((-1 / Y1) * Mu1 * V1 * N) * dT;
  dS2 := ((-1 / Y2) * Mu2 * V2 * N) * dT;
  dE1 := (((Alpha1 * S1 / (A1 + S1)) * U1) - (Beta1 * E1) - ((1 / N) * (dN / dT) * E1)) * dT;
  dE2 := (((Alpha2 * S2 / (A2 + S2)) * U2) - (Beta2 * E2) - ((1 / N) * (dN / dT) * E2)) * dT;
  S1 += dS1; S2 += dS2;
  E1 += dE1; E2 += dE2;
  N += dN;
  if S1 < 0 then
    S1 := 0;
  if S2 < 0 then
    S2 := 0;
  if E1 < 0 then
    E1 := 0;
  if E2 < 0 then
    E2 := 0;
end;

{ Compute T, N, S1/S2 and E1/E2 values for bacteria growth using the extended Monod growth model for 2 substrates }

procedure Monod_XY(N0, S10, S20, R1, R2, A1, A2, Y1, Y2, E10, E20, Alpha1, Alpha2, Beta1, Beta2: Double; Values, Endtime: Integer);

var
  I: Integer;
  T, N, S1, S2, E1, E2, E1Max, E2Max, dT: Double;

begin
  SetLength(GrowthTime, 1);
  SetLength(Bacteria, 1); SetLength(Nutrient1, 1); SetLength(Nutrient2, 1);
  SetLength(Bacteria, 1); SetLength(Enzyme1, 1); SetLength(Enzyme2, 1);
  // Initial values: for T = 0, N = N0, S = S0, E = E0
  GrowthTime[0] := 0; Bacteria[0] := N0;
  Nutrient1[0] := S10; Nutrient2[0] := S20;
  Enzyme1[0] := E10; Enzyme2[0] := E20;
  N := N0; S1 := S10; S2 := S20; E1 := E10; E2 := E20;
  // Calculate maximum enzyme concentrations
  E1Max := Alpha1 / (R1 + Beta1); E2Max := Alpha2 / (R2 + Beta2);
  // Compute numerical approxiation of  differential equations using the Euler method
  dT := Endtime / Values;
  for I := 1 to Values do begin
    T := I * Endtime / Values;
    Monod(N, S1, S2, E1, E2, R1, R2, A1, A2, Y1, Y2, Alpha1, Alpha2, Beta1, Beta2, E1Max, E2Max, dT);  // 2 substrates growth model values for T = (T - 1) + dT
    SetLength(GrowthTime, Length(GrowthTime) + 1);
    SetLength(Bacteria, Length(Bacteria) + 1);
    SetLength(Nutrient1, Length(Nutrient1) + 1);
    SetLength(Nutrient2, Length(Nutrient2) + 1);
    SetLength(Enzyme1, Length(Enzyme1) + 1);
    SetLength(Enzyme2, Length(Enzyme2) + 1);
    GrowthTime[I] := T;
    Bacteria[I] := N;
    Nutrient1[I] := S1; Nutrient2[I] := S2;
    Enzyme1[I] := E1;   Enzyme2[I] := E2;                                      // enzyme conc. returned actually not used; later version: include in graph...
  end;
end;

{ Calculate maximum bacteria concentration; estimate time unitil N = maximum; choose number of T / N and S values }

procedure Monod_Extrema(N0, S10, S20, R1, R2, A1, A2, Y1, Y2, E10, E20, Alpha1, Alpha2, Beta1, Beta2: Double; out Values, EndTime: Integer; out NMax: Double);

var
  N, OldN, S1, S2, E1, E2, E1Max, E2Max, T, dT: Double;

begin
  N := N0; S1 := S10; S2 := S20; E1 := E10; E2 := E20;
  dT := 1 / 3600;
  NMax := N0; T := 0;
  // Repeat adding dT to T and calculating corresponding N value unitl N = 0; save the time (in h) that this takes
  repeat
    T += dt;
    E1Max := Alpha1 / (R1 + Beta1); E2Max := Alpha2 / (R2 + Beta2);
    OldN := N;
    Monod(N, S1, S2, E1, E2, R1, R2, A1, A2, Y1, Y2, Alpha1, Alpha2, Beta1, Beta2, E1Max, E2Max, dT);
    if N > NMax then
      NMax := N;
  until N - OldN = 0;                                                          // maximum value is reached, when N doesn't increase anymore
  // Culture time on graph: some hours more than value calculated (is nicer on graph)
  EndTime := Trunc(T + 1) + 2;
  // Use maximum bacteria concentration calculated by formula, if maintenance is ignored; some more hours on graph X-axis
  Values := EndTime * 60;
end;

{ Format values for proper output on the graph }

function GFormat(N: Double): string;

var
  SN: string;

begin
  SN := FloatToStrF(N, ffFixed, 0, 2);
  // Add spaces to the left to rigth-justify the values
  if Length(SN) = 4 then
    SN := '    ' + SN
  else if Length(SN) = 5 then
    SN := '  ' + SN;
  GFormat := SN;
end;

{ Draw the bacteria/nutrient concentration vs time graph (as calculated by the 2 substrates extended Monod model) }

procedure DrawGraph(Values, EndTime: Integer; NMax: Double; XL, XR, YT, YB: Integer);

var
  X, Y, N0Y, NMaxY, S10Y, S20Y, IDiv, I: Integer;
  Max: Double;
  SN: string;

begin
  // Maximum concentration (may be S0 or Nmax)
  Max := Nutrient1[0];
  if Nutrient2[0] > Nutrient1[0] then
    Max := Nutrient2[0];
  if (NMax > Nutrient1[0]) and (NMax > Nutrient2[0]) then
    Max := NMax;
  NMaxY := -1;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 2;
  // Draw bacteria concentration graph (in red)
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clRed;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Bacteria[I] / Max) * (YB - YT));
    if I = 0 then                                                              // first point of the curve:
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)                   // position the pen
    else                                                                       // other points of the curve:
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);                  // draw line from previous point to here
    if I = 0 then
      N0Y := Y                                                                 // save Y-postion of initial bacteria concentration
    else if NMax - Bacteria[I] < 1E-5 then
      NMaxY := Y;                                                              // save Y-postion of maximum bacteria concentration
  end;
  // Draw nutrient concentration graph (in blue/green)
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Nutrient1[I] / Max) * (YB - YT));
    if I = 0 then
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)
    else
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);
    if I = 0 then                                                              // save Y-postion of initial nutrient 1 concentration
      S10Y := Y;
  end;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clGreen;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Nutrient2[I] / Max) * (YB - YT));
    if I = 0 then
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)
    else
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);
    if I = 0 then                                                              // save Y-postion of initial nutrient 2 concentration
      S20Y := Y;
  end;
  // Draw ticks and write values of X-axis
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  IDiv := (EndTime div 25) + 1;
  for I := 1 to EndTime - IDiv do begin
    if I mod IDiv = 0 then begin                                               // limiting the number of ticks to 25
      X := Round(I * (XR - XL) / EndTime) + XL;
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(X, YB - 5, X, YB + 5);
      SN := IntToStr(I);
      if Length(SN) = 1 then                                                   // adapt position of time value, depending on it's 1 or 2 digits
        X -= 3
      else
        X -= 6;
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
      fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(X, YB + 7, SN);
    end;
  end;
  // Draw ticks and write values of Y-axis
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, N0Y, XL + 5, N0Y);
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(42, N0Y - 8, GFormat(Bacteria[0]));
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, NMaxY, XL + 5, NMaxY);
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(42, NMaxY - 8, GFormat(NMax));
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, S10Y, XL + 5, S10Y);
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(42, S10Y - 8, GFormat(Nutrient1[0]));
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, S20Y, XL + 5, S20Y);
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.TextOut(42, S20Y - 8, GFormat(Nutrient2[0]));
  // Reset pen width and pen and font color to default values
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clDefault;
  fBacteria2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clDefault;
end;

{*************}
{ TfBacteria2 }
{*************}

{ Application start: Create bacteria images }

procedure TfBacteria2.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to MaxBacteria do begin
    aiBacteria[I] := TImage.Create(fBacteria2.aiBacteria[I]);
    aiBacteria[I].Parent  := Self;
    aiBacteria[I].Width   := imBacteria.Width;
    aiBacteria[I].Height  := imBacteria.Height;
    aiBacteria[I].Picture := imBacteria.Picture;
    aiBacteria[I].Visible := False;
  end;
  SetLength(GrowthTime, 0); SetLength(Bacteria, 0);
  SetLength(Nutrient1, 0); SetLength(Nutrient2, 0);
  SetLength(Enzyme1, 0); SetLength(Enzyme2, 0);
  iInterval := 1000;
end;

{ Menu item "File > New": Clear simulation form }

procedure TfBacteria2.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  edSimTime.Text := '';
  edSimBacteria.Text := '';
  edSimBacteriaDiff.Text := ''; edSimBacteriaDiff.Color := clDefault;
  edSimNutrient1.Text := '';    edSimNutrient2.Text := '';
  for I := 1 to MaxBacteria do
    aiBacteria[I].Visible := False;                                            // remove all bacteria by hiding them
  shNutrient1.Width := MaxNutrient;                                            // set nutrient display to maximum for S1 = S10
  shNutrient2.Width := MaxNutrient;                                            // set nutrient display to maximum for S2 = S20
  btStart.Caption := 'Start';
  btStart.Enabled := True;
  btPause.Caption := 'Pause';
  btPause.Enabled := False;
  btStart.SetFocus;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfBacteria2.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Simulation time interval > Automatic": Set simulation time interval to default (1sec for 15min) }

procedure TfBacteria2.mSettingsTimeAutoClick(Sender: TObject);

begin
  mSettingsTimeAuto.Checked := True;
  mSettingsTimeCustom.Checked := False;
  iInterval := 1000;
end;

{ Menu item "Settings > Simulation time interval > Custom": Ask user for simulation time interval }

procedure TfBacteria2.mSettingsTimeCustomClick(Sender: TObject);

var
  S: string;

// Simulation time interval = timer interval = how many secs real time (between 2 display updates) is used for 15min culture (growth) time

begin
  mSettingsTimeAuto.Checked := False;
  mSettingsTimeCustom.Checked := True;
  S := InputBox('Simulation time interval', 'Simulation time in sec for 15min real time', FloatToStr(iInterval / 1000));
  if S <> '' then                                                              // this is if user pushed "OK" (and not "Cancel")
    iInterval := Round(StrToFloat(S) * 1000);
end;

{ Menu item "Help > Bacteria growth": Display biology background help text }

procedure TfBacteria2.mHelpBacteriaClick(Sender: TObject);

begin
  if fBacteria2H.Visible then
    fBacteria2H.Hide;
  fBacteria2H.memoHelp.Lines.LoadFromFile('help1.txt');
  fBacteria2H.Show;
end;

{ Menu item "Help > Program help": Display program help text }

procedure TfBacteria2.mHelpHelpClick(Sender: TObject);

begin
  if fBacteria2H.Visible then
    fBacteria2H.Hide;
  fBacteria2H.memoHelp.Lines.LoadFromFile('help2.txt');
  fBacteria2H.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfBacteria2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Visible := False
  else begin
    S := 'Bacteria growth on 2 substrates (based on extended Monod model).' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, August, 2018.';
    pnAbout.Text := S;
    pnAbout.Visible := True;
  end;
end;

{ Button "Start/stop": Start resp. stop the simlation }

procedure TfBacteria2.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Starting simulation with actual parameters
  if btStart.Caption = 'Start' then begin
    ReadData(rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, Mess);  // read user data from form
    if Mess = '' then begin                                                    // proceed if there were no error message
      // Get 2 substrates extended Monod model values (incl. arrays with T / N, S and E values)
      Monod_Extrema(rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, iValues, iEndTime, rMaxBacteria);
      Monod_XY(rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, iValues, iEndTime);
      // Start the timer (with simulation time = 0)
      iValue := 0; iBacteriaDisplayed := 0;
      tiBacteria.Enabled := True;
      // Next push on button will be "Stop"
      btStart.Caption := 'Stop';
      btPause.Caption := 'Pause';
      btPause.Enabled := True;
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

procedure TfBacteria2.btPauseClick(Sender: TObject);

begin
  if btPause.Caption = 'Pause' then begin
    tiBacteria.Enabled := False;
    btPause.Caption := 'Resume';
  end
  else begin
    tiBacteria.Enabled := True;
    btPause.Caption := 'Pause';
  end;
end;

{ Button "Graph": Draw the graph using actual parameters (will be done on form fBacteria2G) }

procedure TfBacteria2.btGraphClick(Sender: TObject);

var
  LegendX, Legend1Y, Legend2Y, Mess: string;

begin
  ReadData(rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, Mess);  // read user data from form
  if Mess = '' then begin                                                      // proceed if there were no error
    // Write title
    fBacteria2G.stTitle.Caption := 'Bacteria growth on 2 substrates (Extended Monod model).';
    // Calculate 2 substrates extended Monod model values and create the graph
    GraphClean(fBacteria2G.iImageWidth, fBacteria2G.iImageHeight);
    LegendX := 'Time [h]'; Legend1Y := 'Bacteria'; Legend2Y := 'Nutrient';
    DrawAxis(fBacteria2G.iGraphLeft, fBacteria2G.iGraphRight, fBacteria2G.iGraphTop, fBacteria2G.iGraphBottom, LegendX, Legend1Y, Legend2Y);
    Monod_Extrema(rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, iValues, iEndTime, rMaxBacteria);
    Monod_XY(rN0, rS10, rS20, rR1, rR2, rA1, rA2, rY1, rY2, rE10, rE20, rAlpha1, rAlpha2, rBeta1, rBeta2, iValues, iEndTime);
    DrawGraph(iValues, iEndTime, rMaxBacteria, fBacteria2G.iGraphLeft, fBacteria2G.iGraphRight, fBacteria2G.iGraphTop, fBacteria2G.iGraphBottom);
    fBacteria2G.Show;
  end;
end;

{ Simulation timer routine }

procedure TfBacteria2.tiBacteriaTimer(Sender: TObject);

var
  NBacteria, I: Integer;
  DBacteria: Double;

begin
  // If not yet all T / N and S values have been processed
  if iValue <= iValues then begin
    // Display actual bacteria and nutrient concentrations
    edSimBacteria.Text  := FloatToStrF(Bacteria[iValue], ffFixed, 0, 8) + ' g/l';
    edSimNutrient1.Text  := FloatToStrF(Nutrient1[iValue], ffFixed, 0, 6) + ' g/l';
    edSimNutrient2.Text  := FloatToStrF(Nutrient2[iValue], ffFixed, 0, 6) + ' g/l';
    // Simulation start (time = 0)
    if iValue = 0 then begin
      // Choose an appropriate number of bacteria to display for N = N0
      iMaxBacteria := Round(25 / (Bacteria[0] / rMaxBacteria));
      if iMaxBacteria < 2 * MaxBacteria / 3 then
        iMaxBacteria := Round(2 * MaxBacteria / 3)
      else if iMaxBacteria > MaxBacteria then
        iMaxBacteria := MaxBacteria;
      // Set the correct timer interval
      tiBacteria.Interval := iInterval;
    end
    // Simulation running (t > 0)
    else begin
      edSimTime.Text := FloatToStrF(iValue / (iValues div iEndTime), ffFixed, 0, 2) + ' h';
      // Set ΔN to 0 (and clear the field) at display for last value
      if iValue = iValues then
        DBacteria := 0
      else
        DBacteria := Bacteria[iValue] - Bacteria[iValue - 1];
      if DBacteria = 0 then
        edSimBacteriaDiff.Text := ''
      else
        edSimBacteriaDiff.Text := FloatToStrF(DBacteria, ffFixed, 0, 8) + ' g/l';
      // Use 'lime' to indicate that bacteria number increses, 'red' if it decreases
      if DBacteria > 0 then
        edSimBacteriaDiff.Color := clLime
      else if DBacteria < 0 then
        edSimBacteriaDiff.Color := clRed
      else
        edSimBacteriaDiff.Color := clDefault;
    end;
    // Calculate how many bacteria images have to be displayed
    NBacteria := Round((Bacteria[iValue] / rMaxBacteria) * iMaxBacteria);
    if NBacteria > MaxBacteria then
      NBacteria := MaxBacteria;
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
    // Set nutrient display bar to actual value
    shNutrient1.Width := Round(Nutrient1[iValue] / Nutrient1[0] * MaxNutrient);
    shNutrient2.Width := Round(Nutrient2[iValue] / Nutrient2[0] * MaxNutrient);
    // Save number of bacteria actually displayed
    iBacteriaDisplayed := NBacteria;
    // Next time value (simulation step = 15min culture time)
    iValue += Round(iValues / (4 * iEndTime));
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

