{********************************************}
{* Main unit for LotkaVolterra2 application *}
{********************************************}

unit lv2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, IniFiles, lv2_graph, lv2_samples, lv2_help;

type
  TSimulationValues = array of Real;
  TAnimalImages = array of TImage;
  {*******}
  { TfLV2 }
  {*******}
  TfLV2 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSample, mFileExit: TMenuItem;
    mSettings, mSettingsTime, mSettingsPics, mSettingsNoWarnings: TMenuItem;
    mHelp, mHelpLV, mHelpLV3, mHelpHelp, mHelpAbout: TMenuItem;
    shSimulation: TShape;
    Label1, Label4, Label5, Label6, Label8, Label12, Label13, Label14: TLabel;
    Label15, Label16, Label17, Label18, Label19, Label20, Label21: TLabel;
    Label27, Label25, Label23, Label28, Label29, Label30: TLabel;
    Label31, Label32, Label33, Label26, Label24, Label22: TLabel;
    edName1, edName2, edName3: TEdit;
    edN0, edP0, edQ0: TEdit;
    edA, edB, edC, edD, edE, edF, edG, edEndTime: TEdit;
    edSimT, edSimN, edSimND, edSimP, edSimPD, edSimQ, edSimQD: TEdit;
    imgSpecies1, imgSpecies2, imgSpecies3: TImage;
    imgSpecies1b, imgSpecies2b, imgSpecies3b: TImage;
    btStart: TButton;
    btPause: TButton;
    btGraph: TButton;
    tiLV: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSampleClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsTimeClick(Sender: TObject);
    procedure mSettingsPicsClick(Sender: TObject);
    procedure mSettingsWarningsClick(Sender: TObject);
    procedure mHelpLVClick(Sender: TObject);
    procedure mHelpLV3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btGraphClick(Sender: TObject);
    procedure tiLVTimer(Sender: TObject);
  private
    iMaxSpecies1, iMaxSpecies2, iMaxSpecies3, iDisplayedSpecies1, iDisplayedSpecies2, iDisplayedSpecies3: Integer;
    iEndTime, iValues, iInterval, iValue, iOldSpecies1, iOldSpecies2, iOldSpecies3: Integer;
    iW1, iW2, iW3, iH1, iH2, iH3: Integer;
    rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, rMaxSpecies1, rMaxSpecies2, rMaxSpecies3: Real;
    sSpecies1, sSpecies2, sSpecies3: string;
    aSimulationTime, aSpecies1, aSpecies2, aSpecies3: TSimulationValues;
    imSpecies1, imSpecies2, imSpecies3: TAnimalImages;
  end;

const
  MaxAllAnimals = 753;
  SUB_0 = #$E2#$82#$80;

var
  fLV2: TfLV2;
  SamplesFile: TINIFile;

implementation

{$R *.lfm}

{ String to float conversion (independent of decimal separator) }

function StringToFloat(S: string): Real;

var
  S0: string;

begin
  S0 := FloatToStrF(1.0, ffFixed, 0, 1);                                       // convert a float number to string
  if Copy(S0, 2, 1) = ',' then                                                 // if decimal separator = comma, replace dot by comma
    S := StringReplace(S, '.', ',', []);                                       // now you can safely use the StrToFoat function of the SysUtils unit
  Result := StrToFloat(S);
end;

{ Get sign of a real number as string }

function Sign(R: Real): string;

var
  Sgn: string;

begin
  Sgn := '';
  if R > 0 then
    Sgn := '+'
  else if R < 0 then
    Sgn := '-';
  Result := Sgn;
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

{ Read sample data from .ini file and fill-in the form }

procedure ReadSampleData(Sample: Integer; out Species1, Species2, Species3: string;
  out N0, P0, Q0, A, B, C, D, E, F, G: Real; out EndTime: Integer);

var
  Section: string;

begin
  Section := 'Sample' + IntToStr(Sample);
  Species1 := SamplesFile.ReadString(Section, 'Species1', ''); fLV2.edName1.Text := Species1;
  Species2 := SamplesFile.ReadString(Section, 'Species1', ''); fLV2.edName2.Text := Species2;
  Species3 := SamplesFile.ReadString(Section, 'Species1', ''); fLV2.edName3.Text := Species3;
  N0 := StringToFloat(SamplesFile.ReadString(Section, 'n0', '0')); fLV2.edN0.Text := FloatToStr(N0);
  P0 := StringToFloat(SamplesFile.ReadString(Section, 'p0', '0')); fLV2.edP0.Text := FloatToStr(P0);
  Q0 := StringToFloat(SamplesFile.ReadString(Section, 'q0', '0')); fLV2.edQ0.Text := FloatToStr(Q0);
  A := StringToFloat(SamplesFile.ReadString(Section, 'a', '0')); fLV2.edA.Text := FloatToStr(A);
  B := StringToFloat(SamplesFile.ReadString(Section, 'b', '0')); fLV2.edB.Text := FloatToStr(B);
  C := StringToFloat(SamplesFile.ReadString(Section, 'c', '0')); fLV2.edC.Text := FloatToStr(C);
  D := StringToFloat(SamplesFile.ReadString(Section, 'd', '0')); fLV2.edD.Text := FloatToStr(D);
  E := StringToFloat(SamplesFile.ReadString(Section, 'e', '0')); fLV2.edE.Text := FloatToStr(E);
  F := StringToFloat(SamplesFile.ReadString(Section, 'f', '0')); fLV2.edF.Text := FloatToStr(F);
  G := StringToFloat(SamplesFile.ReadString(Section, 'g', '0')); fLV2.edG.Text := FloatToStr(G);
  EndTime := StrToInt(SamplesFile.ReadString(Section, 'time', '0')); fLV2.edEndTime.Text := IntToStr(EndTime);
end;

{ Get user data for this simulation from form }

procedure ReadUserData(out Species1, Species2, Species3: string;
  out N0, P0, Q0, A, B, C, D, E, F, G: Real; out EndTime: Integer; out Mess: string);

// There is no 'real' validity check of the values that the user enters; thus up to her not to choose 'senseless' data!

begin
  Mess := ''; N0 := 0; P0 := 0; Q0 := 0;
  A := 0; B := 0; C := 0; D := 0; E := 0; F := 0; G := 0;
  EndTime := 0;
  if fLV2.edName1.Text = '' then
    Species1 := 'lowest-level species'
  else
    Species1 := fLV2.edName1.Text;
  if fLV2.edName2.Text = '' then
    Species2 := 'mid-level species'
  else
    Species2 := fLV2.edName2.Text;
  if fLV2.edName3.Text = '' then
    Species3 := 'top-level species'
  else
    Species3 := fLV2.edName3.Text;
  if fLV2.edN0.Text <> '' then
    N0 := StringToFloat(fLV2.edN0.Text);
  if fLV2.edP0.Text <> '' then
    P0 := StringToFloat(fLV2.edP0.Text);
  if fLV2.edQ0.Text <> '' then
    Q0 := StringToFloat(fLV2.edQ0.Text);
  if fLV2.edA.Text <> '' then
    A := StringToFloat(fLV2.edA.Text);
  if fLV2.edB.Text <> '' then
    B := StringToFloat(fLV2.edB.Text);
  if fLV2.edC.Text <> '' then
    C := StringToFloat(fLV2.edC.Text);
  if fLV2.edD.Text <> '' then
    D := StringToFloat(fLV2.edD.Text);
  if fLV2.edE.Text <> '' then
    E := StringToFloat(fLV2.edE.Text);
  if fLV2.edF.Text <> '' then
    F := StringToFloat(fLV2.edF.Text);
  if fLV2.edG.Text <> '' then
    G := StringToFloat(fLV2.edG.Text);
  if fLV2.edEndTime.Text <> '' then
    EndTime := StrToInt(fLV2.edEndTime.Text);
  if N0 < 0 then begin
    Mess := 'Species 1 initial population must be greater than 0';
    fLV2.edN0.SetFocus;
  end
  else if P0 < 0 then begin
    Mess := 'Species 2 initial population must be greater than 0';
    fLV2.edP0.SetFocus;
  end
  else if Q0 < 0 then begin
    Mess := 'Species 3 initial population must be greater than 0';
    fLV2.edQ0.SetFocus;
  end
  else if (N0 = 0) and (P0 = 0) and (Q0 = 0) then begin
    Mess := 'At least one species population must be greater than 0';
    fLV2.edN0.SetFocus;
  end;
  if Mess = '' then begin
    if (N0 > 0) and (A = 0) then begin
      Mess := 'Species 1 natural growth rate must be greater than 0';
      fLV2.edA.SetFocus;
    end
    else if (N0 > 0) and (P0 > 0) and (B = 0) then begin
      Mess := 'Species 1 predation death rate must be greater than 0';
      fLV2.edB.SetFocus;
    end
    else if (P0 > 0) and (C = 0) then begin
      Mess := 'Species 2 natural death rate must be greater than 0';
      fLV2.edC.SetFocus;
    end
    else if (N0 > 0) and (P0 > 0) and (D = 0) then begin
      Mess := 'Species 2 predation growth rate must be greater than 0';
      fLV2.edD.SetFocus;
    end
    else if (P0 > 0) and (Q0 > 0) and (E = 0) then begin
      Mess := 'Species 2 predation death rate must be greater than 0';
      fLV2.edE.SetFocus;
    end
    else if (Q0 > 0) and (F = 0) then begin
      Mess := 'Species 3 natural death rate must be greater than 0';
      fLV2.edF.SetFocus;
    end
    else if (P0 > 0) and (Q0 > 0) and (G = 0) then begin
      Mess := 'Species 3 predation growth rate must be greater than 0';
      fLV2.edG.SetFocus;
    end
    else if EndTime < 5 then begin
      Mess := 'Time period must be greater than or equal to 5 years';
      fLV2.edEndTime.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if (N0 = 0) and (P0 = 0) then begin
      if not fLV2.mSettingsNoWarnings.Checked then
        MessageDlg('Single species model', 'Parameters A, B, C, D, E, G set to 0!', mtWarning, [mbOK], 0);
      Species1 := ''; Species2 := ''; A := 0; B := 0; C := 0; D := 0; E := 0; G := 0;
      fLV2.edName1.Text := ''; fLV2.edName2.Text := '';
      fLV2.edA.Text := ''; fLV2.edB.Text := ''; fLV2.edC.Text := '';
      fLV2.edD.Text := ''; fLV2.edE.Text := ''; fLV2.edG.Text := '';
    end
    else if (N0 = 0) and (Q0 = 0) then begin
      if not fLV2.mSettingsNoWarnings.Checked then
        MessageDlg('Single species model', 'Parameters A, B, D, E, F, G set to 0!', mtWarning, [mbOK], 0);
      Species1 := ''; Species3 := ''; A := 0; B := 0; D := 0; E := 0; F := 0; G := 0;
      fLV2.edName1.Text := ''; fLV2.edName3.Text := '';
      fLV2.edA.Text := ''; fLV2.edB.Text := ''; fLV2.edD.Text := '';
      fLV2.edE.Text := ''; fLV2.edF.Text := ''; fLV2.edG.Text := '';
    end
    else if (P0 = 0) and (Q0 = 0) then begin
      if not fLV2.mSettingsNoWarnings.Checked then
        MessageDlg('Single species model', 'Parameters B, C, D, E, F, G set to 0!', mtWarning, [mbOK], 0);
      Species2 := ''; Species3 := ''; B := 0; C := 0; D := 0; E := 0; F := 0; G := 0;
      fLV2.edName2.Text := ''; fLV2.edName3.Text := '';
      fLV2.edB.Text := ''; fLV2.edC.Text := ''; fLV2.edD.Text := '';
      fLV2.edE.Text := ''; fLV2.edF.Text := ''; fLV2.edG.Text := '';
    end
    else if N0 = 0 then begin
      if not fLV2.mSettingsNoWarnings.Checked then
        MessageDlg('Two species model', 'Parameters A, B, D set to 0!', mtWarning, [mbOK], 0);
      Species1 := ''; A := 0; B := 0; D := 0;
      fLV2.edName1.Text := '';
      fLV2.edA.Text := ''; fLV2.edB.Text := ''; fLV2.edD.Text := '';
    end
    else if P0 = 0 then begin
      if not fLV2.mSettingsNoWarnings.Checked then
        MessageDlg('Two species model', 'Parameters B, C, D, E, G set to 0!', mtWarning, [mbOK], 0);
      Species2 := ''; B := 0; C := 0; D := 0; E := 0; G := 0;
      fLV2.edName2.Text := '';
      fLV2.edB.Text := ''; fLV2.edC.Text := ''; fLV2.edD.Text := ''; fLV2.edE.Text := ''; fLV2.edG.Text := '';
    end
    else if Q0 = 0 then begin
      if not fLV2.mSettingsNoWarnings.Checked then
        MessageDlg('Two species model', 'Parameters E, F, G set to 0!', mtWarning, [mbOK], 0);
      Species3 := ''; E := 0; F := 0; G := 0;
      fLV2.edName3.Text := '';
      fLV2.edE.Text := ''; fLV2.edF.Text := ''; fLV2.edG.Text := '';
    end;
  end
  else
    MessageDlg('Invalid parameters', Mess, mtError, [mbOK], 0);
end;

{ Clean graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fLV2G.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fLV2G.imGraph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis and write labels }

procedure DrawAxis(XL, XR, YT, YB: Integer; LSpecies1, LSpecies2, LSpecies3: string);

var
  YAX, YLX, YLabel: Integer;

begin
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  fLV2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);        // draw X-axis
  fLV2G.imGraph.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);          // draw Y-axis
  fLV2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;                   // X-axis label
  fLV2G.imGraph.Picture.Bitmap.Canvas.TextOut(XR - 15, YLX, '[Years]');
  // Display Y-axis label(s)
  YLabel := 2;                                                                 // vert. pos. variable, allowing top-align (even if just 1 or 2 labels)
  if LSpecies1 <> '' then begin
    fLV2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlue;                  // color (blue) for species 1
    fLV2G.imGraph.Picture.Bitmap.Canvas.TextOut(6, YLabel, LSpecies1);
    YLabel += 15;
  end;
  if LSpecies2 <> '' then begin
    fLV2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clFuchsia;               // color (magenta) for species 2
    fLV2G.imGraph.Picture.Bitmap.Canvas.TextOut(6, YLabel, LSpecies2);
    YLabel += 15;
  end;
  if LSpecies3 <> '' then begin
    fLV2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clRed;                   // color (red) for species 3
    fLV2G.imGraph.Picture.Bitmap.Canvas.TextOut(6, YLabel, LSpecies3);
  end;
end;

{ 3-species extended Lotka–Volterra model (differential equations) }

procedure Lotka_Volterra(var N, P, Q: Real; A, B, C, D, E, F, G, dT: Real);

var
  dN, dP, dQ: Real;

begin
  dN := (A * N - B * N * P) * dT;
  dP := (-(C * P) + D * N * P - (E * P * Q)) * dT;
  dQ := (-(F * Q) + G * P * Q) * dT;
  N += dN;
  if N < 0 then                                                                // number of animals can't be < 0!
    N := 0;
  P += dP;
  if P < 0 then
    P := 0;
  Q += dQ;
  if Q < 0 then
    Q := 0;
end;

{ Compute T, N, P and Q values for 3-species extended Lotka–Volterra model }

procedure Lotka_Volterra_XY(N0, P0, Q0, A, B, C, D, E, F, G: Real; Values, Endtime: Integer;
  out SimulationTime, Species1, Species2, Species3: TSimulationValues);

var
  I, J: Integer;
  T, N, P, Q, dT: Real;

begin
  SetLength(SimulationTime, 1); SetLength(Species1, 1); SetLength(Species2, 1); SetLength(Species3, 1);
  // Initial values: for T = 0, N = N0, P = P0, Q = Q0
  SimulationTime[0] := 0; Species1[0] := N0; Species2[0] := P0; Species3[0] := Q0;
  N := N0; P := P0; Q := Q0;
  // Compute numerical approxiation of  differential equations using the EULER method
  dT := Endtime / Values;
  for I := 1 to Values do begin
    T := I * dT;
    // The smaller the time interval, the more accurate the numerical approxiation will be (and the longer the calculations will take)
    // The values chosen (1000 * 240 steps / year) suit well in most cases; for very long time periods, however, the oscillations
    // increase in amplitude as time progresses (use higher end value in "for" statement below to prevent this)
    for J := 1 to 1000 do
      Lotka_Volterra(N, P, Q, A, B, C, D, E, F, G, dT / 1000);                 // Lotka–Volterra model values for T = (T - 1) + dT
    SetLength(SimulationTime, Length(SimulationTime) + 1);
    SetLength(Species1, Length(Species1) + 1);
    SetLength(Species2, Length(Species2) + 1);
    SetLength(Species3, Length(Species3) + 1);
    SimulationTime[I] := T;                                                    // fill the time values into array
    Species1[I] := N; Species2[I] := P; Species3[I] := Q;                      // fill the 3 species population values into array
  end;
end;

{ Draw the 3 species population vs time graph (as calculated by the extended Lotka–Volterra model) }

procedure DrawGraph(Values, EndTime, XL, XR, YT, YB: Integer; var SimulationTime, Species1, Species2, Species3: TSimulationValues);

var
  X, Y, XDiv, YDiv, Mult, I: Integer;
  Max, Temp: Real;
  SN: string;

begin
  // Maximum population value (of all 3 species)
  Max := MaxPopulation(Species1);
  Temp := MaxPopulation(Species2);
  if Temp > Max then
    Max := Temp;
  Temp := MaxPopulation(Species3);
  if Temp > Max then
    Max := Temp;
  // Draw species population vs. time graphs
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 2;
  if Species1[0] <> 0 then begin
    // Draw species 1 population graph (in blue)
    fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlue;
    for I := 0 to Values do begin
      // X-value (time) on graph
      X := Round((SimulationTime[I] / EndTime) * (XR - XL)) + XL;
      // Y-value (lowest-level species) on graph
      Y := YB - Round((Species1[I] / Max) * (YB - YT));
      if I = 0 then
        fLV2G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)                       // first point of the curve: position the pen
      else
        fLV2G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);                      // other points: draw line from previous point to here
    end;
  end;
  if Species2[0] <> 0 then begin
    // Draw species 2 population graph (in magenta)
    fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clFuchsia;
    for I := 0 to Values do begin
      X := Round((SimulationTime[I] / EndTime) * (XR - XL)) + XL;
      Y := YB - Round((Species2[I] / Max) * (YB - YT));
      if I = 0 then
        fLV2G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)
      else
        fLV2G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);
    end;
  end;
  if Species3[0] <> 0 then begin
    // Draw species 2 population graph (in red)
    fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clRed;
    for I := 0 to Values do begin
      X := Round((SimulationTime[I] / EndTime) * (XR - XL)) + XL;
      Y := YB - Round((Species3[I] / Max) * (YB - YT));
      if I = 0 then
        fLV2G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)
      else
        fLV2G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);
    end;
  end;
  // Draw ticks and write values of X-axis
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  XDiv := (EndTime div 25) + 1;
  if XDiv = 3 then                                                             // use x-tick values of 1, 2, 5, 10 or 20 years (depending on simulation time)
    XDiv := 2
  else if XDiv = 4 then
    XDiv := 5
  else if (XDiv > 5) and (XDiv <= 10) then
    XDiv := 10
  else if XDiv > 10 then
    XDiv := 20;
  for I := 1 to EndTime - XDiv do begin
    if I mod XDiv = 0 then begin                                               // limiting the number of ticks!
      X := Round(I * (XR - XL) / EndTime) + XL;
      fLV2G.imGraph.Picture.Bitmap.Canvas.Line(X, YB - 5, X, YB + 5);          // draw the tick
      SN := IntToStr(I);
      if Length(SN) = 1 then                                                   // adapt position of time value, depending on it's 1 or 2 digits
        X -= 3
      else
        X -= 6;
      fLV2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
      fLV2G.imGraph.Picture.Bitmap.Canvas.TextOut(X, YB + 7, SN);              // write the time value
    end;
  end;
  // Draw ticks and write values of Y-axis
  if Max <= 10000 then begin
    // This avoids not only to much ticks for high animal numbers
    // but also a possible hanging of the program if Max is really big (why?)
    YDiv := 1; Mult := 10;
    while 20 * Mult * YDiv < Max do begin                                      // use "round" y-tick values
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
          fLV2G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);      // draw the tick
          fLV2G.imGraph.Picture.Bitmap.Canvas.TextOut(20, Y - 8, GFormat(I));  // write population size value
        end;
      end;
    end;
  end;
  // Reset pen width and pen and font color to default values
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fLV2G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clDefault;
  fLV2G.imGraph.Picture.Bitmap.Canvas.Font.Color := clDefault;
end;

{*******}
{ TfLV2 }
{*******}

{ Application start: Initialisation }

procedure TfLV2.FormCreate(Sender: TObject);

begin
  // Create .ini file
  SamplesFile := TINIFile.Create('samples.ini');
  // Set animal pics offsets from borders (pics have different sizes))
  iW1 := imgSpecies1.Width + 10;  iW2 := imgSpecies2.Width + 10;  iW3 := imgSpecies3.Width + 10;
  iH1 := imgSpecies1.Height + 10; iH2 := imgSpecies2.Height + 10; iH3 := imgSpecies3.Height + 10;
  // Default time interval: 1 sec real time for 1 year simulation time
  iInterval := 1000;
  // Load parameters for sample 7
  ReadSampleData(7, sSpecies1, sSpecies2, sSpecies3, rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, iEndTime);
end;

{ Menu item "File > New": New simulation (clear simulation form) }

procedure TfLV2.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  // Dusable timer
  tiLV.Enabled := False;
  // Clear simulation pane edit fields
  edSimT.Text := ''; edSimN.Text := ''; edSimP.Text := ''; edSimQ.Text := '';
  edSimND.Text := ''; edSimND.Color := clDefault;
  edSimPD.Text := ''; edSimPD.Color := clDefault;
  edSimQD.Text := ''; edSimQD.Color := clDefault;
  // Remove all animals
  if Length(imSpecies1) > 0 then begin
    for I := 0 to iMaxSpecies1 - 1 do
      imSpecies1[I].Visible := False;                                          // remove all species 1 by hiding them
  end;
  if Length(imSpecies2) > 0 then begin
    for I := 0 to iMaxSpecies2 - 1 do
      imSpecies2[I].Visible := False;                                          // remove all species 2 by hiding them
  end;
  if Length(imSpecies3) > 0 then begin
    for I := 0 to iMaxSpecies3 - 1 do
      imSpecies3[I].Visible := False;                                          // remove all species 3 by hiding them
  end;
  // Set button capture for new simulation
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Enabled := False;
end;

{ Menu item "File > Samples": Load sample data }

procedure TfLV2.mFileSampleClick(Sender: TObject);

var
  Sample: Integer;

begin
  // Show-up the sample selection window
  fLV2S.ShowModal;
  // If user pushed "Select" button, load corr. data from .ini file
  if fLV2S.sButton = 'select' then begin
    Sample := fLV2S.iSample;
    mFileNew.Click;                                                            // clear simulation form
    ReadSampleData(Sample, sSpecies1, sSpecies2, sSpecies3, rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, iEndTime);                                                                  // read sample data from file
  end;
end;

{ Menu item "File > Exit": Exit application }

procedure TfLV2.mFileExitClick(Sender: TObject);

begin
  SamplesFile.Free;
  Close;
end;

{ Menu item "Settings > Simulation time interval": Ask user for simulation time interval }

procedure TfLV2.mSettingsTimeClick(Sender: TObject);

var
  S: string;

// Simulation time interval = timer interval = how many secs between 2 display updates is used for 1 year population growth time
// This does not change the number of display updates; it only allows to slow down or speed the simulation

begin
  S := InputBox('Simulation time interval', 'Simulation time in sec for 1 year real time', FloatToStr(iInterval / 1000));
  if S <> '' then
    iInterval := Round(StrToFloat(S) * 1000);                                  // must be in msec
end;

{ Menu item "Settings > Use alternate pictures": Toggle which animal picture set to use }

procedure TfLV2.mSettingsPicsClick(Sender: TObject);

begin
  // First set: mouse, snake, owl
  if mSettingsPics.Checked then begin
    mSettingsPics.Checked := False;

    iW1 := imgSpecies1.Width + 10;  iW2 := imgSpecies2.Width + 10;  iW3 := imgSpecies3.Width + 10;
    iH1 := imgSpecies1.Height + 10; iH2 := imgSpecies2.Height + 10; iH3 := imgSpecies3.Height + 10;
    if edName1.Text = 'worm' then
      edName1.Text := 'mouse';
    if edName1.Text = 'robin' then
      edName1.Text := 'snake';
    if edName1.Text = 'falcon' then
      edName1.Text := 'owl';
  end
  // Second set: worm, robin, falcon
  else begin
    mSettingsPics.Checked := True;
    iW1 := imgSpecies1b.Width + 10;  iW2 := imgSpecies2b.Width + 10;  iW3 := imgSpecies3b.Width + 10;
    iH1 := imgSpecies1b.Height + 10; iH2 := imgSpecies2b.Height + 10; iH3 := imgSpecies3b.Height + 10;
    if edName1.Text = 'mouse' then
      edName1.Text := 'worm';
    if edName1.Text = 'snake' then
      edName1.Text := 'robin';
    if edName1.Text = 'owl' then
      edName1.Text := 'falcon';
  end;
end;

{ Menu item "Settings > Do not display warnings": Toggle to display or not warning messages }

procedure TfLV2.mSettingsWarningsClick(Sender: TObject);

begin
  if mSettingsNoWarnings.Checked then
    mSettingsNoWarnings.Checked := False
  else
    mSettingsNoWarnings.Checked := True;
end;

{ Menu item "Help > The (2 species) Lotka-Volterra model": Display help text concerning the original Lotka-Volterra model }

procedure TfLV2.mHelpLVClick(Sender: TObject);

begin
  fLV2H.memoHelp.Lines.LoadFromFile('lv2.txt');
  fLV2H.memoHelp.Text := StringReplace(fLV2H.memoHelp.Text, 'u0', 'u' + SUB_0, []);
  fLV2H.memoHelp.Text := StringReplace(fLV2H.memoHelp.Text, 'v0', 'v' + SUB_0, []);
  fLV2H.Show;
end;

{ Menu item "Help > 3 species Lotka-Volterra extension": Display help text concerning the extension of the Lotka-Volterra model to 3 species }

procedure TfLV2.mHelpLV3Click(Sender: TObject);

begin
  fLV2H.memoHelp.Lines.LoadFromFile('lv3.txt');
  fLV2H.memoHelp.Text := StringReplace(fLV2H.memoHelp.Text, 'x0', 'x' + SUB_0, []);
  fLV2H.memoHelp.Text := StringReplace(fLV2H.memoHelp.Text, 'y0', 'y' + SUB_0, []);
  fLV2H.memoHelp.Text := StringReplace(fLV2H.memoHelp.Text, 'z0', 'z' + SUB_0, []);
  fLV2H.Show;
end;

{ Menu item "Help > Application help": Display "LotkaVolterra2" application help text }

procedure TfLV2.mHelpHelpClick(Sender: TObject);

begin
  fLV2H.memoHelp.Lines.LoadFromFile('help.txt');
  fLV2H.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfLV2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Biology simulation:' + LineEnding;
  S := '3-species predator-prey model (Lotka–Volterra extension).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March-July 2020.';
  MessageDlg('About "LotkaVolterra2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Stop": Start resp. stop the simulation }

procedure TfLV2.btStartClick(Sender: TObject);

var
  I: Integer;
  Mess: string;

begin
  // Button "Start": Start simulation with actual parameters read from form
  if btStart.Caption = 'Start' then begin
    ReadUserData(sSpecies1, sSpecies2, sSpecies3, rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, iEndTime, Mess);
    if Mess = '' then begin
      // Proceed only if there was no error message
      if ((rN0 <> 0) and (rN0 < 1)) or ((rP0 <> 0) and (rP0 < 1)) or ((rQ0 <> 0) and (rQ0 < 1)) then
        // Don't do simulation, if initial population sizes < 1 (i.e. animals must be individuals)
        Mess := 'Initial number of all species must be >= 1 for proper graphical simulation'
      else begin
        // All ok: Do the simulation
        iValues := iEndTime * 240;                                             // 240 values per year (each 1000 steps for Euler approximation)
        iOldSpecies1 := 0; iOldSpecies2 := 0; iOldSpecies3 := 0;
        // Calculate simulation values using the extended Lotka-Volterra model
        Lotka_Volterra_XY(rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, iValues, iEndTime, aSimulationTime, aSpecies1, aSpecies2, aSpecies3);
        // Determine maxima of the 3 species populations
        rMaxSpecies1 := 0; rMaxSpecies2 := 0; rMaxSpecies3 := 0;
        if rN0 > 0 then
          rMaxSpecies1 := MaxPopulation(aSpecies1);
        if rP0 > 0 then
          rMaxSpecies2 := MaxPopulation(aSpecies2);
        if rQ0 > 0 then
          rMaxSpecies3 := MaxPopulation(aSpecies3);
        // Determine species maximum pics
        if Round(rMaxSpecies1) + Round(rMaxSpecies2) + Round(rMaxSpecies3) <= MaxAllAnimals - 3 then begin
          // If total number of animals less than max. number of pics cretaed, 1 pic = 1 animal
          iMaxSpecies1 := Round(rMaxSpecies1);
          iMaxSpecies2 := Round(rMaxSpecies2);
          iMaxSpecies3 := Round(rMaxSpecies3);
        end
        else begin
          // Otherwise adapt max. number of animals (1 pic will be several animals)
          iMaxSpecies1 := Round(rMaxSpecies1 * (MaxAllAnimals / (rMaxSpecies1 + rMaxSpecies2 + rMaxSpecies3)));
          iMaxSpecies2 := Round(rMaxSpecies2 * (MaxAllAnimals / (rMaxSpecies1 + rMaxSpecies2 + rMaxSpecies3)));
          iMaxSpecies3 := Round(rMaxSpecies3 * (MaxAllAnimals / (rMaxSpecies1 + rMaxSpecies2 + rMaxSpecies3)));
        end;
        // Do the simulation only if at least 1 image of every species will be displayed for initial population values
        // (that will not be the case if the animal populations grow to big)
        if rN0 > 0 then begin
          if Round((rN0 / rMaxSpecies1) * iMaxSpecies1) < 1 then
            Mess := 'Error';
        end;
        if rP0 > 0 then begin
          if Round((rP0 / rMaxSpecies2) * iMaxSpecies2) < 1 then
            Mess := 'Error';
        end;
        if rQ0 > 0 then begin
          if Round((rQ0 / rMaxSpecies3) * iMaxSpecies3) < 1 then
            Mess := 'Error';
        end;
        if Mess = 'Error' then
          Mess := 'To many animals for proper graphical simulation'
        else begin
          // Create arrays with animal images (as much as needed, with a total of 750)
          if Length(imSpecies1) > 0 then begin                                 // destroy all species 1 images
            for I := 0 to Length(imSpecies1) - 1 do
              imSpecies1[I].Destroy;
            SetLength(imSpecies1, 0);
          end;
          if Length(imSpecies2) > 0 then begin                                 // destroy all species 2 images
            for I := 0 to Length(imSpecies2) - 1 do
              imSpecies2[I].Destroy;
            SetLength(imSpecies2, 0);
          end;
          if Length(imSpecies3) > 0 then begin                                 // destroy all species 3 images
            for I := 0 to Length(imSpecies3) - 1 do
              imSpecies3[I].Destroy;
            SetLength(imSpecies3, 0);
          end;
          // Create species 1 images as needed
          for I := 0 to iMaxSpecies1 - 1 do begin
            SetLength(imSpecies1, I + 1);
            imSpecies1[I] := TImage.Create(fLV2.imSpecies1[I]);
            imSpecies1[I].Parent  := Self;
            // Values depending on animal set selected
            if mSettingsPics.Checked then begin
              imSpecies1[I].Width   := imgSpecies1b.Width;
              imSpecies1[I].Height  := imgSpecies1b.Height;
              imSpecies1[I].Picture := imgSpecies1b.Picture;
            end
            else begin
              imSpecies1[I].Width   := imgSpecies1.Width;
              imSpecies1[I].Height  := imgSpecies1.Height;
              imSpecies1[I].Picture := imgSpecies1.Picture;
            end;
            imSpecies1[I].Visible := False;
          end;
          // Create species 2 images as needed
          for I := 0 to iMaxSpecies2 - 1 do begin
            SetLength(imSpecies2, I + 1);
            imSpecies2[I] := TImage.Create(fLV2.imSpecies2[I]);
            imSpecies2[I].Parent  := Self;
            if mSettingsPics.Checked then begin
              imSpecies2[I].Width   := imgSpecies2b.Width;
              imSpecies2[I].Height  := imgSpecies2b.Height;
              imSpecies2[I].Picture := imgSpecies2b.Picture;
            end
            else begin
              imSpecies2[I].Width   := imgSpecies2.Width;
              imSpecies2[I].Height  := imgSpecies2.Height;
              imSpecies2[I].Picture := imgSpecies2.Picture;
            end;
            imSpecies2[I].Visible := False;
          end;
          // Create species 3 images as needed
          for I := 0 to iMaxSpecies3 - 1 do begin
            SetLength(imSpecies3, I + 1);
            imSpecies3[I] := TImage.Create(fLV2.imSpecies3[I]);
            imSpecies3[I].Parent  := Self;
            if mSettingsPics.Checked then begin
              imSpecies3[I].Width   := imgSpecies3b.Width;
              imSpecies3[I].Height  := imgSpecies3b.Height;
              imSpecies3[I].Picture := imgSpecies3b.Picture;
            end
            else begin
              imSpecies3[I].Width   := imgSpecies3.Width;
              imSpecies3[I].Height  := imgSpecies3.Height;
              imSpecies3[I].Picture := imgSpecies3.Picture;
            end;
            imSpecies3[I].Visible := False;
          end;
          // Start the timer (with simulation time = 0)
          iValue := 0; iDisplayedSpecies1 := 0; iDisplayedSpecies2 := 0; iDisplayedSpecies3 := 0;
          tiLV.Enabled := True;
          // Next push on button will be "Stop"
          btStart.Caption := 'Stop';
          // Enable "Pause" button
          btPause.Caption := 'Pause'; btPause.Enabled := True;
        end;
      end;
      // If there was an error, display message
      if Mess <> '' then
        MessageDlg('Graphics problem', Mess + '!', mtError, [mbOK], 0);
    end
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

procedure TfLV2.btPauseClick(Sender: TObject);

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

{ Button "Graph": Draw the graph with parameters read from form (display will be done onto form fLV2G) }

procedure TfLV2.btGraphClick(Sender: TObject);

var
  Mess, S: string;

begin
  ReadUserData(sSpecies1, sSpecies2, sSpecies3, rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, iEndTime, Mess);
  if Mess = '' then begin
    // Proceed if there was no error
    iValues := iEndTime * 240;                                                 // 240 values per year (each 1000 steps for Euler approximation)
    // Write title and Lotka-Volterra model values
    fLV2G.stTitle.Caption := '3-species predator-prey model';
    if (rP0 = 0) and (rQ0 = 0) then
      fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + ' (lowest-level species only)'
    else if (rN0 = 0) and (rQ0 = 0) then
      fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + ' (mid-level species only)'
    else if (rN0 = 0) and (rP0 = 0) then
      fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + ' (top-level species only)'
    else if rN0 = 0 then
      fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + ' (no lowest-level species)'
    else if rP0 = 0 then
      fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + ' (no mid-level species)'
    else if rQ0 = 0 then
      fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + ' (no top-level species)';
    fLV2G.stTitle.Caption := fLV2G.stTitle.Caption + '.';
    S := 'Lotka-Volterra values:  ';
    S += 'a=' + FloatToStr(rA) + '  b=' + FloatToStr(rB) + '  c=' + FloatToStr(rC) + '  d=' + FloatToStr(rD);
    S += '  e=' + FloatToStr(rE) + '  f=' + FloatToStr(rF) + '  g=' + FloatToStr(rG);
    fLV2G.stLV.Caption := S;
    // Calculate 3-species extended Lotka-Volterra model values and create the graph
    Lotka_Volterra_XY(rN0, rP0, rQ0, rA, rB, rC, rD, rE, rF, rG, iValues, iEndTime, aSimulationTime, aSpecies1, aSpecies2, aSpecies3);
    GraphClean(fLV2G.iImageWidth, fLV2G.iImageHeight);
    DrawAxis(fLV2G.iGraphLeft, fLV2G.iGraphRight, fLV2G.iGraphTop, fLV2G.iGraphBottom, sSpecies1, sSpecies2, sSpecies3);
    DrawGraph(iValues, iEndTime, fLV2G.iGraphLeft, fLV2G.iGraphRight, fLV2G.iGraphTop, fLV2G.iGraphBottom, aSimulationTime, aSpecies1, aSpecies2, aSpecies3);
    // Show the graph window
    fLV2G.Show;
  end;
end;

{ Simulation timer routine }

procedure TfLV2.tiLVTimer(Sender: TObject);

var
  N, P, Q, NSpecies1, NSpecies2, NSpecies3, I: Integer;
  DSpecies1, DSpecies2, DSpecies3: Integer;

begin
  // If not yet all T / N, Q, P values have been processed
  if iValue <= iValues then begin
    // Display actual species populations
    N := Round(aSpecies1[iValue]);
    if (N = 0) and (aSpecies1[iValue] > 0.05) then                             // if species left (0.05 < N(t) < 1) set N to 1 (instead of 0)
      N := 1;
    if rMaxSpecies1 > 0 then
      edSimN.Text := IntToStr(N)
    else
      edSimN.Text := '-----';
    P := Round(aSpecies2[iValue]);
    if (P = 0) and (aSpecies2[iValue] > 0.05) then
      P := 1;
    if rMaxSpecies2 > 0 then
      edSimP.Text := IntToStr(P)
    else
      edSimP.Text := '-----';
    Q := Round(aSpecies3[iValue]);
    if (Q = 0) and (aSpecies3[iValue] > 0.05) then
      Q := 1;
    if rMaxSpecies3 > 0 then
      edSimQ.Text := IntToStr(Q)
    else
      edSimQ.Text := '-----';
    // Simulation start (time = 0)
    if iValue = 0 then begin
      // Set the correct timer interval
      tiLV.Interval := iInterval;
      // Initial number of species
      iOldSpecies1 := Round(aSpecies1[0]); iOldSpecies2 := Round(aSpecies2[0]); iOldSpecies3 := Round(aSpecies3[0]);
    end
    // Simulation running (t > 0)
    else begin
      // Simulation time elapsed (in years)
      edSimT.Text := FloatToStrF(iValue / (iValues div iEndTime), ffFixed, 0, 0);
      // Set ΔN, ΔP and ΔQ to 0 at display for last value
      if iValue = iValues then begin
        DSpecies1 := 0; DSpecies2 := 0; DSpecies3 := 0;
      end
      // Calculate species differences (compared to previous simulation step)
      else begin
        DSpecies1 := N - iOldSpecies1;
        DSpecies2 := P - iOldSpecies2;
        DSpecies3 := Q - iOldSpecies3;
      end;
      // Display species differences
      // Use "lime" to indicate that animal number increses, "red" if it decreases
      // To avoid "color flickering", let color as is if animal number does not change
      if edSimN.Text = '-----' then begin
        edSimND.Color := clDefault; edSimND.Text := '-----';
      end
      else begin
        if DSpecies1 = 0 then begin
          if (iValue = iValues) or (N = 0) then begin
            // Clear difference fields at end of simulation and if population = 0
            edSimND.Text := ''; edSimND.Color := clDefault;
          end
          else
            edSimND.Text := '±0';
        end
        else begin
          edSimND.Text := Sign(DSpecies1) + IntToStr(Abs(DSpecies1));
          if DSpecies1 > 0 then
            edSimND.Color := clLime
          else if DSpecies1 < 0 then
            edSimND.Color := clRed;
        end;
      end;
      if edSimP.Text = '-----' then begin
        edSimPD.Color := clDefault; edSimPD.Text := '-----';
      end
      else begin
        if DSpecies2 = 0 then begin
          if (iValue = iValues) or (P = 0) then begin
            edSimPD.Text := ''; edSimPD.Color := clDefault;
          end
          else
            edSimPD.Text := '±0';
        end
        else begin
          edSimPD.Text := Sign(DSpecies2) + IntToStr(Abs(DSpecies2));
          if DSpecies2 > 0 then
            edSimPD.Color := clLime
          else if DSpecies2 < 0 then
            edSimPD.Color := clRed;
        end;
      end;
      if edSimQ.Text = '-----' then begin
        edSimQD.Color := clDefault; edSimQD.Text := '-----';
      end
      else begin
        if DSpecies3 = 0 then begin
          if (iValue = iValues) or (Q = 0) then begin
            edSimQD.Text := ''; edSimQD.Color := clDefault;
          end
          else
            edSimQD.Text := '±0';
        end
        else begin
          edSimQD.Text := Sign(DSpecies3) + IntToStr(Abs(DSpecies3));
          if DSpecies3 > 0 then
            edSimQD.Color := clLime
          else if DSpecies3 < 0 then
            edSimQD.Color := clRed;
        end;
      end;
    end;
    // Calculate how many species images have to be displayed
    NSpecies1 := 0; NSpecies2 := 0; NSpecies3 := 0;
    if rMaxSpecies1 > 0 then begin
      NSpecies1 := Round((N / rMaxSpecies1) * iMaxSpecies1);
      if (NSpecies1 = 0) and (aSpecies1[iValue] > 0.05) then                   // if species left (0.05 < N(t) < 1) set N to 1 (instead of 0)
        NSpecies1 := 1;
    end;
    if rMaxSpecies2 > 0 then begin
      NSpecies2 := Round((P / rMaxSpecies2) * iMaxSpecies2);
      if (NSpecies2 = 0) and (aSpecies2[iValue] > 0.05) then
        NSpecies2 := 1;
    end;
    if rMaxSpecies3 > 0 then begin
      NSpecies3 := Round((Q / rMaxSpecies3) * iMaxSpecies3);
      if (NSpecies3 = 0) and (aSpecies3[iValue] > 0.05) then
        NSpecies3 := 1;
    end;
    // If there are more species 1 than before, display further images (setting their "Visible" property to 'visible')
    if NSpecies1 > iDisplayedSpecies1 then begin
      for I := iDisplayedSpecies1 + 1 to NSpecies1 do begin
        // Species 1 images displayed at random positions
        imSpecies1[I - 1].Left := Random(shSimulation.Width - (iW1 + 4)) + 20;
        imSpecies1[I - 1].Top := Random(shSimulation.Height - (iH1 + 4)) + 70;
        imSpecies1[I - 1].Visible := True;
      end;
    end
    // If there are less species 1 than before, remove images (setting their "Visible" property to not 'visible')
    else if NSpecies1 < iDisplayedSpecies1 then begin
      for I := iDisplayedSpecies1 downto NSpecies1  + 1 do begin
        imSpecies1[I - 1].Visible := False;
      end
    end;
    // If there are more species 2 than before, display further images (setting their "Visible" property to 'visible')
    if NSpecies2 > iDisplayedSpecies2 then begin
      for I := iDisplayedSpecies2 + 1 to NSpecies2 do begin
        // Species 2 images displayed at random positions
        imSpecies2[I - 1].Left := Random(shSimulation.Width - (iW2 + 4)) + 20;
        imSpecies2[I - 1].Top := Random(shSimulation.Height - (iH2 + 4)) + 70;
        imSpecies2[I - 1].Visible := True;
      end;
    end
    // If there are less species 2 than before, remove images (setting their "Visible" property to not 'visible')
    else if NSpecies2 < iDisplayedSpecies2 then begin
      for I := iDisplayedSpecies2 downto NSpecies2  + 1 do begin
        imSpecies2[I - 1].Visible := False;
      end
    end;
    // If there are more species 3 than before, display further images (setting their "Visible" property to 'visible')
    if NSpecies3 > iDisplayedSpecies3 then begin
      for I := iDisplayedSpecies3 + 1 to NSpecies3 do begin
        // Species 3 images displayed at random positions
        imSpecies3[I - 1].Left := Random(shSimulation.Width - (iW3 + 4)) + 20;
        imSpecies3[I - 1].Top := Random(shSimulation.Height - (iH3 + 4)) + 70;
        imSpecies3[I - 1].Visible := True;
      end;
    end
    // If there are less species 3 than before, remove images (setting their "Visible" property to not 'visible')
    else if NSpecies3 < iDisplayedSpecies3 then begin
      for I := iDisplayedSpecies3 downto NSpecies3  + 1 do begin
        imSpecies3[I - 1].Visible := False;
      end
    end;
    // Save actual number of species and number of animals actually displayed
    iOldSpecies1 := N; iOldSpecies2 := P; iOldSpecies3 := Q;
    iDisplayedSpecies1 := NSpecies1; iDisplayedSpecies2 := NSpecies2; iDisplayedSpecies3 := NSpecies3;
    // Next time value (simulation step = 1 year population dynamics)
    iValue += iValues div iEndTime;
  end
  // If all T / N, P, Q values have been processed
  else begin
    // Stop timer
    tiLV.Enabled := False;
    // Disable buttons (user has to choose "File > New" to clear the form before starting new simulation)
    btStart.Caption := 'Start'; btStart.Enabled := False;
    btPause.Caption := 'Pause'; btPause.Enabled := False;
  end;
end;

end.

