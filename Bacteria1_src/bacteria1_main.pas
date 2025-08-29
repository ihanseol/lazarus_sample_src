{***************************************}
{* Main unit for Bacteria1 application *}
{***************************************}

unit bacteria1_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, PopupNotifier, bacteria1_graph, bacteria1_help;

const
  MaxBacteria = 600;
  MaxNutrient = 1150;

type
  { TfBacteria1 }
  TfBacteria1 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileReset, mFileExit: TMenuItem;
    mSettings, mSettingsMaintenance0, mSettingsTime, mSettingsTimeAuto, mSettingsTimeCustom: TMenuItem;
    mHelp, mHelpBacteria, mHelpHelp, mHelpAbout: TMenuItem;
    imBacteria: TImage;
    shCulture, shNutrient0, shNutrient: TShape;
    Label0, Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    Label9, Label10, Label11, Label12, Label13, Label14, Label15, Label16: TLabel;
    laBacteriaUnit, laNutrientUnit, laGrowthRateUnit, laHalfsatUnit, laMaintenanceUnit: TLabel;
    edBacteriaName, edBacteria, edNutrientName, edNutrient: TEdit;
    edGrowthRate, edHalfSaturation, edYieldCoeff, edMaintenance: TEdit;
    edSimTime, edSimBacteria, edSimBacteriaDiff, edSimNutrient: TEdit;
    btStart: TButton;
    btPause: TButton;
    btSamples: TButton;
    btGraph: TButton;
    tiBacteria: TTimer;
    dlgOpen: TOpenDialog;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileResetClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpBacteriaClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mSettingsMaintenance0Click(Sender: TObject);
    procedure mSettingsTimeAutoClick(Sender: TObject);
    procedure mSettingsTimeCustomClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure btSamplesClick(Sender: TObject);
    procedure btGraphClick(Sender: TObject);
    procedure tiBacteriaTimer(Sender: TObject);
  private
    iMaxBacteria, iEndTime, iValues, iInterval, iValue, iBacteriaDisplayed: Integer;
    rN0, rS0, rR, rA, rY, rM, rMaxBacteria: Double;
    sBacteria, sNutrient: string;
    aiBacteria: array[1..MaxBacteria] of TImage;
  end;

var
  fBacteria1: TfBacteria1;
  GrowthTime, Bacteria, Nutrient: array of Double;

implementation

{$R *.lfm}

{ Read sample data from file }

procedure SampleData(Filename: string; out Bacteria, Nutrient: string; out R, A: Double);

var
  P, Code: Integer;
  Line, S: string;
  SDFile: Text;

begin
  Assign(SDFile, Filename); Reset(SDFile);
  while not EoF(SDFile) do begin
    Readln(SDFile, Line);
    if LeftStr(Line, 8) = 'Bacteria' then                                      // bacteria name
      Bacteria := Copy(Line, 10, Length(Line))
    else if LeftStr(Line, 8) = 'Nutrient' then                                 // nutrient name
      Nutrient := Copy(Line, 10, Length(Line))
    else if LeftStr(Line, 2) = 'R=' then begin                                 // maximal growth rate (per hour)
      Line := Copy(Line, 3, Length(Line));
      Line := StringReplace(Line, ',', '.', []);
      Val(Line, R, Code);
    end
    else if LeftStr(Line, 2) = 'A=' then begin                                 // half-saturation constant
      Line := Copy(Line, 3, Length(Line));
      P := Pos('(', Line);
      if P = 0 then
        S := Line
      else
        S := LeftStr(Line, P - 2);
      S := StringReplace(S, ',', '.', []);
      Val(S, A, Code);
      if Copy(Line, P + 1, Length(Line) - P - 1) = 'mg/l'                      // value followed by ' (mg/l)' transformed to g/l value
        then A /= 1000;
    end
  end;
  Close(SDFile);
end;

{ Read sample data and fill-in its values into the main form}

procedure GetSampleData(Filename: string; out Bacteria, Nutrient: string; out R, A: Double);

begin
  SampleData(Filename, Bacteria, Nutrient, R, A);                              // read sample data from this file
  fBacteria1.edBacteriaName.Text := Bacteria;
  fBacteria1.edNutrientName.Text := Nutrient;
  fBacteria1.edGrowthRate.Text := FloatToStr(R);
  fBacteria1.edHalfSaturation.Text := FloatToStr(A);;
end;

{ Get user data for this simulation from form }

procedure ReadData(out Bacteria, Nutrient: string; out N0, S0, R, A, Y, M: Double; out Mess: string);

// There is no 'real' validity check of the values that the user enters; thus up to her not to choose 'senseless' data
// If a field is left blank, it is either set to 0, either filled with a default value (names, growth yield coeff. and maintenance energy)

begin
  Mess := '';
  if fBacteria1.edBacteriaName.Text = '' then
    Bacteria := 'Some bacteria'
  else
    Bacteria := fBacteria1.edBacteriaName.Text;
  if fBacteria1.edNutrientName.Text = '' then
    Nutrient := 'some nutrient'
  else
    Nutrient := fBacteria1.edNutrientName.Text;
  if fBacteria1.edBacteria.Text = '' then
    N0 := 0
  else
    N0 := StrToFloat(fBacteria1.edBacteria.Text);
  if fBacteria1.edNutrient.Text = '' then
    S0 := 0
  else
    S0 := StrToFloat(fBacteria1.edNutrient.Text);
  if fBacteria1.edGrowthRate.Text = '' then
    R := 0
  else
    R := StrToFloat(fBacteria1.edGrowthRate.Text);
  if fBacteria1.edHalfSaturation.Text = '' then
    A := 0
  else
    A := StrToFloat(fBacteria1.edHalfSaturation.Text);
  if fBacteria1.edYieldCoeff.Text = '' then
    Y := 0.23                                                                  // growth yield coefficient default value
  else
    Y := StrToFloat(fBacteria1.edYieldCoeff.Text);
  if fBacteria1.edMaintenance.Text = '' then begin
    if fBacteria1.mSettingsMaintenance0.Checked then
      M := 0
    else
      M := 0.25;                                                               // maintenance energy default value
  end
  else
    M := StrToFloat(fBacteria1.edMaintenance.Text);
  if N0 <= 0 then
    Mess := 'Initial bacteria biomass must be greater than 0'
  else if S0 <= 0 then
    Mess := 'Initial nutrient concentration must be greater than 0'
  else if R <= 0 then
    Mess := 'Bacteria growth rate must be greater than 0'
  else if A <= 0 then
    Mess := 'Half-saturation constant must be greater than 0'
  else if Y <= 0 then
    Mess := 'Growth yield coefficient must be greater than 0'
  else if fBacteria1.mSettingsMaintenance0.Checked and (M < 0) then
    Mess := 'Maintenance energy must be greater than or equal to 0'
  else if not fBacteria1.mSettingsMaintenance0.Checked and (M <= 0) then
    Mess := 'Maintenance energy must be greater than 0';
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Clean the graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis and write labels }

procedure DrawAxis(XL, XR, YT, YB: Integer; LegendX, Legend1Y, Legend2Y: string);

var
  YAX, YLX: Integer;

begin
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);  // draw X-axis
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);    // draw Y-axis
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(XR - 15, YLX, LegendX);    // display X-axis legend
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clRed;               // color (red) for bacteria
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(6, 10, Legend1Y);          // display Y-axis legend for bacteria
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlue;              // color (blue) for nutrient
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(6, 30, Legend2Y);          // display Y-axis legend for nutrient
end;

{ Extended Monod growth model }

procedure Monod(var N, S: Double; R, A, Y, M, dT: Double);

var
  dN, dS: Double;

begin
  dN := ((N * ((R * S) / (A + S))) - M * N) * dT;
  dS := (((-1 / Y) * N) * ((R * S) / (A + S))) * dT;
  N += dN;
  if N < 0 then
    N := 0;
  S += dS;
  if S < 0 then
    S := 0;
end;

{ Compute T, N and S values for bacteria growth using the extended Monod growth model }

procedure Monod_XY(N0, S0, R, A, Y, M: Double; Values, Endtime: Integer; RealConc: Boolean);

var
  I: Integer;
  T, N, S, dT: Double;

begin
  SetLength(GrowthTime, 1); SetLength(Bacteria, 1); SetLength(Nutrient, 1);
  // Initial values: for T = 0, N = N0, S = S0
  GrowthTime[0] := 0; Bacteria[0] := N0; Nutrient[0] := S0;
  N := N0; S := S0;
  // Compute numerical approxiation of  differential equations using the Euler method
  dT := Endtime / Values;
  for I := 1 to Values do begin
    T := I * Endtime / Values;
    Monod(N, S, R, A, Y, M, dT);                                               // extended Monod growth model values for T = (T - 1) + dT
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

{ Calculate maximum bacteria concentration; estimate time unitil N = 0 or maximum; choose number of T / N and S values }

procedure Monod_Extrema(N0, S0, R, A, Y, M: Double; RealConc: Boolean; out Values, EndTime: Integer; out NMax: Double);

var
  N, S, T, dT: Double;
  Break: Boolean;

begin
  N := N0; S := S0;
  dT := 1 / 3600;
  NMax := N0; T := 0;
  Break := False;
  // Repeat adding dT to T and calculating corresponding N value unitl N = 0 (simulation with maintenance)
  // or N = Max = N0 * Y * S0 (simulation without maintenance); save the time (in h) that this takes
  repeat
    T += dt;
    Monod(N, S, R, A, Y, M, dT);
    if N > NMax then
      NMax := N;
    if (M = 0) and ((N0 + Y * S0) - N < 1E-5) then
      Break := True
    else if (M <> 0) and (N < 1E-5) then
      Break := True;
  until Break;
  // Culture time on graph: some hours more than value calculated (is nicer on graph)
  EndTime := Trunc(T + 1) + 2;
  // Use maximum bacteria concentration calculated by formula, if maintenance is ignored; some more hours on graph X-axis
  if M = 0 then begin
    NMax := N0 + Y * S0;
    EndTime += 3;
  end;
  // Adapt maximum bacteria concentration to other N values
  if not RealConc then
    NMax /= (A * Y);
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

{ Draw the bacteria/nutrient concentration vs time graph (as calculated by the extended Monod model) }

procedure DrawGraph(Values, EndTime: Integer; NMax: Double; XL, XR, YT, YB: Integer);

var
  X, Y, N0Y, NMaxY, S0Y, IDiv, I: Integer;
  Max: Double;
  SN: string;

begin
  // Maximum concentration (may be S0 or Nmax)
  Max := Nutrient[0];
  if NMax > Nutrient[0] then
    Max := NMax;
  NMaxY := -1;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 2;
  // Draw bacteria concentration graph (in red)
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clRed;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Bacteria[I] / Max) * (YB - YT));
    if I = 0 then                                                              // first point of the curve:
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)                   // position the pen
    else                                                                       // other points of the curve:
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);                  // draw line from previous point to here
    if I = 0 then                                                              // save Y-postion of initial bacteria concentration
      N0Y := Y
    else if NMax - Bacteria[I] < 1E-5 then                                     // save Y-postion of maximum bacteria concentration
      NMaxY := Y;
  end;
  // Draw nutrient concentration graph (in blue)
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  for I := 0 to Values do begin
    // X-value (time) on graph
    X := Round((GrowthTime[I] / EndTime) * (XR - XL)) + XL;
    // Y-value (bacteria) on graph
    Y := YB - Round((Nutrient[I] / Max) * (YB - YT));
    if I = 0 then
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.MoveTo(X, Y)
    else
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.LineTo(X, Y);
    if I = 0 then                                                              // save Y-postion of initial nutrient concentration
      S0Y := Y;
  end;
  // Draw ticks and write values of X-axis
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  IDiv := (EndTime div 25) + 1;
  for I := 1 to EndTime - IDiv do begin
    if I mod IDiv = 0 then begin                                               // limiting the number of ticks to 25
      X := Round(I * (XR - XL) / EndTime) + XL;
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.Line(X, YB - 5, X, YB + 5);
      SN := IntToStr(I);
      if Length(SN) = 1 then                                                   // adapt position of time value, depending on it's 1 or 2 digits
        X -= 3
      else
        X -= 6;
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clBlack;
      fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(X, YB + 7, SN);
    end;
  end;
  // Draw ticks and write values of Y-axis
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, N0Y, XL + 5, N0Y);
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(42, N0Y - 8, GFormat(Bacteria[0]));
  if (NMaxY <> -1) and (fBacteria1.edMaintenance.Text <> '') and (fBacteria1.edMaintenance.Text <> '0') then begin
    // Write maximum bacteria concentration value only if corresponding Y value on graph could be determined
    // (not always the case) and don't do it in the case M = 0 (output would overlap with the one of S0)
    fBacteria1G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, NMaxY, XL + 5, NMaxY);
    fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(42, NMaxY - 8, GFormat(NMax));
  end;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Line(XL - 5, S0Y, XL + 5, S0Y);
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.TextOut(42, S0Y - 8, GFormat(Nutrient[0]));
  // Reset pen width and pen and font color to default values
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Width := 1;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Pen.Color := clDefault;
  fBacteria1G.imGraph.Picture.Bitmap.Canvas.Font.Color := clDefault;
end;

{*************}
{ TfBacteria1 }
{*************}

{ Application start: Create bacteria images }

procedure TfBacteria1.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to MaxBacteria do begin
    aiBacteria[I] := TImage.Create(fBacteria1.aiBacteria[I]);
    aiBacteria[I].Parent  := Self;
    aiBacteria[I].Width   := imBacteria.Width;
    aiBacteria[I].Height  := imBacteria.Height;
    aiBacteria[I].Picture := imBacteria.Picture;
    aiBacteria[I].Visible := False;
  end;
  SetLength(GrowthTime, 0); SetLength(Bacteria, 0); SetLength(Nutrient, 0);
  iInterval := 1000;
end;

{ Menu item "File > New": Clear simulation form }

procedure TfBacteria1.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  edSimTime.Text := '';
  edSimBacteria.Text := '';
  edSimBacteriaDiff.Text := ''; edSimBacteriaDiff.Color := clDefault;
  edSimNutrient.Text := '';
  for I := 1 to MaxBacteria do
    aiBacteria[I].Visible := False;                                            // remove all bacteria by hiding them
  btStart.Caption := 'Start';
  btStart.Enabled := True;
  btPause.Caption := 'Pause';
  btPause.Enabled := False;
  btStart.SetFocus;
end;

{ Menu item "File > Reset": Load 'e. coli on glucose' as default culture sample; clear simulation form }

procedure TfBacteria1.mFileResetClick(Sender: TObject);

const
  Filename = 'Escherichia_coli_glucose_37.txt';

var
  Path: string;

begin
  Path := './samples/' + Filename;
  DoDirSeparators(Path);
  GetSampleData(Path, sBacteria, sNutrient, rR, rA);
  edBacteria.Text := FloatToStr(0.0046);
  edNutrient.Text := FloatToStr(0.6);
  edYieldCoeff.Text := FloatToStr(0.23);
  edMaintenance.Text := FloatToStr(0.25);
  mFileNew.Click;                                                              // execute "File > New" to clear the form
end;

{ Menu item "File > Exit": Exit the application }

procedure TfBacteria1.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Allow zero maintenance": Allow or not simulation without maintenance }

procedure TfBacteria1.mSettingsMaintenance0Click(Sender: TObject);

begin
  if mSettingsMaintenance0.Checked then
    mSettingsMaintenance0.Checked := False
  else
    mSettingsMaintenance0.Checked := True;
end;

{ Menu item "Settings > Simulation time interval > Automatic": Set simulation time interval to default (1sec) }

procedure TfBacteria1.mSettingsTimeAutoClick(Sender: TObject);

begin
  mSettingsTimeAuto.Checked := True;
  mSettingsTimeCustom.Checked := False;
  iInterval := 1000;
end;

{ Menu item "Settings > Simulation time interval > Custom": Ask user for simulation time interval }

procedure TfBacteria1.mSettingsTimeCustomClick(Sender: TObject);

var
  S: string;

// Simulation time interval = timer interval = how many secs real time (between 2 display updates) is used for 1h culture (growth) time

begin
  mSettingsTimeAuto.Checked := False;
  mSettingsTimeCustom.Checked := True;
  S := InputBox('Simulation time interval', 'Simulation time in sec for 1h real time', FloatToStr(iInterval / 1000));
  if S <> '' then                                                              // this is if user pushed "OK" (and not "Cancel")
    iInterval := Round(StrToFloat(S) * 1000);
end;

{ Menu item "Help > Bacteria growth": Display biology background help text }

procedure TfBacteria1.mHelpBacteriaClick(Sender: TObject);

begin
  if fBacteria1H.Visible then
    fBacteria1H.Hide;
  fBacteria1H.memoHelp.Lines.LoadFromFile('help1.txt');
  fBacteria1H.Show;
end;

{ Menu item "Help > Program help": Display program help text }

procedure TfBacteria1.mHelpHelpClick(Sender: TObject);

begin
  if fBacteria1H.Visible then
    fBacteria1H.Hide;
  fBacteria1H.memoHelp.Lines.LoadFromFile('help2.txt');
  fBacteria1H.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfBacteria1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Visible := False
  else begin
    S := 'Bacteria growth on 1 substrate (based on extended Monod model).' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, July, 2018.';
    pnAbout.Text := S;
    pnAbout.Visible := True;
  end;
end;

{ Button "Start/stop": Start resp. stop the simlation }

procedure TfBacteria1.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Starting simulation with actual parameters
  if btStart.Caption = 'Start' then begin
    ReadData(sBacteria, sNutrient, rN0, rS0, rR, rA, rY, rM, Mess);            // read user data from form
    if Mess = '' then begin                                                    // proceed if there were no error message
      // Get extended Monod model values (incl. arrays with T / N and S values)
      Monod_Extrema(rN0, rS0, rR, rA, rY, rM, True, iValues, iEndTime, rMaxBacteria);
      Monod_XY(rN0, rS0, rR, rA, rY, rM, iValues, iEndTime, True);
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

procedure TfBacteria1.btPauseClick(Sender: TObject);

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

{ Button "Samples": Load data from culture sample text file }

procedure TfBacteria1.btSamplesClick(Sender: TObject);

var
  Dir, Filename: string;

begin
  Dir := './samples';
  DoDirSeparators(Dir);
  dlgOpen.InitialDir := Dir;
  if dlgOpen.Execute then begin                                                // user has selected a samples file
    Filename := dlgOpen.Filename;                                              // get filename
    GetSampleData(Filename, sBacteria, sNutrient, rR, rA);                     // read sample data from this file
  end;
end;

{ Button "Graph": Draw the graph using actual parameters (will be done on form fBacteria1G) }

procedure TfBacteria1.btGraphClick(Sender: TObject);

var
  LegendX, Legend1Y, Legend2Y, Mess, S: string;

begin
  ReadData(sBacteria, sNutrient, rN0, rS0, rR, rA, rY, rM, Mess);              // read user data from form
  if Mess = '' then begin                                                      // proceed if there were no error
    // Write title and Monod model values
    fBacteria1G.stTitle.Caption := 'Growth of ' + sBacteria + ' on ' + sNutrient + '.';
    if rM = 0 then
      S := 'Monod model:  '
    else
      S := 'Extended Monod model:  ';
    S += 'r=' + FloatToStr(rR);
    S += '  a=' + FloatToStr(rA);
    S += '  γ=' + FloatToStr(rY);
    if rM <> 0 then
      S+= '  m=' + FloatToStr(rM);
    fBacteria1G.stMonod.Caption := S;
    // Calculate extended Monod model values and create the graph
    GraphClean(fBacteria1G.iImageWidth, fBacteria1G.iImageHeight);
    LegendX := 'Time [h]'; Legend1Y := 'Bacteria (*)'; Legend2Y := 'Nutrient (*)';
    DrawAxis(fBacteria1G.iGraphLeft, fBacteria1G.iGraphRight, fBacteria1G.iGraphTop, fBacteria1G.iGraphBottom, LegendX, Legend1Y, Legend2Y);
    Monod_Extrema(rN0, rS0, rR, rA, rY, rM, False, iValues, iEndTime, rMaxBacteria);
    Monod_XY(rN0, rS0, rR, rA, rY, rM, iValues, iEndTime, False);
    DrawGraph(iValues, iEndTime, rMaxBacteria, fBacteria1G.iGraphLeft, fBacteria1G.iGraphRight, fBacteria1G.iGraphTop, fBacteria1G.iGraphBottom);
    fBacteria1G.Show;
  end;
end;

{ Simulation timer routine }

procedure TfBacteria1.tiBacteriaTimer(Sender: TObject);

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
      // Set last value of N to 0 (instaed of 'some nothing' remaining)
      if rM <> 0 then
        Bacteria[iValues] := 0;
      // Set the nutrient display to maximum for S = S0
      shNutrient.Width := MaxNutrient;
      // Set the correct timer interval
      tiBacteria.Interval := iInterval;
    end
    // Simulation running (t > 0)
    else begin
      edSimTime.Text := FloatToStrF(iValue / (iValues div iEndTime), ffFixed, 0, 0) + ' h';;
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
    shNutrient.Width := Round(Nutrient[iValue] / Nutrient[0] * MaxNutrient);
    // Save number of bacteria actually displayed
    iBacteriaDisplayed := NBacteria;
    // Next time value (simulation step = 1h culture time)
    iValue += iValues div iEndTime;
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

