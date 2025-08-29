{****************************************}
{* Main unit for Amplifiers application *}
{****************************************}

unit amplifiers_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LCLIntf, amplifiers_u2, amplifiers_u3;

type
  { TfAmplifiers }
  TfAmplifiers = class(TForm)
    mMenu: TMainMenu;
    mCircuit, mNewExercise, mNewCalculation, mNewCalculation2, mExit: TMenuItem;
    mSettings, mSettingsCircuits, mSettingsCircuits1, mSettingsCircuits2, mSettingsCircuits3: TMenuItem;
    mSettingsResistance, MenuItem1, mSettingsDigits2, mSettingsMABC, mSettingsIntBC, mSettingsIntF: TMenuItem;
    mHelp, mHelpResistors, mHelpContent, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    imCircuit: TImage;
    Label1, laAV, laRGiv, Label6, Label8, Label5, laVE, laICalc, laICalc2: TLabel;
    Label12, Label13, laRE, Label15, laUDC, laURgiv, laURGiv2, Label19: TLabel;
    Label20, laURE, laRGiv2, laURIn, laRIn, laUXCE, Label23, Label24: TLabel;
    laUIB, Label26, laUIcalc2, laUF, laUCE, laCE, laXCE, laDC, laF: TLabel;
    edRGiv2, edRin, edCE, edXCE, edF, edDC, edVS, edAV, edRGiv, edVB, edIB: TEdit;
    edBeta, edVE, edICalc, edICalc2, edR1, edR2, edRE, edEvaluation: TEdit;
    btCheck: TButton;
    btShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mNewExerciseClick(Sender: TObject);
    procedure mNewCalculationClick(Sender: TObject);
    procedure mNewCalculation2Click(Sender: TObject);
    procedure mExitClick(Sender: TObject);
    procedure mSettingsCircuits1Click(Sender: TObject);
    procedure mSettingsCircuits2Click(Sender: TObject);
    procedure mSettingsCircuits3Click(Sender: TObject);
    procedure mSettingsResistanceClick(Sender: TObject);
    procedure mSettingsDigits2Click(Sender: TObject);
    procedure mSettingsMABCClick(Sender: TObject);
    procedure mSettingsIntBCClick(Sender: TObject);
    procedure mSettingsIntFClick(Sender: TObject);
    procedure mHelpResistorsClick(Sender: TObject);
    procedure mHelpContentClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCheckClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    iCircuit: Integer;
    rVS, rRC, rF, rRin, rBeta, rVC, rAV: Real;
    rVE, rVB, rIC, rIE, rIB, rI2, rRE, rR1, rR2, rXCE, rCE: Real;
    aExCircuits: array[1..3] of Boolean;
    aResistances: TRealArray;
  end;

var
  fAmplifiers: TfAmplifiers;

implementation

{$R *.lfm}

{ Format real number (at given number of significant decimal digits) }

function RFormat(R: Real): string;

var
  F, I: Integer;
  SR: string;

begin
  // For small numbers use fixed format
  if R < 1E-3 then
    F := 4
  else if R < 1E-2 then
    F := 3
  // In normal case, use number of decimal digits as selected by user
  else begin
    if fAmplifiers.mSettingsDigits2.Checked then
      F := 2
    else
      F := 3;
  end;
  SR := FloatToStrF(R, ffFixed, F, F);
  // Remove non-significant zeros
  I := Length(SR);
  while SR[I] = '0' do
    Dec(I);
  SR := LeftStr(SR, I);
  if (RightStr(SR, 1) = ',') or (RightStr(SR, 1) = '.') then
    Delete(SR, Length(SR), 1);
  Result := SR;
end;

{ Create array with standard resistor values (from file)}

procedure ReadStdResistors(out Resistances: TRealArray);

var
  I, J, K, N: Integer;
  Mult: Real;
  SResistance: string;
  StdValues: array of Integer;
  ResistanceFile: Text;

begin
  // Read standard values (10Ω - 91Ω) from file
  Assign(ResistanceFile, 'resistors.txt'); Reset(ResistanceFile);
  N := 0;
  while not EoF(ResistanceFile) do begin
    Readln(ResistanceFile, SResistance);
    if SResistance <> '' then begin
      Inc(N); SetLength(StdValues, N);
      StdValues[N - 1] := StrToInt(SResistance);
    end;
  end;
  // Create array of all staandard resistances by multiplying the file values by 0.1, 1, 10 ... 1e+5
  K := 0;
  for I := 1 to 7 do begin
    if I = 1 then
      Mult := 0.1
    else
      Mult *= 10;
    for J := 0 to N - 1 do begin
      if Mult * StdValues[J] >= 2 then begin
        Inc(K); SetLength(Resistances, K);
        Resistances[K - 1] := Mult * StdValues[J];
      end;
    end;
  end;
  Inc(K); SetLength(Resistances, K);
  Resistances[K - 1] := 10e+6;                                                           // add 10MΩ resistance
  Close(ResistanceFile);
end;

{ Editfield properties: Visibility, position, labels }

procedure SetFieldProps1(Circuit: Integer; Capacitance: Boolean);

begin
  fAmplifiers.laRGiv2.Visible := False;   fAmplifiers.edRGiv2.Visible := False; fAmplifiers.laURGiv2.Visible := False;
  fAmplifiers.laCE.Visible := False;   fAmplifiers.edCE.Visible := False; fAmplifiers.laUCE.Visible := False;
  fAmplifiers.laRIn.Visible := False; fAmplifiers.edRIn.Visible := False; fAmplifiers.laURIn.Visible := False;
  fAmplifiers.laRE.Visible := False; fAmplifiers.edRE.Visible := False; fAmplifiers.laURE.Visible := False;
  fAmplifiers.laXCE.Visible := False; fAmplifiers.edXCE.Visible := False; fAmplifiers.laUXCE.Visible := False;
  fAmplifiers.laF.Visible := False;  fAmplifiers.edF.Visible := False; fAmplifiers.laUF.Visible := False;
  if Circuit = 1 then begin
    // RE to be calculated for simple circuit biasing
    fAmplifiers.laRE.Visible := True; fAmplifiers.edRE.Visible := True; fAmplifiers.laURE.Visible := True;
  end
  else if Circuit = 3 then begin
    // Extra fields displayed for circuit analysis
    fAmplifiers.laRGiv2.Visible := True;   fAmplifiers.edRGiv2.Visible := True; fAmplifiers.laURGiv2.Visible := True;
    fAmplifiers.laRIn.Visible := True; fAmplifiers.edRIn.Visible := True; fAmplifiers.laURIn.Visible := True;
    if Capacitance then begin
      // Capacitor related fields
      fAmplifiers.laCE.Visible := True;   fAmplifiers.edCE.Visible := True;   fAmplifiers.laUCE.Visible := True;
      fAmplifiers.laXCE.Visible := True; fAmplifiers.edXCE.Visible := True; fAmplifiers.laUXCE.Visible := True;
      fAmplifiers.laF.Visible := True;  fAmplifiers.edF.Visible := True;  fAmplifiers.laUF.Visible := True;
    end;
  end;
  // Default position of some edit fields (looks nicer to display them here than letting a gap)
  fAmplifiers.laDC.Left := fAmplifiers.laRGiv.Left;   fAmplifiers.edDC.Left := fAmplifiers.edRGiv.Left;
  fAmplifiers.laUDC.Left := fAmplifiers.laURGiv.Left; fAmplifiers.laAV.Left := fAmplifiers.laRGiv2.Left;
  fAmplifiers.edAV.Left := fAmplifiers.edRGiv2.Left;
  if (Circuit = 1) or (Circuit = 3) then begin
    // Editfield labels for simple circuit biasing and circuit analysis
    fAmplifiers.laRGiv.Caption := 'Resistance RC'; fAmplifiers.edRGiv.Hint := 'Collector resistance';
    fAmplifiers.laICalc.Caption := 'Current IC';   fAmplifiers.edICalc.Caption := 'Colletor current';
    if Circuit = 1 then begin
      fAmplifiers.laICalc2.Caption := 'Current I2'; fAmplifiers.edICalc2.Hint := 'Resistor R2 current';
    end
    else begin
      fAmplifiers.laICalc2.Caption := 'Current IE'; fAmplifiers.edICalc2.Hint := 'Emitter current';
    end;
    fAmplifiers.laDC.Caption := 'DC output VC';

    if Circuit = 3 then begin
      // Move fields to the right for circuit analysis (as there are supplementary fields)
      fAmplifiers.laDC.Left := fAmplifiers.laRGiv2.Left;   fAmplifiers.edDC.Left := fAmplifiers.edRGiv2.Left;
      fAmplifiers.laUDC.Left := fAmplifiers.laURGiv2.Left; fAmplifiers.laAV.Left := fAmplifiers.laF.Left;
      fAmplifiers.edAV.Left := fAmplifiers.edF.Left;
    end;
  end
  else begin
    // Editfield labels for emitter follower
    fAmplifiers.laRGiv.Caption := 'Resistance RE'; fAmplifiers.edRGiv.Hint := 'Emitter resistance';
    fAmplifiers.laICalc.Caption := 'Current IE';   fAmplifiers.edICalc.Hint := 'Emitter current';
    fAmplifiers.laICalc2.Caption := 'Current I2';  fAmplifiers.edICalc2.Hint := 'Resistor R2 current';
    fAmplifiers.laDC.Caption := 'DC output VE';
  end;
end;

{ Editfield properties: Readonly, tabstop, color }

procedure SetFieldProps2(Mode: string; Circuit: Integer);

var
  RO, TS: Boolean;

begin
  // Readonly and tabstop value depending on usage mode (exercise or calculation)
  if Mode = 'calc' then
    RO := True
  else
    RO := False;
  TS := not RO;
  if Circuit = 1 then begin
    // AV always given for simple circuit analysis
    fAmplifiers.edAV.ReadOnly := True; fAmplifiers.edAV.Tabstop := False;
    fAmplifiers.edAV.Color := clMenuHighlight;
  end
  else begin
    // For other circuits, depends on usage mode
    fAmplifiers.edAV.ReadOnly := RO; fAmplifiers.edAV.Tabstop := TS;
    if Mode = 'calc' then begin
      if Circuit = 2 then
        fAmplifiers.edAV.Color := clYellow
      else
        fAmplifiers.edAV.Color := clFuchsia;
    end
    else begin
      fAmplifiers.edAV.Color := clDefault;
    end;
  end;
  if Circuit = 3 then begin
    // Editfield properties for circuit analysis
    fAmplifiers.edDC.ReadOnly := RO; fAmplifiers.edDC.Tabstop := TS;
    fAmplifiers.edR1.ReadOnly := True; fAmplifiers.edR1.Tabstop := False;
    fAmplifiers.edR2.ReadOnly := True; fAmplifiers.edR2.Tabstop := False;
    fAmplifiers.edR1.Color := clSkyBlue; fAmplifiers.edR2.Color := clSkyBlue;
    if Mode = 'calc' then
      fAmplifiers.edDC.Color := clFuchsia
    else
      fAmplifiers.edDC.Color := clDefault;
  end
  else begin
    // Editfield properties for circuit biasing
    fAmplifiers.edDC.ReadOnly := True; fAmplifiers.edDC.Tabstop := False;
    fAmplifiers.edR1.ReadOnly := RO; fAmplifiers.edR1.Tabstop := TS;
    fAmplifiers.edR2.ReadOnly := RO; fAmplifiers.edR2.Tabstop := TS;
    fAmplifiers.edDC.Color := clMenuHighlight;
    if Mode = 'calc' then begin
      fAmplifiers.edR1.Color := clFuchsia; fAmplifiers.edR2.Color := clFuchsia;
    end
    else begin
      fAmplifiers.edR1.Color := clDefault; fAmplifiers.edR2.Color := clDefault;
    end;
  end;
  // Default properties for other editfields (adapted below as applicable)
  fAmplifiers.edVB.ReadOnly := RO;     fAmplifiers.edVB.Tabstop := TS;
  fAmplifiers.edVE.ReadOnly := RO;     fAmplifiers.edVE.TabStop := TS;
  fAmplifiers.edIB.ReadOnly := RO;     fAmplifiers.edIB.TabStop := TS;
  fAmplifiers.edICalc.ReadOnly := RO;  fAmplifiers.edICalc.TabStop := TS;
  fAmplifiers.edICalc2.ReadOnly := RO; fAmplifiers.edICalc2.TabStop := TS;
  fAmplifiers.edXCE.ReadOnly := RO;    fAmplifiers.edXCE.TabStop := TS;
  fAmplifiers.edRE.ReadOnly := RO;     fAmplifiers.edRE.TabStop := TS;
  fAmplifiers.edF.ReadOnly := RO;      fAmplifiers.edF.TabStop := TS;
  fAmplifiers.edVB.Color := clDefault; fAmplifiers.edVE.Color := clDefault;
  fAmplifiers.edIB.Color := clDefault; fAmplifiers.edICalc.Color := clDefault;
  fAmplifiers.edICalc2.Color := clDefault;
  // Adapt properties, depending on circuit type and usage mode
  if Circuit = 3 then begin
    fAmplifiers.edF.Color := clDefault;
    fAmplifiers.edXCE.Color := clDefault;
  end;
  if Mode = 'calc' then begin
    fAmplifiers.edRE.Color := clFuchsia; fAmplifiers.edF.Color := clFuchsia;
  end
  else begin
    fAmplifiers.edRE.Color := clDefault; fAmplifiers.edF.Color := clDefault;
  end;
  // Clear all edit fields
  fAmplifiers.edVS.Text := '';    fAmplifiers.edRGiv.Text := '';   fAmplifiers.edRGiv2.Text := '';   fAmplifiers.edF.Text := '';
  fAmplifiers.edBeta.Text := '';  fAmplifiers.edRIn.Text := '';
  fAmplifiers.edDC.Text := '';    fAmplifiers.edAV.Text := '';     fAmplifiers.edVB.Text := '';      fAmplifiers.edVE.Text := '';
  fAmplifiers.edIB.Text := '';    fAmplifiers.edICalc.Text := '';  fAmplifiers.edICalc2.Text := '';  fAmplifiers.edXCE.Text := '';
  fAmplifiers.edR1.Text := '';    fAmplifiers.edR2.Text := '';     fAmplifiers.edRE.Text := '';      fAmplifiers.edCE.Text := '';
  // Resistances in kΩ by default
  fAmplifiers.laURgiv.Caption := 'kΩ'; fAmplifiers.laURgiv2.Caption := 'kΩ';
  fAmplifiers.laURE.Caption := 'kΩ'; fAmplifiers.laUXCE.Caption := 'kΩ';
end;

{ Display circuit and given circuit values }

procedure CircuitDisplay1(Circuit: Integer; VS, RC, RE, R1, R2, CE, Beta, RIn, VC, VE, AV: Real);

const
  Circuits: array[1..3] of string = (
    'Simple amplifier biasing', 'Emitter follower biasing', 'Amplifier circuit analysis'
  );

var
  Filename: string;

begin
  // Display title
  fAmplifiers.stTitle.Caption := 'Transistor circuits: ' + Circuits[Circuit] + '.';
  // Load and display circuit (JPG image)
  if (Circuit = 3) and (CE = 0) then
    Filename := 'circuit1.jpg'
  else
    Filename := 'circuit' + IntToStr(Circuit) + '.jpg';
  fAmplifiers.imCircuit.Picture.LoadFromFile(Filename);
  // Display (given) circuit values
  fAmplifiers.edVS.Text := RFormat(VS);
  fAmplifiers.edBeta.Text := RFormat(Beta);
  // Simple circuit biasing: RC is given
  if Circuit = 1 then begin
    fAmplifiers.edAV.Text := RFormat(AV);
    // RC in Ω or kΩ
    if RC < 1000 then begin
      fAmplifiers.edRGiv.Text := RFormat(RC);
      fAmplifiers.laURgiv.Caption := 'Ω';
    end
    else
      fAmplifiers.edRGiv.Text := RFormat(RC * 1E-3);
    fAmplifiers.edDC.Text := RFormat(VC);
  end
  // Emitter follower biasing: RE is given
  else if Circuit = 2 then begin
    // RE in Ω or kΩ
    if RE < 1000 then begin
      fAmplifiers.edRGiv.Text := RFormat(RE);
      fAmplifiers.laURgiv.Caption := 'Ω';
    end
    else
      fAmplifiers.edRGiv.Text := RFormat(RE * 1E-3);
    fAmplifiers.edDC.Text := RFormat(VE);
  end
  // Circuit analysis: Both RC and RE are given
  else begin
    // RC in Ω or kΩ
    if RC < 1000 then begin
      fAmplifiers.edRGiv.Text := RFormat(RC);
      fAmplifiers.laURGiv.Caption := 'Ω';
    end
    else
      fAmplifiers.edRGiv.Text := RFormat(RC * 1E-3);
    // RE in Ω or kΩ
    if RE < 1000 then begin
      fAmplifiers.edRGiv2.Text := RFormat(RE);
      fAmplifiers.laURgiv2.Caption := 'Ω';
    end
    else
      fAmplifiers.edRGiv2.Text := RFormat(RE * 1E-3);
    // Other resistance in kΩ
    fAmplifiers.edR1.Text := RFormat(R1 * 1E-3);
    fAmplifiers.edR2.Text := RFormat(R2 * 1E-3);
    fAmplifiers.edRIn.Text := RFormat(RIn * 1E-3);
    // Capacitance (value is in µF)
    fAmplifiers.edCE.Text := RFormat(CE);
  end;
end;

{ Display calculated circuit values }

procedure CircuitDisplay2(Circuit: Integer; VC, AV, VB, VE, IB, IC, IE, I2, R1, R2, RE, F, XCE: Real);

begin
  if Circuit = 3 then
    fAmplifiers.edDC.Text := RFormat(VC);
  if Circuit <> 1 then
    fAmplifiers.edAV.Text := RFormat(AV);
  fAmplifiers.edVB.Text := RFormat(VB);
  fAmplifiers.edVE.Text := RFormat(VE);
  // Base current in µA (with or without decimal digits) or in mA
  if fAmplifiers.mSettingsMABC.Checked then begin
    if fAmplifiers.mSettingsIntBC.Checked then
      fAmplifiers.edIB.Text := FloatToStrF(IB * 1E+6, ffFixed, 0, 0)
    else
      fAmplifiers.edIB.Text := RFormat(IB * 1E+6);
  end
  else
    fAmplifiers.edIB.Text := RFormat(IB * 1E+3);
  // First curresnt field is IC (for simple circuit biasing and circuit analysis, IE for emitter follower)
  if Circuit = 2 then
    fAmplifiers.edICalc.Text := RFormat(IE * 1E+3)
  else
    fAmplifiers.edICalc.Text := RFormat(IC * 1E+3);
  // Circuit analysis fields
  if Circuit = 3 then begin
    // Second curresnt field is IE
    fAmplifiers.edICalc2.Text := RFormat(IE * 1E+3);
    // Frequency (with or without decimal digits)
    if fAmplifiers.mSettingsIntF.Checked then
      fAmplifiers.edF.Text := FloatToStrF(F, ffFixed, 0, 0)
    else
      fAmplifiers.edF.Text := RFormat(F);
    // Capacitor reactance in Ω or kΩ
    if XCE < 1000 then begin
      fAmplifiers.edXCE.Text := RFormat(XCE);
      fAmplifiers.laUXCE.Caption := 'Ω';
    end
    else
      fAmplifiers.edXCE.Text := RFormat(XCE * 1E-3);
  end
  // Circuit biasing fields
  else begin
    // Biasing resistances in kΩ
    fAmplifiers.edR1.Text := RFormat(R1 * 1E-3);
    fAmplifiers.edR2.Text := RFormat(R2 * 1E-3);
    // Current in mA
    fAmplifiers.edICalc2.Text := RFormat(I2 * 1E+3);
    // Simple amplifier: RE in Ω or kΩ
    if Circuit = 1 then begin
      if RE < 1000 then begin
        fAmplifiers.edRE.Text := RFormat(RE);
        fAmplifiers.laURE.Caption := 'Ω';
      end
      else
        fAmplifiers.edRE.Text := RFormat(RE * 1E-3);
    end;
  end;
end;

{ Get second standard resistor value for given resistance }

function StandardResistance2(R, RStd: Real; Resistances: TRealArray): Real;

// The aim is to get the two standard values, that are closest to the given resistance
// The first of these values has been determined by the StandardResistance function
// (coded within the amplifiers_u2 unit) and is passed as 2nd parameter to this function

var
  I, IX: Integer;
  RStd2: Real;

begin
  // Find index of first standard resistance in standard resistors array
  for I := 0 to Length(Resistances) - 1 do begin
    if RStd = Resistances[I] then
      IX := I;
  end;
  RStd2 := RStd;
  // Determine second standard resistance
  if (IX + 1 <= Length(Resistances) - 1) and (RStd < R) then
    // If the first standard resistance is smaller than given resistance, the second one is the next value in the array
    RStd2 := Resistances[IX + 1]
  else if (IX - 1 >= 0) and (RStd > R) then
    // If the first standard resistance is greater than given resistance, the second one is the previous value in the array
    RStd2 := Resistances[IX - 1];
  Result := RStd2;
end;

{ Simple amplifier biasing calculations }
{ ------------------------------------- }

procedure SimpleAmplifier(VS, RC, Beta, VC, AV: Real; out VB, VE, IB, IC, I2, R1, R2, RE: Real);

begin
  RE := RC / AV;
  VE := (VS - VC) / AV;
  VB := VE + 0.7;                                                                        // silicon based transistor
  IC := (VS - VC) / RC;
  IB := IC / Beta;
  I2 := 10 * IB;                                                                         // rule of thumb
  R2 := VB / I2;
  R1 := (VS - VB) / (I2 + IB);
end;

{ Emitter follower biasing calculations }
{ ------------------------------------- }

procedure EmitterFollower(VS, RE, Beta, VE: Real; out AV, VB, IB, IE, I2, R1, R2: Real);

begin
  AV := 1;
  VB := VE + 0.7;                                                                        // silicon based transistor
  IE := VE / RE;
  IB := IE / Beta;
  I2 := 10 * IB;                                                                         // rule of thumb
  R2 := VB / I2;
  R1 := (VS - VB) / (I2 + IB);
end;

{ Amplifier analysing calculations }
{ -------------------------------- }

procedure AmplifierAnalysis(VS, RC, RE, CE, Beta, RIn, R1, R2: Real; out VC, AV, VB, VE, IB, IC, IE, XCE, F: Real);

var
  VR: Real;

begin
  VB := VS * (R2 / (R1 + R2));
  VE := VB - 0.7;                                                                        // silicon based transistor
  IE := VE / RE; IC := IE; IB := IC / Beta;
  VR := RC * IC;
  VC := VS - VR;
  // Calculations if there is no capacitor
  if CE = 0 then begin
    AV := RC / RE;
    XCE := 0; F := 0;
  end
  // Calculations if there is an emitter bypass capacitor
  else begin
    AV := Beta * (RC / RIn);
    XCE := RE / 10;                                                                      // rule of thumb
    F := 1 / (2 * Pi * (CE * 1E-6) * XCE);
  end;
end;

{**************}
{ TfAmplifiers }
{**************}

{ Application start: Initialisation }

procedure TfAmplifiers.FormCreate(Sender: TObject);

begin
  // Read standard resistor values from file
  ReadStdResistors(aResistances);
  // All circuits may be used in exercises
  aExCircuits[1] := True; aExCircuits[2] := True; aExCircuits[3] := True;
  // Set GUI for simple amplifier biasing calculation
  imCircuit.Picture.LoadFromFile('circuit1.jpg');
  SetFieldProps1(1, False); SetFieldProps2('calc', 1);
  // Start random number generator
  Randomize;
end;

{ Menu item "Circuit > New circuit biasing": Do new biasing calculations for user defined circuit }

procedure TfAmplifiers.mNewCalculationClick(Sender: TObject);

var
  IX, I: Integer;
  R1Std1, R2Std1, R1Std2, R2Std2, VBStd, VBDiff: Real;
  VBStds: array[0..3] of Real;
  S: string;

begin
  // Show the circuit biasing data entry form and wait until it is closed by the user
  fData.StdR := aResistances; fData.bRStd := mSettingsResistance.Checked;
  fData.ShowModal;
  // Proceed only if button pressed was "OK"
  if fData.sButton = 'ok' then begin
    // Read data entered by the user from data entry form
    if fData.rbCircuit1.Checked then
      iCircuit := 1
    else
      iCircuit := 2;
    rVS := fData.rVS;
    if iCircuit = 1 then
      rRC := fData.rRC
    else
      rRE := fData.rRE;
    rBeta := fData.rBeta;
    if iCircuit = 1 then begin
      rVC := fData.rVC;
      rAV := fData.rAV;
    end
    else begin
      rVE := fData.rVE;
    end;
    // Calculate the resistances that bias the circuit to the DC output voltage (and eventually the AC voltage gain) specified
    if iCircuit = 1 then
      SimpleAmplifier(rVS, rRC, rBeta, rVC, rAV, rVB, rVE, rIB, rIC, rI2, rR1, rR2, rRE)
    else
      EmitterFollower(rVS, rRE, rBeta, rVE, rAV, rVB, rIB, rIE, rI2, rR1, rR2);
    // Recalculate resistances (standard resistor values)
    edEvaluation.Visible := False;
    if mSettingsResistance.Checked then begin
      if iCircuit = 2 then
        rRE := StandardResistance(rRE, aResistances)
      else
        rRC := StandardResistance(rRC, aResistances);
      // R1 and R2: Determine "optimal" standard resistances (cf. help text for details)
      R1Std1 := StandardResistance(rR1, aResistances);
      R2Std1 := StandardResistance(rR2, aResistances);
      R1Std2 := StandardResistance2(rR1, R1Std1, aResistances);
      R2Std2 := StandardResistance2(rR2, R2Std1, aResistances);
      if not (((rR1 = R1Std1) or (rR1 = R1Std2)) and ((rR2 = R2Std1) or (rR2 = R2Std2))) then begin
        VBStds[0] := rVS * (R2Std1 / (R1Std1 + R2Std1));
        VBStds[1] := rVS * (R2Std2 / (R1Std2 + R2Std2));
        VBStds[2] := rVS * (R2Std1 / (R1Std2 + R2Std1));
        VBStds[3] := rVS * (R2Std2 / (R1Std1 + R2Std2));
        IX := 0; VBDiff := Abs(rVB - VBStds[IX]);
        for I := 1 to 3 do begin
          if Abs(rVB - VBStds[I]) < VBDiff then begin
            VBDiff := Abs(rVB - VBStds[I]); IX := I;
          end;
        end;
        if VBDiff  <= 0.1 * rVB then begin
          // Smallest difference between VB calculated from VE and VB calculated from standard resistances is less then 10%:
          // Determine the standard resistances pair, for those this is the case
          VBStd := VBStds[IX];
          edEvaluation.Text := 'VB for standard resistors = ' + RFormat(VBStd) + ': OK!';
          edEvaluation.Visible := True; edEvaluation.Color := clDefault;
          case IX of
            0: begin
              rR1 := R1Std1; rR2 := R2Std1;
            end;
            1: begin
              rR1 := R1Std2; rR2 := R2Std2;
            end;
            2: begin
              rR1 := R1Std2; rR2 := R2Std1;
            end;
            3: begin
              rR1 := R1Std1; rR2 := R2Std2;
            end;
          end;
        end
        else begin
          // Smallest difference between VB calculated from VE and VB calculated from standard resistances is more then 10%:
          // Display warning message (and use values of calculated resistances in display)
          S := 'VB for all standard resistors considered is more than 10% off the ideal base voltage!';
          MessageDlg('Biasing resistances', S, mtWarning, [mbOK], 0);
        end;
      end;
    end;
    // Set edit field properties
    SetFieldProps1(iCircuit, False); SetFieldProps2('calc', iCircuit);
    // Display circuit and given circuit values
    CircuitDisplay1(iCircuit, rVS, rRC, rRE, rR1, rR2, rCE, rBeta, rRIn, rVC, rVE, rAV);
    // Display calculated circuit values
    CircuitDisplay2(iCircuit, rVC, rAV, rVB, rVE, rIB, rIC, rIE, rI2, rR1, rR2, rRE, rF, rXCE);
    // Hide evaluation field and buttons
    btCheck.Visible := False; btShow.Visible := False;
  end;
end;

{ Menu item "Circuit > New circuit anaysis": Do new analysis calculations for user defined circuit }

procedure TfAmplifiers.mNewCalculation2Click(Sender: TObject);

begin
  // Show the circuit analysis data entry form and wait until it is closed by the user
  fData2.StdR := aResistances; fData2.bRStd := mSettingsResistance.Checked;
  fData2.ShowModal;
  // Proceed only if button pressed was "OK"
  if fData2.sButton = 'ok' then begin
    iCircuit := 3;
    // Read data entered by the user from data entry
    rVS := fData2.rVS;
    rRC := fData2.rRC;      rRE := fData2.rRE;
    rR1 := fData2.rR1;      rR2 := fData2.rR2;
    rBeta := fData2.rBeta;  rRin  := fData2.rRin;
    rCE:= fData2.rCE;
    // Calculate the circuit values
    AmplifierAnalysis(rVS, rRC, rRE, rCE, rBeta, rRIn, rR1, rR2, rVC, rAV, rVB, rVE, rIB, rIC, rIE, rXCE, rF);
    // Set edit field properties
    if rCE = 0 then
      SetFieldProps1(iCircuit, False)
    else
      SetFieldProps1(iCircuit, True);
    SetFieldProps2('calc', iCircuit);
    // Display circuit and given circuit values
    CircuitDisplay1(iCircuit, rVS, rRC, rRE, rR1, rR2, rCE, rBeta, rRIn, rVC, rVE, rAV);
    // Display calculated circuit values
    CircuitDisplay2(iCircuit, rVC, rAV, rVB, rVE, rIB, rIC, rIE, rI2, rR1, rR2, rRE, rF, rXCE);
    // If user enters unrealistic values, calculation results aren't useful
    if (rVC < 0) or (rVE < 0) then
      MessageDlg('Analysis failure', 'This seems not to be a working amplifier circuit!', mtWarning, [mbOK], 0);
    // Hide evaluation field and buttons
    edEvaluation.Visible := False;
    btCheck.Visible := False; btShow.Visible := False;
  end;
end;

{ Menu item "Circuit > New test": Generate new exercise }

procedure TfAmplifiers.mNewExerciseClick(Sender: TObject);

var
  N, IX, I: Integer;
  R1Std1, R2Std1, R1Std2, R2Std2, VBDiff: Real;
  VBStds: array[0..3] of Real;
  OK: Boolean;

begin
  // Check if user selected at least one circuit
  N := 0;
  for I := 1 to 3 do begin
    if aExCircuits[I] then
      Inc(N);
  end;
  // Generate exercise values
  if N > 0 then begin
    // Random circuit (among those selected)
    repeat
      iCircuit := Random(3) + 1;
    until aExCircuits[iCircuit];
    // Random circuit values (within reasonable limits)
    repeat
      OK := True;
      rVS := Random(21) + 10;                                                            // VS = 10V to 30V
      rBeta := (Random(11) + 5) * 10;                                                    // β  = 50 to 150
      if iCircuit = 1 then begin
        // Simple amplifier biasing
        rRC := (Random(1901) + 100);                                                     // RC = 0.1 to 2.0 kΩ
        rRC := StandardResistance(rRC, aResistances);
        rVC := (Random(3 * Round(rVS / 4) - Round(rVS / 4) + 1) + Round(rVS / 4));       // VC = VS/4 to 3VS/4
        rAV := Random(19) + 2;                                                           // AV = 2 to 20
      end
      else if iCircuit = 2 then begin
        // Emitter follower biasing
        rRE := (Random(1901) + 100);                                                     // RE = 0.1 to 2.0 kΩ
        rRE := StandardResistance(rRE, aResistances);
        rVE := (Random(3 * Round(rVS / 4) - Round(rVS / 4) + 1) + Round(rVS / 4));       // VE = VS/4 to 3VS/4
      end
      else begin
        // Amplifier circuit analysis
        repeat
          rR1 := Random(99551) + 450;                                                    // R1 = 450Ω to 1MΩ
          rR2 := Random(99551) + 450;                                                    // R2 = 450Ω to 1MΩ
        until (rR1 / rR2 >= 5) and (rR1 / rR2 <= 25);                                    // R1 = 5 ... 25 times R2
        rR1 := StandardResistance(rR1, aResistances);
        rR2 := StandardResistance(rR2, aResistances);
        repeat
          rRC := (Random(1901) + 100);                                                   // RC = 0.1 to 2.0 kΩ
          rRE := (Random(1901) + 100);                                                   // RE = 0.1 to 2.0 kΩ
        until rRC / rRE >= 1.25;                                                         // RC = 5/4 * RE or greater
        rRC := StandardResistance(rRC, aResistances);
        rRE := StandardResistance(rRE, aResistances);
        rRIn := 1000 + (Random(10) + 1) * 100;                                           // Rin = 1 to 2 kΩ
        if Random(4) = 0 then
          rCE := 0                                                                       // circuit without capacitor
        else
          rCE := (Random(9) + 2) * 10;                                                   // CE = 20 to 100 µF
      end;
      // Calculate circuit values
      if (iCircuit = 1) or (iCircuit = 2) then begin
        // Amplifier circuit biasing
        if iCircuit = 1 then begin
          SimpleAmplifier(rVS, rRC, rBeta, rVC, rAV, rVB, rVE, rIB, rIC, rI2, rR1, rR2, rRE);
          if mSettingsResistance.Checked then
            rRE := StandardResistance(rRE, aResistances);
        end
        else begin
          EmitterFollower(rVS, rRE, rBeta, rVE, rAV, rVB, rIB, rIE, rI2, rR1, rR2);
        end;
        if mSettingsResistance.Checked then begin
          // Find standard resistances with nearest VB to VB calculated
          R1Std1 := StandardResistance(rR1, aResistances);
          R2Std1 := StandardResistance(rR2, aResistances);
          R1Std2 := StandardResistance2(rR1, R1Std1, aResistances);
          R2Std2 := StandardResistance2(rR2, R2Std1, aResistances);
          if not (((rR1 = R1Std1) or (rR1 = R1Std2)) and ((rR2 = R2Std1) or (rR2 = R2Std2))) then begin
            VBStds[0] := rVS * (R2Std1 / (R1Std1 + R2Std1));
            VBStds[1] := rVS * (R2Std2 / (R1Std2 + R2Std2));
            VBStds[2] := rVS * (R2Std1 / (R1Std2 + R2Std1));
            VBStds[3] := rVS * (R2Std2 / (R1Std1 + R2Std2));
            IX := 0; VBDiff := Abs(rVB - VBStds[IX]);
            for I := 1 to 3 do begin
              if Abs(rVB - VBStds[I]) < VBDiff then begin
                VBDiff := Abs(rVB - VBStds[I]); IX := I;
              end;
            end;
            if VBDiff  <= 0.1 * rVB then begin
              case IX of
                0: begin
                  rR1 := R1Std1; rR2 := R2Std1;
                end;
                1: begin
                  rR1 := R1Std2; rR2 := R2Std2;
                end;
                2: begin
                  rR1 := R1Std2; rR2 := R2Std1;
                end;
                3: begin
                  rR1 := R1Std1; rR2 := R2Std2;
                end;
              end;
            end
            else begin
              // Redo random circuit generation, if no standard resistances with acceptable VB have been found
              OK := False;
            end;
          end;
        end;
      end
      else begin
        // Amplifier circuit analyse
        AmplifierAnalysis(rVS, rRC, rRE, rCE, rBeta, rRIn, rR1, rR2, rVC, rAV, rVB, rVE, rIB, rIC, rIE, rXCE, rF);
        if (rVC < 0) or (rVE < 0) then
          // Be sure that the circuit is a working one
          OK := False;
        if rVC > 0.8 * rVS then
          // Avoid to big DC output (may result in calculation problems, if I remember well?)
          OK := False;
      end;
    until OK;
    // Set edit field properties
    if rCE = 0 then
      SetFieldProps1(iCircuit, False)
    else
      SetFieldProps1(iCircuit, True);
    SetFieldProps2('ex', iCircuit);
    // Display circuit and given circuit values
    CircuitDisplay1(iCircuit, rVS, rRC, rRE, rR1, rR2, rCE, rBeta, rRIn, rVC, rVE, rAV);
    // Clear evaluation field
    edEvaluation.Text := '';
    edEvaluation.Color := clDefault;
    edEvaluation.Visible := True;
    // Set focus on first user entry field
    if iCircuit = 1 then
      edVB.SetFocus
    else if iCircuit = 2 then
      edAV.SetFocus
    else
      edDC.SetFocus;
    // Show buttons ("Show" button enabled only after "Check" has been pushed)
    btCheck.Visible := True; btCheck.Enabled := True;
    btShow.Visible := True; btShow.Enabled := False;
  end
  // Error message if no circuit type is selected
  else
    MessageDlg('Invalid settings', 'You must select at least one of the circuits in the "Settings" menu!', mtError, [mbOK], 0);
end;

{ Menu item "Circuit > Exit": Exit the application }

procedure TfAmplifiers.mExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Circuits for exercises > ...": Selection of the circuits, that should be used in exercises }

procedure TfAmplifiers.mSettingsCircuits1Click(Sender: TObject);

begin
  if mSettingsCircuits1.Checked then
    mSettingsCircuits1.Checked := False
  else
    mSettingsCircuits1.Checked := True;
  aExCircuits[1] := mSettingsCircuits1.Checked;
end;

procedure TfAmplifiers.mSettingsCircuits2Click(Sender: TObject);

begin
  if mSettingsCircuits2.Checked then
    mSettingsCircuits2.Checked := False
  else
    mSettingsCircuits2.Checked := True;
  aExCircuits[2] := mSettingsCircuits2.Checked;
end;

procedure TfAmplifiers.mSettingsCircuits3Click(Sender: TObject);

begin
  if mSettingsCircuits3.Checked then
    mSettingsCircuits3.Checked := False
  else
    mSettingsCircuits3.Checked := True;
  aExCircuits[3] := mSettingsCircuits3.Checked;
end;

{ Menu item "Settings > Use standard resistor values": Select if resistances should be standard resistor values }

procedure TfAmplifiers.mSettingsResistanceClick(Sender: TObject);

begin
  if not mSettingsResistance.Checked then
    mSettingsResistance.Checked := True
  else
    mSettingsResistance.Checked := False;
end;

{ Menu item "Settings > Results with 2 decimal digits": Toggle between 2 and 3 decimal digits precision }

procedure TfAmplifiers.mSettingsDigits2Click(Sender: TObject);

begin
  if mSettingsDigits2.Checked then
    mSettingsDigits2.Checked := False
  else
    mSettingsDigits2.Checked := True;
end;

{ Menu item "Settings > Base current in µA": Toggle between base current in µA and base current in mA }

procedure TfAmplifiers.mSettingsMABCClick(Sender: TObject);

begin
  if mSettingsMABC.Checked then begin
    mSettingsMABC.Checked := False;
    laUIB.Caption := 'mA';
    // Base current as integer only with µA
    mSettingsIntBC.Checked := False;
    mSettingsIntBC.Enabled := False;
  end
  else begin
    mSettingsMABC.Checked := True;
    laUIB.Caption := 'µA';
    mSettingsIntBC.Enabled := True;
  end;
end;

{ Menu item "Settings > Base current as integer": Toggle between base current as integer and base current with 2/3 decimal digits }

procedure TfAmplifiers.mSettingsIntBCClick(Sender: TObject);

begin
  if mSettingsIntBC.Checked then
    mSettingsIntBC.Checked := False
  else
    mSettingsIntBC.Checked := True;
end;

{ Menu item "Settings > Frequency as integer": Toggle between frequency as integer and frequency with 2/3 decimal digits }

procedure TfAmplifiers.mSettingsIntFClick(Sender: TObject);

begin
  if mSettingsIntF.Checked then
    mSettingsIntF.Checked := False
  else
    mSettingsIntF.Checked := True;
end;

{ Menu item "Help > Standard resistors": PDF list of standard resistor values }

procedure TfAmplifiers.mHelpResistorsClick(Sender: TObject);

begin
  OpenDocument('Resistors.pdf');
end;

{ Menu item "Help > Program help": PDF help text concerning the usage of the "Amplifiers" program }

procedure TfAmplifiers.mHelpContentClick(Sender: TObject);

begin
  OpenDocument('Amplifiers.pdf');
end;

{ Menu item "Help > About": Display appliction about }

procedure TfAmplifiers.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronic circuits: Transistor amplifiers.' + LineEnding + LineEnding;
  S += 'The application has two functionaities:' + LineEnding;
  S += '  1. Amplifier circuit biasing and analysis.' + LineEnding;
  S += '  2. Electronics exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September-November 2020.';
  MessageDlg('About "Amplifiers"', S, mtInformation, [mbOK], 0);
end;

{ Button "Check": Check user answer }

procedure TfAmplifiers.btCheckClick(Sender: TObject);

var
  UAV, UVB, UVE, UIB, UICalc, UICalc2, UXCE, UR1, UR2, URE, UCE: Real;
  Wrong: Boolean;

begin
  // Read user's answers from form
  Wrong := False;
  if edAV.Text = '' then
    UAV := 0
  else
    UAV := StrToFloat(edAV.Text);
  if edVB.Text = '' then
    UVB := 0
  else
    UVB := StrToFloat(edVB.Text);
  if edVE.Text = '' then
    UVE := 0
  else
    UVE := StrToFloat(edVE.Text);
  if edIB.Text = '' then
    UIB := 0
  else begin
    if mSettingsMABC.Checked then
      UIB := StrToFloat(edIB.Text) * 1E-6
    else
      UIB := StrToFloat(edIB.Text) * 1E-3;
  end;
  if edICalc.Text = '' then
    UICalc := 0
  else
    UICalc := StrToFloat(edICalc.Text) * 1E-3;
  if edICalc2.Text = '' then
    UICalc2 := 0
  else
    UICalc2 := StrToFloat(edICalc2.Text) * 1E-3;
  if edXCE.Text = '' then
    UXCE := 0
  else
    UXCE := StrToFloat(edXCE.Text);
  if edR1.Text = '' then
    UR1 := 0
  else
    UR1 := StrToFloat(edR1.Text) * 1E+3;
  if edR2.Text = '' then
    UR2 := 0
  else
    UR2 := StrToFloat(edR2.Text) * 1E+3;
  if edRe.Text = '' then
    URE := 0
  else
    URE := StrToFloat(edRE.Text) * 1E+3;
  if edF.Text = '' then
    UCE := 0
  else
    UCE := StrToFloat(edF.Text) * 1E-6;
  // Check user answers, field by field; highlight each false answer (setting editfield color to red)
  if (iCircuit <> 1) and (RFormat(UAV) <> RFormat(rAV)) then begin
    Wrong := True;
    edAV.Color := clRed;
  end;
  if RFormat(UVB) <> RFormat(rVB) then begin
    Wrong := True;
    edVB.Color := clRed;
  end;
  if RFormat(UVE) <> RFormat(rVE) then begin
    Wrong := True;
    edVE.Color := clRed;
  end;
  if RFormat(UIB) <> RFormat(rIB) then begin
    Wrong := True;
    edIB.Color := clRed;
  end;
  if ((iCircuit = 2) and (RFormat(UICalc) <> RFormat(rIE))) or ((iCircuit <> 2) and (RFormat(UICalc) <> RFormat(rIC))) then begin
    Wrong := True;
    edICalc.Color := clRed;
  end;
  if ((iCircuit = 3) and (RFormat(UICalc2) <> RFormat(rIE))) or ((iCircuit <> 3) and (RFormat(UICalc2) <> RFormat(rI2))) then begin
    Wrong := True;
    edICalc2.Color := clRed;
  end;
  if (iCircuit = 3) and (RFormat(UXCE) <> RFormat(rXCE)) then begin
    Wrong := True;
    edXCE.Color := clRed;
  end;
  if RFormat(UR1) <> RFormat(rR1) then begin
    Wrong := True;
    edR1.Color := clRed;
  end;
  if RFormat(UR2) <> RFormat(rR2) then begin
    Wrong := True;
    edR2.Color := clRed;
  end;
  if (iCircuit = 1) and (RFormat(URE) <> RFormat(rRE)) then begin
    Wrong := True;
    edRE.Color := clRed;
  end;
  if (iCircuit = 3) and (RFormat(UCE) <> RFormat(rCE)) then begin
    Wrong := True;
    edF.Color := clRed;
  end;
  // User answers evaluation
  if Wrong then begin
    edEvaluation.Text := 'One or more of your answers are false!';
    edEvaluation.Color := clRed;
    btShow.Enabled := True;                                                              // give user possibility to view correct answers
  end
  else begin
    edEvaluation.Text := 'All of your answers are correct!';
    edEvaluation.Color := clLime;
  end;
  btCheck.Enabled := False;
end;

{ Button "Show": Show all exercise answer values}

procedure TfAmplifiers.btShowClick(Sender: TObject);

begin
  CircuitDisplay2(iCircuit, rVC, rAV, rVB, rVE, rIB, rIC, rIE, rI2, rR1, rR2, rRE, rF, rXCE);
  edEvaluation.Text := StringReplace(edEvaluation.Text, ' are ', ' were ', [rfReplaceAll]);
end;

end.

