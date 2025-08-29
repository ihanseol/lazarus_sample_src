{********************************************}
{* Main unit for the DCircuits2 application *}
{********************************************}

{ Version 1.1: Bug fixed: IB1/IC1 currents < 0.001mA are now displayed with 4 or 5 decimal digits to avoid that they are displayed as 0 }

unit transistors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, PopupNotifier, LCLIntf, data;

type
  TArray3 = array[1..3] of Integer;
  TArray162 = array[1..162] of Real;
  { TfTransistors }
  TfTransistors = class(TForm)
    mTransistors: TMainMenu;
    mCircuit: TMenuItem;
    mNewCalculation: TMenuItem;
    mNewTest: TMenuItem;
    mExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsTransistors: TMenuItem;
    mSettingsTransistors1, mSettingsTransistors2, mSettingsTransistors3: TMenuItem;
    mSettingsIdentical: TMenuItem;
    mSettingsVoltage: TMenuItem;
    mSettingsResistance: TMenuItem;
    mSettingsChecking: TMenuItem;
    mHelp: TMenuItem;
    mHelpTransistors: TMenuItem;
    mHelpResistors: TMenuItem;
    mHelpContent: TMenuItem;
    mHelpAbout: TMenuItem;
    imCircuit: TImage;
    stTitle: TStaticText;
    Label1, Label2:  TLabel;
    laBeta1, laBeta2, laBeta3: TLabel;
    laIB1, laIB2, laIB3, laIC1, laIC2, laIC3: TLabel;
    laR1, laR2, laR3, laR4: TLabel;
    edVS, edIL: TEdit;
    edBeta1, edBeta2, edBeta3: TEdit;
    edIB1, edIB2, edIB3: TEdit;
    edIC1, edIC2, edIC3: TEdit;
    edR1, edR2, edR3, edR4: TEdit;
    edEvaluation: TEdit;
    StaticText2: TStaticText;
    btCheck: TButton;
    Popup: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mNewCalculationClick(Sender: TObject);
    procedure mNewTestClick(Sender: TObject);
    procedure mExitClick(Sender: TObject);
    procedure mSettingsTransistors1Click(Sender: TObject);
    procedure mSettingsTransistors2Click(Sender: TObject);
    procedure mSettingsTransistors3Click(Sender: TObject);
    procedure mSettingsIdenticalClick(Sender: TObject);
    procedure mSettingsVoltageClick(Sender: TObject);
    procedure mSettingsResistanceClick(Sender: TObject);
    procedure mSettingsCheckingClick(Sender: TObject);
    procedure mHelpTransistorsClick(Sender: TObject);
    procedure mHelpResistorsClick(Sender: TObject);
    procedure mHelpContentClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCheckClick(Sender: TObject);
  private
    iCircuits, iCircuit, iBeta1, iBeta2, iBeta3: Integer;
    rVS, rIL, rIC1, rIC2, rIC3, rIB1, rIB2, rIB3, rR1, rR2, rR3, rR4: Real;
    rUIB1, rUIC1, rUIB2, rUIC2, rUIB3, rUIC3, rUR1, rUR2, rUR3, rUR4: Real;
    sMaterial1, sMaterial2, sMaterial3: string;
    aCircuits: TArray3;
    aResistances: TArray162;
  end;

var
  fTransistors: TfTransistors;

procedure CircuitDisplay(N: Integer; U, I: Real; B1, B2, B3: Integer; M1, M2, M3: string);
procedure ResistanceDisplay(N: Integer; IC1, IC2, IC3, IB1, IB2, IB3, R1, R2, R3, R4: Real; Calc: Boolean);
procedure ResistanceCalculation(N: Integer; U, I: Real; B1, B2, B3: Integer; M1, M2, M3: string;
  var Resistances: TArray162; var IC1, IC2, IC3, IB1, IB2, IB3, R1, R2, R3, R4: Real);
procedure ResistanceCalculationTr1(U, I: Real; B1: Integer; M1: string; var IC1, IB1, R1: Real);
procedure ResistanceCalculationTr3(U, I: Real; B1, B2, B3: Integer; M1, M2, M3: string; var IC1, IC2, IC3, IB1, IB2, IB3, R1, R3, R4: Real);
procedure ResistanceCalculationTr2(U, I: Real; B1, B2: Integer; M1, M2: string; var IC1, IC2, IB1, IB2, R1, R3: Real);
procedure ReadStdResistors(var Resistances: TArray162);
procedure UpdateArray(Circuit: Integer; Operation: Char; var Circuits: TArray3; var NCircuits: Integer);
procedure SetFieldProps(Circuit: Integer; SetProp: Boolean);
function BEVoltage(M: string): Real;
function StandardResistance(R: Real; Resistances: TArray162): Real;

implementation

{$R *.lfm}

{ Display given circuit values }

procedure CircuitDisplay(N: Integer; U, I: Real; B1, B2, B3: Integer; M1, M2, M3: string);

var
  Filename, S: string;

begin
  // Display title
  S := IntToStr(N) + ' transistor';
  if N > 1 then
    S += 's';
  S += ' digital current circuit';
  fTransistors.stTitle.Caption := S;
  // Load and display circuit (as .jpg image)
  Filename := 't' + IntToStr(N) + '.jpg';
  fTransistors.imCircuit.Picture.LoadFromFile(Filename);
  // Display voltage and load current
  I := I * 1000; I := Round(1000 * I) / 1000;                                  // transform to mA and round up to 3 decimal digits
  fTransistors.edVS.Text := FloatToStr(U);
  fTransistors.edIL.Text := FloatToStr(I);
  // Display current gain
  if fTransistors.mSettingsVoltage.Checked then begin                          // BE voltage ignored (transistor material without importance)
    fTransistors.laBeta1.Caption := 'Q1 gain ß1';
    fTransistors.laBeta2.Caption := 'Q2 gain ß2';
    fTransistors.laBeta3.Caption := 'Q3 gain ß3'
  end
  else begin                                                                   // BE voltage considered (display transistor material)
    fTransistors.laBeta1.Caption := 'Q1 (' + LeftStr(M1, 2) + ') ß1';
    fTransistors.laBeta2.Caption := 'Q2 (' + LeftStr(M2, 2) + ') ß2';
    fTransistors.laBeta3.Caption := 'Q3 (' + LeftStr(M3, 2) + ') ß3';
  end;
  fTransistors.edBeta1.Text := IntToStr(B1);
  // Make transistor data visible/invisible depending on number of transistors in the circuit
  if N = 1 then begin                                                          // 1 transistor
    fTransistors.laBeta2.Visible := False;
    fTransistors.edBeta2.Visible := False;
  end
  else begin                                                                   // 2 or 3 transistors
    fTransistors.edBeta2.Text := IntToStr(B2);
    fTransistors.laBeta2.Visible := True;
    fTransistors.edBeta2.Visible := True;
  end;
  if N <> 3 then begin                                                         // 1 or 2 tansistors
    fTransistors.laBeta3.Visible := False;
    fTransistors.edBeta3.Visible := False;
  end
  else begin                                                                   // 3 transistors
    fTransistors.edBeta3.Text := IntToStr(B3);
    fTransistors.laBeta3.Visible := True;
    fTransistors.edBeta3.Visible := True;
  end;
end;

{ Display base and collector current(s) and resistance(s) }

procedure ResistanceDisplay(N: Integer; IC1, IC2, IC3, IB1, IB2, IB3, R1, R2, R3, R4: Real; Calc: Boolean);

begin
  // At start, make all invisible (except first transistor values);
  // make visible afterwards as used in the circuit
  fTransistors.edIB2.Visible := False; fTransistors.laIB2.Visible := False;
  fTransistors.edIB3.Visible := False; fTransistors.laIB3.Visible := False;
  fTransistors.edIC2.Visible := False; fTransistors.laIC2.Visible := False;
  fTransistors.edIC3.Visible := False; fTransistors.laIC3.Visible := False;
  fTransistors.edR3.Visible := False;  fTransistors.laR3.Visible := False;
  fTransistors.edR4.Visible := False;  fTransistors.laR4.Visible := False;
  fTransistors.edIB1.Font.Color := clDefault; fTransistors.edIC1.Font.Color := clDefault;
  fTransistors.edR1.Font.Color := clDefault; fTransistors.edR2.Font.Color := clDefault;
  // Calculation mode: Display IB1, IC1, R1 and R2
  // Currents to mA, resistances to kΩ (3 decimal digits rounding)
  if Calc then begin
    IB1 := IB1 * 1000;
    if Round(1000 * IB1) / 1000 = 0 then begin
      if Round(10000 * IB1) / 10000 = 0 then
        IB1 := Round(100000 * IB1) / 100000
      else
        IB1 := Round(10000 * IB1) / 10000
    end
    else
      IB1 := Round(1000 * IB1) / 1000;
    IC1 := IC1 * 1000;
    if Round(1000 * IC1) / 1000 = 0 then
      IC1 := Round(10000 * IC1) / 10000
    else
      IC1 := Round(1000 * IC1) / 1000;
    R1 := R1 / 1000;   R1 := Round(1000 * R1) / 1000;
    R2 := R2 / 1000;   R2 := Round(1000 * R2) / 1000;
    fTransistors.edIB1.Text := FloatToStr(IB1);
    fTransistors.edIC1.Text := FloatToStr(IC1);
    fTransistors.edR1.Text := FloatToStr(R1);
    fTransistors.edR2.Text := FloatToStr(R2);
  end;
  // Circuit has more than 1 transistor
  if N > 1 then begin
    // Make transistor 2 data visible
    fTransistors.edIB2.Visible := True; fTransistors.laIB2.Visible := True;
    fTransistors.edIC2.Visible := True; fTransistors.laIC2.Visible := True;
    fTransistors.edR3.Visible := True;  fTransistors.laR3.Visible := True;
    fTransistors.edIB2.Font.Color := clDefault; fTransistors.edIC2.Font.Color := clDefault;
    fTransistors.edR3.Font.Color := clDefault;
    // Calculation mode: Display IB2, IC2, R3
    if Calc then begin
      IB2 := IB2 * 1000; IB2 := Round(1000 * IB2) / 1000;
      IC2 := IC2 * 1000; IC2 := Round(1000 * IC2) / 1000;
      R3 := R3 / 1000;   R3 := Round(1000 * R3) / 1000;
      fTransistors.edIB2.Text := FloatToStr(IB2);
      fTransistors.edIC2.Text := FloatToStr(IC2);
      fTransistors.edR3.Text := FloatToStr(R3);
    end;
  end;
  // Circuit has 3 transistors
  if N = 3 then begin
    // Make transistor 3 data visible
    fTransistors.edIB3.Visible := True; fTransistors.laIB3.Visible := True;
    fTransistors.edIC3.Visible := True; fTransistors.laIC3.Visible := True;
    fTransistors.edR4.Visible := True;  fTransistors.laR4.Visible := True;
    fTransistors.edIB3.Font.Color := clDefault; fTransistors.edIC3.Font.Color := clDefault;
    fTransistors.edR4.Font.Color := clDefault;
    // Calculation mode: Display IB3, IC3, R4
    if Calc then begin
      IB3 := IB3 * 1000; IB3 := Round(1000 * IB3) / 1000;
      IC3 := IC3 * 1000; IC3 := Round(1000 * IC3) / 1000;
      R4 := R4 / 1000;   R4 := Round(1000 * R4) / 1000;
      fTransistors.edIB3.Text := FloatToStr(IB3);
      fTransistors.edIC3.Text := FloatToStr(IC3);
      fTransistors.edR4.Text := FloatToStr(R4);
    end;
  end;
end;

{ Calculate base resistance(s) }

procedure ResistanceCalculation(N: Integer; U, I: Real; B1, B2, B3: Integer; M1, M2, M3: string;
  var Resistances: TArray162; var IC1, IC2, IC3, IB1, IB2, IB3, R1, R2, R3, R4: Real);

begin
  R1 := 0; R2 := 0; R3 := 0; R4 := 0;
  IC1 := 0; IC2 := 0; IC3 := 0;
  IB1 := 0; IB2 := 0; IB3 := 0;
  // Calculation depending on number of transistors in the circuit
  case N of
    1: ResistanceCalculationTr1(U, I, B1, M1, IC1, IB1, R1);
    2: ResistanceCalculationTr2(U, I, B1, B2, M1, M2, IC1, IC2, IB1, IB2, R1, R3);
    3: ResistanceCalculationTr3(U, I, B1, B2, B3, M1, M2, M3, IC1, IC2, IC3, IB1, IB2, IB3, R1, R3, R4);
  end;
  // Use standard resistor instead of calculated value (if this option is checked)
  if fTransistors.mSettingsResistance.Checked then begin
    R1 := StandardResistance(R1, Resistances);
    if N > 1 then
      R3 := StandardResistance(R3, Resistances);
    if N = 3 then
      R4 := StandardResistance(R4, Resistances);
  end;
  // Set R2 = R1 (if R1 in 1kΩ - 1MΩ limits)
  if R1 < 1e+3 then
    R2 := 1e+3
  else if R1 > 1e+6 then
    R2 := 1e+6
  else
    R2 := R1;
end;

// Calculate base resistance (1 transistor circuit)

procedure ResistanceCalculationTr1(U, I: Real; B1: Integer; M1: string; var IC1, IB1, R1: Real);

begin
  // Collector current = load current
  IC1 := I;
  // Base current = collector current / transistor current gain
  IB1 := IC1 / B1;
  // Base resistance = voltage / base current
  R1 := (U - BEVoltage(M1)) / IB1;                                             // call BEVoltage to get base-emitter voltage
end;

// Calculate base resistances (2 transistors circuit)

procedure ResistanceCalculationTr2(U, I: Real; B1, B2: Integer; M1, M2: string; var IC1, IC2, IB1, IB2, R1, R3: Real);

begin
  // Do calculation for transistor Q2
  ResistanceCalculationTr1(U, I, B2, M2, IC2, IB2, R3);
  // Do calculation for transistor Q1 (with IC1 = IB2)
  ResistanceCalculationTr1(U, IB2, B1, M1, IC1, IB1, R1);
end;

// Calculate base resistances (3 transistors circuit)

procedure ResistanceCalculationTr3(U, I: Real; B1, B2, B3: Integer; M1, M2, M3: string; var IC1, IC2, IC3, IB1, IB2, IB3, R1, R3, R4: Real);

begin
  // Do calculation for transistor Q3
  ResistanceCalculationTr1(U, I, B3, M3, IC3, IB3, R4);
  // Do calculation for transistor Q2 (with IC2 = IB3)
  ResistanceCalculationTr1(U, IB3, B2, M2, IC2, IB2, R3);
  // Do calculation for transistor Q1 (with IC1 = IB2)
  ResistanceCalculationTr1(U, IB2, B1, M1, IC1, IB1, R1);
end;

{ Create array with standard resistor values (from file)}

procedure ReadStdResistors(var Resistances: TArray162);

var
  I, J, K, N: Integer;
  Mult: Real;
  SResistance: string;
  StdValues: array[1..24] of Integer;
  ResistanceFile: Text;

begin
  // Read standard values (10Ω - 91Ω) from file
  Assign(ResistanceFile, 'resistors.txt'); Reset(ResistanceFile);
  N := 0;
  while not EoF(ResistanceFile) do begin
    Readln(ResistanceFile, SResistance);
    if SResistance <> '' then begin
      Inc(N);
      StdValues[N] := StrToInt(SResistance);
    end;
  end;
  // Create array of all staandard resistances by multiplying the file values by 0.1, 1, 10 ... 1e+5
  K := 0;
  for I := 1 to 7 do begin
    if I = 1 then
      Mult := 0.1
    else
      Mult *= 10;
    for J := 1 to N do begin
      if Mult * StdValues[J] >= 2 then begin
        Inc(K);
        Resistances[K] := Mult * StdValues[J];
      end;
    end;
  end;
  Resistances[K + 1] := 10e+6;                                                 // add 10MΩ resistance
  Close(ResistanceFile);
end;

{ Add/delete element to Circuits array }

procedure UpdateArray(Circuit: Integer; Operation: Char; var Circuits: TArray3; var NCircuits: Integer);

var
  I, J: Integer;

begin
  // This array contains the values (1, 2 or 3) from which the random circuit (with 1, 2 or 3 transistors) is chosen
  // Each time the user changes the transistor number in the "Settings" menu, this procedure is called to update the array's content
  if Operation = '+' then begin
    // Add element to array
    Inc(NCircuits);
    Circuits[NCircuits] := Circuit;
  end
  else begin
    // Remove element from array
    J := 0;
    for I := 1 to NCircuits do begin
      if Circuits[I] = Circuit then
        J := 1
      else
        Circuits[I - J] := Circuits[I];
    end;
    Dec(NCircuits);
  end;
end;

{ Set the TabStop/ReadOnly properties of the form's edit fields }

procedure SetFieldProps(Circuit: Integer; SetProp: Boolean);

var
  ResetProp: Boolean;

begin
  // In calculation mode (and in exercise mode, after the user has entered the answers), fields will be set to readonly;
  // in exercise mode (when the program waits for the user's answer) the fields have to be editable
  ResetProp := not SetProp;
  fTransistors.edIB1.TabStop := SetProp; fTransistors.edIB2.TabStop := SetProp; fTransistors.edIB3.TabStop := SetProp;
  fTransistors.edIC1.TabStop := SetProp; fTransistors.edIC2.TabStop := SetProp; fTransistors.edIC3.TabStop := SetProp;
  fTransistors.edR1.TabStop := SetProp; fTransistors.edR2.TabStop := SetProp;
  fTransistors.edR3.TabStop := SetProp; fTransistors.edR4.TabStop := SetProp;
  fTransistors.edIB1.ReadOnly := ResetProp; fTransistors.edIB2.ReadOnly := ResetProp; fTransistors.edIB3.ReadOnly := ResetProp;
  fTransistors.edIC1.ReadOnly := ResetProp; fTransistors.edIC2.ReadOnly := ResetProp; fTransistors.edIC3.ReadOnly := ResetProp;
  fTransistors.edR1.ReadOnly := ResetProp; fTransistors.edR2.ReadOnly := ResetProp;
  fTransistors.edR3.ReadOnly := ResetProp; fTransistors.edR4.ReadOnly := ResetProp;
  // Set focus to first edit field of last transistor (first value calculated by the user)
  if SetProp then begin
    case Circuit of
      1: fTransistors.edIC1.SetFocus;
      2: fTransistors.edIC2.SetFocus;
      3: fTransistors.edIC3.SetFocus;
    end;
  end;
end;

{ Get base-emitter voltage }

function BEVoltage(M: string): Real;

var
  VBE: Real;

begin
  // Return 0V if IBE is chosen to be ignored
  if fTransistors.mSettingsVoltage.Checked then
    VBE := 0
  // Return constant IBE value, depending on transistor's material, otherwisse
  else begin
    if M = 'Silicon' then
      VBE := 0.7
    else if M = 'Germanium' then
      VBE := 0.3;
  end;
  BEVoltage := VBE;
end;

{ Determine standard resistor value for given resistnce }

function StandardResistance(R: Real; Resistances: TArray162): Real;

var
  StdX, I: Integer;
  StdR, MinDiff: Real;

begin
  StdR := R;
  I := 0; MinDiff := R; StdX := 0;
  // Lookup the standard resistor table, and return the values that is nearest to the given R
  repeat
    Inc(I);
    if Abs(R - Resistances[I]) < MinDiff then begin
      MinDiff := Abs(R - Resistances[I]);
      StdX := I;
    end;
  until I = 162;
  if StdX <> 0 then
    StdR := Resistances[StdX];
  StandardResistance := StdR;
end;

{*****************}
{* TfTransistors *}
{*****************}

{ Application start: Initialisation }

procedure TfTransistors.FormCreate(Sender: TObject);

begin
  ReadStdResistors(aResistances);
  iCircuits := 3;
  aCircuits[1] := 1; aCircuits[2] := 2; aCircuits[3] := 3;
  imCircuit.Picture.LoadFromFile('t3.jpg');
  Randomize;
end;

{ Menu item "Circuit > New calculation": Do new calculation for user defined circuit }

procedure TfTransistors.mNewCalculationClick(Sender: TObject);

begin
  // If IBE has to be ignored disable transistor material fields
  btCheck.Visible := False; edEvaluation.Visible := False;
  if mSettingsVoltage.Checked then begin
    fData.cbMaterial1.Enabled := False;
    fData.cbMaterial2.Enabled := False;
    fData.cbMaterial3.Enabled := False;
  end
  else begin
    fData.cbMaterial1.Enabled := True;
    if fData.rbTransistors2.Checked or fData.rbTransistors3.Checked then
      fData.cbMaterial2.Enabled := True;
    if fData.rbTransistors3.Checked then
      fData.cbMaterial3.Enabled := True;
  end;
  fData.bMaterial := not mSettingsVoltage.Checked;                             // tell data entry form if material has to be considered or not
  // Show the data entry form and wait until it is closed by the user
  fData.ShowModal;
  // Read data entered by the user from data entry form
  // Proceed only if button pressed was "OK"
  if fData.sButton = 'ok' then begin
    // Number of transistors
    if fData.rbTransistors1.Checked then
      iCircuit := 1
    else if fData.rbTransistors2.Checked then
      iCircuit := 2
    else
      iCircuit := 3;
    // Battery voltage and load current
    rVS := StrToFloat(fData.edVoltage.Text);
    if fData.edCurrent.Text <> '' then
      rIL := StrToFloat(fData.edCurrent.Text)
    else
      rIL := rVS / StrToFloat(fData.edResistance.Text);
    // Transisstrs' current gain values
    iBeta1 := StrToInt(fData.edBeta1.Text);
    iBeta2 := 0; iBeta3 := 0;
    if fData.edBeta2.Enabled then
      iBeta2 := StrToInt(fData.edBeta2.Text);
    if fData.edBeta3.Enabled then
      iBeta3 := StrToInt(fData.edBeta3.Text);
    // Transistors' material
    sMaterial1 := fData.cbMaterial1.Text;
    sMaterial2 := fData.cbMaterial2.Text;
    sMaterial3 := fData.cbMaterial3.Text;
    // Calculate the base resistance(s)
    ResistanceCalculation(iCircuit, rVS, rIL, iBeta1, iBeta2, iBeta3, sMaterial1, sMaterial2, sMaterial3, aResistances,
      rIC1, rIC2, rIC3, rIB1, rIB2, rIB3, rR1, rR2, rR3, rR4);
    // Display the circuit and the calculation results
    CircuitDisplay(iCircuit, rVS, rIL, iBeta1, iBeta2, iBeta3, sMaterial1, sMaterial2, sMaterial3);
    ResistanceDisplay(iCircuit, rIC1, rIC2, rIC3, rIB1, rIB2, rIB3, rR1, rR2, rR3, rR4, True);
    SetFieldProps(iCircuit, False);                                            // set edit fields to readonly
  end;
end;

{ Menu item "Circuit > New test": Generate new exercise }

procedure TfTransistors.mNewTestClick(Sender: TObject);

const
  Materials: array[0..1] of string = ('Silicon', 'Germanium');

var
  M: Integer;

begin
  // Clear edit fields
  edEvaluation.Text := '';
  edEvaluation.Color := clForm;
  edEvaluation.Visible := True;
  edIB1.Text := ''; edIC1.Text := '';
  edIB2.Text := ''; edIC2.Text := '';
  edIB3.Text := ''; edIC3.Text := '';
  edR1.Text := ''; edR2.Text := ''; edR3.Text := ''; edR4.Text := '';
  // Generate exercise values
  if iCircuits > 0 then begin
    iCircuit := aCircuits[Random(iCircuits) + 1];
    repeat
      rVS := Random(21) + 10;                                                  // VS = 10V to 30V
      rIL := ((Random(25) + 1) * 200) / 1000;                                  // IL = 200 mA to 5A
      if mSettingsIdentical.Checked then begin
        // Identical transistors
        iBeta1 := (Random(11) + 5) * 10;
        iBeta2 := iBeta1;
        iBeta3 := iBeta1;
        M := Random(2); sMaterial1 := Materials[M];
        sMaterial2 := sMaterial1; sMaterial3 := sMaterial1;
      end
      else begin
          // Traansistors with different characteristics
        repeat
          iBeta1 := (Random(16) + 5) * 10;
          iBeta2 := (Random(14) + 2) * 10;
          iBeta3 := (Random(10) + 1) * 10;
        until (iBeta1 >= iBeta2) and (iBeta2 >= iBeta3);
        M := Random(2); sMaterial1 := Materials[M];
        M := Random(2); sMaterial2 := Materials[M];
        M := Random(2); sMaterial3 := Materials[M];
      end;
      // Calculate base resistance(s)
      ResistanceCalculation(iCircuit, rVS, rIL, iBeta1, iBeta2, iBeta3, sMaterial1, sMaterial2, sMaterial3, aResistances, rIC1, rIC2, rIC3, rIB1, rIB2, rIB3, rR1, rR2, rR3, rR4);
    until rIB1 > 0.000010;                                                     // use minimum base current = 10mA
    // Display circuit and set properties of edit fields
    CircuitDisplay(iCircuit, rVS, rIL, iBeta1, iBeta2, iBeta3, sMaterial1, sMaterial2, sMaterial3);
    ResistanceDisplay(iCircuit, rIC1, rIC2, rIC3, rIB1, rIB2, rIB3, rR1, rR2, rR3, rR4, False);
    SetFieldProps(iCircuit, True);                                            // make edit fields editable
    // Enable and show the "Check" button
    btCheck.Visible := True; btCheck.Enabled := True;
  end
  // Error message if no transistor number has been selected
  else
    MessageDlg('Invalid settings', 'Please select at least one of the transistor number options', mtError, [mbOK], 0);
end;

{ Menu item "Circuit > Exit": Exit the application }

procedure TfTransistors.mExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Number of transistors > ...": Set number of transistors for exercise circuit }

procedure TfTransistors.mSettingsTransistors1Click(Sender: TObject);

begin
  if mSettingsTransistors1.Checked then begin
    mSettingsTransistors1.Checked := False;
    UpdateArray(1, '-', aCircuits, iCircuits);                      // Remove 1 transistor circuit from random circuits selection array
  end
  else begin
    mSettingsTransistors1.Checked := True;
    UpdateArray(1, '+', aCircuits, iCircuits);                       // Add 1 transistor circuit to random circuits selection array
  end;
end;

procedure TfTransistors.mSettingsTransistors2Click(Sender: TObject);

begin
  if mSettingsTransistors2.Checked then begin
    mSettingsTransistors2.Checked := False;
    UpdateArray(2, '-', aCircuits, iCircuits);                      // Remove 2 transistors circuit from random circuits selection array
  end
  else begin
    mSettingsTransistors2.Checked := True;
    UpdateArray(2, '+', aCircuits, iCircuits);                      // Add 2 transistors circuit to random circuits selection array
  end;
end;

procedure TfTransistors.mSettingsTransistors3Click(Sender: TObject);

begin
  if mSettingsTransistors3.Checked then begin
    mSettingsTransistors3.Checked := False;
    UpdateArray(3, '-', aCircuits, iCircuits);                      // Remove 3 transistors circuit from random circuits selection array
  end
  else begin
    mSettingsTransistors3.Checked := True;
    UpdateArray(3, '+', aCircuits, iCircuits);                      // Add 3 transistors circuit to random circuits selection array
  end;
end;

{ Menu item "Settings > Use all identical transistors": Select if all transistors should have same characteristics }

procedure TfTransistors.mSettingsIdenticalClick(Sender: TObject);

begin
  if mSettingsIdentical.Checked then
    mSettingsIdentical.Checked := False
  else
    mSettingsIdentical.Checked := True;
end;

{ Menu item "Settings > Ignore base-emitter voltage": Select if IBE should be ignored when calculating base resistance }

procedure TfTransistors.mSettingsVoltageClick(Sender: TObject);

begin
   if mSettingsVoltage.Checked then
     mSettingsVoltage.Checked := False
   else
     mSettingsVoltage.Checked := True;
end;

{ Menu item "Settings > Use standard resistor values": Select if base resistance(s) should be standard resistor values }

procedure TfTransistors.mSettingsResistanceClick(Sender: TObject);

begin
  if not mSettingsResistance.Checked then
    mSettingsResistance.Checked := True
  else
    mSettingsResistance.Checked := False;
end;

{ Menu item "Settings > Check resistances only": Select if user has to enter current values when answering exercise }

procedure TfTransistors.mSettingsCheckingClick(Sender: TObject);

begin
  if mSettingsChecking.Checked then
    mSettingsChecking.Checked := False
  else
    mSettingsChecking.Checked := True;
end;

{ Menu item "Help > Transistor switches": PDF help text concerning transistor switches }

procedure TfTransistors.mHelpTransistorsClick(Sender: TObject);

begin
  OpenDocument('Transistors.pdf');
end;

{ Menu item "Help > Standard resistors": PDF list of standard resistor values }

procedure TfTransistors.mHelpResistorsClick(Sender: TObject);

begin
  OpenDocument('Resistors.pdf');
end;

{ Menu item "Help > Program help": PDF help text concerning the usage of the DCircuits2 program }

procedure TfTransistors.mHelpContentClick(Sender: TObject);

begin
  OpenDocument('DCircuits2.pdf');
end;

{ Menu item "Help > Program about": Information about the DCircuits2 program }

procedure TfTransistors.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if Popup.Visible then
    Popup.Visible := False
  else begin
    S := 'Transistor circuits resistance calculation and problems generator.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, February, 2018.' + Chr(13);
    S += 'Version 1.1, © allu, August, 2018.';
    Popup.Text := S;
    Popup.Visible := True;                                                     // display info as popup notification
  end;
end;





{ Button "Check" clicked: check user's answer to exercise }

procedure TfTransistors.btCheckClick(Sender: TObject);

var
  Wrong: Boolean;

begin
  // Read user's answers from form
  Wrong := False;
  if edIB1.Text = '' then
    rUIB1 := 0
  else
    rUIB1 := StrToFloat(edIB1.Text);
  if edIC1.Text = '' then
    rUIC1 := 0
  else
    rUIC1 := StrToFloat(edIC1.Text);
  if edIB2.Text = '' then
    rUIB2 := 0
  else
    rUIB2 := StrToFloat(edIB2.Text);
  if edIC2.Text = '' then
    rUIC2 := 0
  else
    rUIC2 := StrToFloat(edIC2.Text);
  if edIB3.Text = '' then
    rUIB3 := 0
  else
    rUIB3 := StrToFloat(edIB3.Text);
  if edIC3.Text = '' then
    rUIC3 := 0
  else
    rUIC3 := StrToFloat(edIC3.Text);
  if edR1.Text = '' then
    rUR1 := 0
  else
    rUR1 := StrToFloat(edR1.Text);
  if edR2.Text = '' then
    rUR2 := 0
  else
    rUR2 := StrToFloat(edR2.Text);
  if edR3.Text = '' then
    rUR3 := 0
  else
    rUR3 := StrToFloat(edR3.Text);
  if edR4.Text = '' then
    rUR4 := 0
  else
    rUR4 := StrToFloat(edR4.Text);
  // Transform currents to mA and resistances to kΩ (and round to 3 decimal digits)
  rR1 := rR1 / 1000; rR1 := Round(1000 * rR1) / 1000;
  rR2 := rR2 / 1000; rR2 := Round(1000 * rR2) / 1000;
  rR3 := rR3 / 1000; rR3 := Round(1000 * rR3) / 1000;
  rR4 := rR4 / 1000; rR4 := Round(1000 * rR4) / 1000;
  rIB1 := rIB1 * 1000; rIB1 := Round(1000 * rIB1) / 1000;
  rIB2 := rIB2 * 1000; rIB2 := Round(1000 * rIB2) / 1000;
  rIB3 := rIB3 * 1000; rIB3 := Round(1000 * rIB3) / 1000;
  rIC1 := rIC1 * 1000; rIC1 := Round(1000 * rIC1) / 1000;
  rIC2 := rIC2 * 1000; rIC2 := Round(1000 * rIC2) / 1000;
  rIC3 := rIC3 * 1000; rIC3 := Round(1000 * rIC3) / 1000;
  // Check resistance R1
  if Abs(rR1 - rUR1) > 0.001 then begin
    edR1.Text := FloatToStr(rR1);
    edR1.Font.Color := clRed;
    Wrong := True;
  end;
  // Check resistance R2 (should be 1kΩ to 1MΩ)
  if (rUR2 < 1) or (rUR2 > 1000) then begin
    edR2.Text := FloatToStr(rR2);
    edR2.Font.Color := clRed;
    Wrong := True;
  end;
  // Check IB1 and IC1
  if not mSettingsChecking.Checked and (Abs(rIB1 - rUIB1) > 0.001) then begin
    edIB1.Text := FloatToStr(rIB1);
    edIB1.Font.Color := clRed;
    Wrong := True;
  end;
  if not mSettingsChecking.Checked and (Abs(rIC1 - rUIC1) > 0.001) then begin
    edIC1.Text := FloatToStr(rIC1);
    edIC1.Font.Color := clRed;
    Wrong := True;
  end;
  // If more than 1 transistor, check R3, IB2 and IC2
  if (iCircuit > 1) and (Abs(rR3 - rUR3) > 0.001) then begin
    edR3.Text := FloatToStr(rR3);
    edR3.Font.Color := clRed;
    Wrong := True;
  end;
  if not mSettingsChecking.Checked and (iCircuit > 1) and (Abs(rIB2 - rUIB2) > 0.001) then begin
    edIB2.Text := FloatToStr(rIB2);
    edIB2.Font.Color := clRed;
    Wrong := True;
  end;
  if not mSettingsChecking.Checked and (iCircuit > 1) and (Abs(rIC2 - rUIC2) > 0.001) then begin
    edIC2.Text := FloatToStr(rIC2);
    edIC2.Font.Color := clRed;
    Wrong := True;
  end;
  // If 3 transistors, check R4, IB3 and IC3
  if (iCircuit = 3) and (Abs(rR4 - rUR4) > 0.001) then begin
    edR4.Text := FloatToStr(rR4);
    edR4.Font.Color := clRed;
    Wrong := True;
  end;
  if not mSettingsChecking.Checked and (iCircuit = 3) and (Abs(rIB3 - rUIB3) > 0.001) then begin
    edIB3.Text := FloatToStr(rIB3);
    edIB3.Font.Color := clRed;
    Wrong := True;
  end;
  if not mSettingsChecking.Checked and (iCircuit = 3) and (Abs(rIC3 - rUIC3) > 0.001) then begin
    edIC3.Text := FloatToStr(rIC3);
    edIC3.Font.Color := clRed;
    Wrong := True;
  end;
  // User answers evaluation
  if Wrong then begin
    edEvaluation.Text := 'One or more of your answers are false!';
    edEvaluation.Color := clRed;
  end
  else begin
    edEvaluation.Text := 'All of your answers are correct!';
    edEvaluation.Color := clLime;
  end;
  SetFieldProps(iCircuit, False);                                              // disable edit field editing
  // Disable "Check" button
  btCheck.Enabled := False;
end;

end.

