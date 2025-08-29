{****************************************}
{*      Electronics:  RLC circuits      *}
{*--------------------------------------*}
{* Simple physics problems generator    *}
{* Main unit for ACircuits1 application *}
{****************************************}

unit circuits1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, Math, help;

type
  TInputFields   = array[1..22] of TEdit;
  TCircuitValues = array[1..22] of Real;
  TProgramValues = array[1..22] of Boolean;
  {**************}
  { TfACircuits1 }
  {**************}
  TfACircuits1 = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mCircuits, mCircuitsR, mCircuitsL, mCircuitsC: TMenuItem;
    mCircuitsRL, mCircuitsRC, mCircuitsRLC1, mCircuitsRLC2: TMenuItem;
    mOptions, mOptionsView: TMenuItem;
    mHelp, mHelpElectronics, mHelpProgram, mHelpAbout: TMenuItem;
    Title: TStaticText;
    imCircuit: TImage;
    Label10, Label11, Label12: TLabel;
    edQuestions, edCorrect, edSuccess: TEdit;
    Label1, Label2, Label3, Label4: TLabel;
    Label13, Label16, Label24, Label29, Label37: TLabel;
    laVS, laF, laT: TLabel;
    laR, laL, laC: TLabel;
    laXL, laXC, laZ: TLabel;
    laG, laBL, laBC, laY: TLabel;
    laVR, laVL, laVC, laV: TLabel;
    laIR, laIL, laIC, laI: TLabel;
    laP, laP2: TLabel;
    edVS, edF, edT: TEdit;
    edR, edL, edC: TEdit;
    edXL, edXC, edZ: TEdit;
    edG, edBC, edBL, edY: TEdit;
    edVR, edVL, edVC, edV: TEdit;
    edIR, edIL, edIC, edI: TEdit;
    edP: TEdit;
    cobP: TComboBox;
    shP: TShape;
    imEval: TImage;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mCircuitsRCClick(Sender: TObject);
    procedure mCircuitsLClick(Sender: TObject);
    procedure mCircuitsCClick(Sender: TObject);
    procedure mCircuitsRLClick(Sender: TObject);
    procedure mCircuitsRClick(Sender: TObject);
    procedure mCircuitsRLC1Click(Sender: TObject);
    procedure mCircuitsRLC2Click(Sender: TObject);
    procedure mOptionsViewClick(Sender: TObject);
    procedure mHelpElectronicsClick(Sender: TObject);
    procedure mHelpProgramClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
    iP, iQuestion, iCorrect: Integer;
    rVS, rF, rT, rR, rL, rC, rXL, rXC, rZ, rG, rBL, rBC, rY, rVR, rVL, rVC, rV, rIR, rIL, rIC, rI, rP: Real;
    bAppStart: Boolean;
    sV: string;
    aCircuitsUsed: array[1..7] of Boolean;
    aInputFields: TInputFields;
    aCircuitValues: TCircuitValues;
    aProgramValues: TProgramValues;
  end;

const
  Circuits: array[1..7] of string = ('pure resistor', 'pure inductor', 'pure capacitor', 'resistor-inductor', 'resistor-capacitor', 'series RLC', 'parallel RLC');
  CircuitFiles: array[1..7] of string = ('R', 'L', 'C', 'RL', 'RC', 'RLC1', 'RLC2');

var
  fACircuits1: TfACircuits1;

implementation

{$R *.lfm}

{ Format number for 'nice' display }

function RFormat(N: Real): string;

var
  F: Integer;

begin
  // Round number to 4 decimals (as used for exercises)
  N := Round(10000 * N) / 10000;
  // Display significant decimal digits only
  if N = Int(N) then
    F := 0
  else if 10 * N = Int(10 * N) then
    F := 1
  else if 100 * N = Int(100 * N) then
    F := 2
  else if 1000 * N = Int(1000 * N) then
    F := 3
  else
    F := 4;
  RFormat := FloatToStrF(N, ffFixed, 0, F);
end;

{ Start a new test }

procedure NewTest(out Question, Correct: Integer);

begin
  // Clear variables
  Question := 0; Correct := 0;
  fACircuits1.edQuestions.Clear; fACircuits1.edCorrect.Clear; fACircuits1.edSuccess.Clear;
  fACircuits1.edSuccess.Color := clDefault;
  // Execute a "Click start button" action
  fACircuits1.btStart.Caption := 'Start';
  fACircuits1.btStart.Click;
end;

{ Reset form fields }

procedure FieldsReset;

begin
  // Disable labels (enable for actual circuit later)
  fACircuits1.laR.Enabled := False; fACircuits1.laL.Enabled := False; fACircuits1.laC.Enabled := False;
  fACircuits1.laXL.Enabled := False; fACircuits1.laXC.Enabled := False;
  fACircuits1.laG.Enabled := False; fACircuits1.laBL.Enabled := False; fACircuits1.laBC.Enabled := False;
  fACircuits1.laVR.Enabled := False; fACircuits1.laVL.Enabled := False; fACircuits1.laVC.Enabled := False;
  fACircuits1.laIR.Enabled := False; fACircuits1.laIL.Enabled := False; fACircuits1.laIC.Enabled := False;
  // Clear all edit fields
  fACircuits1.edVS.Clear; fACircuits1.edF.Clear; fACircuits1.edT.Clear;
  fACircuits1.edR.Clear; fACircuits1.edL.Clear; fACircuits1.edC.Clear;
  fACircuits1.edXL.Clear; fACircuits1.edXC.Clear; fACircuits1.edZ.Clear;
  fACircuits1.edG.Clear; fACircuits1.edBL.Clear; fACircuits1.edBC.Clear; fACircuits1.edY.Clear;
  fACircuits1.edVR.Clear; fACircuits1.edVL.Clear; fACircuits1.edVC.Clear; fACircuits1.edV.Clear;
  fACircuits1.edIR.Clear; fACircuits1.edIL.Clear; fACircuits1.edIC.Clear; fACircuits1.edI.Clear;
  fACircuits1.edP.Clear;  fACircuits1.cobP.ItemIndex := 0;
  // Disable edit fields (enable for actual circuit later)
  fACircuits1.edR.Enabled := False; fACircuits1.edL.Enabled := False; fACircuits1.edC.Enabled := False;
  fACircuits1.edXL.Enabled := False; fACircuits1.edXC.Enabled := False;
  fACircuits1.edG.Enabled := False; fACircuits1.edBL.Enabled := False; fACircuits1.edBC.Enabled := False;
  fACircuits1.edVR.Enabled := False; fACircuits1.edVL.Enabled := False; fACircuits1.edVC.Enabled := False;
  fACircuits1.edIR.Enabled := False; fACircuits1.edIL.Enabled := False; fACircuits1.edIC.Enabled := False;
  // Set edit fields color to default (set to clCream for actual exercise later)
  fACircuits1.edVS.Color := clDefault; fACircuits1.edF.Color := clDefault; fACircuits1.edT.Color := clDefault;
  fACircuits1.edR.Color := clDefault; fACircuits1.edL.Color := clDefault; fACircuits1.edC.Color := clDefault;
  fACircuits1.edI.Color := clDefault;
  // Set edit fields to writable (set to read-only for actual exercise later)
  fACircuits1.edVS.ReadOnly := False; fACircuits1.edF.ReadOnly := False; fACircuits1.edT.ReadOnly := False;
  fACircuits1.edR.ReadOnly := False; fACircuits1.edL.ReadOnly := False; fACircuits1.edC.ReadOnly := False;
  fACircuits1.edI.ReadOnly := False;
  // Set edit fields tabstops to enabled (set to disabled for actual exercise later)
  fACircuits1.edVS.TabStop := True; fACircuits1.TabStop := True; fACircuits1.edT.TabStop := True;
  fACircuits1.edR.TabStop := True; fACircuits1.edL.TabStop := True; fACircuits1.edC.TabStop := True;
  fACircuits1.edI.TabStop := True;
  // Set edit field font color to default (set to clRed for wrong answers later)
  fACircuits1.edVS.Font.Color := clDefault; fACircuits1.edF.Font.Color := clDefault; fACircuits1.edT.Font.Color := clDefault;
  fACircuits1.edR.Font.Color := clDefault; fACircuits1.edL.Font.Color := clDefault; fACircuits1.edC.Font.Color := clDefault;
  fACircuits1.edXL.Font.Color := clDefault; fACircuits1.edXC.Font.Color := clDefault; fACircuits1.edZ.Font.Color := clDefault;
  fACircuits1.edG.Font.Color := clDefault; fACircuits1.edBL.Font.Color := clDefault; fACircuits1.edBC.Font.Color := clDefault; fACircuits1.edY.Font.Color := clDefault;
  fACircuits1.edVR.Font.Color := clDefault; fACircuits1.edVL.Font.Color := clDefault; fACircuits1.edVC.Font.Color := clDefault;; fACircuits1.edV.Font.Color := clDefault;
  fACircuits1.edIR.Font.Color := clDefault; fACircuits1.edIL.Font.Color := clDefault; fACircuits1.edIC.Font.Color := clDefault; fACircuits1.edI.Font.Color := clDefault;
  fACircuits1.edP.Font.Color := clDefault;  fACircuits1.shP.Visible := False;
  // Clear the circuit picture
  fACircuits1.imEval.Picture.Clear;
end;

{ Enable labels and edit fields for actual exercise }

procedure FieldsEnable(CType: Integer);

begin
  case CType of
       1: begin
            fACircuits1.laR.Enabled := True; fACircuits1.laG.Enabled := True; fACircuits1.laVR.Enabled := True; fACircuits1.laIR.Enabled := True;
            fACircuits1.edR.Enabled := True; fACircuits1.edG.Enabled := True; fACircuits1.edVR.Enabled := True; fACircuits1.edIR.Enabled := True;
          end;
       2: begin
            fACircuits1.laL.Enabled := True; fACircuits1.laXL.Enabled := True; fACircuits1.laBL.Enabled := True; fACircuits1.laVL.Enabled := True; fACircuits1.laIL.Enabled := True;
            fACircuits1.edL.Enabled := True; fACircuits1.edXL.Enabled := True; fACircuits1.edBL.Enabled := True; fACircuits1.edVL.Enabled := True; fACircuits1.edIL.Enabled := True;
          end;
       3: begin
            fACircuits1.laC.Enabled := True; fACircuits1.laXC.Enabled := True; fACircuits1.laBC.Enabled := True; fACircuits1.laVC.Enabled := True; fACircuits1.laIC.Enabled := True;
            fACircuits1.edC.Enabled := True; fACircuits1.edXC.Enabled := True; fACircuits1.edBC.Enabled := True; fACircuits1.edVC.Enabled := True; fACircuits1.edIC.Enabled := True;
          end;
       4: begin
            fACircuits1.laR.Enabled := True; fACircuits1.laL.Enabled := True; fACircuits1.laXL.Enabled := True; fACircuits1.laG.Enabled := True; fACircuits1.laBL.Enabled := True;
            fACircuits1.laVR.Enabled := True; fACircuits1.laVL.Enabled := True; fACircuits1.laIR.Enabled := True; fACircuits1.laIL.Enabled := True;
            fACircuits1.edR.Enabled := True; fACircuits1.edL.Enabled := True; fACircuits1.edXL.Enabled := True; fACircuits1.edG.Enabled := True; fACircuits1.edBL.Enabled := True;
            fACircuits1.edVR.Enabled := True; fACircuits1.edVL.Enabled := True; fACircuits1.edIR.Enabled := True; fACircuits1.edIL.Enabled := True;
          end;
       5: begin
            fACircuits1.laR.Enabled := True; fACircuits1.laC.Enabled := True; fACircuits1.laXC.Enabled := True; fACircuits1.laG.Enabled := True; fACircuits1.laBC.Enabled := True;
            fACircuits1.laVR.Enabled := True; fACircuits1.laVC.Enabled := True; fACircuits1.laIR.Enabled := True; fACircuits1.laIC.Enabled := True;
            fACircuits1.edR.Enabled := True; fACircuits1.edC.Enabled := True; fACircuits1.edXC.Enabled := True; fACircuits1.edG.Enabled := True; fACircuits1.edBC.Enabled := True;
            fACircuits1.edVR.Enabled := True; fACircuits1.edVC.Enabled := True; fACircuits1.edIR.Enabled := True; fACircuits1.edIC.Enabled := True;
          end;
    6, 7: begin
            fACircuits1.laR.Enabled := True; fACircuits1.laL.Enabled := True; fACircuits1.laC.Enabled := True; fACircuits1.laXL.Enabled := True; fACircuits1.laXC.Enabled := True;
            fACircuits1.laG.Enabled := True; fACircuits1.laBL.Enabled := True; fACircuits1.laBC.Enabled := True; fACircuits1.laVR.Enabled := True; fACircuits1.laVL.Enabled := True;
            fACircuits1.laVC.Enabled := True; fACircuits1.laIR.Enabled := True; fACircuits1.laIL.Enabled := True; fACircuits1.laIC.Enabled := True;
            fACircuits1.edR.Enabled := True; fACircuits1.edL.Enabled := True; fACircuits1.edC.Enabled := True; fACircuits1.edXL.Enabled := True; fACircuits1.edXC.Enabled := True;
            fACircuits1.edG.Enabled := True; fACircuits1.edBL.Enabled := True; fACircuits1.edBC.Enabled := True; fACircuits1.edVR.Enabled := True; fACircuits1.edVL.Enabled := True;
            fACircuits1.edVC.Enabled := True; fACircuits1.edIR.Enabled := True; fACircuits1.edIL.Enabled := True; fACircuits1.edIC.Enabled := True;
          end;
  end;
end;

{ Fill-in circuit values for actual exercise }

procedure FieldsFill(V: string; var CValues: TCircuitValues; var PValues: TProgramValues; Fields: TInputFields);

var
  J: Integer;

begin
  // Use string variable for source voltage (as may be given as sin function)
  if fACircuits1.mOptionsView.Checked or PValues[1] then
    Fields[1].Text := V;
  // For other fields, use actual value
  for J := 2 to 22 do begin
    if Fields[J].Enabled then begin                                                      // value must be applicable for actual circuit
      if fACircuits1.mOptionsView.Checked or PValues[J] then                             // display value if given by program (or if View mode is selected)
        Fields[J].Text := RFormat(CValues[J]);
    end;
  end;
end;

{--------------------------------}
{ Random circuit values routines }
{--------------------------------}

{ Random voltage value }

function Voltage: Real;

begin
  Voltage := 10 * (Random(15) + 10);                                                     // Voltage between 100 and 240 volts
end;

{ Random voltage frequency }

function Frequency: Real;

begin
  Frequency := 10 * (Random(6) + 5);                                                     // Frequency between 50 and 100 Hz
end;

{ Random voltage period }

function Period: Real;

begin
  Period := 1 / (10 * (Random(6) + 5));                                                  // Period for frequency between 50 and 100 Hz
end;

{ Random current value }

function Current: Real;

var
  ImA: Integer;
  OK: Boolean;

begin
  repeat
    OK := True;
    ImA := 10 * (Random(391) + 10);                                                      // current between 0.10 and 4.00 amps
    if ((ImA < 1000) and (ImA mod 50 <> 0)) or (((ImA >= 1000) and (ImA mod 100 <> 0))) then
      // Just for 'nice' current values...
      OK := False;
  until OK;
  Current := ImA / 1000;
end;

{ Random resistance value }

function Resistance: Real;

begin
  Resistance := Random(991) + 10;                                                        // resistance between 10 and 1000 ohms
end;

{ Random inductance value }

function Inductance: Real;

begin
  Inductance := (Random(10) + 1) / 10;                                                   // inductance between 0.1 and 1 henrys
end;

{ Random capacitance value }

function Capacitance: Real;

begin
  Capacitance := Random(461) + 10;                                                       // capacitance between 10 and 470 microFarads
end;

{-----------------------------------}
{ RLC circuits calculation routines }
{-----------------------------------}

{ Pure resistor (R) circuit }

procedure CircuitR(var V, I, R, Z, P: Real; var XP: Integer);

begin
  if R = 0 then begin
    // If R not given, calculate impedance from V and I
    Z := V / I; R := Z;
  end
    // If R given, impedance is equal to R
  else
    Z := R;
  // If V or I not given, calculate it
  if V = 0 then
    V := I * Z
  else if I = 0 then
    I := V / Z;
  // Phase angle = 0º for R-circuit
  P := 0; XP := 0;
end;

{ Pure inductor (L) circuit }

procedure CircuitL(var V, I, F, L, XL, Z, P: Real; var XP: Integer);

begin
  if L = 0 then begin
    // If L not given, calculate XL and L from V and I
    XL := V / I;
    L := XL / (2 * 3.14 * F);
  end
  else
    // If L given, calculate XL from L
    XL := 2 * 3.14 * F * L;
  // Impedance is equal to XL
  Z := XL;
  // If V or I not given, calculate it
  if V = 0 then
    V := I * Z
  else if I = 0 then
    I := V / Z;
  // Phase angle = 90º lagging for L-circuits
  P := 90; XP := 2;
end;

{ Pure capacitor (C) circuit }

procedure CircuitC(var V, I, F, C, XC, Z, P: Real; var XP: Integer);

var
  CF: Real;

begin
  CF := C * 1E-6;                                                                        // transform capacitance from µF to F
  if CF = 0 then begin
    // If C not given, calculate XC and C from V and I
    XC := V / I;
    CF := 1 / (2 * 3.14 * F * XC);
    C := CF * 1E+6;
  end
  else
    // If C given, calculate XC from C
    XC := 1 / (2 * 3.14 * F * CF);
  // Impedance is equal to XC
  Z := XC;
  // If V or I not given, calculate it
  if V = 0 then
    V := I * Z
  else if I = 0 then
    I := V / Z;
  // Phase angle = 90º leading for C-circuits
  P := 90; XP := 1;
end;

{ Resistor-inductor (RL) circuit }

procedure CircuitRL(var V, I, F, R, L, XL, Z, P: Real; var XP: Integer);

begin
  if (V = 0) or (I = 0) then begin
    // If voltage or current not given, R and L are both given and Z may be calculated from R and L
    XL := 2 * 3.14 * F * L;
    Z := Sqrt(Sqr(R) + Sqr(XL));
    if V = 0 then
      V := I * Z
    else
      I := V / Z;
  end
  else begin
    // If voltage and current given, Z may be calculated from V and I
    Z := V / I;
    if R = 0 then begin
      // If R not given, L is given; XL may be calculated from L; R may be calculated from Z and XL
      XL := 2 * 3.14 * F * L;
      if Z > XL then
        // Z < XL would give negative square root!
        R := Sqrt(Sqr(Z) - Sqr(XL))
      else
        // If Z < XL, setting Z = -1 will result in an unsatisfied condition of the random values generation loop;
        // generation will be restarted until R/L values will be valid...
        Z := -1;
    end
    else if L = 0 then begin
      // If L not given, R is given; XL may be calculated from Z and R; L may be calculated from XL
      if Z > R then begin
        // Z < R would give negative square root!
        XL := Sqrt(Sqr(Z) - Sqr(R));
        L := XL / (2 * 3.14 * F);
      end
      else
        // If Z < R, setting Z = -1 will result in an unsatisfied condition of the random values generation loop;
        // generation will be restarted until R/Z values will be valid...
        Z := -1;
    end;
  end;
  if Z <> -1 then begin
    // Calculate (lagging) phase angle
    P := RadToDeg(Arctan(XL / R)); XP := 2;
  end;
end;

{ Resistor-capacitor (RC) circuit }

procedure CircuitRC(var V, I, F, R, C, XC, Z, P: Real; var XP: Integer);

var
  CF: Real;

begin
  CF := C * 1E-6;                                                                        // transform capacitance from µF to F
  if (V = 0) or (I = 0) then begin
    // If voltage or current not given, R and C are both given and Z may be calculated from R and C
    XC := 1 / (2 * 3.14 * F * CF);
    Z := Sqrt(Sqr(R) + Sqr(XC));
    if V = 0 then
      V := I * Z
    else
      I := V / Z;
  end
  else begin
    // If voltage and current given, Z may be calculated from V and I
    Z := V / I;
    if R = 0 then begin
      // If R not given, L is given; XC may be calculated from C; R may be calculated from Z and XC
      XC := 1 / (2 * 3.14 * F * CF);
      if Z > XC then
        // Z < XC would give negative square root!
        R := Sqrt(Sqr(Z) - Sqr(XC))
      else
        // If Z < XC, setting Z = -1 will result in an unsatisfied condition of the random values generation loop;
        // generation will be restarted until R/C values will be valid...
        Z := -1;
    end
    else if C = 0 then begin
      // If C not given, R is given; XC may be calculated from Z and R; C may be calculated from XC
      if Z > R then begin
        // Z < R would give negative square root!
        XC := Sqrt(Sqr(Z) - Sqr(R));
        CF := 1 / (2 * 3.14 * F * XC);
        C := CF * 1E+6;
      end
      else
        // If Z < R, setting Z = -1 will result in an unsatisfied condition of the random values generation loop;
        // generation will be restarted until R/Z values will be valid...
        Z := -1;
    end;
  end;
  if Z <> -1 then begin
    // Calculate (leading) phase angle
    P := RadToDeg(Arctan(XC / R)); XP := 1;
  end;
end;

{ Series RLC circuits }

procedure CircuitSeriesRLC(var V, I, F, R, L, C, XL, XC, Z, P: Real; var XP: Integer);

var
  CF: Real;

begin
  CF := C * 1E-6;                                                                        // transform capacitance from µF to F
  if (V = 0) or (I = 0) then begin
    // If voltage or current not given, R, L and C are given and Z may be calculated from R, L and C
    XL := 2 * 3.14 * F * L;
    XC := 1 / (2 * 3.14 * F * CF);
    Z := Sqrt(Sqr(R) + Sqr(XL - XC));
    if V = 0 then
      V := I * Z
    else
      I := V / Z;
  end
  else begin
    // If voltage and current given, Z may be calculated from V and I
    Z := V / I;
    if R = 0 then begin
      // If R not given, L and C are given; XL and XC may be calculated from L resp. C; R may be calculated from Z and XL and XC
      XL := 2 * 3.14 * F * L;
      XC := 1 / (2 * 3.14 * F * CF);
      if Sqr(Z) > Sqr(XL - XC) then
        // Z^2 < (XL - XC)^2 would give negative square root!
        R := Sqrt(Sqr(Z) - Sqr(XL - XC))
      else
        // If Z^2 < (XL - XC)^2, setting Z = -1 will result in an unsatisfied condition of the random values generation loop;
        // generation will be restarted until R/L/C values will be valid...
        Z := -1;
    end
    else if L = 0 then begin
      // Question type not implemented in this version (L always given)
    end
    else if C = 0 then begin
      // Question type not implemented in this version (C always given)
    end;
  end;
  if Z <> -1 then begin
    // Calculate phase angle and determine if current leads or lags vs. voltage
    P := RadToDeg(Arctan((XL - XC) / R));
    if P < 0 then
      XP := 1
    else
      XP := 2;
  end;
end;

{ Parallel RLC circuits }

procedure CircuitParallelRLC(var V, I, F, R, L, C, XL, XC, Z, P: Real; var XP: Integer);

var
  CF, Y, G, B, BL, BC: Real;

begin
  CF := C * 1E-6;                                                                        // transform capacitance from µF to F
  if (V = 0) or (I = 0) then begin
    // If voltage or current not given, R, L and C are given and Z may be calculated from R, L and C
    XL := 2 * 3.14 * F * L; BL := 1 / XL;
    XC := 1 / (2 * 3.14 * F * CF); BC := 1 / XC;
    B := BL - BC; G := 1 / R;
    Y := Sqrt(Sqr(1 / R) + Sqr(BL - BC)); Z := 1 / Y;
    if V = 0 then
      V := I * Z
    else
      I := V / Z;
  end
  else begin
      // If voltage and current given, Z may be calculated from V and I
    Z := V / I; Y := 1 / Z;
    if R = 0 then begin
      // If R not given, L and C are given; XL and XC may be calculated from L resp. C; R may be calculated from Z and XL and XC
      XL := 2 * 3.14 * F * L; BL := 1 / XL;
      XC := 1 / (2 * 3.14 * F * CF); BC := 1 / XC;
      B := BL - BC;
      if Sqr(Y) > Sqr(BL - BC) then begin
        // Y^2 < (BL - BC)^2 would give negative square root!
        G := Sqrt(Sqr(Y) - Sqr(BL - BC)); R := 1 / G;
      end
      else
        // If Y^2 < (BL - BC)^2, setting Z = -1 will result in an unsatisfied condition of the random values generation loop;
        // generation will be restarted until R/L/C values will be valid...
        Z := -1;
    end
    else if L = 0 then begin
      // Question type not implemented in this version (L always given)
    end
    else if C = 0 then begin
      // Question type not implemented in this version (C always given)
    end;
  end;
  if Z <> -1 then begin
    // Calculate phase angle and determine if current leads or lags vs. voltage
    P := RadToDeg(Arctan(B / G));
    if P < 0 then
      XP := 1
    else
      XP := 2;
  end;
end;

{**************}
{ TfACircuits1 }
{**************}

{ Application start: Initialisation }

procedure TfACircuits1.FormCreate(Sender: TObject);

var
  J: Integer;

begin
  // Put form input fields into an array
  aInputFields[1] := edVS;  aInputFields[2] := edF;   aInputFields[3] := edT;  aInputFields[4] := edR;   aInputFields[5] := edL;
  aInputFields[6] := edC;   aInputFields[7] := edXL;  aInputFields[8] := edXC;  aInputFields[9] := edZ;
  aInputFields[10] := edG;  aInputFields[11] := edBL; aInputFields[12] := edBC; aInputFields[13] := edY;
  aInputFields[14] := edVR; aInputFields[15] := edVL; aInputFields[16] := edVC; aInputFields[17] := edV;
  aInputFields[18] := edIR; aInputFields[19] := edIL; aInputFields[20] := edIC; aInputFields[21] := edI; aInputFields[22] := edP;
  // Use all 7 types of circuit
  for J := 1 to 7 do
    aCircuitsUsed[J] := True;
  // Init and start test (with default settings)
  Randomize;
  bAppStart := True;
  NewTest(iQuestion, iCorrect);
end;

{ Menu item "Test > New": Start a new test }

procedure TfACircuits1.mTestNewClick(Sender: TObject);

begin
  NewTest(iQuestion, iCorrect);
end;

{ Menu item "Test > Exit": Exit the application }

procedure TfACircuits1.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Circuits > R circuits": (Un)select R circuits for test questions }

procedure TfACircuits1.mCircuitsRClick(Sender: TObject);

begin
  if mCircuitsR.Checked then
    mCircuitsR.Checked := False
  else
    mCircuitsR.Checked := True;
  aCircuitsUsed[1] := mCircuitsR.Checked;
end;

{ Menu item "Circuits > L circuits": (Un)select L circuits for test questions }

procedure TfACircuits1.mCircuitsLClick(Sender: TObject);

begin
  if mCircuitsL.Checked then
    mCircuitsL.Checked := False
  else
    mCircuitsL.Checked := True;
  aCircuitsUsed[2] := mCircuitsL.Checked;
end;

{ Menu item "Circuits > C circuits": (Un)select C circuits for test questions }

procedure TfACircuits1.mCircuitsCClick(Sender: TObject);

begin
  if mCircuitsC.Checked then
    mCircuitsC.Checked := False
  else
    mCircuitsC.Checked := True;
  aCircuitsUsed[3] := mCircuitsC.Checked;
end;

{ Menu item "Circuits > RL circuits": (Un)select RL circuits for test questions }

procedure TfACircuits1.mCircuitsRLClick(Sender: TObject);

begin
  if mCircuitsRL.Checked then
    mCircuitsRL.Checked := False
  else
    mCircuitsRL.Checked := True;
  aCircuitsUsed[4] := mCircuitsRL.Checked;
end;

{ Menu item "Circuits > RC circuits": (Un)select RC circuits for test questions }

procedure TfACircuits1.mCircuitsRCClick(Sender: TObject);

begin
  if mCircuitsRC.Checked then
    mCircuitsRC.Checked := False
  else
    mCircuitsRC.Checked := True;
  aCircuitsUsed[5] := mCircuitsRC.Checked;
end;

{ Menu item "Circuits > Series RLC": (Un)select series RLC circuits for test questions }

procedure TfACircuits1.mCircuitsRLC1Click(Sender: TObject);

begin
  if mCircuitsRLC1.Checked then
    mCircuitsRLC1.Checked := False
  else
    mCircuitsRLC1.Checked := True;
  aCircuitsUsed[6] := mCircuitsRLC1.Checked;
end;

{ Menu item "Circuits > Parallel RLC": (Un)select parallel RLC circuits for test questions }

procedure TfACircuits1.mCircuitsRLC2Click(Sender: TObject);

begin
  if mCircuitsRLC2.Checked then
    mCircuitsRLC2.Checked := False
  else
    mCircuitsRLC2.Checked := True;
  aCircuitsUsed[7] := mCircuitsRLC2.Checked;
end;

{ Menu item "Options > View results": Adapt form controls depending on test/view mode }

procedure TfACircuits1.mOptionsViewClick(Sender: TObject);

begin
  // Change to test mode
  if mOptionsView.Checked then begin
    mOptionsView.Checked := False;
    // Proceed with next exercise as test
    if btStart.Caption = 'View' then
      btStart.Caption := 'Next';
  end
  // Change to view mode
  else begin
    mOptionsView.Checked := True;
    // Clear evaluation values on the form (but keep actual values)
    edCorrect.Clear; edSuccess.Clear;
    edSuccess.Color := clDefault;
    // Setting the button to "View" will allow to view the results of the actual (unsolved) exercise
    if btStart.Caption = 'Check' then
      btStart.Caption := 'View';
  end;
end;

{ Menu item "Help > Electronics help": Display RLC circuits help }

procedure TfACircuits1.mHelpElectronicsClick(Sender: TObject);

begin
  fHelp.Caption := 'RLC circuits - Electronics Help.';
  fHelp.HelpText.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display program usage help }

procedure TfACircuits1.mHelpProgramClick(Sender: TObject);

begin
  fHelp.Caption := 'RLC circuits - Program Help.';
  fHelp.HelpText.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program info }

procedure TfACircuits1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'RLC circuits: Electronics exercice generator.' + LineEnding;
  S += 'Version 1.0, © allu, January 2019.';
  MessageDlg('About "ACircuits1"', S, mtInformation, [mbOK], 0);
end;

{ Pushbutton pressed: Main program action, depending on actual situation (button caption) }

procedure TfACircuits1.btStartClick(Sender: TObject);

var
  CType, QType, QType2, C, J, JF: Integer;
  Omega, UValue, P: Real;
  Filename, S: string;
  AllComponents, OK: Boolean;
  RLCComponents: array[0..2] of Boolean;

begin
  // At least one circuit type must be selected
  OK := False;
  for J := 1 to 7 do
    if aCircuitsUsed[J] then
      OK := True;
  if OK then begin
    // Button "Start"/"Next": Generate a new exercise
    if (btStart.Caption = 'Start') or (btStart.Caption = 'Next') then begin
      // Circuit type (R, C, laL, RC, RL, series RLC, parallel RLC)
      repeat
        CType := Random(7) + 1;
      until aCircuitsUsed[CType];                                                        // circuit type must have been selected to be used
      S := 'RLC circuirs: ' + Circuits[CType] + ' circuit.'; Title.Caption := S;
      FieldsReset;                                                                       // clear form input values
      FieldsEnable(CType);                                                               // enable fields for actual circuit
      // Values given by program
      QType := Random(4);
      repeat                                                                             // continue generating random values until all values are valid and as wanted
        // Reset all values
        rVS := 0;
        rR := 0; rL := 0; rC := 0;
        rXL := 0; rXC := 0; rZ := 0;
        rG := 0; rBL := 0; rBC := 0; rY := 0;
        rVR := 0; rVL := 0; rVC := 0; rV := 0;
        rIR := 0; rIL := 0; rIC := 0; rI := 0;
        rP := 0; cobP.ItemIndex := 0;
        // Reset array of circuit values and values given by the program
        for J := 1 to 22 do begin
          aCircuitValues[J] := 0;
          aProgramValues[J] := False;
        end;
        // Reset field colors for values potentially given by the program
        fACircuits1.edVS.Color := clDefault;
        fACircuits1.edR.Color  := clDefault; fACircuits1.edL.Color := clDefault; fACircuits1.edC.Color := clDefault;
        fACircuits1.edI.Color  := clDefault;
        if QType = 1 then begin
          // Voltage given as sin function: ω given (f and T to be calculated)
          repeat
            rF := Frequency;
            Omega := Round(2 * 3.14 * rF);
            rF := Omega / (2 * 3.14); rT := 1 / rF;
          until rF = Int(rF);
        end
        else begin
          // Voltage given as sin function: f or T given
          if Random(4) = 0 then begin
            // Period given (f to be calculated)
            rT := Period; rF := 1 / rT;
            edT.ReadOnly := True; edT.Color := clCream; edT.TabStop := False;
            aProgramValues[3] := True;
          end
          else begin
            // Frequency given (T to be calculated)
            rF := Frequency; rT := 1 / rF;
            edF.ReadOnly := True; edF.Color := clCream; edF.TabStop := False;
            aProgramValues[2] := True;
          end;
        end;
        if QType = 0 then begin
          // Current given (and all components given; voltage to be calculated)
          rI := Current;
          edI.ReadOnly := True; edI.Color := clCream; edI.TabStop := False;
          aProgramValues[21] := True;
          AllComponents := True;                                                         // all components values have to be given by the program
          sV := '';
        end
        else begin
          // Voltage given
          rVS := Voltage;
          edVS.ReadOnly := True; edVS.Color := clCream; edVS.TabStop := False;
          aProgramValues[1] := True;
          if QType = 1 then begin
            // Voltage given as sin function (VS = peak value)
            rV := 0.707 * rVS;
            sV := RFormat(rVS) + '·sin(' + RFormat(Omega) + 't)';
          end
          else begin
            // Voltgae given as number value (VS = RMS value)
            rV := rVS;
            sV := RFormat(rVS);
          end;
          QType2 := Random(2);
          // Voltage given with or without current given
          if QType2 = 0 then begin
            // Current given
            rI := Current;
            edI.ReadOnly := True; edI.Color := clCream; edI.TabStop := False;
            aProgramValues[21] := True;
            AllComponents := False;                                                      // as V and I given, 1 component value may be calculated by the user
          end
          else begin
            // Current not given
            AllComponents := True;                                                       // all components values have to be given by the program
          end;
        end;
        // Determine components of actual circuit
        for J := 0 to 2 do
          RLCComponents[J] := False;
        case CType of
             1: RLCComponents[0] := True;
             2: RLCComponents[1] := True;
             3: RLCComponents[2] := True;
             4: begin RLCComponents[0] := True; RLCComponents[1] := True; end;
             5: begin RLCComponents[0] := True; RLCComponents[2] := True; end;
          6..7: begin RLCComponents[0] := True; RLCComponents[1] := True; RLCComponents[2] := True; end;
        end;
        // If not all components values are given by the program (i.e. when V and I are given),
        // randomly select the one not to give (the one, the user has to calculate)
        if not AllComponents then begin
          // For all three components RLC circuits, always give C and L (R to calculate)
          // Exercises with L or C not given (needing to resolve 2nd degree equation) previewed for later version
          if CType in [6, 7] then
            C := 0
          else begin
            repeat
              C := Random(3);
            until RLCComponents[C];                                                      // the value to be calculated must of course be one of the actual circuit's components
          end;
          RLCComponents[C] := False;                                                     // this component value will not be given (will have to be calculated)
        end;
        // Randomly choose values for the components to be given by the program
        if RLCComponents[0] then begin
          // Random resistance value
          rR := Resistance;
          edR.ReadOnly := True; edR.Color := clCream; edR.TabStop := False;
          aProgramValues[4] := True;
        end;
        if RLCComponents[1] then begin
          // Random inductance value
          rL := Inductance;
          edL.ReadOnly := True; edL.Color := clCream; edL.TabStop := False;
          aProgramValues[5] := True;
        end;
        if RLCComponents[2] then begin
          // Random capacitance value
          rC := Capacitance;
          edC.ReadOnly := True; edC.Color := clCream; edC.TabStop := False;
          aProgramValues[6] := True;
        end;
        // Calculate (not given) values of actual circuit
        case CType of
          1: CircuitR(rV, rI, rR, rZ, rP, iP);
          2: CircuitL(rV, rI, rF, rL, rXL, rZ, rP, iP);
          3: CircuitC(rV, rI, rF, rC, rXC, rZ, rP, iP);
          4: CircuitRL(rV, rI, rF, rR, rL, rXL, rZ, rP, iP);
          5: CircuitRC(rV, rI, rF, rR, rC, rXC, rZ, rP, iP);
          6: CircuitSeriesRLC(rV, rI, rF, rR, rL, rC, rXL, rXC, rZ, rP, iP);
          7: CircuitParallelRLC(rV, rI, rF, rR, rL, rC, rXL, rXC, rZ, rP, iP);
        end;
        // Set source voltage value if not already set
        if rVS = 0 then
          rVS := rV;
        // Calculate conductance, inductive and capacitive susceptance and admittance
        if edR.Enabled and (rR <> 0) then
          rG := 1 / rR;
        if edL.Enabled and (rXL <> 0) then
          rBL := 1 / rXL;
        if edC.Enabled and (rXC <> 0) then
          rBC := 1 / rXC;
        rY := 1 / rZ;
        // Calculate component voltages and currents
        if CType = 7 then begin
          // Parallel RLC circuit (all voltages equal; different currents)
          if edR.Enabled and (rR <> 0) then begin
            rVR := rV; rIR := rVR / rR;
          end;
          if edL.Enabled and (rXL <> 0) then begin
            rVL := rV; rIL := rVL / rXL;
          end;
          if edC.Enabled and (rXC <> 0) then begin
            rVC := rV; rIC := rVC / rXC;
          end;
        end
        else begin
          // All other circuits are series circuits (all currents equal; different voltages)
          if edR.Enabled then begin
            rIR := rI; rVR := rR * rIR;
          end;
          if edL.Enabled then begin
            rIL := rI; rVL := rXL * rIL;
          end;
          if edC.Enabled then begin
            rIC := rI; rVC := rXC * rIC;
          end;
        end;
      // This ends the random values generation loop; values have to be valid (cf. circuit calculation routines)
      // and should also be within reasonable limits (somewhat less limited values as those chosen in the random values routines)
      until (rV >= 10) and (rV <= 300) and (rI >= 0.1) and (rI <= 4) and (rZ >= 1) and (rZ <= 10000);
      // Fill the circuit values array
      aCircuitValues[1] := rVS; aCircuitValues[2] := rF; aCircuitValues[3] := rT;
      aCircuitValues[4] := rR; aCircuitValues[5] := rL; aCircuitValues[6] := rC;
      aCircuitValues[7] := rXL; aCircuitValues[8] := rXC; aCircuitValues[9] := rZ;
      aCircuitValues[10] := rG; aCircuitValues[11] := rBL; aCircuitValues[12] := rBC; aCircuitValues[13] := rY;
      aCircuitValues[14] := rVR; aCircuitValues[15] := rVL; aCircuitValues[16] := rVC; aCircuitValues[17] := rV;
      aCircuitValues[18] := rIR; aCircuitValues[19] := rIL; aCircuitValues[20] := rIC; aCircuitValues[21] := rI;
      aCircuitValues[22] := rP;
      // Draw the circuit (= load circuit file)
      Filename := './circuits/' + CircuitFiles[CType] + '.jpg';
      DoDirSeparators(Filename);
      imCircuit.Picture.LoadFromFile(Filename);
      // Take action, depending on being in 'test' or 'view' mode
      if mOptionsView.Checked then begin
        // View mode: Show all values
        if sV = '' then
          // Be sure the voltage string variable is set
          sV := RFormat(rVS);
        FieldsFill(sV, aCircuitValues, aProgramValues, aInputFields);                    // fill edit fields (all values)
        cobP.ItemIndex := iP;                                                            // set the correct current/voltage wave relationship
        btStart.Caption := 'Next';                                                       // next button push will be another (all resolved) exercise
        btStart.SetFocus;
      end
      else begin
        FieldsFill(sV, aCircuitValues, aProgramValues, aInputFields);                    // fill edit fields (values given)
        if bAppStart then
          // At application start, do nothing (to avoid "cant't focus" error)
          bAppStart := False
        else begin
          // Set focus to edit field of first not given value
          JF := 0; J := 0;
          repeat
            Inc(J);
            if aInputFields[J].Enabled and not aProgramValues[J] then begin
              aInputFields[J].SetFocus;
              JF := J;
            end;
          until (JF <> 0);
        end;
        btStart.Caption := 'Check';                                                      // next button push will be to check user's answers
      end;
    end
    else if btStart.Caption = 'View' then begin
      // Button 'View': Show results of current exercise
      FieldsFill(sV, aCircuitValues, aProgramValues, aInputFields);                      // fill edit fields (all values)
      cobP.ItemIndex := iP;                                                              // set the correct current/voltage wave relationship
      // Set button caption and focus (for next exercise)
      btStart.Caption := 'Next';
      btStart.SetFocus;
    end
    else begin
      // Check user's answer
      Inc(iQuestion);
      OK := True;
      for J := 1 to 22 do begin
        // If one of the values on the form differs from those in the array, the global answer is false
        if aInputFields[J].Enabled and not aProgramValues[J] then begin
          if aInputFields[J].Text = '' then
            // Empty field => global answer is false
            OK := False
          else
            // Get user value from form
            UValue := StrToFloat(aInputFields[J].Text);
          // Compare user values with array values rounded to 4 decimal digits
          if UValue <> Round(10000 * aCircuitValues[J]) / 10000 then begin
            // False answer for this value
            aInputFields[J].Text := RFormat(aCircuitValues[J]);                        // display correct answer
            aInputFields[J].Font.Color := clRed;                                       // use red font color to indicate there was a false answer
            OK := False;                                                               // global answer is false
          end;
        end;
      end;
      // Also check the current/voltage wave relationship
      if cobP.Items[cobP.ItemIndex] <> cobP.Items[iP] then begin
        OK := False;                                                                     // global answer is false
        cobP.ItemIndex := iP;                                                            // display correct answer
        shP.Visible := True;                                                             // as red font not possible (?), display a red 'underline' by making shape visible
      end;
      // Display global result (evaluation)
      if OK then begin
        // Global answer = correct
        Inc(iCorrect);
        imEval.Picture.LoadFromFile('correct.png');
      end
      else
        // Global answer = false
        imEval.Picture.LoadFromFile('false.png');
      edQuestions.Text := IntToStr(iQuestion); edCorrect.Text := IntToStr(iCorrect);
      // Success percentage
      P := Round(100 * (100 * iCorrect / iQuestion)) / 100;
      edSuccess.Text := FloatToStr(P) + '%';
      // Use different colors depending on success percenage
      if P < 50 then
        edSuccess.Color := clRed
      else if P < 60 then
        edSuccess.Color := clYellow
      else
        edSuccess.Color := clLime;
      // Set button caption and focus (for next exercise)
      btStart.Caption := 'Next';
      btStart.SetFocus;
    end;
  end
  // No circuit type selected: Display error message
  else
    MessageDlg('Invalid settings','No circuit type selected!', mtError, [mbOK], 0);
end;

end.

