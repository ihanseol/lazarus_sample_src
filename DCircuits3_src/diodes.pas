{****************************************}
{* Main unit for DCircuits3 application *}
{****************************************}

unit diodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, data, help;

type
  {**********}
  { TfDiodes }
  {**********}
  TfDiodes = class(TForm)
    mMenu: TMainMenu;
    mCircuit, mCircuitCalc, mCircuitTest, mCircuitExit: TMenuItem;
    mSettings, mSettingsModel, mSettingsModelIdeal, mSettingsModelPractical, mSettingsModelComplete: TMenuItem;
    mSettingsCircuits, mSettingsCircuits1, mSettingsCircuits2, mSettingsCircuits3, mSettingsCircuits4, mSettingsCircuits5: TMenuItem;
    mHelp, mHelpElectronics, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imCircuit: TImage;
    lacircuit, laDiode, laVS, laVS2, laVLamp, laILamp, laR1, laR2, laComplete: TLabel;
    laVD, laVR, laID, laIR, laPZ, laIDVS1, laITVS1: TLabel;
    laUVS, laUVS2, laUVlamp, laUILamp, laUR1, laUR2, laUComplete, laUVD, laUVR: TLabel;
    laUID, laUIR, laUPZ, laUIDVS1, laUITVS1: TLabel;
    edVS, edVS2, edVLamp, edILamp, edR1, edR2, edComplete: TEdit;
    edPZ, edVD, edID, edVR, edIR, edIDVS1, edITVS1: TEdit;
    edCircuit: TMemo;
    imEval: TImage;
    btAction: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mCircuitCalcClick(Sender: TObject);
    procedure mCircuitTestClick(Sender: TObject);
    procedure mCircuitExitClick(Sender: TObject);
    procedure mSettingsModelIdealClick(Sender: TObject);
    procedure mSettingsModelPracticalClick(Sender: TObject);
    procedure mSettingsModelCompleteClick(Sender: TObject);
    procedure mSettingsCircuits1Click(Sender: TObject);
    procedure mSettingsCircuits2Click(Sender: TObject);
    procedure mSettingsCircuits3Click(Sender: TObject);
    procedure mSettingsCircuits4Click(Sender: TObject);
    procedure mSettingsCircuits5Click(Sender: TObject);
    procedure mHelpElectronicsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
  private
    iCircuit, iModel: Integer;
    rVS, rPIV, rVS1, rVS2, rR1, rR2, rVL, rIL, rRfwd, rIrev, rIDVS1, rIDVS2, rVD, rVR, rID, rIR, rPZ, rITVS1, rITVS2: Real;
    sAction, sDiodeMaterial: string;
    bCircuits: array[1..5] of Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  Circuits: array[1..5] of string = (
    'Forward-biased diode circuit (1 resistor)', 'Forward-biased diode circuit (2 resistors)',
    'Reversed-biased diode circuit', 'Zener diode circuit', 'Diode breakdown voltage (PIV)'
  );
  Details: array[1..5] of string = (
    'Determination of the forward voltage and the forward current of a forward-biased diode circuit',
    'Determination of the forward voltage and the forward current of a forward-biased diode circuit (with parallel resistor)',
    'Determination of the reverse voltage and the reverse current of a reverse-biased diode circuit',
    'Determination of a suitable resistance and a suitable zener diode in order to keep the voltage across a lamp constant and avoid diode burn out',
    'Determination of a suitable resistance to avoid diode (with given PIV) to break down'
  );
  Models: array[1..3] of string = (
    'ideal', 'practical', 'complete'
  );

var
  fDiodes: TfDiodes;

implementation

{$R *.lfm}

{ Clear form (reset all entry fields) }

procedure ClearForm(out VS, R1, R2, VS2, VL, IL, IDVS1, RFwd, IRev: Real);

begin
  fDiodes.edVS.Text := ''; fDiodes.edVS2.Text := '';
  fDiodes.edR1.Text := ''; fDiodes.edR2.Text := '';
  fDiodes.edVLamp.Text := ''; fDiodes.edILamp.Text := '';
  fDiodes.edComplete.Text := ''; fDiodes.edIDVS1.Text := '';
  VS := 0; R1 := 0; R2 := 0; VS2 := 0;
  VL := 0; IL := 0; IDVS1 := 0; RFwd := 0; IRev := 0;
  fDiodes.edID.Text := ''; fDiodes.edIR.Text := '';
  fDiodes.edVD.Text := ''; fDiodes.edVR.Text := '';
  fDiodes.edIDVS1.Text := ''; fDiodes.edITVS1.Text := ''; fDiodes.edPZ.Text := '';
  fDiodes.imEval.Picture.Clear;
end;

{ Form field settings, depending on the diode model actually used }

procedure DiodeModelInit(Circuit, Model: Integer);

var
  S: string;

begin
  S := Details[Circuit] + '.';
  if (Circuit <> 4) and (Circuit <> 5) then
    S += ' Calculations done, using the ' + Models[Model] + ' diode model.';
  fDiodes.edCircuit.Clear; fDiodes.edCircuit.Lines.AddText(S);
  fDiodes.laComplete.Visible := False; fDiodes.edComplete.Visible := False; fDiodes.laUComplete.Visible := False;
  if (Circuit in [1..3]) and (Model = 3) then begin
    // Complete diode model
    fDiodes.laComplete.Visible := True; fDiodes.edComplete.Visible := True; fDiodes.laUComplete.Visible := True;
    if Circuit = 3 then begin
      fDiodes.laComplete.Caption := 'Diode reverse current'; fDiodes.laUComplete.Caption := 'µA';
    end
    else begin
      fDiodes.laComplete.Caption := 'Diode dyn. forw. resistance'; fDiodes.laUComplete.Caption := 'Ω';
    end;
  end;
end;

{ Form field initialisation (at start of calculation or exercise ) }

procedure CircuitInit(Action: string; Circuit, Model: Integer; DiodeMaterial: string);

var
  Img: Integer;
  S: string;
  RO1, RO2: Boolean;

begin
  fDiodes.lacircuit.Caption := Circuits[Circuit] + '.';
  if Circuit = 5 then
    Img := 3                                                                   // same circuit picture for breakdown as for reverse bias
  else
    Img := Circuit;
  S := 'd' + IntToStr(Img) + '.jpg';
  // Default field settings (will be adapted above as needed)
  fDiodes.imCircuit.Picture.LoadFromFile(S);                                   // display actual circuit picture
  fDiodes.laVS2.Visible      := False;  fDiodes.edVS2.Visible      := False;  fDiodes.laUVS2.Visible      := False;
  fDiodes.laR1.Top           := 212;    fDiodes.edR1.Top           := 208;    fDiodes.laUR1.Top           := 212;
  fDiodes.laR2.Visible       := False;  fDiodes.edR2.Visible       := False;  fDiodes.laUR2.Visible       := False;
  fDiodes.laR2.Caption := StringReplace(fDiodes.laR2.Caption, '2', SUB_2, []);
  fDiodes.laVLamp.Visible    := False;  fDiodes.edVLamp.Visible    := False;  fDiodes.laUVLamp.Visible    := False;
  fDiodes.laILamp.Visible    := False;  fDiodes.edILamp.Visible    := False;  fDiodes.laUILamp.Visible    := False;
  fDiodes.laIDVS1.Visible    := False;  fDiodes.edIDVS1.Visible    := False;  fDiodes.laUIDVS1.Visible    := False;
  fDiodes.laITVS1.Visible    := False;  fDiodes.edITVS1.Visible    := False;  fDiodes.laUITVS1.Visible    := False;
  fDiodes.laVD.Visible       := True;   fDiodes.edVD.Visible       := True;   fDiodes.laUVD.Visible       := True;
  fDiodes.laVR.Visible       := True;   fDiodes.edVR.Visible       := True;   fDiodes.laUVR.Visible       := True;
  fDiodes.laID.Top           := 329;    fDiodes.edID.Top           := 325;    fDiodes.laUID.Top     := 329;
  fDiodes.laIR.Visible       := True;   fDiodes.edIR.Visible       := True;   fDiodes.laUIR.Visible       := True;
  fDiodes.laPZ.Visible       := False;  fDiodes.edPZ.Visible       := False;  fDiodes.laUPZ.Visible       := False;
  fDiodes.laComplete.Visible := False;  fDiodes.edComplete.Visible := False;  fDiodes.laUComplete.Visible := False;
  fDiodes.laVS.Caption := 'Source voltage VS'; fDiodes.laR1.Caption := 'Limiting resistance R'; fDiodes.laR1.Font.Style := [];
  fDiodes.laVR.Caption := 'Resistance voltage VR'; fDiodes.laIR.Caption := 'Resistance current IR'; fDiodes.laUIR.Caption := 'mA';
  fDiodes.laID.Caption := 'Diode current ID'; fDiodes.laUID.Caption := 'mA';
  fDiodes.laID.Font.Style := [fsBold]; fDiodes.laVD.Font.Style := [fsBold];
  if Circuit in [1..3, 5] then begin
    // Non-Zener-diode circuits
    fDiodes.laDiode.Visible := True;
    fDiodes.laDiode.Caption := DiodeMaterial;
    fDiodes.laDiode.Left := 305; fDiodes.laDiode.Top := 264;
    if Circuit = 2 then begin
      // Reversed bias diode circuit
      fDiodes.laR2.Visible := True; fDiodes.edR2.Visible := True; fDiodes.laUR2.Visible := True;
      fDiodes.laDiode.Left := fDiodes.laDiode.Left + 45; fDiodes.laDiode.Top := fDiodes.laDiode.Top - 7;
      fDiodes.laR1.Caption := 'Resistance R1';
      fDiodes.laR1.Caption := StringReplace(fDiodes.laR1.Caption, '1', SUB_1, []);
      fDiodes.laVR.Caption := 'Resistance voltage VR1';
      fDiodes.laVR.Caption := StringReplace(fDiodes.laVR.Caption, '1', SUB_1, []);
      fDiodes.laIR.Caption := 'Resistance current IR1';
      fDiodes.laIR.Caption := StringReplace(fDiodes.laIR.Caption, '1', SUB_1, []);
    end
    else if Circuit = 5 then begin
      // Diode breakdown circuit
      fDiodes.laVS2.Visible := True;  fDiodes.laUVS2.Visible := True;  fDiodes.edVS2.Visible := True;
      fDiodes.laIR.Visible  := False; fDiodes.edIR.Visible   := False; fDiodes.laUIR.Visible := False;
      fDiodes.laR1.Top      := 329;   fDiodes.edR1.Top       := 325;   fDiodes.laUR1.Top     := 329;
      fDiodes.laID.Top      := 212;   fDiodes.edID.Top       := 208;   fDiodes.laUID.Top     := 212;
      fDiodes.laVS2.Caption := 'Breakdown voltage VD';
      fDiodes.laR1.Caption  := 'Min. resistance R'; fDiodes.laR1.Font.Style := [fsBold];
      fDiodes.laID.Caption := 'Max. circuit current'; fDiodes.laUID.Caption := 'A';
      fDiodes.laID.Font.Style := []; fDiodes.laVD.Font.Style := [];
    end;
  end
  else begin
    // Zener-diode circuit
    fDiodes.laVS2.Visible   := True;   fDiodes.edVS2.Visible   := True;   fDiodes.laUVS2.Visible   := True;
    fDiodes.laR1.Top        := 292;    fDiodes.edR1.Top        := 288;    fDiodes.laUR1.Top        := 292;
    fDiodes.laVLamp.Visible := True;   fDiodes.edVLamp.Visible := True;   fDiodes.laUVLamp.Visible := True;
    fDiodes.laILamp.Visible := True;   fDiodes.edILamp.Visible := True;   fDiodes.laUILamp.Visible := True;
    fDiodes.laIDVS1.Visible := True;   fDiodes.edIDVS1.Visible := True;   fDiodes.laUIDVS1.Visible := True;
    fDiodes.laIDVS1.Caption := StringReplace(fDiodes.laIDVS1.Caption, '1', SUB_1, []);
    fDiodes.laITVS1.Visible := True;   fDiodes.edITVS1.Visible := True;   fDiodes.laUITVS1.Visible := True;
    fDiodes.laITVS1.Caption := StringReplace(fDiodes.laITVS1.Caption, '1', SUB_1, []);
    fDiodes.laVD.Visible    := False;  fDiodes.edVD.Visible    := False;  fDiodes.laUVD.Visible    := False;
    fDiodes.laVR.Visible    := False;  fDiodes.edVR.Visible    := False;  fDiodes.laUVR.Visible    := False;
    fDiodes.laPZ.Visible    := True;   fDiodes.edPZ.Visible    := True;   fDiodes.laUPZ.Visible    := True;
    fDiodes.laDiode.Visible := False;
    fDiodes.laVS.Caption  := 'Min. source voltage VS1';
    fDiodes.laVS.Caption := StringReplace(fDiodes.laVS.Caption, '1', SUB_1, []);
    fDiodes.laVS2.Caption := 'Max. source voltage VS2';
    fDiodes.laVS2.Caption := StringReplace(fDiodes.laVS2.Caption, '2', SUB_2, []);
    fDiodes.laR1.Caption  := 'Resistance R'; fDiodes.laR1.Font.Style := [fsBold];
    fDiodes.laIR.Caption  := 'Total current IT (VS=VS2)';
    fDiodes.laIR.Caption := StringReplace(fDiodes.laIR.Caption, '2', SUB_2, []);
    fDiodes.laID.Caption  := 'Diode current IZ (VS=VS2)'; fDiodes.laID.Font.Style := [];
    fDiodes.laID.Caption := StringReplace(fDiodes.laID.Caption, '2', SUB_2, []);
    fDiodes.laUID.Caption := 'A'; fDiodes.laUIR.Caption := 'A';
  end;
  DiodeModelInit(Circuit, Model);                                              // make modifications that depend on actual diode model
  if (Action = 'start') or (Action = 'calc') then begin
    RO1 := False; RO2 := True;
  end
  else begin
    RO1 := True; RO2 := False;
  end;
  // Give or not user the possibility to edit the different fields
  fDiodes.edVS.ReadOnly := RO1; fDiodes.edVS.TabStop := not RO1;
  fDiodes.edVR.ReadOnly := RO2; fDiodes.edVR.TabStop := not RO2;
  fDiodes.edID.ReadOnly := RO2; fDiodes.edID.TabStop := not RO2;
  if fDiodes.edID.TabStop then
    fDiodes.edID.TabOrder := 11;
  fDiodes.edIR.ReadOnly := RO2; fDiodes.edIR.TabStop := not RO2;
  if Circuit = 4 then begin
    // Zener-diode circuit
    fDiodes.edR1.ReadOnly := RO2; fDiodes.edR1.TabStop := not RO2;
    fDiodes.edVS2.ReadOnly := RO1; fDiodes.edVS2.TabStop := not RO1;
    fDiodes.edVLamp.ReadOnly := RO1; fDiodes.edVLamp.TabStop := not RO1;
    fDiodes.edILamp.ReadOnly := RO1; fDiodes.edILamp.TabStop := not RO1;
    fDiodes.edIDVS1.ReadOnly := RO1; fDiodes.edIDVS1.TabStop := not RO1;
    fDiodes.edITVS1.ReadOnly := RO2; fDiodes.edITVS1.TabStop := not RO2;
    fDiodes.edPZ.ReadOnly := RO2; fDiodes.edPZ.TabStop := not RO2;
  end
  else begin
    // Non-Zener-diode circuit
    fDiodes.edR1.ReadOnly := RO1; fDiodes.edR1.TabStop := not RO1;
    fDiodes.edR1.TabOrder := 2;
    fDiodes.edVD.ReadOnly := RO2; fDiodes.edVD.TabStop := not RO2;
    if Circuit = 2 then begin
      fDiodes.edR1.ReadOnly := RO1; fDiodes.edR1.TabStop := not RO1;
    end
    else if Circuit = 5 then begin
      // Diode breakdown circuit
      fDiodes.edVS2.ReadOnly := RO1; fDiodes.edVS2.TabStop := not RO1;
      fDiodes.edID.ReadOnly := RO1; fDiodes.edID.TabStop := not RO1;
      if fDiodes.edID.TabStop then
        fDiodes.edID.TabOrder := 5;
      fDiodes.edR1.ReadOnly := RO2; fDiodes.edR1.TabStop := not RO2;
      if fDiodes.edR1.TabStop then
        fDiodes.edR1.TabOrder := 10;
    end;
    if Model = 3 then begin
      // Complete diode model
      fDiodes.edComplete.ReadOnly := RO1; fDiodes.edComplete.TabStop := not RO1;
    end;
  end;
end;

{ Forward biased diode circuit calculations }
{ ----------------------------------------- }

procedure CircuitForward1(Model: Integer; Material: string; VS, R, Rfwd: Real; out VD, VR, ID, IR: Real);

var
  VD1: Real;

begin
  if Model = 1 then begin
    // Ideal diode model
    VD1 := 0;
  end
  else begin
    // Non-ideal diode models
    if Material = 'Si' then
      VD1 := 0.7
    else
      VD1 := 0.3;
  end;
  // Ideal or practical diode models calculations
  if (Model = 1) or (Model = 2) then begin
    VD := VD1;
    ID := (VS - VD) / R;
    IR := ID;
    VR := IR * R;
  end
  // Complete diode model calculations
  else begin
    ID := (VS - VD1) / (R + Rfwd);
    VD := VD1 + ID * Rfwd;
    IR := ID;
    VR := IR * R;
  end;
end;

{ Forward biased diode with parallel resistor circuit calculations }
{ ---------------------------------------------------------------- }

procedure CircuitForward2(Model: Integer; Material: string; VS, R1, R2, Rfwd: Real; out VD, VR, ID, IR: Real);

var
  VD1, I2, IT, R: Real;

begin
  if Model = 1 then begin
    // Ideal diode model
    VD1 := 0;
  end
  else begin
    // Non-ideal diode model
    if Material = 'Si' then
      VD1 := 0.7
    else
      VD1 := 0.3;
  end;
  // Ideal or practical diode models calculations
  if (Model = 1) or (Model = 2) then begin
    VD := VD1;
    I2 := VD / R2;
    VR := VS - VD;
    IT := VR / R1; IR := IT;
    ID := IT - I2;
  end
  // Complete diode models calculations (not 100% sure, if this correct)
  else begin
    R := R1 + R2 * RFwd / (R2 + RFwd);
    IT := (VS - VD1) / R;
    VR := R1 * IT;
    I2 := (VS - VR) / R2; ID := IT - I2;
    VD := VD1 + ID * Rfwd;
    I2 := VD / R2;
    VR := VS - VD;
    IT := VR / R1; IR := IT;
    ID := IT - I2;
  end;
end;

{ Reverse biased diode circuit calculations }
{ ----------------------------------------- }

procedure CircuitReverse(Model: Integer; VS, R, Irev: Real; out VD, VR, ID, IR: Real);

begin
  // Ideal or practical diode model
  if (Model = 1) or (Model = 2) then begin
    ID := 0;  IR := ID;
    VD := VS; VR := 0;
  end
  // Complete diode model
  else begin
    ID := Irev; IR := ID;
    VR := R * IR;
    VD := VS - VR;
  end;
end;

{ Diode breakdown calculations }
{ ---------------------------- }

procedure CircuitBreakDown(VS, PIV, I: Real; out R, VD, VR: Real);

begin
  VD := PIV; VR := VS - VD;
  R := VR / I;
end;

{ Zener diode circuit calculations }
{ --------------------------------- }

procedure CircuitZener(VS1, VS2, VL, IL, IDVS1: Real; var R, PZ, ITVS1, ITVS2, IZVS2: Real);

begin
  ITVS1 := IL + IDVS1;
  R := (VS1 - VL) / ITVS1;
  ITVS2 := (VS2 - VL) / R;
  IZVS2 := ITVS2 - IL;
  PZ := VL * IZVS2;
end;

{**********}
{ TfDiodes }
{**********}

{ Application start: Initialisations }

procedure TfDiodes.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  Randomize;
  sAction := 'start'; iCircuit := 1; iModel := 2; sDiodeMaterial := 'Si';
  for I := 1 to 5 do
    bCircuits[I] := True;
  CircuitInit(sAction, iCircuit, iModel, sDiodeMaterial);
end;

{ Menu item "Circuit > New calculation": Start new calculation }

procedure TfDiodes.mCircuitCalcClick(Sender: TObject);

var
  SPIV: string;

begin
  sAction := 'calc';
  // Show up data entry window (for circuit type selection)
  fData.ShowModal;
  if fData.sButton = 'select' then begin
    // Get circuit data from data entry form
    if fData.rbCircuit1.Checked then
      iCircuit := 1
    else if fData.rbCircuit2.Checked then
      iCircuit := 2
    else if fData.rbCircuit3.Checked then
      iCircuit := 3
    else if fData.rbCircuit4.Checked then
      iCircuit := 4
    else
      iCircuit := 5;
    if iCircuit = 4 then begin
      // Zener diode circuit
      sDiodeMaterial := '';
      mSettingsModel.Enabled := False;
    end
    else begin
      // Si and Ge diode circuits
      sDiodeMaterial := LeftStr(fData.cobDiodeMaterial.Text, 2);
      mSettingsModel.Enabled := True;
      if iCircuit = 5 then begin
        // PIV value for diode breakdown circuit
        mSettingsModel.Enabled := False;
        SPIV := fData.edDiodePIV.Text;
        SPIV := StringReplace(SPIV, ' ', '', [rfReplaceAll]);
        SPIV := StringReplace(SPIV, 'V', '', []);
        rPIV := StrToFloat(SPIV);
      end;
    end;
    // Display form for actual circuit calculation
    ClearForm(rVS, rR1, rR2, rVS2, rVL, rIL, rIDVS1, rRFwd, rIRev);
    CircuitInit(sAction, iCircuit, iModel, sDiodeMaterial);
    if iCircuit = 5 then
      edVS2.Text := FloatToStr(rPIV);                                          // PIV as given on data entry form (however editable here)
    edVS.SetFocus;
    btAction.Caption := 'Calculation';
  end;
end;

{ Menu item "Circuit > New test": Generate new test question }

procedure TfDiodes.mCircuitTestClick(Sender: TObject);

const
  // Silicon and germanium diode PIV values
  PIVSi: array[0..4] of Integer = (
    50, 100, 200, 400, 600
  );
  PIVGe: array[0..4] of Integer = (
    50, 60, 80, 100, 120
  );
  // Standard resistor values
  Resistances: array[0..13] of Integer = (
    430, 470, 510, 560, 620, 680, 750, 820, 910, 1000, 1100, 1200, 1300, 1500
  );

var
  I: Integer;
  OK: Boolean;

begin
  OK := False;
  for I := 1 to 5 do
    if bCircuits[I] then
      OK := True;
  // Proceed if at leat one circuit type is selected
  if OK then begin
    sAction := 'test';
    ClearForm(rVS, rR1, rR2, rVS2, rVL, rIL, rIDVS1, rRFwd, rIRev);
    repeat
      iCircuit := Random(5) + 1;
    until bCircuits[iCircuit];                                                 // random circuit type among those actually selected
    // Forward or reverse biased diode circuits
    if iCircuit in [1..3] then begin
      // Random source voltage and resistance(s)
      rVS := Random(16) + 5; rR1 := Resistances[Random(14)];
      edVS.Text := FloatToStr(rVS); edR1.Text := FloatToStr(rR1);
      if iCircuit = 2 then begin
        // 2nd resistance for circuit with parallel resistor
        repeat
          rR2 := Resistances[Random(14)];
        until rR2 >= rR1;
        if Random(2) = 1 then begin
          rR1 /= 10; rR2 /= 10;
        end;
        edR2.Text := FloatToStr(rR2); edR1.Text := FloatToStr(rR1);
      end;
      // Random diode type
      if Random(4) = 0 then
        sDiodeMaterial := 'Ge'
      else
        sDiodeMaterial := 'Si';
      laDiode.Caption := sDiodeMaterial;
      // Complete diode model parameters
      if iModel = 3 then begin
        if iCircuit = 1 then begin
          rRFwd := Resistances[Random(14)] / 100;
          edComplete.Text := FloatToStr(rRFwd);
        end
        else if iCircuit = 2 then begin
          repeat
            rRFwd := Resistances[Random(14)] / 100;
          until rRFwd <= rR2 / 5;
          edComplete.Text := FloatToStr(rRFwd);
        end
        else begin
          rIRev := (Random(11) + 10) / 10;
          edComplete.Text := FloatToStr(rIRev);
        end;
      end;
    end
    else if iCircuit = 4 then begin
      // Zener diode circuit
      repeat
        rVS1 := Random(21) + 20; rVS2 := Random(36) + 40; rVL := Random(11) + 10;
      until (rVS2 > rVS1) and (rVS1 > RVL);
      edVS.Text := FloatToStr(rVS1); edVS2.Text := FloatToStr(rVS2); edVLamp.Text := FloatToStr(rVL);
      rIL := (Random(21) + 10) / 10; rIDVS1 := (Random(11) + 5) / 10;
      edILamp.Text := FloatToStr(rIL);
      edIDVS1.Text := FloatToStr(rIDVS1);;
    end
    else begin
      // Diode breakdown calculation
      if Random(4) = 0 then begin
        sDiodeMaterial := 'Ge';
        rPIV := PIVGe[Random(5)];
      end
      else begin
        sDiodeMaterial := 'Si';
        rPIV := PIVSi[Random(5)];
      end;
      laDiode.Caption := sDiodeMaterial;
      repeat
        rVS := (Random(10) + 1) * 100;
        rID := (Random(20) + 1) / 2;
      until (rVS > rPIV) and (rVS <> 2 * rPIV) and (rVS <= 10 * rPIV) and ((rVS - rPIV) / rID >= 50);
      edVS.Text := FloatToStr(rVS); edVS2.Text := FloatToStr(rPIV);
      edID.Text := FloatToStr(rID);
    end;
    // Show form for this circuit exercise
    CircuitInit(sAction, iCircuit, iModel, sDiodeMaterial);
    if iCircuit = 4 then
       edITVS1.SetFocus
    else
      edVD.SetFocus;
    btAction.Caption := 'Answer';
  end
  // Error message if no circuit type selected
  else
    MessageDlg('Invalid selection', 'At least one exercise circuit has to be selected!', mtError, [mbOK], 0);
end;

{ Menu item "Circuit > Exit": Exit application }

procedure TfDiodes.mCircuitExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Diode model > ...": Select diode model to be used for calculations }

procedure TfDiodes.mSettingsModelIdealClick(Sender: TObject);

begin
  mSettingsModelIdeal.Checked := True;
  mSettingsModelPractical.Checked := False;
  mSettingsModelComplete.Checked := False;
  iModel := 1;
  DiodeModelInit(iCircuit, iModel);
end;

procedure TfDiodes.mSettingsModelPracticalClick(Sender: TObject);

begin
  mSettingsModelIdeal.Checked := False;
  mSettingsModelPractical.Checked := True;
  mSettingsModelComplete.Checked := False;
  iModel := 2;
  DiodeModelInit(iCircuit, iModel);
end;

procedure TfDiodes.mSettingsModelCompleteClick(Sender: TObject);

begin
  mSettingsModelIdeal.Checked := False;
  mSettingsModelPractical.Checked := False;
  mSettingsModelComplete.Checked := True;
  iModel := 3;
  DiodeModelInit(iCircuit, iModel);
end;

{ Menu items "Settings > Exercise circuits": Select circuit types, that will be included in exercise questions }

procedure TfDiodes.mSettingsCircuits1Click(Sender: TObject);

begin
  if mSettingsCircuits1.Checked then
    mSettingsCircuits1.Checked := False
  else
    mSettingsCircuits1.Checked := True;
  bCircuits[1] := mSettingsCircuits1.Checked;
  if mSettingsCircuits1.Checked or mSettingsCircuits2.Checked or mSettingsCircuits3.Checked then
    mSettingsModel.Enabled := True
  else
    mSettingsModel.Enabled := False;
end;

procedure TfDiodes.mSettingsCircuits2Click(Sender: TObject);

begin
  if mSettingsCircuits2.Checked then
    mSettingsCircuits2.Checked := False
  else
    mSettingsCircuits2.Checked := True;
  bCircuits[2] := mSettingsCircuits2.Checked;
  if mSettingsCircuits1.Checked or mSettingsCircuits2.Checked or mSettingsCircuits3.Checked then
    mSettingsModel.Enabled := True
  else
    mSettingsModel.Enabled := False;
end;

procedure TfDiodes.mSettingsCircuits3Click(Sender: TObject);

begin
  if mSettingsCircuits3.Checked then
    mSettingsCircuits3.Checked := False
  else
    mSettingsCircuits3.Checked := True;
  bCircuits[3] := mSettingsCircuits3.Checked;
  if mSettingsCircuits1.Checked or mSettingsCircuits2.Checked or mSettingsCircuits3.Checked then
    mSettingsModel.Enabled := True
  else
    mSettingsModel.Enabled := False;
end;

procedure TfDiodes.mSettingsCircuits4Click(Sender: TObject);

begin
  if mSettingsCircuits4.Checked then
    mSettingsCircuits4.Checked := False
  else
    mSettingsCircuits4.Checked := True;
  bCircuits[4] := mSettingsCircuits4.Checked;
end;

procedure TfDiodes.mSettingsCircuits5Click(Sender: TObject);

begin
  if mSettingsCircuits5.Checked then
    mSettingsCircuits5.Checked := False
  else
    mSettingsCircuits5.Checked := True;
  bCircuits[5] := mSettingsCircuits5.Checked;
end;

{ Menu item "Help > Electronics help": Display electronics help text }

procedure TfDiodes.mHelpElectronicsClick(Sender: TObject);

begin
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Help > Application help": Display "DCircuits3" usage help text }

procedure TfDiodes.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display "DCircuits3" about }

procedure TfDiodes.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronics: Simple diode circuits.' + LineEnding;
  S += 'Circuit values calculator and exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, December 2019 - May 2020.';
  MessageDlg('About "DCircuits3"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculation/Question/Answer": Do calculation resp. generate test question resp. check user answer }

procedure TfDiodes.btActionClick(Sender: TObject);

var
  VD, VR, ID, IR, IDVS2, ITVS1, ITVS2, R1, PZ: Real;
  Mess: string;
  OK: Boolean;

begin
  // Button "Calculation": Calculation: Do calculation for actual circuit
  if btAction.Caption = 'Calculation' then begin
    // Get circuit data, as entered by user
    Mess := '';
    if edVS.Text = '' then
      rVS := 0
    else
      rVS := StrToFloat(edVS.Text);
    if rVS <= 0 then begin
      if iCircuit = 4 then
        Mess := 'Minimum source voltage must be greater than 0'
      else
        Mess := 'Source voltage must be greater than 0';
      edVS.SetFocus;
    end;
    if Mess = '' then begin
      if iCircuit = 4 then begin
        // Zener diode circuit
        rVS1 := rVS;
        if edVS2.Text = '' then
          rVS2 := 0
        else
          rVS2 := StrToFloat(edVS2.Text);
        if rVS2 <= 0 then begin
          Mess := 'Maximum source voltage must be greater than 0';
          edVS2.SetFocus;
        end
        else if rVS1 > rVS2 then begin
          Mess := 'Minimum voltage must be less than maximum voltage';
          edVS.SetFocus;
        end;
        if Mess = '' then begin
          if edVLamp.Text = '' then
            rVL := 0
          else
            rVL := StrToFloat(edVLamp.Text);
          if edILamp.Text = '' then
            rIL := 0
          else
            rIL := StrToFloat(edILamp.Text);
          if edIDVS1.Text = '' then
            rIDVS1 := 0
          else
            rIDVS1 := StrToFloat(edIDVS1.Text);
          if rVL <= 0 then begin
            Mess := 'Lamp voltage must be greater than 0';
            edVLamp.SetFocus;
          end
          else if rVL > rVS1 then begin
            Mess := 'Lamp voltage must be less than minimum source voltage';
            edVLamp.SetFocus;
          end
          else if rIL <= 0 then begin
            Mess := 'Lamp current must be greater than 0';
            edILamp.SetFocus;
          end
          else if rIDVS1 <= 0 then begin
            Mess := 'Zener diode current must be greater than 0';
            edIDVS1.SetFocus;
          end;
        end;
      end
      else if iCircuit = 5 then begin
        // Diode breakdown circuit
        if edVS2.Text = '' then
          rPIV := 0
        else
          rPIV := StrToFloat(edVS2.Text);
        if edID.Text = '' then
          rID := 0
        else
          rID := StrToFloat(edID.Text);
        if rPIV <= 0 then begin
          Mess := 'Diode breakdown voltage must be greater than 0';
          edVS2.SetFocus;
        end
        else if rPIV >= rVS then begin
          Mess := 'Diode breakdown voltage must be less than source voltage';
          edVS.SetFocus;
        end
        else begin
          if rID <= 0 then begin
            Mess := 'Maximum circuit current must be greater than 0';
            edID.SetFocus;
          end
        end;
      end
      else begin
        // Other circuits
        if edR1.Text = '' then
          rR1 := 0
        else
          rR1 := StrToFloat(edR1.Text);
        if rR1 <= 0 then begin
          if iCircuit = 2 then
            Mess := 'Resistance R1'
          else
            Mess := 'Limiting resistance R';
          Mess += ' must be greater than 0';
          Mess := StringReplace(Mess, 'R1', 'R' + SUB_1, []);
          edR1.SetFocus;
        end;
        if Mess = '' then begin
          if iCircuit = 2 then begin
            // 2nd resistance for circuit with parallel resistor
            if edR2.Text = '' then
              rR2 := 0
            else
              rR2 := StrToFloat(edR2.Text);
            if rR2 <= 0 then begin
              Mess := 'Resistance R2 must be greater than 0';
              Mess := StringReplace(Mess, 'R2', 'R' + SUB_2, []);
              edR2.SetFocus;
            end;
          end;
        end;
      end;
    end;
    if Mess = '' then begin
      if iModel = 3 then begin
        // Complete diode model parameters (NO realistic value check done!)
        if iCircuit = 3 then begin
          // Reverse biased diode circuit
          if edComplete.Text = '' then
            rIrev := 0
          else
            rIrev := StrToFloat(edComplete.Text);
        if rIrev <= 0 then begin
          Mess := 'Diode reverse current must be greater than 0';
          edComplete.SetFocus;
        end;
      end
      else if (iCircuit = 1) or (iCircuit = 2) then begin
        // Forward biased diode circuits
        if edComplete.Text = '' then
          rRfwd := 0
        else
          rRfwd := StrToFloat(edComplete.Text);
          if rRfwd <= 0 then begin
            Mess := 'Diode dyn. forward resistance must be greater than 0';
            edComplete.SetFocus;
          end;
        end;
      end;
    end;
    if Mess = '' then begin
      // Calculate circuit values (depending of actual circuit type)
      case iCircuit of
        1: CircuitForward1(iModel, sDiodeMaterial, rVS, rR1, rRfwd, rVD, rVR, rID, rIR);
        2: CircuitForward2(iModel, sDiodeMaterial, rVS, rR1, rR2, rRfwd, rVD, rVR, rID, rIR);
        3: CircuitReverse(iModel, rVS, rR1, rIrev * 1E-6, rVD, rVR, rID, rIR);
        4: CircuitZener(rVS1, rVS2, rVL, rIL, rIDVS1, rR1, rPZ, rITVS1, rITVS2, rIDVS2);
        5: CircuitBreakDown(rVS, rPIV, rID, rR1, rVD, rVR);
      end;
      // Fill in calculated circuit values
      if iCircuit = 4 then begin
        // Zener diode circuit
        edR1.Text := FloatToStrF(rR1, ffFixed, 0, 3);
        edPZ.Text := FloatToStrF(rPZ, ffFixed, 0, 3);
        edITVS1.Text := FloatToStrF(rITVS1, ffFixed, 0, 3);
        edID.Text := FloatToStrF(rIDVS2, ffFixed, 0, 3);
        edIR.Text := FloatToStrF(rITVS2, ffFixed, 0, 3);
      end
      else if iCircuit = 5 then begin
        // Diode breakdown calculation
        edR1.Text := FloatToStrF(rR1, ffFixed, 0, 3);;
        edVD.Text := FloatToStrF(rVD, ffFixed, 0, 3);
        edVR.Text := FloatToStrF(rVR, ffFixed, 0, 3);
      end
      else begin
        // Other circuits
        edVD.Text := FloatToStrF(rVD, ffFixed, 0, 3);
        edVR.Text := FloatToStrF(rVR, ffFixed, 0, 3);
        laUID.Caption := 'mA'; laUIR.Caption := 'mA';
        edID.Text := FloatToStrF(rID * 1000, ffFixed, 0, 3);
        edIR.Text := FloatToStrF(rIR * 1000, ffFixed, 0, 3);
      end;
    end
    else
      MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
  end
  // Button "Question/Answer": Exercise: Generate new exercise circuit resp. check user answer
  else begin
    // Button "Question": Generate new exercise
    if btAction.Caption = 'Question' then
      mCircuitTest.Click
    // Button "Answer": Check user answer
    else begin
      // Calculate circuit values (values to be given as answers by user)
      case iCircuit of
        1: CircuitForward1(iModel, sDiodeMaterial, rVS, rR1, rRfwd, rVD, rVR, rID, rIR);
        2: CircuitForward2(iModel, sDiodeMaterial, rVS, rR1, rR2, rRfwd, rVD, rVR, rID, rIR);
        3: CircuitReverse(iModel, rVS, rR1, rIrev * 1E-6, rVD, rVR, rID, rIR);
        4: CircuitZener(rVS1, rVS2, rVL, rIL, rIDVS1, rR1, rPZ, rITVS1, rITVS2, rIDVS2);
        5: CircuitBreakDown(rVS, rPIV, rID, rR1, rVD, rVR);
      end;
      OK := False;
      // Get user answers from form and check if they are all correct
      if iCircuit in [1..3] then begin
        // Forward and reverse biased diode circuits
        if edVD.Text = '' then
          VD := 0
        else
          VD := StrToFloat(edVD.Text);
        if edVR.Text = '' then
          VR := 0
        else
          VR := StrToFloat(edVR.Text);
        if edID.Text = '' then
          ID := 0
        else
          ID := StrToFloat(edID.Text) / 1000;
        if edIR.Text = '' then
          IR := 0
        else
          IR := StrToFloat(edIR.Text) / 1000;
        if (Abs(rVD - VD) < 0.001) and (Abs(rVR - VR) < 0.001) and (Abs(rID - ID) < 0.001) and (Abs(rIR - IR) < 0.001) then
          OK := True;
      end
      else if iCircuit = 4 then begin
        // Zener diode circuit
        if edR1.Text = '' then
          R1 := 0
        else
          R1 := StrToFloat(edR1.Text);
        if edPZ.Text = '' then
          PZ := 0
        else
          PZ := StrToFloat(edPZ.Text);
        if edITVS1.Text = '' then
          ITVS1 := 0
        else
          ITVS1 := StrToFloat(edITVS1.Text);
        if edID.Text = '' then
          IDVS2 := 0
        else
          IDVS2 := StrToFloat(edID.Text);
        if edIR.Text = '' then
          ITVS2 := 0
        else
          ITVS2 := StrToFloat(edIR.Text);
        if (Abs(rR1 - R1) < 0.001) and (Abs(rPZ - PZ) < 0.001) and (Abs(rITVS1 - ITVS1) < 0.001) and (Abs(rITVS2 - ITVS2) < 0.001) and (Abs(rIDVS2 - IDVS2) < 0.001) then
          OK := True;
      end
      else begin
        // Diode breakdown circuit
        if edVD.Text = '' then
          VD := 0
        else
          VD := StrToFloat(edVD.Text);
        if edVR.Text = '' then
          VR := 0
        else
          VR := StrToFloat(edVR.Text);
        if edR1.Text = '' then
          R1 := 0
        else
          R1 := StrToFloat(edR1.Text);
        if (Abs(rR1 - R1) < 0.001) and (Abs(rVD - VD) < 0.001) and (Abs(rVR - VR) < 0.001) then
          OK := True;
      end;
      // Exercise evaluation
      if OK then
        imEval.Picture.LoadFromFile('correct.png')
      else
        imEval.Picture.LoadFromFile('false.png');
      btAction.Caption := 'Question';
    end;
  end;
end;

end.

