{****************************************}
{* Electronics: Resonance RLC circuits  *}
{*--------------------------------------*}
{* Simple physics problems generator    *}
{* Main unit for ACircuits2 application *}
{****************************************}

unit circuits2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, help;

type
  TInputFields   = array[1..15] of TEdit;
  TCircuitValues = array[1..15] of Real;
  {**************}
  { TfACircuits2 }
  {**************}
  TfACircuits2 = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mCircuits, mCircuitsRLC1, mCircuitsRLC2: TMenuItem;
    mOptions, mOptionsView: TMenuItem;
    mHelp, mHelpProgram, mHelpAbout: TMenuItem;
    Title: TStaticText;
    imCircuit: TImage;
    Label1, Label4, Label13, Label16, Label24, Label29: TLabel;
    Label10, Label11, Label12, Label2, Label25, Label26, Label30, Label31: TLabel;
    laVS, laR, laL, laC: TLabel;
    laXL, laXC, laZ: TLabel;
    laF, laI, laQ, laBW, laFL, laFH: TLabel;
    laVIC, laVIL2, laVIL, laVIC2: TLabel;
    edVS, edR, edL, edC, edXL, edXC, edZ: TEdit;
    edF, edI, edQ, edBW, edFH, edFL, edVIL, edVIC: TEdit;
    imEval: TImage;
    edQuestion, edCorrect, edSuccess: TEdit;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mCircuitsRLC1Click(Sender: TObject);
    procedure mCircuitsRLC2Click(Sender: TObject);
    procedure mOptionsViewClick(Sender: TObject);
    procedure mHelpProgramClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
    iQuestion, iCorrect: Integer;
    rVS, rR, rL, rC, rXL, rXC, rZ, rF, rI, rVIL, rVIC, rQ, rBW, rFL, rFH: Real;
    aCircuitsUsed: array[1..2] of Boolean;
    aInputFields: TInputFields;
    aCircuitValues: TCircuitValues;
  end;

const
  Pi = 3.14;
  Circuits: array[1..2] of string = ('series RLC', 'parallel RLC');
  CircuitFiles: array[1..2] of string = ('RLC1', 'RLC2');

var
  fACircuits2: TfACircuits2;

implementation

{$R *.lfm}

{ Integer power of a real number }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Real number formatting }

function RFormat(R: Real; F: Integer): string;

var
  R0: Real;
  SR: string;
begin
  SR := '';
  if R = 0 then
    SR := '0'
  else begin
    if F >= 0 then begin
      R0 := Round(R * Power(10, F)) / Power(10, F);
      if Abs(R0) < Power(10, -F) then
        SR := FloatToStrF(R, ffExponent, F, 0)
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Reset form fields }

procedure FieldsReset(var InputFields: TInputFields);

var
  I: Integer;

begin
  // Clear all edit fields
  for I := 1 to Length(InputFields) do begin
    InputFields[I].Text := '';
    InputFields[I].Font.Color := clDefault;
    InputFields[I].Color := clDefault;
  end;
  // Clear the circuit picture
  fACircuits2.imEval.Picture.Clear;
end;

{--------------------------------}
{ Random circuit values routines }
{--------------------------------}

{ Random voltage value }

function Voltage: Real;

begin
  Result := 10 * (Random(22) + 1);                                             // voltage between 10 and 220 volts
end;

{ Random resistance value }

function Resistance: Real;

begin
  Result := 10 * (Random(100) + 1);                                            // resistance between 10 and 1000 ohms
end;

{ Random frequency value }

function Frequency: Real;

begin
  Result := Random(476) + 25;                                                  // frequency between 25 and 500 hertz
end;

{ Random inductance value }

function Inductance: Real;

begin
  Result := (Random(10) + 1) / 10;                                             // inductance between 0.1 and 1 henrys
end;

{ Random capacitance value }

function Capacitance: Real;

begin
  Result := 10 * (Random(47) + 1);                                             // capacitance between 10 and 470 microFarads
end;

{--------------------------------}
{ Resonance calculation routines }
{--------------------------------}

{ Series RLC circuits }

procedure CircuitSeriesRLC(CType: Integer; V, R: Real; var L, C, F: Real; out  XL, XC, Z, I, VL, VC, Q, BW, FL, FH: Real);

var
  CF: Real;

begin
  CF := C * 1E-6;                                                              // transform capacitance from µF to F
  fACircuits2.laVIL2.Caption := 'Inductor voltage';
  fACircuits2.laVIC2.Caption := 'Capacitor voltage';
  fACircuits2.laVIL.Caption := 'VL'; fACircuits2.laVIC.Caption := 'VC';
  fACircuits2.edVIL.Hint := 'Voltage across inductor (V)';
  fACircuits2.edVIC.Hint := 'Voltage across capacitor (V)';
  case CType of
    // Calculation depending on exercise type, i.e. on value given by the application
    1: F  := 1 / (2 * Pi * Sqrt(L * CF));
    2: begin CF := 1 / (Sqr(F) * 4 * Sqr(Pi) * L); C := CF * 1E+6; end;
    3: L  := 1 / (Sqr(F) * 4 * Sqr(Pi) * CF);
  end;
  I  := V / R;
  XL := 2 * Pi * F * L; XC := 1 / (2 * Pi * F * CF); Z  := V / I;
  VL := I * XL; VC := I * XC;
  Q  := XL / R; BW := F / Q;
  FL := F - BW / 2; FH := F + BW / 2;
end;

{ Parallel RLC circuits }

procedure CircuitParallelRLC(CType: Integer; V, R: Real; var L, C, F: Real; out  XL, XC, Z, I, IL, IC, Q, BW, FL, FH: Real);

var
  CF: Real;

begin
  CF := C * 1E-6;                                                              // transform capacitance from µF to F
  fACircuits2.laVIL2.Caption := 'Inductor current';
  fACircuits2.laVIC2.Caption := 'Capacitor current';
  fACircuits2.laVIL.Caption := 'IL'; fACircuits2.laVIC.Caption := 'IC';
  fACircuits2.edVIL.Hint := 'Current through inductor (A)';
  fACircuits2.edVIC.Hint := 'Current through capacitor (A)';
  case CType of
    // Calculation depending on exercise type, i.e. on value given by the application
    1: F  := 1 / (2 * Pi * Sqrt(L * CF));
    2: begin CF := 1 / (Sqr(F) * 4 * Sqr(Pi) * L); C := CF * 1E+6; end;
    3: L  := 1 / (Sqr(F) * 4 * Sqr(Pi) * CF);
  end;
  I  := V / R;
  XL := 2 * Pi * F * L; XC := 1 / (2 * Pi * F * CF); Z  := V / I;
  IL := V / XL; IC := V / XC;
  Q  := R / XL; BW := F / Q;
  FL := F - BW / 2; FH := F + BW / 2;
end;

{**************}
{ TfACircuits2 }
{**************}

{ Application start: Initialisation }

procedure TfACircuits2.FormCreate(Sender: TObject);

var
  J: Integer;

begin
  // Put form input fields into an array
  aInputFields[1]  := edVS;  aInputFields[2]  := edR;   aInputFields[3] := edL;   aInputFields[4] := edC;
  aInputFields[5]  := edXL;  aInputFields[6]  := edXC;  aInputFields[7] := edZ;   aInputFields[8]  := edF;
  aInputFields[9]  := edI;   aInputFields[10] := edQ;   aInputFields[11] := edBW; aInputFields[12] := edFL;
  aInputFields[13] := edFH;  aInputFields[14] := edVIL; aInputFields[15] := edVIC;
  // Use the 2 circuit types
  for J := 1 to 2 do
    aCircuitsUsed[J] := True;
  // Init test (with default settings)
  Randomize;
  mTestNew.Click;
end;

{ Menu item "Test > New": Init a new test }

procedure TfACircuits2.mTestNewClick(Sender: TObject);

begin
  iQuestion := 0; iCorrect := 0;
  edQuestion.Clear; edCorrect.Clear; edSuccess.Clear;
  edSuccess.Color := clDefault;
  FieldsReset(aInputFields);
  fACircuits2.btStart.Caption := 'Start';
end;

{ Menu item "Test > Exit": Exit the application }

procedure TfACircuits2.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Circuits > Series RLC": Toggle include/exclude series RLC circuits exercises }

procedure TfACircuits2.mCircuitsRLC1Click(Sender: TObject);

begin
  if mCircuitsRLC1.Checked then
    mCircuitsRLC1.Checked := False
  else
    mCircuitsRLC1.Checked := True;
  aCircuitsUsed[1] := mCircuitsRLC1.Checked;
end;

{ Menu item "Circuits > Parallel RLC": Toggle include/exclude parallel RLC circuits exercises }

procedure TfACircuits2.mCircuitsRLC2Click(Sender: TObject);

begin
  if mCircuitsRLC2.Checked then
    mCircuitsRLC2.Checked := False
  else
    mCircuitsRLC2.Checked := True;
  aCircuitsUsed[2] := mCircuitsRLC2.Checked;
end;

{ Menu item "Options > View results": Toggle test/view mode  }

procedure TfACircuits2.mOptionsViewClick(Sender: TObject);

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

{ Menu item "Help > Application help": Display application usage help }

procedure TfACircuits2.mHelpProgramClick(Sender: TObject);

begin
  fHelp.Caption := 'Resonance RLC circuits - Application Help.';
  fHelp.HelpText.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfACircuits2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronics:' + LineEnding;
  S += 'RLC resonance circuits exercice generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April 2022.';
  MessageDlg('About "ACircuits2"', S, mtInformation, [mbOK], 0);
end;

{ Pushbutton pressed: Main program action, depending on actual situation (button caption) }

procedure TfACircuits2.btStartClick(Sender: TObject);

var
  CType, CType2, P, J: Integer;
  UValue: Real;
  Filename, S: string;
  OK: Boolean;

begin
  // At least one circuit type must be selected
  OK := False;
  for J := 1 to 2 do
    if aCircuitsUsed[J] then
      OK := True;
  if OK then begin
    // Button "Start"/"Next": Generate a new exercise
    if (btStart.Caption = 'Start') or (btStart.Caption = 'Next') then begin
      // Circuit type (series RLC, parallel RLC)
      repeat
        CType := Random(2) + 1;
      until aCircuitsUsed[CType];                                              // circuit type must have been selected to be used
      CType2 := Random(3) + 1;                                                 // exercise types: user must calculate F, C or L
      S := 'Resonance ' + Circuits[CType] + ' circuit.'; Title.Caption := S;
      FieldsReset(aInputFields);                                               // clear form input values
      // Random circuit values (repeat values generation until values are as wanted)
      repeat
        OK := True;
        rVS := Voltage; rR := Resistance;
        case CType2 of
          1: begin rL := Inductance; rC := Capacitance; end;
          2: begin rF := Frequency;  rL := Inductance; end;
          3: begin rF := Frequency;  rC := Capacitance; end;
        end;
        // Calculate resonance values
        if CType = 1 then
          CircuitSeriesRLC(CType2, rVS, rR, rL, rC, rF, rXL, rXC, rZ, rI, rVIL, rVIC, rQ, rBW, rFL, rFH)
        else
          CircuitParallelRLC(CType2, rVS, rR, rL, rC, rF, rXL, rXC, rZ, rI, rVIL, rVIC, rQ, rBW, rFL, rFH);
        // Accept random values only, if they are within given (arbitrarily chosen) limits
        if (rF < 25) or (rF > 500) then
          OK := False
        else if (rL < 0.1) or (rL > 1) then
          OK := False
        else if (rC < 1) or (rC > 470) then
          OK := False
        else if (rFL < 0) or (rBW < 1) then
          OK := False;
      until OK;
      aCircuitValues[1] := rVS; aCircuitValues[2] := rR;
      aCircuitValues[3] := rL; aCircuitValues[4] := rC;
      aCircuitValues[5] := rXL; aCircuitValues[6] := rXC; aCircuitValues[7] := rZ;
      aCircuitValues[8] := rF; aCircuitValues[9] := rI;
      aCircuitValues[10] := rQ; aCircuitValues[11] := rBW;
      aCircuitValues[12] := rFL; aCircuitValues[13] := rFH;
      aCircuitValues[14] := rVIL; aCircuitValues[15] := rVIC;
      // Display (given circuit values)
      edVS.Text := RFormat(rVS, 2); edR.Text := RFormat(rR, 2);
      edL.ReadOnly := True; edC.ReadOnly := True; edF.ReadOnly := True;
      edL.TabStop := False; edC.TabStop := False; edF.TabStop := False;
      edL.Color := clCream; edC.Color := clCream; edF.Color := clCream;
      case CType2 of
        1: begin
            edF.ReadOnly := False; edF.TabStop := True; edL.Color := clDefault;
            edL.Text := RFormat(rL, 2); edC.Text := RFormat(rC, 2);
        end;
        2: begin
            edC.ReadOnly := False; edC.TabStop := True; edC.Color := clDefault;
            edL.Text := RFormat(rL, 2); edF.Text := RFormat(rF, 2);
        end;
        3: begin
            edL.ReadOnly := False; edL.TabStop := True; edL.Color := clDefault;
            edC.Text := RFormat(rC, 2); edF.Text := RFormat(rF, 2);
        end;
      end;
      // Draw the circuit (= load circuit file)
      Filename := GetCurrentDir + '/' + CircuitFiles[CType] + '.jpg'; DoDirSeparators(Filename);
      imCircuit.Picture.LoadFromFile(Filename);
      // Take action, depending on being in 'test' or 'view' mode
      if mOptionsView.Checked then begin
        // View mode: Show all values
        for J := 1 to 15 do
          aInputFields[J].Text := RFormat(aCircuitValues[J], 2);
        btStart.Caption := 'Next';                                             // next button push will be another (all resolved) exercise
        btStart.SetFocus;
      end
      else begin
        Inc(iQuestion); edQuestion.Text := IntToStr(iQuestion);
        case CType2 of
          1: aInputFields[5].SetFocus;
          2: aInputFields[4].SetFocus;
          3: aInputFields[3].SetFocus;
        end;
        btStart.Caption := 'Check';                                            // next button push will be to check user's answers
      end;
    end
    else if btStart.Caption = 'View' then begin
      // Button 'View': Show results of current exercise
      for J := 1 to 15 do
        aInputFields[J].Text := RFormat(aCircuitValues[J], 2);
      btStart.Caption := 'Next';                                               // next button push will be another (all resolved) exercise
      btStart.SetFocus;
    end
    else begin
      // Button 'Check': Check user's answer
      OK := True;
      for J := 3 to 15 do begin
        // If one of the values on the form differs from those in the array, the global answer is false
        if aInputFields[J].Text = '' then begin
          // Empty field => global answer is false
          OK := False;
        end
        else begin
          // Get user value from form
          UValue := StrToFloat(aInputFields[J].Text);
        end;
        // Compare user values with array values rounded to 2 decimal digits
        if Round(Power(10, 2) * UValue) / Power(10, 2) <> Round(Power(10, 2) * aCircuitValues[J]) / Power(10, 2) then begin
          // False answer for this value
          aInputFields[J].Text := RFormat(aCircuitValues[J], 2);               // display correct answer
          aInputFields[J].Font.Color := clRed;                                 // use red highlighting to indicate that this answer is false
          OK := False;                                                         // global answer is false
        end;
      end;
      // Display global result (evaluation)
      if OK then begin
        // Global answer = correct
        Inc(iCorrect);
        imEval.Picture.LoadFromFile('correct.png');
      end
      else begin
        // Global answer = false
        imEval.Picture.LoadFromFile('false.png');
      end;
      edCorrect.Text := IntToStr(iCorrect);
      // Success percentage
      P := Round(100 * iCorrect / iQuestion);
      edSuccess.Text := IntToStr(P) + '%';
      // Use different colors depending on success percenage
      if P < 50 then
        edSuccess.Color := clRed
      else if P < 60 then
        edSuccess.Color := clYellow
      else
        edSuccess.Color := clLime;
      btStart.Caption := 'Next';                                               // next button push will be another (not resolved) exercise
      btStart.SetFocus;
    end;
  end
  // No circuit type selected: Display error message
  else
    MessageDlg('Invalid settings','No circuit type selected!', mtError, [mbOK], 0);
end;

end.

