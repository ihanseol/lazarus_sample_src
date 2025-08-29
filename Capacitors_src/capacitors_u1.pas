{****************************************}
{* Main unit for Capacitors application *}
{****************************************}

unit capacitors_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, LazUTF8, capacitors_u2, capacitors_u3, capacitors_u4;

type
  TDielectric = record
    DName, DType: string;
    Dielectric: Double;
  end;
  TDielectrics = array of TDielectric;
  {**************}
  { TfCapacitors }
  {**************}
  TfCapacitors = class(TForm)
    stTitle: TStaticText;
    mMenu: TMainMenu;
    mCalc, mCalcCapacitance1, mCalcCapacitance2, mCalcCircuits, mCalcCharge, mCalcExit: TMenuItem;
    mOptions, mOptionsInput, mOptionsInputV, mOptionsInputQ: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    rbCapacitor1, rbCapacitor2, rbCapacitor3, rbCapacitor4: TRadioButton;
    rbDielectrics1, rbDielectrics2, rbDielectrics3, rbDielectrics4: TRadioButton;
    imCalc: TImage;
    laParam0, laParam1, laParam2, laParam3, laParam4: TLabel;
    laParam5, laParam6, laParam7, laParam8, laParam9: TLabel;
    edParam0, edParam1, edParam2, edParam3, edParam4: TEdit;
    edParam5, edParam6, edParam7, edParam8, edParam9: TEdit;
    cobDielectrics: TComboBox;
    cobUPlatesArea, cobUPlatesDistance: TComboBox;
    cobUCylinderRadius1, cobUCylinderRadius2, cobUCylinderLength: TComboBox;
    cobUPlatesCharge1: TComboBox;
    laUPlatesCharge1, laUPlatesCharge2: TLabel;
    laUParam4, laUParam7, laUParam8, laUParam9: TLabel;
    btCalc: TButton;
    procedure cobUPlatesCharge1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mCalcCapacitance1Click(Sender: TObject);
    procedure mCalcCapacitance2Click(Sender: TObject);
    procedure mCalcCircuitsClick(Sender: TObject);
    procedure mCalcChargeClick(Sender: TObject);
    procedure mCalcExitClick(Sender: TObject);
    procedure mOptionsInputVClick(Sender: TObject);
    procedure mOptionsInputQClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure rbCapacitor1Change(Sender: TObject);
    procedure rbCapacitor2Change(Sender: TObject);
    procedure rbCapacitor3Change(Sender: TObject);
    procedure rbCapacitor4Change(Sender: TObject);
    procedure rbDielectrics1Change(Sender: TObject);
    procedure rbDielectrics2Change(Sender: TObject);
    procedure rbDielectrics3Change(Sender: TObject);
    procedure rbDielectrics4Change(Sender: TObject);
    procedure edParam5Change(Sender: TObject);
    procedure cobDielectricsChange(Sender: TObject);
  private
    iCalc, iP: Integer;
    rA, rD, rR1, rR2, rL, rK, rV, rQ1, rQ2, rC, rW, rE: Double;
    aDielectrics: TDielectrics;
  end;

const
  E0 = 8.85E-12;
  SUB_Digits: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84,
    #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  fCapacitors: TfCapacitors;
  edParams: array[0..9] of TEdit;
  laParams, laUParams: array[0..9] of TLabel;

implementation

{$R *.lfm}

{ Read dielectric constants from text file }

procedure ReadDielectrics(out Dielectrics: TDielectrics);

var
  N, I: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Dielectrics, 0);
  Assign(InFile, 'dielectrics.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Dielectrics, N);
      with Dielectrics[N - 1] do begin
        DName := Trim(LeftStr(Line, 35));
        for I := 0 to 9 do
          DName := StringReplace(DName, IntToStr(I), SUB_Digits[I], [rfreplaceAll]);
        DType := Copy(Line, 36, 2);
        Dielectric := StrToInt(StringReplace(Copy(Line, 39, 8), '.', '', [])) / 100000;  // dielectrics given with 5 decimal digits
      end;
    end;
  end;
  Close(InFile);
end;

{ Init form fields }

procedure InitForm(Calc: Integer; var edParams: array of TEdit; var laParams, laUParams: array of TLabel);

const
  Titles: array[0..1] of string = (
    'Capacitance for capacitor with given shape',
    'Capacitance for capacitor with given dielectric'
  );

var
  I: Integer;

begin
  // Radiobuttons visibility depends on calculation done (shapes resp. dielectrics)
  fCapacitors.rbCapacitor1.Visible := False;   fCapacitors.rbCapacitor2.Visible := False;
  fCapacitors.rbCapacitor3.Visible := False;   fCapacitors.rbCapacitor4.Visible := False;
  fCapacitors.rbDielectrics1.Visible := False; fCapacitors.rbDielectrics2.Visible := False;
  fCapacitors.rbDielectrics3.Visible := False; fCapacitors.rbDielectrics4.Visible := False;
  if Calc = 0 then begin
    fCapacitors.rbCapacitor1.Visible := True;   fCapacitors.rbCapacitor2.Visible := True;
    fCapacitors.rbCapacitor3.Visible := True;   fCapacitors.rbCapacitor4.Visible := True;
  end
  else begin
    fCapacitors.rbDielectrics1.Visible := True; fCapacitors.rbDielectrics2.Visible := True;
    fCapacitors.rbDielectrics3.Visible := True; fCapacitors.rbDielectrics4.Visible := True;
  end;
  // Adapt title
  fCapacitors.stTitle.Caption := Titles[Calc] + '.';
  // Enable "voltage or charge" option
  fCapacitors.mOptionsInput.Enabled := True;
  // Show or hide edit fields (+ associated labels) as needed
  for I := 0 to 9 do begin
    if I = 3 then begin
      // Dielectric constant field
      if Calc = 0 then begin
        // User input
        edParams[I].Readonly := False; edParams[I].TabStop := True; edParams[I].Color := clDefault;
        // Keep actual dielectric constant (if any)
        if edParams[I].Text = '' then
          edParams[I].Text := '1';
      end
      else begin
        // Automatically filled in, if user chooses a material from combobox
        edParams[I].Readonly := True; edParams[I].TabStop := False; edParams[I].Color := clCream;
      end;
    end
    else begin
      // Other fields
      edParams[I].Text := '';
      if (I = 2) or (I = 9) then begin
        // Hide these fields by default (show them if needed later, when a such selection is done)
        edParams[I].Visible := False; laParams[I].Visible := False;
        if laUParams[I] <> nil then
          laUParams[I].Visible := False;
      end;
    end;
  end;
  // Hide the 'value units' comboboxes by default (shown later if needed)
  fCapacitors.cobUPlatesArea.Visible := False; fCapacitors.cobUPlatesDistance.Visible := False;
  fCapacitors.cobUPlatesCharge1.Visible := False; fCapacitors.laUPlatesCharge1.Visible := False;
  fCapacitors.cobUCylinderRadius1.Visible := False; fCapacitors.cobUCylinderRadius2.Visible := False;
  fCapacitors.cobUCylinderLength.Visible := False;
  // Hide the dielectrics combobox by default (shown later if needed)
  fCapacitors.cobDielectrics.Visible := False;
end;

{ Read calculation parameters from form }

procedure ReadParams(Calc: Integer; var Params: array of TEdit;
  out A, D: Double; out NP: Integer; out R1, R2, L, K, V, Q1, Q2: Double; out Mess: string);

begin
  A := 0; D := 0; NP := 0; R1 := 0; R2 := 0; L := 0; K := 0; V := 0; Q1 := 0; Q2 := 0; Mess := '';
  // Parallel plates (and interleaved) capacitor
  if (Calc = 1) or (fCapacitors.rbCapacitor1.Checked or fCapacitors.rbCapacitor2.Checked) then begin
    // Plates surface area
    if Params[0].Text <> '' then
      A := StrToFloat(Params[0].Text);
    case fCapacitors.cobUPlatesArea.ItemIndex of
      1: A /= 100;                                                             // area is given in dm²
      2: A /= 10000;                                                           // area is given in cm²
    end;;
    if A <= 0 then begin
      Mess := 'Invalid plates area: Must be greater than 0';
      Params[0].SetFocus;
    end;
    if Mess = '' then begin
      // Plates distance
      if Params[1].Text <> '' then
        D := StrToFloat(Params[1].Text) * 1E-3;
      if fCapacitors.cobUPlatesDistance.ItemIndex = 1 then
        D /= 1000;                                                             // distance is given in µm
      if D <= 0 then begin
        Mess := 'Invalid plates distance: Must be greater than 0';
        Params[1].SetFocus;
      end;
    end;
    if Mess = '' then begin
      // Number of plates
      if (Calc = 1) or fCapacitors.rbCapacitor1.Checked then
        // Normal parallel plates capacitor
        NP := 2
      else if fCapacitors.rbCapacitor2.Checked then begin
        // Interleaved capacitor
        if Params[2].Text <> '' then
          NP := StrToInt(Params[2].Text);
        if NP <= 2 then begin
          Mess := 'Invalid plates number: Must be greater than 2';
          Params[2].SetFocus;
        end;
      end;
    end;
  end
  // Spherical or cylindrical capacitor
  else begin
    // Inner and outer radius
    if Params[0].Text <> '' then
      R1 := StrToFloat(Params[0].Text);
    case fCapacitors.cobUCylinderRadius1.ItemIndex of
      1: R1 /= 10;                                                             // inner radius was given in dm
      2: R1 /= 100;                                                            // inner radius was given in cm
    end;
    if R1 <= 0 then begin
      Mess := 'Invalid inner radius: Must be greater than 0';
      Params[0].SetFocus;
    end;
    if Mess = '' then begin
      if Params[1].Text <> '' then
        R2 := StrToFloat(Params[1].Text);
      case fCapacitors.cobUCylinderRadius2.ItemIndex of
        1: R2 /= 10;                                                           // outer radius was given in dm
        2: R2 /= 100;                                                          // outer radius was given in cm
      end;
      if R2 <= 0 then begin
        Mess := 'Invalid outer radius: Must be greater than 0';
        Params[1].SetFocus;
      end;
    end;
    if Mess = '' then begin
      // Outer radius must of course be greater than inner radius
      if R1 >= R2 then begin
        Mess := 'Invalid radii: Outer radius must be greater than inner radius';
        Params[0].SetFocus;
      end;
    end;
    if Mess = '' then begin
      // For cylindrical capacitor, user must also enter cylinder length
      if fCapacitors.rbCapacitor4.Checked then begin
        if Params[2].Text <> '' then
          L := StrToFloat(Params[2].Text);
        case fCapacitors.cobUCylinderLength.ItemIndex of
          1: L /= 10;                                                          // length was given in dm
          2: L /= 100;                                                         // length was given in cm
        end;
        if L <= 0 then begin
          Mess := 'Invalid cylinder length: Must be greater than 0';
          Params[2].SetFocus;
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    // Dielectric constant (cannot be less than 1)
    if Params[3].Text <> '' then
      K := StrToFloat(Params[3].Text);
    if K < 1 then begin
      Mess := 'Invalid dielectric constant: Must be greater than or equal to 1';
      Params[3].SetFocus;
    end;
  end;
  if Mess = '' then begin
    // Capacitor applied voltage or stored charge (depending on user settings)
    if fCapacitors.mOptionsInputV.Checked then begin
      // User has selected to input voltage
      if Params[7].Text <> '' then
        V := StrToFloat(Params[7].Text);
      if V <= 0 then begin
        Mess := 'Invalid capacitor voltage: Must be greater than 0';
        Params[7].SetFocus;
      end;
    end
    else begin
      // User has selected to enter charge
      if Params[5].Text <> '' then
        Q1 := StrToFloat(Params[5].Text);
      case fCapacitors.cobUPlatesCharge1.ItemIndex of
        0: Q1 /= 1E+3;                                                         // charge is given in mC
        1: Q1 /= 1E+6;                                                         // charge is given in µC
        2: Q1 /= 1E+9;                                                         // charge is given in nC
      end;
      if Q1 = 0 then begin
        Mess := 'Invalid plate charge: Must be different from 0';
        Params[5].SetFocus;
      end
      else begin
        // Set charge of plate 2 the same as for plate 1 (but negative, of course)
        // Note, that the field will be hidden, except for (simple) parallel plates capacitor
        Q2 := -Q1;
        Params[6].Text := '-' + Params[5].Text;
        if fCapacitors.cobUPlatesCharge1.Visible then
          fCapacitors.laUPlatesCharge2.Caption := fCapacitors.cobUPlatesCharge1.Text
        else
          fCapacitors.laUPlatesCharge2.Caption := fCapacitors.laUPlatesCharge1.Caption;
      end;
    end;
  end;
  // Error message if user input is invalid
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Fill the dielectric combobox (depending on material type selected) }

procedure FillDielectrics(var Dielectrics: TDielectrics; DType: string);

var
  I, IX, P: Integer;
  Dielectric: string;

begin
  fCapacitors.cobDielectrics.Items.Clear;
  IX := -1;
  for I := 0 to Length(Dielectrics) - 1 do begin
    if Dielectrics[I].DType = DType then begin
      if IX = -1 then
        IX := I;                                                               // save index of first item of current material type
      Dielectric := Dielectrics[I].DName;
      if (DType = 'PF') or (DType = 'EL') then begin
        // Adapt material name for combobox display
        P := UTF8Pos('(', Dielectric);
        if DType = 'PF' then begin
          UTF8Delete(Dielectric, 1, P); UTF8Delete(Dielectric, UTF8Length(Dielectric), 1);
        end
        else begin
          Dielectric := UTF8Copy(Dielectric, 1, P - 1);
        end;
      end;
      // Add material name to combobox
      fCapacitors.cobDielectrics.Items.AddText(Dielectric);
    end;
  end;
  // Set combobox itemindex to first material list entry
  // Fill the "dielectric constant" field with the value for this material
  fCapacitors.cobDielectrics.ItemIndex := 0;
  fCapacitors.edParam3.Text := FloatToStr(Dielectrics[IX].Dielectric);
end;

{**************}
{ TfCapacitors }
{**************}

{ Application start: Initialisation }

procedure TfCapacitors.FormCreate(Sender: TObject);

begin
  // Create arrays with parameter edit fields and labels
  edParams[0]  := edParam0;  edParams[1]  := edParam1;   edParams[2]  := edParam2;
  edParams[3]  := edParam3;  edParams[4]  := edParam4;   edParams[5]  := edParam5;
  edParams[6]  := edParam6;  edParams[7]  := edParam7;   edParams[8]  := edParam8;  edParams[9] := edParam9;
  laParams[0]  := laParam0;  laParams[1]  := laParam1;   laParams[2]  := laParam2;
  laParams[3]  := laParam3;  laParams[4]  := laParam4;   laParams[5]  := laParam5;
  laParams[6]  := laParam6;  laParams[7]  := laParam7;   laParams[8]  := laParam8;  laParams[9] := laParam9;
  laUParams[0] := nil;       laUParams[1] := nil;        laUParams[2] := nil;
  laUParams[3] := nil;       laUParams[4] := laUParam4;  laUParams[5] := nil;
  laUParams[6] := nil;       laUParams[7] := laUParam7;  laUParams[8] := laUParam8;  laUParams[9] := laUParam9;
  // Re-arrange controls on the form
  cobUCylinderRadius1.Left := cobUPlatesArea.Left; cobUCylinderRadius2.Left := cobUPlatesDistance.Left;
  rbDielectrics1.Top    := rbCapacitor1.Top;        rbDielectrics2.Top  := rbCapacitor2.Top;
  rbDielectrics3.Top    := rbCapacitor3.Top;        rbDielectrics4.Top  := rbCapacitor4.Top;
  laUPlatesCharge1.Left := cobUPlatesCharge1.Left;  cobDielectrics.Left := edParam2.Left;
  // Apply subscripts
  laParam5.Caption := StringReplace(laParam5.Caption, 'Q1', 'Q' + SUB_Digits[1], []);
  laParam6.Caption := StringReplace(laParam6.Caption, 'Q2', 'Q' + SUB_Digits[2], []);
  // Read dielectric constants
  ReadDielectrics(aDielectrics);
  // Start application with "Capacitance (given shape)" calculation
  mCalcCapacitance1.Click;
end;

{ Menu item "Calculation > Capacitance (given shape)": Determine capacitance for capacitor with shape selected by user }

procedure TfCapacitors.mCalcCapacitance1Click(Sender: TObject);

begin
  iCalc := 0;
  InitForm(iCalc, edParams, laParams, laUParams);
  rbCapacitor1.Checked := True;                                                // always start with showing parallel plates capacitor
  // Adapt edit field user access, depending on value to be entered (voltage resp. charge)
  if mOptionsInputV.Checked then begin
    // User has to enter voltage
    edParam5.ReadOnly := True; edParam7.ReadOnly := False;
    edParam5.TabStop := False; edParam7.TabStop := True;
    edParam5.Color := clCream; edParam7.Color := clDefault;
    laUPlatesCharge1.Visible := True;
  end
  else begin
    // User has to enter charge
    edParam5.ReadOnly := False; edParam7.ReadOnly := True;
    edParam5.TabStop := True; edParam7.TabStop := False;
    edParam5.Color := clDefault; edParam7.Color := clCream;
    cobUPlatesCharge1.Visible := True;
  end;
  // Always start with showing parallel plates capacitor
  imCalc.Picture.LoadFromFile('plates.jpg');
  laParam0.Caption := 'Area of the plates (A)';
  laParam1.Caption := 'Plates distance (d)';
  cobUPlatesArea.Visible := True; cobUPlatesDistance.Visible := True;
  laParam9.Visible := True; edParam9.Visible := True; laUParam9.Visible := True;
end;

{ Menu item "Calculation > Capacitance (given dielectric)": Determine capacitance for capacitor with dielectric chosen from lists }

procedure TfCapacitors.mCalcCapacitance2Click(Sender: TObject);

begin
  iCalc := 1;
  InitForm(iCalc, edParams, laParams, laUParams);
  rbDielectrics1.Checked := True;                                              // always start with showing natural materials dielectrics
  laParam5.Caption := 'Charge plate 1 (Q' + SUB_Digits[1] + ')'; laParam6.Visible := True; edParam6.Visible := True;
  cobUPlatesCharge1.Visible := False; laUPlatesCharge1.Visible := False; laUPlatesCharge2.Visible := True;
  // Adapt edit field user access, depending on value to be entered (voltage resp. charge)
  if mOptionsInputV.Checked then begin
    edParam5.ReadOnly := True; edParam7.ReadOnly := False;
    edParam5.TabStop := False; edParam7.TabStop := True;
    edParam5.Color := clCream; edParam7.Color := clDefault;
    laUPlatesCharge1.Visible := True;
  end
  else begin
    edParam5.ReadOnly := False; edParam7.ReadOnly := True;
    edParam5.TabStop := True; edParam7.TabStop := False;
    edParam5.Color := clDefault; edParam7.Color := clCream;
    cobUPlatesCharge1.Visible := True;
  end;
  // Calculation is always for parallel plates capacitor
  imCalc.Picture.LoadFromFile('plates.jpg');
  laParam0.Caption := 'Area of the plates (A)';
  laParam1.Caption := 'Plates distance (d)';
  cobUPlatesArea.Visible := True; cobUPlatesDistance.Visible := True;
  laParam2.Visible := True; laParam2.Caption := 'Dielectric material'; cobDielectrics.Visible := True;
  laParam9.Visible := True; edParam9.Visible := True; laUParam9.Visible := True;
end;

{ Menu item "Calculation > Capacitors in series/parallel": Determine total capacitance for capacitors in series resp. in parallel circuit }

procedure TfCapacitors.mCalcCircuitsClick(Sender: TObject);

begin
  iCalc := 2;
  mOptionsInput.Enabled := False;
  fCircuits.ShowModal;                                                         // open the "series/parallel" window to do this calculation
end;

{ Menu item "Calculation > Capacitor charge/discharge": Determine relevant values for capacitor charge resp. discharge through a resistance }

procedure TfCapacitors.mCalcChargeClick(Sender: TObject);

begin
  iCalc := 3;
  mOptionsInput.Enabled := False;
  fDC.ShowModal;                                                               // open the "charge/discharge" window to do this calculation
end;

{ Menu item "Calculation > Exit": Exit application }

procedure TfCapacitors.mCalcExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Capacitance calculation > ...": For capacitance calculations toggle user input between voltage and charge }

procedure TfCapacitors.mOptionsInputVClick(Sender: TObject);

begin
  mOptionsInputV.Checked := True; mOptionsInputQ.Checked := False;
  cobUPlatesCharge1.Visible := False; laUPlatesCharge1.Visible := True;
  edParam5.ReadOnly := True; edParam7.ReadOnly := False;
  edParam5.TabStop := False; edParam7.TabStop := True;
  edParam5.Color := clCream; edParam7.Color := clDefault;
end;

procedure TfCapacitors.mOptionsInputQClick(Sender: TObject);

begin
  mOptionsInputV.Checked := False; mOptionsInputQ.Checked := True;
  cobUPlatesCharge1.Visible := True; laUPlatesCharge1.Visible := False;
  edParam5.ReadOnly := False; edParam7.ReadOnly := True;
  edParam5.TabStop := True; edParam7.TabStop := False;
  edParam5.Color := clDefault; edParam7.Color := clCream;
end;

{ Menu item "Help > Help": Display application help }

procedure TfCapacitors.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfCapacitors.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronics: Capacitor calculations.' + LineEnding;
  S += 'Capacitance for different capacitor shapes and dielectrics. Capacitors in series and in parallel. ';
  S += 'Charge and discharge of a capacitor.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, July-August 2021.';
  MessageDlg('About "Capacitors"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculation": Do the actually selected calculation with the parameter valurs entered by the user }

procedure TfCapacitors.btCalcClick(Sender: TObject);

var
  Mess: string;

begin
  // Capacitance calculation (for given capacitor shape or given dielectrics material)
  // Capacitors in serial/parallel and capacitor charge/discharge calculations are done in separate windows
  if (iCalc = 0) or (iCalc = 1) then begin
    ReadParams(iCalc, edParams, rA, rD, iP, rR1, rR2, rL, rK, rV, rQ1, rQ2, Mess);  // read calculation parameters from form
    // Proceed for valid user input only
    if Mess = '' then begin
      // Calculate the capacitance
      if (iCalc = 1) or rbCapacitor1.Checked then begin
        rC := rK * E0 * rA / rD;                                               // parallel plates capacitor
      end
      else begin
        if rbCapacitor2.Checked then
          rC := (iP - 1) * (rK * E0 * rA / rD)                                 // interleaved capacitor
        else if rbCapacitor3.Checked then
          rC := (4 * Pi * E0 * rK * rR1 * rR2) / (rR2 - rR1)                   // spherical capacitor
        else
          rC := (2 * Pi * E0 * rK * rL) / Ln(rR2 / rR1);                       // cylindrical capacitor
      end;
      // Adapt capacitance unit for display
      if rC * 1E+6 > 1 then begin
        edParam4.Text := FloatToStrF(rC * 1E+6, ffFixed, 0, 3);
        laUParam4.Caption := 'µF';
      end
      else if  rC * 1E+9 > 1 then begin
        edParam4.Text := FloatToStrF(rC * 1E+9, ffFixed, 0, 3);
        laUParam4.Caption := 'nF';
      end
      else begin
        if rC * 1E+12 > 0.01 then
          edParam4.Text := FloatToStrF(rC * 1E+12, ffFixed, 0, 3)
        else
          edParam4.Text := FloatToStrF(rC * 1E+12, ffExponent, 4, 0);
        laUParam4.Caption := 'pF';
      end;
      // Calculate the capacitor's charge or the maximum voltage (depending on user options)
      if mOptionsInputV.Checked then begin
        // Charge calculation
        rQ1 := rV * rC; rQ2 := -rQ1;
        // Adapt charge unit for display
        if rQ1 * 1E+3 > 1 then begin
          edParam5.Text := FloatToStrF(rQ1 * 1E+3, ffFixed, 0, 3);
          cobUPlatesCharge1.ItemIndex := 0; laUPlatesCharge1.Caption := 'mC';
        end
        else if rQ1 * 1E+6 > 1 then begin
          edParam5.Text := FloatToStrF(rQ1 * 1E+6, ffFixed, 0, 3);
          cobUPlatesCharge1.ItemIndex := 1; laUPlatesCharge1.Caption := 'µC';
        end
        else begin
          if rQ1 * 1E+9 > 0.01 then
            edParam5.Text := FloatToStrF(rQ1 * 1E+9, ffFixed, 0, 3)
          else
            edParam5.Text := FloatToStrF(rQ1 * 1E+9, ffExponent, 4, 0);
          cobUPlatesCharge1.ItemIndex := 2; laUPlatesCharge1.Caption := 'nC';
        end;
        // Same charge (but negative) for 2nd plate (fields only visible for simple parallel plates capacitor)
        edParam6.Text := '-' + edParam5.Text;
        laUPlatesCharge2.Caption := cobUPlatesCharge1.Text;
      end
      else begin
        // Voltage calculation
        rV := Abs(rQ1) / rC;
        edParam7.Text := FloatToStrF(rV, ffFixed, 0, 3);
      end;
      // Calculate capacitor's energy (and adapt unit for display)
      rW := 0.5 * rC * Sqr(rV);
      if rW > 1 then begin
        edParams[8].Text := FloatToStrF(rW, ffFixed, 0, 3);
        laUParam8.Caption := 'J';
      end
      else begin
        if rW * 1000 >= 0.01 then
          edParams[8].Text := FloatToStrF(rW * 1000, ffFixed, 0, 3)
        else
          edParams[8].Text := FloatToStrF(rW * 1000, ffExponent, 4, 0);
        laUParam8.Caption := 'mJ';
      end;
      // For simple parallel plates capacitor, calculate electric field
      if (iCalc = 1) or rbCapacitor1.Checked then begin
        rE := rV / rD;
        if rE < 1E+4 then
          edParam9.Text := FloatToStrF(rE, ffFixed, 0, 3)
        else
          edParam9.Text := FloatToStrF(rE, ffExponent, 4, 0);
      end;
    end;
  end;
end;

{ User selection of the capacitor shape (change of corr. radiobuttons) }

procedure TfCapacitors.rbCapacitor1Change(Sender: TObject);

// Parallel plates capacitor

var
  I: Integer;

begin
  if rbCapacitor1.Checked then begin
    for I := 0 to 9 do begin
      if I <> 3 then
        edParams[I].Text := '';                                                // keep actual dielectric constant
    end;
    edParam2.Visible := False; laParams[2].Visible := False;
    edParam9.Visible := True; laParams[9].Visible := True; laUParam9.Visible := True;
    laParam0.Caption := 'Area of the plates (A)';
    laParam1.Caption := 'Plates distance (d)';
    cobUPlatesArea.Visible := True; cobUPlatesDistance.Visible := True;
    laParam5.Caption := 'Charge plate 1 (Q' + SUB_Digits[1] + ')'; laParam6.Visible := True; edParam6.Visible := True;
    cobUPlatesCharge1.Visible := False; laUPlatesCharge1.Visible := False; laUPlatesCharge2.Visible := True;
    if mOptionsInputQ.Checked then
      cobUPlatesCharge1.Visible := True
    else
      laUPlatesCharge1.Visible := True;
    cobUCylinderRadius1.Visible := False; cobUCylinderRadius2.Visible := False; cobUCylinderLength.Visible := False;
    mOptionsInput.Enabled := True;
    imCalc.Picture.LoadFromFile('plates.jpg');
  end;
end;

procedure TfCapacitors.rbCapacitor2Change(Sender: TObject);

// Interleaved capacitor

var
  I: Integer;

begin
  if rbCapacitor2.Checked then begin
    for I := 0 to 9 do begin
      if I <> 3 then
        edParams[I].Text := '';
    end;
    mOptionsInput.Enabled := True;
    edParam2.Visible := True; laParam2.Visible := True;

    edParam9.Visible := False; laParam9.Visible := False; laUParam9.Visible := False;
    laParam0.Caption := 'Area of the plates (A)';
    laParam1.Caption := 'Plates distance (d)';
    laParam2.Caption := 'Number of plates (n)';
    cobUPlatesArea.Visible := True; cobUPlatesDistance.Visible := True;
    laParam5.Caption := 'Charge'; laParam6.Visible := False; edParam6.Visible := False;
    cobUPlatesCharge1.Visible := False; laUPlatesCharge1.Visible := False; laUPlatesCharge2.Visible := False;
    if mOptionsInputQ.Checked then
      cobUPlatesCharge1.Visible := True
    else
      laUPlatesCharge1.Visible := True;
    cobUCylinderRadius1.Visible := False; cobUCylinderRadius2.Visible := False; cobUCylinderLength.Visible := False;
    imCalc.Picture.LoadFromFile('interleaved.jpg');
  end;
end;

procedure TfCapacitors.rbCapacitor3Change(Sender: TObject);

// Spherical capacitor

var
  I: Integer;

begin
  if rbCapacitor3.Checked then begin
    for I := 0 to 9 do begin
      if I <> 3 then
        edParams[I].Text := '';
    end;
    mOptionsInput.Enabled := True;
    edParam2.Visible := False; laParam2.Visible := False;
    edParam9.Visible := False; laParam9.Visible := False; laUParam9.Visible := False;
    laParam0.Caption := 'Inner radius (r' + SUB_Digits[1] + ')';
    laParam1.Caption := 'Outer radius (r' + SUB_Digits[2] + ')';
    cobUPlatesArea.Visible := False; cobUPlatesDistance.Visible := False;
    laParam5.Caption := 'Charge'; laParam6.Visible := False; edParam6.Visible := False;
    cobUPlatesCharge1.Visible := False; laUPlatesCharge1.Visible := False; laUPlatesCharge2.Visible := False;
    if mOptionsInputQ.Checked then
      cobUPlatesCharge1.Visible := True
    else
      laUPlatesCharge1.Visible := True;
    cobUCylinderRadius1.Visible := True; cobUCylinderRadius2.Visible := True; cobUCylinderLength.Visible := False;
    imCalc.Picture.LoadFromFile('sphere.jpg');
  end;
end;

procedure TfCapacitors.rbCapacitor4Change(Sender: TObject);

// Cylindrical capacitor

var
  I: Integer;

begin
  if rbCapacitor4.Checked then begin
    for I := 0 to 9 do begin
      if I <> 3 then
        edParams[I].Text := '';
    end;
    mOptionsInput.Enabled := True;
    edParam9.Visible := False; laParam9.Visible := False; laUParam9.Visible := False;
    laParam0.Caption := 'Inner radius (r' + SUB_Digits[1] + ')';
    laParam1.Caption := 'Outer radius (r' + SUB_Digits[2] + ')';
    cobUPlatesArea.Visible := False; cobUPlatesDistance.Visible := False;
    laParam5.Caption := 'Charge'; laParam6.Visible := False; edParam6.Visible := False;
    cobUPlatesCharge1.Visible := False; laUPlatesCharge1.Visible := False; laUPlatesCharge2.Visible := False;
    if mOptionsInputQ.Checked then
      cobUPlatesCharge1.Visible := True
    else
      laUPlatesCharge1.Visible := True;
    cobUCylinderRadius1.Visible := True; cobUCylinderRadius2.Visible := True;
    edParam2.Visible := True; laParam2.Visible := True;
    laParam2.Caption := 'Cylinder length (l)';
    cobUCylinderLength.Visible := True;
    imCalc.Picture.LoadFromFile('cylinder.jpg');
  end;
end;

{ User selection of the dielectric material (change of corr. radiobuttons) }

procedure TfCapacitors.rbDielectrics1Change(Sender: TObject);

var
  I: Integer;

begin
  if rbDielectrics1.Checked then begin
    for I := 3 to 9 do begin
      if (I in [3, 4, 8, 9]) or ((I in [5, 6]) and mOptionsInputV.Checked) or ((I = 7) and mOptionsInputQ.Checked) then
        edParams[I].Text := '';
    end;
    FillDielectrics(aDielectrics, 'NM');                                       // fill combobox with names of this group's dielectrics
  end;
end;

procedure TfCapacitors.rbDielectrics2Change(Sender: TObject);

var
  I: Integer;

begin
  if rbDielectrics2.Checked then begin
    for I := 3 to 9 do begin
      if (I in [3, 4, 8, 9]) or ((I in [5, 6]) and mOptionsInputV.Checked) or ((I = 7) and mOptionsInputQ.Checked) then
        edParams[I].Text := '';
    end;
    FillDielectrics(aDielectrics, 'CE');
  end;
end;

procedure TfCapacitors.rbDielectrics3Change(Sender: TObject);

var
  I: Integer;

begin
  if rbDielectrics3.Checked then begin
    for I := 3 to 9 do begin
      if (I in [3, 4, 8, 9]) or ((I in [5, 6]) and mOptionsInputV.Checked) or ((I = 7) and mOptionsInputQ.Checked) then
        edParams[I].Text := '';
    end;
    FillDielectrics(aDielectrics, 'PF');
  end;
end;

procedure TfCapacitors.rbDielectrics4Change(Sender: TObject);

var
  I: Integer;

begin
  if rbDielectrics4.Checked then begin
    for I := 3 to 9 do begin
      if (I in [3, 4, 8, 9]) or ((I in [5, 6]) and mOptionsInputV.Checked) or ((I = 7) and mOptionsInputQ.Checked) then
        edParams[I].Text := '';
    end;
    FillDielectrics(aDielectrics, 'EL');
  end;
end;

{ Get dielectric constant for user selected material (selection in dielectrics combobox change) }

procedure TfCapacitors.cobDielectricsChange(Sender: TObject);

var
  I, P: Integer;

begin
  for I := 0 to Length(aDielectrics) - 1 do begin
    P := UTF8Pos(cobDielectrics.Text, aDielectrics[I].DName);                  // the material name displayed is always a substring of the full name
    if P > 0 then
      rK := aDielectrics[I].Dielectric;
  end;
  edParam3.Text := FloatToStr(rK);
end;

{ Automatically fill in the value of the 2nd plate's charge (if there are changes for the 1st one) }

procedure TfCapacitors.edParam5Change(Sender: TObject);

begin
  if edParam5.Text = '' then
    edParam6.Text := ''
  else
    edParam6.Text := '-' + edParam5.Text;
end;

{ Automatically fill in the unit of the 2nd plate's charge (if there are changes for the 1st one) }

procedure TfCapacitors.cobUPlatesCharge1Change(Sender: TObject);

begin
  if cobUPlatesCharge1.Visible then
    laUPlatesCharge2.Caption := cobUPlatesCharge1.Text
  else
    laUPlatesCharge2.Caption := laUPlatesCharge1.Caption;
end;

end.

