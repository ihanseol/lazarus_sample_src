{**************************************}
{* Main unit for TempHeat application *}
{**************************************}

unit tempheat_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8, tempheat_help;

type
  TMaterial = record
    MName: string;
    MType: Char;
    MExpCoeffLin, mExpCoeffVol: Real;
    MSpecHeatJ, MSpecHeatKCal: Real;
    MMelting, MBoiling: Real;
    MLatHeatFusionJ, MLatHeatFusionKCal, MLatHeatVaporizationJ, MLatHeatVaporizationKCal: Real;
    MThConductivity: Real;
  end;
  TMaterials = array of TMaterial;
  {************}
  { TfTempHeat }
  {************}
  TfTempHeat = class(TForm)
    mMenu: TMainMenu;
    mCalc, mCalcScales, mCalcExpansion, mCalcTransfer, mCalcPhases, mCalcConduction, mCalcRadiation: TMenuItem;
    mCalcExit: TMenuItem;
    mSettings, mSettingsKcal, mSettingsNoConstCheck: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laCalculation, laTemperature, laTemperature2, laScale, laQuantity, laQuantity2: TLabel;
    laConstant, laConstant2, laMaterial, laObject, laCalcResult, laPhase: TLabel;
    laUQuantity, laUQuantity2, laUConstant2, laUCalcResult: TLabel;
    edTemperature, edTemperature2, edQuantity, edQuantity2, edConstant, edConstant2, edCalcResult: TEdit;
    rbSelect1, rbSelect2: TRadioButton;
    cobScales, cobMaterial, cobObject: TComboBox;
    sgScale: TStringGrid;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mCalcScalesClick(Sender: TObject);
    procedure mCalcExpansionClick(Sender: TObject);
    procedure mCalcTransferClick(Sender: TObject);
    procedure mCalcPhasesClick(Sender: TObject);
    procedure mCalcConductionClick(Sender: TObject);
    procedure mCalcRadiationClick(Sender: TObject);
    procedure mCalcExitClick(Sender: TObject);
    procedure mSettingsKcalClick(Sender: TObject);
    procedure mSettingsNoConstCheckClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure rbSelect1Change(Sender: TObject);
    procedure rbSelect2Change(Sender: TObject);
    procedure cobScalesChange(Sender: TObject);
    procedure cobMaterialChange(Sender: TObject);
    procedure cobObjectChange(Sender: TObject);
  private
    iCalc, iMaterial: Integer;
    rT1, rT2, rQ1, rQ2, rC1, rC2, rCalcResult: Real;
    sExpansionCalc, sPhaseChange, sTempUnit, sHeatUnit: string;
    bStart: Boolean;
    aMaterials: TMaterials;
  end;

const
  SUP_2 = #$C2#$B2;
  Calculations: array[0..5] of string = (
    'Temperature scales', 'Thermal expansion', 'Heat transfer', 'Phase changes', 'Heat conduction', 'Heat radiation'
  );

var
  fTempHeat: TfTempHeat;

implementation

{ Temperature scale conversion routines }

function CelsiusToFahrenheit(TC: Real): Real;

begin
  Result := (9 / 5) * TC + 32;
end;

function FahrenheitToCelsius(TF: Real): Real;

begin
  Result := (5 / 9) * (TF - 32);
end;

function CelsiusToKelvin(TC: Real): Real;

begin
  Result := TC + 273.15;
end;

function KelvinToCelsius(TK: Real): Real;

begin
  Result := TK - 273.15;
end;

function FahrenheitToKelvin(TF: Real): Real;

begin
  Result := FahrenheitToCelsius(TF) + 273.15;
end;

function KelvinToFahrenheit(TK: Real): Real;

begin
  Result := CelsiusToFahrenheit(TK - 273.15);
end;

{ Right-alignment of grid values }

function GFormat(R: Real): string;

var
  I: Integer;
  SR: string;

begin
  SR := ' ' + FloatToStrF(R, ffFixed, 2, 2);
  for I := 0 to 9 - Length(SR) do
    SR := ' ' + SR;
  Result := SR;
end;

{ Get (heat constant) value from string }

function GetValue(SR: string): Real;

// The function converts a 7-characters (or empty) string (as read from the materials.txt file) to a real number
// Using the Val function (instead of StrToFloat) avoids problems with local decimal separator (when using the dot in the file)

var
  Code: Integer;
  R: Real;

begin
  if (SR = '') or (SR = '       ') then
    // Undefined values at end of line resp. within the line
    R := 0
  else begin
    // Strings, that should always contain a numberic value
    Val(SR, R, Code);
    if Code > 1 then begin
      // Code = 0 for positive, code = 1 for negative number; others = not numeric
      MessageDlg('File error', 'Invalid or corrupted materials file!', mtError, [mbOK], 0);
      Halt;                                                                    // abort application if file contains non-numeric data
    end;
  end;
  Result := R;
end;

{ Read heat constants data from materials.txt file }

procedure ReadMaterials(out Materials: TMaterials);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'materials.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N); SetLength(Materials, N);
      with Materials[N - 1] do begin
        MName := UTF8Trim(UTF8Copy(Line, 1, 27));
        MType := UTF8Copy(Line, 29, 1)[1];
        MExpCoeffLin := GetValue(UTF8Copy(Line, 31, 7)) * 1E-6;
        MExpCoeffVol := GetValue(UTF8Copy(Line, 41, 7)) * 1E-6;
        MSpecHeatJ := GetValue(UTF8Copy(Line, 51, 7));
        MSpecHeatKcal := GetValue(UTF8Copy(Line, 61, 7));
        MMelting := GetValue(UTF8Copy(Line, 71, 7));
        MBoiling := GetValue(UTF8Copy(Line, 101, 7));
        MLatHeatFusionJ := GetValue(UTF8Copy(Line, 81, 7));
        MLatHeatFusionKCal := GetValue(UTF8Copy(Line, 91, 7));
        MLatHeatVaporizationJ := GetValue(UTF8Copy(Line, 111, 7));
        MLatHeatVaporizationKCal := GetValue(UTF8Copy(Line, 121, 7));
        MThConductivity := GetValue(UTF8Copy(Line, 131, 7));
      end;
    end;
  end;
  Close(InFile);
end;

{ Start a new calculation: Show hide controls as needed }

procedure NewCalculation(Calc: Integer);

var
  I, J: Integer;

begin
  // Clear the scales table grid
  for I := 1 to 2 do begin
    for J := 0 to 2 do begin
      fTempHeat.sgScale.Cells[J, I] := '';
    end;
  end;
  // Default show/hide settings (will individually be changed, as needed)
  fTempHeat.laCalculation.Caption := Calculations[Calc] + '.';
  fTempHeat.laTemperature.Caption := 'Temperature 1';
  fTempHeat.laTemperature2.Visible := True; fTempHeat.edTemperature2.Visible := True;
  fTempHeat.edTemperature.ShowHint := False; fTempHeat.edTemperature2.ShowHint := False;
  fTempHeat.rbSelect1.Visible := False; fTempHeat.rbSelect2.Visible := False;
  fTempHeat.laMaterial.Visible := True; fTempHeat.cobMaterial.Visible := True;
  fTempHeat.laObject.Visible := False; fTempHeat.cobObject.Visible := False;
  fTempHeat.laQuantity.Visible := True; fTempHeat.edQuantity.Visible := True; fTempHeat.laUQuantity.Visible := True;
  fTempHeat.laQuantity2.Visible := False; fTempHeat.edQuantity2.Visible := False; fTempHeat.laUQuantity2.Visible := False;
  fTempHeat.laConstant.Visible := True; fTempHeat.edConstant.Visible := True;
  fTempHeat.laConstant2.Visible := False; fTempHeat.edConstant2.Visible := False; fTempHeat.laUConstant2.Visible := False;
  fTempHeat.laPhase.Visible := False;
  fTempHeat.laCalcResult.Visible := True; fTempHeat.edCalcResult.Visible := True; fTempHeat.laUCalcResult.Visible := True;
  fTempHeat.laCalcResult.Left := 766;
  case Calc of
    // Temperature scales
    0: begin
      fTempHeat.laMaterial.Visible := False; fTempHeat.cobMaterial.Visible := False;
      fTempHeat.laQuantity.Visible := False; fTempHeat.edQuantity.Visible := False; fTempHeat.laUQuantity.Visible := False;
      fTempHeat.laConstant.Visible := False; fTempHeat.edConstant.Visible := False;
      fTempHeat.laCalcResult.Visible := False; fTempHeat.edCalcResult.Visible := False; fTempHeat.laUCalcResult.Visible := False;
    end;
    // Thermal expansion
    1: begin
      fTempHeat.rbSelect1.Visible := True; fTempHeat.rbSelect2.Visible := True;
    end;
    // Phase changes
    3: begin
      fTempHeat.rbSelect1.Visible := True; fTempHeat.rbSelect2.Visible := True;
      fTempHeat.laTemperature.Caption := 'Temperature';
      fTempHeat.laTemperature2.Visible := False; fTempHeat.edTemperature2.Visible := False;
      fTempHeat.laConstant2.Visible := True; fTempHeat.edConstant2.Visible := True; fTempHeat.laUConstant2.Visible := True;
      fTempHeat.laPhase.Visible := True; fTempHeat.laPhase.Caption := '';
    end;
    // Heat conduction
    4: begin
        fTempHeat.edTemperature.ShowHint := True; fTempHeat.edTemperature2.ShowHint := True;
        fTempHeat.edTemperature.Hint := 'Temperature of the "hot" object';
        fTempHeat.edTemperature2.Hint := 'Temperature of the "cold" object';
        fTempHeat.laQuantity2.Visible := True; fTempHeat.edQuantity2.Visible := True; fTempHeat.laUQuantity2.Visible := True;
    end;
    // Heat radiation
    5: begin
      fTempHeat.edTemperature.ShowHint := True; fTempHeat.edTemperature2.ShowHint := True;
      fTempHeat.edTemperature.Hint := 'Temperature of the object';
      fTempHeat.edTemperature2.Hint := 'Temperature of the environment';
      fTempHeat.laMaterial.Visible := False; fTempHeat.cobMaterial.Visible := False;
      fTempHeat.laObject.Visible := True; fTempHeat.cobObject.Visible := True;
      fTempHeat.laCalcResult.Left := 750;
    end;
  end;
  // Clear edit fields
  fTempHeat.edTemperature.Text := ''; fTempHeat.edTemperature2.Text := '';
  fTempHeat.edQuantity.Text := ''; fTempHeat.edQuantity2.Text := '';
  if not fTempHeat.edConstant.ReadOnly then
    fTempHeat.edConstant.Text := '';
  if not fTempHeat.edConstant2.ReadOnly then
    fTempHeat.edConstant2.Text := '';
  fTempHeat.edCalcResult.Text := '';
end;

{ Fill the materials combobox (depending on actual calculation ) }

procedure FillMaterials(Calc: Integer; var Materials: TMaterials; MType: string; var XMaterial: Integer);

var
  I: Integer;
  Material: string;
  OK: Boolean;

begin
  // Save actual material name
  if fTempHeat.cobMaterial.Items.Count > 1 then
    Material := fTempHeat.cobMaterial.Items[fTempHeat.cobMaterial.ItemIndex]
  else
    Material := '';
  // Refill the combobox
  fTempHeat.cobMaterial.Clear;
  for I := 0 to Length(Materials) - 1 do begin
    if (MType = 'all') or ((MType = 'solids') and (Materials[I].MType = 'S')) then begin
      // Consider all materials or only solids (as told by argument passed)
      OK := True;
      // Eliminate all materials, for which the actually considered constsnt is undefined (was blank in the data file)
      case Calc of
        1: if Materials[I].MExpCoeffVol = 0 then
          OK := False;
        2: if Materials[I].MSpecHeatJ = 0 then
          OK := False;
        3: if Materials[I].MLatHeatFusionJ = 0 then
          OK := False;
        4: if Materials[I].MThConductivity = 0 then
          OK := False;
      end;
      // If the constant is actually defined, insert material name into combobox
      if OK then
        fTempHeat.cobMaterial.Items.AddText(Materials[I].MName);
    end;
  end;
  // Add "Custom" item at end of combobox
  fTempHeat.cobMaterial.Items.AddText('--custom--');
  // Try to keep material selected in combbox unchanged, when doing another type of calculation
  // As material names and/or their position in the list, vary from one calculation to the other, this would normally not be the case
  // Thus, find the material name displayed before (saved at beginning of the procedure) in the new list and adapt the material
  // index (iCalc in the "main" routines, XMaterial here) and set combbox index to this value. If the material has not been found,
  // set to first material in the list
  XMaterial := 0;
  for I := 0 to fTempHeat.cobMaterial.Items.Count - 1 do begin
    if Material = fTempHeat.cobMaterial.Items[I] then
      XMaterial := I;
  end;
  fTempHeat.cobMaterial.ItemIndex := XMaterial;
end;

{ Adapt form controls, depending on actual calculation }

procedure FillParameters(Calc: Integer; Calc2, Calc3: string);

// Making controls visible or hiding them having been done by the NewCalculation procedure, this routine
// set the correct labels corresponding to the edit fields.

begin
  // Default settings (adapted later as needed)
  fTempHeat.edConstant.ReadOnly := False; fTempHeat.edConstant.TabStop := True;
  fTempHeat.edConstant.Text := ''; fTempHeat.edConstant.Color := clDefault;
  fTempHeat.edConstant2.ReadOnly := False; fTempHeat.edConstant2.TabStop := True;
  fTempHeat.edConstant2.Text := ''; fTempHeat.edConstant2.Color := clDefault;
  case Calc of
    // Thermal expansion
    1: begin
      fTempHeat.rbSelect1.Caption := ' Linear expansion'; fTempHeat.rbSelect2.Caption := ' Volume expansion';
      if Calc2 = 'linear' then begin
        // Linear expansion
        fTempHeat.laQuantity.Caption := 'Length';
        fTempHeat.laUQuantity.Caption := 'm';
        fTempHeat.laConstant.Caption := 'Coefficient of linear expansion (1/°C)';
        fTempHeat.laCalcResult.Caption := 'Linear expansion';
        fTempHeat.laUCalcResult.Caption := 'mm';
      end
      else begin
        // Volume expansion
        fTempHeat.laQuantity.Caption := 'Volume';
        fTempHeat.laUQuantity.Caption := 'L';
        fTempHeat.laConstant.Caption := 'Coefficient of volume expansion (1/°C)';
        fTempHeat.laCalcResult.Caption := 'Volume expansion';
        fTempHeat.laUCalcResult.Caption := 'mL';
      end;
    end;
    // Heat transfer
    2: begin
      fTempHeat.laQuantity.Caption := 'Mass';
      fTempHeat.laUQuantity.Caption := 'kg';
      fTempHeat.laConstant.Caption := 'Specific Heat';
      fTempHeat.laConstant.Caption := fTempHeat.laConstant.Caption + ' (' + Calc2 + '/kg·°C)';
      fTempHeat.laCalcResult.Caption := 'Heat transfer';
      fTempHeat.laUCalcResult.Caption := 'kJ';
    end;
    // Phase changes
    3: begin
      fTempHeat.rbSelect1.Caption := ' Melting'; fTempHeat.rbSelect2.Caption := ' Vaporization';
      fTempHeat.laQuantity.Caption := 'Mass';
      fTempHeat.laUQuantity.Caption := 'kg';
      fTempHeat.laConstant.Caption := 'Latent heat of ' + Calc2;
      fTempHeat.laConstant.Caption := fTempHeat.laConstant.Caption + ' (' + Calc3 + '/kg)';
      if Calc2 = 'fusion' then
        // Melting
        fTempHeat.laConstant2.Caption := 'Melting point'
      else
        // Vaporization
        fTempHeat.laConstant2.Caption := 'Boiling point';
      fTempHeat.laCalcResult.Caption := 'Heat of ' + Calc2;
      fTempHeat.laUCalcResult.Caption := 'kJ';
    end;
    // Heat conduction
    4: begin
      fTempHeat.laQuantity.Caption := 'Surface area';
      fTempHeat.laUQuantity.Caption := 'm' + SUP_2;
      fTempHeat.laConstant.Caption := 'Thermal conductivity (W/m°C)';
      fTempHeat.laCalcResult.Caption := 'Rate of heat transfer';
      fTempHeat.laUCalcResult.Caption := 'W';
    end;
    // Heat radiation
    5: begin
      fTempHeat.laQuantity.Caption := 'Surface area';
      fTempHeat.laUQuantity.Caption := 'm' + SUP_2;
      fTempHeat.laConstant.Caption := 'Object emissivity';
      fTempHeat.laCalcResult.Caption := 'Net rate of heat transfer';
      fTempHeat.laUCalcResult.Caption := 'W';
    end;
  end;
end;

{ Get material constant from list and display it onto form }

procedure WriteConstant(Calc: Integer; Calc2, Calc3: string; Material: string; var Materials: TMaterials);

// This procedure also sets the material constant edit field writable or read-only as applicable

var
  I, IX: Integer;

begin
  // Get material index from name (actual combobox display)
  IX := -1;
  for I := 0 to Length(Materials) - 1 do begin
    if Material = Materials[I].MName then
      IX := I;
  end;
  // If a constant has been found: Display it
  if IX >= 0 then begin
    // Set edit fields read-only
    fTempHeat.edConstant.ReadOnly := True;  fTempHeat.edConstant.TabStop := False;  fTempHeat.edConstant.Color := clCream;
    fTempHeat.edConstant2.ReadOnly := True; fTempHeat.edConstant2.TabStop := False; fTempHeat.edConstant2.Color := clCream;
    // Display constant (the one used with current calculation type and settings)
    case Calc of
      // Coefficient of linear resp volume expansion
      1: begin
        if Calc2 = 'linear' then
          fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MExpCoeffLin, ffExponent, 1, 1)
        else
          fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MExpCoeffVol, ffExponent, 1, 1);
      end;
      // Specific heat (Joule or kilocalories based unit)
      2: begin
        if Calc2 = 'J' then
          fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MSpecHeatJ, ffFixed, 0, 0)
        else
          fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MSpecHeatKCal, ffFixed, 0, 4);
      end;
      // Latent heat of fusion resp. vaporization (Joule or kilocalories based unit)
      3: begin
        if Calc2 = 'fusion' then begin
          if Calc3 = 'kJ' then
            fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MLatHeatFusionJ, ffFixed, 0, 2)
          else
            fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MLatHeatFusionKCal, ffFixed, 0, 2);
          fTempHeat.edConstant2.Text := FloatToStrF(Materials[IX].MMelting, ffFixed, 0, 1);
        end
        else begin
          if Calc3 = 'kJ' then
            fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MLatHeatVaporizationJ, ffFixed, 0, 2)
          else
            fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MLatHeatVaporizationKCal, ffFixed, 0, 2);
          fTempHeat.edConstant2.Text := FloatToStrF(Materials[IX].MBoiling, ffFixed, 0, 1);
        end;
      end;
      // Thermal conductivity
      4: begin
        fTempHeat.edConstant.Text := FloatToStrF(Materials[IX].MThConductivity, ffFixed, 0, 3);
      end;
    end;
  end
  // If no constant has been found, make corr. edit field writeable (for custom user entry)
  else begin
    fTempHeat.edConstant.ReadOnly := False; fTempHeat.edConstant.TabStop := True; fTempHeat.edConstant.Color := clDefault;
    fTempHeat.edConstant2.ReadOnly := False; fTempHeat.edConstant2.Color := clDefault;
    fTempHeat.edConstant.Text := ''; fTempHeat.edConstant2.Text := '';
  end;
end;

{$R *.lfm}

{************}
{ TfTempHeat }
{************}

{ Application start: Initialisation }

procedure TfTempHeat.FormCreate(Sender: TObject);

begin
  ReadMaterials(aMaterials);
  sExpansionCalc := 'linear'; sTempUnit := '°C'; sHeatUnit := 'J'; sPhaseChange := 'fusion';
  bStart := True;
  mCalcScales.Click;
end;

{ Menu item "Calculation > Temperature scales": Start new temperature scales conversion }

procedure TfTempHeat.mCalcScalesClick(Sender: TObject);

begin
  iCalc := 0;
  NewCalculation(iCalc);
  if bStart then
    bStart := False
  else
    edTemperature.SetFocus;
end;

{ Menu item "Calculation > Thermal expansion": New thermal expansion calculation }

procedure TfTempHeat.mCalcExpansionClick(Sender: TObject);

begin
  iCalc := 1;
  NewCalculation(iCalc);
  FillParameters(iCalc, sExpansionCalc, '');
  if sExpansionCalc = 'linear' then
    FillMaterials(iCalc, aMaterials, 'solids', iMaterial)
  else
    FillMaterials(iCalc, aMaterials, 'all', iMaterial);
  WriteConstant(iCalc, sExpansionCalc, '', cobMaterial.Items[iMaterial], aMaterials);
  edTemperature.SetFocus;
end;

{ Menu item "Calculation > Heat transfer": New heat transfer calculation }

procedure TfTempHeat.mCalcTransferClick(Sender: TObject);

begin
  iCalc := 2;
  if sHeatUnit = 'kJ' then
    sHeatUnit := 'J';
  NewCalculation(iCalc);
  FillParameters(iCalc, sHeatUnit, '');
  FillMaterials(iCalc, aMaterials, 'all', iMaterial);
  WriteConstant(iCalc, sHeatUnit, '', cobMaterial.Items[iMaterial], aMaterials);
  edTemperature.SetFocus;
end;

{ Menu item "Calculation > Phase changes": New phase changes calculation }

procedure TfTempHeat.mCalcPhasesClick(Sender: TObject);

begin
  iCalc := 3;
  if sHeatUnit = 'J' then
    sHeatUnit := 'kJ';
  NewCalculation(iCalc);
  FillParameters(iCalc, sPhaseChange, sHeatUnit);
  FillMaterials(iCalc, aMaterials, 'all', iMaterial);
  WriteConstant(iCalc, sPhaseChange, sHeatUnit, cobMaterial.Items[iMaterial], aMaterials);
  edTemperature.SetFocus;
end;

{ Menu item "Calculation > Heat conduction": New heat conduction calculation }

procedure TfTempHeat.mCalcConductionClick(Sender: TObject);

begin
  iCalc := 4;
  NewCalculation(iCalc);
  FillParameters(iCalc, '', '');
  FillMaterials(iCalc, aMaterials, 'all', iMaterial);
  WriteConstant(iCalc, '', '', cobMaterial.Items[iMaterial], aMaterials);
  edTemperature.SetFocus;
end;

{ Menu item "Calculation > Heat radiation": New heat radiation calculation }

procedure TfTempHeat.mCalcRadiationClick(Sender: TObject);

begin
  iCalc := 5;
  NewCalculation(iCalc);
  FillParameters(iCalc, '', '');
  cobObject.ItemIndex := cobObject.Items.Count - 1;
  edConstant.ReadOnly := False; edConstant.TabStop := True; edConstant.Color := clDefault;
  edConstant.Text := '';
  edTemperature.SetFocus;
end;

{ Menu item "Calculation > Exit": Exit application }

procedure TfTempHeat.mCalcExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Heat quantities in kcal": Toggle heat quantities in J/kcal }

procedure TfTempHeat.mSettingsKcalClick(Sender: TObject);

begin
  if mSettingsKcal.Checked then begin
    mSettingsKcal.Checked := False;
    if iCalc = 3 then
      sHeatUnit := 'kJ'
    else
      sHeatUnit := 'J';
  end
  else begin
    mSettingsKcal.Checked := True;
    sHeatUnit := 'kcal'
  end;
  // If actual calculation concerns heat quantity, adapt edit field labels and rewrite the heat constant value
  if (iCalc = 2) or (iCalc = 3) then begin
    // Unit of heat quantity calculation result
    if (sHeatUnit = 'kcal') then
      laUCalcResult.Caption := 'kcal'
    else
      laUCalcResult.Caption := 'kJ';
    // Heat constants
    if iCalc = 2 then begin
      // Specific heat
      laConstant.Caption := 'Specific heat (' + sHeatUnit + '/kg·°C)';
      WriteConstant(iCalc, sHeatUnit, '', cobMaterial.Items[iMaterial], aMaterials);
    end
    else if iCalc = 3 then begin
      // Latent heat coefficients
      if sPhaseChange = 'fusion' then
        laConstant.Caption := 'Latent heat of fusion (' + sHeatUnit + '/kg)'
      else
        laConstant.Caption := 'Latent heat of vaporization (' + sHeatUnit + '/kg)';
      WriteConstant(iCalc, sPhaseChange, sHeatUnit, cobMaterial.Items[iMaterial], aMaterials);
    end;
  end;
end;

{ Menu item "Settings > Disable material constants checking": Toggle non-checking/checking of material constants }

procedure TfTempHeat.mSettingsNoConstCheckClick(Sender: TObject);

begin
  if mSettingsNoConstCheck.Checked then
    mSettingsNoConstCheck.Checked := False
  else
    mSettingsNoConstCheck.Checked := True;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfTempHeat.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfTempHeat.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics:' + LineEnding;
  S += 'Some simple calculations, concerning temperature and heat.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2020.';
  MessageDlg('About "TempHeat"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculation": Do calculation (as chosen in the "Calculation" menu) }

procedure TfTempHeat.btCalcClick(Sender: TObject);

const
  B = 5.67E-8;

var
  NT, I, J: Integer;
  T: Real;
  Phase, Mess: string;
  TC, TF, TK: array[1..2] of Real;

begin
  Mess := ''; NT := 0;                                                         // NT = number of temperature edit fields that MUST be filled in
  for I := 1 to 2 do begin
    for J := 0 to 2 do
      sgScale.Cells[J, I] := '';
  end;
  // Reading temperatures
  if edTemperature.Text = '' then begin
    if iCalc = 3 then                                                          // fill-in of temperature is not mandatory here
      NT := 0
    else begin                                                                 // temperature 1 must be filled in
      Mess := 'temperature 1';
      edTemperature.SetFocus;
    end;
  end
  else begin
    NT := 1;
    rT1 := StrToFloat(edTemperature.Text);
    if edTemperature2.Visible then begin
      if (edTemperature2.Text = '') and (iCalc <> 0) then begin                // temperature 2 must be filled in
        Mess := 'temperature 2';
        edTemperature2.SetFocus;
      end
      else begin
        if edTemperature2.Text <> '' then begin                                // for scale conversion, temperature 2 may be left blank
          NT := 2;
          rT2 := StrToFloat(edTemperature2.Text);
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    // Reading length/volume/mass/area
    if edQuantity.Visible then begin
      if (edQuantity.Text = '') or (StrToFloat(edQuantity.Text) < 0) then begin
        case iCalc of
          1   : if sExpansionCalc = 'linear' then Mess := 'length' else Mess := 'volume';
          2, 3: Mess := 'mass';
          4, 5: Mess := 'surface area';
        end;
        edQuantity.SetFocus;
      end
      else
        rQ1 := StrToFloat(edQuantity.Text);
    end;
  end;
  if Mess = '' then begin
    // Reading thickness (heat conduction case)
    if edQuantity2.Visible then begin
      if (edQuantity2.Text = '') or (StrToFloat(edQuantity2.Text) < 0) then begin
        Mess := 'thickness';
        edQuantity2.SetFocus;
      end
      else
        rQ2 := StrToFloat(edQuantity2.Text);
    end;
  end;
  if Mess = '' then begin
    // Reading heat constants
    if edConstant.Visible then begin
      if (edConstant.Text = '') or ((StrToFloat(edConstant.Text) < 0) and (iCalc <> 1)) then begin
        case iCalc of
          1: Mess := 'expansion coefficient';
          2: Mess := 'specific heat';
          3: Mess := 'latent heat';
          4: Mess := 'thermal conductivity';
          5: Mess := 'emissivity';
        end;
        edConstant.SetFocus;
      end
      else begin
        rC1 := StrToFloat(edConstant.Text);
        // Reading melting/boiling point (phase changes case)
        if edConstant2.Visible and (edConstant2.Text <> '') then
          rC2 := StrToFloat(edConstant2.Text);
      end;
    end;
  end;
  // Conversion of temperatures entered by user (depending on actual value in scales combobox)
  if Mess = '' then begin
    for I := 1 to NT do begin
      // Do this for those temperatures that are mandatory
      if I = 1 then
        T := rT1
      else
        T := rT2;
      if sTempUnit = '°C' then begin
        TC[I] := T; TF[I] := CelsiusToFahrenheit(T); TK[I] := CelsiusToKelvin(T);
      end
      else if sTempUnit = '°F' then begin
        TC[I] := FahrenheitToCelsius(T); TF[I] := T; TK[I] := FahrenheitToKelvin(T);
      end
      else begin
        TC[I] := KelvinToCelsius(T); TF[I] := KelvinToFahrenheit(T); TK[I] := T;
      end;
      // Check if temperature >= absolute zero
      if TK[I] < 0 then begin
        if (I = 1) or ((I = 2) and (Mess <> '')) then
          edTemperature.SetFocus
        else
          edTemperature2.SetFocus;
        Mess := '*Temperature cannot be under absolute zero!';
      end;
    end;
  end;
  if Mess = '' then begin
    // Check emissivity (must be between 0 and 1) (heat ratiation case)
    if iCalc = 5 then begin
      if (rC1 < 0) or (rC1 > 1) then
        Mess := '*Emissivity must be a value between 0 and 1!';
    end;
    if not mSettingsNoConstCheck.Checked then begin
      // Check heat constants (have to be within given limits; cf. application help text)
      case iCalc of
        1: begin
          if sExpansionCalc = 'linear' then begin
            if (Abs(rC1) < 4E-7) or (Abs(rC1) > 2.9E-5) then
              Mess := '*Coefficient of linear expansion is supposed to be between ±0.4E-6 and ±29E-6!';
          end
          else begin
            if (Abs(rC1) < 1E-6) or (Abs(rC1) > 3.4E-3) then
              Mess := '*Coefficient of volume expansion is supposed to be between ±1E-6 and ±3400E-6!';
          end;
        end;
        2: begin
          if sHeatUnit = 'J' then begin
            if (Abs(rC1) < 128) or (Abs(rC1) > 4186) then
              Mess := '*Specific heat is supposed to be between 128 and 4186!';
          end
          else begin
            if (Abs(rC1) < 0.0305) or (Abs(rC1) > 1) then
              Mess := '*Specific heat is supposed to be between 0.0305 and 1!';
          end;
        end;
        3: begin
          if rbSelect1.Checked then begin
            if sHeatUnit = 'kJ' then begin
              if (Abs(rC1) < 5.23) or (Abs(rC1) > 380) then
                Mess := '*Latent heat of fusion is supposed to be between 5.23 and 380!';
            end
            else begin
              if (Abs(rC1) < 1.25) or (Abs(rC1) > 90) then
                Mess := '*Latent heat of fusion is supposed to be between 1.25 and 90!';
            end;
          end
          else begin
            if sHeatUnit = 'kJ' then begin
              if (Abs(rC1) < 20.9) or (Abs(rC1) > 11400) then
                Mess := '*Latent heat of vaporization is supposed to be between 20.9 and 11400!';
            end
            else begin
              if (Abs(rC1) < 4.99) or (Abs(rC1) > 2720) then
                Mess := '*Latent heat of vaporization is supposed to be between 4.99 and 2720!';
            end;
          end;
        end;
        4: begin
          if (rC1 < 0.01) or (rC1 > 2000) then
            Mess := '*Thermal Conductivity is supposed to be between 0.010 and 2000!';
        end;
      end;
    end;
    if Mess <> '' then
      edConstant.SetFocus;
  end;
  if Mess = '' then begin
    // Check melting/boiling temperature vs. absolute zero (phase changes case)
    if (iCalc = 3) and (edConstant2.Text <> '') and (CelsiusToKelvin(rC2) < 0) then begin
      Mess := '*Temperature cannot be under absolute zero!';
      edConstant2.SetFocus;
    end
    // Automatically exchange temperatures if needed so (heat conduction case)
    else if (iCalc = 4) and (rT1 < rT2) then begin
      MessageDlg('Data error', '"Hot" object temperature must be greater than "cold" object temperature. Values will be exchanged!' , mtWarning, [mbOK], 0);
      T := rT1; rT1 := rT2; rT2 := T;
      T := TC[1]; TC[1] := TC[2]; TC[2] := T;
      T := TF[1]; TF[1] := TF[2]; TF[2] := T;
      T := TK[1]; TK[1] := TK[2]; TK[2] := T;
      edTemperature.Text := FloatToStr(rT1); edTemperature2.Text := FloatToStr(rT2);
    end;
  end;
  // If there was no error, do the calculations
  if Mess = '' then begin
    // Fill in the scales table
    for I := 1 to NT do begin
      sgScale.Cells[0, I] := GFormat(TC[I]);
      sgScale.Cells[1, I] := GFormat(TF[I]);
      sgScale.Cells[2, I] := GFormat(TK[I]);
    end;
    // Do calculation
    // --------------
    if iCalc = 1 then begin
      // Thermal expansion
      rCalcResult := 1000 * (rC1 * rQ1 * (TC[2] - TC[1]));                     // result displayed in mm resp. ml
    end
    else if iCalc = 2 then begin
      // Heat transfer
      rCalcResult := rC1 * rQ1 * (TC[2] - TC[1]);
      if sHeatUnit = 'J' then begin
        laUCalcResult.Caption := 'kJ';
        rCalcResult /= 1000;                                                 // result displayed in kJ
      end
      else
        laUCalcResult.Caption := 'kcal';
    end
    else if iCalc = 3 then begin
      // Phase changes (calculation)
      laUCalcResult.Caption := sHeatUnit;
      rCalcResult := rQ1 * rC1;
      // Phase changes (determination of actual material phase)
      if NT = 0 then
        // If no temperature given, just ignore
        laPhase.Visible := False
      else begin
        // If actual temperature given, determine material phase
        laPhase.Visible := True;
        if cobMaterial.Text = '--custom--' then begin
          // Custom material: Trying to guess the phase
          if edConstant2.Text = '' then
            // No melting/boiling temperature entered
            Phase := 'unknown'
          else begin
            // Melting/boiling temperature has been entered by user
            if rbSelect1.Checked then begin
              // Phase change temperature given = melting point
              if TC[1] < rC2 then
                Phase := 'solid'
              else
              Phase := 'liquid or gasiform';
            end
            else if rbSelect2.Checked then begin
              // Phase change temperature given = boiling point
              if TC[1] > rC2 then
                Phase := 'gasiform'
              else
                Phase := 'solid or liquid';
            end
          end;
        end
        else begin
          // Material included in list: Knowing both melting and boiling point, actual phase can always be determined
          for I := 0 to Length(aMaterials) - 1 do begin
            if cobMaterial.Text = aMaterials[I].MName then begin
              if TC[1] >= aMaterials[I].MBoiling then begin
                Phase := 'gasiform';
                if LowerCase(aMaterials[I].MName) = 'water' then
                  Phase += ' (steam)';
              end
              else if TC[1] >= aMaterials[I].MMelting then begin
                Phase := 'liquid'
              end
              else begin
                Phase := 'solid';
                if LowerCase(aMaterials[I].MName) = 'water' then
                  Phase += ' (ice)';
              end;
            end;
          end;
        end;
      end;
      // Display material phase for actual temperature
      if cobMaterial.Text = '--custom--' then
        laPhase.Caption := 'Phase of material at actual t° = '
      else
        laPhase.Caption := 'Phase of ' + LowerCase(cobMaterial.Text) + ' at actual t° = ';
      laPhase.Caption := laPhase.Caption + Phase;
    end
    else if iCalc = 4 then begin
      // Heat conduction
      rCalcResult := rC1 * rQ1 * (TC[1] - TC[2]) / (rQ2 * 0.01);
    end
    else begin
      // Heat radiation
      rCalcResult := B * rQ1 * rC1 * (TK[2] * TK[2] * TK[2] * TK[2] - TK[1] * TK[1] * TK[1] * TK[1]);
    end;
    // Display calculation result
    edCalcresult.Text := FloatToStrF(rCalcResult, ffFixed, 0, 2);
  end
  // If there was an error, issue a message
  else begin
    if LeftStr(Mess, 1) = '*' then
      Delete(Mess, 1, 1)
    else
      Mess := 'Invalid or missing value for ' + Mess + '!';
    MessageDlg('Data error', Mess, mtError, [mbOK], 0);
  end;
end;

{ Radio-button changes: Adapt edit fields labels and combobox content }

procedure TfTempHeat.rbSelect1Change(Sender: TObject);

begin
  if rbSelect1.Checked then begin
    sExpansionCalc := 'linear';
    sPhaseChange := 'fusion';
    if iCalc = 1 then begin
      // Thermal expansion (linear resp. volume)
      FillMaterials(iCalc, aMaterials, 'solids', iMaterial);
      FillParameters(iCalc, sExpansionCalc, '');
      WriteConstant(iCalc, sExpansionCalc, '', cobMaterial.Items[iMaterial], aMaterials);
    end
    else if iCalc = 3 then begin
      // Phase changes (melting resp. vaporization)
      FillMaterials(iCalc, aMaterials, 'all', iMaterial);
      FillParameters(iCalc, sPhaseChange, sHeatUnit);
      WriteConstant(iCalc, sPhaseChange, sHeatUnit, cobMaterial.Items[iMaterial], aMaterials);
    end;
  end;
end;

procedure TfTempHeat.rbSelect2Change(Sender: TObject);

begin
  if rbSelect2.Checked then begin
    sExpansionCalc := 'volume';
    sPhaseChange := 'vaporization';
    FillMaterials(iCalc, aMaterials, 'all', iMaterial);
    if iCalc = 1 then begin
      FillParameters(iCalc, sExpansionCalc, '');
      WriteConstant(iCalc, sExpansionCalc, '', cobMaterial.Items[iMaterial], aMaterials);
    end
    else if iCalc = 3 then begin
      FillParameters(iCalc, sPhaseChange, sHeatUnit);
      WriteConstant(iCalc, sPhaseChange, sHeatUnit, cobMaterial.Items[iMaterial], aMaterials);
    end;
  end;
end;

{ Temperature scales combobox content changes }

procedure TfTempHeat.cobScalesChange(Sender: TObject);

begin
  case cobScales.ItemIndex of
    0: sTempUnit := '°C';
    1: sTempUnit := '°F';
    2: sTempUnit := 'K';
  end;
end;

{ Materials combobox content changes: Rewrite the (appropriate) heat constant }

procedure TfTempHeat.cobMaterialChange(Sender: TObject);

var
  Calc2, Calc3: string;

begin
  iMaterial := cobMaterial.ItemIndex;
  if iMaterial = cobMaterial.Items.Count - 1 then begin
    // Custom material: make heat constant writeable (for user input)
    edConstant.ReadOnly := False; edConstant.TabStop := True; edConstant.Color := clDefault; edConstant.Text := '';
    edConstant2.ReadOnly := False; edConstant2.TabStop := True; edConstant2.Color := clDefault; edConstant2.Text := '';
  end
  else begin
    // List material: Display the heat constant
    edConstant.ReadOnly := True; edConstant.TabStop := False; edConstant.Color := clCream;
    edConstant2.ReadOnly := True; edConstant2.TabStop := False; edConstant2.Color := clCream;
    case iCalc of
      1: begin Calc2 := sExpansionCalc; Calc3 := ''; end;
      2: begin Calc2 := sHeatUnit; Calc3 := ''; end;
      3: begin Calc2 := sPhaseChange; Calc3 := sHeatUnit; end;
      4: begin Calc2 := ''; Calc3 := ''; end;
    end;
  end;
  // Write the constant (or clear the edit field)
  WriteConstant(iCalc, Calc2, Calc3, cobMaterial.Items[iMaterial], aMaterials);
end;

{ Objects combobox content changes: Set and write the emissivity constant }

procedure TfTempHeat.cobObjectChange(Sender: TObject);

var
  E: Real;

begin
  if cobObject.Text = '--custom--' then begin
    // Custom object: Make edit field writeable
    edConstant.ReadOnly := False; edConstant.TabStop := True; edConstant.Color := clDefault;
    edConstant.Text := '';
  end
  else begin
    // List object: Display the constant
    edConstant.ReadOnly := True; edConstant.TabStop := False; edConstant.Color := clCream;
    case cobObject.ItemIndex of
      0: E := 1;
      1: E := 0;
      2: E := 0.5;
      3: E := 0.95;
      4: E := 0.97;
    end;
    edConstant.Text := FloatToStrF(E, ffFixed, 0, 2);
  end;
end;

end.

