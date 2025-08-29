{*****************************************}
{* Main unit for BloodVolume application *}
{*****************************************}

unit vblood;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, help;

type
  {**********}
  { TfVBlood }
  {**********}
  TfVBlood = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mOptions: TMenuItem;
    mOptionsPounds, mOptionsInches, mOptionsLiters, mOptionsUsualInBV, mOptionsDecimals: TMenuItem;
    mOptionsDecimals0, mOptionsDecimals1, mOptionsDecimals2, mOptionsDecimals3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label4, Label5, Label6, laBloodVolAvg, Label8: TLabel;
    rbRefBook, rbGilcher, rbNadler, rbLBB: TRadioButton;
    cobIndividual, cobAppearance: TComboBox;
    edWeight, edHeight: TEdit;
    laWeight, laHeight: TLabel;
    laBloodVolTitle: TLabel;
    edBloodVolAvg, edBloodVolInd, edBloodVol: TEdit;
    laBloodVol, laUBloodVolAvg, laBloodVolInd, laUBloodVolInd: TLabel;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsPoundsClick(Sender: TObject);
    procedure mOptionsInchesClick(Sender: TObject);
    procedure mOptionsLitersClick(Sender: TObject);
    procedure mOptionsUsualInBVClick(Sender: TObject);
    procedure mOptionsDecimals0Click(Sender: TObject);
    procedure mOptionsDecimals1Click(Sender: TObject);
    procedure mOptionsDecimals2Click(Sender: TObject);
    procedure mOptionsDecimals3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure rbRefBookChange(Sender: TObject);
    procedure rbGilcherChange(Sender: TObject);
    procedure rbNadlerChange(Sender: TObject);
    procedure rbLBBChange(Sender: TObject);
    procedure cobIndividualChange(Sender: TObject);
    procedure cobAppearanceChange(Sender: TObject);
  private
    iDecimals: Integer;
    rWeight, rHeight, rVol, rVolAvg, rVolInd, rMultVol: Real;
    sMethod: string;
  end;

const
  // Individual's specifics (to be used as combobox items)
  AllIndividuals: array[0..10] of string = (
    'Adult (male)', 'Adult (female)', 'Adolescent (male)', 'Adolescent (female)',
    'Child', 'Infant', 'Newborn (full-term)', 'Newborn (premature)', 'Adult (male)', 'Adult (female)', 'Adult'
  );

var
  fVBlood: TfVBlood;

implementation

{$R *.lfm}

{ Function: Cube of real number }

function Cube(R: Real): Real;

begin
  Result := R * R * R;
end;

{ Function: Get real number rounded to given number of decimal digits }

function DecimalFormat(R: Real; Decimals: Integer): Real;

var
  Mult10, I: Integer;

begin
  if Decimals = 0 then
    R := Round(R)
  else begin
    Mult10 := 1;
    for I := 1 to Decimals do
      Mult10 *= 10;
    R := Round(Mult10 * R) / Mult10;
  end;
  Result := R;
end;

{ Fill the individual's specifics combobox, depending on TBV calculation method }

procedure FillSpecifics(Method: string);

var
  I1, I2, I: Integer;

begin
  if Method = 'refbook' then begin
    I1 := 0; I2 := 7;
  end
  else if Method = 'lbb' then begin
    I1 := 10; I2 := 10;
  end
  else begin
    I1 := 8; I2 := 9;
  end;
  fVBlood.cobIndividual.Clear;
  for I := I1 to I2 do
    fVBlood.cobIndividual.Items.AddText(AllIndividuals[I]);
  fVBlood.cobIndividual.ItemIndex := 0;
end;

{ Get average blood volume for given individuals group }

procedure BloodVolumePerKg(Method, Individual, Appearance: string; out Vol: Real);

const
  // Reference Book method values in ml/kg (values are common averages, of course)
  VolMan     = 75.00; VolWoman     = 65.00;
  VolBoy     = 70.00; VolGirl      = 65.00;
  VolChild   = 75.00; VolInfant    = 80.00;
  VolNewborn = 85.00; VolPremature = 96.00;
  // Gilcher's Rule of Five physical appearance adjustment values in ml/kg (muscular, normal, thin, obese)
  VolMen5: array[0..3] of Real = (
    75.00, 70.00, 65.00, 60.00
  );
  VolWomen5: array[0..3] of Real = (
    70.00, 65.00, 60.00, 55.00
  );

var
  IX: Integer;

begin
  Vol := 0;
  if Method = 'refbook' then begin
    // Reference book method: blood average value, depending on sex and age
    if Individual = AllIndividuals[0] then
      Vol := VolMan
    else if Individual = AllIndividuals[1] then
      Vol := VolWoman
    else if Individual = AllIndividuals[2] then
      Vol := VolBoy
    else if Individual = AllIndividuals[3] then
      Vol := VolGirl
    else if Individual = AllIndividuals[4] then
      Vol := VolChild
    else if Individual = AllIndividuals[5] then
      Vol := VolInfant
    else if Individual = AllIndividuals[6] then
      Vol := VolNewBorn
    else if Individual = AllIndividuals[7] then
      Vol := VolPremature;
  end
  else if Method = 'gilcher' then begin
    // Gilcher's Rule of Five: blood average value, depending on (adult) individual's sex and appearance
    IX := fVBlood.cobAppearance.Items.IndexOf(Appearance);                               // get combobox index for actual text = index of AverageVol arrays
    if Individual = 'Adult (male)' then
      Vol := VolMen5[IX]
    else
      Vol := VolWomen5[IX];
  end;
end;

{************}
{* TfVBlood *}
{************}

{ Application start: Initialisation }

procedure TfVBlood.FormCreate(Sender: TObject);

begin
  // Move InBV fields at same position as AverageBV fileds (and hide them)
  laBloodVolInd.Left := laBloodVolAvg.Left; edBloodVolInd.Left := edBloodVolAvg.Left; laUBloodVolInd.Left := laUBloodVolAvg.Left;
  laBloodVolInd.Visible := False; edBloodVolInd.Visible := False; laUBloodVolInd.Visible := False;
  // Default values at application startup
  sMethod := 'refbook'; rMultVol := 1; iDecimals := 2;
end;

{ Menu item "File > Exit": Exit application }

procedure TfVBlood.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Weight in pounds": Change weight labels on form accordingly to kg/lb selection }

procedure TfVBlood.mOptionsPoundsClick(Sender: TObject);

begin
  if mOptionsPounds.Checked then begin
    // "Weight in pounds" to be unchecked: change weight unit to kg
    mOptionsPounds.Checked := False;
    laWeight.Caption := 'kg';
    if mOptionsLiters.Checked then begin
      laUBloodVolAvg.Caption := 'l/kg';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'l/kg';
    end
    else begin
      laUBloodVolAvg.Caption := 'ml/kg';
      laUBloodVolInd.Caption := 'ml/kg';
    end;
  end
  else begin
    // "Weight in pounds" to be checked: change weight unit to lb
    mOptionsPounds.Checked := True;
    laWeight.Caption := 'lb';
    if mOptionsLiters.Checked then begin
      laUBloodVolAvg.Caption := 'l/lb';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'l/lb';
    end
    else begin
      laUBloodVolAvg.Caption := 'ml/lb';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'ml/lb';
    end;
  end;
  edBloodVol.Text := ''; edBloodVolAvg.Text := ''; edBloodVolInd.Text := '';
end;

{ Menu item "Options > Height in inches": Change height labels on form accordingly to cm/in selection }

procedure TfVBlood.mOptionsInchesClick(Sender: TObject);

begin
  if mOptionsInches.Checked then begin
    // "Height in inches" to be unchecked: change height unit to cm
    mOptionsInches.Checked := False;
    laHeight.Caption := 'cm';
  end
  else begin
    // "Height in inches" to be checked: change height unit to in
    mOptionsInches.Checked := True;
    laHeight.Caption := 'in';
  end;
  edBloodVol.Text := ''; edBloodVolAvg.Text := ''; edBloodVolInd.Text := '';
end;

{ Menu item "Options > Volume in liters": Change volume labels on form accordingly to ml/l selection }

procedure TfVBlood.mOptionsLitersClick(Sender: TObject);

begin
  if mOptionsLiters.Checked then begin
    // "Volume in liters" to be unchecked: change volume unit to ml
    mOptionsLiters.Checked := False;
    rMultVol := 1;
    laBloodVol.Caption := 'ml';
    if mOptionsPounds.Checked then begin
      laUBloodVolAvg.Caption := 'ml/lb';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'ml/lb';
    end
    else begin
      laUBloodVolAvg.Caption := 'ml/kg';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'ml/kg';
    end;
  end
  else begin
    // "Volume in liters" to be checked: change volume unit to l
    mOptionsLiters.Checked := True;
    rMultVol := 0.001;
    laBloodVol.Caption := 'l';
    if mOptionsPounds.Checked then begin
      laUBloodVolAvg.Caption := 'l/lb';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'l/lb';
    end
    else begin
      laUBloodVolAvg.Caption := 'l/kg';
      if not mOptionsUsualInBV.Checked then
        laUBloodVolInd.Caption := 'l/kg';
    end;
  end;
  edBloodVolAvg.Text := ''; edBloodVol.Text := ''; edBloodVolInd.Text := '';
end;

{ Menu item "Options > InBV always in ml/kg": Toggle InBV unit usual ml/kg or not }

procedure TfVBlood.mOptionsUsualInBVClick(Sender: TObject);

begin
  if mOptionsUsualInBV.Checked then begin
    // InBV always in ml/kg to be unchecked: Use unit, depending on actual volume and weight units
    mOptionsUsualInBV.Checked := False;
    if mOptionsPounds.Checked then begin
      if mOptionsLiters.Checked then
        laUBloodVolInd.Caption := 'l/lb'
      else
        laUBloodVolInd.Caption := 'ml/lb';
    end
    else begin
      if mOptionsLiters.Checked then
        laUBloodVolInd.Caption := 'l/kg'
      else
        laUBloodVolInd.Caption := 'ml/kg';
    end;
  end
  else begin
    // InBV always in ml/kg to be checked: Use ml/kg
    mOptionsUsualInBV.Checked := True;
    laUBloodVolInd.Caption := 'ml/kg';
  end;
  edBloodVol.Text := ''; edBloodVolAvg.Text := ''; edBloodVolInd.Text := '';
end;

{ Menu items "Options > Decimal precision > ...": Select number of decimal digits (0 - 3) }

procedure TfVBlood.mOptionsDecimals0Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := True;  mOptionsDecimals1.Checked := False;
  mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := False;
  iDecimals := 0;
end;

procedure TfVBlood.mOptionsDecimals1Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := False; mOptionsDecimals1.Checked := True;
  mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := False;
  iDecimals := 1;
end;

procedure TfVBlood.mOptionsDecimals2Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := False; mOptionsDecimals1.Checked := False;
  mOptionsDecimals2.Checked := True;  mOptionsDecimals3.Checked := False;
  iDecimals := 2;
end;

procedure TfVBlood.mOptionsDecimals3Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := False; mOptionsDecimals1.Checked := False;
  mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := True;
  iDecimals := 3;
end;

{ Menu item "Help > Help": Display application help }

procedure TfVBlood.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfVBlood.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Determination of the total blood volume (TBV), using the reference book method, ';
  S += 'Gilcher''s Rule of Five, Nadler''s equation or the Lemmens-Bernstein-Brodsky equation.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, May 2018 - October 2020.';
  MessageDlg('About "BloodVolume"', S, mtInformation, [mbOK], 0);
end;

{ Button "Compute": Compute TBV for given individual, using method selected }

procedure TfVBlood.btCalcClick(Sender: TObject);

var
  WeightKg, WeightLb, HeightCm, HeightIn, BMI: Real;
  ErrMess: string;

begin
  ErrMess := '';
  // Get weight entered by user
  if edWeight.Text = '' then
    rWeight := 0
  else
    rWeight := StrToFloat(edWeight.Text);
  if mOptionsPounds.Checked then begin
    WeightLb := rWeight; WeightKg := rWeight / 2.2;
  end
  else begin
    WeightKg := rWeight; WeightLb := rWeight * 2.2;
  end;
  // If Nadler's or the Lemmens-Bernstein-Brodsky equation is used, get height entered by user
  if rbNadler.Checked or rbLBB.Checked then begin
    if edHeight.Text = '' then
      rHeight := 0
    else
    rHeight := StrToFloat(edHeight.Text);
    if mOptionsInches.Checked then begin
      HeightIn := rHeight; HeightCm := rHeight * 2.54;
    end
    else begin
      HeightCm := rHeight; HeightIn := rHeight / 2.54;
    end;
  end;
  // Check weight and height validity (just checked if greater than 0!)
  if WeightKg <= 0 then begin
    ErrMess := 'Individual''s weight must be greater than 0';
    edWeight.SetFocus;
  end
  else begin
    if rbNadler.Checked or rbLBB.Checked then begin
      if HeightCm <= 0 then begin
        ErrMess := 'Individual''s height must be greater than 0';
        edHeight.SetFocus;
      end;
    end;
  end;
  // If weight and height values ok, continue...
  if ErrMess = '' then begin
    rVolAvg := 0; rVolInd := 0; rVol := 0;
    // Reference Book method or Gilcher's Rule of Five
    // -----------------------------------------------
    if (sMethod = 'refbook') or (sMethod = 'gilcher') then begin
      // Do TBV estimation, based on estimated average blood volume
      BloodVolumePerKg(sMethod, cobIndividual.Text, cobAppearance.Text, rVolAvg);        // get average blood volume form table
      if mOptionsPounds.Checked then
        rVolAvg := rVolAvg / 2.2;                                                        // convert to ml/lb
      edBloodVolAvg.Text := FloatToStr(DecimalFormat(rVolAvg * rMultVol, iDecimals));
      if mOptionsPounds.Checked then
        rVol := WeightLb * rVolAvg                                                       // calculate TBV (with weight in lb)
      else
        rVol := WeightKg * rVolAvg;                                                      // calculate TBV (with weight in kg)
    end
    // Nadler's equation
    // -----------------
    else if sMethod = 'nadler' then begin
      if cobIndividual.Text = AllIndividuals[8] then begin
        // Nadler's equation for men
        rVol := (0.006012 * Cube(HeightIn)) + (14.6 * WeightLb) + 604;                   // weight in pounds, height in inches!
      end
      else begin
        // Nadler's equation for women
        rVol := (0.005835 * Cube(HeightIn)) + (15 * WeightLb) + 183;                     // weight in pounds, height in inches!
      end
    end
    // Lemmens-Bernstein-Brodsky equation
    // ----------------------------------
    else begin
      BMI := WeightKg / Sqr(HeightCm * 1E-2);
      rVolInd := 70 / (Sqrt(BMI / 22));
      rVol := rVolInd * WeightKg;
      if not mOptionsUsualInBV.Checked then begin
        // Display InBV in actual volume and weight units (not common to do so)
        if laUBloodVolInd.Caption = 'l/kg' then
          rVolInd /= 1000
        else if laUBloodVolInd.Caption = 'ml/lb' then
          rVolInd /= 2.2
        else if laUBloodVolInd.Caption = 'l/lb' then
          rVolInd /= 1000 * 2.2;
      end;
      edBloodVolInd.Text := FloatToStr(DecimalFormat(rVolInd, iDecimals));
    end;
    // Display estimated or calculated TBV
    if rVol = 0 then
      edBloodVol.Text := ''
    else
      edBloodVol.Text := FloatToStr(DecimalFormat(rVol * rMultVol, iDecimals));
  end
  // Invalid user data
  else begin
    MessageDlg('Invalid data', ErrMess + '!', mtError, [mbOK], 0);
    edBloodVolAvg.Text := ''; edBloodVolInd.Text := ''; edBloodVol.Text := '';
  end;
end;

{ User selection of calculation method (radiobutton changes): Adapt form controls }

procedure TfVBlood.rbRefBookChange(Sender: TObject);

begin
  if rbRefBook.Checked then begin
    sMethod := 'refbook'; FillSpecifics(sMethod);
    cobAppearance.ItemIndex := -1; cobAppearance.Enabled := False;
    edHeight.Text := ''; edHeight.Enabled := False;
    laBloodVolTitle.Caption := 'Estimated blood volume:';
    laBloodVolAvg.Visible := True; laUBloodVolAvg.Visible := True; edBloodVolAvg.Visible := True;
    edBloodVolAvg.Enabled := True; edBloodVolAvg.Text := '';
    laBloodVolInd.Visible := False; laUBloodVolInd.Visible := False; edBloodVolInd.Visible := False;
    edBloodVol.Text := '';
  end;
end;

procedure TfVBlood.rbGilcherChange(Sender: TObject);

begin
  if rbGilcher.Checked then begin
    sMethod := 'gilcher'; FillSpecifics(sMethod);
    cobAppearance.ItemIndex := 0; cobAppearance.Enabled := True;
    edHeight.Text := ''; edHeight.Enabled := False;
    laBloodVolTitle.Caption := 'Estimated blood volume:';
    laBloodVolAvg.Visible := True; laUBloodVolAvg.Visible := True; edBloodVolAvg.Visible := True;
    edBloodVolAvg.Enabled := True; edBloodVolAvg.Text := '';
    laBloodVolInd.Visible := False; laUBloodVolInd.Visible := False; edBloodVolInd.Visible := False;
    edBloodVol.Text := '';
  end;
end;

procedure TfVBlood.rbNadlerChange(Sender: TObject);

begin
  if rbNadler.Checked then begin
    sMethod := 'nadler'; FillSpecifics(sMethod);
    cobAppearance.ItemIndex := -1; cobAppearance.Enabled := False;
    edHeight.Enabled := True;
    laBloodVolTitle.Caption := 'Calculated blood volume:';
    laBloodVolAvg.Visible := True; laUBloodVolAvg.Visible := True; edBloodVolAvg.Visible := True;
    edBloodVolAvg.Enabled := False; edBloodVolAvg.Text := '';
    laBloodVolInd.Visible := False; laUBloodVolInd.Visible := False; edBloodVolInd.Visible := False;
    edBloodVol.Text := '';
  end;
end;

procedure TfVBlood.rbLBBChange(Sender: TObject);

begin
  if rbLBB.Checked then begin
    sMethod := 'lbb'; FillSpecifics(sMethod);
    cobAppearance.ItemIndex := -1; cobAppearance.Enabled := False;
    edHeight.Enabled := True;
    laBloodVolTitle.Caption := 'Calculated blood volume:';
    laBloodVolAvg.Visible := False; laUBloodVolAvg.Visible := False; edBloodVolAvg.Visible := False;
    laBloodVolInd.Visible := True; laUBloodVolInd.Visible := True; edBloodVolInd.Visible := True;
    edBloodVolInd.Text := '';
    edBloodVol.Text := '';
    if mOptionsUsualInBV.Checked then
      laUBloodVolInd.Caption := 'ml/kg';
  end;
end;

{ User selection of individua's specifics (comboboxes changes): Clear blood volume fields }

procedure TfVBlood.cobIndividualChange(Sender: TObject);

begin
  edBloodVolAvg.Text := ''; edBloodVol.Text := '';
end;

procedure TfVBlood.cobAppearanceChange(Sender: TObject);

begin
  edBloodVolAvg.Text := ''; edBloodVol.Text := '';
end;

end.

