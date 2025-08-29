{****************************************************}
{* Adder circuits unit for OpAmplifiers application *}
{****************************************************}

unit opamp4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, ComCtrls;

type
  {**********}
  { TfOpAmp4 }
  {**********}
  TfOpAmp4 = class(TForm)
    mMenu: TMainMenu;
    mWindow, mWindowClose: TMenuItem;
    mOptions, mOptionsInputs, mOptionsInputs2, mOptionsInputs3, mOptionsTrackBar: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    Label10, Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18: TLabel;
    Shape1: TShape;
    imAmp: TImage;
    laVIn3, laR3, laUR3, laI3, laUI3: TLabel;
    edVIn1, edVIn2, edVIn3, edVOut: TEdit;
    edR1, edR2, edR3, edRf: TEdit;
    edVA, edI1, edI2, edI3, edIf: TEdit;
    btStart: TButton;
    tbVIn1, tbVIn2, tbVIn3: TTrackBar;
    procedure FormActivate(Sender: TObject);
    procedure mWindowCloseClick(Sender: TObject);
    procedure mOptionsInputs2Click(Sender: TObject);
    procedure mOptionsInputs3Click(Sender: TObject);
    procedure mOptionsTrackBarClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tbVIn1Change(Sender: TObject);
    procedure tbVIn2Change(Sender: TObject);
    procedure tbVIn3Change(Sender: TObject);
  private
    iInputs, iOldPos1, iOldPos2, iOldPos3: Integer;
    bResetR: Boolean;
  public
    rVCC: Real;
  end;

var
  fOpAmp4: TfOpAmp4;

implementation

{$R *.lfm}

{ Reset circuit parameters (on form) }

procedure ResetForm(ResetR: Boolean; out OldPos1, OldPos2, OldPos3: Integer);

begin
  fOpAmp4.edVIn1.Text := '0,00'; fOpAmp4.edVIn2.Text := '0,00'; fOpAmp4.edVIn3.Text := '0,00'; fOpAmp4.edVOut.Text := '';
  fOpAmp4.edR1.Enabled := True; fOpAmp4.edR2.Enabled := True; fOpAmp4.edR3.Enabled := True; fOpAmp4.edRf.Enabled := True;
  if ResetR then begin
    // Reset these resistances only if ResetR argument is true
    fOpAmp4.edR1.Text := ''; fOpAmp4.edR2.Text := ''; fOpAmp4.edRf.Text := '';
  end;
  fOpAmp4.edR3.Text := '';
  fOpAmp4.edVA.Text := ''; fOpAmp4.edI1.Text := ''; fOpAmp4.edI2.Text := ''; fOpAmp4.edI3.Text := ''; fOpAmp4.edIf.Text := '';
  // Set trackbars to 0V
  fOpAmp4.tbVIn1.Position := 0; fOpAmp4.tbVIn2.Position := 0; fOpAmp4.tbVIn3.Position := 0;
  OldPos1 := 0; OldPos2 := 0; OldPos3 := 0;
  // Reset "Start" button
  if fOpAmp4.btStart.Caption <> 'Calculate' then
    fOpAmp4.btStart.Caption := 'Start';
end;

{ Calculate op-amp adder circuit output voltage (and other circuit values) }

procedure CalculateVOut(VCC: Real; Inputs: Integer; FieldVIn1, FieldVIn2, FieldVIn3, FieldR1, FieldR2, FieldR3, FieldRf: TEdit;
  var FieldVA, FieldI1, FieldI2, FieldI3, FieldIf, FieldVOut: TEdit; out Mess: string);

// The procedure reads all user parameters from the form fields and fills all calculated values into the form fields

var
  VSatPlus, VSatMinus, R1, R2, R3, Rf, VIn1, VIn2, VIn3, VA, J1, J2, J3, Jf, VOut: Real;

begin
  Mess := '';
  // Get user parameters
  if (FieldR1.Text = '') or (FieldR2.Text = '') or ((Inputs = 3) and (FieldR3.Text = '')) or (FieldRf.Text = '') then begin
    if FieldR1.Text = '' then begin
      Mess := 'Resistance R1 not specified!';
      FieldR1.SetFocus;
    end
    else if FieldR2.Text = '' then begin
      Mess := 'Resistance R2 not specified!';
      FieldR2.SetFocus;
    end
    else if (Inputs = 3) and (FieldR3.Text = '') then begin
      Mess := 'Resistance R3 not specified!';
      FieldR3.SetFocus;
    end
    else begin
      Mess := 'Resistance Rf not specified!';
      FieldRf.SetFocus;
    end;
  end
  else begin
    R1 := StrToFloat(FieldR1.Text); R2 := StrToFloat(FieldR2.Text);
    if Inputs = 3 then
      R3 := StrToFloat(FieldR3.Text);
    Rf := StrToFloat(FieldRf.Text);
    if (R1 <= 0) or (R2 <= 0) or ((Inputs = 3) and (R3 <= 0)) or (Rf <= 0) then begin
      Mess := 'Resistances must be greater than 0!';
      if R1 <= 0 then
        FieldR1.SetFocus
      else if R2 <= 0 then
        FieldR2.SetFocus
      else if (Inputs = 3) and (R3 <= 0) then
        FieldR3.SetFocus
      else
        FieldRf.SetFocus;
    end
    else if (FieldVIn1.Text = '') or (FieldVIn2.Text = '') or ((Inputs = 3) and (FieldVIn3.Text = '')) then begin
      if FieldVIn1.Text = '' then begin
        Mess := 'Input voltage Vin1 not specified!';
        FieldVIn1.SetFocus;
      end
      else if FieldVIn2.Text = '' then begin
        Mess := 'Input voltage Vin2 not specified!';
        FieldVIn2.SetFocus;
      end
      else if (Inputs = 3) and (FieldVIn3.Text = '') then begin
        Mess := 'Input voltage Vin3 not specified!';
        FieldVIn3.SetFocus;
      end;
    end;
    if Mess = '' then begin
      VIn1 := StrToFloat(FieldVIn1.Text); VIn2 := StrToFloat(FieldVIn2.Text);
      if Inputs = 3 then
        VIn3 := StrToFloat(FieldVIn3.Text);
      if (VIn1 < -VCC) or (VIn2 < -VCC) or ((Inputs = 3) and (VIn3 < -VCC)) then
        Mess := 'Input voltage should be greater than ' + FloatToStrF(-VCC, ffFixed, 0, 0) + 'V!'
      else if (VIn1 > VCC) or (VIn2 > VCC) or ((Inputs = 3) and (VIn3 > VCC)) then
        Mess := 'Input voltage should be less than ' + FloatToStrF(VCC, ffFixed, 0, 0) + 'V!';
      if (VIn1 < -VCC) or (VIn1 > VCC) then
        FieldVIn1.SetFocus
      else if (VIn2 < -VCC) or (VIn2 > VCC) then
        FieldVIn2.SetFocus
      else if (Inputs = 3) and ((VIn3 < -VCC) or (VIn3 > VCC)) then
        FieldVIn3.SetFocus;
    end;
    if Mess = '' then begin
      // Adder circuit calculations
      VSatPlus := VCC - 1; VSatMinus := -VCC + 1;
      R1 *= 1000; R2 *= 1000;
      if Inputs = 3 then
        R3 *= 1000;
      Rf *= 1000;
      VA := 0;
      J1 := VIn1 / R1; J2 := VIn2 / R2;
      if Inputs = 3 then begin
        J3 := VIn3 / R3;
        Jf := J1 + J2 + J3;
      end
      else
        Jf := J1 + J2;
      VOut := (Rf / R1) * VIn1 + (Rf / R2) * VIn2;                             // Vout for 2 input voltages
      if Inputs = 3 then
        VOut += (Rf / R3) * VIn3;                                              // term to add for 3rd input voltage (and similar, if there were further inputs)
      VOut := -VOut;                                                           // adder circuit output is inverted!
      if Abs(VOut) > VSatPlus then begin
        // Amplification can't exceed VSat
        if VOut > 0 then
          VOut := VSatPlus
        else
          VOut := VSatMinus;
        MessageDlg('Saturated output voltage', 'Vout is saturated. No correct voltage addition for actual parameters!', mtWarning, [mbOK], 0);
      end;
      FieldVA.Text := FloatToStrF(VA, ffFixed, 0, 2);
      FieldI1.Text := FloatToStrF(1000 * J1, ffFixed, 0, 2);
      FieldI2.Text := FloatToStrF(1000 * J2, ffFixed, 0, 2);
      if Inputs = 3 then
        FieldI3.Text := FloatToStrF(1000 * J3, ffFixed, 0, 2);
      FieldIf.Text := FloatToStrF(1000 * Jf, ffFixed, 0, 2);
      FieldVOut.Text := FloatToStrF(VOut, ffFixed, 0, 2);
    end;
  end;
end;

{**********}
{ TfOpAmp4 }
{**********}

{ Form show-up: Initialisation }

procedure TfOpAmp4.FormActivate(Sender: TObject);

begin
  bResetR := True;
  if mOptionsInputs2.Checked then
    mOptionsInputs2.Click
  else
    mOptionsInputs3.Click;
end;

{ Menu item "Window > Close": Close adder circuit window }

procedure TfOpAmp4.mWindowCloseClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of inputs > 2 inputs": Select adder with 2 inputs }

procedure TfOpAmp4.mOptionsInputs2Click(Sender: TObject);

var
  Filename: string;

begin
  mOptionsInputs2.Checked := True; mOptionsInputs3.Checked := False;
  iInputs := 2;
  // Adapt circuit picture
  Filename := './amp/amp4a.jpg'; DoDirSeparators(Filename);                    // adder with 2 inputs picture
  imAmp.Picture.LoadFromFile(Filename);
  // Reset form fields
  ResetForm(bResetR, iOldPos1, iOldPos2, iOldPos3); bResetR := False;
  // Hide all controls specific to 3-inputs adder
  edVIn3.Visible := False;
  edR3.Visible := False; laR3.Visible := False; laUR3.Visible := False;
  edI3.Visible := False; laI3.Visible := False; laUI3.Visible := False;
  laVIn3.Visible := False; tbVIn3.Visible := False;
end;

{ Menu item "Options > Number of inputs > 3 inputs": Select adder with 3 inputs }

procedure TfOpAmp4.mOptionsInputs3Click(Sender: TObject);

var
  Filename: string;

begin
  mOptionsInputs3.Checked := True; mOptionsInputs2.Checked := False;
  iInputs := 3;
  // Adapt circuit picture
  Filename := './amp/amp4b.jpg'; DoDirSeparators(Filename);                    // adder with 3 inputs picture
  imAmp.Picture.LoadFromFile(Filename);
  // Show all controls specific to 3-inputs adder
  edVIn3.Visible := True;
  edR3.Visible := True; laR3.Visible := True; laUR3.Visible := True;
  edI3.Visible := True; laI3.Visible := True; laUI3.Visible := True;
  laVIn3.Visible := True; tbVIn3.Visible := True;
  // Reset form fields
  ResetForm(bResetR, iOldPos1, iOldPos2, iOldPos3); bResetR := False;
end;

{ Menu item "Options > Use trackbar": Toggle between trackbar and manual input }

procedure TfOpAmp4.mOptionsTrackBarClick(Sender: TObject);

begin
  edVIn1.Text := '0,00'; edVIn2.Text := '0,00'; edVIn3.Text := '0,00'; edVOut.Text := '';
  tbVIn1.Position := 0; tbVIn2.Position := 0; tbVIn3.Position := 0;
  edVa.Text := ''; edIf.Text := '';
  edI1.Text := ''; edI2.Text := ''; edI3.Text := '';
  edR1.Enabled := True; edR2.Enabled := True; edR3.Enabled := True; edRf.Enabled := True;
  if mOptionsTrackBar.Checked then begin
    mOptionsTrackBar.Checked := False;
    tbVIn1.Enabled := False; tbVIn2.Enabled := False; tbVIn3.Enabled := False;
    edVIn1.ReadOnly := False; edVIn1.TabStop := True;
    edVIn2.ReadOnly := False; edVIn2.TabStop := True;
    edVIn3.ReadOnly := False; edVIn3.TabStop := True;
    edVIn1.SetFocus;
    btStart.Caption := 'Calculate';
  end
  else begin
    mOptionsTrackBar.Checked := True;
    tbVIn1.Enabled := True; tbVIn2.Enabled := True; tbVIn3.Enabled := True;
    edVIn1.ReadOnly := True; edVIn1.TabStop := False;
    edVIn2.ReadOnly := True; edVIn2.TabStop := False;
    edVIn3.ReadOnly := True; edVIn3.TabStop := False;
    btStart.Caption := 'Start';
    btStart.SetFocus;
  end;
end;

{ Button "Start/Stop/Calculate": Start/stop adder circuits simulation resp. do calculation for manually entered voltage }

procedure TfOpAmp4.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Button "Start" resp. "Calculate": Do calculations
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Calculate') then begin
    CalculateVOut(rVCC, iInputs, edVIn1, edVIn2, edVIn3, edR1, edR2, edR3, edRf, edVA, edI1, edI2, edI3, edIf, edVOut, Mess);
    if Mess = '' then begin
      if btStart.Caption = 'Start' then begin
        edR1.Enabled := False; edR2.Enabled := False; edR3.Enabled := False; edRf.Enabled := False;
        btStart.Caption := 'Stop';
      end;
    end
    else
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end
  // Button "Stop": Stop simulation
  else begin
    edR1.Enabled := True; edR2.Enabled := True; edR3.Enabled := True; edRf.Enabled := True;
    btStart.Caption := 'Start';
  end;
end;

{ Trackbars position change: Get Vin and update calculations for this value }

procedure TfOpAmp4.tbVIn1Change(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn1.Text := FloatToStrF(tbVIn1.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, iInputs, edVIn1, edVIn2, edVIn3, edR1, edR2, edR3, edRf, edVA, edI1, edI2, edI3, edIf, edVOut, Mess);
      iOldPos1 := tbVIn1.Position;
    end
    else
      // Trackbar only active if simulation is started
      tbVIn1.Position := iOldPos1;
  end;
end;

procedure TfOpAmp4.tbVIn2Change(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn2.Text := FloatToStrF(tbVIn2.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, iInputs, edVIn1, edVIn2, edVIn3, edR1, edR2, edR3, edRf, edVA, edI1, edI2, edI3, edIf, edVOut, Mess);
      iOldPos2 := tbVIn2.Position;
    end
    else
      // Trackbar only active if simulation is started
      tbVIn2.Position := iOldPos2;
  end;
end;

procedure TfOpAmp4.tbVIn3Change(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn3.Text := FloatToStrF(tbVIn3.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, iInputs, edVIn1, edVIn2, edVIn3, edR1, edR2, edR3, edRf, edVA, edI1, edI2, edI3, edIf, edVOut, Mess);
      iOldPos3 := tbVIn3.Position;
    end
    else
      // Trackbar only active if simulation is started
      tbVIn3.Position := iOldPos3;
  end;
end;

end.

