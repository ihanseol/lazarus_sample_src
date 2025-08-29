{*************************************************************}
{* Differential amplifiers unit for OpAmplifiers application *}
{*************************************************************}

unit opamp5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus;

type
  {**********}
  { TfOpAmp5 }
  {**********}
  TfOpAmp5 = class(TForm)
    mMenu: TMainMenu;
    mWindow, mWindowClose: TMenuItem;
    mOptions, mOptionsTrackBar: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9: TLabel;
    Label10, Label11, Label12, Label13, Label14, Label16, Label17, Label18: TLabel;
    Shape1: TShape;
    imAmp: TImage;
    edVIn1, edVIn2, edVOut: TEdit;
    edR1, edR2, edRf: TEdit;
    edVA, edVB, edI1, edI2: TEdit;
    btStart: TButton;
    tbVIn1, tbVIn2: TTrackBar;
    procedure FormActivate(Sender: TObject);
    procedure mWindowCloseClick(Sender: TObject);
    procedure mOptionsTrackBarClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tbVIn1Change(Sender: TObject);
    procedure tbVIn2Change(Sender: TObject);
  private
    iOldPos1, iOldPos2: Integer;
  public
    rVCC: Real;
  end;

var
  fOpAmp5: TfOpAmp5;

implementation

{$R *.lfm}

{ Calculate op-amp differential amplifier circuit output voltage (and other circuit values) }

procedure CalculateVOut(VCC: Real; FieldVIn1, FieldVIn2, FieldR1, FieldR2, FieldRf: TEdit; var FieldVA, FieldVB, FieldI1, FieldI2, FieldVOut: TEdit; out Mess: string);

// The procedure reads all user parameters from the form fields and fills all calculated values into the form fields

var
  VSatPlus, VSatMinus, R1, R2, Rf, VIn1, VIn2, VA, VB, J1, J2, VOut: Real;

begin
  Mess := '';
  // Get user parameters
  if (FieldR1.Text = '') or (FieldR2.Text = '') or (FieldRf.Text = '') then begin
    if FieldR1.Text = '' then begin
      Mess := 'Resistance R1 not specified!';
      FieldR1.SetFocus;
    end
    else if FieldR2.Text = '' then begin
      Mess := 'Resistance R2 not specified!';
      FieldR2.SetFocus;
    end
    else begin
      Mess := 'Resistance Rf not specified!';
      FieldRf.SetFocus;
    end;
  end
  else begin
    R1 := StrToFloat(FieldR1.Text); R2 := StrToFloat(FieldR2.Text); Rf := StrToFloat(FieldRf.Text);
    if (R1 <= 0) or (R2 <= 0) or (Rf <= 0) then begin
      Mess := 'Resistances must be greater than 0!';
      if R1 <= 0 then
        FieldR1.SetFocus
      else if R2 <= 0 then
        FieldR2.SetFocus
      else
        FieldRf.SetFocus;
    end
    else if (FieldVIn1.Text = '') or (FieldVIn2.Text = '') then begin
      if FieldVIn1.Text = '' then begin
        Mess := 'Input voltage Vin1 not specified!';
        FieldVIn1.SetFocus;
      end
      else if FieldVIn2.Text = '' then begin
        Mess := 'Input voltage Vin2 not specified!';
        FieldVIn2.SetFocus;
      end;
    end;
    if Mess = '' then begin
      VIn1 := StrToFloat(FieldVIn1.Text); VIn2 := StrToFloat(FieldVIn2.Text);
      if (VIn1 < -VCC) or (VIn2 < -VCC) then
        Mess := 'Input voltage should be greater than ' + FloatToStrF(-VCC, ffFixed, 0, 0) + 'V!'
      else if (VIn1 > VCC) or (VIn2 > VCC) then
        Mess := 'Input voltage should be less than ' + FloatToStrF(VCC, ffFixed, 0, 0) + 'V!';
      if (VIn1 < -VCC) or (VIn1 > VCC) then
        FieldVIn1.SetFocus
      else if (VIn2 < -VCC) or (VIn2 > VCC) then
        FieldVIn2.SetFocus;
    end;
    if Mess = '' then begin
      // Differential amplifier calculations
      VSatPlus := VCC - 1; VSatMinus := -VCC + 1;
      R1 *= 1000; R2 *= 1000; Rf *= 1000;
      VB := (R2 / (R1 + R2)) * VIn2; VA := VB;
      J1 := (VIn1 - VA) / R1; J2 := J1;
      VOut := (R2 / R1) * (VIn2 - VIn1);
      if Abs(VOut) > VSatPlus then begin
        // Amplification can't exceed VSat
        if VOut > 0 then
          VOut := VSatPlus
        else
          VOut := VSatMinus;
        MessageDlg('Saturated output voltage', 'Vout is saturated. No correct voltage addition for actual parameters!', mtWarning, [mbOK], 0);
      end;
      FieldVA.Text := FloatToStrF(VA, ffFixed, 0, 2);
      FieldVB.Text := FloatToStrF(VB, ffFixed, 0, 2);
      FieldI1.Text := FloatToStrF(1000 * J1, ffFixed, 0, 2);
      FieldI2.Text := FloatToStrF(1000 * J2, ffFixed, 0, 2);
      FieldVOut.Text := FloatToStrF(VOut, ffFixed, 0, 2);
    end;
  end;
end;

{**********}
{ TfOpAmp5 }
{**********}

{ Form show-up: Initialisation }

procedure TfOpAmp5.FormActivate(Sender: TObject);

begin
  edVIn1.Text := '0,00'; edVIn2.Text := '0,00'; edVOut.Text := '';
  edR1.Enabled := True; edR2.Enabled := True; edRf.Enabled := True;
  edR1.Text := ''; edR2.Text := ''; edRf.Text := '';
  edVA.Text := ''; edVB.Text := '';
  edI1.Text := ''; edI2.Text := '';
  tbVIn1.Position := 0; tbVIn2.Position := 0;
  iOldPos1 := 0; iOldPos2 := 0;
  if btStart.Caption <> 'Calculate' then
    btStart.Caption := 'Start';
end;

{ Menu item "Window > Close": Close differential amplifier circuit window }

procedure TfOpAmp5.mWindowCloseClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Use trackbar": Toggle between trackbar and manual input }

procedure TfOpAmp5.mOptionsTrackBarClick(Sender: TObject);

begin
  edVIn1.Text := '0,00'; edVIn2.Text := '0,00'; edVOut.Text := '';
  tbVIn1.Position := 0; tbVIn2.Position := 0;
  edVA.Text := ''; edVB.Text := '';
  edI1.Text := ''; edI2.Text := '';
  edR1.Enabled := True; edR2.Enabled := True; edRf.Enabled := True;
  if mOptionsTrackBar.Checked then begin
    mOptionsTrackBar.Checked := False;
    tbVIn1.Enabled := False; tbVIn2.Enabled := False;
    edVIn1.ReadOnly := False; edVIn1.TabStop := True;
    edVIn2.ReadOnly := False; edVIn2.TabStop := True;
    edVIn1.SetFocus;
    btStart.Caption := 'Calculate';
  end
  else begin
    mOptionsTrackBar.Checked := True;
    tbVIn1.Enabled := True; tbVIn2.Enabled := True;
    edVIn1.ReadOnly := True; edVIn1.TabStop := False;
    edVIn2.ReadOnly := True; edVIn2.TabStop := False;
    btStart.Caption := 'Start';
    btStart.SetFocus;
  end;
end;

{ Button "Start/Stop/Calculate": Start/stop differential amplifier simulation resp. do calculation for manually entered voltage }

procedure TfOpAmp5.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Button "Start" resp. "Calculate": Do calculations
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Calculate') then begin
    CalculateVOut(rVCC, edVIn1, edVIn2, edR1, edR2, edRf, edVA, edVB, edI1, edI2, edVOut, Mess);
    if Mess = '' then begin
      if btStart.Caption = 'Start' then begin
        edR1.Enabled := False; edR2.Enabled := False; edRf.Enabled := False;
        btStart.Caption := 'Stop';
      end;
    end
    else
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end
  // Button "Stop": Stop simulation
  else begin
    edR1.Enabled := True; edR2.Enabled := True; edRf.Enabled := True;
    btStart.Caption := 'Start';
  end;
end;

{ Trackbars position change: Get Vin and update calculations for this value }

procedure TfOpAmp5.tbVIn1Change(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn1.Text := FloatToStrF(tbVIn1.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, edVIn1, edVIn2, edR1, edR2, edRf, edVA, edVB, edI1, edI2, edVOut, Mess);
      iOldPos1 := tbVIn1.Position;
    end
    else
      // Trackbar only active if simulation is started
      tbVIn1.Position := iOldPos1;
  end;
end;

procedure TfOpAmp5.tbVIn2Change(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn2.Text := FloatToStrF(tbVIn2.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, edVIn1, edVIn2, edR1, edR2, edRf, edVA, edVB, edI1, edI2, edVOut, Mess);
      iOldPos2 := tbVIn2.Position;
    end
    else
      // Trackbar only active if simulation is started
      tbVIn2.Position := iOldPos2;
  end;
end;

end.

