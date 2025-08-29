{****************************************************}
{* Charge/discharge unit for Capacitors application *}
{****************************************************}

unit capacitors_u3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  {******}
  { TfDC }
  {******}
  TfDC = class(TForm)
    stTitle: TStaticText;
    Label1: TLabel;
    rbCharge, rbDischarge: TRadioButton;
    imCalc: TImage;
    laVS, laCapacitance, laR: TLabel;
    laV0, laI0, laTau, laTCharge: TLabel;
    edVS, edC, edR: TEdit;
    edV0, edI0, edTau, edTCharge: TEdit;
    edT, edV, edI: TEdit;
    cobUVS, cobUC, cobUR, cobUT: TComboBox;
    laUV0, laUI0, laUTau, laUTCharge, laUV, laUI: TLabel;
    btCalc: TButton;
    btTau: TButton;
    btClear: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure rbChargeChange(Sender: TObject);
    procedure rbDischargeChange(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btTauClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
   rVS, rR, rC, rV0, rI0, rTau, rTC, rT, rV, rI: Double;
  end;

const
  SUB_0 = #$E2#$82#$80;

var
  fDC: TfDC;

implementation

{$R *.lfm}

{ Clear form fields }

procedure ClearForm(ClearAll: Boolean);

begin
  if ClearAll then begin
    // Clear user input fields only if ClearAll=True
    fDC.edVS.Text := ''; fDC.edR.Text := ''; fDC.edC.Text := '';
    fDC.edT.Text := '';
  end;
  fDC.edV0.Text := ''; fDC.edI0.Text := '';
  fDC.edTau.Text := ''; fDC.edTCharge.Text := '';
  fDC.edV.Text := ''; fDC.edI.Text := '';
  if fDC.edVS.Text = '' then
    fDC.edVS.SetFocus
  else
    fDC.btCalc.SetFocus;
end;

{******}
{ TfDC }
{******}

{ Application start: Initialisation }

procedure TfDC.FormCreate(Sender: TObject);

begin
  laV0.Caption := StringReplace(laV0.Caption, '0', SUB_0, []);
  laI0.Caption := StringReplace(laI0.Caption, '0', SUB_0, []);
end;

{ Button "Calculation": Calculate capacitor charge/discharge values }

procedure TfDC.btCalcClick(Sender: TObject);

var
  P: Integer;
  ST, Mess: string;

begin
  // Read user input values from form
  rVS := -1; rR := -1; rC := -1; rT := -1;
  if edVS.Text <> '' then begin
    rVS := StrToFloat(edVS.Text);
    if cobUVS.ItemIndex = 1 then
      rVS *= 1E-3;                                                             // voltage was given in mV
  end;
  if edR.Text <> '' then begin
    rR := StrToFloat(edR.Text);
    case cobUR.ItemIndex of
      1: rR *= 1E+3;                                                           // resistance was given in kΩ
      2: rR *= 1E+6;                                                           // resistance was given in MΩ
    end;
  end;
  if edC.Text <> '' then begin
    rC := StrToFloat(edC.Text);
    case cobUC.ItemIndex of
      0: rC *= 1E-6;                                                           // capacitance was given in µF
      1: rC *= 1E-9;                                                           // capacitance was given in nF
      2: rC *= 1E-12;                                                          // capacitance was given in pF
    end;
  end;
  if edT.Text <> '' then begin
    // Check if time value is a number or some value incl. the letter τ
    P := Pos('τ', edT.Text);
    if P = 0 then begin
      // Numeric time value
      cobUT.Visible := True;
      rT := StrToFloat(edT.Text);
      if cobUT.ItemIndex = 1 then
        rT *= 1E-3;                                                            // time was given in ms
    end
    else begin
      // Time is multiple of τ
      cobUT.Visible := False;
      rT := 0;                                                                 // get corr. time value later...
    end;
  end;
  // Check user input values
  if rVS <= 0 then begin
    Mess := 'Source voltage V must be greater than 0';
    edVS.SetFocus;
  end
  else if rR <= 0 then begin
    Mess := 'Resistance R must be greater than 0';
    edR.SetFocus;
  end
  else if rC <= 0 then begin
    Mess := 'Capacitance C must be greater than 0';
    edC.SetFocus;
  end
  else if (edT.Text <> '') and (rT < 0) then begin                             // the "Time" field may be left blank (no calculation done in this case)
    Mess := 'Time must be greater than or equal to 0';
    edT.SetFocus;
  end;
  // Do DC circuit charge/discharge calculations
  if Mess = '' then begin
    rV0 := rVS; rI0 := rV0 / rR;
    rTau := rR * rC; rTC := 5 * rTau;
    if rbCharge.Checked then begin
      laTCharge.Caption := 'Charging time';
    end
    else begin
      laTCharge.Caption := 'Discharging time';
      rI0 := -rI0;
    end;
    if rV0 > 1 then begin
      edV0.Text := FloatToStrF(rV0, ffFixed, 0, 3);
      laUV0.Caption := 'V';
    end
    else begin
      edV0.Text := FloatToStrF(rV0 * 1E+3, ffFixed, 0, 3);
      laUV0.Caption := 'mV';
    end;
    if Abs(rI0) > 1 then begin
      edI0.Text := FloatToStrF(rI0, ffFixed, 0, 3);
      laUI0.Caption := 'A';
    end
    else if Abs(rI0 * 1E+3) > 1 then begin
      edI0.Text := FloatToStrF(rI0 * 1E+3, ffFixed, 0, 3);
      laUI0.Caption := 'mA';
    end
    else begin
      edI0.Text := FloatToStrF(rI0 * 1E+6, ffFixed, 0, 3);
      laUI0.Caption := 'µA';
    end;
    if rTC > 1 then begin
      edTau.Text := FloatToStrF(rTau, ffFixed, 0, 3);
      edTCharge.Text := FloatToStrF(rTC, ffFixed, 0, 3);
      laUTau.Caption := 's'; laUTCharge.Caption := 's';
    end
    else begin
      edTau.Text := FloatToStrF(rTau * 1E+3, ffFixed, 0, 3);
      edTCharge.Text := FloatToStrF(rTC * 1E+3, ffFixed, 0, 3);
      laUTau.Caption := 'ms'; laUTCharge.Caption := 'ms';
    end;
    // Calculate instant voltage and current only if the time field is not empty
    if edT.Text <> '' then begin
      // Calculate time value for time input as a multiple of τ
      P := Pos('τ', edT.Text);
      if P <> 0 then begin
        ST := StringReplace(edT.Text, 'τ', '', []);
        if ST = '' then
          rT := rTau
        else
          rT := StrToFloat(ST) * rTau;
        cobUT.Text := laUTau.Caption;
      end;
      if rbCharge.Checked then begin
        rV := rV0 * (1 - Exp(-rT / rTau));
        rI := rI0 * Exp(-rT / rTau);
      end
      else begin
        rV := rV0 * Exp(-rT / rTau);
        rI := -Abs(rI0) * Exp(-rT / rTau);
      end;
      if rV > 1 then begin
        edV.Text := FloatToStrF(rV, ffFixed, 0, 3);
        laUV.Caption := 'V';
      end
      else begin
        edV.Text := FloatToStrF(rV * 1E+3, ffFixed, 0, 3);
        laUV.Caption := 'mV';
      end;
      if Abs(rI) > 1 then begin
        edI.Text := FloatToStrF(rI, ffFixed, 0, 3);
        laUI.Caption := 'A';
      end
      else if Abs(rI * 1E+3) > 1 then begin
        edI.Text := FloatToStrF(rI * 1E+3, ffFixed, 0, 3);
        laUI.Caption := 'mA';
      end
      else begin
        edI.Text := FloatToStrF(rI * 1E+6, ffFixed, 0, 3);
        laUI.Caption := 'µA';
      end;
    end;
  end
  // Invalid user input data
  else
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Button "τ": Insert letter 'τ' into time edit field }

procedure TfDC.btTauClick(Sender: TObject);

begin
  edT.Text := edT.Text + 'τ';
end;

{ Button "Clear": Clear all editi fields }

procedure TfDC.btClearClick(Sender: TObject);

begin
  ClearForm(True);
end;

{ Button "Close": Close the charge/discharge calculation window }

procedure TfDC.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Charge or discharge selection (corr. radiobutton checked) }

procedure TfDC.rbChargeChange(Sender: TObject);

begin
  if rbCharge.Checked then begin
    ClearForm(False);
    imCalc.Picture.LoadFromFile('charge.jpg');
  end;
end;

procedure TfDC.rbDischargeChange(Sender: TObject);

begin
  if rbDisCharge.Checked then begin
    ClearForm(False);
    imCalc.Picture.LoadFromFile('discharge.jpg');
  end;
end;

end.

