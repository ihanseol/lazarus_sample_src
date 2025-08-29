{***************************************************************}
{* Circuits biasing data entry unit for Amplifiers application *}
{***************************************************************}

unit amplifiers_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TRealArray = array of Real;
  {********}
  { TfData }
  {********}
  TfData = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label6: TLabel;
    rbCircuit1, rbCircuit2: TRadioButton;
    laV, laUV, laR, laUR, laAV, laBias: TLabel;
    edVS, edR, edBeta, edV, edAV: TEdit;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure rbCircuit1Change(Sender: TObject);
    procedure rbCircuit2Change(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    rR, rRStd, rV: Real;
    sVS, sR, sBeta, sV, sAV: string;
    bCircuits1, bCircuits2, bAV: Boolean;
  public
    bRStd: Boolean;
    StdR: TRealArray;
    rVS, rRC, rRE, rBeta, rVC, rVE, rAV: Real;
    sButton: string;
  end;

var
  fData: TfData;

function StandardResistance(R: Real; var Resistances: TRealArray): Real;

implementation

{$R *.lfm}

{ Determine standard resistor value for given resistnce }

function StandardResistance(R: Real; var Resistances: TRealArray): Real;

var
  StdX, I: Integer;
  StdR, MinDiff: Real;

begin
  StdR := R;
  I := -1; MinDiff := R; StdX := -1;
  // Lookup the standard resistor table, and return the value that is nearest to the given R
  repeat
    Inc(I);
    if Abs(R - Resistances[I]) < MinDiff then begin
      MinDiff := Abs(R - Resistances[I]);
      StdX := I;
    end;
  until I = Length(Resistances);
  if StdX <> -1 then
    StdR := Resistances[StdX];
  Result := StdR;
end;

{********}
{ TfData }
{********}

{ Window show-up: Save actual variable values}

procedure TfData.FormActivate(Sender: TObject);

begin
  bCircuits1 := rbCircuit1.Checked; bCircuits2 := rbCircuit2.Checked;
  sVS := edVS.Text; sR := edR.Text; sBeta := edBeta.Text;
  sV := edV.Text; bAV := edAV.Visible; sAV := edAV.Text;
  edVS.SetFocus;
end;

{ Circuit type changed: adapt entry values labels }

procedure TfData.rbCircuit1Change(Sender: TObject);

begin
  if rbCircuit1.Checked then begin
    laR.Caption := 'Resistance RC'; edR.Hint := 'Collector resistance';
    laV.Caption := 'DC output voltage VC';
    laBias.Caption := 'Circuit to be biased to specified DC output voltage (VC) and specified AC voltage gain (AV).';
    laAV.Visible := True; edAV.Visible := True;
  end;
end;

procedure TfData.rbCircuit2Change(Sender: TObject);

begin
  if rbCircuit2.Checked then begin
    laR.Caption := 'Resistance RE'; edR.Hint := 'Emitter resistance';
    laV.Caption := 'DC output voltage VE';
    laBias.Caption := 'Circuit to be biased to specified DC output voltage (VE).';
    laAV.Visible := False; edAV.Visible := False;
  end;
end;

{ Button "OK": Check data validity and close the form if data is valid }

procedure TfData.btOKClick(Sender: TObject);

var
  Ret: Cardinal;
  Mess, Mess2: string;

begin
  rVS := 0; rR := 0; rBeta := 0; rV := 0; rAV := 0;
  Mess := ''; Mess2 := '';
  // Read user entered values from form
  if edVS.Text <> '' then
    rVS := StrToFloat(edVS.Text);
  if edR.Text <> '' then
    rR := StrToFloat(edR.Text) * 1E+3;
  rRStd := rR;
  if bRStd and (rRStd > 0) then
    rRStd := StandardResistance(rRStd, StdR);
  if edBeta.Text <> '' then
    rBeta := StrToFloat(edBeta.Text);
  if edV.Text <> '' then
    rV := StrToFloat(edV.Text);
  if rbCircuit1.Checked then begin
    if edAV.Text <> '' then
      rAV := StrToFloat(edAV.Text);
  end;
  // Check user entered values
  if rVS <= 0 then begin
    Mess := 'Source voltage must be greater than 0!';
    edVS.SetFocus;
  end
  else if rR <= 0 then begin
    if rbCircuit1.Checked then
      Mess := 'Resistance RC must be greater than 0!'
    else
      Mess := 'Resistance RE must be greater than 0!';
    edR.SetFocus;
  end
  else if rR <> rRStd then begin
    // Resistance must have standard resistor value, if this option is selected
    if rbCircuit1.Checked then
      Mess := 'RC should have a standard resistance value!'
    else
      Mess := 'RE should have a standard resistance value!';
    edR.SetFocus;
  end
  else if rBeta <= 1 then begin
    Mess := 'Transistor current gain must be greater than 1!';
    edBeta.SetFocus;
  end
  else if rV <= 0 then begin
    Mess := 'DC output voltage must be greater than 0!';
    edV.SetFocus;
  end
  else if rV > rVS then begin
    Mess := 'DC output voltage must be less than source voltage!';
    edV.SetFocus;
  end
  else if rbCircuit1.Checked and (rAV <= 1) then begin
    Mess := 'AC voltage gain must be greater than 1!';
    edAV.SetFocus;
  end
  else if (rBeta < 10) or (rBeta > 300) then begin
    // Arbitrarily chosen common transistor current gain
    Mess2 := 'Typical transistor current gain values are between 10 and 300!';
    edBeta.SetFocus;
  end;
  if Mess = '' then begin
    // All values entered are valid
    if Mess2 <> '' then begin
      // If transistor gain is outside common values, give user possibility to change (or keep) the entered value
      Mess2 += ' Continue anyway?';
      Ret := MessageDlg('Data check', Mess2, mtWarning, [mbYes, mbNo], 0);
      if Ret = mrYes then begin
        btOK.SetFocus;
        Mess2 := '';
      end;
    end;
  end
  else begin
    // Some value(s) entered is (are) invalid
    MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end;
  if (Mess = '') and (Mess2 = '') then begin
    // All values such, that calculation can be done
    if rbCircuit1.Checked then begin
      rRC := rRStd;
      rVC := rV;
    end
    else begin
      rRE := rRStd;
      rVE := rV;
    end;
    sButton := 'ok';                                                           // tell main form that "OK" button was pressed
    Close;
  end;
end;

{ Button "Cancel": Restore data and close the form }

procedure TfData.btCancelClick(Sender: TObject);

begin
  rbCircuit1.Checked := bCircuits1; rbCircuit2.Checked := bCircuits2;
  edVS.Text := sVS; edR.Text := sR;
  edBeta.Text := sBeta;
  edV.Text := sV;
  edAV.Visible := bAV; laAV.Visible := bAV; edAV.Text := sAV;
  sButton := 'cancel';                                                         // tell main form that "Cancel" button was pressed
  Close;
end;

end.

