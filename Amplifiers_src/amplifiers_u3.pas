{****************************************************************}
{* Circuits analysis data entry unit for Amplifiers application *}
{****************************************************************}

unit amplifiers_u3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TRealArray = array of Real;
  {*********}
  { TfData2 }
  {*********}
  TfData2 = class(TForm)
    StaticText1: TStaticText;
    Label2, Label3, Label6: TLabel;
    laRC, laRE, laR1, laR2, laRIn, laCE: TLabel;
    laURC, laURE, laUR1, laUR2, laURIn, laUCE: TLabel;
    edVS, edRC, edRE, edR1, edR2, edBeta, edRIn, edCE: TEdit;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    rRCStd, rREStd, rR1Std, rR2Std: Real;
    sVS, sRC, sRE, sR1, sR2, sBeta, sRIn, sCE: stRIng;
  public
    StdR: TRealArray;
    bRStd: Boolean;
    rVS, rRC, rRE, RR1, rR2, rBeta, rRIn, rCE: Real;
    sButton: stRIng;
  end;

var
  fData2: TfData2;

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
  StandardResistance := StdR;
end;

{*********}
{ TfData2 }
{*********}

{ Window show-up: Save variables' actual values}

procedure TfData2.FormActivate(Sender: TObject);

begin
  sVS := edVS.Text; sRC := edRC.Text; sRE := edRE.Text; sR1 := edR1.Text; sR2 := edR2.Text;
  sBeta := edBeta.Text; sRIn := edRIn.Text; sCE := edCE.Text;
  edVS.SetFocus;
end;

{ Button "OK": Check data validity and close the form if data is valid }

procedure TfData2.btOKClick(Sender: TObject);

var
  Ret: Cardinal;
  Mess, Mess2, Mess3, Mess4: stRIng;

begin
  rVS := 0; rRC := 0; rRE := 0; rR1 := 0; rR2 := 0;
  rBeta := 0; rRIn := 0; rCE := 0;
  Mess := ''; Mess2 := ''; Mess3 := ''; Mess4 := '';
  // Read user entered values from form
  if edVS.Text <> '' then
    rVS := StrToFloat(edVS.Text);
  if edRC.Text <> '' then
    rRC := StrToFloat(edRC.Text) * 1E+3;
  rRCStd := rRC;
  if bRStd and (rRCStd > 0) then
    rRCStd := StandardResistance(rRCStd, StdR);
  if edRE.Text <> '' then
    rRE := StrToFloat(edRE.Text) * 1E+3;
  rREStd := rRE;
  if bRStd and (rREStd > 0) then
    rREStd := StandardResistance(rREStd, StdR);
  if edR1.Text <> '' then
    rR1 := StrToFloat(edR1.Text) * 1E+3;
  rR1Std := rR1;
  if bRStd and (rR1Std > 0) then
    rR1Std := StandardResistance(rR1Std, StdR);
  if edR2.Text <> '' then
    rR2 := StrToFloat(edR2.Text) * 1E+3;
  rR2Std := rR2;
  if bRStd and (rR2Std > 0) then
    rR2Std := StandardResistance(rR2Std, StdR);
  if edBeta.Text <> '' then
    rBeta := StrToFloat(edBeta.Text);
  if edRIn.Text <> '' then
    rRIn := StrToFloat(edRIn.Text) * 1E+3;
  if edCE.Text <> '' then
    rCE := StrToFloat(edCE.Text);
  // Check user entered values
  if rVS <= 0 then begin
    Mess := 'Source voltage must be greater than 0!';
    edVS.SetFocus;
  end
  else if rRC <= 0 then begin
    Mess := 'Resistance RC must be greater than 0!';
    edRC.SetFocus;
  end
  else if rRC <> rRCStd then begin
    Mess := 'RC should have a standard resistance value!';
    edRC.SetFocus;
  end
  else if rRE <= 0 then begin
    Mess := 'Resistance RE must be greater than 0!';
    edRE.SetFocus;
  end
  else if rRE <> rREStd then begin
    Mess := 'RE should have a standard resistance value!';
    edRE.SetFocus;
  end
  else if rR1 <= 0 then begin
    Mess := 'Resistance R1 must be greater than 0!';
    edR1.SetFocus;
  end
  else if rR1 <> rR1Std then begin
    Mess := 'R1 should have a standard resistance value!';
    edR1.SetFocus;
  end
  else if rR2 <= 0 then begin
    Mess := 'Resistance R2 must be greater than 0!';
    edR2.SetFocus;
  end
  else if rR2 <> rR2Std then begin
    Mess := 'R2 should have a standard resistance value!';
    edR2.SetFocus;
  end
  else if rBeta <= 0 then begin
    Mess := 'Transistor current gain must be greater than 0!';
    edBeta.SetFocus;
  end
  else if rRIn <= 0 then begin
    Mess := 'Transistor internal resistance must be greater than 0!';
    edRIn.SetFocus;
  end
  else if rCE < 0 then begin
    Mess := 'Capacitance must be greater than or equal to 0!';
    edCE.SetFocus;
  end
  else begin
    if (rBeta < 10) or (rBeta > 300) then begin
      // Arbitrarily chosen common transistor current gain values
      Mess2 := 'Typical transistor current gain values are between 10 and 300!';
    end;
    if (rRin < 1000) or (rRin > 2000) then begin
      // Arbitrarily chosen common transistor internal resistance values
      Mess3 := 'Typical transistor input resistances are between 1 and 2 kΩ!';
    end;
    if rCE = 0 then begin
      // Warn user if capacitance field is empty (may have forgotten to fill-in...)
      Mess4 := 'Capacitance is set to 0. The circuit will be without capacitor parallel to RE!';
    end
  end;
  if Mess = '' then begin
    // Valid user entered values
    if Mess2 <> '' then begin
      // Give user possibility to change or keep uncommon transistor gain value
      Mess2 += ' Continue anyway?';
      Ret := MessageDlg('Data check', Mess2, mtWarning, [mbYes, mbNo], 0);
      if Ret = mrYes then
        Mess2 := ''
      else
        edBeta.SetFocus;
    end;
    if (Mess2 = '') and (Mess3 <> '') then begin
      // Give user possibility to change or keep uncommon transistor internal resistance
      Mess3 += ' Continue anyway?';
      Ret := MessageDlg('Data check', Mess3, mtWarning, [mbYes, mbNo], 0);
      if Ret = mrYes then
        Mess3 := ''
      else
        edRIn.SetFocus;
    end;
    if (Mess2 = '') and (Mess3 = '') and (Mess4 <> '') then begin
      // Give user possibility to add a capacitance value (or do calculation for circuit without capacitor)
      Mess4 += ' Continue anyway?';
      Ret := MessageDlg('Data check', Mess4, mtWarning, [mbYes, mbNo], 0);
      if Ret = mrYes then
        Mess4 := ''
      else
        edCE.SetFocus;
    end;
  end
  else begin
    // Some value(s) entered is (are) invalid
    MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end;
  if (Mess = '') and (Mess2 = '') and (Mess3 = '') and (Mess4 = '')then begin
    // All values such, that calculation can be done
    rRC := rRCStd; rRE := rREStd; rR1 := rR1Std; rR2 := rR2Std;
    sButton := 'ok';                                                           // tell main form that "OK" button was pressed
    btOK.SetFocus;
    Close;
  end;
end;

{ Button "Cancel": Restore data and close the form }

procedure TfData2.btCancelClick(Sender: TObject);

begin
  edVS.Text := sVS; edRC.Text := sRC; edRE.Text := sRE; edR1.Text := sR1; edR2.Text := sR2;
  edBeta.Text := sBeta; edRIn.Text := sRIn; edCE.Text := sCE;
  sButton := 'cancel';                                                         // tell main form that "Cancel" button was pressed
  Close;
end;

end.

