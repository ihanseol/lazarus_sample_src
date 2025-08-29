{**************************************************}
{* Hay bridge unit for BridgeCircuits application *}
{**************************************************}

unit hay;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {*******}
  { TfHay }
  {*******}
  TfHay = class(TForm)
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    edDetails: TMemo;
    Image1: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    laR1, laR2, laR3, laR4, laF, laC1, laL4: TLabel;
    laUR1, laUR2, laUR3, laUR4, laUF, laUC1, laUL4: TLabel;
    edR1, edR2, edR3, edR4, edF, edC1, edL4: TEdit;
    edFormula, edFormula2: TEdit;
    btCalc: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    rOldMultR, rOldMultC, rOldMultL: Real;
    aData: array[0..6] of Real;
  public
    iDecimals: Integer;
    rMultR, rMultC, rMultL: Real;
    bKeepInput: Boolean;
    edData: array[0..6] of TEdit;
  end;

var
  fHay: TfHay;

implementation

{$R *.lfm}

{*******}
{ TfHay }
{*******}

{ Application start: Initialisation }

procedure TfHay.FormCreate(Sender: TObject);

var
  S: string;

begin
  // Apply subscripts and superscripts
  S := edDetails.Text; edDetails.Text := ApplySubscripts(S);
  S := edFormula.Text; S := ApplySubscripts(S);
  S := StringReplace(S, '2', SUP_2, [rfReplaceAll]); edFormula.Text := S;
  S := edFormula2.Text; S := ApplySubscripts(S);
  S := StringReplace(S, '2', SUP_2, [rfReplaceAll]); edFormula2.Text := S;
  S := edFormula2.Text; edFormula2.Text := ApplySubscripts(S);
  S := laR1.Caption; laR1.Caption := ApplySubscripts(S);
  S := laR2.Caption; laR2.Caption := ApplySubscripts(S);
  S := laR3.Caption; laR3.Caption := ApplySubscripts(S);
  S := laR4.Caption; laR4.Caption := ApplySubscripts(S);
  S := laC1.Caption; laC1.Caption := ApplySubscripts(S);
  S := laL4.Caption; laL4.Caption := ApplySubscripts(S);
  // Create array with form input/output fields
  edData[0] := edF;  edData[1] := edR2; edData[2] := edR3;
  edData[3] := edR1; edData[4] := edC1;
  edData[5] := edR4; edData[6] := edL4;
  // Initial unit conversion factors
  rOldMultR := 1E+3; rOldMultC := 1E-6; rOldMultL := 1E-3;
end;

{ Window activation: Adapt form labels and values to units currently used }

procedure TfHay.FormActivate(Sender: TObject);

var
  I: Integer;
  Mult: Real;
  R, C: Real;

begin
  if rMultR <> rOldMultR then begin
    // If resistance unit has been changed (in main form "Options" menu), change resistance unit and values
    if rMultR = 1E+3 then begin
      laUR1.Caption := 'kΩ'; laUR2.Caption := 'kΩ';
      laUR3.Caption := 'kΩ'; laUR4.Caption := 'kΩ';
      Mult := 1E-3;
    end
    else begin
      Mult := 1E+3;
      laUR1.Caption := 'Ω'; laUR2.Caption := 'Ω';
      laUR3.Caption := 'Ω'; laUR4.Caption := 'Ω';
    end;
    if bKeepInput then begin
      // If input values has to be kept, transform resistances to actually used unit
      for I := 1 to 3 do begin
        if edData[I].Text <> '' then begin
          R := StrToFloat(edData[I].Text) * Mult;
          edData[I].Text := RFormat(R, iDecimals);
        end;
      end;
    end;
    rOldMultR := rMultR;                                                       // save actual resistance unit
  end;
  if rMultC <> rOldMultC then begin
    // If capacitance unit has been changed (in main form "Options" menu), change capacitance unit and value
    if rMultC = 1E-6 then begin
      laUC1.Caption := 'µF';
      Mult := 1E-3;
    end
    else begin
      laUC1.Caption := 'nF';
      Mult := 1E+3;
    end;
    if bKeepInput then begin
      // If input values has to be kept, transform capacitance to actually used unit
      if edData[4].Text <> '' then begin
        C := StrToFloat(edData[4].Text) * Mult;
        edData[4].Text := RFormat(C, iDecimals);
      end;
    end;
    rOldMultC := rMultC;                                                       // save actual capacitance unit
  end;
  if rMultL <> rOldMultL then begin
    // If inductance unit has been changed (in main form "Options" menu), change inductance unit
    if rMultL = 1 then
      laUL4.Caption := 'H'
    else
      laUL4.Caption := 'mH';
    rOldMultL := rMultL;                                                       // save actual inductance unit
  end;
  if not bKeepInput then begin
    // If input values have not to be kept, clear the input edit fields
    for I := 0 to 4 do
      edData[I].Text := '';
  end;
  // Clear the results fields and focus first input field
  edR4.Text := ''; edL4.Text := '';
  edF.SetFocus;
end;

{ Button "Calculate" pushed: Calculate Hay bridge inductance (and resistance) }

procedure TfHay.btCalcClick(Sender: TObject);

var
  F, R1, R2, R3, R4, C1, L4: Real;
  Mess: string;

begin
  GetData(edData, 5, False, aData, Mess);                                      // read user data from form
  if Mess = '' then begin
    F := aData[0];
    R2 := aData[1] * rMultR; R3 := aData[2] * rMultR;                          // transform input data to actual units
    R1 := aData[3] * rMultR; C1 := aData[4] * rMultC;
    L4 := (R2 * R3 * C1 / (1 + Sqr(2 * Pi * F) * Sqr(R1) * Sqr(C1))) / rMultL; // claculate L4
    R4 := (Sqr(2 * Pi * F) * Sqr(C1) * R1 * R2 * R3 / (1 + Sqr(2 * Pi * F) * Sqr(R1) * Sqr(C1))) / rMultR;  // calculate R4
    edL4.Text := RFormat(L4, iDecimals);                                       // display L4 and R4
    edR4.Text := RFormat(R4, iDecimals);
  end;
end;

{ Button "Close" pushed: Close the "Hay window" }

procedure TfHay.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

