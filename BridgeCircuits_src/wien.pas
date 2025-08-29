{***************************************************}
{* Wien bridge unit for BridgeCircuits application *}
{***************************************************}

unit wien;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {********}
  { TfWien }
  {********}
  TfWien = class(TForm)
    StaticText1, StaticText2: TStaticText;
    edDetails: TMemo;
    Image1: TImage;
    Label1, Label2, Label3, Label4, Label7: TLabel;
    laR1, laR2, laR3, laR4, laC1, laC3, laF: TLabel;
    laUR1, laUR2, laUR3, laUR4, laUC1, laUC3, laUF: TLabel;
    edR1, edR2, edR3, edR4, edC1, edC3, edF: TEdit;
    edFormula: TEdit;
    btCalc: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    rOldMultR, rOldMultC: Real;
    aData: array[0..6] of Real;
  public
    iDecimals: Integer;
    rMultR, rMultC: Real;
    bKeepInput: Boolean;
    edData: array[0..6] of TEdit;
  end;

var
  fWien: TfWien;

implementation

{$R *.lfm}

{********}
{ TfWien }
{********}

{ Application start: Initialisation }

procedure TfWien.FormCreate(Sender: TObject);

var
  S: string;

begin
  // Apply subscripts
  S := edDetails.Text; edDetails.Text := ApplySubscripts(S);
  S := edFormula.Text; edFormula.Text := ApplySubscripts(S);
  S := laR1.Caption; laR1.Caption := ApplySubscripts(S);
  S := laR2.Caption; laR2.Caption := ApplySubscripts(S);
  S := laR3.Caption; laR3.Caption := ApplySubscripts(S);
  S := laR4.Caption; laR4.Caption := ApplySubscripts(S);
  S := laC1.Caption; laC1.Caption := ApplySubscripts(S);
  S := laC3.Caption; laC3.Caption := ApplySubscripts(S);
  // Create array with form input/output fields
  edData[0] := edR2; edData[1] := edR4;
  edData[2] := edC1; edData[3] := edC3;
  edData[4] := edR1; edData[5] := edR3;
  edData[6] := edF;
  // Initial unit conversion factors
  rOldMultR := 1E+3; rOldMultC := 1E-6;
end;

{ Window activation: Adapt form labels and values to units currently used }

procedure TfWien.FormActivate(Sender: TObject);

var
  I, J, K: Integer;
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
      for I := 0 to 1 do begin
        for J := 0 to 1 do begin
          K := I * 4 + J;
          if edData[K].Text <> '' then begin
            R := StrToFloat(edData[K].Text) * Mult;
            edData[K].Text := RFormat(R, iDecimals);
          end;
        end;
      end;
    end;
    rOldMultR := rMultR;                                                       // save actual resistance unit
  end;
  if rMultC <> rOldMultC then begin
    // If capacitance unit has been changed (in main form "Options" menu), change capacitance unit and value
    if rMultC = 1E-6 then begin
      laUC1.Caption := 'µF'; laUC3.Caption := 'µF';
      Mult := 1E-3;
    end
    else begin
      laUC1.Caption := 'nF'; laUC3.Caption := 'nF';
      Mult := 1E+3;
    end;
    if bKeepInput then begin
      // If input values has to be kept, transform capacitance to actually used unit
      for I := 2 to 3 do begin
        if edData[I].Text <> '' then begin
          C := StrToFloat(edData[I].Text) * Mult;
          edData[I].Text := RFormat(C, iDecimals);
        end;
      end;
    end;
    rOldMultC := rMultC;                                                       // save actual capacitance unit
  end;
  if not bKeepInput then begin
    // If input values have not to be kept, clear the input edit fields
    for I := 0 to 5 do
      edData[I].Text := '';
  end;
  // Clear the results field and focus first input field
  edF.Text := '';
  edR2.SetFocus;
end;

{ Button "Calculate" pushed: Calculate Wien bridge frequency }

procedure TfWien.btCalcClick(Sender: TObject);

var
  R1, R3, C1, C3, F: Real;
  Mess: string;

begin
  GetData(edData, 6, False, aData, Mess);                                      // read user data from form
  if Mess = '' then begin
    R1 := aData[4] * rMultR; R3 := aData[5] * rMultR;                          // transform input data to actual units
    C1 := aData[2] * rMultC; C3 := aData[3] * rMultC;
    F := 1 / (2 * Pi * Sqrt(R1 * R3 * C1 * C3));                               // calculate F
    edF.Text := RFormat(F, iDecimals);                                         // display F
  end;
end;

{ Button "Close" pushed: Close the "Maxwell window" }

procedure TfWien.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

