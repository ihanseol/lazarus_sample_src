{*******************************************************}
{* Schering bridge unit for BridgeCircuits application *}
{*******************************************************}

unit schering;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {************}
  { TfSchering }
  {************}
  TfSchering = class(TForm)
    StaticText1, StaticText2: TStaticText;
    edDetails: TMemo;
    Image1: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    laR1, laR2, laR4, laF, laC1, laC3, laC4: TLabel;
    laUR1, laUR2, laUR4, laUF, laUC1, laUC3, laUC4: TLabel;
    edR1, edR2, edR4, edF, edC1, edC3, edC4: TEdit;
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
  fSchering: TfSchering;

implementation

{$R *.lfm}

{************}
{ TfSchering }
{************}

{ Application start: Initialisation }

procedure TfSchering.FormCreate(Sender: TObject);

var
  S: string;

begin
  // Apply subscripts
  S := edDetails.Text; edDetails.Text := ApplySubscripts(S);
  S := edFormula.Text; edFormula.Text := ApplySubscripts(S);
  S := laR1.Caption; laR1.Caption := ApplySubscripts(S);
  S := laR2.Caption; laR2.Caption := ApplySubscripts(S);
  S := laR4.Caption; laR4.Caption := ApplySubscripts(S);
  S := laC1.Caption; laC1.Caption := ApplySubscripts(S);
  S := laC3.Caption; laC3.Caption := ApplySubscripts(S);
  S := laC4.Caption; laC4.Caption := ApplySubscripts(S);
  // Create array with form input/output fields
  edData[0] := edF;  edData[1] := edR2; edData[2] := edC3;
  edData[3] := edR1; edData[4] := edC1;
  edData[5] := edR4; edData[6] := edC4;
  // Initial unit conversion factors
  rOldMultR := 1E+3; rOldMultC := 1E-6;
end;

{ Window activation: Adapt form labels and values to units currently used }

procedure TfSchering.FormActivate(Sender: TObject);

var
  I, J: Integer;
  Mult: Real;
  R, C: Real;

begin
  if rMultR <> rOldMultR then begin
    // If resistance unit has been changed (in main form "Options" menu), change resistance unit and values
    if rMultR = 1E+3 then begin
      laUR1.Caption := 'kΩ'; laUR2.Caption := 'kΩ'; laUR4.Caption := 'kΩ';
      Mult := 1E-3;
    end
    else begin
      Mult := 1E+3;
      laUR1.Caption := 'Ω'; laUR2.Caption := 'Ω'; laUR4.Caption := 'Ω';
    end;
    if bKeepInput then begin
      // If input values has to be kept, transform resistances to actually used unit
      for I := 1 to 2 do begin
        J := 2 * I - 1;
        if edData[J].Text <> '' then begin
          R := StrToFloat(edData[J].Text) * Mult;
          edData[J].Text := RFormat(R, iDecimals);
        end;
      end;
    end;
    rOldMultR := rMultR;                                                       // save actual resistance unit
  end;
  if rMultC <> rOldMultC then begin
    // If capacitance unit has been changed (in main form "Options" menu), change capacitance unit and value
    if rMultC = 1E-6 then begin
      laUC1.Caption := 'µF'; laUC3.Caption := 'µF'; laUC4.Caption := 'µF';
      Mult := 1E-3;
    end
    else begin
      laUC1.Caption := 'nF'; laUC3.Caption := 'nF'; laUC4.Caption := 'nF';
      Mult := 1E+3;
    end;
    if bKeepInput then begin
      // If input values has to be kept, transform capacitance to actually used unit
      for I := 1 to 2 do begin
        J := 2 * I;
        if edData[J].Text <> '' then begin
          C := StrToFloat(edData[J].Text) * Mult;
          edData[J].Text := RFormat(C, iDecimals);
        end;
      end;
    end;
    rOldMultC := rMultC;                                                       // save actual capacitance unit
  end;
  if not bKeepInput then begin
    // If input values have not to be kept, clear the input edit fields
    for I := 0 to 4 do
      edData[I].Text := '';
  end;
  // Clear the results fields and focus first input field
  edR4.Text := ''; edC4.Text := '';
  edF.SetFocus;
end;

{ Button "Calculate" pushed: Calculate Schering bridge conductance (and resistance) }

procedure TfSchering.btCalcClick(Sender: TObject);

var
  R1, R2, R4, C1, C3, C4: Real;
  Mess: string;

begin
  GetData(edData, 5, True, aData, Mess);                                       // read user data from form
  if Mess = '' then begin
    R2 := aData[1] * rMultR; R1 := aData[3] * rMultR;                          // transform input data to actual units
    C3 := aData[2] * rMultC; C1 := aData[4] * rMultC;
    C4 := (C3 * (R1 / R2)) / rMultC;                                           // calculate C4
    R4 := (R2 * (C1 / C3)) / rMultR;                                           // calculate R4
    edC4.Text := RFormat(C4, iDecimals);                                       // display C4 and R4
    edR4.Text := RFormat(R4, iDecimals);
  end;
end;

{ Button "Close" pushed: Close the "Schering window" }

procedure TfSchering.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

