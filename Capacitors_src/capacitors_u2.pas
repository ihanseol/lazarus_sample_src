{***************************************************}
{* Parallel/series unit for Capacitors application *}
{***************************************************}

unit capacitors_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfCircuits }
  {************}
  TfCircuits = class(TForm)
    stTitle: TStaticText;
    Label5: TLabel;
    rbParallel, rbSeries: TRadioButton;
    imCalc: TImage;
    laCapacitance1, laCapacitance2, laCapacitance3, laCapacitance4: TLabel;
    edCapacitance1, edCapacitance2, edCapacitance3, edCapacitance4, edCapacitance: TEdit;
    cobUCapacitance1, cobUCapacitance2, cobUCapacitance3, cobUCapacitance4: TComboBox;
    laUCapacitance: TLabel;
    btCalc: TButton;
    btClear: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure rbParallelChange(Sender: TObject);
    procedure rbSeriesChange(Sender: TObject);
  private
    rC: Double;
    aCapacitances: array[0..3] of Double;
    laCapacitances: array[0..3] of TLabel;
    edCapacitances: array[0..3] of TEdit;
    cobUCapacitances: array[0..3] of TCombobox;
  end;

const
  SUB_Digits: array[1..4] of string = (
    #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84
  );

var
  fCircuits: TfCircuits;

implementation

{$R *.lfm}

{************}
{ TfCircuits }
{************}

{ Application start: Initialisation }

procedure TfCircuits.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create array with capacitor labels, capacitance value edit fields and capacitance unit comboboxes
  laCapacitances[0] := laCapacitance1; laCapacitances[1] := laCapacitance2;
  laCapacitances[2] := laCapacitance3; laCapacitances[3] := laCapacitance4;
  edCapacitances[0] := edCapacitance1; edCapacitances[1] := edCapacitance2;
  edCapacitances[2] := edCapacitance3; edCapacitances[3] := edCapacitance4;
  cobUCapacitances[0] := cobUCapacitance1; cobUCapacitances[1] := cobUCapacitance2;
  cobUCapacitances[2] := cobUCapacitance3; cobUCapacitances[3] := cobUCapacitance4;
  // Apply subscripts
  for I := 1 to 4 do
    laCapacitances[I - 1].Caption := StringReplace(laCapacitances[I - 1].Caption, IntToStr(I), SUB_Digits[I], []);
end;

{ Button "Calculation": Calculate total capacitance for parallel resp. series circuit }

procedure TfCircuits.btCalcClick(Sender: TObject);

var
  N, I: Integer;
  Mess: string;
  CInv: Double;

begin
  // Read capacitance values from form
  Mess := ''; N := 0;
  for I := 0 to 3 do begin
    if Mess = '' then begin
      if edCapacitances[I].Text = '' then
        aCapacitances[I] := -1                                                 // blank field: just ignore it
      else begin
        // Get capacitance value
        Inc(N);
        aCapacitances[I] := StrToFloat(edCapacitances[I].Text);
        if aCapacitances[I] > 0 then begin
          case cobUCapacitances[I].ItemIndex of
            0: aCapacitances[I] *= 1E-6;                                       // capacitance was given in µF
            1: aCapacitances[I] *= 1E-9;                                       // capacitance was given in nF
            2: aCapacitances[I] *= 1E-12;                                      // capacitance was given in pF
          end;
        end
        else begin
          // Invalid capacitance value
          Mess := 'Capacitance C' + SUB_Digits[I + 1] + ' must be greater than 0';
          edCapacitances[I].SetFocus;
        end;
      end;
    end;
  end;
  // Do parallel resp. series circuit calculation
  if Mess = '' then begin
    if N > 1 then begin
      rC := 0; CInv := 0;
      for I := 0 to 3 do begin
        if aCapacitances[I] <> -1 then begin
          // Do calculation for fields that are filled in, only
          if rbParallel.Checked then                                           // parallel circuit: C = C1 + C2 + ...
            rC += aCapacitances[I]
          else
            CInv += 1 / aCapacitances[I];                                      // series circuit: 1/C = 1/C1 + 1/C2 + ...
        end;
      end;
      if rbSeries.Checked then
        // For series circuit sum calculated is 1/C...
        rC := 1 / CInv;
      // Display total capacitance value
      if rC * 1E+6 > 1 then begin
        edCapacitance.Text := FloatToStrF(rC * 1E+6, ffFixed, 0, 3);
        laUCapacitance.Caption := 'µF';
      end
      else if rC * 1E+9 > 1 then begin
        edCapacitance.Text := FloatToStrF(rC * 1E+9, ffFixed, 0, 3);
        laUCapacitance.Caption := 'nF';
      end
      else begin
        edCapacitance.Text := FloatToStrF(rC * 1E+12, ffFixed, 0, 3);
        laUCapacitance.Caption := 'pF';
      end
    end
    else
      // At least 2 capacitance values must be filled in
      Mess := 'There must at least be 2 capacitors';
  end;
  // Invalid user input data
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Button "Clear": Clear form fields }

procedure TfCircuits.btClearClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to 3 do
    edCapacitances[I].Text := '';
  edCapacitance.Text := '';
end;

{ Button "Close": Close the parallel/series calculations window }

procedure TfCircuits.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Parallel or series circuit selection (corr. radiobutton checked) }

procedure TfCircuits.rbParallelChange(Sender: TObject);

begin
  if rbParallel.Checked then begin
    imcalc.Picture.LoadFromFile('parallel.jpg');
    edCapacitance.Text := '';
  end;
end;

procedure TfCircuits.rbSeriesChange(Sender: TObject);

begin
  if rbSeries.Checked then begin
    imcalc.Picture.LoadFromFile('series.jpg');
    edCapacitance.Text := '';
  end;
end;

end.

