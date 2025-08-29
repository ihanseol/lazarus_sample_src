{**************************************************}
{* "Compound interest" unit for Loans application *}
{**************************************************}

unit compound;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, Math;

type
  {************}
  { TfCompound }
  {************}
  TfCompound = class(TForm)
    Label1, Label2, Label5, Label6: TLabel;
    laCalculation, laCalculation2: TLabel;
    edPrincipal, edInterest, edCompounds, edCalculation, edCalculation2: TEdit;
    sgRepayments: TStringGrid;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    rP, rR, rPMT, rT, rN, rI: Real;
  end;

var
  fCompound: TfCompound;

implementation

{$R *.lfm}

{************}
{ TfCompound }
{************}

{ Form activation ("called" from main unit): Do "Compound interest" calculations }

procedure TfCompound.FormActivate(Sender: TObject);

var
  M, T : Integer;
  OldBalance, NewBalance, InterestTotal, PrincipalTotal, InterestPaid, PrincipalPaid: Real;

begin
  OldBalance := rP + rI; NewBalance := OldBalance;
  InterestPaid := 0; PrincipalPaid := 0;
  InterestTotal := 0; PrincipalTotal := 0;
  T := Round(rT); rPMT *= 12;                                                  // actually values variation from year to year
  // Adapt number of stringgrid rows
  if T <= 12 then
    sgRepayments.RowCount := 13
  else begin
    sgRepayments.RowCount := T + 1;
    for M := 13 to T do
      sgRepayments.Cells[0, M] := '  ' + IntToStr(M);
  end;
  sgRepayments.Clean(1, 1, 5, T, []);
  // Do calculation for T periods
  M := 0;
  while M < T do begin
    // Calculate table values, adapting them in order to correct small differences (due to roundings) with "real" values
    Inc(M);
    InterestPaid := rP * power(1 + rR / rN, M * rN) - rP - InterestTotal; InterestPaid := Round(100 * InterestPaid) / 100;
    if (rPMT >= OldBalance) or (M = T) then
      rPMT := OldBalance;
    PrincipalPaid := rPMT - InterestPaid;
    if PrincipalPaid < 0 then begin
      PrincipalPaid := 0;
    end;
    InterestTotal += InterestPaid;
    PrincipalTotal += PrincipalPaid;
    if (M = T) and (InterestTotal <> rI) then
      InterestTotal := rI;
    PrincipalTotal += PrincipalPaid;
    if (M = T) and (PrincipalTotal <> rP) then
      PrincipalTotal := rP;
    NewBalance := OldBalance - PrincipalPaid - InterestPaid;
    // Fill in the grid
    sgRepayments.Cells[1, M] := FloatToStrF(OldBalance, ffFixed, 0, 2);
    sgRepayments.Cells[2, M] := FloatToStrF(rPMT, ffFixed, 0, 2);
    sgRepayments.Cells[3, M] := FloatToStrF(InterestPaid, ffFixed, 0, 2);
    sgRepayments.Cells[4, M] := FloatToStrF(PrincipalPaid, ffFixed, 0, 2);
    sgRepayments.Cells[5, M] := FloatToStrF(InterestTotal, ffFixed, 0, 2);
    sgRepayments.Cells[6, M] := FloatToStrF(PrincipalTotal, ffFixed, 0, 2);
    sgRepayments.Cells[7, M] := FloatToStrF(NewBalance, ffFixed, 0, 2);
    OldBalance := NewBalance;
  end;
end;

{ Button "Close" pushed: Close the window }

procedure TfCompound.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

