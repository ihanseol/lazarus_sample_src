{****************************************************************}
{* "Simple add-on/discount interest" unit for Loans application *}
{****************************************************************}

unit addon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  {*********}
  { TfAddon }
  {*********}
  TfAddon = class(TForm)
    Label1, Label2, Label5: TLabel;
    laCalculation, laCalculation2: TLabel;
    edPrincipal, edInterest, edCalculation, edCalculation2: TEdit;
    sgRepayments: TStringGrid;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    rP, rR, rPMT, rI, rT: Real;
    sMethod: string;
  end;

var
  fAddon: TfAddon;

implementation

{$R *.lfm}

{*********}
{ TfAddon }
{*********}

{ Form activation ("called" from main unit): Do "Simple add-on interest" calculations }

procedure TfAddon.FormActivate(Sender: TObject);

var
  M, M2, T : Integer;
  InterestFactor, OldBalance, NewBalance, InterestTotal, PrincipalTotal, InterestPaid, PrincipalPaid: Real;

begin
  if sMethod = 'discount' then
    rP -= rI;
  OldBalance := rP + rI; NewBalance := OldBalance;
  InterestPaid := 0; PrincipalPaid := 0;
  InterestTotal := 0; PrincipalTotal := 0;
  T := Round(12 * rT);
  // Adapt number of stringgrid rows
  if T <= 12 then
    sgRepayments.RowCount := 13
  else begin
    sgRepayments.RowCount := T + 1;
    for M := 13 to T do
      sgRepayments.Cells[0, M] := '  ' + IntToStr(M);
  end;
  sgRepayments.Clean(1, 1, 8, T, []);
  // Do calculation for T periods
  M := 0;
  while M < T do begin
    // Calculate interest paid and principal paid during this month
    M2 := T - M;
    InterestFactor := (2 * M2) / (M2 * (M2 + 1)); InterestFactor := Round(1E+6 * InterestFactor) / 1E+6;
    Inc(M);
    InterestPaid := InterestFactor * (rI - InterestTotal); InterestPaid := Round(100 * InterestPaid) / 100;
    // Adapt values in order to correct small differences (due to roundings) with "real" values
    if InterestPaid > rPMT then
      InterestPaid := rPMT;
    if M < T then
      PrincipalPaid := rPMT - InterestPaid
    else
      PrincipalPaid := OldBalance - InterestPaid;
    if PrincipalPaid < 0 then
      PrincipalPaid := 0;
    // Calculate other table values
    InterestTotal += InterestPaid;
    PrincipalTotal += PrincipalPaid;
    NewBalance := OldBalance - PrincipalPaid - InterestPaid;
    // Fill in the grid
    sgRepayments.Cells[1, M] := FloatToStrF(OldBalance, ffFixed, 0, 2);
    sgRepayments.Cells[2, M] := FloatToStrF(rPMT, ffFixed, 0, 2);
    sgRepayments.Cells[3, M] := FloatToStrF(InterestFactor, ffFixed, 0, 6);
    sgRepayments.Cells[4, M] := FloatToStrF(InterestPaid, ffFixed, 0, 2);
    sgRepayments.Cells[5, M] := FloatToStrF(PrincipalPaid, ffFixed, 0, 2);
    sgRepayments.Cells[6, M] := FloatToStrF(InterestTotal, ffFixed, 0, 2);
    sgRepayments.Cells[7, M] := FloatToStrF(PrincipalTotal, ffFixed, 0, 2);
    sgRepayments.Cells[8, M] := FloatToStrF(NewBalance, ffFixed, 0, 2);
    OldBalance := NewBalance;
  end;
end;

{ Button "Close" pushed: Close the window }

procedure TfAddon.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

