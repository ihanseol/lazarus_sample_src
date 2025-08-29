{********************************************************}
{* "Amino acids by number" unit for AAStats application *}
{********************************************************}

unit aastats_count;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type
  TArray20I = array[1..20] of LongInt;
  TArray20R = array[1..20] of Real;
  { TfAACount }
  TfAACount = class(TForm)
    stProtein: TStaticText;
    Label1, Label2: TLabel;
    edCount: TEdit;
    edMolWeight: TEdit;
    sgCounts: TStringGrid;
    chCounts: TChart;
    chbarCounts: TBarSeries;
    lcsData: TListChartSource;
    lcsLabels: TListChartSource;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iCount: LongInt;
    rMolWeight: Real;
    sFasta: string;
    aCounts: TArray20I;
    aMolWeights: TArray20R;
  end;

var
  fAACount: TfAACount;

implementation

{$R *.lfm}

{*************}
{* TfAACount *}
{*************}

{ Draw the chart (each time this form is shown) }

procedure TfAACount.FormActivate(Sender: TObject);

var
  I: Integer;
  Count, MolWeight, Percent: Real;
  SCount, SMolWeight, SPercent: string;

begin
  // Clear all
  chBarCounts.Clear;
  for I := 1 to 20 do begin
    sgCounts.Cells[3, I] := '';
    sgCounts.Cells[4, I] := '';
    sgCounts.Cells[5, I] := '';
  end;
  // Display FASTA header as title
  if sFasta = '' then
    sFasta := 'Unknown protein';
  stProtein.Caption := sFasta;
  // Display amino acids total count and molecular weight
  Count := iCount;
  edCount.Text := FloatToStrF(Count, ffNumber, 0, 0);
  edMolWeight.Text := FloatToStrF(rMolWeight, ffNumber, 0, 2);
  for I := 1 to 20 do begin
    // Fill in the grid values (number, molecular weight and percentage)
    if aCounts[I] <> 0 then begin
      Count := aCounts[I];
      MolWeight := aMolWeights[I];
      Percent := 100 * (aCounts[I] / iCount);
      // Amino acids number
      SCount := ' ' + FloatToStrF(Count, ffNumber, 0, 0);                      // ffNumber format = same as ffFixed, but with 1000s separators
      if Count < 10000 then                                                    // add spaces to the left for numbers right-alignment
        SCount := '  ' + SCount;
      if Count < 1000 then
        SCount := '  ' + SCount;
      if Count < 100 then
        SCount := ' ' + SCount;
      if Count < 10 then
        SCount := ' ' + SCount;
      // Amino acids molecular weight
      SMolWeight := ' ' + FloatToStrF(MolWeight, ffNumber, 0, 2);
      if MolWeight < 100000 then
        SMolWeight := ' ' + SMolWeight;
      if MolWeight < 10000 then
        SMolWeight := ' ' + SMolWeight;
      if MolWeight < 1000 then
        SMolWeight := '  ' + SMolWeight;
      if MolWeight < 100 then
        SMolWeight := ' ' + SMolWeight;
      // Amino acids number as percentage
      SPercent := FloatToStrF(Percent, ffFixed, 0, 2);
      if Percent < 100 then
        SPercent := ' ' + SPercent;
      if Percent < 10 then
        SPercent := ' ' + SPercent;
      sgCounts.Cells[3, I] := SCount;
      sgCounts.Cells[4, I] := SMolWeight;
      sgCounts.Cells[5, I] := SPercent;
    end;
    // Add data (amino acids number) to the bar chart
    lcsData.Add(I, aCounts[I], '', clBlue);
  end;
end;

{ Button "Close": Close the form }

procedure TfAACount.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

