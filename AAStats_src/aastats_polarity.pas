{**********************************************************}
{* "Amino acids by polarity" unit for AAStats application *}
{**********************************************************}

unit aastats_polarity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type
  TArray2I = array[1..2] of LongInt;
  TArray4I = array[1..4] of LongInt;
  { TfAAPolarity }
  TfAAPolarity = class(TForm)
    stProtein: TStaticText;
    laClassification: TLabel;
    sgCounts: TStringGrid;
    sgCounts2: TStringGrid;
    chCounts: TChart;
    chBarCounts: TBarSeries;
    lcsData: TListChartSource;
    lcsLabels: TListChartSource;
    chCounts2: TChart;
    chBarCounts2: TBarSeries;
    lcsData2: TListChartSource;
    lcsLabels2: TListChartSource;
    chPercents: TChart;
    chPercentsPies: TPieSeries;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iCount: LongInt;
    sFasta: string;
    aCounts1: TArray2I;
    aCounts2: TArray4I;
  end;

var
  fAAPolarity: TfAAPolarity;

implementation

{$R *.lfm}

{****************}
{* TfAAPolarity *}
{****************}

{ Display the charts each time the form is shown }

procedure TfAAPolarity.FormActivate(Sender: TObject);

const
  Colors1: array[1..2] of Integer = (
    $0000FF, $FFFFFF
  );
  Colors2: array[1..4] of Integer = (
    $0080FF, $8000FF, $8080FF, $FFFFFF
  );

var
  I: Integer;
  Max: LongInt;
  Count, Percent: Real;
  SCount, SPercent: string;

begin
  // Clear all
  chBarCounts.Clear; chBarCounts2.Clear; chPercentsPies.Clear;
  for I := 1 to 2 do begin
    sgCounts.Cells[1, I] := '';
    sgCounts.Cells[2, I] := '';
  end;
  for I := 1 to 4 do begin
    sgCounts2.Cells[2, I] := '';
    sgCounts2.Cells[3, I] := '';
  end;
  // Display FASTA header as title
  if sFasta = '' then
    sFasta := 'Unknown protein';
  stProtein.Caption := sFasta;
  Count := iCount;
  // Classification by polarity
  for I := 1 to 2 do begin
    // Fill in the first grid values (amino acids numbers and percentages)
    if aCounts1[I] <> 0 then begin
      Count := aCounts1[I];
      Percent := 100 * (aCounts1[I] / iCount);
      SCount := ' ' + FloatToStrF(Count, ffNumber, 0, 0);
      if Count < 100000 then
        SCount := ' ' + SCount;
      if Count < 10000 then
        SCount := ' ' + SCount;
      if Count < 1000 then
        SCount := '  ' + SCount;
      if Count < 100 then
        SCount := ' ' + SCount;
      if Count < 10 then
        SCount := ' ' + SCount;
      SPercent := FloatToStrF(Percent, ffFixed, 0, 2);
      if Percent < 100 then
        SPercent := ' ' + SPercent;
      if Percent < 10 then
        SPercent := ' ' + SPercent;
      sgCounts.Cells[1, I] := SCount;
      sgCounts.Cells[2, I] := SPercent;
    end;
    // Add data (amino acids number) to first bar chart
    lcsData.Add(I, aCounts1[I], '', Colors1[I]);
  end;
  // Usage of the same Y-axis scaling (same bar height for same values) for the 2 bar charts
  // Realizing this by setting the maximal range of the second's chart left axis to the actual maximal Y-value on the first chart
  if aCounts1[2] > aCounts1[1] then
    Max := aCounts1[2]
  else
    Max := aCounts1[1];
  chCounts2.LeftAxis.Range.Max := Max;
  chCounts2.LeftAxis.Range.UseMax := True;
  // Classification by sidechain (splitting up "polar" into "polar positive" and "polar negative" and "polar uncharged")
  for I := 1 to 4 do begin
    // Fill in the second grid values (amino acids number and percentage)
    if aCounts2[I] <> 0 then begin
      Count := aCounts2[I];
      Percent := 100 * (aCounts2[I] / iCount);
      SCount := ' ' + FloatToStrF(Count, ffNumber, 0, 0);
      if Count < 100000 then
        SCount := ' ' + SCount;
      if Count < 10000 then
        SCount := ' ' + SCount;
      if Count < 1000 then
        SCount := '  ' + SCount;
      if Count < 100 then
        SCount := ' ' + SCount;
      if Count < 10 then
        SCount := ' ' + SCount;
      SPercent := FloatToStrF(Percent, ffFixed, 0, 2);
      if Percent < 100 then
        SPercent := ' ' + SPercent;
      if Percent < 10 then
        SPercent := ' ' + SPercent;
      sgCounts2.Cells[2, I] := SCount;
      sgCounts2.Cells[3, I] := SPercent;
    end;
    // Add data (amino acids number) to the second bar chart
    lcsData2.Add(I, aCounts2[I], '', Colors2[I]);
    // Add data (amino acids percentages) to the pie chart
    Percent := 100 * (aCounts2[I] / iCount);
    if Percent >= 0.5 then
      chPercentsPies.AddPie(Percent, '', Colors2[I]);
  end;
end;

{ Button "Close": Close the form }

procedure TfAAPolarity.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

