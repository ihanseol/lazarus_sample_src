{********************************************************}
{* "Amino acids by volume" unit for AAStats application *}
{********************************************************}

unit aastats_vol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  TArray5I = array[1..5] of LongInt;
  { TfAAVol }
  TfAAVol = class(TForm)
    stProtein: TStaticText;
    laClassification: TLabel;
    sgCounts: TStringGrid;
    chCounts: TChart;
    chBarCounts: TBarSeries;
    lcsData: TListChartSource;
    lcsLabels: TListChartSource;
    chPercents: TChart;
    chPercentsPies: TPieSeries;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iCount: LongInt;
    sFasta: string;
    aCounts: TArray5I;
  end;

var
  fAAVol: TfAAVol;

implementation

{$R *.lfm}

{***********}
{* TfAAVol *}
{***********}

{ Display the charts each time the form is shown }

procedure TfAAVol.FormActivate(Sender: TObject);

const
  Colors: array[1..5] of Integer = (
    $00FF00, $008000, $00FFFF, $0080FF, $0000FF
  );

var
  I: Integer;
  Count, Percent: Real;
  SCount, SPercent: string;

begin
  // Clear all
  chBarCounts.Clear; chPercentsPies.Clear;
  for I := 1 to 5 do begin
    sgCounts.Cells[1, I] := '';
    sgCounts.Cells[2, I] := '';
  end;
  // Display FASTA header as title
  if sFasta = '' then
    sFasta := 'Unknown protein';
  stProtein.Caption := sFasta;
  Count := iCount;
  for I := 1 to 5 do begin
    // Fill in the grid values (amino acids number and percentage)
    if aCounts[I] <> 0 then begin
      Count := aCounts[I];
      Percent := 100 * (aCounts[I] / iCount);
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
    // Add data (amino acids number) to the bar chart
    lcsData.Add(I, aCounts[I], '', Colors[I]);
    // Add data (amino acids percentage) to the pie chart
    Percent := 100 * (aCounts[I] / iCount);
    if Percent >= 0.5 then
      chPercentsPies.AddPie(Percent, '', Colors[I]);
  end;
end;

{ Button "Close": Close the form }

procedure TfAAVol.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

