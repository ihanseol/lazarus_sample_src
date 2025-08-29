{***************************************************************************}
{* "Amino acids by other classifications" unit for AAStats application     *}
{* (used for: hydropathy, diet-requirements and metabolism classification) *}
{***************************************************************************}

unit aastats_others;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TASources, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

const
  Classification: array[1..3] of string = (
    'Hydropathy', 'Diet-requirement', 'Metabolism'
  );
  ClassificationItems: array[1..3, 1..3] of string = (
    ( 'hydrophobic', 'hydrophilic', 'neutral' ),
    ( 'essential', 'semi-essential', 'non-essential' ),
    ( 'glucogenic', 'ketogenic', 'gluco/keto' )
  );

type
  TArray3I = array[1..3] of LongInt;
  { TfAAOthers }
  TfAAOthers = class(TForm)
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
    iClassification: Integer;                                                  // 1=hydropathy, 2=diet-requirements, 3=metabolism
    iCount: LongInt;
    sFasta: string;
    aCounts: TArray3I;
  end;

var
  fAAOthers: TfAAOthers;

implementation

{$R *.lfm}

{**************}
{* TfAAOthers *}
{**************}

{ Display charts each time the form is shown }

procedure TfAAOthers.FormActivate(Sender: TObject);

const
  Colors: array[1..3, 1..3] of Integer = (
    ( $FF8080, $FF80FF, $FFFFFF ),
    ( $0080FF, $00FFFF, $FFFFFF ),
    ( $FF8000, $FF0080, $FF8080 )
  );

var
  I: Integer;
  Count, Percent: Real;
  SCount, SPercent, Item: string;

begin
  // Clear all
  chBarCounts.Clear; chPercentsPies.Clear;
  lcsLabels.DataPoints.Clear;
  for I := 1 to 3 do begin
    sgCounts.Cells[0, I] := ClassificationItems[iClassification, I];
    sgCounts.Cells[1, I] := '';
    sgCounts.Cells[2, I] := '';
  end;
  // Display FASTA header as title and what classification is used as subtitle
  if sFasta = '' then
    sFasta := 'Unknown protein';
  stProtein.Caption := sFasta;
  laClassification.Caption := 'Classification by ' + LowerCase(Classification[iClassification]);
  sgCounts.Cells[0, 0] := Classification[iClassification];                     // fill classification used into grid cell (0,0)
  Count := iCount;
  for I := 1 to 3 do begin
    // Fill in grid values (amino acids number and percentage)
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
    // Adapt classification items for chart (to avoid overlap with some longer names)
    Item := ClassificationItems[iClassification, I];
    if iClassification = 2 then begin
      if I = 2 then
        Item := 'semi-ess.'
      else if I = 3 then
        Item := 'non-ess.'
    end;
    // Add data (amino acids number) to bar chart
    lcsData.Add(I, aCounts[I], '', Colors[iClassification, I]);
    // Add bar chart's bottom-axis labels (the actual classification items)
    lcsLabels.DataPoints.Append(IntToStr(I) + '|' + IntToStr(I) + '|?|' + Item);
    // Add data (amino acids percentage) to pie chart
    Percent := 100 * (aCounts[I] / iCount);
    if Percent >= 0.5 then
      chPercentsPies.AddPie(Percent, '', Colors[iClassification, I]);
  end;
end;

{ Button "Close": Close the form }

procedure TfAAOthers.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

