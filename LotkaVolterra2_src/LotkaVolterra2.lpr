{********************************************************************************}
{* Biology simulation: 3-species predator-prey model (Lotka-Volterra extension) *}
{********************************************************************************}

program LotkaVolterra2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  lv2_main,
  lv2_graph,
  lv2_samples,
  lv2_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLV2, fLV2);
  Application.CreateForm(TfLV2G, fLV2G);
  Application.CreateForm(TfLV2S, fLV2S);
  Application.CreateForm(TfLV2H, fLV2H);
  Application.Run;
end.

