{************************************************************}
{* Biology simulation: Predator-prey (Lotka-Volterra) model *}
{************************************************************}

// Version 1.0 (January-July 2019)
// Version 1.1 (October 2019):
//   - Optional usage of logistic growth for the prey.
//   - Correction of some improper displays.

program LotkaVolterra1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  lv1_main,
  lv1_graph,
  lv1_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLV1, fLV1);
  Application.CreateForm(TfLV1G, fLV1G);
  Application.CreateForm(TfLV1H, fLV1H);
  Application.Run;
end.

