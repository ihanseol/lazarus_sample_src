{**********************************}
{* Chemistry: Solubility of salts *}
{**********************************}

program Solubility;

// Solubility data from https://en.wikipedia.org/wiki/Solubility_chart

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  solubility_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfSolubility, fSolubility);
  Application.Run;
end.

