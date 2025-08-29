{*************************************}
{* Proteines: Amino acids statistics *}
{*************************************}

program AAStats;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, tachartlazaruspkg, aastats_main, aastats_count, aastats_class, aastats_vol, aastats_polarity, aastats_charge, aastats_others;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfAAStats, fAAStats);
  Application.CreateForm(TfAACount, fAACount);
  Application.CreateForm(TfAAClass, fAAClass);
  Application.CreateForm(TfAAVol, fAAVol);
  Application.CreateForm(TfAAPolarity, fAAPolarity);
  Application.CreateForm(TfAACharge, fAACharge);
  Application.CreateForm(TfAAOthers, fAAOthers);
  Application.Run;
end.

