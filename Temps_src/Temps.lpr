{**********************************************}
{* Grammaire françsise: Concordance des temps *}
{**********************************************}

program Temps;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  temps_u1, temps_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfTemps, fTemps);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

