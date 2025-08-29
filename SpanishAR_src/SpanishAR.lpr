{***************************************}
{* Conjugation of Spanish verbs in -ar *}
{***************************************}

program SpanishAR;

// Change log:
// Version 1.0 (March - September 2021):
//   - Original program, based on French verbs conjugation applications
// Version 1.0.1 (April 2022):
//   - Exercise verbs selection bug fix
//   - Irregularities coloration bug fix

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  spar_main, spar_conj, spar_ex, spar_help, conjar;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSpanishAR, fSpanishAR);
  Application.CreateForm(TfConj, fConj);
  Application.CreateForm(TfSelectEx, fSelectEx);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

