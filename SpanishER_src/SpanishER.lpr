{***************************************}
{* Conjugation of Spanish verbs in -er *}
{***************************************}

program SpanishER;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  sper_main, sper_conj, sper_ex, sper_help, conjer;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSpanishER, fSpanishER);
  Application.CreateForm(TfConj, fConj);
  Application.CreateForm(TfSelectEx, fSelectEx);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

