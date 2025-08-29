{*********************************}
{* Conjugaison des verbes en -re *}
{*********************************}

program ConjugaisonRE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  conjRE, conj, selectconj, unknownconj, selectex, common, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfConjRE, fConjRE);
  Application.CreateForm(TfConj, fConj);
  Application.CreateForm(TfSelectEx, fSelectEx);
  Application.CreateForm(TfSelectConj, fSelectConj);
  Application.CreateForm(TfUnknown, fUnknown);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

