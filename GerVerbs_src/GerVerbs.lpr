{*************************************************************************************}
{* Irregular and strong German verbs: Present tense, simple past and past participle *}
{*                          Conjugation  exercise generator                          *}
{*************************************************************************************}

program GerVerbs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  gverbs, gconj, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfGVerbs, fGVerbs);
  Application.CreateForm(TfConj, fConj);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

