{************************************************************}
{* Luxembourgish verbs: Participle and compound past tenses *}
{*             Conjugation   exercise generator             *}
{************************************************************}

program Luxverbes2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  verbes2, conj2, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfVerbes, fVerbes);
  Application.CreateForm(TfConj, fConj);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

