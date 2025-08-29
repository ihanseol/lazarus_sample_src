{*************************************}
{* Luxembourgish verbs: Present time *}
{*  Conjugation  exercise generator  *}
{*************************************}

{
  Version 1.0: October, 2018
  Version 1.1: November 2018
    - correcting the "vowel doubling" orthography rule: Insertion of "e", if the consonant is "r"
    - adding code (with verb-list) to not consider Luxembourgish verbs in -éieren as dirived from French
    - correcting some GUI-related errors
}

program Luxverbes1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  verbes1, conj1, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfVerbes, fVerbes);
  Application.CreateForm(TfConj, fConj);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

