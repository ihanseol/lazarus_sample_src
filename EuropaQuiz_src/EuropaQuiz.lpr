{***************}
{* Europe Quiz *}
{***************}

// Changelog:
// Version 1.0 (January 2018): Original program
// Version 1.1 (December 2019):
//   - Extending the quiz options by adding the EU entry year
// Version 2.0 (March 2020):
//   - Usage of a second form (instead of opening the "flags" directory) for flag selection

program EuropaQuiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  euroquiz, euroflags;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TEQuiz, EQuiz);
  Application.CreateForm(TEFlags, EFlags);
  Application.Run;
end.

