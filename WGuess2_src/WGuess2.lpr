{**********************************}
{* "Guess the word II" game, v1.0 *}
{**********************************}

program WGuess2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  wguess2_u1, wguess2_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TWGuess2Form, WGuess2Form);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

