//
// "Guess the word" game, v1.0
//

program WGuess;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  wguess_u1, wguess_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TWGuessForm, WGuessForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

