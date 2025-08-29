{**********************************}
{* Yahtzee game for 1 - 3 players *}
{**********************************}

program Yahtzee;

// Change log:
// Version 1.0 (March 2020): Original program
// Version 1.1 (December 2020):
//   - adding choice between dices rolled by program or user entry of real dice rolling values

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  yahtzee_main, yahtzee_dice, yahtzee_help1, yahtzee_help2, yahtzee_dice2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfYahtzee, fYahtzee);
  Application.CreateForm(TfDice, fDice);
  Application.CreateForm(TfScoring, fScoring);
  Application.CreateForm(TfHelp, fHelp);
  Application.CreateForm(TfDice2, fDice2);
  Application.Run;
end.

