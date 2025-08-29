{********************************}
{* Blackjack game for 2 players *}
{********************************}

// Version 1.0 (January 2018)
// Version 2.0 (March-June 2019):
//   - Realistic card dealing: Dealer takes cards when both players are done,
//     i.e. he plays with the same cards against the 2 players.
//   - 1-player option: NOT YET implemented (feature disabled).

program BlackJack2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  blackjack2_u1, blackjack2_u2;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfBlackjack2, fBlackjack2);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

