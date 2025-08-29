{***************************************************}
{* Five Dice Grid: Yahtzee-based game for 1 player *}
{***************************************************}

program FiveDiceGrid;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  fivedice, scores, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfFiveDice, fFiveDice);
  Application.CreateForm(TfScores, fScores);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

