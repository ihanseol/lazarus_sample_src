{**************************************}
{* 17 + 4 dice game for 1 - 3 players *}
{**************************************}

program SiebzehnUndVier;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  suv_main, suv_dice, suv_dice2, suv_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSuv, fSuv);
  Application.CreateForm(TfDice, fDice);
  Application.CreateForm(TfHelp, fHelp);
  Application.CreateForm(TfDice2, fDice2);
  Application.Run;
end.

