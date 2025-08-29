{*****************************************************}
{* Game: Drop numbered orbs into columns to total 20 *}
{*****************************************************}

program Orbs20;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  orbs,
  highscores;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfOrbs20, fOrbs20);
  Application.CreateForm(TfHighScores, fHighScores);
  Application.Run;
end.

