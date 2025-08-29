{************************************************}
{*           Number calculation game            *}
{* -------------------------------------------- *}
{* Inspired form "Des chiffres et des lettres", *}
{* a game-show on the French television         *}
{************************************************}

program Zuelespill;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  numbergame;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfNumberGame, fNumberGame);
  Application.Run;
end.

