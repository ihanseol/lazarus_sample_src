{**********************************************************************}
{*                     Logic Game:  Color Circles                     *}
{* ------------------------------------------------------------------ *}
{* Based on the game "Farbkreise", found in Mathematik alpha 2018,    *}
{* copyright © 1985-2018 Steffen Polster - http://mathematikalpha.de * }
{**********************************************************************}

program ColorCircles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  circles, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfColorCircles, fColorCircles);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

