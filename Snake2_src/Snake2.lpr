{************************************************************}
{*                    Simple SNAKE game                     *}
{* -------------------------------------------------------- *}
{* Extended GUI version of the "snake" command line program *}
{************************************************************}


program Snake2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  snake2_u1, snake2_u2, snake2_u3;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSnake2, fSnake2);
  Application.CreateForm(TfGame, fGame);
  Application.CreateForm(TfGameOver, fGameOver);
  Application.Run;
end.

