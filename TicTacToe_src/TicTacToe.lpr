{********************}
{* Tic-Tac-Toe game *}
{********************}

program TicTacToe;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  game;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfTicTacToe, fTicTacToe);
  Application.Run;
end.

