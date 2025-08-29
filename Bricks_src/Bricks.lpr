{*************************************}
{* Logic game: Bricks (Brick Sudoku) *}
{*************************************}

program Bricks;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  bricks_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBricks, fBricks);
  Application.Run;
end.

