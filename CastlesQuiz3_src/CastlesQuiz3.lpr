{********************************************************}
{* Castle quiz for 1 or 2 players: Castles of NW Europe *}
{********************************************************}

program CastlesQuiz3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  castles3;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCQuiz3, fCQuiz3);
  Application.Run;
end.

