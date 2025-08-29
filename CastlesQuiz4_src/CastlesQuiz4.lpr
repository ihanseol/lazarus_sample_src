{**********************************}
{* Castle quiz for 1 or 2 players *}
{**********************************}

program CastlesQuiz4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  castles4;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCQuiz4, fCQuiz4);
  Application.Run;
end.

