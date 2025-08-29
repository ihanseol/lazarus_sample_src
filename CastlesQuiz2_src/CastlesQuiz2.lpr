{**********************************}
{* Castle quiz for 1 or 2 players *}
{**********************************}

program CastlesQuiz2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  castles2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCQuiz2, fCQuiz2);
  Application.Run;
end.

