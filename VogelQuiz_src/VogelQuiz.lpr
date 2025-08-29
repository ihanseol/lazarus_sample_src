{********************************}
{* Bird quiz for 1 or 2 players *}
{********************************}

program VogelQuiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  vquiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfVQ, fVQ);
  Application.Run;
end.

