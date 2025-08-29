{**************}
{* China Quiz *}
{**************}

program ChinaQuiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  quiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfChinaQuiz, fChinaQuiz);
  Application.Run;
end.

