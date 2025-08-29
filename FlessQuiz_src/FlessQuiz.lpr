{*********************************}
{* Quiz: "D'Flëss zu Lëtzebuerg" *}
{*********************************}

program FlessQuiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, fquiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfFQuiz, fFQuiz);
  Application.Run;
end.

