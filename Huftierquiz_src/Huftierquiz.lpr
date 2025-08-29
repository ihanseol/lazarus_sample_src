{*************************************}
{* Ungulates quiz for 1 or 2 players *}
{*************************************}

program Huftierquiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  htquiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfHtQuiz, fHtQuiz);
  Application.Run;
end.

