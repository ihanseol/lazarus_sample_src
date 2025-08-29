{******************************}
{* Germany Quiz: Bundesländer *}
{******************************}

program DeutschlandQuiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  dequiz, deflags;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDEQuiz, fDEQuiz);
  Application.CreateForm(TfDEFlags, fDEFlags);
  Application.Run;
end.

