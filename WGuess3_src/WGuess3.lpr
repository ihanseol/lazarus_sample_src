{*****************************************}
{* Specialized subjects word search game *}
{*****************************************}

program WGuess3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  wguess, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfWGuess3, fWGuess3);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

