{**********************************************}
{* Maya numbers conversion and knowledge test *}
{**********************************************}

program MayaNumbers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  numbers, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfNumbers, fNumbers);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

