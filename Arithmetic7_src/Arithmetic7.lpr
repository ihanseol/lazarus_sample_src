{****************************************************}
{* Algebra trainer: Column addition and subtraction *}
{****************************************************}

program Arithmetic7;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  arithmetic7_main, arithmetic7_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfArithmetic7, fArithmetic7);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

