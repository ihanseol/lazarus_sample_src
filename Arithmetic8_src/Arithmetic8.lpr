{*********************************************}
{* Arithmetic trainer: Column multiplication *}
{*********************************************}

program Arithmetic8;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  arithmetic8_main, arithmetic8_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfArithmetic8, fArithmetic8);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

