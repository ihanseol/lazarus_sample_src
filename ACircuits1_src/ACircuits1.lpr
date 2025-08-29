{*************************************}
{*     Electronics: RLC circuits     *}
{*-----------------------------------*}
{* Simple physics problems generator *}
{*************************************}

program ACircuits1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  circuits1, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfACircuits1, fACircuits1);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

