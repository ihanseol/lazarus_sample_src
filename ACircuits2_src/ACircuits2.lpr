{***************************************}
{* Electronics: Resonance RLC circuits *}
{*-------------------------------------*}
{* Simple physics problems generator   *}
{***************************************}

program ACircuits2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  circuits2, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfACircuits2, fACircuits2);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

