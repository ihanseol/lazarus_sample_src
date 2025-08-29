{*************************************************}
{* Physics trainer: Static Equilibrium exercises *}
{* Balancing the forces that act upon an object  *}
{*************************************************}

program Equilibrium;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, balance, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBalance, fBalance);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

