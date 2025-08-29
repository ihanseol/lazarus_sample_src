{*********************************************************}
{* Electronics: Some calculations concerning capacitors: *}
{*  - Capacitance for given shape or given dielectric    *}
{*  - Capacitors in parallel and in series               *}
{*  - DC circuits: Charge and discharge of a capacitor   *}
{*********************************************************}

program Capacitors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  capacitors_u1, capacitors_u2, capacitors_u3, capacitors_u4;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfCapacitors, fCapacitors);
  Application.CreateForm(TfCircuits, fCircuits);
  Application.CreateForm(TfDC, fDC);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

