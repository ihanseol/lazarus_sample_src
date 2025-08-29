{**************************}
{* Physics: Hydrodynamics *}
{**************************}

program Fluides2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, fluides;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfFluides2, fFluides2);
  Application.Run;
end.

