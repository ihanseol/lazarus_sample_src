{*******************************************************}
{* Physics simulation: Hydrostatic pressure in liquids *}
{*******************************************************}

program HSPressure;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  hsp;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfHSPressure, fHSPressure);
  Application.Run;
end.

