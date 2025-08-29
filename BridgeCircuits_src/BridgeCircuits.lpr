{********************************************}
{* Electronic circuits: Measurement bridges *}
{********************************************}

program BridgeCircuits;

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
  bridges, maxwell, hay, schering, wien, common;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfBridges, fBridges);
  Application.CreateForm(TfMaxwell, fMaxwell);
  Application.CreateForm(TfHay, fHay);
  Application.CreateForm(TfSchering, fSchering);
  Application.CreateForm(TfWien, fWien);
  Application.Run;
end.

