{****************}
{* Determinants *}
{****************}

program Determinants;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  det, det3, det4, detcalc;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDet, fDet);
  Application.CreateForm(TfDet3, fDet3);
  Application.CreateForm(TfDet4, fDet4);
  Application.Run;
end.

