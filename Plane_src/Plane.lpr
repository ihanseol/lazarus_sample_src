{********************************************}
{* Kinematics simulation: Plane in the wind *}
{********************************************}

program Plane;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  plane_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPlane, fPlane);
  Application.Run;
end.

