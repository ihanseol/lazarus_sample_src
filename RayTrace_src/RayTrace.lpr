{***********************************}
{* Geometrical optics: Ray tracing *}
{***********************************}

program RayTrace;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  raytrace_u1, raytrace_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfOptics, fOptics);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

