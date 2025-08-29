{********************************************}
{* Geometry: Properties of regular polygons *}
{********************************************}

program Polygons;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  polygons_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPolygons, fPolygons);
  Application.Run;
end.

