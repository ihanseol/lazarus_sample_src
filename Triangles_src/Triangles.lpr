{****************************************************}
{*      Centroid and orthocenter of a triangle      *}
{* (Centre de gravité et orthocentre d'un triangle) *}
{****************************************************}

program Triangles;

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
  triangles_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfTriangles, fTriangles);
  Application.Run;
end.

