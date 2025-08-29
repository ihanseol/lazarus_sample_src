{**********************************************}
{* Mathematics: Analytical Geometry - Circles *}
{**********************************************}

program Circles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, circles_main, circles_graph;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCircles, fCircles);
  Application.CreateForm(TfGraph, fGraph);
  Application.Run;
end.

