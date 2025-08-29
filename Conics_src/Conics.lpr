{************************************************}
{* Mathematics: Standard form of conic sections *}
{************************************************}

program Conics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, conics_main, conics_graph;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfConics, fConics);
  Application.CreateForm(TfGraph, fGraph);
  Application.Run;
end.

