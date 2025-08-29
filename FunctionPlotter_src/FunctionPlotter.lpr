{************************************}
{* Function plotter (using Gnuplot) *}
{************************************}

program FunctionPlotter;

// Version history:
//   Version 1.0 (August 2024): Original program
//   Version 2.0 (December 2004):
//     - added "2 functions plot" feature

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
  fplotter,
  graph, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfFunctionPlotter, fFunctionPlotter);
  Application.CreateForm(TfGraph, fGraph);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

