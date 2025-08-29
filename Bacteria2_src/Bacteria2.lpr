{**********************************************************}
{* Bacteria growth on 2 substrates (extended Monod model) *}
{**********************************************************}

program Bacteria2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  bacteria2_main,
  bacteria2_graph,
  bacteria2_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBacteria2, fBacteria2);
  Application.CreateForm(TfBacteria2G, fBacteria2G);
  Application.CreateForm(TfBacteria2H, fBacteria2H);
  Application.Run;
end.

