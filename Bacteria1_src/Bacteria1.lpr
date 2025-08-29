{*********************************************************}
{* Bacteria growth on 1 substrate (extended Monod model) *}
{*********************************************************}

program Bacteria1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  bacteria1_main,
  bacteria1_graph,
  bacteria1_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBacteria1, fBacteria1);
  Application.CreateForm(TfBacteria1G, fBacteria1G);
  Application.CreateForm(TfBacteria1H, fBacteria1H);
  Application.Run;
end.

