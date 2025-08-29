{***************************************************************************}
{* Bacteria growth in the chemostat (1 substrate Monod based growth model) *}
{***************************************************************************}

program Bacteria3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  bacteria3_main,
  bacteria3_graph,
  bacteria3_help, bacteria3_analysis;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBacteria3, fBacteria3);
  Application.CreateForm(TfBacteria3G, fBacteria3G);
  Application.CreateForm(TfBacteria3H, fBacteria3H);
  Application.CreateForm(TfBacteria3A, fBacteria3A);
  Application.Run;
end.

