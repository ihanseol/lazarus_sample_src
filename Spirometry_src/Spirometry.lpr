{**********************************************************}
{* Spirometry: Measurement of lung volumes and capacities *}
{**********************************************************}

program Spirometry;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  lv, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLV, fLV);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

