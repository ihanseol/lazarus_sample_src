{************************************************************************}
{* Number puzzle: Guess what numbers correspond to the different shapes *}
{************************************************************************}

program ZueleRaetsel;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, raetsel;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfRaetsel, fRaetsel);
  Application.Run;
end.

