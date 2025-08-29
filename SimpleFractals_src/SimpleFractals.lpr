{**************************************************}
{* Simple fractals, created with a basic L-System *}
{**************************************************}

program SimpleFractals;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  sfractals;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfSFractals, fSFractals);
  Application.Run;
end.

