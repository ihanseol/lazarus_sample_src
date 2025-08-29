{*******************************************}
{* Geometry: Surfaces area & circumference *}
{*******************************************}

program Geometry1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  geo1, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfGeo1, fGeo1);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

