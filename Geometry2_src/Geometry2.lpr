{******************************************}
{* Geometry: Solids volume & surface area *}
{******************************************}

program Geometry2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  geo2, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfGeo2, fGeo2);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

