{*************************************}
{* "Mirrors" game, v1.0, © allu 2018 *}
{*************************************}

program Mirrors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mirrors_u1,
  mirrors_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMirrors, fMirrors);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

