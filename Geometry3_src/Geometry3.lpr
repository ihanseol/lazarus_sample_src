{***************************************************************************}
{*                 Geometry: The geometric transformations                 *}
{* (translations, simple rotations, simple reflexions, simple homotheties) *}
{***************************************************************************}

program Geometry3;

// Change log:
// Version 1.0 (December 2020): Original program (translations and simple rotations)
// Version 2.0 (March 2021): Addition of simple reflexions and simple homotheties

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, geo3, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfGeo3, fGeo3);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

