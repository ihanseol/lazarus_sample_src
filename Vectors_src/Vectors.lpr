{********************************************}
{* Geometry: Adding and subtracting vectors *}
{********************************************}

program Vectors;

// Change log:
//   - version 1.0 (January 2020): Original program
//   - version 1.0.1 (April 2023):
//       - angle display error (-90°, 360°) fixed

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, vector, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfVectors, fVectors);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

