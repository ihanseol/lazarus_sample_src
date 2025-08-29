{**********************************************************************************************}
{*                                  Planetary orbits simulation                               *}
{* ------------------------------------------------------------------------------------------ *}
{* 2D Lazarus/FreePascal simulation of the orbital movement of several planets around the sun *}
{* Application based on article "Simulating Planetary Orbits — 50 Examples 1.0 documentation" *}
{* Website: http://fiftyexamples.readthedocs.io/en/latest/index.html                          *}
{* Planets data from NASA website: https://nssdc.gsfc.nasa.gov/planetary/factsheet/           *}
{**********************************************************************************************}

program PlanetaryOrbits;

// Change log:
// Version 1.0 (June 2018): Original program
// Version 1.1 (September 2021)
//   - Pause button reset, when Stop is pressed correction
//   - Code review, comment addition, help texts spellchecking

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  orbits_simul,
  orbits_classes,
  orbits_draw,
  orbits_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfOrbits, fOrbits);
  Application.CreateForm(TfDraw, fDraw);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

