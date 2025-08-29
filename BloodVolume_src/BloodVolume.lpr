{***************************}
{* Blood volume calculator *}
{***************************}

program BloodVolume;

// Change log:
//   Version 1.0 (May 2018): original program
//   Version 2.0 (October 2020):
//     - addition of TBV calculation using the Lemmens-Bernstein-Brodsky equation
//     - changing inches to cm conversion factor
//     - allowing TBV determination with Gilcher's Rule of Five with adults only

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  vblood, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfVBlood, fVBlood);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

