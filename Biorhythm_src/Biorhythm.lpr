{*****************************}
{* 3-months biorhythm graphs *}
{*****************************}

program Biorhythm;

// Version history:
//  - Version 1.0 (2016): Original program
//  - Version 1.1 (2024):
//     - Birthdate validation vs actual date (instead of fixed year)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  biorhythm_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfBiorhythm, fBiorhythm);
  Application.Run;
end.

