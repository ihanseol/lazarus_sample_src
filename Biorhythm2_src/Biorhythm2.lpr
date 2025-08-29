{***********************************}
{* Biorhythm partner compatibility *}
{***********************************}

program Biorhythm2;

// Partner compatibility formulas from: https://github.com/cmsrs/biorhythm

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

