{********************************************}
{* Physiology: Tetanized muscle contraction *}
{********************************************}

program MuscleContraction;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  muscles, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMuscles, fMuscles);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

