{**************************************}
{* Bohr’s Theory of the Hydrogen Atom *}
{**************************************}


program Bohr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  bohr_main, bohr_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfBohr, fBohr);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

