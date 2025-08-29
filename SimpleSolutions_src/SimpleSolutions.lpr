{**************************************}
{* Chemistry: Solution Concentrations *}
{**************************************}

program SimpleSolutions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  solutions, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSolutions, fSolutions);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

