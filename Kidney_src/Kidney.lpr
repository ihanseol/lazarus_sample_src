{********************}
{* Renal Physiology *}
{********************}

program Kidney;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  kidney_main, kidney_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfKidney, fKidney);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

