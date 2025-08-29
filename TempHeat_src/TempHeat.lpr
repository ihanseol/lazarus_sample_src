{**********************************************************************}
{* Physics: Some simple calculations, concerning temperature and heat *}
{**********************************************************************}

program TempHeat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, tempheat_main, tempheat_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfTempHeat, fTempHeat);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

