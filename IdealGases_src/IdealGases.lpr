{**********************************}
{*  Chemistry: The Ideal Gas Law  *}
{**********************************}

program IdealGases;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  gases, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfGases, fGases);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

