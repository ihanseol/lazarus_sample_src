{**********************************}
{* Geography quiz: European lakes *}
{**********************************}

program EuroLakes;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  lakes, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLakes, fLakes);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

