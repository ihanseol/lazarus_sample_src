{**************************}
{* Logic game: Lightbulbs *}
{**************************}

program Lightbulbs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  lbulbs;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLightbulbs, fLightbulbs);
  Application.Run;
end.

