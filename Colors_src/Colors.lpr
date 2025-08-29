{*************************}
{* The physics of colors *}
{*************************}

program Colors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  colors_main, colors_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfColors, fColors);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

