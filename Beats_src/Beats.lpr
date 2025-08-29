{*****************************************************}
{* Physics: Beats - Superposition of two sound waves *}
{*****************************************************}

program Beats;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  beats_main, beats_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfBeats, fBeats);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

