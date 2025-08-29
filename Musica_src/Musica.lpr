{***********************************}
{* (Very simple) music application *}
{***********************************}

program Musica;

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
  music, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMusica, fMusica);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

