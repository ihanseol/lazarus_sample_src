{**********************************************}
{* List of MP3 files in Windows Music Library *}
{**********************************************}

program MusicList;

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
  music, help, WinLibs, ID3v2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMusic, fMusic);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

