{******************************************************************************}
{*                   Search for MP3 files on local computer                   *}
{******************************************************************************}
{* Search is done based on the file properties, as well as on the MP3 tags    *}
{* -------------------------------------------------------------------------- *}
{* Notes: 1. Application uses the (modified!) ID3v2 unit to read the MP3 tags *}
{*        2. Windows-only application (usage of Windows file utility units)!  *}
{******************************************************************************}

program SearchMP3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mp3serach,
  mp3files;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSearchMP3, fSearchMP3);
  Application.CreateForm(TfFilesMP3, fFilesMP3);
  Application.Run;
end.

