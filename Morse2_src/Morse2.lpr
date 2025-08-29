{****************************}
{* International Morse Code *}
{****************************}

program Morse2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  morse, translation, common, test, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMorse2, fMorse2);
  Application.CreateForm(TfTranslation, fTranslation);
  Application.CreateForm(TfTest, fTest);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

