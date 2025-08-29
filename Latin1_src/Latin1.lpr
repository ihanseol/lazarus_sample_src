{*****************************************}
{* Latin declensions: exercise generator *}
{*****************************************}

program Latin1;

// Version history:
//   Version 1.0 (August 2018): original program
//   Version 1.1 (October 2024):
//     - Replacing the popup notifier by a dialog box
//     - Changing the "number of exercises" input routine (temporary variable)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  latin1_main, latin1_nouns, latin_declensions, latin1_adjs, latin1_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLatin1, fLatin1);
  Application.CreateForm(TfNouns, fNouns);
  Application.CreateForm(TfAdjs, fAdjs);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

