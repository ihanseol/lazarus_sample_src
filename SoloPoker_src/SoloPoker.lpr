{***************************}
{* Poker game for 1 player *}
{***************************}

program SoloPoker;

// Version history:
//   Version 1.0 (July 2024): Original program
//   Version 1.1 (February 2025)
//     - Add option to choose between 1 and 2 card decks

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  poker, help;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfPoker, fPoker);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

