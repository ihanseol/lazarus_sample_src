{*********************************************************}
{* French grammar: Feminine and plural of the adjectives *}
{*********************************************************}

// Change log:
//   - Version 1.0 (January 2020): original program
//   - Version 1.1 (April 2020): Bug fixes:
//       - correct letter insert for 'â' button
//       - marking of the adjectives already asked
//       - applying modification of "no regular adj" option only with new exercise
//         (avoiding hanging of program if question number > irreg. adj. number)

program Adjectifs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, adj, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfAdj, fAdj);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

