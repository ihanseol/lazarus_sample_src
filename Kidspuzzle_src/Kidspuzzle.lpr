{********************************}
{* Classic puzzle game for kids *}
{********************************}

program Kidspuzzle;

// Version history:
// V1.0 (April 2020): Original program
// V2.0 (June 2020):
//     - Replacing the puzzle selection in the "Puzzle" menu by a combination of a TSelectDirectoryDialog and a configuration file,
//       allowing to later add any number of puzzles, without having to change the program
//     - Renaming the puzzle directories (number of pieces + full puzzle name)
//     - Allowing specific picture format (not necessarily .jpg) for a given puzzle
//     - Extending the maximum puzzle size to 6 x 10 = 60 pieces
//     - Addition of 3 new puzzle pictures
// V3.0 (November 2020):
//     - Save and Load functions (making possible to quit and resume later)
//     - Puzzle download function from website (manual download)
//     - Extending the maximum puzzle size to 6 x 12 = 72 pieces
//     - Addition of 6 new puzzle pictures
// V3.0.1 (April 2021):
//     - Changing code for opening of .html help file (did not work with Chromium browsers!)
// V4.0 (November 2021):
//     - Extending the maximum puzzle size to 8 x 15 = 120 pieces
//     - New default puzzle picture and 3 new "version 4" puzzle pictures

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, puzzle;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPuzzle, fPuzzle);
  Application.Run;
end.

