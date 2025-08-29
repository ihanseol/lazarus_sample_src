{****************************}
{* Puzzle game: Word search *}
{****************************}

program WordSearch;

//
// Change log:
//
// Version 1.0 (April-Mai 2019): Original application
// Version 2.0 (August-September 2019): Application extension (new word display features) and amelioration
//   - Possibility of diagonal words not only from left to right, but also from right to left
//   - Possibility of inverted words (displayed from end to beginning) for horizontal, vertical and diagonal directions
//   - Special marking of word crosses (diagonal-cross instead of solid brush style)
//   - Word list extended (using 6-10 chars instead of 8-10 chars words, what allows more diagonals)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, words, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfWords, fWords);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

