{******************************************************************************}
{*                                Memory  Game                                *}
{******************************************************************************}

program Memory;

// Change log:
//  - Version 1.0 (May 2016):
//      - 6x6 squares puzzle with orchid pictures
//  - Version 2.0 (February 2018):
//      - choice of 4x6, 5x6 or 6x6 squares
//      - 6 different picture sets (orchids, flowers, beaches, sunrise, space and angels)
//  - Version 3.0 (July 2020):
//      - complete code review and optimization
//      - picture set selection, using a TSelectDirectory object
//      - Settings menu for puzzle size (from New menu) and choice betwwen 2- and new 3-same-pictures game mode
//      - partial picture exchange; 4 new picture sets: waterfalls, winter, birds, fishes
//  - Version 3.1 (January 2021):
//      - separating application from picture sets (from now on downloadable ZIP archives)
//      - 4 new picture sets: mountains, lakes, fall, castles
//  - Version 3.2 (May 2034):
//      - adding the menu command "Restart": new game without browsing for the picture set
//      - adding the possibility to choose between 2 picture series
//      - new picture set (classic cars), the first one with 2 picture series


{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  memory_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMemory, fMemory);
  Application.Run;
end.

