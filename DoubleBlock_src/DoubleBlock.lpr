{****************************}
{* Logic game: Double Block *}
{****************************}

program DoubleBlock;

// Change log:
//   - version 1.0 (November 2020): Original program
//   - version 1.1 (November 2022):
//       - number generation algorithm changed (lots faster now)
//       - "Show" button display bug corrected

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  dbleblock;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDoubleBlock, fDoubleBlock);
  Application.Run;
end.

