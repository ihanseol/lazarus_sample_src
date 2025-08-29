{*************************}
{* Logic game: Futoshiki *}
{*************************}

program Futoshiki;

// Change log:
// Version 1.0 (September 2021): Original program
// Version 1.1 (November 2021):
//   - starting application with 6x6 () instead of 5x5) puzzle resolves
//     issue of incomplete comparison operator display between 5th and 6th column
//   - speeding up puzzle generation (using IF OK... within the FOR loop)
//   - some other minor changes

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  futoshiki_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfFutoshiki, fFutoshiki);
  Application.Run;
end.

