{**********************************}
{* Castle quiz for 1 or 2 players *}
{**********************************}

program CastlesQuiz1;

// Change log:
// Version 1.0 (May 2021): Original program
// Version 1.0.1 (June 2021):
//   - replacement of a certain numbers of photos (that may be copyright protected)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  castles1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCQuiz1, fCQuiz1);
  Application.Run;
end.

