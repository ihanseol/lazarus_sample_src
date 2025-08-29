{********************************************}
{* Learn the Greek alphabet (Greek letters) *}
{********************************************}
program Greek;

// Change log:
// Version 1.0 (January 2020): Original program
// Version 1.1 (January 2021):
//   - random position of letter buttons
//   - display of number of correct answers

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  greek_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfGreek, fGreek);
  Application.Run;
end.

