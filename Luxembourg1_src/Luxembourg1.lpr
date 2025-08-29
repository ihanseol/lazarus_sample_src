{*********************************************}
{* Quiz: Cantons and townships of Luxembourg *}
{*********************************************}

program Luxembourg1;

// Change log:
// Version 1.0 (October 2019): original program
// Version 2.0 (November 2020):
//   - user choice of number of questions
//   - addition of canton emblems quiz

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  luxbg1, emblems;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLuxbg1, fLuxbg1);
  Application.CreateForm(TfEmblems, fEmblems);
  Application.Run;
end.

