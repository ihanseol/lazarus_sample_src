{****************}
{* Flowers quiz *}
{****************}

program Blumenquiz;

// Change log:
//   - V1.0 (October 2020): original program
//   - V1.0.1 (April 2021):
//   - Changing code for opening of .html help file (did not work with Chromium browsers!)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  bquiz, flowers;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBQuiz, fBQuiz);
  Application.CreateForm(TfFlowers, fFlowers);
  Application.Run;
end.

