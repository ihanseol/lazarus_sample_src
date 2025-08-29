{************}
{* USA Quiz *}
{************}

program USAQuiz;

// Change log:
//  - Version 1.0 (May 2020): original program
//  - Version 2.0 (August 2020):
//      - New quiz selection: state seals

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  usquiz, usflagseals;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfUSQuiz, fUSQuiz);
  Application.CreateForm(TfUSAFlagSeals, fUSAFlagSeals);
  Application.Run;
end.

