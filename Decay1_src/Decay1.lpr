{***********************************************************}
{* Nuclear physics: Radioactive decay (exercise generator) *}
{***********************************************************}

program Decay1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, decay1_main, decay1_elements, decay1_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDecay1, fDecay1);
  Application.CreateForm(TfElements, fElements);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

