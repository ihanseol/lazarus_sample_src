{*********************************************************}
{*          Molecular genetics: Point mutations          *}
{* ----------------------------------------------------- *}
{* Simple point mutation analyser and exercise generator *}
{*********************************************************}

program Mutations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mutations_u1, mutations_u2, mutations_u3;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMutations, fMutations);
  Application.CreateForm(TfGenCode, fGenCode);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

