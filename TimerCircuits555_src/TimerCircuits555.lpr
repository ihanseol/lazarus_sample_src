{*************************************************************************}
{* Electronics: 555 timer IC - Bistable, monostable and astable circuits *}
{*************************************************************************}

program TimerCircuits555;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, tc555, bistable, monostable, astable, common;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfTC555, fTC555);
  Application.CreateForm(TfBistable, fBistable);
  Application.CreateForm(TfMonoStable, fMonoStable);
  Application.CreateForm(TfAstable, fAstable);
  Application.Run;
end.

