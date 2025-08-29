{**********************************************************}
{* Physics trainer (dynamics exercise generator): Pulleys *}
{**********************************************************}

program Pulleys;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, pulley, pulley1, pulley2, pulley3, pulley4, pulley5,
  pulley6, pulley7, pulley8, pulley9, pulley10, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPulleys, fPulleys);
  Application.CreateForm(TfPulleys1, fPulleys1);
  Application.CreateForm(TfPulleys2, fPulleys2);
  Application.CreateForm(TfPulleys3, fPulleys3);
  Application.CreateForm(TfPulleys4, fPulleys4);
  Application.CreateForm(TfPulleys5, fPulleys5);
  Application.CreateForm(TfPulleys6, fPulleys6);
  Application.CreateForm(TfPulleys7, fPulleys7);
  Application.CreateForm(TfPulleys8, fPulleys8);
  Application.CreateForm(TfPulleys9, fPulleys9);
  Application.CreateForm(TfPulleys10, fPulleys10);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

