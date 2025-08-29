{***************************************}
{* Physics simulation: Simple pendulum *}
{***************************************}

program Pendulum;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, pendulum_main, pendulum_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPendulum, fPendulum);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

