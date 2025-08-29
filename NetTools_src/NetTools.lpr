{***********************************************}
{* Network tools user interface for MS Windows *}
{***********************************************}

program NetTools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, net, tools, cmd, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfNetTools, fNetTools);
  Application.CreateForm(TfTools, fTools);
  Application.CreateForm(TfCmd, fCmd);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

