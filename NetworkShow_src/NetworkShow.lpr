{**************************************************}
{* Show computers actually present on the network *}
{**************************************************}

program NetworkShow;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  net, cmd, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfNetShow, fNetShow);
  Application.CreateForm(TfCmd, fCmd);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

