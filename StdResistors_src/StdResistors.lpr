{***********************************}
{* Electronics: Standard resistors *}
{***********************************}

program StdResistors;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  resistors,
  help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfResistors, fResistors);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

