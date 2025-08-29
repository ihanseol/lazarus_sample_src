{**************************************}
{* Electronics: Simple diode circuits *}
{**************************************}

program DCircuits3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, diodes, data, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDiodes, fDiodes);
  Application.CreateForm(TfData, fData);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

