{*********************}
{* 1x1 maths trainer *}
{*********************}

program EinMalEins;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  einxeins;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tf1x1, f1x1);
  Application.Run;
end.

