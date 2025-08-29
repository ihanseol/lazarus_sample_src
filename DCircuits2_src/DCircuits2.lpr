{************************************************}
{*   Electrical circuits: Transistor switches   *}
{*----------------------------------------------*}
{* Transistor circuits resistance determination *}
{************************************************}

program DCircuits2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  transistors, data;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfTransistors, fTransistors);
  Application.CreateForm(TfData, fData);
  Application.Run;
end.

