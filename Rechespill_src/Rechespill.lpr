{*****************************************}
{* Simple calculation dice game for kids *}
{*****************************************}

program Rechespill;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, spill;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfRSpill, fRSpill);
  Application.Run;
end.

