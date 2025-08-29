{**********************}
{* Logic game: Yakuso *}
{**********************}

program Yakuso;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, yakuso_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfYakuso, fYakuso);
  Application.Run;
end.

