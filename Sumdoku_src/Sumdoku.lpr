{***********************}
{* Logic game: Sumdoku *}
{***********************}

program Sumdoku;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, sumdoku_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSumdoku, fSumdoku);
  Application.Run;
end.

