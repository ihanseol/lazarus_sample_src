{***********************}
{* Logic game: Kendoku *}
{***********************}

program Kendoku;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  kendoku_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfKendoku, fKendoku);
  Application.Run;
end.

