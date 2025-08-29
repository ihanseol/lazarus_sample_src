{******************************}
{* Learn the Sanskrit letters *}
{******************************}
program Sanskrit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  sanskrit_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSanskrit, fSanskrit);
  Application.Run;
end.

