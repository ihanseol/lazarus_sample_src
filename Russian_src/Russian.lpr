{********************************************************}
{* Learn the Russian alphabet (Cyrillic script letters) *}
{********************************************************}
program Russian;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  russian_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfRussian, fRussian);
  Application.Run;
end.

