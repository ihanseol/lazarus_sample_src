{**********************************************}
{* Learn the Hebrew alphabet (Hebrew letters) *}
{**********************************************}
program Hebrew;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, hebrew_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfHebrew, fHebrew);
  Application.Run;
end.

