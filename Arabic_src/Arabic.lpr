{**********************************************}
{* Learn the Arabic alphabet (Arabic letters) *}
{**********************************************}
program Arabic;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  arabic_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfArabic, fArabic);
  Application.Run;
end.

