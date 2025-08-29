{********************************************************}
{* Maths trainer: US and UK measurement unit conversion *}
{********************************************************}

program Converter2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  convert2, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfConverter2, fConverter2);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

