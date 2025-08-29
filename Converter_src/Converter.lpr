{*****************************************}
{* Maths trainer: Measurement conversion *}
{*****************************************}

program Converter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  convert;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfConverter, fConverter);
  Application.Run;
end.

