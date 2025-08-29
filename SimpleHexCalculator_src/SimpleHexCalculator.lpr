{*****************************************}
{* Simple hexadecimal numbers calculator *}
{*****************************************}

program SimpleHexCalculator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  calculator;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfCalculator, fCalculator);
  Application.Run;
end.

