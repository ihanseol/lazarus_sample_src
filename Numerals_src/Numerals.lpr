{************************************************************}
{* Foreign languages: Numerals from 1 to 10 in 45 languages *}
{************************************************************}

program Numerals;

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
  num10;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfNumerals, fNumerals);
  Application.Run;
end.

