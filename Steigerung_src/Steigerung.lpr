{*******************************************************}
{* German grammar: Degrees of comparison of adjectives *}
{*******************************************************}

program Steigerung;

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
  steigerung_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfSteigerung, fSteigerung);
  Application.Run;
end.

