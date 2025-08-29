{******************************************************}
{* Mineral chemistry: Nomenclature of ionic compounds *}
{******************************************************}

program IonicCompounds;

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
  compounds;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfCompounds, fCompounds);
  Application.Run;
end.

