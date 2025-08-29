{**************************************************************}
{* Visual FoxPro 9 record addition, deletion and modification *}
{**************************************************************}

program FoxPro2;

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
  foxpro2_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfFoxPro2, fFoxPro2);
  Application.Run;
end.

