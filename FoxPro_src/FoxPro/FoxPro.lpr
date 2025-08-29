{************************************************************}
{* Test Lazarus/Free Pascal access to Visual FoxPro 9 table *}
{************************************************************}

program FoxPro;

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
  foxpro_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfFoxPro, fFoxPro);
  Application.Run;
end.

