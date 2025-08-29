{***************************************************}
{* Driving license test preparation: Traffic signs *}
{***************************************************}

program Verkehrszeichen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, verkehrszeichen_u1, verkehrszeichen_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfVerkehrszeichen, fVerkehrszeichen);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

