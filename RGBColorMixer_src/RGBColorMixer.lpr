{************************************************}
{ Electronics simulation: Simple RGB color mixer }
{************************************************}

program RGBColorMixer;

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
  rgbled, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfRGBColorMixer, fRGBColorMixer);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

