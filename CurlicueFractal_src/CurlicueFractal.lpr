{****************************************}
{* Lazarus/Free Pascal Curlicue Fractal *}
{****************************************}

program CurlicueFractal;

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
  curlicue, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfCurlicue, fCurlicue);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

