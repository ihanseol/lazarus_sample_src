{*************************************************}
{* Mathematics: Tangent and normal to an ellipse *}
{*************************************************}

program Ellipse;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, ellipse_u1, ellipse_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfEllipse, fEllipse);
  Application.CreateForm(TfGraph, fGraph);
  Application.Run;
end.

