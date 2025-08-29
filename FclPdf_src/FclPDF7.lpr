{************************************}
{*     fcl-pdf (fppdf) example      *}
{* Drawing an image onto an A4 page *}
{*     (c) allu, February 2025      *}
{************************************}

program FclPDF7;

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
  pdf7;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfPDF7, fPDF7);
  Application.Run;
end.

