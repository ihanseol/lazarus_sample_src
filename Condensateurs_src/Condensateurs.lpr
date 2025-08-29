{************************************}
{* Electrical circuits: RC circuits *}
{*----------------------------------*}
{* Charge/discharge of a capacitor  *}
{************************************}

program Condensateurs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  rc, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormRC, FormRC);
  Application.CreateForm(TFormHelp, FormHelp);
  Application.Run;
end.

