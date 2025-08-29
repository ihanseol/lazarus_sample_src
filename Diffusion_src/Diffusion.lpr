{*************************************************}
{* Chemistry simulation: Fick's Law of Diffusion *}
{*************************************************}

program Diffusion;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  fick;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDiffusion, fDiffusion);
  Application.Run;
end.

