{*************************************************************************}
{* Magic Square: A calculation game (not only) for primary school pupils *}
{*************************************************************************}

program MagicSquare;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  sqmagic;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSqMagic, fSqMagic);
  Application.Run;
end.

