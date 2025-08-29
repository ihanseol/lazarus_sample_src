{*****************************************************************}
{* Electronic circuits: Multivibrators made with npn-transistors *}
{*****************************************************************}

program Multivibrator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mv, astable, monostable, bistable;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMV, fMV);
  Application.CreateForm(TfMVA, fMVA);
  Application.CreateForm(TfMVM, fMVM);
  Application.CreateForm(TfMVB, fMVB);
  Application.Run;
end.

