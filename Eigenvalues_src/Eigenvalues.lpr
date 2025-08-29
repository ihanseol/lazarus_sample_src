{*************************************************************}
{* Mathematics: Eigenvalues and eigenvectors of a 2x2 matrix *}
{*************************************************************}

program Eigenvalues;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms, eigenv;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfEigenvalues, fEigenvalues);
  Application.Run;
end.

