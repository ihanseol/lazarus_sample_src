{**********************************************************}
{* Chemistry: Graphical reresentation of atomic structure *}
{**********************************************************}

program Atoms;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  atoms_u1, atoms_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfAtoms, fAtoms);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

