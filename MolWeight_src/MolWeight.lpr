{*******************************}
{* Molecular weight calculator *}
{*******************************}

program MolWeight;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mw, comp, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMolWeight, fMolWeight);
  Application.CreateForm(TfComposition, fComposition);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

