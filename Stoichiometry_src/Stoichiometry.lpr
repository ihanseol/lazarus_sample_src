{**************************************************************}
{* Chemistry exercise generator: Balancing chemical equations *}
{**************************************************************}

program Stoichiometry;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  stoichiometry_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfStoichiometry, fStoichiometry);
  Application.Run;
end.

