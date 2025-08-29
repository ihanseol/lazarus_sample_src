{***********************************************************************}
{*                            Maths Trainer                            *}
{* ------------------------------------------------------------------- *}
{* Equation trainer : Linear equations in 3 variables (3-by-3 systems) *}
{***********************************************************************}

program Equations3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  equations3_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfEquations3, fEquations3);
  Application.Run;
end.

