{***************************************************************************}
{*                               Maths Trainer                             *}
{* ----------------------------------------------------------------------- *}
{* Equation trainer : Linear equations in 2 variable (2-by-2 systems)      *}
{***************************************************************************}

program Equations2;

// Version 1.1 (one single answer in case of no solution or infinity of solutions)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  equations2_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfEquations2, fEquations2);
  Application.Run;
end.

