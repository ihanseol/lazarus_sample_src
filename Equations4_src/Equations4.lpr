{**************************************}
{* Maths trainer: Logarithm equations *}
{**************************************}


program Equations4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  equations4_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfEquations4, fEquations4);
  Application.Run;
end.

