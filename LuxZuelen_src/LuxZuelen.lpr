{**************************************}
{* Basic arithmetics in luxembourgish *}
{**************************************}

program LuxZuelen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  calc;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCalc, fCalc);
  Application.Run;
end.

