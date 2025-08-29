{ ******************** }
{ Time calculator v1.0 }
{ ******************** }

program TimeCalculator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  calculator, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TCalculatorForm, CalculatorForm);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

