{****************************************************}
{* Basic arithmetic operations with complex numbers *}
{****************************************************}

program SimpleCplxCalculator;

// Version history
//  Version 1.0 (October-December 2018): Original program
//  Version 1.1 (April 2025):
//     - Improvement of the ComplexToString function, avoiding possible R+0i and R-0i display

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
  Application.CreateForm(TfCalculator, fCalculator);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

