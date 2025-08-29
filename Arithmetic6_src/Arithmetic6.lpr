{**************************************************}
{ Maths trainer: Complex numbers basic operations *}
{**************************************************}

program Arithmetic6;

// Version history:
//   Version 1.0 (June 2019):  Original program
//   Version 1.1 (April 2025):
//     - Improvement of the ComplexToString function, avoiding possible R+0i and R-0i display

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  arithmetic6_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

