{ **************************************************************************** }
{                    Mathtrainer : Arithmetic (fractions)                   * }
{ **************************************************************************** }

// Basic arithmetics operations with fractions (positive numbers only)

program Arithmetic2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, arithmetic2_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

