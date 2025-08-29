{**************************************************************}
{* Matrix calculations: Arithmetic operations, row operations *}
{**************************************************************}

program Matrix;

// Actually support for nxm matrices with max size = 4x4

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, matrix_u1, matrix_u2, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMatrix, fMatrix);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

