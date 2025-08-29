{******************************************}
{* Mathtrainer : Arithmetic (percentages) *}
{******************************************}

program Arithmetic5;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  arithmetic5_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfArith5, fArith5);
  Application.Run;
end.

