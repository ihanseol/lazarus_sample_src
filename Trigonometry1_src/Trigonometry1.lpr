{**********************************************}
{* Numerical trigonometry: Triangle exercises *}
{**********************************************}

program Trigonometry1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, trigo1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfTrigo, fTrigo);
  Application.Run;
end.

