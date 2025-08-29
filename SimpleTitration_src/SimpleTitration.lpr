{************************************************************************************}
{* Titration of a strong acid by a strong base resp. a strong base by a strong acid *}
{************************************************************************************}

program SimpleTitration;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, titration;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfTitration, fTitration);
  Application.Run;
end.

