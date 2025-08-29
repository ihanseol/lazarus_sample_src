{*************************************}
{* Right of way at a street crossing *}
{*     Test questions  generator     *}
{*************************************}

program Priorite;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, priorite_u1, priorite_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPriorite, fPriorite);
  Application.CreateForm(TfData, fData);
  Application.Run;
end.

