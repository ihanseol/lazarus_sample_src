{*************************************}
{*  Electrical circuits:  Ohm's Law  *}
{*-----------------------------------*}
{* Simple physics problems generator *}
{*************************************}

program DCircuits1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  circuits1, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TDCircuits1Form, DCircuits1Form);
  Application.CreateForm(THelpForm, HelpForm);
  Application.Run;
end.

