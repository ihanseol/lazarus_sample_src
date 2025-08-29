{****************************************************************}
{*         Electronical circuits: Transistor amplifiers         *}
{*--------------------------------------------------------------*}
{* Transistor circuits exercise generator and values calculator *}
{****************************************************************}

program Amplifiers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  amplifiers_u1,
  amplifiers_u2, amplifiers_u3;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfAmplifiers, fAmplifiers);
  Application.CreateForm(TfData, fData);
  Application.CreateForm(TfData2, fData2);
  Application.Run;
end.

