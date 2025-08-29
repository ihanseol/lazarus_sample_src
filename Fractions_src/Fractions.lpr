{*************************************}
{* Maths game: Addition of fractions *}
{*************************************}

program Fractions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  fractions_u1, fractions_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfFractions, fFractions);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

