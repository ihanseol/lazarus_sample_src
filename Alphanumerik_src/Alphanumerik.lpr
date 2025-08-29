{********************************************************************************************}
{* "Guess the word" game, based on Alphanumerik puzzle found in the newspaper "L'essentiel" *}
{********************************************************************************************}

program Alphanumerik;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, alphanumerik_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfAlphanumerik, fAlphanumerik);
  Application.Run;
end.

