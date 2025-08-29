{*******************************************************}
{* Latin grammar: Usage of the cases with prepositions *}
{*******************************************************}

program Latin2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  latin2_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLatin2, fLatin2);
  Application.Run;
end.

