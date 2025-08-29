{*******************************************************************************}
{* Educational program for children: The alphabet (English and German letters) *}
{*******************************************************************************}

program Alphabet;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  letters;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfAlphabet, fAlphabet);
  Application.Run;
end.

