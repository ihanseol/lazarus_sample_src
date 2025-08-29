{******************}
{* Roman numerals *}
{******************}

program Roman;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  roman_u1, roman_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfRoman, fRoman);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

