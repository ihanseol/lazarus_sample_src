{**************************************************************}
{* English grammar trainer (exercise generator): Prepositions *}
{**************************************************************}

program Prepositions1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  prepositions;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfPrepositions, fPrepositions);
  Application.Run;
end.

