{************************************************}
{* French grammar: Feminine of the common nouns *}
{************************************************}

program Substantifs2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, subst2, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSubst2, fSubst2);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

