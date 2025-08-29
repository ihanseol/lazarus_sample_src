{***************************************}
{* French grammar: Plural of the nouns *}
{***************************************}

program Substantifs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, subst, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSubst, fSubst);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

