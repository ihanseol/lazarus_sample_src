{***********************************}
{* Language trainer: English verbs *}
{***********************************}

program EngVerbs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  enverbs, vlist, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfEngVerbs, fEngVerbs);
  Application.CreateForm(TfList, fList);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

