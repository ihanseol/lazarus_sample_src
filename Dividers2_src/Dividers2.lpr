{**************************************}
{* Maths game for 2 players: Dividers *}
{**************************************}

program Dividers2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  divider2, keys, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDividers2, fDividers2);
  Application.CreateForm(TfHelp, fHelp);
  Application.CreateForm(TfKeys, fKeys);
  Application.Run;
end.

