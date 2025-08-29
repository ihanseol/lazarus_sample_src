{************************}
{* Maths game: Dividers *}
{************************}

program Dividers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  divider, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDividers, fDividers);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

