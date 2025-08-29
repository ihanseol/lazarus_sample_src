{***********************************}
{* Classic Skyscrapers puzzle game *}
{***********************************}

program Skyscrapers;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  skyscrapers_u1, skyscrapers_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfSkyscrapers, fSkyscrapers);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

