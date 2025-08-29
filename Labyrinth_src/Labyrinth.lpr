{*************}
{* Labyrinth *}
{*************}

program Labyrinth;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  labyrinth_u1, labyrinth_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLabyrinth, fLabyrinth);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

