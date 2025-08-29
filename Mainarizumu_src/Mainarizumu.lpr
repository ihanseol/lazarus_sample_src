{***************************}
{* Logic game: Mainarizumu *}
{***************************}

program Mainarizumu;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mainarizumu_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMainarizumu, fMainarizumu);
  Application.Run;
end.

