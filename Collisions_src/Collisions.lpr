{*******************************************}
{* Physics: Linear momentum and collisions *}
{*******************************************}

program Collisions;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  collisions_u1, collisions_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCollisions, fCollisions);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

