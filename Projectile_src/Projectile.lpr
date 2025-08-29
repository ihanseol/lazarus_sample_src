{***********************************************}
{* Physics: Projectile motion (firing a canon) *}
{***********************************************}

program Projectile;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  projectile_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfProjectile, fProjectile);
  Application.Run;
end.

