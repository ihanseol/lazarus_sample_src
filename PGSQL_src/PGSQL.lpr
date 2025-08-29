{***************************************************************}
{* Test connection to PostgreSQL, using a TPQConnection object *}
{***************************************************************}

program PGSQL;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  pgsql_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfPGSQL, fPGSQL);
  Application.Run;
end.

