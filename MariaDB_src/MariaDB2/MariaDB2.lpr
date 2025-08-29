{**************************************************************}
{* Test connection to MariaDB, using a TODBCConnection object *}
{**************************************************************}

program MariaDB2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mariadb;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMariaDB2, fMariaDB2);
  Application.Run;
end.

