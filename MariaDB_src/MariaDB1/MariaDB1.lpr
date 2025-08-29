{***************************************************************}
{* Test connection to MariaDB, using a TMySQLConnection object *}
{***************************************************************}

program MariaDB1;

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
  Application.CreateForm(TfMariaDB1, fMariaDB1);
  Application.Run;
end.

