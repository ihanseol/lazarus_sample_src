{************************************************}
{* Connection to MariaDB, using ZEOS components *}
{************************************************}

program MariaDBZeos;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mariadbz, zcomponent;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMariaDBZ, fMariaDBZ);
  Application.Run;
end.

