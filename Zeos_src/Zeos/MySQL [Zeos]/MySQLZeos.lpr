{**********************************************}
{* Connection to MySQL, using ZEOS components *}
{**********************************************}

program MySQLZeos;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mysqlz, zcomponent;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMySQLZ, fMySQLZ);
  Application.Run;
end.

