{**********************************************************}
{* Test connection to DB2, using a TODBCConnection object *}
{**********************************************************}

program DB2Connect;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  db2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDB2, fDB2);
  Application.Run;
end.

