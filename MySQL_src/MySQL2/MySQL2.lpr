{************************************************************}
{* Test connection to MySQL, using a TODBCConnection object *}
{************************************************************}

program MySQL2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mysql;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMySQL2, fMySQL2);
  Application.Run;
end.

