{*************************************************************}
{* Test connection to MySQL, using a TMySQLConnection object *}
{*************************************************************}

program MySQL1;

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
  Application.CreateForm(TfMySQL1, fMySQL1);
  Application.Run;
end.

