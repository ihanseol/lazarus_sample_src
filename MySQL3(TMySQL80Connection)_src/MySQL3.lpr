{*******************************************}
{* Reading UTF8 data from a MySQL database *}
{*******************************************}

// Sample application to be used with MySQL 8.0
// Lazarus connection to the database using a TMySQL80Connection object

program MySQL3;

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
  Application.CreateForm(TfMySQL3, fMySQL3);
  Application.Run;
end.

