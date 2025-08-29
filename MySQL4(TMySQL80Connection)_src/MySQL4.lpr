{************************************}
{* Writing data to a MySQL database *}
{************************************}

// Sample application to be used with MySQL 8.0
// Lazarus connection to the database using a TMySQL80Connection object

program MySQL4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mysql4_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMySQL4, fMySQL4);
  Application.Run;
end.

