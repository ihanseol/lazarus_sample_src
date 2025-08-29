{**********************************************}
{* Connector-type independent database access *}
{**********************************************}

program DBConnect;

// This sample database application shows how to use the same TSQLConnector object to read
//   - the number of cities in the "world" database (MySQL 5.7, MySQL 8.0, MariaDB, PostgreSQL)
//   - the number of customers in the "BikeStores" database (MSSQLServer)
//   - the number of customers in the "employee" database (Firebird)

// For details, please visit the tutorial at https://www.streetinfo.lu/computing/lazarus/project/dbconnect.html

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  dbconnect_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDBConnect, fDBConnect);
  Application.Run;
end.

