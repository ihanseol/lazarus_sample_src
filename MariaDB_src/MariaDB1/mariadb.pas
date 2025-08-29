{**************************************}
{* Main unit for MariaDB1 application *}
{**************************************}

unit mariadb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {************}
  { TfMariaDB1 }
  {************}
  TfMariaDB1 = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edCities, edMess: TEdit;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    dbMySQLConnection: TMySQL57Connection;
    dbSQLTransaction: TSQLTransaction;
    dbSQLQuery: TSQLQuery;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fMariaDB1: TfMariaDB1;

implementation

{$R *.lfm}

{************}
{ TfMariaDB1 }
{************}

{ Button "Connect": Connect to "world" database }

procedure TfMariaDB1.btConnectClick(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                          // close (open) database connection
    dbMySQLConnection.Close;
  // Set the connection parameters.
  dbMySQLConnection.HostName := 'localhost';
  dbMySQLConnection.Port := 3307;                                              // with MySQL running on port 3306, I use port 3307 for MariaDB
  dbMySQLConnection.UserName := 'nemo';
  dbMySQLConnection.Password := 'nemo';
  dbMySQLConnection.DatabaseName := 'world';
  // Connect to the world database
  try
    dbMySQLConnection.Open;
    edMess.Text := 'Connection to MariaDB database "world" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := 'Connection to MariaDB database "world" FAILED!';
  end;
end;

{ Button "Query": Read number of cities from "city" table }

procedure TfMariaDB1.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbMySQLConnection.Connected then begin
    // Query the database
    dbSQLQuery.SQL.Text := 'SELECT count(*) FROM city';
    try
      dbSQLQuery.Open;
      if dbSQLQuery.EOF then
        Count := 0
      else
        Count := dbSQLQuery.Fields[0].AsInteger;
      dbSQLQuery.Close;
      // Display the query result
      edCities.Text := IntToStr(Count);
    except
      on E: ESQLDatabaseError do
        edMess.Text := E.Message;
    end;
  end;
end;

{ Button "Exit": Close database connection and exit application }

procedure TfMariaDB1.btExitClick(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                          // close (open) database connection
    dbMySQLConnection.Close;
  Close;
end;

end.

