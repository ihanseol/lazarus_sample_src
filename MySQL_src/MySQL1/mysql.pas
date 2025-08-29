{************************************}
{* Main unit for MySQL1 application *}
{************************************}

unit mysql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {**********}
  { TfMySQL1 }
  {**********}
  TfMySQL1 = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edCities: TEdit;
    edMess: TEdit;
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
  fMySQL1: TfMySQL1;

implementation

{$R *.lfm}

{**********}
{ TfMySQL1 }
{**********}

{ Button "Connect": Connect to "world" database }

procedure TfMySQL1.btConnectClick(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                          // close (open) database connection
    dbMySQLConnection.Close;
  // Set the connection parameters.
  dbMySQLConnection.HostName := 'localhost';
  dbMySQLConnection.UserName := 'nemo';
  dbMySQLConnection.Password := 'nemo';
  dbMySQLConnection.DatabaseName := 'world';
  // Connect to the world database
  try
    dbMySQLConnection.Open;
    edMess.Text := 'Connection to MySQL database "world" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := 'Connection to MySQL database "world" FAILED!';
  end;
end;

{ Button "Query": Read number of cities from "city" table }

procedure TfMySQL1.btQueryClick(Sender: TObject);

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

procedure TfMySQL1.btExitClick(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                          // close (open) database connection
    dbMySQLConnection.Close;
  Close;
end;

end.

