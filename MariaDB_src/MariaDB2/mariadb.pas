{**************************************}
{* Main unit for MariaDB2 application *}
{**************************************}

unit mariadb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, odbcconn, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {************}
  { TfMariaDB2 }
  {************}
  TfMariaDB2 = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edCities, edMess: TEdit;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    dbODBCConnection: TODBCConnection;
    dbSQLTransaction: TSQLTransaction;
    dbSQLQuery: TSQLQuery;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fMariaDB2: TfMariaDB2;

implementation

{$R *.lfm}

{************}
{ TfMariaDB2 }
{************}

{ Button "Connect": Connect to "world" databse }

procedure TfMariaDB2.btConnectClick(Sender: TObject);

begin
  if dbODBCConnection.Connected then                                           // close (open) database connection
    dbODBCConnection.Close;
  // Set the connection parameters.
  dbODBCConnection.DatabaseName := 'MariaDB64';
  dbODBCConnection.UserName := 'nemo';
  dbODBCConnection.Password := 'nemo';
  dbODBCConnection.Params.Add('DATABASE=world');
  dbODBCConnection.Params.Add('AUTOCOMMIT=1');
  // Connect to the world database
  try
    dbODBCConnection.Open;
    edMess.Text := 'Connection to MariaDB database "world" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Display number of cities in "city" table }

procedure TfMariaDB2.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbODBCConnection.Connected then begin
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

{ Button "Exit": Disconnect from databse and exit application }

procedure TfMariaDB2.btExitClick(Sender: TObject);

begin
  if dbODBCConnection.Connected then                                           // close (open) database connection
    dbODBCConnection.Close;
  Close;
end;

end.

