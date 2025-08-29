{****************************************}
{* Main unit for DB2Connect application *}
{****************************************}

unit db2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, odbcconn, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {*******}
  { TfDB2 }
  {*******}
  TfDB2 = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edEmployees, edMess: TEdit;
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
  fDB2: TfDB2;

implementation

{$R *.lfm}

{*******}
{ TfDB2 }
{*******}

{ Button "Connect": Connect to "sample" databse }

procedure TfDB2.btConnectClick(Sender: TObject);

begin
  if dbODBCConnection.Connected then                                           // close (open) database connection
    dbODBCConnection.Close;
  // Set the connection parameters.
  dbODBCConnection.DatabaseName := 'DB2_sample';
  dbODBCConnection.UserName := 'Allu';
  dbODBCConnection.Password := 'AlluUser0';
  dbODBCConnection.Params.Add('DATABASE=sample');
  dbODBCConnection.Params.Add('AUTOCOMMIT=1');
  // Connect to the sample database
  try
    dbODBCConnection.Open;
    edMess.Text := 'Connection to DB2 database "sample" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Display number of records in "employee" table }

procedure TfDB2.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbODBCConnection.Connected then begin
    // Query the database
    dbSQLQuery.SQL.Text := 'SELECT count(*) FROM Admin.employee';
    try
      dbSQLQuery.Open;
      if dbSQLQuery.EOF then
        Count := 0
      else
        Count := dbSQLQuery.Fields[0].AsInteger;
      dbSQLQuery.Close;
      // Display the query result
      edEmployees.Text := IntToStr(Count);
    except
      on E: ESQLDatabaseError do
        edMess.Text := E.Message;
    end;
  end;
end;

{ Button "Exit": Disconnect from databse and exit application }

procedure TfDB2.btExitClick(Sender: TObject);

begin
  if dbODBCConnection.Connected then                                           // close (open) database connection
    dbODBCConnection.Close;
  Close;
end;

end.

