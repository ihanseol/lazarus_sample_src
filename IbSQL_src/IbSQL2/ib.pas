{************************************}
{* Main unit for IbSQL2 application *}
{************************************}

unit ib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, sqldb, odbcconn, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {**********}
  { TfIbSQL2 }
  {**********}
  TfIbSQL2 = class(TForm)
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
  fIbSQL2: TfIbSQL2;

implementation

{$R *.lfm}

{**********}
{ TfIbSQL2 }
{**********}

{ Button "Connect": Connect to "Tutorial" databse }

procedure TfIbSQL2.btConnectClick(Sender: TObject);

begin
  if dbODBCConnection.Connected then
    dbODBCConnection.Close;
  // Connection parameters are set in DSN
  // Connect to the employee database
  try
    dbODBCConnection.Open;
    edMess.Text := 'Connection to InterBase database "Tutorial" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Display number of records in "employee" table }

procedure TfIbSQL2.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbODBCConnection.Connected then begin
    // Query the database
    dbSQLQuery.SQL.Text := 'SELECT count(*) FROM employee';
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

procedure TfIbSQL2.btExitClick(Sender: TObject);

begin
  if dbODBCConnection.Connected then
    dbODBCConnection.Close;
  Close;
end;

end.

