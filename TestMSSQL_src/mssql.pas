{***************************************}
{* Main unit for TestMSSQL application *}
{***************************************}

unit mssql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MSSQLConn, SQLDB, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {*********}
  { TfMSSQL }
  {*********}
  TfMSSQL = class(TForm)
    dbConn: TMSSQLConnection;
    dbQuery: TSQLQuery;
    dbTrans: TSQLTransaction;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edCount, edMess: TEdit;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
  end;

var
  fMSSQL: TfMSSQL;

implementation

{$R *.lfm}

{*********}
{ TfMSSQL }
{*********}

{ Button "Connect": Connect to the database }

procedure TfMSSQL.btConnectClick(Sender: TObject);

begin
  if dbConn.Connected then
    dbConn.Close;
  // Set the connection parameters.
  dbConn.HostName := 'localhost';
  dbConn.UserName := 'nemo';
  dbConn.Password := 'nemo';
  dbConn.DatabaseName := 'BikeStores';
  // Connect to the BikeStores database
  try
    dbConn.Open;
    edMess.Text := 'Connection to SQL Server database "BikeStores" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := 'Connection to SQL Server database "BikeStores" FAILED!';
  end;
end;

{ Button "Query": Read number of customers from the database }

procedure TfMSSQL.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  edCount.Text := '';
  if dbConn.Connected then begin
    // Query the database
    dbQuery.SQL.Text := 'SELECT COUNT(*) FROM sales.customers';
    try
      dbQuery.Open;
      if dbQuery.EOF then
        Count := 0
      else
        Count := dbQuery.Fields[0].AsInteger;
      dbQuery.Close;
      // Display the query result
      edCount.Text := IntToStr(Count);
    except
      on E: ESQLDatabaseError do
        edMess.Text := E.Message;
    end;
  end;
end;

{ Button "Exit": Exit application }

procedure TfMSSQL.btExitClick(Sender: TObject);

begin
  if dbConn.Connected then
    dbConn.Close;
  Close;
end;

end.

