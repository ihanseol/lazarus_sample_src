{***********************************}
{* Main unit for FbSQL application *}
{***********************************}

unit fb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {*********}
  { TfFbSQL }
  {*********}
  TfFbSQL = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edCustomers, edMess: TEdit;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    dbSQLTransaction: TSQLTransaction;
    dbFBSQLConnection: TIBConnection;
    dbSQLQuery: TSQLQuery;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fFbSQL: TfFbSQL;

implementation

{$R *.lfm}

{*********}
{ TfFbSQL }
{*********}

{ Button "Connect": Connect to "employee" databse }

procedure TfFbSQL.btConnectClick(Sender: TObject);

begin
  if dbFBSQLConnection.Connected then                                          // close (open) database connection
    dbFBSQLConnection.Close;
  // Set the connection parameters.
  dbFBSQLConnection.HostName := 'localhost';
  dbFBSQLConnection.Port := 3050;
  dbFBSQLConnection.DatabaseName := 'employee';
  dbFBSQLConnection.UserName := 'nemo';
  dbFBSQLConnection.Password := 'nemo';
  // Connect to the employee database
  try
    dbFBSQLConnection.Open;
    edMess.Text := 'Connection to Firebird database "employee" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Display number of customers in "employee" table }

procedure TfFbSQL.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbFBSQLConnection.Connected then begin
    // Query the database
    dbSQLQuery.SQL.Text := 'SELECT count(*) FROM customer';
    try
      dbSQLQuery.Open;
      if dbSQLQuery.EOF then
        Count := 0
      else
        Count := dbSQLQuery.Fields[0].AsInteger;
      dbSQLQuery.Close;
      // Display the query result
      edCustomers.Text := IntToStr(Count);
    except
      on E: ESQLDatabaseError do
        edMess.Text := E.Message;
    end;
  end;
end;

{ Button "Exit": Disconnect from databse and exit application }

procedure TfFbSQL.btExitClick(Sender: TObject);

begin
  if dbFBSQLConnection.Connected then                                          // close (open) database connection
    dbFBSQLConnection.Close;
  Close;
end;

end.

