{***********************************}
{* Main unit for PGSQL application *}
{***********************************}

unit pgsql_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, sqldb, PQConnection, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {*********}
  { TfPGSQL }
  {*********}
  TfPGSQL = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edCities: TEdit;
    edMess: TMemo;
    dbSQLTransaction: TSQLTransaction;
    dbPQConnection: TPQConnection;
    dbSQLQuery: TSQLQuery;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fPGSQL: TfPGSQL;

implementation

{$R *.lfm}

{*********}
{ TfPGSQL }
{*********}

{ Button "Connect": Connect to "world" database }

procedure TfPGSQL.btConnectClick(Sender: TObject);

begin
  if dbPQConnection.Connected then                                             // close (open) database connection
    dbPQConnection.Close;
  // Set the connection parameters.
  dbPQConnection.HostName := 'localhost';
  dbPQConnection.UserName := 'nemo';
  dbPQConnection.Password := 'nemo';
  dbPQConnection.DatabaseName := 'world';
  // Connect to the world database
  edMess.Lines.Clear;
  try
    dbPQConnection.Open;
    edMess.Lines.AddText('Connection to PostgreSQL database "world" = OK!');
  except
    on E: Exception do
      edMess.Lines.AddText(E.Message);
  end;
end;

{ Button "Query": Read number of cities from "city" table }

procedure TfPGSQL.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbPQConnection.Connected then begin
    // Query the database
    dbSQLQuery.SQL.Text := 'SELECT count(*) FROM public.city';
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
      on E: Exception do begin
        edMess.Lines.Clear;
        edMess.Lines.AddText(E.Message);
      end;
    end;
  end;
end;

{ Button "Exit": Close database connection and exit application }

procedure TfPGSQL.btExitClick(Sender: TObject);

begin
  if dbPQConnection.Connected then                                             // close (open) database connection
    dbPQConnection.Close;
  Close;
end;

end.

