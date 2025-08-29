{***********************************}
{* Main unit for IbSQL application *}
{***********************************}

unit ib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {*********}
  { TfIbSQL }
  {*********}
  TfIbSQL = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edEmployees, edMess: TEdit;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    dbSQLTransaction: TSQLTransaction;
    dbIbSQLConnection: TIBConnection;
    dbSQLQuery: TSQLQuery;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fIbSQL: TfIbSQL;

implementation

{$R *.lfm}

{*********}
{ TfIbSQL }
{*********}

{ Button "Connect": Connect to "Tutorial" databse }

procedure TfIbSQL.btConnectClick(Sender: TObject);

begin
  if dbIbSQLConnection.Connected then
    dbIbSQLConnection.Close;
  // Set user name and password
  // Other connection params have been set in dbIbSQLConnection propery sheet
  dbIbSQLConnection.UserName := 'ALLU';
  dbIbSQLConnection.Password := 'Montreal';
  // Connect to "Tutorial" database
  try
    dbIbSQLConnection.Open;
    edMess.Text := 'Connection to InterBase database "Tutorial" = OK!';
  except
    on E: ESQLDatabaseError do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Display number of records in "employee" table }

procedure TfIbSQL.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbIbSQLConnection.Connected then begin
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

procedure TfIbSQL.btExitClick(Sender: TObject);

begin
  if dbIbSQLConnection.Connected then
    dbIbSQLConnection.Close;
  Close;
end;

end.

