unit access;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, odbcconn, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfAccess }
  TfAccess = class(TForm)
    Label1, Label2, Label3: TLabel;
    edConnect, edCustomers: TEdit;
    dbTransaction: TSQLTransaction;
    dbQuery: TSQLQuery;
    dbConnection: TODBCConnection;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fAccess: TfAccess;

implementation

{$R *.lfm}

{ TfAccess }

{ Button "Connect" pushed: Connect to "Northwind" database }

procedure TfAccess.btConnectClick(Sender: TObject);

begin
  if dbConnection.Connected then
    dbConnection.Close();
  dbConnection.Params.Add('DBQ=C:\Program Files\Microsoft Office\Office10\Samples\Northwind.mdb');
  try
    dbConnection.Open;
    edConnect.Text := 'Successfully connected to "Northwind" database';
  except
    on E: ESQLDatabaseError do
      edConnect.Text := E.Message;
  end;
end;

{ Button "Query" pushed: Read number of customers }

procedure TfAccess.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if dbConnection.Connected then begin
    dbQuery.SQL.Text := 'SELECT COUNT(*) FROM Customers';
    try
      dbQuery.Open;
      if dbQuery.EOF then
        Count := 0
      else
        Count := dbQuery.Fields[0].AsInteger;
      edCustomers.Text := IntToStr(Count);;
    except
      on E: ESQLDatabaseError do
        edCustomers.Text := E.Message;
    end;
  end;
end;

{ Button "Exit" pushed: Exit application }

procedure TfAccess.btExitClick(Sender: TObject);

begin
  if dbConnection.Connected then
    dbConnection.Close();
  Close;
end;

end.

