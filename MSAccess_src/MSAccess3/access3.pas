unit access3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, odbcconn, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfAccess3 }
  TfAccess3 = class(TForm)
    dbTransaction: TSQLTransaction;
    dbQuery: TSQLQuery;
    dbConnection: TODBCConnection;
    Label1, Label2, Label3: TLabel;
    edConnect, edAdd, edUpdate, edDelete: TEdit;
    btConnect: TButton;
    btAdd: TButton;
    btUpdate: TButton;
    btDelete: TButton;
    btExit: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btUpdateClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

const
  EmployeeName = 'Baba';
  EmployeeFirstname = 'Ali';
  EmployeeCity = 'Luxembourg';
  EmployeeCountry = 'Luxembourg';

var
  fAccess3: TfAccess3;

implementation

{$R *.lfm}

{ TfAccess3 }

{ Button "Connect" pushed: Connect to "Northwind" database }

procedure TfAccess3.btConnectClick(Sender: TObject);

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

{ Button "Add" pushed: Add record to "Employees" table }

procedure TfAccess3.btAddClick(Sender: TObject);

var
  Query: string;

begin
  if dbConnection.Connected then begin
    edAdd.Text := ''; edUpdate.Text := ''; edDelete.Text := '';
    try
      // Check if employee doesn't yet exist
      Query := 'SELECT COUNT(EmployeeID) FROM Employees WHERE LastName = ''' + EmployeeName + '''';
      Query += ' AND FirstName = ''' + EmployeeFirstName + '''';
      dbQuery.SQL.Text := Query;
      dbQuery.Open;
      if dbQuery.Fields[0].AsInteger = 0 then begin
        // Employee does not yet exist: Add the record
        dbQuery.Close;
        Query := 'INSERT INTO Employees(FirstName, LastName, City, Country)';
        Query += ' Values(''' + EmployeeFirstName + ''', ''' + EmployeeName + ''', ''';
        Query += EmployeeCity + ''', ''' + EmployeeCountry + ''')';
        dbQuery.SQL.Text := Query;
        dbQuery.ExecSQL;
        dbTransaction.Commit;
        // Get new employee's ID
        Query := 'SELECT EmployeeID FROM Employees WHERE LastName = ''' + EmployeeName + '''';
        Query += ' AND FirstName = ''' + EmployeeFirstName + '''';
        dbQuery.SQL.Text := Query;
        dbQuery.Open;
        if dbQuery.EOF then
          edAdd.Text := 'Unknown error: Employee has not been added!'
        else
          edAdd.Text := 'Employee has been added with ID = ' + IntToStr(dbQuery.Fields[0].AsInteger);
        dbQuery.Close;
      end
      else begin
        // Employee already exists
        edAdd.Text := 'Employee already exists!';
        dbQuery.Close;
      end;
    except
      on E: ESQLDatabaseError do
        edAdd.Text := E.Message;
    end;
  end;
end;

{ Button "Update" pushed: Change city for new employee }

procedure TfAccess3.btUpdateClick(Sender: TObject);

var
  EmployeeID: Integer;
  Query: string;

begin
  if dbConnection.Connected then begin
    edAdd.Text := ''; edUpdate.Text := ''; edDelete.Text := '';
    try
      // Check if employee exists
      Query := 'SELECT EmployeeID FROM Employees WHERE LastName = ''' + EmployeeName + '''';
      Query += ' AND FirstName = ''' + EmployeeFirstName + '''';
      dbQuery.SQL.Text := Query;
      dbQuery.Open;
      if dbQuery.EOF then begin
        // Employee does not exist
        edUpdate.Text := 'Employee does not exist!';
        dbQuery.Close;
      end
      else begin
        // Employee exists: Change the city
        EmployeeID := dbQuery.Fields[0].AsInteger;
        dbQuery.Close;
        Query := 'UPDATE Employees';
        Query += ' SET City = ''Echternach'' WHERE EmployeeID = ' + IntToStr(EmployeeID);
        dbQuery.SQL.Text := Query;
        dbQuery.ExecSQL;
        dbTransaction.Commit;
        // Get new city
        Query := 'SELECT City FROM Employees WHERE LastName = ''' + EmployeeName + '''';
        Query += ' AND FirstName = ''' + EmployeeFirstName + '''';
        dbQuery.SQL.Text := Query;
        dbQuery.Open;
        if dbQuery.EOF then
          edUpdate.Text := 'Unknown error: Employee not found!'
        else
          edUpdate.Text := 'City has been changed to ' + dbQuery.Fields[0].AsString;
        dbQuery.Close;
      end;
    except
      on E: ESQLDatabaseError do
        edUpdate.Text := E.Message;
    end;
  end;
end;

{ Button "Delete" pushed: Delete the new employee }

procedure TfAccess3.btDeleteClick(Sender: TObject);

var
  EmployeeID: Integer;
  Query: string;

begin
  if dbConnection.Connected then begin
    edAdd.Text := ''; edUpdate.Text := ''; edDelete.Text := '';
    try
      // Check if employee exists
      Query := 'SELECT EmployeeID FROM Employees WHERE LastName = ''' + EmployeeName + '''';
      Query += ' AND FirstName = ''' + EmployeeFirstName + '''';
      dbQuery.SQL.Text := Query;
      dbQuery.Open;
      if dbQuery.EOF then begin
        // Employee does not exist
        edDelete.Text := 'Employee does not exist!';
        dbQuery.Close;
      end
      else begin
        // Employee exists: Delete the record
        EmployeeID := dbQuery.Fields[0].AsInteger;
        dbQuery.Close;
        Query := 'DELETE FROM Employees';
        Query += ' WHERE EmployeeID = ' + IntToStr(EmployeeID);
        dbQuery.SQL.Text := Query;
        dbQuery.ExecSQL;
        dbTransaction.Commit;
        // Check if the deletion has been done
        Query := 'SELECT EmployeeID FROM Employees WHERE LastName = ''' + EmployeeName + '''';
        Query += ' AND FirstName = ''' + EmployeeFirstName + '''';
        dbQuery.SQL.Text := Query;
        dbQuery.Open;
        if dbQuery.EOF then
          edDelete.Text := 'Employee has been deleted'
        else
          edDelete.Text := 'Unknown error: Employee still exits!';
        dbQuery.Close;
      end;
    except
      on E: ESQLDatabaseError do
        edUpdate.Text := E.Message;
    end;
  end;
end;

{ Button "Exit" pushed: Exit application }

procedure TfAccess3.btExitClick(Sender: TObject);

begin
  if dbConnection.Connected then
    dbConnection.Close();
  Close;
end;

end.

