unit access2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, odbcconn, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  { TfAccess2 }
  TfAccess2 = class(TForm)
    dbTransaction: TSQLTransaction;
    dbQuery: TSQLQuery;
    dbConnection: TODBCConnection;
    Label1, Label2, Label3: TLabel;
    edConnect, edQuery, edCategory: TEdit;
    sgProducts: TStringGrid;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fAccess2: TfAccess2;

implementation

{$R *.lfm}

{ TfAccess2 }

{ Button "Connect" pushed: Connect to "Northwind" database }

procedure TfAccess2.btConnectClick(Sender: TObject);

begin
  if dbConnection.Connected then
    dbConnection.Close();
  dbConnection.Params.Add('DBQ=C:\Program Files\Microsoft Office\Office10\Samples\Northwind.mdb');
  try
    dbConnection.Open;
    edConnect.Text := 'Successfully connected to "Northwind" database';
    edCategory.SetFocus;
  except
    on E: ESQLDatabaseError do
      edConnect.Text := E.Message;
  end;
end;

{ Button "Query" pushed: Read products for category ID entered by user }

procedure TfAccess2.btQueryClick(Sender: TObject);

var
  Category, Count, I, J: Integer;
  Query: string;

begin
  for I := 1 to sgProducts.RowCount - 1 do begin
    for J := 0 to 2 do
      sgProducts.Cells[J, I] := '';
  end;
  if edCategory.Text = '' then
    Category := -1
  else
    Category := StrToInt(edCategory.Text);
  if Category < 1 then begin
    // Invalid user input
    edQuery.Text := 'Missing or invalid category!';
    edCategory.SetFocus;
  end
  else if dbConnection.Connected then begin
    // User input and connection ok: Execute the query
    try
      // Read number of query records and adapt number of stringgrid rows accordingly
      Query := 'SELECT COUNT(ProductName) FROM Products WHERE CategoryID = ' + IntToStr(Category);
      dbQuery.SQL.Text := Query;
      dbQuery.Open;
      if dbQuery.EOF then
        Count := 0
      else
        Count := dbQuery.Fields[0].AsInteger;
      dbQuery.Close;
      if Count = 0 then
        edQuery.Text := 'Unknown category, or no products for this category!'
      else begin
        // Add rows to stringgrid if required
        if Count > 10 then
          sgProducts.RowCount := Count + 1
        else
          sgProducts.RowCount := 11;
        edQuery.Text := 'Number of procults found: ' + IntToStr(Count);
        // Now run the products by category query
        Query := 'SELECT ProductName, UnitsInStock, UnitsOnOrder FROM Products';
        Query += ' WHERE CategoryID = ' + IntToStr(Category);
        Query += ' ORDER by ProductName';
        dbQuery.SQL.Text := Query;
        dbQuery.Open;
        dbQuery.First;
        I := 0;
        while not dbQuery.EOF do begin
          Inc(I);
          sgProducts.Cells[0, I] := dbQuery.Fields[0].AsString;
          sgProducts.Cells[1, I] := IntToStr(dbQuery.Fields[1].AsInteger);
          sgProducts.Cells[2, I] := IntToStr(dbQuery.Fields[2].AsInteger);
          dbQuery.Next;
        end;
        dbQuery.Close;
      end;
    except
      // Error during SELECT
      on E: ESQLDatabaseError do
        edQuery.Text := E.Message;
    end;
  end;
end;

{ Button "Exit" pushed: Exit application }

procedure TfAccess2.btExitClick(Sender: TObject);

begin
  if dbConnection.Connected then
    dbConnection.Close();
  Close;
end;

end.

