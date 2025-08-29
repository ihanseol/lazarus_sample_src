{***************************************}
{* Main unit for DBConnect application *}
{***************************************}

unit dbconnect_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, SysUtils, FileUtil,
  sqldb, mysql57conn, mysql80conn, PQConnection, MSSQLConn, IBConnection;

type
  {*************}
  { TfDBConnect }
  {*************}
  TfDBConnect = class(TForm)
    stTitle: TStaticText;
    Label1, Label2, Label3, Label4, laCount: TLabel;
    edCount, edMess: TEdit;
    rbConnector1, rbConnector2, rbConnector4: TRadioButton;
    rbConnector3, rbConnector5, rbConnector6: TRadioButton;
    dbSQLTransaction: TSQLTransaction;
    dbSQLConnector: TSQLConnector;
    dbSQLQuery: TSQLQuery;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbConnector1Change(Sender: TObject);
    procedure rbConnector2Change(Sender: TObject);
    procedure rbConnector3Change(Sender: TObject);
    procedure rbConnector4Change(Sender: TObject);
    procedure rbConnector5Change(Sender: TObject);
    procedure rbConnector6Change(Sender: TObject);
  private
    iConnector: Integer;
    bStart, bMissingDLL: Boolean;
  end;

var
  fDBConnect: TfDBConnect;

implementation

{$R *.lfm}

{ User choice (radio button selection) of a new database connector }

procedure ConnectorChange(SQLConnector: TSQLConnector; Connector: Integer; Title: TStaticText; LCount: TLabel; Mess, Count: TEdit; BQuery: TButton);

begin
  if SQLConnector.Connected then                                               // close (open) database connection!!!
    SQLConnector.Close;
  // Set title caption and query result label
  if Connector = 5 then begin
    Title.Caption := 'Reading the number of customers in the "employee" database.';
    LCount.Caption := 'Customers';
  end
  else if Connector = 4 then begin
    Title.Caption := 'Reading the number of customers in the "BikeStores" database.';
    LCount.Caption := 'Customers';
  end
  else begin
    Title.Caption := 'Reading the number of cities in the "world" database.';
    LCount.Caption := 'Cities';
  end;
  // Clear output fields
  Mess.Text := ''; Count.Text := '';
  // Disable "Query" button (user must first connect to the database)
  BQuery.Enabled := False;
end;

{*************}
{ TfDBConnect }
{*************}

{ Application start: Initialization }

procedure TfDBConnect.FormCreate(Sender: TObject);

begin
  bStart := True;
  // Check which version of libmysql.dll is present
  if FileExists('libmysql.dll') then begin
    // DLL is present
    bMissingDLL := False;
    if FileSize('libmysql.dll') > 6E+6 then begin
      // "Big size" DLL indicates MySQL 8.0: MySQL 5.7 databases cannot be accessed
      rbConnector1.Enabled := False;
      rbConnector2.Enabled := True;
      rbConnector2.Checked := True;                                            // MySQL 8.0 is first available choice
    end
    else begin
      // "Small size" DLL indicates MySQL 5.7: MySQL 8.0 databases cannot be accessed
      rbConnector1.Enabled := True;
      rbConnector2.Enabled := False;
      rbConnector1.Checked := True;                                            // MySQL 5.7 is first available choice
    end;
  end
  else begin
    // DLL is missing: MySQL and MariaDB databases cannot be accessed
    bMissingDLL := True;
    rbConnector1.Enabled := False;
    rbConnector2.Enabled := False;
    rbConnector3.Enabled := False;
    rbConnector4.Checked := True;                                              // PostgreSQL is first available choice
  end;
end;

{ Application window becoming active: Display error message if no libmysql.dll hasn't been found }

procedure TfDBConnect.FormActivate(Sender: TObject);

begin
  if bStart then begin
    // Do only at start of application
    if bMissingDLL then
      MessageDlg('DLL not found', 'libmysql.dll not found! Access to MySQL and MariaDB databases will not be supported!', mtWarning, [mbOK], 0);
    bStart := False;
  end;
end;

{ Button "Connect": Connect to the database }

procedure TfDBConnect.btConnectClick(Sender: TObject);

const
  // Connector-type strings that define the TSQLConnection descendant to be used
  Connectors: array[0..5] of string = (
    'MySQL 5.7', 'MySQL 8.0', 'MySQL n.m',
    'PostgreSQL', 'MSSQLServer', 'Firebird'
  );
  // Ports of the different database servers
  Ports: array[0..5] of string = (
    '3306', '3306', '3307',
    '5432', '1433', '3050'
  );
  // Databases that will be accessed on the different servers
  Databases: array[0..5] of string = (
    'world', 'world', 'world',
    'world', 'BikeStores', 'employee'
  );

var
  Connector: string;

begin
  if dbSQLConnector.Connected then                                             // close (open) database connection
    dbSQLConnector.Close;
  // Determine the database specific TSQLConnection descendant
  // In practice, this means: Determine the connector-type specific string value
  Connector := Connectors[iConnector];                                         // connector-type value taken from the array constant
  if iConnector = 2 then begin
    // Special case for MariaDB: We must use the TMySQLnnConnection corresponding to the DLL actually present
    if rbConnector1.Enabled then begin
      // Use MySQL57Connection component
      Connector := StringReplace(Connector, 'n.m', '5.7', []);
    end
    else begin
      // Use MySQL80Connection component
      Connector := StringReplace(Connector, 'n.m', '8.0', []);
    end;
  end;
  // Set the "ConnectorType" property of the TSQLConnector component
  // This will create the corresponding TSQLConnection descendant instance
  dbSQLConnector.ConnectorType := Connector;
  // Set the connection parameters.
  dbSQLConnector.HostName := 'localhost';
  dbSQLConnector.UserName := 'nemo';
  dbSQLConnector.Password := 'nemo';
  dbSQLConnector.DatabaseName := Databases[iConnector];
  // Set the port as additional parameter (not directly)
  dbSQLConnector.Params.Clear;
  dbSQLConnector.Params.Add('port=' + Ports[iConnector]);
  // Connect to the database
  btQuery.Enabled := False;
  try
    // Connection success: Enable "Query" button
    dbSQLConnector.Open;
    edMess.Text := 'Connection to database = OK!';
    btQuery.Enabled := True;
  except
    // Connection failure: Display database error message
    on E: Exception do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Read record count }

procedure TfDBConnect.btQueryClick(Sender: TObject);

const
  // Tables to be accessed on the different servers
  Tables: array[0..5] of string = (
    'city', 'city', 'city', 'city', 'sales.customers', 'customer'
  );

var
  Count: Integer;

begin
  if dbSQLConnector.Connected then begin
    // Query the database
    dbSQLQuery.SQL.Text := 'SELECT count(*) FROM ' + Tables[iConnector];
    try
      // Successful SELECT operation: Display the query result
      dbSQLQuery.Open;
      if dbSQLQuery.EOF then
        Count := 0
      else
        Count := dbSQLQuery.Fields[0].AsInteger;
      dbSQLQuery.Close;
      edCount.Text := IntToStr(Count);
      edMess.Text := 'Select from database = OK!';
    except
      // SELECT operation failure: Display database error message
      on E: Exception do
        edMess.Text := E.Message;
    end;
  end;
end;

{ Button "Exit": Close database connection and exit application }

procedure TfDBConnect.btExitClick(Sender: TObject);

begin
  if dbSQLConnector.Connected then
    dbSQLConnector.Close;
  Close;
end;

{ Database connector-type selection (radio button checked by user) }

procedure TfDBConnect.rbConnector1Change(Sender: TObject);

begin
  if rbConnector1.Checked then begin
    iConnector := 0;
    ConnectorChange(dbSQLConnector, iConnector, stTitle, laCount, edMess, edCount, btQuery);
  end;
end;

procedure TfDBConnect.rbConnector2Change(Sender: TObject);

begin
  if rbConnector2.Checked then begin
    iConnector := 1;
    ConnectorChange(dbSQLConnector, iConnector, stTitle, laCount, edMess, edCount, btQuery);
  end;
end;

procedure TfDBConnect.rbConnector3Change(Sender: TObject);

begin
  if rbConnector3.Checked then begin
    iConnector := 2;
    ConnectorChange(dbSQLConnector, iConnector, stTitle, laCount, edMess, edCount, btQuery);
  end;
end;

procedure TfDBConnect.rbConnector4Change(Sender: TObject);

begin
  if rbConnector4.Checked then begin
    iConnector := 3;
    ConnectorChange(dbSQLConnector, iConnector, stTitle, laCount, edMess, edCount, btQuery);
  end;
end;

procedure TfDBConnect.rbConnector5Change(Sender: TObject);

begin
  if rbConnector5.Checked then begin
    iConnector := 4;
    ConnectorChange(dbSQLConnector, iConnector, stTitle, laCount, edMess, edCount, btQuery);
  end;
end;

procedure TfDBConnect.rbConnector6Change(Sender: TObject);

begin
  if rbConnector6.Checked then begin
    iConnector := 5;
    ConnectorChange(dbSQLConnector, iConnector, stTitle, laCount, edMess, edCount, btQuery);
  end;
end;

end.

