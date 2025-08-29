{************************************}
{* Main unit for MySQLZ application *}
{************************************}

unit mysqlz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ZConnection, ZDataset;

type
  {**********}
  { TfMySQLZ }
  {**********}
  TfMySQLZ = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edCities: TEdit;
    edMess: TEdit;
    btConnect: TButton;
    btQuery: TButton;
    btExit: TButton;
    zdbConnection: TZConnection;
    zdbQuery: TZQuery;
    procedure btConnectClick(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fMySQLZ: TfMySQLZ;

implementation

{$R *.lfm}

{**********}
{ TfMySQLZ }
{**********}

{ Button "Connect": Connect to "world" database }

procedure TfMySQLZ.btConnectClick(Sender: TObject);

begin
  if zdbConnection.Connected then                                              // close (open) database connection
    zdbConnection.Disconnect;
  // Set the connection parameters
  zdbConnection.Protocol := 'mysql';                                           // connect to a MySQL database
  zdbConnection.HostName := 'localhost';
  zdbConnection.Port := 3306;
  zdbConnection.User := 'nemo';
  zdbConnection.Password := 'nemo';
  zdbConnection.Database := 'world';
  // Connect to the world database
  try
    zdbConnection.Connect;
    edMess.Text := 'Connection to MySQL database "world" = OK!';
  except
    on E: Exception do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Read number of cities from "city" table }

procedure TfMySQLZ.btQueryClick(Sender: TObject);

var
  Count: Integer;

begin
  if zdbConnection.Connected then begin
    // Query the database
    zdbQuery.SQL.Clear;
    zdbQuery.SQL.Add('SELECT count(*) FROM city');
    try
      zdbQuery.Open;
      if zdbQuery.EOF then
        Count := 0
      else
        Count := zdbQuery.Fields[0].AsInteger;
      zdbQuery.Close;
      // Display the query result
      edCities.Text := IntToStr(Count);
    except
      on E: Exception do
        edMess.Text := E.Message;
    end;
  end;
end;

{ Button "Exit": Close database connection and exit application }

procedure TfMySQLZ.btExitClick(Sender: TObject);

begin
  if zdbConnection.Connected then                                              // close (open) database connection
    zdbConnection.Disconnect;
  Close;
end;

end.

