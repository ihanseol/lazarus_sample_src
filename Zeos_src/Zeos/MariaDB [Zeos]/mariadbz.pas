{**************************************}
{* Main unit for MariaDBZ application *}
{**************************************}

unit mariadbz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ZConnection, ZDataset;

type
  {************}
  { TfMariaDBZ }
  {************}
  TfMariaDBZ = class(TForm)
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
  fMariaDBZ: TfMariaDBZ;

implementation

{$R *.lfm}

{************}
{ TfMariaDBZ }
{************}

{ Button "Connect": Connect to "world" database }

procedure TfMariaDBZ.btConnectClick(Sender: TObject);

begin
  if zdbConnection.Connected then                                              // close (open) database connection
    zdbConnection.Disconnect;
  // Set the connection parameters
  zdbConnection.Protocol := 'MariaDB-10';                                      // connect to a MariaDB 10 database
  zdbConnection.HostName := 'localhost';
  zdbConnection.Port := 3307;
  zdbConnection.User := 'nemo';
  zdbConnection.Password := 'nemo';
  zdbConnection.Database := 'world';
  // Connect to the world database
  try
    zdbConnection.Connect;
    edMess.Text := 'Connection to MariaDB database "world" = OK!';
  except
    on E: Exception do
      edMess.Text := E.Message;
  end;
end;

{ Button "Query": Read number of cities from "city" table }

procedure TfMariaDBZ.btQueryClick(Sender: TObject);

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

procedure TfMariaDBZ.btExitClick(Sender: TObject);

begin
  if zdbConnection.Connected then                                              // close (open) database connection
    zdbConnection.Disconnect;
  Close;
end;

end.

