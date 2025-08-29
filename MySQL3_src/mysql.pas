{************************************}
{* Main unit for MySQL3 application *}
{************************************}

unit mysql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  {**********}
  { TfMySQL3 }
  {**********}
  TfMySQL3 = class(TForm)
    StaticText1: TStaticText;
    sgCities: TStringGrid;
    Label2, laCountry: TLabel;
    edCountry: TEdit;
    edMess: TEdit;
    btQuery: TButton;
    btExit: TButton;
    dbMySQLConnection: TMySQL57Connection;
    dbSQLTransaction: TSQLTransaction;
    dbSQLQuery: TSQLQuery;
    procedure FormCreate(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fMySQL3: TfMySQL3;

implementation

{$R *.lfm}

{**********}
{ TfMySQL3 }
{**********}

{ Application start: Connect to the database }

procedure TfMySQL3.FormCreate(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                          // close (open) database connection
    dbMySQLConnection.Close;
  // Set the connection parameters.
  dbMySQLConnection.HostName := 'localhost';
  dbMySQLConnection.UserName := 'nemo';
  dbMySQLConnection.Password := 'nemo';
  dbMySQLConnection.DatabaseName := 'world';
  // Connect to the world database
  try
    dbMySQLConnection.Open;
    edMess.Color := clDefault;;
    edMess.Text  := 'Connected to MySQL database "world"';
    // Mandatory queries when using UTF-8 data (at least for MySQL 5.7 and prior)!
    // It is possible that for MySQL 8.0 (and higher) these queries must NOT be executed anymore
    // If you get "garbage" with non-ANSI characters, try commenting the following 2 lines
    dbSQLQuery.SQL.Text := 'SET CHARACTER SET "utf8"'; dbSQLQuery.ExecSQL;
    dbSQLQuery.SQL.Text := 'SET NAMES "utf8"'; dbSQLQuery.ExecSQL;
  except
    on E: ESQLDatabaseError do begin
      edMess.Color := clRed;
      edMess.Text := 'Connection to MySQL database "world" FAILED!';
    end;
  end;
end;

{ Button "Query": Read the cities from the "city" table }

procedure TfMySQL3.btQueryClick(Sender: TObject);

var
  Population, Count: Integer;
  Query, CountryCode, CountryName: string;

begin
  if dbMySQLConnection.Connected then begin
    if edCountry.Text <> '' then begin
      sgCities.Clean(0, 1, 2, 40, []);
      // Create query, depending on country code or name entered
      Query := 'SELECT country.Name AS CountryName, city.Name AS CityName, District, city.Population AS CityPopulation FROM country, city ';
      Query += 'WHERE Code = CountryCode ';
      if Length(edCountry.Text) = 3 then begin
        // Considering 3 letters as country code
        CountryCode := edCountry.Text;
        Query += 'AND CountryCode = "' + CountryCode + '" ';
      end
      else begin
        // Consider other entries as country name
        CountryName := edCountry.Text;
        Query += 'AND country.Name = "' + CountryName + '" ';
      end;
      Query += 'AND city.Population > 1000000 ';
      Query += 'ORDER BY city.Population DESC';
      // Query the database
      dbSQLQuery.SQL.Text := Query; Count := 0;
      try
        dbSQLQuery.Open;                                                       // start the query
        dbSQLQuery.First;                                                      // read first record of query result set
        while not dbSQLQuery.EOF do begin                                      // while not all records done, execute this loop
          Inc(Count);
          CountryName := dbSQLQuery.FieldByName('CountryName').AsString;
          sgCities.Cells[0, Count] := dbSQLQuery.FieldByName('CityName').AsString;
          sgCities.Cells[1, Count] := dbSQLQuery.FieldByName('District').AsString;
          Population := dbSQLQuery.FieldByName('CityPopulation').AsInteger;
          sgCities.Cells[2, Count] := FloatToStrF(Population, ffNumber, 0, 0);
          if Population < 10000000 then
            sgCities.Cells[2, Count] := ' ' + sgCities.Cells[2, Count];
          dbSQLQuery.Next;                                                     // read next record of query result set
        end;                                                                   // end the query
        dbSQLQuery.Close;
        laCountry.Caption := 'Cities';
        if CountryName <> '' then
          laCountry.Caption := laCountry.Caption + ' in ' + CountryName;
        laCountry.Caption := laCountry.Caption + ' with more than 1 million inhabitants:';
        // Display count of cities
        if Count = 0 then
          edMess.Text := 'No cities found in database!'
        else
          edMess.Text := 'Number of cities in database = ' + IntToStr(Count);
        edMess.Color := clDefault;
      except
        on E: ESQLDatabaseError do begin
          edMess.Text := E.Message;
          edMess.Color := clRed;
        end;
      end;
    end;
  end;
end;

{ Button "Exit": Close database connection and exit application }

procedure TfMySQL3.btExitClick(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                          // close database connection
    dbMySQLConnection.Close;
  Close;
end;

end.

