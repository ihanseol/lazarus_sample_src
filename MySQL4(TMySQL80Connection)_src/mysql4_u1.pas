{************************************}
{* Main unit for MySQL4 application *}
{************************************}

unit mysql4_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mysql57conn, mysql80conn, sqldb, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, db;

type
  {**********}
  { TfMySQL4 }
  {**********}
  TfMySQL4 = class(TForm)
    dbMySQLConnection: TMySQL80Connection;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label7, Label8, laPopulationNew: TLabel;
    rbSelect, rbInsert, rbDelete, rbUpdate: TRadioButton;
    edCountryCode, edCountryName, edCityCode, edCityName: TEdit;
    edDistrict, edPopulation, edPopulationNew: TEdit;
    edMess: TMemo;
    btQuery: TButton;
    btClear: TButton;
    btExit: TButton;
    dbSQLTransaction: TSQLTransaction;
    dbSQLQuery: TSQLQuery;
    procedure btClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btQueryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
    procedure rbDeleteChange(Sender: TObject);
    procedure rbInsertChange(Sender: TObject);
    procedure rbSelectChange(Sender: TObject);
    procedure rbUpdateChange(Sender: TObject);
  end;

var
  fMySQL4: TfMySQL4;

implementation

{$R *.lfm}

{ Count records (rows) in the "world.city" table }

procedure MySQLCount(Query: TSQLQuery; out Count: Integer; out MySQLError: string);

var
  Sql: string;

begin
  Sql := 'SELECT COUNT(*) AS CityCount FROM city';
  Query.SQL.Text := Sql; Count := 0;
  try
    Query.Open;                                                           // start the query
    Query.First;                                                          // read first (and only) record of query result set
    if not Query.EOF then begin
      Count := Query.FieldByName('CityCount').AsInteger;
    end;
    Query.Close;                                                          // end the query
  except
    on E: ESQLDatabaseError do
      MySQLError := E.Message;
  end;
end;

{ SQL SELECT: Read information for given city form "world.city" table }

procedure MySQLSelect(Query: TSQLQuery; var CountryCode, CountryName, CityID, CityName: string; out District: string; out Population: Integer; out Count: Integer; out MySQLError: string);

var
  Sql: string;

begin
  District := ''; Population := 0; MySQLError := '';
  // Create query for given country and city
  Sql := 'SELECT CountryCode, country.Name AS CountryName, ID AS CityID, city.name AS CityName, District, city.Population AS Population ';
  Sql += 'FROM country, city ';
  Sql += 'WHERE Code = CountryCode ';
  if CountryCode <> '' then
    Sql += 'AND CountryCode = "' + CountryCode + '" '
  else
    Sql += 'AND country.Name = "' + CountryName + '" ';
  if CityID <> '' then
    Sql += 'AND ID = ' + CityID + ' '
  else
    Sql += 'AND city.Name = "' + CityName + '"';
  // Query the database
  Query.SQL.Text := Sql;                                                 // the SELECT query text (standard SQL statement)
  Count := 0;
  try
    Query.Open;                                                           // start the query
    Query.First;                                                          // read first (and, here, only) record of query result set
    if not Query.EOF then begin
      Inc(Count);                                                         // count number of rows returned (will be 0 or 1; 0, if city not found)
      CountryCode := Query.FieldByName('CountryCode').AsString;
      CountryName := Query.FieldByName('CountryName').AsString;
      CityID := IntToStr(Query.FieldByName('CityID').AsInteger);
      CityName := Query.FieldByName('CityName').AsString;
      District := Query.FieldByName('District').AsString;
      Population := Query.FieldByName('Population').AsInteger;
    end;
    Query.Close;                                                          // end the query
  except
    on E: ESQLDatabaseError do
      MySQLError := E.Message;
  end;
end;

{ SQL INSERT: Add new city to the "world.city" database }

procedure MySQLInsert(Transaction: TSQLTransaction; Query: TSQLQuery; CountryCode, CityName, District: string; Population: Integer; out MySQLError: string);

var
  Sql: string;

begin
  Sql := 'INSERT INTO city (Name, CountryCode, District, Population) ';
  Sql += 'VALUES ("' + CityName + '", "' + CountryCode + '", "' + District + '", ' + IntToStr(Population) + ')';
  try
    Query.SQL.Text := Sql;                                                // the INSERT query text (standard SQL statement)
    Query.ExecSQL;                                                        // execute the INSERT query
    Transaction.Commit;                                                   // applay changes to the database
  except
    on E: ESQLDatabaseError do
      MySQLError := E.Message;
  end;
end;

{ SQL DELETE: Remove given city from the "world.city" database }

procedure MySQLDelete(Transaction: TSQLTransaction; Query: TSQLQuery; CountryCode, CityID, CityName: string; out MySQLError: string);

var
  Sql: string;

begin
  Sql := 'DELETE FROM city ';
  Sql += 'WHERE CountryCode = "' + CountryCode + '" ';
  if CityID <> '' then
    Sql += 'AND ID = ' + CityID + ' '
  else
    Sql += 'AND city.Name = "' + CityName + '"';
  try
    Query.SQL.Text := Sql;                                                // the DELETE query text (standard SQL statement)
    Query.ExecSQL;                                                        // execute the DELETE query
    Transaction.Commit;                                                   // applay changes to the database
  except
    on E: ESQLDatabaseError do
      MySQLError := E.Message;
  end;
end;

{ SQL UPDATE: Change population of given city in the "world.city" database }

procedure MySQLUpdate(Transaction: TSQLTransaction; Query: TSQLQuery; CountryCode, CityID, CityName: string; NewPopulation: Integer; out MySQLError: string);

var
  Sql: string;

begin
  Sql := 'UPDATE city ';
  Sql += 'SET Population = ' + IntToStr(NewPopulation) + ' ';
  Sql += 'WHERE CountryCode = "' + CountryCode + '" ';
  if CityID <> '' then
    Sql += 'AND ID = ' + CityID + ' '
  else
    Sql += 'AND city.Name = "' + CityName + '"';
  try
    Query.SQL.Text := Sql;                                                // the UPDATE query text (standard SQL statement)
    Query.ExecSQL;                                                        // execute the DELETE query
    Transaction.Commit;                                                   // applay changes to the database
  except
    on E: ESQLDatabaseError do
      MySQLError := E.Message;
  end;
end;

{**********}
{ TfMySQL4 }
{**********}

{ Application start: Connect to the database }

procedure TfMySQL4.FormCreate(Sender: TObject);

begin
  if dbMySQLConnection.Connected then                                     // close database connection (if should be open...)
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
  except
    on E: ESQLDatabaseError do begin
      edMess.Color := clRed;
      edMess.Text := 'Connection to MySQL database "world" FAILED!';
    end;
  end;
end;

{ Button "Query": Perform database task (select, insert, delete, update), depending on user selection (radiobutton checked) }

procedure TfMySQL4.btQueryClick(Sender: TObject);

var
  Population, Count, CountBefore, CountAfter: Integer;
  CountryCode, CountryName, CityCode, CityName, District, MySQLError, DataError: string;
  Ret: Cardinal;

begin
  if dbMySQLConnection.Connected then begin
    CountryCode := edCountryCode.Text; CountryName := edCountryName.Text;
    CityCode := edCityCode.Text; CityName := edCityName.Text;
    MySQLError := ''; DataError := '';
    // All queries need a country and a city entered by the user
    if (CountryCode = '') and (CountryName = '') then begin
      DataError := 'You have to enter a country';
      edCountryCode.SetFocus;
    end
    else if rbInsert.Checked and (CountryCode = '') then begin
      DataError := 'You have to enter a country code';
      edCountryCode.SetFocus;
    end
    else if (rbDelete.Checked or rbUpdate.Checked) and (CountryCode = '') and (CountryName <> '') then begin
      DataError := 'Database update by country name is not implemented in this application';
      edCountryCode.SetFocus;
    end
    else if (not rbInsert.Checked) and (CityCode = '') and (CityName = '') then begin
      DataError := 'You have to enter a city';
      edCityCode.SetFocus;
    end;
    // Proceed if country and city has been entered (as awaited by the application)
    if DataError = '' then begin
      // SQL Select: Read city info from database
      // ----------------------------------------
      if rbSelect.Checked then begin
        if (CountryCode <> '') and (CountryName <> '') then begin
          // The country may be given either by code or by name
          // Display warning (and use code), if user entered both the code and the name
          MessageDlg('Data issue', 'Country code and name are both filled in! Using code, ignoring name.', mtWarning, [mbOK], 0);
          CountryName := ''; edCountryName.Text := '';
        end;
        if (CityCode <> '') and (CityName <> '') then begin
          // The city may be given either by code or by name
          // Display warning (and use code), if user entered both the code and the name
          MessageDlg('Data issue', 'City ID and name are both filled in! Using ID, ignoring name.', mtWarning, [mbOK], 0);
          CityName := ''; edCityName.Text := '';
        end
        else if (CityCode = '') then begin
          // As (for tutorial simplicity reasons) the Select proccedure returns only one record, giving the city by name
          // will ressult in cities with duplicate names (for same country) will never be read! Display warning...
          MessageDlg('Data issue', 'Giving the city by name will result in some cities never shown! Use ID to resolve this issue.', mtWarning, [mbOK], 0);
        end;
        // Read the city info from database and display it
        MySQLSelect(dbSQLQuery, CountryCode, CountryName, CityCode, CityName, District, Population, Count, MySQLError);
        edCountryCode.Text := CountryCode; edCountryName.Text := CountryName;
        edCityCode.Text := CityCode; edCityName.Text := CityName;
        edDistrict.Text := District;
        // If the city has not been found set population field to blank (instaed of "0")
        if Count = 0 then
          edPopulation.Text := ''
        else
          edPopulation.Text := FloatToStrF(Population, ffNumber, 0, 0);
      end
      // Other database (SQL) actions
      else begin
        // Get record count of "world.city" table, before performing the database change requessted
        MySQLCount(dbSQLQuery, CountBefore, MySQLError);
        // SQL Insert: Add new city to database
        // ------------------------------------
        if rbInsert.Checked then begin
          District := edDistrict.Text;
          if edPopulation.Text = '' then
            Population := 0
          else
            Population := StrToInt(edPopulation.Text);
          if CityName = '' then begin
            // Do not allow entering a city without name
            DataError := 'You have to enter a name for the new city';
            edCityName.SetFocus;
          end
          else begin
            // Insertion will only concern the "world.city" table; thus, not possible to add/change the country names
            // Insertion of cities with a not existing country code is possible with the original implementation of the
            // "world" database. Adding a Foreign Key constraint on the "CountryCode" field in the "city" table (as I
            // described in my tutorial), would result in an error message of the MySQL server, if you try to do so!
            if CountryName <> '' then begin
              // Ignore country name
              MessageDlg('Data issue', 'You cannot add/change country names! The name entered will be ignored.', mtWarning, [mbOK], 0);
              CountryName := ''; edCountryName.Text := '';
            end;
            if CityCode <> '' then begin
              // Normally, all INSERT statements must have the value of the table's Primary Key set. With the "world.city" it's the contrary:
              // The PK field "ID" is an auto-increment field, thus the database server fills in the value of this field
              MessageDlg('Data issue', 'The database city ID field is an auto-increment field! The ID entered will be ignored.', mtWarning, [mbOK], 0);
              CityCode := ''; edCityCode.Text := '';
            end;
            if (District = '') or (edPopulation.Text = '') then
              // Allow user to let the district blank; id. for the population (set to 0 in this case); but, display warning
              MessageDlg('Data issue', 'Incomplete city information! Missing data will be set to null.', mtWarning, [mbOK], 0);
          end;
          // If there was no data (user) error, insert the new city into the database
          if DataError = '' then
            MySQLInsert(dbSQLTransaction, dbSQLQuery, CountryCode, CityName, District, Population, MySQLError);
        end
        else begin
          // SQL Delete or Update
          // --------------------
          if (CityCode = '') and (CityName = '') then begin
            // Either the code or the name of the city to be deleted/updated has to be entered
            DataError := 'You have to enter a city';
            edCityCode.SetFocus;
          end
          else if (CityCode = '') and (CityName <> '') then begin
            // Deleting/updating a city by name is dangerous! Ask user for confirmation!
            Ret := MessageDlg('Data confirmation', 'Executing this query may affect more than one country! Continue anyway?', mtWarning, [mbYes, mbNo], 0, mbNo);
            if Ret = mrNo then begin
              DataError := 'Query canceled by user';
              edCityName.Text := ''; edCityCode.SetFocus;
            end;
          end;
          if DataError = '' then begin
            // If user entered both a country/city code and name, display warning; ignore the name
            if (CountryCode <> '') and (CountryName <> '') then begin
              MessageDlg('Data issue', 'Country code and name are both filled in! Using code, ignoring name.', mtWarning, [mbOK], 0);
              CountryName := ''; edCountryName.Text := '';
            end;
            if (CityCode <> '') and (CityName <> '') then begin
              MessageDlg('Data issue', 'City ID and name are both filled in! Using ID, ignoring name.', mtWarning, [mbOK], 0);
              CityName := ''; edCityName.Text := '';
            end;
            // SQL Delete: Remove city from the database
            // -----------------------------------------
            if rbDelete.Checked then begin
              MySQLDelete(dbSQLTransaction, dbSQLQuery, CountryCode, CityCode, CityName, MySQLError);
            end
            // SQL Update: Change city population in the database
            // --------------------------------------------------
            else begin
              if edPopulationNew.Text = '' then begin
                // User must have entered a value for the new population
                DataError := 'You have to enter the new city population';
                edPopulationNew.SetFocus;
              end
              else if StrToInt(edPopulationNew.Text) < 0 then begin
                DataError := 'The new city population must be a positive value';
                edPopulationNew.SetFocus;
              end
              else begin
                // Update the population
                MySQLUpdate(dbSQLTransaction, dbSQLQuery, CountryCode, CityCode, CityName, StrToInt(edPopulationNew.Text), MySQLError);
              end;
            end;
          end;
        end;
      end;
    end;
    edMess.Text := ''; edMess.Color := clDefault;
    // Check if database query was successful or if it failed
    if MySQLError = '' then begin
      // No error condition returned by the MySQL server...
      if DataError <> '' then begin
        // ...but some data (user entry) error
        if DataError = 'Query canceled by user' then
          edMess.Color := clDefault
        else
          edMess.Color := clRed;
        edMess.Text  := DataError + '!';
      end
      else begin
        // No error, thus the query was executed. Now, must check if it requested action was successfully done
        if rbSelect.Checked then begin
          // If the SELECT query didn't return any row, it's that the city entered does not exist in the database
          if Count = 0 then begin
            edMess.Color := clYellow;
            edMess.Text := 'City not found in "world" database!';
          end;
        end
        else begin
          // Other queries: Get actual number of records in the "world.city" table and compare with the one before the database
          // operation was done (the simplest way to see, if the query was successful?)
          MySQLCount(dbSQLQuery, CountAfter, MySQLError);
          // INSERT query: Actual number of rows must be greater than before the insertion
          if rbInsert.Checked then begin
            if CountAfter > CountBefore then begin
              edMess.Color := clLime;
              edMess.Text := 'New city successfully inserted!';
            end
            else begin
              // Normally, this will never be the case. Either the insertion was done, or there was something, resulting in a MySQL error...
              edMess.Color := clRed;
              edMess.Text := 'Unknown database error! Insertion of new city failed!';
            end;
          end
          // DELETE query: Actual number of rows must be less than before the insertion
          else if rbDelete.Checked then begin
            if CountAfter < CountBefore then begin
              edMess.Color := clLime;
              edMess.Text := 'City successfully deleted!';
            end
            else begin
              // If no deletion was done, must be because the city to be deleted was not found...
              edMess.Color := clYellow;
              edMess.Text := 'Deletion of city failed (because city did not exist in database)!';
            end;
          end
          // UPDATE query: Number of rows does not change. Perform a SELECT on the city, that should have been updated, to check operation success
          else begin
            MySQLSelect(dbSQLQuery, CountryCode, CountryName, CityCode, CityName, District, Population, Count, MySQLError);
            if Count = 0 then begin
              // If there isn't any database row returned, it's that this city does not exist. Thus, the city couldn't be found during the
              // update and the update was a failure
              edMess.Color := clYellow;
              edMess.Text := 'Update of city population failed (because city did not exist in database)!';
            end
            else if Population = StrToInt(edPopulationNew.Text) then begin
              // If the actual city population was set to the new value (the one entered by the user), the update succeeded
              edMess.Color := clLime;
              edMess.Text := 'City population successfully updated!';
              edPopulation.Text := FloatToStrF(Population, ffNumber, 0, 0);
            end
            else begin
              // Normally, this will never be the case. Either the update was done, or there was something, resulting in a MySQL error...
              edMess.Color := clRed;
              edMess.Text := 'Unknown database error! Update of city population failed!';
            end;
          end;
        end;
      end;
    end
    else begin
      // The database server returned an error condition; display the MySQL error message
      edMess.Color := clRed;
      edMess.Text  := MySQLError + '!';
    end;
  end;
end;

{ Button "Clear": Set all form fields to blank }

procedure TfMySQL4.btClearClick(Sender: TObject);

begin
  edCountryCode.Text := ''; edCountryName.Text := '';
  edCityCode.Text := ''; edCityName.Text := '';
  edDistrict.Text := ''; edPopulation.Text := ''; edPopulationNew.Text := '';
  edMess.Text := ''; edMess.Color := clDefault;
end;

{ Button "Exit": Close database connection and exit application }

procedure TfMySQL4.btExitClick(Sender: TObject);

begin
  if dbMySQLConnection.Connected then
    dbMySQLConnection.Close;                                              // close database connection
  Close;
end;

{ User database operation selections (radiobutton changes): Apply edit field attributes as needed }

procedure TfMySQL4.rbSelectChange(Sender: TObject);

begin
  if rbSelect.Checked then begin
    edDistrict.ReadOnly := True; edDistrict.TabStop := False;
    edPopulation.ReadOnly := True; edPopulation.TabStop := False;
    laPopulationNew.Visible := False; edPopulationNew.Visible := False;
  end;
end;

procedure TfMySQL4.rbInsertChange(Sender: TObject);

begin
  if rbInsert.Checked then begin
    // To add all info of a new city to be inserted, user needs access to "district" and "population" fields
    edDistrict.ReadOnly := False; edDistrict.TabStop := True;
    edPopulation.ReadOnly := False; edPopulation.TabStop := True;
    laPopulationNew.Visible := False; edPopulationNew.Visible := False;
  end;
end;

procedure TfMySQL4.rbDeleteChange(Sender: TObject);

begin
  if rbDelete.Checked then begin
    edDistrict.ReadOnly := True; edDistrict.TabStop := False;
    edPopulation.ReadOnly := True; edPopulation.TabStop := False;
    laPopulationNew.Visible := False; edPopulationNew.Visible := False;
  end;
end;

procedure TfMySQL4.rbUpdateChange(Sender: TObject);

begin
  if rbUpdate.Checked then begin
    edDistrict.ReadOnly := True; edDistrict.TabStop := False;
    edPopulation.ReadOnly := True; edPopulation.TabStop := False;
    // To change the city's population, user needs access to "new population" field
    laPopulationNew.Visible := True; edPopulationNew.Visible := True;
  end;
end;

end.

