{**********************************************}
{* Create data file for WorldQuiz application *}
{**********************************************}

program wqinit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  LazUTF8;

type
  TCountry = record
    Continent: Integer;
    CountryName, CapitalName: string[30];
    CountryArea, CountryPop, CapitalPop: Integer;
    IsDependentCountry: Boolean;
  end;

var
  N1, N2, N, NC, ND, CX, I: Integer;
  Line: string;
  Country: TCountry;
  Countries: array of TCountry;
  InFile1, InFile2: Text;
  OutFile: file of TCountry;

{ Get numeric value from population/area string }

function GetNumber(S: string): Integer;

begin
  S := UTF8Trim(S);                                                            // remove all spaces
  S := StringReplace(S, ',', '', [rfReplaceAll]);                              // remove thousands separators
  Result := StrToInt(S);
end;

{ Get country data from text file line and fill it into TCountry record }

procedure GetCountryData(Line: string; out Country: TCountry);

const
  Continents: array[0..5] of string = (
    'AF', 'AS', 'EU', 'NA', 'OC', 'SA'
  );

var
  CX, I: Integer;

begin
  for I := 0 to 5 do begin
    if LeftStr(Line, 2) = Continents[I] then
      CX := I;
  end;
  with Country do begin
    Continent := CX;
    CountryName := UTF8Trim(UTF8Copy(Line, 4, 30));
    CapitalName := '';
    CountryArea := GetNumber(UTF8Copy(Line, 54, 10));
    CountryPop := GetNumber(UTF8Copy(Line, 34, 13));
    CapitalPop := 0;
    IsDependentCountry := False;
  end;
end;

{ Get capital data from text file line and fill it into TCountry record }

procedure GetCapitalData(Line: string; var Country: TCountry);

begin
  with Country do begin
    CapitalName := UTF8Trim(UTF8Copy(Line, 31, 30));
    CapitalPop := GetNumber(UTF8Copy(Line, 61, 10));
    if (UTF8Length(Line) > 70) and (UTF8Copy(Line, 71, 1) = '?') then          // population marked "unreliable source?" on wiki webpage
      CapitalPop := -1;
    if UTF8Length(Line) > 72 then begin
      Line := UTF8Trim(UTF8Copy(Line, 72, UTF8Length(Line)));
      if Line <> '' then                                                       // for dependent territories, country is given at end of line
        IsDependentCountry := True;
    end;
  end;
end;

{****************}
{* Main program *}
{****************}

begin
  Writeln;
  Writeln('Create data file for WorldQuiz application'); Writeln;
  Assign(InFile1, 'countries.txt'); Reset(InFile1);
  Assign(InFile2, 'capitals.txt'); Reset(InFile2);
  Assign(OutFile, 'worldquiz.dat'); Rewrite(OutFile);
  // Read country data from countries.txt file
  N1 := 0;
  while not EoF(InFile1) do begin
    Readln(InFile1, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N1);
      GetCountryData(Line, Country);
      SetLength(Countries, N1);
      Countries[N1 - 1] := Country;                                            // fill country data in array of TCountry records
    end;
  end;
  // Read capitals data from capitals.txt file
  N2 := 0;
  while not EoF(InFile2) do begin
    Readln(InFile2, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N2); CX := -1;
      // Check if country exists in countries.txt file
      for I := 0 to Length(Countries) - 1 do begin
        if UTF8Trim(UTF8Copy(Line, 1, 30)) = Countries[I].CountryName then
          CX := I;
      end;
      if CX <> -1 then begin
        // Country is listed in countries.txt file
        GetCapitalData(Line, Countries[CX]);                                   // fill capitals data in array of TCountry records
      end
      else begin
        // Country is not listed in countries.txt file: just ignore it
        Writeln(UTF8Trim(UTF8Copy(Line, 1, 30)), ' not found in countries file - country ignored');
      end;
    end;
  end;
  // Countries with "unknown" capital (not listed in capitals.txt file) and other statistical counts
  N := 0; NC := 0; ND := 0;
  for I := 0 to Length(Countries) - 1 do begin
    Write(OutFile, Countries[I]);
    Inc(N);
    if Countries[I].CapitalName = '' then begin
      // Country is not listed in capitals.txt file: let it (without capitals data) in output file
      Writeln(Countries[I].CountryName, ' not found in capitals file - added without capital data');
      Inc(NC);
    end;
    if Countries[I].IsDependentCountry then
      Inc(ND);
  end;
  Close(InFile1); Close(InFile2); Close(OutFile);
  // Display statistics ccounts
  Writeln;
  Writeln('wqinit input lines:    countries in country file    = ', N1:3);
  Writeln('                       countries in capitals file   = ', N2:3);
  Writeln;
  Writeln('wqinit output records: total countries              = ', N:3);
  Writeln('                       countries with known capital = ', N - NC:3);
  Writeln('                       dependent countries          = ', ND:3);
  Writeln;
  Write('Hit ENTER to terminate the program... '); Readln;
end.

