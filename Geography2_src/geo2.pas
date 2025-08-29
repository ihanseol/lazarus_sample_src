{****************************************}
{* Main unit for Geography2 application *}
{****************************************}

unit geo2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8, help;

type
  TContinentData = record
    CCode, CName: string;
    CCountries, CRegions: array of string;
    CLimits: array[0..9] of Integer;
  end;
  TAllContinentData = array[0..6] of TContinentData;
  TMountain = record
    MName: string;
    MElevationM, MElevationFT: Integer;
    MRegion, MContinent: string;
    MCountries: array of string;
  end;
  TMountains = array of TMountain;
  {********}
  { TfGeo2 }
  {********}
  TfGeo2 = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizElevation, mQuizRegion, mQuizContinent, mQuizCountries, mQuizExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsList: TMenuItem;
    mOptionsListAF, moptionsListAS, moptionsListAS2, moptionsListAU, moptionsListEU, mOptionsListNA, mOptionsListSA, mOptionsListAll: TMenuItem;
    mOptionsRegion, mOptionsRegionManual, mOptionsRegionSelection, mOptionsFeet: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    MenuItem2: TMenuItem;
    stTitle: TStaticText;
    Label1, laUElevation, laEval, laElevation, laRegion, laContinent, laCountries: TLabel;
    edName, edRegion, edEval: TEdit;
    cobElevation, cobRegion, cobContinent: TComboBox;
    cbCountry1, cbCountry2, cbCountry3, cbCountry4, cbCountry5: TCheckBox;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizElevationClick(Sender: TObject);
    procedure mQuizRegionClick(Sender: TObject);
    procedure mQuizContinentClick(Sender: TObject);
    procedure mQuizCountriesClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsListAFClick(Sender: TObject);
    procedure moptionsListASClick(Sender: TObject);
    procedure moptionsListAS2Click(Sender: TObject);
    procedure moptionsListAUClick(Sender: TObject);
    procedure moptionsListEUClick(Sender: TObject);
    procedure mOptionsListNAClick(Sender: TObject);
    procedure mOptionsListSAClick(Sender: TObject);
    procedure mOptionsListAllClick(Sender: TObject);
    procedure mOptionsRegionManualClick(Sender: TObject);
    procedure mOptionsRegionSelectionClick(Sender: TObject);
    procedure mOptionsFeetClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    iQuiz, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sContinent, sElevationUnit, sAnswer: string;
    aAllMountains, aMountains: TMountains;
    aMountainsDone: array of Boolean;
    aAllContinentData: TAllContinentData;
    aWorldLimits: array[0..9] of Integer;
    laQuizQuestions: array[0..2] of TLabel;
    cobQuizQuestions: array[0..2] of TCombobox;
    cbCountries:  array[0..4] of TCheckbox;
  end;

const
  Quizes: array[0..3] of string = (
    'Mountain elevation', 'Mountain range/region', 'Mountain continent', 'Mountain countries'
  );
  ContinentCodes: array[0..6] of string = (
    'AF', 'A1', 'A2', 'AU', 'EU', 'NA', 'SA'
  );
  ContinentNames: array[0..6] of string = (
    'Africa', 'Asia', 'Asia', 'Australia', 'Europe', 'North America', 'South America'
  );

var
  fGeo2: TfGeo2;

implementation

{$R *.lfm}

{ Format values for the grid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN += S;                                                                   // this is for the "%" sign
  Result := SN;
end;

{ Transform a string to "English letters only" }

function GetEnglishChars(S0: string): string;

const
  ForeignChars: array[1..21] of string = (
    'á', 'à', 'â', 'ä', 'ã', 'é', 'è', 'ê', 'ë', 'í', 'î', 'ï', 'ó', 'ô', 'ö', 'ú', 'ù', 'û', 'ü', 'ç', 'ñ'
  );
  EnglishChars: array[1..21] of string = (
    'a', 'a', 'a', 'a', 'a', 'e', 'e', 'e', 'e', 'i', 'i', 'i', 'o', 'o', 'o', 'u', 'u', 'u', 'u', 'c', 'n'
  );

var
  I, J: Integer;
  S, Char: string;

begin
  S := '';
  for I := 1 to UTF8Length(S0) do begin
    Char := UTF8Copy(S0, I, 1);
    for J := 1 to 20 do begin
      if UTF8Copy(S0, I, 1) = ForeignChars[J] then
        Char := EnglishChars[J]
      else if UTF8UpperString(UTF8Copy(S0, I, 1)) = UTF8UpperString(ForeignChars[J]) then
        Char := UTF8UpperString(EnglishChars[J]);
    end;
    S += Char;
  end;
  Result := S;
end;

{ Read mountains data from text file }

procedure ReadMountains(out Mountains: TMountains);

var
  N, C, P: Integer;
  Filename, Line, Mountain, Countries: string;
  InFile: Text;

begin
  SetLength(Mountains, 0); N := 0;
  Filename := './data/mountains.txt'; DoDirSeparators(Filename);
  Assign(InFile, Filename); Reset(InFile);
  while not EoF(Infile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Mountain := UTF8Trim(UTF8Copy(Line, 1, 30));
      if (UTF8Copy(Mountain, UTF8Length(Mountain), 1) <> '*') and (UTF8Copy(Mountain, UTF8Length(Mountain), 1) <> '?') then begin
        Inc(N);
        SetLength(Mountains, N);
        // Fill the TMountains array with the data read
        with Mountains[N - 1] do begin
          MName := Mountain;                                                   // mountain name
          MElevationM := StrToInt(UTF8Copy(Line, 31, 4));                      // mountain elevation in m
          if UTF8Copy(Line, 41, 5) = '     ' then
            MElevationFT := Round(MElevationM * 3.28084)                       // mountain elevation in ft calculated...
          else
            MElevationFT := StrToInt(UTF8Copy(Line, 41, 5));                   // ...or taken from file, if given
          MRegion := UTF8Trim(UTF8Copy(Line, 51, 30));                         // mountain range/region
          MContinent := UTF8Copy(Line, 81, 2);                                 // mountain continent
          // Fill the TCountries array (memeber of the actual TMountains record) with the countries read
          Countries := UTF8Trim(UTF8Copy(Line, 91, UTF8Length(Line)));
          SetLength(MCountries, 0); C := 0;
          repeat
            // Repeat for all countries for actual mountain
            Inc(C);
            SetLength(MCountries, C);
            P := UTF8Pos(', ', Countries);                                     // in the file, the different countries are separated by ", "
            if P > 0 then begin
              MCountries[C - 1] := UTF8Copy(Countries, 1, P - 1);
              UTF8Delete(Countries, 1, P + 1);
            end
            else begin
              MCountries[C - 1] := Countries;
              Countries := '';
            end;
          until Countries = '';
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Read the continents data from the different text files }

procedure ReadContinentData(out AllContinentData: TAllContinentData; out WorldLimits: array of Integer);

var
  C, N1, N2, N3, I, P: Integer;
  Filename1, Filename2, Filename3, Limits, Line: string;
  InFile1, InFile2, InFile3: Text;

begin
  // Fill in data for each of the 6 continents
  for I := 0 to 6 do begin
    AllContinentData[I].CCode := ContinentCodes[I]; AllContinentData[I].CName := ContinentNames[I];
    SetLength(AllContinentData[I].CCountries, 0); SetLength(AllContinentData[I].CRegions , 0);
    Filename1 := LowerCase('./data/countries' + '_' + ContinentCodes[I] + '.txt'); DoDirSeparators(Filename1);
    Filename2 := LowerCase('./data/regions' + '_' + ContinentCodes[I] + '.txt'); DoDirSeparators(Filename1);
    Assign(InFile1, Filename1); Reset(InFile1); Assign(InFile2, Filename2); Reset(InFile2);
    N1 := 0; N2 := 0;
    // Read continent countries and fill them into the corr. array, member of the actual record of the TAllContinents array
    while not EoF(InFile1) do begin
      Readln(InFile1, Line);
      if Line <> '' then begin
        Inc(N1); SetLength(AllContinentData[I].CCountries, N1);
        AllContinentData[I].CCountries[N1 - 1] := UTF8Trim(Line);
      end;
    end;
    // Read continent regions and fill them into the corr. array, member of the actual record of the TAllContinents array
    while not EoF(InFile2) do begin
      Readln(InFile2, Line);
      if Line <> '' then begin
        Inc(N2); SetLength(AllContinentData[I].CRegions, N2);
        AllContinentData[I].CRegions[N2 - 1] := UTF8Trim(Line);
      end;
    end;
    Close(InFile1); Close(InFile2);
  end;
  // Read continent limits and fill them into the corr. array, member of the actual record of the TAllContinents array
  Filename3 := LowerCase('./data/limits.txt'); DoDirSeparators(Filename3);
  Assign(InFile3, Filename3); Reset(InFile3); C := 0;
  while not EoF(InFile3) do begin
    Readln(InFile3, Line); Line := UTF8Trim(Line);
    if Line <> '' then begin
      Inc(C);
      N3 := 0;
      repeat
        // Repeat for all limit values
        Inc(N3);
        P := UTF8Pos(', ', Line);                                              // in the file, the different limit values are separated by ", "
        if P > 0 then begin
          Limits := UTF8Copy(Line, 1, P - 1);
          UTF8Delete(Line, 1, P + 1);
        end
        else begin
          Limits := Line;
          Line := '';
        end;
        if C <= 6 then
          AllContinentData[C - 1].CLimits[N3 - 1] := StrToInt(Limits)
        else
          WorldLimits[N3 - 1] := StrToInt(Limits);
      until Line = '';
    end;
  end;
  Close(InFile3);
end;

{ Set quiz title }

procedure SetTitle(Quiz: Integer; Continent: string);

var
  I: Integer;

begin
  fGeo2.stTitle.Caption := 'Geography quiz: ';
  if Continent = '--' then
    fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + 'World Mountains'
  else begin
    for I := 0 to 6 do begin
      if Continent = ContinentCodes[I] then begin
        fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + ContinentNames[I];
        if Continent = 'EU' then
          fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + 'an Mountains'
        else
          fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + 'n Mountains';
        if Continent = 'A1' then
          fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + ' (I)'
        else if Continent = 'A2' then
          fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + ' (II)';
      end;
    end;
  end;
  fGeo2.stTitle.Caption := fGeo2.stTitle.Caption + ' - ' + Quizes[Quiz] + '.';
end;

{ Fill the mountains elevation combobox (for actual continent) }

procedure FillComboboxElevation(Continent, ElevationUnit: string; var AllContinentData: TAllContinentData;
  var Limits: array of Integer; var CElevations: TCombobox);

var
  I, IX: Integer;
  Elevation1, Elevation2, Mult: Real;
  S: string;

begin
  IX := -1;
  for I := 0 to 6 do begin
    if Continent = ContinentCodes[I] then
      IX := I;
  end;
  CElevations.Items.Clear;
  if ElevationUnit = 'm' then
    Mult := 1                                                                  // elevation values in m
  else
    Mult := 3.28084;                                                           // elevation values in ft
  // First list entry
  if IX = -1 then
    Elevation1 := Mult * Limits[1]                                             // world mountains list
  else
    Elevation1 := Mult * AllContinentData[IX].CLimits[1];                      // given country mountains list
  CElevations.Items.AddText('less than ' + FloatToStrF(Elevation1, ffNumber, 0, 0));
  // Following list entries
  for I := 1 to 8 do begin
    if IX = -1 then begin
      Elevation1 := Mult * Limits[I]; Elevation2 := Mult * Limits[I + 1];
    end
    else begin
      Elevation1 := Mult * AllContinentData[IX].CLimits[I]; Elevation2 := Mult * AllContinentData[IX].CLimits[I + 1];
    end;
    S := 'between ' + FloatToStrF(Elevation1 + 1, ffNumber, 0, 0) + ' and ' + FloatToStrF(Elevation2, ffNumber, 0, 0);
    CElevations.Items.AddText(S);
  end;
  // Last list entry
  if IX = -1 then
    Elevation2 := Mult * Limits[9]
  else
    Elevation2 := Mult * AllContinentData[IX].CLimits[9];
  CElevations.Items.AddText('more than ' + FloatToStrF(Elevation2, ffNumber, 0, 0));
  CElevations.ItemIndex := 0;
end;

{ Fill the mountains regions combobox (for actual continent) }

procedure FillComboboxRegions(Continent: string; AllContinentData: TAllContinentData; CRegions: TCombobox);

var
  I, IX: Integer;

begin
  IX := -1;
  for I := 0 to 6 do begin
    if Continent = ContinentCodes[I] then
      IX := I;
  end;
  CRegions.Items.Clear;
  if IX <> -1 then begin
    // For world mountains, the combobox will be filled later (refill for each question)
    for I := 0 to Length(AllContinentData[IX].CRegions) - 1 do
      CRegions.Items.AddText(AllContinentData[IX].CRegions[I]);
    end;
  CRegions.ItemIndex := 0;
end;

{ Select the mountain countries and set the corr. checkbox labels }

procedure FillCountries(Continent: string; Countries: array of string; AllContinentData: TAllContinentData; CBCountries: array of TCheckbox );

var
  CX, IX, R, I, J: Integer;
  Country: string;
  OK: Boolean;

begin
  // Determine continent index for TAllContinentData array
  for I := 0 to 6 do begin
    if Continent = ContinentCodes[I] then
      CX := I;
  end;
  // Clear all checkbox labels
  for I := 0 to 4 do begin
    CBCountries[I].Caption := '';
  end;
  // For all 5 checkboxes, get a country and set it as its label
  for IX := 0 to 4 do begin
    // Get a country (depending on the mountain's continent)
    repeat
      OK := True;
      if (Continent = 'AU') or (Continent = 'NA') then begin
        // Special case for Australia and North America (only 1 resp. 3 countries available):
        // Get countries from: 1. correct answers, 2. continent countries,
        //                     3. North America for Australia resp. Australia for North America
        //                     4. South America
        R := Random(8);
        case R of
          0..2: Country := Countries[Random(Length(Countries))];
          3..5: Country := AllContinentData[CX].CCountries[Random(Length(AllContinentData[CX].CCountries))];
          6..7: begin
                  if Continent = 'AU' then
                    Country := AllContinentData[4].CCountries[Random(Length(AllContinentData[4].CCountries))]
                  else
                    Country := AllContinentData[2].CCountries[Random(Length(AllContinentData[2].CCountries))];
                end;
             8: Country := AllContinentData[5].CCountries[Random(Length(AllContinentData[5].CCountries))];
        end;
      end
      else begin
        // Other continents ("normal" case):
        // Get countries from: 1. correct answers, 2. continent countries
        R := Random(2);
        case R of
          0: Country := Countries[Random(Length(Countries))];
          1: Country := AllContinentData[CX].CCountries[Random(Length(AllContinentData[CX].CCountries))];
        end;
      end;
      // Countries have to be unique
      for J := 0 to IX - 1 do begin
        if Country = UTF8Trim(CBCountries[J].Caption) then
          OK := False;
      end;
    until OK;
    // Set the country as checkbox label
    CBCountries[IX].Caption := ' ' + Country;
  end;
  // Sort the checkbox labels (order of the country checkboxes on the form)
  for I := 1 to 4 do begin
    for J := 0 to I - 1 do begin
      if CBCountries[J].Caption > CBCountries[I].Caption then begin
        Country := CBCountries[I].Caption; CBCountries[I].Caption := CBCountries[J].Caption; CBCountries[J].Caption := Country;
      end;
    end;
  end;
end;

{ Prepare for a new quiz }

procedure NewQuiz(Quiz, QuestionsTemp: Integer; Continent: string; var AllMountains: TMountains; var AllContinentData: TAllContinentData;
  WorldLimits: array of Integer; ElevationUnit: string; LQuestions: array of TLabel; CQuestions: array of TCombobox;
  ERegion: TEdit; LCountry: TLabel; CCountries: array of TCheckbox; out Mountains: TMountains; out Questions, Question, Correct: Integer);

var
  N, I: Integer;
  DoInsert, Visibility: Boolean;

begin
  // Set quiz title
  SetTitle(Quiz, Continent);
  // Create the mountains list for actual continent
  N := 0; SetLength(Mountains, 0);
  for I := 0 to Length(AllMountains) - 1 do begin
    DoInsert := False;
    if (Continent = '--') then begin
      // World mountains: As some mountains are part of both Asian lists, eliminate doubles here
      // (mountains not to be included in world list, are tagged with "#")
      if RightStr(AllMountains[I].MName, 1) <> '#' then
        DoInsert := True;
    end
    else begin
      // Specific continent list: Include only mountains of this continent
      if AllMountains[I].MContinent = Continent then
        DoInsert := True;
    end;
    // Insert the mountain into the actual list
    if DoInsert then begin
      Inc(N); SetLength(Mountains, N);
      Mountains[N - 1] := AllMountains[I];
      if Continent = 'A1' then
        Mountains[N - 1].MName := StringReplace(Mountains[N - 1].MName, '#', '', []);  // remove tag (if any)
    end;
  end;
  // Display/hide form input fields as needed
  fGeo2.laUElevation.Visible := False;
  if Quiz = 3 then begin
    //Counties quiz: Display the countries checkboxes
    for I := 0 to 2 do begin
      LQuestions[I].Visible := False; CQuestions[I].Visible := False;
    end;
    LCountry.Visible := True;
    for I := 0 to 4 do begin
      CCountries[I].Visible := True;
      CCountries[I].Caption := 'Country ' + IntToStr(I + 1);
    end;
  end
  else begin
    // Other quizes: Display the (quiz dependent) combobox
    for I := 0 to 2 do begin
      if I = Quiz then
        Visibility := True
      else
        Visibility := False;
      LQuestions[I].Visible := Visibility;
      CQuestions[I].Visible := Visibility; CQuestions[I].ItemIndex := 0;
    end;
    if Quiz = 0 then
      fGeo2.laUElevation.Visible := True;
    LCountry.Visible := False;
    for I := 0 to 4 do
      CCountries[I].Visible := False;
  end;
  // For mountain regions quiz, display the edit field for manual entry (otherwise the combobox)
  ERegion.Visible := False;
  if Quiz = 1 then begin
    if fGeo2.mOptionsRegionManual.Checked then begin
      ERegion.Visible := True; ERegion.Text := ''; CQuestions[1].Visible := False;
    end
    else
      CQuestions[1].Items.Clear;
  end;
  // Fill (dynamic) comboboxes, depending on quiz and continent
  if Quiz = 0 then begin
    FillComboboxElevation(Continent, ElevationUnit, AllContinentData, WorldLimits, CQuestions[0]);
  end
  else if Quiz = 1 then begin
    if fGeo2.mOptionsRegionSelection.Checked then
      FillComboboxRegions(Continent, AllContinentData, CQuestions[1]);
  end;
  // Reset variables and form controls
  Questions := QuestionsTemp; Question := 0; Correct := 0;
  fGeo2.edName.Text := '';
  fGeo2.edEval.Text := ''; fGeo2.edEval.Color := cldefault;
  fGeo2.laEval.Caption := 'Evaluation:';
  for I := 0 to 3 do
    fGeo2.sgEval.Cells[1, I] := '';
  fGeo2.mOptions.Enabled := True;                                              // be sure, that user has access to "Options" menu
  fGeo2.btQuestion.Caption := 'Start';
end;

{ Determine elevation interval for given elevation value }

function GetElevationInterval(Elevations: array of Integer; ElevationUnit: string; Elevation: Integer): string;

// The user elevation answer (combobox item) will be evaluated by comparing it to this elevation string

var
  I: Integer;
  Mult: Real;
  ElevationInterval: string;

begin
  ElevationInterval := '';
  if ElevationUnit = 'm' then
    Mult := 1
  else
    Mult := 3.28084;
  // In order to evaluate the user answer correctly, the elevation string must, of course, be exactly equal to one of the combobox entries
  if Elevation > Elevations[9] * Mult then
    ElevationInterval := 'more than ' + FloatToStrF(Elevations[9] * Mult, ffNumber, 0, 0)
  else begin
    for I := 0 to 8 do begin
      if (Elevation > (Elevations[I] * Mult)) and (Elevation <= (Elevations[I + 1] * Mult)) then begin
        if I = 0 then
          ElevationInterval := 'less than ' + FloatToStrF(Elevations[1] * Mult, ffNumber, 0, 0)
        else
          ElevationInterval := 'between ' + FloatToStrF(Elevations[I] * Mult + 1, ffNumber, 0, 0) + ' and ' + FloatToStrF(Elevations[I + 1] * Mult, ffNumber, 0, 0);
      end;
    end;
  end;
  Result := ElevationInterval;
end;

{ Get continent name (for givn continent code) }

function GetContinentName(CCode: string): string;

var
  I: Integer;
  CName: string;

begin
  CName := '';
  for I := 0 to 6 do begin
    if CCode = ContinentCodes[I] then
      CName := ContinentNames[I];
  end;
  Result := CName;
end;

{ Get (correct) answer for mountain countries quiz }

function GetCountryQuizAnswer(CBCountries: array of TCheckbox; Countries: array of string): string;

// The answer will be a 5-character string, each character corresponding to one of the 5
// countries given (one of the 5 checkboxes) and set to "1" if country is part of answer,
// or to "0", if not

var
  I, J: Integer;
  CountriesYN, YN: string;

begin
  CountriesYN := '';
  for I := 0 to 4 do begin
    YN := '0';
    for J := 0 to Length(Countries) - 1 do begin
      if UTF8Trim(CBCountries[I].Caption) = Countries[J] then
        // Checking the checkbox is a correct answer, if its label is a country listed for the actual mountain
        YN := '1';
    end;
    CountriesYN += YN;
  end;
  Result := CountriesYN;
end;

{ Get user answer for mountains countries quiz }

function GetCountryUserAnswer(CBCountries: array of TCheckbox): string;

// The answer will be a 5-character string, each character corresponding to one of the
// 5 country checkboxes and set to "1" if the checkbox is checked, or to "0", if not

var
  I: Integer;
  CountriesYN: string;

begin
  CountriesYN := '';
  for I := 0 to 4 do begin
    if CBCountries[I].Checked then
      CountriesYN += '1'
    else
      CountriesYN += '0';
  end;
  Result := CountriesYN;
end;

{********}
{ TfGeo2 }
{********}

{ Application start: Initialisation }

procedure TfGeo2.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create arrays with labels, comboboxes and checkboxes
  laQuizQuestions[0]  := laElevation;  laQuizQuestions[1]  := laRegion;  laQuizQuestions[2]  := laContinent;
  cobQuizQuestions[0] := cobElevation; cobQuizQuestions[1] := cobRegion; cobQuizQuestions[2] := cobContinent;
  cbCountries[0] := cbCountry1; cbCountries[1] := cbCountry2; cbCountries[2] := cbCountry3;
  cbCountries[3] := cbCountry4; cbCountries[4] := cbCountry5;
  // Re-arrange the controls on the form
  // In fact, the form presented to the user is completely different to the one initially created in Lazarus
  // I opted for this way to do, in order to make all controls visible and easily accessible to the programmer
  // and then, with the application start, moving them to the position, where they should appear in the GUI
  for I := 0 to 2 do begin
    laQuizQuestions[I].Top  := 68;
    cobQuizQuestions[I].Top := 64;
  end;
  edRegion.Top := 64;
  for I := 0 to 4 do begin
    cbCountries[I].Left := 10 + I * 175; cbCountries[I].Top := 152;
  end;
  edEval.Top := 190;
  btQuestion.Top := 232; btCancel.Top := 232;
  fGeo2.Height := 285;
  // Read quiz data from text files
  ReadMountains(aAllMountains);
  ReadContinentData(aAllContinentData, aWorldLimits);
  // Set application startup values
  sContinent := 'EU';
  iQuestionsTemp := 20;
  sElevationUnit := 'm';
  // Start random number generator
  Randomize;
  // Prepare for a "mountain elevation" quiz
  mQuizElevation.Click;
end;

{ Menu item "Quiz > Mountain elevation": Prepare for a "mountain elevation" quiz }

procedure TfGeo2.mQuizElevationClick(Sender: TObject);

begin
  iQuiz := 0;
  mOptionsList.Enabled := True;
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Mountain range/region": Prepare for a "mountain range/region" quiz }

procedure TfGeo2.mQuizRegionClick(Sender: TObject);

begin
  iQuiz := 1;
  mOptionsList.Enabled := True;
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Mountain continent": Prepare for a "mountain continent" quiz }

procedure TfGeo2.mQuizContinentClick(Sender: TObject);

begin
  iQuiz := 2;
  mOptionsListAll.Click; mOptionsList.Enabled := False;
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Mountain contries": Prepare for a "mountain countries" quiz }

procedure TfGeo2.mQuizCountriesClick(Sender: TObject);

begin
  iQuiz := 3;
  mOptionsList.Enabled := True;
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfGeo2.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User entry of number of quiz questions }

procedure TfGeo2.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Geography quiz', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestionsTemp := StrToInt(S);
    if iQuestionsTemp < 10 then                                                // arbitrarily fixed minimum
      iQuestionsTemp := 10;
  end;
end;

{ Menu items "Options > Quiz mountain list > ...": Select the continent for the mountains quiz questions list }

procedure TfGeo2.mOptionsListAFClick(Sender: TObject);

begin
  mOptionsListAF.Checked := True;   mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := False;
  sContinent := 'AF';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.moptionsListASClick(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := True ;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := False;
  sContinent := 'A1';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.moptionsListAS2Click(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := True;   mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := False;
  sContinent := 'A2';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.moptionsListAUClick(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := True;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := False;
  sContinent := 'AU';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.moptionsListEUClick(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := True;   mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := False;
  sContinent := 'EU';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.mOptionsListNAClick(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := True;   mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := False;
  sContinent := 'NA';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.mOptionsListSAClick(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := True;   mOptionsListAll.Checked := False;
  sContinent := 'SA';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

procedure TfGeo2.mOptionsListAllClick(Sender: TObject);

begin
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListAU.Checked  := False;
  mOptionsListEU.Checked := False;  mOptionsListNA.Checked := False;  mOptionsListSA.Checked  := False;  mOptionsListAll.Checked := True;
  sContinent := '--';
  NewQuiz(iQuiz, iQuestionsTemp, sContinent, aAllMountains, aAllContinentData, aWorldLimits, sElevationUnit, laQuizQuestions, cobQuizQuestions,
    edRegion, laCountries, cbCountries, aMountains, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Options > Region entry > Manual entry": Usage of an edit field, where the user has to manually enter the mountain range/region }

procedure TfGeo2.mOptionsRegionManualClick(Sender: TObject);

begin
  mOptionsRegionManual.Checked := True;
  mOptionsRegionSelection.Checked := False;
  if iQuiz = 1 then begin
    edRegion.Visible := True; edRegion.Text := ''; cobRegion.Visible := False;
  end;
end;

{ Menu item "Options > Region entry > Selection list": Usage of a combobox, where the user can choose the mountain range/region }

procedure TfGeo2.mOptionsRegionSelectionClick(Sender: TObject);

begin
  mOptionsRegionManual.Checked := False;
  mOptionsRegionSelection.Checked := True;
  if iQuiz = 1 then begin
    edRegion.Visible := False; cobRegion.Visible := True; cobRegion.Items.Clear;
  end;
end;

{Menu item "Options > Mountain elevation in feet": Toggle to use meters or feet as mountain elevation unit }

procedure TfGeo2.mOptionsFeetClick(Sender: TObject);

begin
  if mOptionsFeet.Checked then begin
    mOptionsFeet.Checked := False;
    sElevationUnit := 'm'; laUElevation.Caption := sElevationUnit;
  end
  else begin
    mOptionsFeet.Checked := True;
    sElevationUnit := 'ft'; laUElevation.Caption := sElevationUnit;
  end;
  if iQuiz = 0 then
    // Refill mountain elevation combobox
    FillComboboxElevation(sContinent, sElevationUnit, aAllContinentData, aWorldLimits, cobQuizQuestions[0]);
end;

{ Menu item "Help > Help": Display application help text }

procedure TfGeo2.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfGeo2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Geography quiz:' + LineEnding;
  S += 'Mountains of the world.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June 2020 - March 2021.';
  MessageDlg('About "Geography2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfGeo2.btQuestionClick(Sender: TObject);

var
  ListMax, Count, RX, I, IX: Integer;
  Answer, UAnswer, Countries, S: string;
  Elevations: array[0..9] of Integer;

begin
  // Button "Start/Question": Generate new question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Some stuff to at quiz start
    if btQuestion.Caption = 'Start' then begin
      SetLength(aMountainsDone, Length(aMountains));
      for I := 0 to Length(aMountainsDone) - 1 do
        aMountainsDone[I] := False;                                            // set all mountains in list to "not yet done"
      // Determine "real" number of mountains in list
      // (as some mountain ranges/regions are actually missing...)
      ListMax := Length(aMountains);
      if iQuiz = 1 then begin
        for I := 0 to Length(aMountains) - 1 do begin
          if aMountains[I].MRegion = '' then begin
            aMountainsDone[I] := True;
            Dec(ListMax);
          end;
        end;
      end;
      // Make sure number of questions to ask is not greater than (real) number of mountains
      if iQuestions > ListMax then begin
        S := 'Number of questions exceeds number of mountains in the list!' + LineEnding;
        S += 'Auto-reset to maximum available.';
        MessageDlg('Geography quiz', S, mtWarning, [mbOK], 0);
        iQuestions := ListMax;
      end;
      // Reset evaluation counters
      iQuestion := 0; iCorrect := 0;
      laEval.Caption := 'Evaluation:';
      for I := 0 to 3 do
        sgEval.Cells[1, I] := '';
      fGeo2.mOptions.Enabled := False;                                         // disable "Options" menu during quiz
    end;
    // Proceed, if there are still questions (mountains) left
    if iQuestion < iQuestions then begin
      Inc(iQuestion);
      laEval.Caption := 'Evaluation (' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + '):';
      edEval.Text := ''; edEval.Color := clDefault;
      edRegion.Text := '';
      for I := 0 to 4 do
        cbCountries[I].Checked := False;
      // Repeat getting a random mountain, until a "not done" one has been found
      repeat
        RX := Random(Length(aMountains));
      until not aMountainsDone[RX];
      // Display the mountain name (and adapt dynamic comboboxes / set checkbox labels, if necessary)
      aMountainsDone[RX] := True;                                              // mark mountain as "done"
      edName.Text := aMountains[RX].MName;
      if (iQuiz = 1) and mOptionsRegionSelection.Checked and mOptionsListAll.Checked then begin
        // World mountain range/region quiz: Fill the regions combobox with regions of actual mountain continent
        FillComboboxRegions(aMountains[RX].MContinent, aAllContinentData, cobQuizQuestions[1]);;
      end
      else if iQuiz = 3 then
        // Countries quiz: Choose countries and use them as checkbox labels, depending on continent of actual mountain
        FillCountries(aMountains[RX].MContinent, aMountains[RX].MCountries, aAllContinentData, cbCountries);
      // Determine answer of actual question (depending on kind of quiz)
      case iQuiz of
        0: begin
             if sContinent = '--' then
               Elevations := aWorldLimits
             else begin
               for I := 0 to 6 do begin
                 if sContinent = ContinentCodes[I] then
                   IX := I;
               end;
               Elevations := aAllContinentData[IX].CLimits;
             end;
             if mOptionsFeet.Checked then
               sAnswer := GetElevationInterval(Elevations, 'ft', aAllMountains[RX].MElevationFT)
             else
               sAnswer := GetElevationInterval(Elevations, 'm', aAllMountains[RX].MElevationM);
           end;
        1: sAnswer := aMountains[RX].MRegion;
        2: sAnswer := GetContinentName(aMountains[RX].MContinent);
        3: sAnswer := GetCountryQuizAnswer(cbCountries, aMountains[RX].MCountries);
      end;
      // Next push on button will be to check user answer
      btQuestion.Caption := 'Answer';
    end;
  end
  // Button "Answer": Check user answer
  else begin
    if iQuestion <= iQuestions then begin
      Answer := sAnswer;
      // Get user answer from form (depending on actual quiz)
      case iQuiz of
        0: UAnswer := cobElevation.Items[cobElevation.ItemIndex];
        1: if mOptionsRegionManual.Checked then begin
             // Get mountain range from edit field (and consider checking validity of answer with "English letters" names)
             Answer  := GetEnglishChars(sAnswer);
             UAnswer := GetEnglishChars(edRegion.Text);
           end
           else
             // Get answer from combobox
             UAnswer := cobRegion.Items[cobRegion.ItemIndex];
        2: UAnswer := cobContinent.Items[cobContinent.ItemIndex];
        3: UAnswer := GetCountryUserAnswer(cbCountries);
      end;
      // Check answer and display evaluation message
      if UAnswer = Answer then begin
        // Correct answer
        if Answer = sAnswer then begin
          // "Normal" case
          edEval.Text := 'This is correct!'; edEval.Color := clLime;
        end
        else begin
          // Misspelled mountain range name
          edEval.Text := 'Correctly spelled answer = ' + sAnswer; edEval.Color := clYellow;
        end;
        Inc(iCorrect);
      end
      else begin
        // False answer: Display the correct one
        edEval.Text := 'False! Correct answer = ';
        if iQuiz = 3 then begin
          // Transform the "0/1" answer string to a readable answer with country names
          Countries := ''; Count := 0;
          for I := 1 to 5 do begin
            if sAnswer[I] = '1' then begin
              if Countries <> ''  then
                Countries += ', ';
              Countries += cbCountries[I - 1].Caption;
              Inc(Count);
            end;
          end;
          if Count > 0 then
            edEval.Text := edEval.Text + Countries
          else
            edEval.Text := 'none...';
        end
        else
          edEval.Text := edEval.Text + sAnswer;
        edEval.Color := clRed;
      end;
      //  Update evaluation counters
      sgEval.Cells[1, 0] := GFormat(iQuestion, '');
      sgEval.Cells[1, 1] := GFormat(iCorrect, '');
      sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
      sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
      // If all mountains have been done, terminate the quiz
      if iQuestion = iQuestions then begin
        MessageDlg('Geography quiz', 'All questions have been done. Quiz over.', mtInformation, [mbOK], 0);
        btQuestion.Caption := 'Start';
        mOptions.Enabled := True;                                              // give user access to "Options" menu again
      end
      // If there are mountains left, continue the quiz
      else
        btQuestion.Caption := 'Question';                                      // next push on button will be to generate a new question
    end;
  end;
end;

{ Button "Cancel": User termination of actual quiz }

procedure TfGeo2.btCancelClick(Sender: TObject);

begin
  MessageDlg('Geography quiz', 'Quiz canceled by user.', mtInformation, [mbOK], 0);
  btQuestion.Caption := 'Start';
  mOptions.Enabled := True;                                                    // give user access to "Options" menu again
end;

end.

