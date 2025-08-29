{****************************************}
{* Main unit for Geography1 application *}
{****************************************}

unit geo1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8, help;

type
  TContinentData = record
    CCode, CName: string;
    CCountries, COutflows: array of string;
  end;
  TAllContinentData = array[0..5] of TContinentData;
  TArray1 = array[0..14] of Integer;
  TArray2 = array[0..1, 0..12] of Integer;
  TRiver = record
    RName: string;
    RLengthKM, RLengthMI, RDrainage, RDischarge: Integer;
    ROutflow, RContinent: string;
    RCountries: array of string;
  end;
  TRivers = array of TRiver;
  TLabels = array[1..10] of TLabel;
  TComboboxes = array[1..5] of TComboBox;
  TCheckboxes = array[1..5] of TCheckbox;
  {********}
  { TfGeo1 }
  {********}
  TfGeo1 = class(TForm)
    mMenu: TMainMenu;
    mQuiz,mQuizLongest, mQuizLength, mQuizDrainage, mQuizDischarge: TMenuItem;
    mQuizOutflow, mQuizContinent, mQuizCountries, mQuizExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsList: TMenuItem;
    mOptionsList25, mOptionsList50, mOptionsList100, mOptionsListFull: TMenuItem;
    mOptionsListAF, moptionsListAS, moptionsListAS2, moptionsListEU, mOptionsListNA, mOptionsListSA: TMenuItem;
    mOptionsOutflow, mOptionsOutFlowManual, mOptionsOutflowSelection, mOptionsMiles: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout, MenuItem1, MenuItem2: TMenuItem;
    stTitle: TStaticText;
    laLength, laDischarge, laDrainage, laOutflow, laContinent, laCountries: TLabel;
    Label1, laULength, laUDrainage, laUDischarge, laEval: TLabel;
    edName, edOutflow, edEval: TEdit;
    cobLength, cobDischarge, cobDrainage, cobOutflow, cobContinent: TComboBox;
    cbCountry1, cbCountry2, cbCountry3, cbCountry4, cbCountry5: TCheckBox;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizLengthClick(Sender: TObject);
    procedure mQuizDrainageClick(Sender: TObject);
    procedure mQuizDischargeClick(Sender: TObject);
    procedure mQuizOutflowClick(Sender: TObject);
    procedure mQuizContinentClick(Sender: TObject);
    procedure mQuizCountriesClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsList25Click(Sender: TObject);
    procedure mOptionsList50Click(Sender: TObject);
    procedure mOptionsList100Click(Sender: TObject);
    procedure mOptionsListFullClick(Sender: TObject);
    procedure mOptionsListAFClick(Sender: TObject);
    procedure moptionsListASClick(Sender: TObject);
    procedure moptionsListAS2Click(Sender: TObject);
    procedure moptionsListEUClick(Sender: TObject);
    procedure mOptionsListNAClick(Sender: TObject);
    procedure mOptionsListSAClick(Sender: TObject);
    procedure mOptionsOutFlowManualClick(Sender: TObject);
    procedure mOptionsOutflowSelectionClick(Sender: TObject);
    procedure mOptionsMilesClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuiz, iListMax, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sLengthUnit, sListContinent, sAnswer: string;
    aRivers: TRivers;
    aRiversDone: array of Boolean;
    aAllContinentData: TAllContinentData;
    laQuizQuestions: TLabels;
    cobQuizQuestions: TComboBoxes;
    cbCountries: TCheckboxes;
  end;

const
  SUP_2 = #$C2#$B2; SUP_3 = #$C2#$B3;
  Quizes: array[0..6] of string = (
    '', 'River length', 'Drainage area', 'Average discharge', 'River outflow', 'Continent', 'Drainage countries'
  );
  ContinentCodes: array[0..5] of string = (
    'AF', 'AS', 'AU', 'EU', 'NA', 'SA'
  );
  ContinentNames: array[0..5] of string = (
    'Africa', 'Asia', 'Australia', 'Europe', 'North America', 'South America'
  );
  LengthLimits: TArray2 = (
    (1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000, 6500, 8000),
    (600, 900, 1200, 1500, 1800, 2200, 2500, 2800, 3100, 3400, 3700, 4000, 5000)
  );
  DrainageLimits: TArray1 = (
    0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 2000000, 3000000, 5000000, 10000000
  );
  DischargeLimits: TArray1 = (
    0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 20000, 30000, 50000, 250000
  );

var
  fGeo1: TfGeo1;

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

{ Transform a string to "English characters" only }

function GetEnglishChars(S0: string): string;

const
  ForeignChars: array[1..21] of string = (
    'á', 'à', 'â', 'ä', 'ã', 'é', 'è', 'ê', 'ë', 'í', 'î', 'ï', 'ó', 'ô', 'ö', 'ú', 'ù', 'û', 'ü', 'ç', 'ñ'
  );
  EnglishChars: array[1..21] of string = (
    'a', 'a', 'a', 'ae', 'a', 'e', 'e', 'e', 'e', 'i', 'i', 'i', 'o', 'o', 'oe', 'u', 'u', 'u', 'ue', 'c', 'n'
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
        Char := EnglishChars[J];
    end;
    S += Char;
  end;
  Result := S;
end;

{ Read river data from text file }

procedure ReadRivers(out Rivers: TRivers);

var
  N, C, P: Integer;
  Filename, Line, Countries: string;
  InFile: Text;

begin
  SetLength(Rivers, 0); N := 0;
  Filename := './data/rivers.txt'; DoDirSeparators(Filename);
  Assign(InFile, Filename); Reset(InFile);
  while not EoF(Infile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N);
      SetLength(Rivers, N);
      // Fill the TRivers array with the data read
      with Rivers[N - 1] do begin
        RName := UTF8Trim(UTF8Copy(Line, 1, 20));
        RLengthKM := StrToInt(UTF8Copy(Line, 21, 4)); RLengthMI := StrToInt(UTF8Copy(Line, 31, 4));
        if UTF8Copy(Line, 41, 7) = '       ' then
          RDrainage := -1                                                      // -1 meaning, that no drainage data is available
        else
          RDrainage := StrToInt(UTF8Copy(Line, 41, 7));
        if  UTF8Copy(Line, 51, 6) = '      ' then
          RDischarge := -1                                                     // -1 meaning, that no discharge data is available
        else
          RDischarge := StrToInt(UTF8Copy(Line, 51, 6));
        ROutflow := UTF8Trim(UTF8Copy(Line, 61, 20));
        RContinent := UTF8Copy(Line, 81, 2);
        // Fill the TCountries array (memeber of the actual TRiver record) with the countries read
        Countries := UTF8Trim(UTF8Copy(Line, 91, UTF8Length(Line)));
        SetLength(RCountries, 0); C := 0;
        repeat
          // Repeat for all countries for actual river
          Inc(C);
          SetLength(RCountries, C);
          P := Pos(', ', Countries);                                           // in the text file, the different countries are separated by ", "
          if P > 0 then begin
            RCountries[C - 1] := Copy(Countries, 1, P - 1);                    // usage of Copy (not UTF8Copy) with Pos() !!
            Delete(Countries, 1, P + 1);
          end
          else begin
            RCountries[C - 1] := Countries;
            Countries := '';
          end;
        until Countries = '';
      end;
    end;
  end;
  Close(InFile);
end;

{ Read the continent data from the different text files }

procedure ReadContinentData(out AllContinentData: TAllContinentData);

var
  N1, N2, I: Integer;
  Filename1, Filename2, Line: string;
  InFile1, InFile2: Text;

begin
  for I := 0 to 5 do begin
    // Fill in data for each of the 6 continents
    AllContinentData[I].CCode := ContinentCodes[I]; AllContinentData[I].CName := ContinentNames[I];
    SetLength(AllContinentData[I].CCountries, 0); SetLength(AllContinentData[I].COutflows , 0);
    Filename1 := './data/countries' + '_' + ContinentCodes[I] + '.txt'; DoDirSeparators(Filename1);
    Filename2 := './data/outflow' + '_' + ContinentCodes[I] + '.txt'; DoDirSeparators(Filename1);
    Assign(InFile1, Filename1); Reset(InFile1); Assign(InFile2, Filename2); Reset(InFile2);
    N1 := 0; N2 := 0;
    // Read continent countries and fill them in the corr. array, member of the actual record of the TAllContinents array
    while not EoF(InFile1) do begin
      Readln(InFile1, Line);
      if Line <> '' then begin
        Inc(N1); SetLength(AllContinentData[I].CCountries, N1);
        AllContinentData[I].CCountries[N1 - 1] := UTF8Trim(Line);
      end;
    end;
    // Read continent outflows and fill them in the corr. array, member of the actual record of the TAllContinents array
    while not EoF(InFile2) do begin
      Readln(InFile2, Line);
      if Line <> '' then begin
        Inc(N2); SetLength(AllContinentData[I].COutflows, N2);
        AllContinentData[I].COutflows[N2 - 1] := UTF8Trim(Line);
      end;
    end;
    Close(InFile1); Close(InFile2);
  end;
end;

{ Set the quiz title }

procedure SetTitle(Quiz: Integer; Continent: string);

var
  I: Integer;

begin
  fGeo1.stTitle.Caption := 'Geography quiz: ';
  if Continent = '--' then
    fGeo1.stTitle.Caption := fGeo1.stTitle.Caption + 'World rivers'
  else begin
    for I := 0 to 5 do begin
      if Continent = ContinentCodes[I] then
        fGeo1.stTitle.Caption := fGeo1.stTitle.Caption + ContinentNames[I] + 'n rivers';
    end;
  end;
  fGeo1.stTitle.Caption := fGeo1.stTitle.Caption + ' - ' + Quizes[Quiz] + '.';
end;

{ Get the maximum of rivers available for actual selections }

function GetListMax(var Rivers: TRivers; Quiz: Integer; Continent: string): Integer;

var
  Max0, Max, I, N: Integer;

begin
  Max0 := 0;
  // "Normal" number of rivers, depending on the rivers list selected
  if fGeo1.mOptionsList25.Checked or fGeo1.moptionsListAS.Checked or fGeo1.mOptionsListNA.Checked or fGeo1.mOptionsListSA.Checked then
    Max0 := 25
  else if fGeo1.mOptionsList50.Checked or fGeo1.moptionsListAS2.Checked then
    Max0 := 50
  else if fGeo1.mOptionsList100.Checked then
    Max0 := 100
  else if fGeo1.mOptionsListFull.Checked then
    Max0 := Length(Rivers)
  else if fGeo1.mOptionsListAF.Checked then
    Max0 := 23
  else if fGeo1.mOptionsListEU.Checked then
    Max0 := 22;
  Max := Max0; N := 0;
  // Special case for drainage and discharge quizes (can't ask for value, that isn't given):
  // Decrement the "normal" value for each not given drainage/discharge
  if (Quiz = 2) or (Quiz = 3) then begin
    for I := 0 to Length(Rivers) - 1 do begin
      if (Continent = '--') or (Rivers[I].RContinent = Continent) then begin
        // World rivers or rivers being on the actual selected continent
        Inc(N);
        if N <= Max0 then begin
          // Rivers have to be part of those "normally" considered
          if (Quiz = 2) and (Rivers[I].RDrainage = -1) then
            Dec(Max)
          else if (Quiz = 3) and (Rivers[I].RDischarge = -1) then
            Dec(Max);
        end;
      end;
    end;
  end;
  Result := Max;
end;

{ Prepare for a new quiz }

procedure NewQuiz(Quiz, QuestionsTemp: Integer; var Rivers: TRivers; Continent: string; Labels: TLabels; Comboboxes: TComboboxes;
  Outflow: TEdit; LCountry: TLabel; Countries: TCheckboxes; out ListMax, Questions, Question, Correct: Integer);

var
  I: Integer;
  Visibility: Boolean;

begin
  SetTitle(Quiz, Continent);
  Outflow.Visible := False;
  // Counties quiz: Display the countries checkboxes
  if Quiz = 6 then begin
    for I := 1 to 5 do begin
      Labels[I].Visible := False; Comboboxes[I].Visible := False;
      if Labels[5 + I] <> nil then
        Labels[5 + I].Visible := False;
    end;
    LCountry.Visible := True;
    for I := 1 to 5 do begin
      Countries[I].Visible := True;
      Countries[I].Caption := 'Country ' + IntToStr(I);
    end;
  end
  // Other quizes: Display the (quiz dependent) comboboxx
  else begin
    for I := 1 to 5 do begin
      if I = Quiz then
        Visibility := True
      else
        Visibility := False;
      Labels[I].Visible := Visibility;
      Comboboxes[I].Visible := Visibility;
      Comboboxes[I].ItemIndex := 0;
      if Labels[5 + I] <> nil then
        Labels[5 + I].Visible := Visibility;
    end;
    fGeo1.laCountries.Visible := False;
    for I := 1 to 5 do
      Countries[I].Visible := False;
  end;
  // For outflow quiz, display the edit field for manual entry (otherwise the combobox)
  if Quiz = 4 then begin
    if fGeo1.mOptionsOutFlowManual.Checked then begin
      Outflow.Visible := True; Outflow.Text := ''; Comboboxes[4].Visible := False;
    end
    else
      Comboboxes[4].Items.Clear;
  end;
  ListMax := GetListMax(Rivers, Quiz, Continent);                              // get correct number of actual rivers
  Questions := QuestionsTemp; Question := 0; Correct := 0;
  fGeo1.edName.Text := '';
  fGeo1.edEval.Text := ''; fGeo1.edEval.Color := cldefault;
  fGeo1.laEval.Caption := 'Evaluation:';
  for I := 0 to 3 do
    fGeo1.sgEval.Cells[1, I] := '';
  fGeo1.mOptions.Enabled := True;                                              // be sure, that user has access to "Options" menu
  fGeo1.btQuestion.Caption := 'Start';
end;

{ Fill the river lengths combobox }

procedure ComboboxFillLength(var LLimits: TArray2; CBLength: TCombobox; LengthUnit: string);

var
  I, J: Integer;
  S: string;

begin
  CBLength.Items.Clear;
  if LengthUnit = 'km' then
    J := 0
  else
    J := 1;
  for I := 0 to 10 do begin
    S := 'between ' + FloatToStrF(LLimits[J, I], ffNumber, 0, 0) + ' and ' + FloatToStrF(LLimits[J, I + 1], ffNumber, 0, 0);
    CBLength.Items.AddText(S);
  end;
  CBLength.Items.AddText('over ' + FloatToStrF(LLimits[J, I + 1], ffNumber, 0, 0));
  CBLength.ItemIndex := 0;
end;

{ Fill the river drainages resp. river discharges combobox }

procedure ComboboxFillDrDi(var Limits: TArray1; CBox: TCombobox);

var
  I: Integer;
  S: string;

begin
  CBox.Items.Clear;
  CBox.Items.AddText('less than ' + FloatToStrF(Limits[1], ffNumber, 0, 0));
  for I := 1 to 12 do begin
    S := 'between ' + FloatToStrF(Limits[I], ffNumber, 0, 0) + ' and ' + FloatToStrF(Limits[I + 1], ffNumber, 0, 0);
    CBox.Items.AddText(S);
  end;
  CBox.Items.AddText('over ' + FloatToStrF(Limits[I + 1], ffNumber, 0, 0));
  CBox.ItemIndex := 0;
end;

{ Fill the river outflows combobox }

procedure ComboboxFillOutflow(var AllContinentData: TAllContinentData; CBOutflow: TCombobox; Continent, River, Outflow: string);

// This combobox is refilled for each quiz question
// Beside the correct outflow, 9 random outflows on the same continent will be added
// (in fact, randomly selected among those listed in the corr. continent text file)

var
  CX, I, J: Integer;
  Oflow: string;
  OK: Boolean;

begin
  for I := 0 to 5 do begin
    if Continent = ContinentCodes[I] then
      CX := I;
  end;
  CBOutflow.Items.Clear;
  // Add the correct outflow
  CBOutflow.Items.AddText(Outflow);
  // Do for each of the 9 combobox entries left
  for I := 1 to 9 do begin
    // Get a valid random outflow
    repeat
      OK := True;
      Oflow := AllContinentData[CX].COutflows[Random(Length(AllContinentData[CX].COutflows))];
      if Oflow = River then
        // The outflow can't be the river itself
        OK := False
      else begin
        // The outflow has to be unique in the combobox list
        for J := 0 to CBOutflow.Items.Count - 1 do begin
          if Oflow = CBOutflow.Items[J] then
            OK := False;
        end;
      end;
    until OK;
    // Add the (random) outflow to the combobox
    CBOutflow.Items.AddText(Oflow);
  end;
  // Sort the combobox items
  for I := 1 to CBOutflow.Items.Count - 1 do begin
    for J := 0 to I - 1 do begin
      if CBOutflow.Items[J] > CBOutflow.Items[I] then begin
        Oflow := CBOutflow.Items[I]; CBOutflow.Items[I] := CBOutflow.Items[J]; CBOutflow.Items[J] := Oflow;
      end;
    end;
  end;
  CBOutflow.ItemIndex := 0;
end;

{ Select the drainage countries and set the corr. checkbox labels }

procedure FillCountries(var AllContinentData: TAllContinentData; CBCountries: TCheckboxes; Continent: string; var Countries: array of string);

var
  CX, IX, R, I, J: Integer;
  Country: string;
  OK: Boolean;

begin
  // Determine continent index for TAllContinentData array
  for I := 0 to 5 do begin
    if Continent = ContinentCodes[I] then
      CX := I;
  end;
  // Clear all checkbox labels
  for I := 1 to 5 do begin
    CBCountries[I].Caption := '';
  end;
  // Get a country and set it as label for one of the checkboxes (until all of them have a label)
  for I := 1 to 5 do begin
    // Get a random checkbox, not yet having a label
    // (in fact this could be omitted, as the labels will be sorted later...)
    repeat
      IX := Random(5) + 1;
    until CBCountries[IX].Caption = '';
    // Get a country (depending on the river's continent)
    repeat
      OK := True;
      // Special case for Australia and North America (only 1 resp. 3 countries available):
      // Get countries from: 1. correct answers, 2. continent countries,
      //                     3. North America for Australia resp. Australia for North America
      //                     4. South America
      if (Continent = 'AU') or (Continent = 'NA') then begin
        R := Random(8);
        case R of
          0..2: Country := Countries[Random(Length(Countries))];
          3..5:Country := AllContinentData[CX].CCountries[Random(Length(AllContinentData[CX].CCountries))];
          6..7: begin
                  if Continent = 'AU' then
                    Country := AllContinentData[4].CCountries[Random(Length(AllContinentData[4].CCountries))]
                  else
                    Country := AllContinentData[2].CCountries[Random(Length(AllContinentData[2].CCountries))];
                end;
             8: Country := AllContinentData[5].CCountries[Random(Length(AllContinentData[5].CCountries))];
        end;
      end
      // Other continents ("normal" case):
      // Get countries from: 1. correct answers, 2. continent countries,
      else begin
        R := Random(2);
        case R of
          0: Country := Countries[Random(Length(Countries))];
          1: Country := AllContinentData[CX].CCountries[Random(Length(AllContinentData[CX].CCountries))];
        end;
      end;
      // Countries have to be unique
      for J := 1 to 5 do begin
        if Country = UTF8Trim(CBCountries[J].Caption) then
          OK := False;
      end;
    until OK;
    // Set the country as checkbox label
    CBCountries[IX].Caption := ' ' + Country;
  end;
  // Sort the checkbox labels
  for I := 2 to 5 do begin
    for J := 1 to I - 1 do begin
      if CBCountries[J].Caption > CBCountries[I].Caption then begin
        Country := CBCountries[I].Caption; CBCountries[I].Caption := CBCountries[J].Caption; CBCountries[J].Caption := Country;
      end;
    end;
  end;
end;

{ Check if a country is part of the actual list selection }

function IsNotInList(River: Integer; Continent, ListContinent: string): Boolean;

var
  IsNot: Boolean;

begin
  IsNot := False;
  if not fGeo1.mOptionsListFull.Checked then begin
    // For world rivers, river has to be one of the 25, 50 or 100 longest
    if fGeo1.mOptionsList25.Checked and (River >= 25) then
      IsNot := True
    else if fGeo1.mOptionsList50.Checked and (River >= 50) then
      IsNot := True
    else if fGeo1.mOptionsList100.Checked and (River >= 100) then
      IsNot := True
    // For continent rivers, it must be one of the N longest on this continent
    // Note: The TRiver array index compare values only work correctly with the river.txt file as provided
    //       THus: DO NOT CHANGE IT!
    else if ListContinent <> '--' then begin
      if Continent <> ListContinent then
        IsNot := True
      else begin
        if fGeo1.mOptionsListAS.Checked and (River > 57) then
          IsNot := True
        else if fGeo1.mOptionsListAS2.Checked and (River >= 137) then
          IsNot := True
        else if fGeo1.mOptionsListNA.Checked and (River > 155) then
          IsNot := True
        else if fGeo1.mOptionsListSA.Checked and (River > 109) then
          IsNot := True;
      end;
    end;
  end;
  Result := IsNot;
end;

{ Get the river length intervals (for the combobox) }

function GetLengthInterval(M, Len: Integer): string;

var
  I: Integer;
  LengthInterval: string;

begin
  LengthInterval := '';
  for I := 0 to 11 do begin
    if (Len >= (LengthLimits[M, I])) and (Len < (LengthLimits[M, I + 1])) then begin
      if I = 11 then
        LengthInterval := 'over ' + FloatToStrF(LengthLimits[M, 11], ffNumber, 0, 0)
      else
        LengthInterval := 'between ' + FloatToStrF(LengthLimits[M, I], ffNumber, 0, 0) + ' and ' + FloatToStrF(LengthLimits[M, I + 1], ffNumber, 0, 0);
    end;
  end;
  Result := LengthInterval;
end;

{ Get the river drainage intervals (for the combobox) }

function GetDrainageInterval(Drainage: Integer): string;

var
  I: Integer;
  DrainageInterval: string;

begin
  DrainageInterval := '';
  for I := 0 to 13 do begin
    if Drainage < DrainageLimits[0] then
      DrainageInterval := 'less than ' + FloatToStrF(DrainageLimits[0], ffNumber, 0, 0)
    else if (Drainage >= (DrainageLimits[I])) and (Drainage < (DrainageLimits[I + 1])) then begin
      if I = 13 then
        DrainageInterval := 'over ' + FloatToStrF(DrainageLimits[13], ffNumber, 0, 0)
      else
        DrainageInterval := 'between ' + FloatToStrF(DrainageLimits[I], ffNumber, 0, 0) + ' and ' + FloatToStrF(DrainageLimits[I + 1], ffNumber, 0, 0);
    end;
  end;
  Result := DrainageInterval;
end;

{ Get the river discharge intervals (for the combobox) }

function GetDischargeInterval(Discharge: Integer): string;

var
  I: Integer;
  DischargeInterval: string;

begin
  DischargeInterval := '';
  for I := 0 to 13 do begin
    if Discharge < DischargeLimits[0] then
      DischargeInterval := 'less than ' + FloatToStrF(DischargeLimits[0], ffNumber, 0, 0)
    else if (Discharge >= (DischargeLimits[I])) and (Discharge < (DischargeLimits[I + 1])) then begin
      if I = 13 then
        DischargeInterval := 'over ' + FloatToStrF(DischargeLimits[13], ffNumber, 0, 0)
      else
        DischargeInterval := 'between ' + FloatToStrF(DischargeLimits[I], ffNumber, 0, 0) + ' and ' + FloatToStrF(DischargeLimits[I + 1], ffNumber, 0, 0);
    end;
  end;
  Result := DischargeInterval;
end;

{ Get the continent names (for the combobox) }

function GetContinentName(CCode: string): string;

var
  I: Integer;
  CName: string;

begin
  CName := '';
  for I := 0 to 5 do begin
    if CCode = ContinentCodes[I] then
      CName := ContinentNames[I];
  end;
  Result := CName;
end;

{ Get (correct) answer for drainage countries quiz }

function GetCountryQuizAnswer(CBCountries: TCheckboxes; Countries: array of string): string;

// The answer will be a 5-character string, each character corresponding to one of the 5
// countries given (one of the 5 checkboxes) and set to "1" if country is part of answer,
// or to "0", if not

var
  I, J: Integer;
  CountriesYN, YN: string;

begin
  CountriesYN := '';
  for I := 1 to 5 do begin
    YN := '0';
    for J := 0 to Length(Countries) - 1 do begin
      if UTF8Trim(CBCountries[I].Caption) = Countries[J] then
        // Checking the checkbox is a correct answer, if its label is a country listed for the actual river
        YN := '1';
    end;
    CountriesYN += YN;
  end;
  Result := CountriesYN;
end;

{ Get user answer for drainage countries quiz }

function GetCountryUserAnswer(CBCountries: TCheckboxes): string;

// The answer will be a 5-character string, each character corresponding to one of the
// 5 country checkboxes and set to "1" if the checkbox is checked, or to "0", if not

var
  I: Integer;
  CountriesYN: string;

begin
  CountriesYN := '';
  for I := 1 to 5 do begin
    if CBCountries[I].Checked then
      CountriesYN += '1'
    else
      CountriesYN += '0';
  end;
  Result := CountriesYN;
end;

{********}
{ TfGeo1 }
{********}

{ Application start: Initialisation }

procedure TfGeo1.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create arrays with labels and comboboxes
  laUDrainage.Caption := StringReplace(laUDrainage.Caption, '2', SUP_2, []);
  laUDischarge.Caption := StringReplace(laUDischarge.Caption, '3', SUP_3, []);
  laQuizQuestions[1]  := laLength;      laQuizQuestions[6] := laULength;
  laQuizQuestions[2]  := laDrainage;    laQuizQuestions[7] := laUDrainage;
  laQuizQuestions[3]  := laDischarge;   laQuizQuestions[8] := laUDischarge;
  laQuizQuestions[4]  := laOutflow;     laQuizQuestions[9] := nil;
  laQuizQuestions[5]  := laContinent;   laQuizQuestions[10] := nil;
  cobQuizQuestions[1] := cobLength;     cobQuizQuestions[2] := cobDrainage;
  cobQuizQuestions[3] := cobDischarge;  cobQuizQuestions[4] := cobOutflow;
  cobQuizQuestions[5] := cobContinent;
  cbCountries[1] := cbCountry1;         cbCountries[2] := cbCountry2;
  cbCountries[3] := cbCountry3;         cbCountries[4] := cbCountry4;
  cbCountries[5] := cbCountry5;
  // Re-arrange the controls on the form
  // In fact, the form presented to the user is completely different to the one initially created in Lazarus
  // I opted for this way to do, in order to make all controls visible and easily accessible to the programmer
  // and then, with the application start, moving them to the position, where they should appear in the GUI
  for I := 1 to 5 do begin
    laQuizQuestions[I].Top  := 68;
    cobQuizQuestions[I].Top := 64;
    if laQuizQuestions[5 + I] <> nil then
      laQuizQuestions[5 + I].Top  := 68;
  end;
  edOutflow.Top := 64;
  for I := 1 to 5 do begin
    cbCountries[I].Left := 10 + (I - 1) * 175; cbCountries[I].Top := 152;
  end;
  edEval.Top := 191;
  btQuestion.Left := 741; btQuestion.Top := 191;
  fGeo1.Height := 245;
  // Read quiz data from text files
  ReadRivers(aRivers);
  SetLength(aRiversDone, Length(aRivers));
  ReadContinentData(aAllContinentData);
  // Set application startup values
  sListContinent := '--'; iListMax := 25;
  iQuestionsTemp := 20; sLengthUnit := 'km';
  // Fill the (static) combboxes (the continent combobox has been filled in Lazarus)
  ComboboxFillDrDi(DrainageLimits, cobDrainage);
  ComboboxFillDrDi(DischargeLimits, cobDischarge);
  ComboboxFillLength(LengthLimits, cobLength, sLengthUnit);
  // Start random number generator
  Randomize;
  // Prepare for a "river length" quiz
  mQuizLength.Click;
end;

{ Menu item "Quiz > River length": Prepare for a "river length" quiz }

procedure TfGeo1.mQuizLengthClick(Sender: TObject);

begin
  iQuiz := 1;
  NewQuiz(iQuiz, iQuestionsTemp, aRivers, sListContinent, laQuizQuestions, cobQuizQuestions,
    edOutflow, laCountries, cbCountries, iListMax, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Drainage area": Prepare for a "river drainage area" quiz }

procedure TfGeo1.mQuizDrainageClick(Sender: TObject);

begin
  iQuiz := 2;
  NewQuiz(iQuiz, iQuestionsTemp, aRivers, sListContinent, laQuizQuestions, cobQuizQuestions,
    edOutflow, laCountries, cbCountries, iListMax, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Average discharge": Prepare for a "river discharge" quiz }

procedure TfGeo1.mQuizDischargeClick(Sender: TObject);

begin
  iQuiz := 3;
  NewQuiz(iQuiz, iQuestionsTemp, aRivers, sListContinent, laQuizQuestions, cobQuizQuestions,
    edOutflow, laCountries, cbCountries, iListMax, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > River outflow": Prepare for a "outflow river/sea" quiz }

procedure TfGeo1.mQuizOutflowClick(Sender: TObject);

begin
  iQuiz := 4;
  NewQuiz(iQuiz, iQuestionsTemp, aRivers, sListContinent, laQuizQuestions, cobQuizQuestions,
    edOutflow, laCountries, cbCountries, iListMax, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > River continent": Prepare for a "river continent" quiz }

procedure TfGeo1.mQuizContinentClick(Sender: TObject);

begin
  iQuiz := 5;
  NewQuiz(iQuiz, iQuestionsTemp, aRivers, sListContinent, laQuizQuestions, cobQuizQuestions,
    edOutflow, laCountries, cbCountries, iListMax, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Drainage contries": Prepare for a "river drainage countries" quiz }

procedure TfGeo1.mQuizCountriesClick(Sender: TObject);

begin
  iQuiz := 6;
  NewQuiz(iQuiz, iQuestionsTemp, aRivers, sListContinent, laQuizQuestions, cobQuizQuestions,
    edOutflow, laCountries, cbCountries, iListMax, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfGeo1.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User entry of number of quiz questions }

procedure TfGeo1.mOptionsQuestionsClick(Sender: TObject);

// The number of questions, entered by the user, will be automatically corrected, if it exceeds
// the number of rivers available in the actual selected list (and further reduced, if some rivers
// can't be asked, because of data deficiency)

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

{ Menu items "Options > Quiz river list > ...": Select the rivers, that should be part of the quiz questions list }

procedure TfGeo1.mOptionsList25Click(Sender: TObject);

begin
  mOptionsList25.Checked := True;   mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := '--'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.mOptionsList50Click(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := True;   mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := '--'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.mOptionsList100Click(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := True;   mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := '--'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.mOptionsListFullClick(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := True;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := '--'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.mOptionsListAFClick(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := True;   mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := 'AF'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.moptionsListASClick(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := True;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := 'AS'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.moptionsListAS2Click(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := True;   mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := 'AS'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.moptionsListEUClick(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := True;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := False;
  sListContinent := 'EU'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.mOptionsListNAClick(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := True;   mOptionsListSA.Checked := False;
  sListContinent := 'NA'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

procedure TfGeo1.mOptionsListSAClick(Sender: TObject);

begin
  mOptionsList25.Checked := False;  mOptionsList50.Checked := False;  mOptionsList100.Checked := False;  mOptionsListFull.Checked := False;
  mOptionsListAF.Checked := False;  mOptionsListAS.Checked := False;  mOptionsListAS2.Checked := False;  mOptionsListEU.Checked := False;
  mOptionsListNA.Checked := False;  mOptionsListSA.Checked := True;
  sListContinent := 'SA'; iListMax := GetListMax(aRivers, iQuiz, sListContinent);
  SetTitle(iQuiz, sListContinent);
end;

{ Menu item "Options > Outflow entry > Manual entry": Usage of an edit field, where the user has to manually enter the outflow }

procedure TfGeo1.mOptionsOutFlowManualClick(Sender: TObject);

begin
  mOptionsOutFlowManual.Checked := True;
  mOptionsOutflowSelection.Checked := False;
  if iQuiz = 4 then begin
    edOutflow.Visible := True; cobOutflow.Visible := False;
  end;
end;

{ Menu item "Options > Outflow entry > Selection list": Usage of a combobox, where the user may select an outflow from }

procedure TfGeo1.mOptionsOutflowSelectionClick(Sender: TObject);

begin
  mOptionsOutFlowManual.Checked := False;
  mOptionsOutflowSelection.Checked := True;
  if iQuiz = 4 then begin
    edOutflow.Visible := False; cobOutflow.Visible := True; cobOutflow.Items.Clear;
  end;
end;

{Menu item "Options > River lengths in miles": Toggle to use km or mi as river length unit }

procedure TfGeo1.mOptionsMilesClick(Sender: TObject);

begin
  if mOptionsMiles.Checked then begin
    mOptionsMiles.Checked := False;
    sLengthUnit := 'km'; laULength.Caption := sLengthUnit;
    ComboboxFillLength(LengthLimits, cobLength, sLengthUnit);
  end
  else begin
    mOptionsMiles.Checked := True;
    sLengthUnit := 'mi'; laULength.Caption := sLengthUnit;
    ComboboxFillLength(LengthLimits, cobLength, sLengthUnit);
  end;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfGeo1.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfGeo1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Geography quiz:' + LineEnding;
  S += 'Rivers of the world.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April 2020.';
  MessageDlg('About "Geography1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfGeo1.btQuestionClick(Sender: TObject);

var
  Count, RX, I: Integer;
  Answer, UAnswer, Countries: string;
  OK: Boolean;

begin
  // Button "Start/Question": Generate new question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    OK := True;
    // Some validity checings and initialisations to do before starting the quiz
    if btQuestion.Caption = 'Start' then begin
      // Do not allow useless or senseless quizes
      if (iQuiz = 5) and (sListContinent <> '--') then begin
        MessageDlg('Geography quiz', 'You can''t do a continent quiz with actual selections!', mtError, [mbOK], 0); OK := False;
      end
      else if (iQuiz = 6) and ((sListContinent = 'AU') or (sListContinent = 'NA')) then begin
        MessageDlg('Geography quiz', 'You can''t do a country quiz with actual selections!', mtError, [mbOK], 0);  OK := False;
      end
      // If selections ok, init the quiz
      else begin
        if iQuestions > iListMax then
          iQuestions := iListMax;                                              // max. of questions limited by rivers actually available in the list
        iQuestion := 0; iCorrect := 0;
        for I := 0 to Length(aRiversDone) - 1 do
          aRiversDone[I] := False;                                             // set all rivers to "not yet done"
        laEval.Caption := 'Evaluation:';
        for I := 0 to 3 do
          sgEval.Cells[1, I] := '';
        fGeo1.mOptions.Enabled := False;                                       // disable "Options" menu (until end of quiz or a "Quiz" menu item selected)
      end;
    end;
    // Proceed, if all is ok and there are still questions (rivers) left
    if OK and (iQuestion < iQuestions) then begin
      Inc(iQuestion);
      laEval.Caption := 'Evaluation (' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + '):';
      edEval.Text := ''; edEval.Color := clDefault;
      edOutflow.Text := '';
      for I := 1 to 5 do
        cbCountries[I].Checked := False;
      // Repeat getting a random river, until a "valid" one has been found
      repeat
        OK := True;
        RX := Random(Length(aRivers));
        if aRiversDone[RX] or IsNotInList(RX, aRivers[RX].RContinent, sListContinent) then
          // River has not yet to be asked and must be one of those in the actual list
          OK := False
        else if (iQuiz = 2) and (aRivers[RX].RDrainage = -1) then
          // For a drainage quiz, the drainage area value must be known
          OK := False
        else if (iQuiz = 3) and (aRivers[RX].RDischarge = -1) then
          // For a discharge quiz, the discharge value must be known
          OK := False;
      until OK;
      // Display the river name (and adapt dynamic comboboxes / set checkbox labels)
      aRiversDone[RX] := True;                                                 // mark river as "done"
      edName.Text := aRivers[RX].RName;
      if (iQuiz = 4) and mOptionsOutflowSelection.Checked then
        // Outflow quiz: Fill the outflow combobox with rivers/seas depending on actual river
        ComboboxFillOutflow(aAllContinentData, cobOutflow, aRivers[RX].RContinent, aRivers[RX].RName, aRivers[RX].ROutflow)
      else if iQuiz = 6 then
        // Countries quiz: Choose countries and use them as checkbox labels, depending on continent of actual river
        FillCountries(aAllContinentData, cbCountries, aRivers[RX].RContinent, aRivers[RX].RCountries);
      // Determine answer of actual question (depending on kind of quiz)
      case iQuiz of
        1: if mOptionsMiles.Checked then
             sAnswer := GetLengthInterval(1, aRivers[RX].RLengthMI)
           else
             sAnswer := GetLengthInterval(0, aRivers[RX].RLengthKM);
        2: sAnswer := GetDrainageInterval(aRivers[RX].RDrainage);
        3: sAnswer := GetDischargeInterval(aRivers[RX].RDischarge);
        4: sAnswer := aRivers[RX].ROutflow;
        5: sAnswer := GetContinentName(aRivers[RX].RContinent);
        6: sAnswer := GetCountryQuizAnswer(cbCountries, aRivers[RX].RCountries);
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
        1: UAnswer := cobLength.Items[cobLength.ItemIndex];
        2: UAnswer := cobDrainage.Items[cobDrainage.ItemIndex];
        3: UAnswer := cobDischarge.Items[cobDischarge.ItemIndex];
        4: if moptionsOutflowManual.Checked then begin
             // Get outflow from edit field (and consider checking validity of answer with "English letters" names)
             Answer  := GetEnglishChars(sAnswer);
             UAnswer := GetEnglishChars(edOutflow.Text);
           end
           else
             // Get answer from combobox
             UAnswer := cobOutflow.Items[cobOutflow.ItemIndex];
        5: UAnswer := cobContinent.Items[cobContinent.ItemIndex];
        6: UAnswer := GetCountryUserAnswer(cbCountries);
      end;
      // Check answer and display evaluation message
      if UAnswer = Answer then begin
        // Correct answer
        if Answer = sAnswer then begin
          // "Normal" case
          edEval.Text := 'This is correct!'; edEval.Color := clLime;
        end
        else begin
          // Misspelled outflow name
          edEval.Text := 'Correctly spelled answer = ' + sAnswer; edEval.Color := clYellow;
        end;
        Inc(iCorrect);
      end
      else begin
        // False answer: Display the correct one
        edEval.Text := 'False! Correct answer = ';
        if iQuiz = 6 then begin
          // Transform the "0/1" answer string to a readable answer with country names
          Countries := ''; Count := 0;
          for I := 1 to 5 do begin
            if sAnswer[I] = '1' then begin
              if Countries <> ''  then
                Countries += ', ';
              Countries += cbCountries[I].Caption;
              Inc(Count);
            end;
          end;
          if Count > 0 then
            edEval.Text := edEval.Text + Countries
          else
            edEval.Text := 'none...';
        end
        else
          edEval.Text := edEval.Text + sAnswer; edEval.Color := clRed;
      end;
      //  Update evaluation counters
      sgEval.Cells[1, 0] := GFormat(iQuestion, '');
      sgEval.Cells[1, 1] := GFormat(iCorrect, '');
      sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
      sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
      // If all rivers have been done, terminate the quiz
      if iQuestion = iQuestions then begin
        MessageDlg('Geography quiz', 'All questions have been done. Quiz over.', mtInformation, [mbOK], 0);
        btQuestion.Caption := 'Start';
        mOptions.Enabled := True;                                              // give user access to "Options" menu again
      end
      // If there are rivers left, continue the quiz
      else
        btQuestion.Caption := 'Question';                                      // next push on button will be to generate a new question
    end;
  end;
end;

end.

