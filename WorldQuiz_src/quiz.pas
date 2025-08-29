{***************************************}
{* Main unit for WorldQuiz application *}
{***************************************}

unit quiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

type
  TCountry = record
    Continent: Integer;
    CountryName, CapitalName: string[30];
    CountryArea, CountryPopulation, CapitalPopulation: Integer;
    IsDependentCountry: Boolean;
  end;
  TCountries = array of TCountry;
  {*************}
  { TfWorldQuiz }
  {*************}
  TfWorldQuiz  = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizNew, mQuizExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsQuiz, mOptionsContinent, mOptionsDependent: TMenuItem;
    mOptionsQuiz1a, mOptionsQuiz1b, mOptionsQuiz2, mOptionsQuiz3, mOptionsQuiz4: TMenuItem;
    mOptionsContinentAF, mOptionsContinentAS, mOptionsContinentEU, mOptionsContinentNA: TMenuItem;
    mOptionsContinentOC, mOptionsContinentSA, mOptionsContinentAll: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laQuestion, laCountryCity, laQuizItem, Label2, Label3, Label4: TLabel;
    edCountryCity, edQuizItem, edEval: TEdit;
    cobQuizItem: TComboBox;
    edQuestion, edCorrect, edSuccess: TEdit;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MenuQuizNewClick(Sender: TObject);
    procedure MenuQuizExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsQuiz1aClick(Sender: TObject);
    procedure mOptionsQuiz1bClick(Sender: TObject);
    procedure mOptionsQuiz2Click(Sender: TObject);
    procedure mOptionsQuiz3Click(Sender: TObject);
    procedure mOptionsQuiz4Click(Sender: TObject);
    procedure mOptionsContinentAFClick(Sender: TObject);
    procedure mOptionsContinentAllClick(Sender: TObject);
    procedure mOptionsContinentASClick(Sender: TObject);
    procedure mOptionsContinentEUClick(Sender: TObject);
    procedure mOptionsContinentNAClick(Sender: TObject);
    procedure mOptionsContinentOCClick(Sender: TObject);
    procedure mOptionsContinentSAClick(Sender: TObject);
    procedure mOptionsDependentClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuizSelection, iCountry, iMaxCountries, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sContinent: string;
    aCountries: TCountries;
    aCountriesDone: array of Boolean;
  end;

const
  aQuizSelections1: array[1..5] of string = (
    'Capitals', 'Capitals', 'Land Areas', 'Country Inhabitants', 'Capital Inhabitants');
  aQuizSelections2: array[1..5] of string = (
    'Capital', 'Capital', 'Land area', 'Inhabitants', 'Inhabitants');
  aContinents: array[0..5] of string = (
    'Africa', 'Asia', 'Europe', 'North America', 'Oceania', 'South America'
  );
  // Land area limit values for combobox selection list
  aAreaLimits: array[0..16] of Integer = (
    0, 1000, 10000, 50000, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000,
    1000000, 2000000, 5000000, 20000000
  );
  // Country population limit values for combobox selection list
  aPopulationLimits: array[0..16] of Integer = (
    0, 10000, 50000, 100000, 500000, 1000000, 2500000, 5000000, 7500000,
    10000000, 25000000, 50000000, 75000000, 100000000, 200000000, 1000000000, 2000000000
  );
  // Capital population limit values for combobox selection list
  aPopulationLimits2: array[0..16] of Integer = (
    0, 1000, 10000, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000,
    1000000, 2500000, 5000000, 10000000, 25000000
  );

var
  fWorldQuiz: TfWorldQuiz;

implementation

{$R *.lfm}

{ Read country data from file }

procedure ReadCountries(out Countries: TCountries);

var
  N: Integer;
  CountryFile: file of TCountry;

begin
  SetLength(Countries, 0);
  Assign(CountryFile, 'worldquiz.dat'); Reset(CountryFile); N := 0;
  while not EoF(CountryFile) do begin
    Inc(N); SetLength(Countries, N);
    Read(CountryFile, Countries[N - 1]);
  end;
  Close(CountryFile);
end;

{ Reset the form controls }

procedure ClearForm(out NQ: Integer);

begin
  fWorldQuiz.laQuestion.Caption := 'Question:';
  fWorldQuiz.edCountryCity.Text := '';
  fWorldQuiz.edQuizItem.Text := ''; fWorldQuiz.cobQuizItem.ItemIndex := -1;
  fWorldQuiz.edEval.Text := '';
  fWorldQuiz.edQuestion.Text := ''; fWorldQuiz.edCorrect.Text := '';
  fWorldQuiz.edSuccess.Text := '';  fWorldQuiz.edSuccess.Color := clDefault;
  fWorldQuiz.mOptions.Enabled := True;                                         // give user access to "Options" menu
  fWorldQuiz.btQuestion.Caption := 'Question'; fWorldQuiz.btQuestion.Enabled := True;
  NQ := 0;
end;

{ Quiz selection: Set title and labels; display selection combobox or edit field }

procedure QuizSelection(Selection: Integer; Continent: string);

begin
  fWorldQuiz.stTitle.Caption := Continent + ' Quiz : ' + aQuizSelections1[Selection] + '.';
  if Selection = 5 then
    fWorldQuiz.laCountryCity.Caption := 'City'
  else
    fWorldQuiz.laCountryCity.Caption := 'Country';
  fWorldQuiz.laQuizItem.Caption := aQuizSelections2[Selection];
  if Selection = 2 then begin
    // Capitals manual entry: User answer will be retrieved from edit field
    fWorldQuiz.edQuizItem.Visible := True; fWorldQuiz.edQuizItem.Text := '';
    fWorldQuiz.cobQuizItem.Visible := False;
  end
  else begin
    // Capitals from list, areas, or inhabitants: User answer will be retrieved from combobox
    fWorldQuiz.edQuizItem.Visible := False;
    fWorldQuiz.cobQuizItem.Visible := True; fWorldQuiz.cobQuizItem.Clear;
  end;
  fWorldQuiz.btQuestion.Caption := 'Question';
end;

{ Fill combobox with selection list of capitals }

procedure FillCapitals(Country0: Integer; var Countries: TCountries; Continent: string; Dependent: Boolean; MaxCountries: Integer);

var
  Country, N, I, J: Integer;
  Temp: string;
  OK: Boolean;

begin
  // Number of list items = 15, except for world quiz (20)
  if Continent = 'World' then
    N := 20
  else
    N := 15;
  // Adapt number of list items, if this number is bigger than the number of total countries for actual setttings (Oceania without dependent territories...)
  if N > MaxCountries then
    N := MaxCountries - 1;
  // Fill the combobox (countries must varify actual settings criteria!)
  fWorldQuiz.cobQuizItem.Items.Clear;
  fWorldQuiz.cobQuizItem.Items.AddText(Countries[Country0].CapitalName);       // this is the "correct answer" capital
  for I := 1 to N - 1 do begin
    repeat
      OK := True;
      // Random country
      Country := Random(Length(Countries));
      // Continent based quiz: Use only capitals for countries of given country's continent
      if (Continent <> 'World') and (aContinents[Countries[Country].Continent] <> Continent) then
        OK := False
      // World countries based quiz: Use 10 capitals for countries of given country's continent and 10 from any continent
      else if (Continent = 'World') and (I < 10) and (aContinents[Countries[Country].Continent] <> aContinents[Countries[Country0].Continent]) then
        OK := False
      // Do not include capitals of dependent territories into list, if these countries are not set to be included
      else if not Dependent and Countries[Country].IsDependentCountry then
        OK := False
      // Some countries have no capital stated: can't be used in list
      else if Countries[Country].CapitalName = '' then
        OK := False
      // And finally: List entries have to be unique
      else begin
        for J := 0 to I - 1 do begin
          if Countries[Country].CapitalName = fWorldQuiz.cobQuizItem.Items[J] then
            OK := False;
        end;
      end;
    until OK;
    fWorldQuiz.cobQuizItem.Items.AddText(Countries[Country].CapitalName);
  end;
  // Sort combobox items
  for I := 1 to N - 1 do begin
    for J := 0 to I - 1 do begin
      if fWorldQuiz.cobQuizItem.Items[J] > fWorldQuiz.cobQuizItem.Items[I] then begin
        Temp := fWorldQuiz.cobQuizItem.Items[J];
        fWorldQuiz.cobQuizItem.Items[J] := fWorldQuiz.cobQuizItem.Items[I];
        fWorldQuiz.cobQuizItem.Items[I] := Temp;
      end;
    end;
  end;
end;

{ Fill combobox with area/population limits selection list }

procedure FillLimitsList(var Limits: array of Integer);

var
  I: Integer;

begin
  fWorldQuiz.cobQuizItem.Items.Clear;
  for I := 0 to Length(Limits) - 2 do begin
    if I = 0 then
      fWorldQuiz.cobQuizItem.Items.AddText('less than ' + FloatToStrF(Limits[I + 1], ffNumber, 0, 0))
    else if I = Length(Limits) - 2 then
      fWorldQuiz.cobQuizItem.Items.AddText('more than ' + FloatToStrF(Limits[I], ffNumber, 0, 0))
    else
      fWorldQuiz.cobQuizItem.Items.AddText('between ' + FloatToStrF(Limits[I], ffNumber, 0, 0) + ' and ' + FloatToStrF(Limits[I + 1], ffNumber, 0, 0));
  end;
end;

{ Get maximum number of countries for given quiz settings }

function GetNumberOfCountries(var Countries: TCountries; Quiz: Integer; Continent: string; Dependent: Boolean): Integer;

var
  N, I: Integer;

begin
  // Begin with setting maximum = total number of countries
  N := Length(Countries);
  // Check all countries in the list, and decrease maximum number, if they do not verify the actual quiz selections
  for I := 0 to Length(Countries) - 1 do begin
    if (Continent <> 'World') and (Continent <> aContinents[Countries[I].Continent]) then
      // For continent based quizes, countries must be part of this continent
      Dec(N)
    else if not Dependent and Countries[I].IsDependentCountry then
      // If dependent territories are not set to be included, such "countries" must not be used
      Dec(N)
    else if (Quiz in [1, 2]) and (Countries[I].CapitalName = '') then
      // For the capitals quiz, countries must have capital name set
      Dec(N)
    else if (Quiz = 3) and (Countries[I].CountryArea = 0) then
      // For land area quiz, do not use areas of zero (Holy See)
      Dec(N)
    else if (Quiz = 5) and (Countries[I].CapitalPopulation <= 0) then
      // For capital population quiz, population must be set and not be "uncertain source?"
      Dec(N)
  end;
  Result := N;
end;

{ For a given area/population value, get the corresponding limits string (as is part of the combobox selection list) }

function GetLimitsAnswer(Value: Integer; Limits: array of Integer; LimitsCbox: TCombobox): string;

var
  IX, I: Integer;

begin
  I := 0; IX := -1;
  while (I < Length(Limits) - 1) and (IX < 0) do begin
    if (Value > Limits[I]) and (Value <= Limits[I + 1]) then
      IX := I;
    Inc(I);
  end;
  Result := LimitsCbox.Items[IX];
end;

{*************}
{ TfWorldQuiz }
{*************}

{ Application start: Initialisation }

procedure TfWorldQuiz.FormCreate(Sender: TObject);

begin
  ReadCountries(aCountries); SetLength(aCountriesDone, Length(aCountries));
  iQuizSelection := 1; sContinent := 'Europe'; iQuestionsTemp := 20;
  ClearForm(iQuestion);
  QuizSelection(iQuizSelection, sContinent);                                   // startup: capitals quiz with list selection
  Randomize;
end;

{ Menu item "Quiz > New": Reset form controls for new quiz }

procedure TfWorldQuiz.MenuQuizNewClick(Sender: TObject);

begin
  ClearForm(iQuestion);                                                        // clear form controls and re-enable the "Options" menu
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfWorldQuiz.MenuQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Number of questions ...": User entry of number of questions }

procedure TfWorldQuiz.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('World quiz', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
  if iQuestionsTemp < 10 then
    iQuestionsTemp := 10;                                                      // minimum number of questions arbitrarily set to 10
end;

{ Menu items "Options > Quiz type selection > ...": Select quiz type }

procedure TfWorldQuiz.mOptionsQuiz1aClick(Sender: TObject);

begin
  mOptionsQuiz1a.Checked := True;  mOptionsQuiz1b.Checked := False;
  mOptionsQuiz2.Checked  := False; mOptionsQuiz3.Checked  := False; mOptionsQuiz4.Checked := False;
  iQuizSelection := 1;
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsQuiz1bClick(Sender: TObject);

begin
  mOptionsQuiz1a.Checked := False; mOptionsQuiz1b.Checked := True;
  mOptionsQuiz2.Checked  := False; mOptionsQuiz3.Checked  := False; mOptionsQuiz4.Checked := False;
  iQuizSelection := 2;
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsQuiz2Click(Sender: TObject);

begin
  mOptionsQuiz1a.Checked := False; mOptionsQuiz1b.Checked := False;
  mOptionsQuiz2.Checked  := True;  mOptionsQuiz3.Checked  := False; mOptionsQuiz4.Checked := False;
  iQuizSelection := 3;
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsQuiz3Click(Sender: TObject);

begin
  mOptionsQuiz1a.Checked := False; mOptionsQuiz1b.Checked := False;
  mOptionsQuiz2.Checked  := False; mOptionsQuiz3.Checked  := True; mOptionsQuiz4.Checked := False;
  iQuizSelection := 4;
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsQuiz4Click(Sender: TObject);

begin
  mOptionsQuiz1a.Checked := False; mOptionsQuiz1b.Checked := False;
  mOptionsQuiz2.Checked  := False; mOptionsQuiz3.Checked  := False; mOptionsQuiz4.Checked := True;
  iQuizSelection := 5;
  QuizSelection(iQuizSelection, sContinent);
end;

{ Menu items "Options > Continent selection: Select continent"}

procedure TfWorldQuiz.mOptionsContinentAFClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := True;  mOptionsContinentAS.Checked := False;
  mOptionsContinentEU.Checked := False; mOptionsContinentNA.Checked := False;
  mOptionsContinentOC.Checked := False; mOptionsContinentSA.Checked := False; mOptionsContinentAll.Checked := False;
  sContinent := 'Africa';
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsContinentASClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := False; mOptionsContinentAS.Checked := True;
  mOptionsContinentEU.Checked := False; mOptionsContinentNA.Checked := False;
  mOptionsContinentOC.Checked := False; mOptionsContinentSA.Checked := False; mOptionsContinentAll.Checked := False;
  sContinent := 'Asia';
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsContinentEUClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := False; mOptionsContinentAS.Checked := False;
  mOptionsContinentEU.Checked := True;  mOptionsContinentNA.Checked := False;
  mOptionsContinentOC.Checked := False; mOptionsContinentSA.Checked := False; mOptionsContinentAll.Checked := False;
  sContinent := 'Europe';
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsContinentNAClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := False; mOptionsContinentAS.Checked := False;
  mOptionsContinentEU.Checked := False; mOptionsContinentNA.Checked := True;
  mOptionsContinentOC.Checked := False; mOptionsContinentSA.Checked := False; mOptionsContinentAll.Checked := False;
  sContinent := 'North America';
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsContinentOCClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := False; mOptionsContinentAS.Checked := False;
  mOptionsContinentEU.Checked := False; mOptionsContinentNA.Checked := False;
  mOptionsContinentOC.Checked := True;  mOptionsContinentSA.Checked := False; mOptionsContinentAll.Checked := False;
  sContinent := 'Oceania';
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsContinentSAClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := False; mOptionsContinentAS.Checked := False;
  mOptionsContinentEU.Checked := False; mOptionsContinentNA.Checked := False;
  mOptionsContinentOC.Checked := False; mOptionsContinentSA.Checked := True; mOptionsContinentAll.Checked := False;
  sContinent := 'South America';
  QuizSelection(iQuizSelection, sContinent);
end;

procedure TfWorldQuiz.mOptionsContinentAllClick(Sender: TObject);

begin
  mOptionsContinentAF.Checked := False; mOptionsContinentAS.Checked := False;
  mOptionsContinentEU.Checked := False; mOptionsContinentNA.Checked := False;
  mOptionsContinentOC.Checked := False; mOptionsContinentSA.Checked := False; mOptionsContinentAll.Checked := True;
  sContinent := 'World';
  QuizSelection(iQuizSelection, sContinent);
end;

{ Menu item "Options > Include dependent territories": Toggle inclusion or not of dependent territories }

procedure TfWorldQuiz.mOptionsDependentClick(Sender: TObject);

begin
  if mOptionsDependent.Checked then
    mOptionsDependent.Checked := False
  else
    mOptionsDependent.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfWorldQuiz.MenuHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'World countries quiz:' + LineEnding;
  S += 'Capitals, country surface areas, country and capital inhabitants.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, November 2020.';
  MessageDlg('About "WorldQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new question resp. check user answer }

procedure TfWorldQuiz.btQuestionClick(Sender: TObject);

var
  I: Integer;
  OK, IsCorrect: Boolean;
  Percent: Real;
  CorrectAnswer, CorrectAnswer2, UserAnswer: string;

begin
  // Button "Question": Generate quiz question
  if btQuestion.Caption = 'Question' then begin
    // User selections checking and initialisations at quiz start
    if iQuestion = 0 then begin
      // Check if  a quiz type and a continent is selected
      OK := True;
      if not (mOptionsQuiz1a.Checked or mOptionsQuiz1b.Checked or mOptionsQuiz2.Checked or mOptionsQuiz3.Checked or mOptionsQuiz4.Checked) then begin
        MessageDlg('Selection error', 'There is no quiz type selected!', mtError, [mbOK], 0);
        OK := False;
      end;
      if not (mOptionsContinentAF.Checked or mOptionsContinentAS.Checked or mOptionsContinentEU.Checked
        or mOptionsContinentNA.Checked or mOptionsContinentOC.Checked or mOptionsContinentSA.Checked or mOptionsContinentAll.Checked) then begin
        MessageDlg('Selection error', 'There is no continent selected!', mtError, [mbOK], 0);
        OK := False;
      end;
      // If all ok, proceed with initialisations
      if OK then begin
        stTitle.Caption := sContinent + ' Quiz : ' + aQuizSelections1[iQuizSelection] + '.';
        // Number of quiz questions: Use the one entered by user, unless it is bigger than total number of countries for actual quiz settings
        iQuestions := iQuestionsTemp;
        iMaxCountries := GetNumberOfCountries(aCountries, iQuizSelection, sContinent, mOptionsDependent.Checked);
        if iQuestions > iMaxCountries then
          iQuestions := iMaxCountries;
        // At start of quiz, set all countries to "not done"
        for I := 1 to Length(aCountriesDone) - 1 do
          aCountriesDone[I] := False;
        // Reset correct answers counter
        iCorrect := 0;
        // Block user access to "Options" menu (until "Quiz > New" has been selected)
        mOptions.Enabled := False;
      end;
    end;
    // If all ok, generate the quiz question
    if OK then begin
      Inc(iQuestion);
      laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ':';
      // Get a random country (among those verifying selection criteria)
      repeat
        OK := True;
        iCountry := Random(Length(aCountries));
        if aCountriesDone[iCountry] then
          // Country must be "not already done"
          OK := False
        else if (sContinent <> 'World') and (aContinents[aCountries[iCountry].Continent] <> sContinent) then
          // For a continent based quiz, country must be on this continent
          OK := False
        else if not mOptionsDependent.Checked and aCountries[iCountry].IsDependentCountry then
          // If dependent territories aren't to be included, country must be an independnt one
          OK := False
        else if (iQuizSelection in [1, 2]) and (aCountries[iCountry].CapitalName = '') then
          // Can't use countries with capital name not set in capitals quiz
          OK := False
        else if (iQuizSelection = 3) and (aCountries[iCountry].CountryArea = 0) then
          // Exclude Holy See (area = 0) in country area quiz
          OK := False
        else if (iQuizSelection = 5) and (aCountries[iCountry].CapitalPopulation <= 0) then
          // Can't use countries with capital population not set (or uncertain) in capitals populations quiz
          OK := False;
      until OK;
      aCountriesDone[iCountry] := True;                                        // mark the country as "done"
      // Display country or city (case of capitals population quiz)
      if iQuizSelection = 5 then
        edCountryCity.Text := aCountries[iCountry].CapitalName
      else
        edCountryCity.Text := aCountries[iCountry].CountryName;
      // Fill combobox with selection list items for actual quiz (except for capitals quiz with manual entry)
      case iQuizSelection of
        1: FillCapitals(iCountry, aCountries, sContinent, mOptionsDependent.Checked, iMaxCountries);
        3: FillLimitsList(aAreaLimits);
        4: FillLimitsList(aPopulationLimits);
        5: FillLimitsList(aPopulationLimits2);
      end;
      // Clear all answer fields
      edQuizItem.Text  := ''; cobQuizItem.Text := '';
      edEval.Text := '';
      // Next button action will be to check user's answer
      btQuestion.Caption := 'Answer';
    end;
  end
  // Button "Answer": Check user's answer
  else begin
    // Get user answer from form
    case iQuizSelection of
      1, 3, 4, 5: UserAnswer := cobQuizItem.Text;                              // answer retrieved from edit field
               2: UserAnswer := edQuizItem.Text;                               // answer retrieved from combobox
    end;
    // Get (correct) answer for actual quiz and actual country
    case iQuizSelection of
      1, 2: CorrectAnswer := aCountries[iCountry].CapitalName;
         3: CorrectAnswer := GetLimitsAnswer(aCountries[iCountry].CountryArea, aAreaLimits, cobQuizItem);
         4: CorrectAnswer := GetLimitsAnswer(aCountries[iCountry].CountryPopulation, aPopulationLimits, cobQuizItem);
         5: CorrectAnswer := GetLimitsAnswer(aCountries[iCountry].CapitalPopulation, aPopulationLimits2, cobQuizItem);
    end;
    // Check user's answer
    if UserAnswer = CorrectAnswer then
      // If the two answers (string compare) are equal, the quiz question has been correctly answered
      IsCorrect := True
    else begin
      // If the two answers (string compare) unare equal, the quiz question has been incorrectly answered
      // However, for capitals manual entry, allow user to use all "base letters" (a instead of á, e instead of é...)
      IsCorrect := False;
      if iQuizSelection = 2 then begin
        // User answer check with simplified capital name spelling
        CorrectAnswer2 := StringReplace(CorrectAnswer, 'á', 'a', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'å', 'a', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'ă', 'a', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'é', 'e', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'ó', 'o', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'ñ', 'n', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'ş', 's', [rfReplaceAll]);
        CorrectAnswer2 := StringReplace(CorrectAnswer2, 'St.', 'Saint', []);
        if UserAnswer = CorrectAnswer2 then
          IsCorrect := True;
      end;
    end;
    // Quiz question evaluation
    if IsCorrect then begin
      // Answer is correct
      Inc(iCorrect);
      if UserAnswer = CorrectAnswer then begin
        // Correct answer correctly spelled
        edEval.Text := 'Correct!';
        edEval.Font.Color := clGreen;
      end
      else begin
        // Correct answer with bad spelling
        edEval.Text := 'Correct = ' + CorrectAnswer;
        edEval.Font.Color := clFuchsia;
      end;
    end
    else begin
      // Answer is false
      edEval.Text := 'False! Correct = ' + CorrectAnswer;
      edEval.Font.Color := clRed;
    end;
    // Update questions done, correct answers and success percentage
    edQuestion.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    Percent := 100 * iCorrect / iQuestion;
    edSuccess.Text := IntToStr(Round(Percent)) + '%';
    // Color success field, depending on actual success percentage
    if Percent < 50 then
       edSuccess.Color := clRed
    else if Percent < 60 then
       edSuccess.Color := clYellow
    else
       edSuccess.Color := clLime;
    // Next button action will be to display a question
    btQuestion.Caption := 'Question';
    // All questions done: Terminate the quiz
    if iQuestion = iQuestions then begin
      MessageDlg('End of quiz','All countries have been done...', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // disable the button (until "Quiz > New" selected)
    end;
  end;
end;

end.

