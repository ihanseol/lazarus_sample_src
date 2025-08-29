{***************************************}
{* Main unit for EuroLakes application *}
{***************************************}

unit lakes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8, help;

type
  TLake = record
    LName: string;
    LCountries: array of string;
    LAreaKm2, LAreaMi2: Integer;
    LRegion: string;
    LCountriesLargest: array of string;
  end;
  TLakes = array of TLake;
  TCountries = array of string;
  {*********}
  { TfLakes }
  {*********}
  TfLakes = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizArea, mQuizLargest, mQuizRegion, mQuizCountries, mQuizExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsMiles, mOptionsRussia: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laLake, laLake2, laCountry, laUArea, laEval, laArea, laRegion, laCountries: TLabel;
    edLake, edCountry, edEval: TEdit;
    cobArea, cobRegion, cobLakes: TComboBox;
    cbCountry1, cbCountry2, cbCountry3, cbCountry4, cbCountry5: TCheckBox;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizRegionClick(Sender: TObject);
    procedure mQuizCountriesClick(Sender: TObject);
    procedure mQuizAreaClick(Sender: TObject);
    procedure mQuizLargestClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsMilesClick(Sender: TObject);
    procedure mOptionsRussiaClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuiz, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sAreaUnitTemp, sAreaUnit, sAnswer, sAnswer2: string;
    bRussianTemp, bRussian: Boolean;
    aQuestionsDone: array of Boolean;
    aLakes: TLakes;
    aCountries: TCountries;
    laQuizQuestions: array[0..2] of TLabel;
    cobQuizQuestions: array[0..2] of TCombobox;
    cbCountries:  array[0..4] of TCheckbox;
  end;

const
  Quizes: array[0..3] of string = (
    'lake region', 'lake countries', 'lake area', 'country''s largest lake'
  );
  LimitsKm2: array[0..12] of Integer = (
    125, 150, 200, 300, 400, 500, 600, 800, 1000, 2000, 4000, 5000, 10000
  );
  LimitsMi2: array[0..12] of Integer = (
    50, 60, 75, 100, 150, 200, 250, 300, 400, 800, 1600, 2000, 5000
  );

var
  fLakes: TfLakes;

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
    SN += S;                                                                                  // this is for the "%" sign
  Result := SN;
end;

{ Read lakes data from text file }

procedure ReadLakes(out Lakes: TLakes; out Countries: TCountries);

var
  N, C, I, J, P: Integer;
  Filename, Line, CountryList, LargestList: string;
  Ch: Char;
  NewCountry: Boolean;
  InFile: Text;

begin
  SetLength(Lakes, 0); SetLength(Countries, 0);
  Filename := 'lakes.txt';
  Assign(InFile, Filename); Reset(InFile);
  N := 0;
  while not EoF(Infile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N);
      SetLength(Lakes, N);
      // Fill the TLakes array with the data read
      with Lakes[N - 1] do begin
        LName := UTF8Trim(UTF8Copy(Line, 1, 25));                                             // lake name
        CountryList := UTF8Trim(UTF8Copy(Line, 26, 35));                                      // countries list
        SetLength(LCountries, 0); C := 0;
        repeat
          // Extract individual countries from the list
          Inc(C);
          SetLength(LCountries, C);
          P := UTF8Pos(', ', CountryList);                                                    // countries are separated by ", "
          if P > 0 then begin
            LCountries[C - 1] := UTF8Copy(CountryList, 1, P - 1);
            UTF8Delete(CountryList, 1, P + 1);
          end
          else begin
            LCountries[C - 1] := CountryList;
            CountryList := '';
          end;
        until CountryList = '';
        LAreaKm2 := StrToInt(UTF8Trim(StringReplace(UTF8Copy(Line, 61, 6), ',', '', [])));    // lake area in km²
        LAreaMi2 := StrToInt(UTF8Trim(StringReplace(UTF8Copy(Line, 71, 5), ',', '', [])));    // lake area in km²
        Ch := UTF8Copy(Line, 77, 1)[1];                                                       // lake region code
        case Ch of
          'M': LRegion := 'Middle Europe';
          'N': LRegion := 'North Europe';
          'S': LRegion := 'South Europe';
          'E': LRegion := 'East Europe';
          'R': LRegion := 'Russia';
        end;
        SetLength(LCountriesLargest, 0);
        if UTF8Length(Line) > 80 then begin
          LargestList := UTF8Trim(UTF8Copy(Line, 79, UTF8Length(Line)));                      //  largest lake countries list
          C := 0;
          repeat
            // Extract individual countries from the list
            Inc(C);
            SetLength(LCountriesLargest, C);
            P := UTF8Pos(', ', LargestList);                                                  // countries are separated by ", "
            if P > 0 then begin
              LCountriesLargest[C - 1] := UTF8Copy(LargestList, 1, P - 1);
              UTF8Delete(LargestList, 1, P + 1);
            end
            else begin
              LCountriesLargest[C - 1] := LargestList;
              LargestList := '';
            end;
            LCountriesLargest[C - 1] := StringReplace(LCountriesLargest[C - 1], '?', '', []);
          until LargestList = '';
        end;
      end;
    end;
  end;
  Close(InFile);
  // Fill countries array
  C := 0;
  for N := 0 to Length(Lakes) - 1 do begin
    for I := 0 to Length(Lakes[N].LCountries) - 1 do begin
      NewCountry := True;
      for J := 0 to Length(Countries) - 1 do begin
        // Country names have to be unique
        if Lakes[N].LCountries[I] = Countries[J] then
          NewCountry := False;
      end;
      if NewCountry then begin
        Inc(C); SetLength(Countries, C);
        Countries[C - 1] := Lakes[N].LCountries[I];
      end;
    end;
  end;
end;

{ Fill lake areas combobox }

procedure FillComboboxArea(AreaUnit: string);

var
  I: Integer;
  Limits: array[0..12] of Integer;

begin
  if AreaUnit = 'km²' then
    Limits := LimitsKm2
  else
    Limits := LimitsMi2;
  fLakes.laUArea.Caption := AreaUnit;
  fLakes.cobArea.Items.Clear;
  // First list entry
  fLakes.cobArea.Items.AddText('less than ' + IntToStr(Limits[0]));
  // Following list entries
  for I := 0 to 11 do
    fLakes.cobArea.Items.AddText('between ' + IntToStr(Limits[I]) + ' and ' + IntToStr(Limits[I + 1]));
  // Last list entry
  fLakes.cobArea.Items.AddText('more than ' + IntToStr(Limits[12]));
  fLakes.cobArea.ItemIndex := 0;
end;

{ Select random lake countries and set the corr. checkbox labels }

procedure FillCountries(var AllCountries: TCountries; var LakeCountries: array of string; var CountryBoxes: array of TCheckbox);

var
  IX, R, I, J: Integer;
  Country: string;
  OK: Boolean;

begin
  // Clear all checkbox labels
  for I := 0 to 4 do begin
    CountryBoxes[I].Caption := '';
  end;
  // For all 5 checkboxes, get a country and set it as its label
  for IX := 0 to 4 do begin
    // Get a random country
    repeat
      OK := True;
      // Get countries from: correct answers, or the all countries list
      R := Random(3);
      case R of
          0: Country := LakeCountries[Random(Length(LakeCountries))];
        1,2: Country := AllCountries[Random(Length(AllCountries))];
      end;
      // Country names have to be unique
      for J := 0 to IX - 1 do begin
        if Country = UTF8Trim(CountryBoxes[J].Caption) then
          OK := False;
      end;
    until OK;
    // Set the country name as checkbox label
    CountryBoxes[IX].Caption := ' ' + Country;
  end;
  // Sort the checkbox labels (order of the country checkboxes on the form)
  for I := 1 to 4 do begin
    for J := 0 to I - 1 do begin
      if CountryBoxes[J].Caption > CountryBoxes[I].Caption then begin
        Country := CountryBoxes[I].Caption; CountryBoxes[I].Caption := CountryBoxes[J].Caption; CountryBoxes[J].Caption := Country;
      end;
    end;
  end;
end;

{ Fill the lakes combobox for a given lake being the correct answer of a "largest lake" quiz question }

procedure FillLakes(LX: Integer; var Lakes: TLakes);

var
  IX, I, J: Integer;
  Lake: string;
  OK: Boolean;

begin
  fLakes.cobLakes.Items.Clear;
  // Add correct answer
  fLakes.cobLakes.Items.AddText(Lakes[LX].LName);
  // Add 9 further lakes (of same region, if possible)
  for I := 1 to 9 do begin
    repeat
      OK := True;
      IX := Random(Length(Lakes));
      if Lakes[IX].LRegion <> Lakes[LX].LRegion then
        OK := False
      else begin
        // Lake names have to be unique
        for J := 0 to I - 1 do begin
          if Lakes[IX].LName = fLakes.cobLakes.Items[J] then
            OK := False;
        end;
      end;
    until OK;
    // Add lake to combobox
    fLakes.cobLakes.Items.AddText(Lakes[IX].LName);
  end;
  // Sort the combobox items
  for I := 1 to 9 do begin
    for J := 0 to I - 1 do begin
      if fLakes.cobLakes.Items[J] > fLakes.cobLakes.Items[I] then begin
        Lake := fLakes.cobLakes.Items[I]; fLakes.cobLakes.Items[I] := fLakes.cobLakes.Items[J]; fLakes.cobLakes.Items[J] := Lake;
      end;
    end;
  end;
  fLakes.cobLakes.ItemIndex := 0;
end;

{ Prepare for a new quiz }

procedure NewQuiz(Quiz: Integer; var QuestionsTemp: Integer; AreaUnitTemp: string; RussianTemp: Boolean; var Lakes: TLakes;
  NCountries: Integer; var Labels: array of TLabel; CBoxes: array of TCombobox; var CountryBoxes: array of TCheckbox;
  out Questions: Integer; out AreaUnit: string; out Russian: Boolean);

var
  MaxLakes, I: Integer;
  S: string;

begin
  // Set quiz title
  fLakes.stTitle.Caption := 'Geography quiz: European lakes - ' + Quizes[Quiz] + '.';
  // Quiz options become active now
  Questions := QuestionsTemp;
  AreaUnit := AreaUnitTemp;
  Russian := RussianTemp;
  if Quiz = 3 then begin
    // For "largest lake" quiz, maximum of questions possible = number of countries in the list
    if Questions > NCountries then begin
      S := 'Number of questions exceeds number of countries in the list!' + LineEnding;
      S += 'Auto-reset to maximum available.';
      MessageDlg('Geography quiz', S, mtWarning, [mbOK], 0);
      Questions := NCountries;
    end;
  end
  else begin
    // For other quizzes, maximum of questions possible = number of lakes (with or without Russian ones) in the list
    MaxLakes := Length(Lakes);
    if not Russian then begin
      // If Russian lakes are excluded, the maximum of questions possible must be adapted
      for I := 0 to Length(Lakes) - 1 do begin
        if Lakes[I].LRegion = 'Russia' then
          Dec(MaxLakes);
      end;
    end;
    if Questions > MaxLakes then begin
      S := 'Number of questions exceeds number of lakes in the list!' + LineEnding;
      S += 'Auto-reset to maximum available.';
      MessageDlg('Geography quiz', S, mtWarning, [mbOK], 0);
      Questions := MaxLakes;
    end;
  end;
  QuestionsTemp := Questions;                                                                 // to avoid repetitive warnings
  // Reset variables and form controls
  fLakes.edLake.Text := ''; fLakes.edCountry.Text := '';
  fLakes.edEval.Text := ''; fLakes.edEval.Color := cldefault;
  fLakes.laEval.Caption := 'Evaluation:';
  for I := 0 to 3 do
    fLakes.sgEval.Cells[1, I] := '';
  fLakes.btQuestion.Caption := 'Start';
  // Display/hide form input fields as needed
  fLakes.laLake.Visible := True; fLakes.edLake.Visible := True;
  fLakes.laCountry.Visible := False; fLakes.edCountry.Visible := False;
  fLakes.laCountries.Visible := False;
  for I := 0 to 2 do begin
    Labels[I].Visible := False; CBoxes[I].Visible := False;
  end;
  for I := 0 to 4 do begin
    CountryBoxes[I].Visible := False;
  end;
  fLakes.laUArea.Visible := False; fLakes.laCountries.Visible := False;
  case Quiz of
    0: begin
      Labels[0].Visible := True; CBoxes[0].Visible := True;
    end;
    1: begin
      fLakes.laCountries.Visible := True;
      for I := 0 to 4 do begin
        CountryBoxes[I].Visible := True;
        CountryBoxes[I].Caption := 'Country ' + IntToStr(I + 1);
      end;
    end;
    2: begin
      Labels[1].Visible := True; CBoxes[1].Visible := True;
      FillComboboxArea(AreaUnit);
      fLakes.laUArea.Visible := True;
    end;
    3: begin
      fLakes.laLake.Visible := False; fLakes.edLake.Visible := False;
      fLakes.laCountry.Visible := True; fLakes.edCountry.Visible := True;
      Labels[2].Visible := True; CBoxes[2].Visible := True;
    end;
  end;
end;

{ Determine area interval for given area value }

function GetAreaInterval(AreaUnit: string; Area: Integer): string;

// The user area answer (combobox item) will be evaluated by comparing it to this area string

var
  I: Integer;
  AreaInterval: string;
  Limits: array[0..12] of Integer;

begin
  AreaInterval := '';
  if AreaUnit = 'km²' then
    Limits := LimitsKm2
  else
    Limits := LimitsMi2;
  // In order to evaluate the user answer correctly, the area string must, of course, be exactly equal to one of the combobox entries
  if Area < Limits[0] then
    AreaInterval := 'less than ' + IntToStr(Limits[0])
  else if Area > Limits[12] then
      AreaInterval := 'more than ' + IntToStr(Limits[12])
  else begin
    for I := 0 to 11 do begin
      if (Area >= (Limits[I])) and (Area < (Limits[I + 1])) then
        AreaInterval := 'between ' + IntToStr(Limits[I]) + ' and ' + IntToStr(Limits[I + 1]);
    end;
  end;
  Result := AreaInterval;
end;

{ Get (correct) answer for lake countries quiz }

function GetCountryQuizAnswer(var Countries: array of string; var CountryBoxes: array of TCheckbox): string;

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
      if UTF8Trim(CountryBoxes[I].Caption) = Countries[J] then
        // Checking the checkbox is a correct answer, if its label is a country listed for the actual lake
        YN := '1';
    end;
    CountriesYN += YN;
  end;
  Result := CountriesYN;
end;

{ Get user answer for lake countries quiz }

function GetCountryUserAnswer(var CountryBoxes: array of TCheckbox): string;

// The answer will be a 5-character string, each character corresponding to one of the
// 5 country checkboxes and set to "1" if the checkbox is checked, or to "0", if not

var
  I: Integer;
  CountriesYN: string;

begin
  CountriesYN := '';
  for I := 0 to 4 do begin
    if CountryBoxes[I].Checked then
      CountriesYN += '1'
    else
      CountriesYN += '0';
  end;
  Result := CountriesYN;
end;

{*********}
{ TfLakes }
{*********}

{ Application start: Initialisation }

procedure TfLakes.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create arrays with labels, comboboxes and checkboxes
  laQuizQuestions[0]  := laRegion; laQuizQuestions[1]  := laArea;  laQuizQuestions[2]  := laLake2;
  cobQuizQuestions[0] := cobRegion; cobQuizQuestions[1] := cobArea;  cobQuizQuestions[2] := cobLakes;
  cbCountries[0] := cbCountry1; cbCountries[1] := cbCountry2; cbCountries[2] := cbCountry3;
  cbCountries[3] := cbCountry4; cbCountries[4] := cbCountry5;
  // Re-arrange the controls on the form
  // In fact, the form presented to the user is completely different to the one initially created in Lazarus
  // I opted for this way to do, in order to make all controls visible and easily accessible to the programmer
  // and then, with the application start, moving them to the position, where they should appear in the GUI
  for I := 0 to 2 do begin
    laQuizQuestions[I].Top  := laLake.Top;
    cobQuizQuestions[I].Top := edLake.Top;
  end;
  laCountry.Top := laLake.Top; edCountry.Top := edLake.Top;
  for I := 0 to 4 do begin
    cbCountries[I].Left := 10 + I * 175; cbCountries[I].Top := 152;
  end;
  edEval.Top := 190; btQuestion.Top := 232;
  fLakes.Height := 285;
  // Read quiz data from text files
  ReadLakes(aLakes, aCountries);
  // Set application startup values
  iQuestionsTemp := 20;
  sAreaUnitTemp := 'km²';
  bRussianTemp := True;
  // Start random number generator
  Randomize;
  // Prepare for a "lake region" quiz
  mQuizRegion.Click;
end;

{ Menu item "Quiz > Lake region": Prepare for a "lake region" quiz }

procedure TfLakes.mQuizRegionClick(Sender: TObject);

begin
  iQuiz := 0;
  NewQuiz(iQuiz, iQuestionsTemp, sAreaUnitTemp, bRussianTemp, aLakes, -1, laQuizQuestions, cobQuizQuestions,
    cbCountries, iQuestions, sAreaUnit, bRussian);
end;

{ Menu item "Quiz > Lake contries": Prepare for a "lake countries" quiz }

procedure TfLakes.mQuizCountriesClick(Sender: TObject);

begin
  iQuiz := 1;
  NewQuiz(iQuiz, iQuestionsTemp, sAreaUnitTemp, bRussianTemp, aLakes, -1, laQuizQuestions, cobQuizQuestions,
    cbCountries, iQuestions, sAreaUnit, bRussian);
end;

{ Menu item "Quiz > Lake areas": Prepare for a "lake surface areas" quiz }

procedure TfLakes.mQuizAreaClick(Sender: TObject);

begin
  iQuiz := 2;
  NewQuiz(iQuiz, iQuestionsTemp, sAreaUnitTemp, bRussianTemp, aLakes, -1, laQuizQuestions, cobQuizQuestions,
    cbCountries, iQuestions, sAreaUnit, bRussian);
end;

{ Menu item "Quiz > Largest lakes": Prepare for a "country's largest lakes" quiz }

procedure TfLakes.mQuizLargestClick(Sender: TObject);

begin
  iQuiz := 3;
  NewQuiz(iQuiz, iQuestionsTemp, sAreaUnitTemp, bRussianTemp, aLakes, Length(aCountries), laQuizQuestions, cobQuizQuestions,
    cbCountries, iQuestions, sAreaUnit, bRussian);
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfLakes.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User entry of number of quiz questions }

procedure TfLakes.mOptionsQuestionsClick(Sender: TObject);

// The value entered by the user will, as all other options, only become active when a new quiz is started
// This number may have to be adapted, if the number of lakes (resp. countries for quiz 4) actually available is less than the value entered
// This is actually done in the "NewQuiz" procedure

var
  S: string;

begin
  S := InputBox('Geography quiz', 'Number of questions', IntToStr(iQuestionsTemp));
  if S <> '' then begin
    iQuestionsTemp := StrToInt(S);
    if iQuestionsTemp < 10 then                                                               // arbitrarily fixed minimum
      iQuestionsTemp := 10;
  end;
end;

{ Menu item "Options > Include Russian lakes": Toggle include/exclude Russian lakes }

procedure TfLakes.mOptionsRussiaClick(Sender: TObject);

begin
  if mOptionsRussia.Checked then
    mOptionsRussia.Checked := False
  else
    mOptionsRussia.Checked := True;
  bRussianTemp := mOptionsRussia.Checked;
end;

{ Menu item "Options > Lake areas in mi²": Toggle lake surface areas displayed in km²/mi² }

procedure TfLakes.mOptionsMilesClick(Sender: TObject);

begin
  if mOptionsMiles.Checked then begin
    mOptionsMiles.Checked := False;
    sAreaUnitTemp := 'km²';
  end
  else begin
    mOptionsMiles.Checked := True;
    sAreaUnitTemp := 'mi²';
  end;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfLakes.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfLakes.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Geography quiz:' + LineEnding;
  S += 'Lakes of Europe.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2022.';
  MessageDlg('About "EuroLakes"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfLakes.btQuestionClick(Sender: TObject);

var
  Count, LX, CX, I, J: Integer;
  UAnswer, Countries: string;

begin
  // Button "Start/Question": Generate new question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Some stuff to do at quiz start
    if btQuestion.Caption = 'Start' then begin
      // Reset variables and form controls
      iQuestion := 0; iCorrect := 0;
      if iQuiz = 3 then
        SetLength(aQuestionsDone, Length(aCountries))
      else
        SetLength(aQuestionsDone, Length(aLakes));
      for I := 0 to Length(aQuestionsDone) - 1 do begin
        aQuestionsDone[I] := False;
        if iQuiz <> 3 then begin
          // This excludes Russian lakes from being included in the quiz, if user selected to do so
          if not bRussian and (aLakes[I].LRegion = 'Russia') then
            aQuestionsDone[I] := True;
        end;
      end;
      laEval.Caption := 'Evaluation:';
      for I := 0 to 3 do
        sgEval.Cells[1, I] := '';
    end;
    // Proceed, if there are still questions left
    if iQuestion < iQuestions then begin
      Inc(iQuestion);
      laEval.Caption := 'Evaluation (' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + '):';
      edEval.Text := ''; edEval.Color := clDefault;
      for I := 0 to 4 do
        cbCountries[I].Checked := False;
      // "Largest lake" quiz (given: a country; to find: lake)
      if iQuiz = 3 then begin
        // Repeat getting a random country, until a "not done" one has been found
        repeat
          CX := Random(Length(aCountries));
        until not aQuestionsDone[CX];
        aQuestionsDone[CX] := True;                                                           // mark country as "done"
        // Display contry name
        edCountry.Text := aCountries[CX];
        // Determine answer of actual question (= get largest lake for this country)
        for I := 0 to Length(aLakes) - 1 do begin
          for J := 0 to Length(aLakes[I].LCountriesLargest) - 1 do begin
            if aCountries[CX] = aLakes[I].LCountriesLargest[J] then begin
              LX := I;
              sAnswer := aLakes[LX].LName;
            end;
          end;
        end;
        // Fill-in the lakes combobox (as proposed user answers for actual country)
        FillLakes(LX, aLakes);
      end
      // Other quizzes (given: a lake; to find: region, countries, area)
      else begin
        // Repeat getting a random lake, until a "not done" one has been found
        repeat
          LX := Random(Length(aLakes));
        until not aQuestionsDone[LX];
        aQuestionsDone[LX] := True;                                                           // mark lake as "done"
        // Display lake name
        edLake.Text := aLakes[LX].LName;
        // Determine answer of actual question (depending on kind of quiz)
        case iQuiz of
          0: sAnswer := aLakes[LX].LRegion;
          1: begin
            FillCountries(aCountries, aLakes[LX].LCountries, cbCountries);                    // display 5 countries as answer proposals
            sAnswer := GetCountryQuizAnswer(aLakes[LX].LCountries, cbCountries);              // determine correct answer for actual question and answer proposals
          end;
          2: begin
            if sAreaUnit = 'km²' then
              sAnswer := GetAreaInterval(sAreaUnit, aLakes[LX].LAreaKm2)
            else
              sAnswer := GetAreaInterval(sAreaUnit, aLakes[LX].LAreaMi2);
          end;
        end;
      end;
      sAnswer2 := sAnswer;
      if (iQuiz = 0) and (Length(aLakes[LX].LCountries) = 2) and (aLakes[LX].LCountries[1] = 'Russia') then
        // This allows to evaluate as correct answer both "East Europe" and "Russia" for lakes belonging to these two regions
        sAnswer2 := 'Russia';
      btQuestion.Caption := 'Answer';
    end;
  end
  // Button "Answer": Check user answer
  else begin
    if iQuestion <= iQuestions then begin
      // Get user answer from form (depending on actual quiz)
      case iQuiz of
        0: UAnswer := cobRegion.Text;
        1: UAnswer := GetCountryUserAnswer(cbCountries);                                      // determine user answer for actual question and contry answer proposals
        2: UAnswer := cobArea.Text;
        3: UAnswer := cobLakes.Text;
      end;
      // Check answer and display evaluation message
      if (UAnswer = sAnswer) or (UAnswer = sAnswer2) then begin
        // Correct answer
        edEval.Text := 'This is correct!'; edEval.Color := clLime;
        Inc(iCorrect);
      end
      else begin
        // False answer: Display the correct one
        edEval.Text := 'False! Correct answer = ';
        if iQuiz = 1 then begin
          // "Lake countries" quiz: Transform the "0/1" answer string (cf. function GetCountryQuizAnswer) to a readable answer with country names
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
        else begin
          // Other quizzes: Simply display correct answer(s)
          edEval.Text := edEval.Text + sAnswer;
          if sAnswer2 <> sAnswer then
            // Cases with two correct answers
            edEval.Text := edEval.Text + ' or ' + sAnswer2;
        end;
        edEval.Color := clRed;
      end;
      //  Update evaluation counters
      sgEval.Cells[1, 0] := GFormat(iQuestion, '');
      sgEval.Cells[1, 1] := GFormat(iCorrect, '');
      sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
      sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
      // If all questions have been done, terminate the quiz
      if iQuestion = iQuestions then begin
        MessageDlg('Geography quiz', 'All questions have been done. Quiz over.', mtInformation, [mbOK], 0);
        btQuestion.Caption := 'Start';
      end
      // If there are questions left, continue with the next one
      else
        btQuestion.Caption := 'Question';
    end;
  end;
end;

end.

