{***************************************}
{* Main unit for ChinaQuiz application *}
{***************************************}

unit quiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

const
  NProvinces   = 22;
  NPAreaGroups = 10;
  NPPopGroups  = 10;
  NPDensGroups = 10;
  NCPopGroups  = 12;

type
  TArrayN      = array[1..NProvinces] of LongInt;
  TArrayR      = array[1..NProvinces] of Real;
  TArrayS      = array[1..NProvinces] of string;
  TArrayS3     = array[1..NProvinces, 1..3] of string;
  TPAreaGroups = array[1..NPAreaGroups] of string;
  TPPopGroups  = array[1..NPPopGroups] of string;
  TPDensGroups = array[1..NPDensGroups] of string;
  TCPopGroups  = array[1..NCPopGroups] of string;
  TPAreaLimits = array[0..NPAreaGroups] of Real;
  TPPopLimits  = array[0..NPPopGroups] of Real;
  TPDensLimits = array[0..NPDensGroups] of Real;
  TCPopLimits  = array[0..NCPopGroups] of Real;
  {*************}
  { TfChinaQuiz }
  {*************}
  TfChinaQuiz  = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizNew, mQuizExit: TMenuItem;
    mSelect, mSelCapitalsList, mSelCapitalsManual, mSelPArea, mSelPPopulation, mSelPDensity, mSelCPopulation: TMenuItem;
    mOptions, mOptionsPNames, mOptionsPNames1, mOptionsPNames2, mOptionsPNames3: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laQuestionItem, Label2, Label3, Label4, laAnswerItem: TLabel;
    edQuestion, edAnswer, edEval: TEdit;
    cobAnswers: TComboBox;
    edQuestions, edCorrect, edSuccess: TEdit;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizNewClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mSelCapitalsListClick(Sender: TObject);
    procedure mSelCapitalsManualClick(Sender: TObject);
    procedure mSelPAreaClick(Sender: TObject);
    procedure mSelPDensityClick(Sender: TObject);
    procedure mSelPPopulationClick(Sender: TObject);
    procedure mSelCPopulationClick(Sender: TObject);
    procedure mOptionsPNames1Click(Sender: TObject);
    procedure mOptionsPNames2Click(Sender: TObject);
    procedure mOptionsPNames3Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuizSelection, iQuestion, iCorrect, iProvince: Integer;
    aCapitalsList, aPAreas, aPPopulations, aPDensities, aCPopulations: TArrayS;
    aPAreasList: TPAreaGroups;
    aPPopulationsList: TPPopGroups;
    aPDensitiesList: TPDensGroups;
    aCPopulationsList: TCPopGroups;
    aProvincesDone: array[1..NProvinces] of Boolean;
  end;

const
  Provinces: TArrayS3 = (
    ('Anhui', '安徽省', 'Ānhuī Shěng'),
    ('Fujian', '福建省', 'Fújiàn Shěng'),
    ('Guangdong', '广东省', 'Guǎngdōng Shěng'),
    ('Gansu', '甘肃省', 'Gānsù Shěng'),
    ('Guizhou', '贵州省', 'Guìzhōu Shěng'),
    ('Henan', '河南省', 'Hénán Shěng'),
    ('Hubei', '湖北省', 'Húběi Shěng'),
    ('Hebei', '河北省', 'Héběi Shěng'),
    ('Hainan', '海南省', 'Hǎinán Shěng'),
    ('Heilongjiang', '黑龙江省', 'Hēilóngjiāng Shěng'),
    ('Hunan', '湖南省', 'Húnán Shěng'),
    ('Jilin', '吉林省', 'Jílín Shěng'),
    ('Jiangsu', '江苏省', 'Jiāngsū Shěng'),
    ('Jiangxi', '江西省', 'Jiāngxī Shěng'),
    ('Liaoning', '辽宁省', 'Liáoníng Shěng'),
    ('Qinghai', '青海省', 'Qīnghǎi Shěng'),
    ('Sichuan', '四川省', 'Sìchuān Shěng'),
    ('Shandong', '山东省', 'Shāndōng Shěng'),
    ('Shaanxi', '陕西省', 'Shǎnxī Shěng'),
    ('Shanxi', '山西省', 'Shānxī Shěng'),
    ('Yunnan', '云南省', 'Yúnnán Shěng'),
    ('Zhejiang', '浙江省', 'Zhèjiāng Shěng')
  );
  Capitals: TArrayS = (
    'Hefei', 'Fuzhou', 'Guangzhou', 'Lanzhou', 'Guiyang', 'Zhengzhou', 'Wuhan',
    'Shijiazhuang', 'Haikou', 'Harbin', 'Changsha', 'Changchun', 'Nanjing', 'Nanchang',
    'Shenyang', 'Xining', 'Chengdu', 'Jinan', 'Xi''an', 'Taiyuan', 'Kunming', 'Hangzhou'
  );
  // Province area (km²)
  PAreas: TArrayN = (
    139879, 123756, 180013, 457382, 176140, 165467, 185776, 189809,
    34259, 472766, 211842, 190282, 99949, 166939, 147076, 690355,
    484056, 157704, 205624, 156713, 383195, 104873
  );
  // Province population (2020)
  PPopulations: TArrayN = (
    61027171, 41540086, 126012510, 25019831, 38562148, 99365519, 57752557,
    74610235, 10081232, 31850088, 66444864, 24073453, 84748016, 45188635,
    42591407, 5923957, 83674866, 101527453, 39528999, 34915616, 47209277, 64567588
  );
  // Province density (per km²; 2020)
  PDensities: TArrayR = (
    436.29, 335.66, 700.02, 54.70, 218.93, 600.52, 310.87, 393.08, 294.27, 67.37, 313.65,
    126.51, 847.91, 270.69, 289.59, 8.58, 174.93, 643.78, 192.24, 222.80, 123.20, 615.67
  );
  // Capital population (urban population 2020)
  CPopulations: TArrayN = (
    5118199, 4094491, 16492590, 3474858, 4506134, 6650532, 9500000, 5758403,
    2250000, 6976136, 5980707, 5691024, 9314685, 3929660, 7885142, 1954795,
    15419445, 8352574, 11904805, 4529141, 5950578, 10711238
  );
  PAreaGroups: TPAreaGroups = (
    'less than 100000 km²', '100,000 - 150,000 km²', '150,000 - 200,000 km²', '200,000 - 250,000 km²', '250,000 - 300,000 km²',
    '300,000 - 350,000 km²', '350,000 - 400,000 km²', '400,000 - 450,000 km²', '450,000 - 500,000 km²', 'over 500,000 km²'
  );
  PAreaLimits: TPAreaLimits = (
    0, 100, 150, 200, 250, 300, 350, 400, 450, 500, 1000
  );
  PPopGroups: TPPopGroups = (
    'less than 20 millions', '20 - 30 millions', '30 - 40 millions', '40 - 50 millions', '50 - 60 millions',
    '60 - 70 millions', '70 - 80 millions', '80 - 90 millions', '90 - 100 millions', 'over 100 millions'
  );
  PPopLimits: TPPopLimits = (
    0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200
  );
  PDensGroups: TPDensGroups = (
    'less than 100 per km²', '100 - 150 per km²', '150 - 200 per km²', '200 - 250 per km²', '250 - 300 per km²',
    '300 - 350 per km²', '350 - 400 per km²', '400 - 450 per km²', '450 - 500 per km²', 'over 500 per km²'
  );
  PDensLimits: TPDensLimits = (
    0, 100, 150, 200, 250, 300, 350, 400, 450, 500, 1000
  );
  CPopGroups: TCPopGroups = (
    'less than 1 million', ' 1 - 2 millions', ' 2 - 3 millions', ' 3 - 4 millions', ' 4 - 5 millions', ' 5 - 6 millions',
    ' 6 - 7 millions', ' 7 - 8 millions', ' 8 - 9 millions', ' 9 - 10 millions', '10 - 15 millions', 'over 15 millions'
  );
  CPopLimits: TCPopLimits = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 30
  );

var
  fChinaQuiz: TfChinaQuiz;

implementation

{$R *.lfm}

{ Reset the form controls }

procedure ClearForm(out NQ, NC: Integer);

begin
  // Set questions and correct answers to 0
  NQ := 0; NC := 0;
  // Reset the form controls
  fChinaQuiz.edQuestion.Text := ''; fChinaQuiz.edAnswer.Text := '';
  fChinaQuiz.cobAnswers.ItemIndex := -1; fChinaQuiz.edEval.Text := '';
  fChinaQuiz.edQuestions.Text := ''; fChinaQuiz.edCorrect.Text := '';
  fChinaQuiz.edSuccess.Text := '';  fChinaQuiz.edSuccess.Color := clDefault;
  // Reset menus and buttons
  fChinaQuiz.mSelect.Enabled := True;                                          // enable quiz selections
  fChinaQuiz.btQuestion.Caption := 'Question';
  fChinaQuiz.btQuestion.Enabled := True;
end;

{ Quiz selection: Set form fields for the quiz actuylly selected }

procedure QuizSelection(Selection: Integer; var QCapitals: TArrayS; var QPAreas: TPAreaGroups; var QPPopulations: TPPopGroups;
  var QPDensities: TPDensGroups; var QCPopulations: TCPopGroups);

const
  QuizSelections1: array[1..6] of string = (
    'Capitals', 'Capitals', 'Province areas', 'Province populations', 'Province population densities', 'Capital urban populations');
  QuizSelections2: array[1..6] of string = (
    'Capital', 'Capital', 'Area', 'Population', 'Density', 'Population');

var
  I: Integer;

begin
  fChinaQuiz.stTitle.Caption := 'China Quiz: ' + QuizSelections1[Selection] + '.';
  if Selection = 6 then begin
    // For this quiz, the capital name is given (always English)
    fChinaQuiz.laQuestionItem.Caption := 'Capital';
    fChinaQuiz.mOptionsPNames.Enabled := False;
  end
  else begin
    // For these quizes, the province name is given (language may be selected)
    fChinaQuiz.laQuestionItem.Caption := 'Province';
    fChinaQuiz.mOptionsPNames.Enabled := True;
  end;
  fChinaQuiz.laAnswerItem.Caption := QuizSelections2[Selection];
  // Capitals direct entry
  if Selection = 2 then begin
    // User answer will be retrieved from edit field
    fChinaQuiz.edAnswer.Visible := True;                                       // edit field for manual answer
    fChinaQuiz.edAnswer.Text := '';
    fChinaQuiz.cobAnswers.Visible := False;
  end
  // Capitals, areas, populations or densities selection from list
  else begin
    fChinaQuiz.edAnswer.Visible := False;
    fChinaQuiz.cobAnswers.Visible := True;                                     // combobox for the list selection answer
    fChinaQuiz.cobAnswers.Clear;
    // Fill combobox with items for actual quiz
    case Selection of
      1: begin
           for I := 1 to NProvinces do
             fChinaQuiz.cobAnswers.Items.Append(QCapitals[I]);                 // capitals list
         end;
      3: begin
           for I := 1 to NPAreaGroups do
             fChinaQuiz.cobAnswers.Items.Append(QPAreas[I]);                   // province area groups list
         end;
      4: begin
           for I := 1 to NPPopGroups do
             fChinaQuiz.cobAnswers.Items.Append(QPPopulations[I]);             // province population groups list
         end;
      5: begin
           for I := 1 to NPDensGroups do
             fChinaQuiz.cobAnswers.Items.Append(QPDensities[I]);               // province density groups list
         end;
      6: begin
           for I := 1 to NCPopGroups do
             fChinaQuiz.cobAnswers.Items.Append(QCPopulations[I]);             // capital population groups list
         end;
    end;
  end;
  fChinaQuiz.btQuestion.Caption := 'Question';
end;

{ Get correct quiz answer (depending on quiz selected) }

procedure QuizAnswer(Quiz, QuizItem: Integer; var Capitals, PAreas, PPopulations, PDensities, CPopulations: TArrayS; out Answer: string);

begin
  Answer := '';
  case Quiz of
    1, 2: Answer := Capitals[QuizItem];
       3: Answer := PAreas[QuizItem];
       4: Answer := PPopulations[QuizItem];
       5: Answer := PDensities[QuizItem];
       6: Answer := CPopulations[QuizItem];
  end;
end;

{*************}
{ TfChinaQuiz }
{*************}

{ Application start: Lists creation and other initializations }

procedure TfChinaQuiz.FormCreate(Sender: TObject);

var
  I, J: Integer;
  STemp: string;

begin
  // Sorted capitals list (for combobox)
  aCapitalsList := Capitals;
  for I := 1 to NProvinces - 1 do begin
    for J := I to NProvinces do begin
      if aCapitalsList[J] < aCapitalsList[I] then begin
        STemp := aCapitalsList[I]; aCapitalsList[I] := aCapitalsList[J]; aCapitalsList[J] := STemp;
      end;
    end;
  end;
  // Group-like areas lists for answer-check (province index) and for combobox (the different groups)
  aPAreasList := PAreaGroups;
  for I := 1 to NProvinces do begin
    for J := 1 to NPAreaGroups do begin
      if (PAreas[I] >= 1E+3 * PAreaLimits[J - 1]) and (PAreas[I] <= 1E+3 * PAreaLimits[J]) then
       aPAreas[I] := PAreaGroups[J];
    end;
  end;
  // Group-like province populations lists for answer-check (province index) and for combobox (the different groups)
  aPPopulationsList := PPopGroups;
  for I := 1 to NProvinces do begin
    for J := 1 to NPPopGroups do begin
      if (PPopulations[I] >= 1E+6 * PPopLimits[J - 1]) and (PPopulations[I] <= 1E+6 * PPopLimits[J]) then
       aPPopulations[I] := PPopGroups[J];
    end;
  end;
  // Group-like densities lists for answer-check (province index) and for combobox (the different groups)
  aPDensitiesList := PDensGroups;
  for I := 1 to NProvinces do begin
    for J := 1 to NPPopGroups do begin
      if (PDensities[I] >= PDensLimits[J - 1]) and (PDensities[I] <= PDensLimits[J]) then
       aPDensities[I] := PDensGroups[J];
    end;
  end;
  // Group-like capital populations lists for answer-check (province index) and for combobox (the different groups)
  aCPopulationsList := CPopGroups;
  for I := 1 to NProvinces do begin
    for J := 1 to NCPopGroups do begin
      if (CPopulations[I] >= 1E+6 * CPopLimits[J - 1]) and (CPopulations[I] <= 1E+6 * CPopLimits[J]) then
       aCPopulations[I] := CPopGroups[J];
    end;
  end;
  // Start random number generator
  Randomize;
  // Start default quiz (capitals from list)
  iQuizSelection := 1;
  QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > New": Reset form controls }

procedure TfChinaQuiz.mQuizNewClick(Sender: TObject);

begin
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfChinaQuiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Selection > Capitals (list)": Capitals quiz (selection from list) }

procedure TfChinaQuiz.mSelCapitalsListClick(Sender: TObject);

begin
  if not mSelCapitalsList.Checked then begin
    mSelCapitalsList.Checked := True; mSelCapitalsManual.Checked := False; mSelPArea.Checked := False;
    mSelPPopulation.Checked := False; mSelPDensity.Checked := False; mSelCPopulation.Checked := False;
    iQuizSelection := 1;
    QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  end;
end;

{ Menu item "Selection > Capitals (manual)": Capitals quiz (manual entry) }

procedure TfChinaQuiz.mSelCapitalsManualClick(Sender: TObject);

begin
  if not mSelCapitalsManual.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := True; mSelPArea.Checked := False;
    mSelPPopulation.Checked := False; mSelPDensity.Checked := False; mSelCPopulation.Checked := False;
    iQuizSelection := 2;
    QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  end;
end;

{ Menu item "Selection > Province areas": Province areas quiz }

procedure TfChinaQuiz.mSelPAreaClick(Sender: TObject);

begin
  if not mSelPArea.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False; mSelPArea.Checked := True;
    mSelPPopulation.Checked := False; mSelPDensity.Checked := False; mSelCPopulation.Checked := False;
    iQuizSelection := 3;
    QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  end;
end;

{ Menu item "Selection > Province populations": Province populations quiz }

procedure TfChinaQuiz.mSelPPopulationClick(Sender: TObject);

begin
  if not mSelPPopulation.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False; mSelPArea.Checked := False;
    mSelPPopulation.Checked := True; mSelPDensity.Checked := False; mSelCPopulation.Checked := False;
    iQuizSelection := 4;
    QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  end;
end;

{ Menu item "Selection > Province densities": Province population density quiz }

procedure TfChinaQuiz.mSelPDensityClick(Sender: TObject);

begin
  if not mSelPDensity.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False; mSelPArea.Checked := False;
    mSelPPopulation.Checked := False; mSelPDensity.Checked := True; mSelCPopulation.Checked := False;
    iQuizSelection := 5;
    QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  end;
end;

{ Menu item "Selection > Capital populations": Capital urban population quiz }

procedure TfChinaQuiz.mSelCPopulationClick(Sender: TObject);

begin
  if not mSelCPopulation.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False; mSelPArea.Checked := False;
    mSelPPopulation.Checked := False; mSelPDensity.Checked := False; mSelCPopulation.Checked := True;
    iQuizSelection := 6;
    QuizSelection(iQuizSelection, aCapitalsList, aPAreasList, aPPopulationsList, aPDensitiesList, aCPopulationsList);
  end;
end;

{ Menu item "Options > Province names > ...": Select province display language }

procedure TfChinaQuiz.mOptionsPNames1Click(Sender: TObject);

begin
  mOptionsPNames1.Checked := True; mOptionsPNames2.Checked := False; mOptionsPNames3.Checked := False;
end;

procedure TfChinaQuiz.mOptionsPNames2Click(Sender: TObject);

begin
  mOptionsPNames1.Checked := False; mOptionsPNames2.Checked := True; mOptionsPNames3.Checked := False;
end;

procedure TfChinaQuiz.mOptionsPNames3Click(Sender: TObject);

begin
  mOptionsPNames1.Checked := False; mOptionsPNames2.Checked := False; mOptionsPNames3.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfChinaQuiz.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Provinces of China quiz:' + LineEnding;
  S += 'Province capitals, areas, populations, population densities, and capitals urban populations.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, July 2023.';
  MessageDlg('About "ChinaQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Do the quiz (display question resp. check user's answer) }

procedure TfChinaQuiz.btQuestionClick(Sender: TObject);

var
  Percent, I: Integer;
  CorrectAnswer, UserAnswer: string;

begin
  // Button "Question": Display the quiz question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    // Not yet all provinces done: Continue the quiz
    if iQuestion <= NProvinces then begin
      if iQuestion = 1 then begin
        for I := 1 to NProvinces do
          aProvincesDone[I] := False;                                          // at start of quiz, set all provinces to "not done"
        mSelect.Enabled := False;                                              // disable the quiz selection menu
      end;
      // Get a random province
      repeat
        iProvince := Random(NProvinces) + 1;
      until not aProvincesDone[iProvince];                                     // the random province must not be already done
      aProvincesDone[iProvince] := True;                                       // mark the state as "done"
      // Display province (or capital)
      if iQuizSelection = 6 then
        edQuestion.Text := Capitals[iProvince]
      else begin
        if mOptionsPNames1.Checked then
          edQuestion.Text := Provinces[iProvince, 1]
        else if mOptionsPNames2.Checked then
          edQuestion.Text := Provinces[iProvince, 2]
        else
          edQuestion.Text := Provinces[iProvince, 3];
      end;
      // Clear answer field
      if iQuizSelection = 2 then
        edAnswer.Text  := ''
      else
        cobAnswers.Text := '';
      edEval.Text := '';
      btQuestion.Caption := 'Answer';                                          // next button action will be to check user's answer
    end;
  end
  // Button "Answer": Check user answer
  else begin
    // Get user answer from form
    if iQuizSelection = 2 then
      UserAnswer := edAnswer.Text                                              // answer retrieved from edit field
    else
      UserAnswer := cobAnswers.Text;                                           // answer retrieved from combobox
    // Get answer for actual quiz and actual province (capital)
    QuizAnswer(iQuizSelection, iProvince, Capitals, aPAreas, aPPopulations, aPDensities, aCPopulations, CorrectAnswer);
    // Check user answer
    if UserAnswer = CorrectAnswer then begin
      // Answer is correct
      Inc(iCorrect);
      edEval.Font.Color := clLime;
      edEval.Text := 'Correct!';
      edCorrect.Text := IntToStr(iCorrect);
    end
    else begin
      // Answer is false
      edEval.Font.Color := clRed;
      edEval.Text := 'False! Correct answer = ' + CorrectAnswer;
    end;
    // Update evaluation counters
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    Percent := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Percent) + '%';
    // Success percentage with colored background, depending on value
    if Percent < 50 then
       edSuccess.Color := clRed
    else if Percent < 60 then
       edSuccess.Color := clYellow
    else
       edSuccess.Color := clLime;
    btQuestion.Caption := 'Question';                                          // next button action will be to display a question
    // All provinces done: Terminate the quiz
    if iQuestion = NProvinces then begin
      MessageDlg('China quiz','All ' + IntToStr(NProvinces) + ' provinces done. End of quiz.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // disable button (until next "New" command)
    end;
  end;
end;

end.

