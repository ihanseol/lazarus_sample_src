{*************************************}
{* Main unit for USAQuiz application *}
{*************************************}

unit usquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, usflagseals;

const
  NInhGroups = 15;
  NAccGroups = 21;

type
  TArrayN    = array[1..NStates] of Integer;
  TArrayR    = array[1..NStates] of Real;
  TArrayS    = array[1..NStates] of string;
  TInhGroups = array[1..NInhGroups] of string;
  TAccGroups = array[1..NAccGroups] of string;
  TInhLimits = array[0..NInhGroups] of Real;
  TAccLimits = array[0..NAccGroups] of Real;
  {**********}
  { TfUSQuiz }
  {**********}
  TfUSQuiz  = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizNew, mQuizExit: TMenuItem;
    mSelect, mSelCapitalsList, mSelCapitalsManual: TMenuItem;
    mSelInhabitants, mSelAccession, mSelFlags, mSelSeals: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Label1, Label2, Label3, Label4, laQuizItem: TLabel;
    edState, edQuizItem, edEval: TEdit;
    cobQuizItem: TComboBox;
    edQuestions, edCorrect, edSuccess: TEdit;
    btQuestion: TButton;
    btFlagSeals: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizNewClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mSelCapitalsListClick(Sender: TObject);
    procedure mSelCapitalsManualClick(Sender: TObject);
    procedure mSelInhabitantsClick(Sender: TObject);
    procedure mSelAccessionClick(Sender: TObject);
    procedure mSelFlagsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btFlagSealsClick(Sender: TObject);
    procedure mSelSealsClick(Sender: TObject);
  private
    iQuizSelection, iQuestion, iCorrect, iState: Integer;
    aCapitalsList, aInhabitants, aAccessions: TArrayS;
    aInhabitantsList: TInhGroups;
    aAccessionList: TAccGroups;
    aStatesDone: array[1..NStates] of Boolean;
  end;

const
  States: TArrayS = (
    'Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia',
    'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland',
    'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey',
    'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina',
    'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming'
  );
  Capitals: TArrayS = (
    'Montgomery', 'Juneau', 'Phoenix', 'Little Rock', 'Sacramento', 'Denver', 'Hartford', 'Dover', 'Tallahassee', 'Atlanta',
    'Honolulu', 'Boise', 'Springfield', 'Indianapolis', 'Des Moines', 'Topeka', 'Frankfort', 'Baton Rouge', 'Augusta', 'Annapolis',
    'Boston', 'Lansing', 'Saint Paul', 'Jackson', 'Jefferson City', 'Helena', 'Lincoln', 'Carson City', 'Concord', 'Trenton',
    'Santa Fe', 'Albany', 'Raleigh', 'Bismarck', 'Columbus', 'Oklahoma City', 'Salem', 'Harrisburg', 'Providence', 'Columbia',
    'Pierre', 'Nashville', 'Austin', 'Salt Lake City', 'Montpelier', 'Richmond', 'Olympia', 'Charleston', 'Madison', 'Cheyenne'
  );
  Inhabitants: TArrayR = (  // Estimated population 2016 (in million)
    4.863, 0.742, 6.931, 2.938, 39.250, 5.541, 3.576, 0.952, 20.612, 10.311,
    1.429, 1.683, 12.802, 6.633, 3.135, 2.907, 4.437, 4.682, 1.331, 6.016,
    6.812, 9.928, 5.520, 2.989, 6.093, 1.043, 1.907, 2.940, 1.335, 8.944,
    2.081, 19.745, 10.147, 0.757, 11.614, 3.923, 4.093, 12.784, 1.052, 4.961,
    0.865, 6.651, 27.863, 3.051, 0.625, 8.412, 7.288, 1.831, 5.779, 586
  );
  Accessions: TArrayN = (
    1819, 1959, 1912, 1836, 1850, 1876, 1788, 1787, 1845, 1788,
    1959, 1890, 1818, 1816, 1846, 1861, 1792, 1812, 1820, 1788,
    1788, 1837, 1858, 1817, 1821, 1889, 1867, 1864, 1788, 1787,
    1912, 1788, 1789, 1889, 1803, 1907, 1859, 1787, 1790, 1788,
    1889, 1796, 1845, 1896, 1791, 1788, 1889, 1863, 1848, 1890
  );
  InhabitantsGroups: TInhGroups = (
    'less than 1 million', '  1 -   2 million', '  2 -   3 million', '  3 -   4 million', '  4 -   5 million', '  5 -   6 million', '  6 -   7 million',
    '  7 -   8 million', '  8 -   9 million', '  9 - 10 million', '10 - 11 million', '11 - 12 million', '12 - 15 million', '15 - 20 million', 'over 20 million');
  InhabitantsLimits: TInhLimits = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 20, 50);
  AccessionGroups: TAccGroups = (
    '1787', '1788', '1789', '1790-1792', '1796', '1803', '1812', '1816-1819', '1820-1821', '1836-1837', '1845-1846',
    '1848-1850', '1858-1861', '1863-1867', '1876', '1889', '1890', '1896', '1907', '1912', '1959'
  );
  AccessionLimits: TAccLimits = (
    0, 1787, 1788, 1789, 1792, 1796, 1803, 1812, 1819, 1821, 1837, 1846,
    1850, 1861, 1867, 1876, 1889, 1890, 1896, 1907, 1912, 1959
  );

var
  fUSQuiz: TfUSQuiz;

implementation

{$R *.lfm}

{ Reset the form controls }

procedure ClearForm(var NQ, NC: Integer);

begin
  // Set questions and correct answers to 0
  NQ := 0; NC := 0;
  // Reset the form controls
  fUSQuiz.edState.Text := ''; fUSQuiz.edQuizItem.Text := ''; fUSQuiz.cobQuizItem.ItemIndex := -1; fUSQuiz.edEval.Text := '';
  fUSQuiz.edQuestions.Text := ''; fUSQuiz.edCorrect.Text := '';
  fUSQuiz.edSuccess.Text := '';  fUSQuiz.edSuccess.Color := clDefault;
  // Reset menus and buttons
  fUSQuiz.mSelect.Enabled := True;                                             // enable quiz selections
  fUSQuiz.btQuestion.Caption := 'Question';
  fUSQuiz.btQuestion.Enabled := True;
end;

{ Quiz selection: Display edit field or selection combobox }

procedure QuizSelection(Selection: Integer; var QCapitals: TArrayS; var QAccessions: TAccGroups; var QInhabitants: TInhGroups);

const
  QuizSelections1: array[1..6] of string = (
    'Capitals', 'Capitals', 'Inhabitants', 'Accession years', 'State flags', 'State seals');
  QuizSelections2: array[1..6] of string = (
    'Capital', 'Capital', 'Inhabitants', 'Accession', 'State flag', 'State seal');

var
  I: Integer;

begin
  fUSQuiz.stTitle.Caption := 'USA Quiz : ' + QuizSelections1[Selection] + '.';
  fUSQuiz.laQuizItem.Caption := QuizSelections2[Selection];
  // Capitals direct entry or flags/seals
  if (Selection = 2) or (Selection = 5) or (Selection = 6) then begin
    // User answer will be retrieved from edit field
    fUSQuiz.edQuizItem.Visible := True;                                        // edit field for manual answer (or auto-fill-in for flags/seals)
    fUSQuiz.edQuizItem.Text := '';
    fUSQuiz.cobQuizItem.Visible := False;
    // Do or do not display the button to select the flag/seal
    if Selection = 2 then
      fUSQuiz.btFlagSeals.Visible := False
    else begin
      fUSQuiz.btFlagSeals.Visible := True; fUSQuiz.btFlagSeals.Enabled := True;
      if Selection = 5 then
        fUSQuiz.btFlagSeals.Caption := 'Flag'
      else
        fUSQuiz.btFlagSeals.Caption := 'Seal';
    end;
  end
  // Capitals, inhabitants or accession year selection from list
  else begin
    fUSQuiz.edQuizItem.Visible := False;
    fUSQuiz.btFlagSeals.Visible := False;
    fUSQuiz.cobQuizItem.Visible := True;                                       // combobox for the list selection answer
    fUSQuiz.cobQuizItem.Clear;
    // Fill combobox with items for actual quiz
    case Selection of
      1: begin
           for I := 1 to NStates do
             fUSQuiz.cobQuizItem.Items.Append(QCapitals[I]);                   // capitals list
         end;
      3: begin
           for I := 1 to NInhGroups do
             fUSQuiz.cobQuizItem.Items.Append(QInhabitants[I]);                // inhabitant groups list
         end;
      4: begin
           for I := 1 to NAccGroups do
             fUSQuiz.cobQuizItem.Items.Append(QAccessions[I]);                 // accession-year groups list
         end;
    end;
  end;
  fUSQuiz.btQuestion.Caption := 'Question';
end;

{ Get quiz answer (depending on quiz selected) }

procedure QuizAnswer(Quiz, State: Integer; var Capitals, Accessions, Inhabitants: TArrayS; var Answer: string);

begin
  Answer := '';
  case Quiz of
    1, 2: Answer := Capitals[State];
       3: Answer := Inhabitants[State];
       4: Answer := Accessions[State];
    5, 6: Answer := IntToStr(State) + '.jpg';
  end;
end;

{**********}
{ TfUSQuiz }
{**********}

{ Application start: Lists creation and other initialisations }

procedure TfUSQuiz.FormCreate(Sender: TObject);

var
  I, J: Integer;
  STemp: string;

begin
  // Sorted capitals list (for combobox)
  aCapitalsList := Capitals;
  for I := 1 to NStates - 1 do begin
    for J := I to NStates do begin
      if aCapitalsList[J] < aCapitalsList[I] then begin
        STemp := aCapitalsList[I]; aCapitalsList[I] := aCapitalsList[J]; aCapitalsList[J] := STemp;
      end;
    end;
  end;
  // Group-like inhabitants lists for answer-check (state index) and for combobox (the different groups)
  aInhabitantsList := InhabitantsGroups;
  for I := 1 to NStates do begin
    for J := 1 to NInhGroups do begin
      if (Inhabitants[I] >= InhabitantsLimits[J - 1]) and (Inhabitants[I] <= InhabitantsLimits[J]) then
       aInhabitants[I] := InhabitantsGroups[J];
    end;
  end;
  // Group-like accession years lists for answer-check (state index) and for combobox (the different groups)
  aAccessionList := AccessionGroups;
  for I := 1 to NStates do begin
    for J := 1 to NAccGroups do begin
      if (Accessions[I] > AccessionLimits[J - 1]) and (Accessions[I] <= AccessionLimits[J]) then
       aAccessions[I] := AccessionGroups[J];
    end;
  end;
  // Start random number generator
  Randomize;
  // Start default quiz (capitals from list)
  iQuizSelection := 1;
  QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > New": Reset form controls }

procedure TfUSQuiz.mQuizNewClick(Sender: TObject);

begin
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfUSQuiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Selection > Capitals (list)": Capitals quiz (selection from list) }

procedure TfUSQuiz.mSelCapitalsListClick(Sender: TObject);

begin
  if not mSelCapitalsList.Checked then begin
    mSelCapitalsList.Checked := True; mSelCapitalsManual.Checked    := False; mSelInhabitants.Checked := False;
    mSelAccession.Checked := False; mSelFlags.Checked := False; mSelSeals.Checked := False;
    iQuizSelection := 1;
    QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  end;
end;

{ Menu item "Selection > Capitals (manual)": Capitals quiz (manual entry) }

procedure TfUSQuiz.mSelCapitalsManualClick(Sender: TObject);

begin
  if not mSelCapitalsManual.Checked then begin
    mSelCapitalsManual.Checked := True; mSelCapitalsList.Checked := False; mSelInhabitants.Checked := False;
    mSelAccession.Checked := False; mSelFlags.Checked := False; mSelSeals.Checked := False;
    iQuizSelection := 2;
    QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  end;
end;

{ Menu item "Selection > Inhabitants": Inhabitants quiz (selection from group-like list) }

procedure TfUSQuiz.mSelInhabitantsClick(Sender: TObject);

begin
  if not mSelInhabitants.Checked then begin
    mSelInhabitants.Checked := True;  mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked    := False;
    mSelAccession.Checked := False; mSelFlags.Checked := False; mSelSeals.Checked := False;
    iQuizSelection := 3;
    QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  end;
end;

{ Menu item "Selection > Accession years": Accession years quiz (selection from group-like list) }

procedure TfUSQuiz.mSelAccessionClick(Sender: TObject);

begin
  if not mSelAccession.Checked then begin
    mSelAccession.Checked := True; mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False;
    mSelInhabitants.Checked := False; mSelFlags.Checked := False; mSelSeals.Checked := False;
    iQuizSelection := 4;
    QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  end;
end;

{ Menu item "Selection > State flags": Flag quiz (flag image selection) }

procedure TfUSQuiz.mSelFlagsClick(Sender: TObject);

begin
  if not mSelFlags.Checked then begin
    mSelFlags.Checked := True; mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False;
    mSelInhabitants.Checked := False; mSelAccession.Checked := False; mSelSeals.Checked := False;
    iQuizSelection := 5;
    QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  end;
end;

{ Menu item "Selection > State seals": Seals quiz (seal image selection) }

procedure TfUSQuiz.mSelSealsClick(Sender: TObject);

begin
  if not mSelSeals.Checked then begin
    mSelSeals.Checked := True; mSelFlags.Checked := False; mSelCapitalsList.Checked := False;
    mSelCapitalsManual.Checked := False; mSelInhabitants.Checked := False; mSelAccession.Checked := False;
    iQuizSelection := 6;
    QuizSelection(iQuizSelection, aCapitalsList, aAccessionList, aInhabitantsList);
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfUSQuiz.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'United States of America quiz:' + LineEnding;
  S += 'State capitals, inhabitants, USA accession years, state flags and seals.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, May - August 2020.';
  MessageDlg('About "USAQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Do the quiz (display question or check user's answer) }

procedure TfUSQuiz.btQuestionClick(Sender: TObject);

var
  Percent, I: Integer;
  CorrectAnswer, UserAnswer: string;

begin
  // Button "Question": Display the quiz question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    // Not yet all states done: continue the quiz
    if iQuestion <= NStates then begin
      if iQuestion = 1 then begin
        for I := 1 to NStates do
          aStatesDone[I] := False;                                             // at start of quiz, set all states to "not done"
        mSelect.Enabled := False;                                              // disable the quiz selection menu
      end;
      // Get a random state
      repeat
        iState := Random(NStates) + 1;
      until not aStatesDone[iState];                                           // the random state must not be already done
      aStatesDone[iState] := True;                                             // mark the state as "done"
      // Display state and clear all answer fields
      edState.Text := States[iState];
      case iQuizSelection of
        1, 3, 4: cobQuizItem.Text := '';
        2, 5, 6: edQuizItem.Text  := '';
      end;
      edEval.Text := '';
      btQuestion.Caption := 'Answer';                                          // next button action will be to check user's answer
      btFlagSeals.Enabled := True;                                             // (re)give user access to "Flag/Seal" button
    end;
  end
  // Button "Answer": Check user answer
  else begin
    // Get user answer from form
    case iQuizSelection of
      1, 3, 4: UserAnswer := cobQuizItem.Text;                                 // answer retrieved from edit field
      2, 5, 6: UserAnswer := edQuizItem.Text;                                  // answer retrieved from combobox
    end;
    // Get answer for actual quiz and actual state
    QuizAnswer(iQuizSelection, iState, Capitals, aAccessions, aInhabitants, CorrectAnswer);
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
    // All states done: Terminate the quiz
    if iQuestion = NStates then begin
      MessageDlg('USA quiz','All ' + IntToStr(NStates) + ' states done. End of quiz.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // disable button (until next "New" command)
    end;
  end;
end;

{ Button "Flag/Seal": Open second form to select a flag/seal image }

procedure TfUSQuiz.btFlagSealsClick(Sender: TObject);

begin
  if iQuizSelection = 5 then
    fUSAFlagSeals.Caption := 'USA Flags'
  else
    fUSAFlagSeals.Caption := 'USA Seals';
  fUSAFlagSeals.iQuiz := iQuizSelection;
  fUSAFlagSeals.ShowModal;
  if fUSAFlagSeals.iState > 0 then
    edQuizItem.Text := IntToStr(fUSAFlagSeals.iState) + '.jpg';                    // use flag/seal filename (= states index) as user answer
  btFlagSeals.Enabled := False;                                                    // disable button (to avoid that user chooses another flag/seal)
end;

end.

