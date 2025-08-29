{*********************************************}
{* Main unit for DeutschlandQuiz application *}
{*********************************************}

unit dequiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, deflags;

const
  NInhGroups  = 11;
  NAreaGroups = 11;

type
  TArrayN     = array[1..NLands] of Integer;
  TArrayR     = array[1..NLands] of Real;
  TArrayS     = array[1..NLands] of string;
  TAreaGroups = array[1..NAreaGroups] of string;
  TAreaLimits = array[0..NAreaGroups] of Real;
  TInhGroups  = array[1..NInhGroups] of string;
  TInhLimits  = array[0..NInhGroups] of Real;
  {**********}
  { TfDEQuiz }
  {**********}
  TfDEQuiz  = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizNew, mQuizExit: TMenuItem;
    mSelect, mSelCapitalsList, mSelCapitalsManual: TMenuItem;
    mSelInhabitants, mSelAreas, mSelFlags: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Label1, Label2, Label3, Label4, laQuizItem: TLabel;
    edLand, edQuizItem, edEval: TEdit;
    cobQuizItem: TComboBox;
    edQuestions, edCorrect, edSuccess: TEdit;
    btQuestion: TButton;
    btFlags: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizNewClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mSelCapitalsListClick(Sender: TObject);
    procedure mSelCapitalsManualClick(Sender: TObject);
    procedure mSelAreaClick(Sender: TObject);
    procedure mSelInhabitantsClick(Sender: TObject);
    procedure mSelFlagsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btFlagsClick(Sender: TObject);
  private
    iQuizSelection, iQuestion, iCorrect, iLand: Integer;
    aCapitalsList, aAreas, aInhabitants: TArrayS;
    aAreasList: TAreaGroups; aInhabitantsList: TInhGroups;
    aLandsDone: array[1..NLands] of Boolean;
  end;

const
  Lands: TArrayS = (
    'Baden-Württemberg', 'Bayern', 'Berlin', 'Brandenburg', 'Bremen', 'Hamburg', 'Hessen', 'Mecklenburg-Vorpommern', 'Niedersachsen',
    'Nordrhein-Westfalen', 'Rheinland-Pfalz', 'Saarland', 'Sachsen', 'Sachsen-Anhalt', 'Schleswig-Holstein', 'Thüringen'
  );
  Capitals: TArrayS = (
    'Stuttgart', 'München', 'Berlin', 'Potsdam', 'Bremen', 'Hamburg', 'Wiesbaden', 'Schwerin', 'Hannover',
    'Düsseldorf', 'Mainz', 'Saarbrücken', 'Dresden', 'Magdeburg', 'Kiel', 'Erfurt'
  );
  Areas: TArrayN = ( // in km²
    35751, 70552, 892, 29486, 419, 755, 21115, 23180, 47635, 34098, 19853, 2570, 18416, 20446, 15799, 16173
  );
  Inhabitants: TArrayR = (  // in million (2020)
    11.1, 13.1, 3.7, 2.5, 0.7, 1.8, 6.3, 1.6, 8.0, 17.9, 4.1, 1.0, 4.1, 2.2, 2.9, 2.1
  );
  AreasGroups: TAreaGroups = (
    'weniger als 500 km²', '   500 -  1.000 km²', ' 1.000 -  2.500 km²', ' 2.500 -  5.000 km²', ' 5.000 - 10.000 km²', '10.000 - 15.000 km²',
    '15.000 - 20.000 km²', '20.000 - 25.000 km²', '25.000 - 30.000 km²', '30.000 - 50.000 km²', 'über 50.000 km²');
  AreasLimits: TAreaLimits = ( // in thousands
    0, 0.5, 1, 2.5, 5, 10, 15, 20, 25, 30, 50, 100);
  InhabitantsGroups: TInhGroups = (
    'weniger als 1 Million', '1 -  2 Millionen', '2 -  3 Millionen', '3 -  4 Millionen', '4 -  5 Millionen', '5 -  6 Millionen',
    '6 -  7 Millionen', '7 -  8 Millionen', '8 -  9 Millionen', '9 - 10 Millionen', 'über 10 Millionen');
  InhabitantsLimits: TInhLimits = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20);

var
  fDEQuiz: TfDEQuiz;

implementation

{$R *.lfm}

{ Reset the form controls }

procedure ClearForm(var NQ, NC: Integer);

begin
  // Set questions and correct answers to 0
  NQ := 0; NC := 0;
  // Reset the form controls
  fDEQuiz.edLand.Text := ''; fDEQuiz.edQuizItem.Text := ''; fDEQuiz.cobQuizItem.ItemIndex := -1; fDEQuiz.edEval.Text := '';
  fDEQuiz.edQuestions.Text := ''; fDEQuiz.edCorrect.Text := '';
  fDEQuiz.edSuccess.Text := '';  fDEQuiz.edSuccess.Color := clDefault;
  // Reset menus and buttons
  fDEQuiz.mSelect.Enabled := True;                                             // enable quiz selections
  fDEQuiz.btQuestion.Caption := 'Frage';
  fDEQuiz.btQuestion.Enabled := True;
end;

{ Quiz selection: Display title and edit field or selection combobox }

procedure QuizSelection(Selection: Integer; var QCapitals: TArrayS; var QAreas: TAreaGroups; var QInhabitants: TInhGroups);

const
  QuizSelections1: array[1..5] of string = (
    'Hauptstädte', 'Hauptstädte', 'Landesflächen', 'Einwohner', 'Flaggen');
  QuizSelections2: array[1..5] of string = (
    'Hauptstadt', 'Hauptstadt', 'Landesfläche', 'Einwohner', 'Flagge');

var
  I: Integer;

begin
  fDEQuiz.stTitle.Caption := 'Deutschland-Quiz : ' + QuizSelections1[Selection] + '.';
  fDEQuiz.laQuizItem.Caption := QuizSelections2[Selection];
  // Capitals direct entry or flags
  if Selection in [2, 5] then begin
    // User answer will be retrieved from edit field
    fDEQuiz.edQuizItem.Visible := True; fDEQuiz.edQuizItem.Text := '';
    fDEQuiz.cobQuizItem.Visible := False;
    // Do or do not display the button to select the flag
    if Selection = 2 then
      fDEQuiz.btFlags.Visible := False
    else begin
      fDEQuiz.btFlags.Visible := True; fDEQuiz.btFlags.Enabled := True;
    end;
  end
  // Capitals, areas or inhabitants selection from list
  else begin
    fDEQuiz.edQuizItem.Visible := False; fDEQuiz.btFlags.Visible := False;
    fDEQuiz.cobQuizItem.Visible := True; fDEQuiz.cobQuizItem.Clear;
    // Fill combobox with items for actual quiz
    case Selection of
      1: begin
           for I := 1 to NLands do
             fDEQuiz.cobQuizItem.Items.Append(QCapitals[I]);                   // capitals list
         end;
      3: begin
           for I := 1 to NAreaGroups do
             fDEQuiz.cobQuizItem.Items.Append(QAreas[I]);                      // land areas groups list
         end;
      4: begin
           for I := 1 to NInhGroups do
             fDEQuiz.cobQuizItem.Items.Append(QInhabitants[I]);                // inhabitants groups list
         end;
    end;
  end;
  fDEQuiz.btQuestion.Caption := 'Frage';
end;

{ Get quiz answer (depending on quiz selected) }

procedure QuizAnswer(Quiz, Land: Integer; var Capitals, Areas, Inhabitants: TArrayS; out Answer: string);

begin
  Answer := '';
  case Quiz of
    1, 2: Answer := Capitals[Land];
       3: Answer := Areas[Land];
       4: Answer := Inhabitants[Land];
       5: Answer := IntToStr(Land) + '.jpg';
  end;
end;

{**********}
{ TfDEQuiz }
{**********}

{ Application start: Lists creation and other initialisations }

procedure TfDEQuiz.FormCreate(Sender: TObject);

var
  I, J: Integer;
  STemp: string;

begin
  // Sorted capitals list (for combobox)
  aCapitalsList := Capitals;
  for I := 1 to NLands - 1 do begin
    for J := I to NLands do begin
      if aCapitalsList[J] < aCapitalsList[I] then begin
        STemp := aCapitalsList[I]; aCapitalsList[I] := aCapitalsList[J]; aCapitalsList[J] := STemp;
      end;
    end;
  end;
  // Group-like areas and inhabitants lists for answer-check (land index) and for combobox (the different groups)
  aAreasList := AreasGroups; aInhabitantsList := InhabitantsGroups;
  for I := 1 to NLands do begin
    for J := 1 to NAreaGroups do begin
      if (Areas[I] >= 1000 * AreasLimits[J - 1]) and (Areas[I] <= 1000 * AreasLimits[J]) then
       aAreas[I] := AreasGroups[J];
    end;
  end;
  for I := 1 to NLands do begin
    for J := 1 to NInhGroups do begin
      if (Inhabitants[I] >= InhabitantsLimits[J - 1]) and (Inhabitants[I] <= InhabitantsLimits[J]) then
       aInhabitants[I] := InhabitantsGroups[J];
    end;
  end;
  // Start random number generator
  Randomize;
  // Start default quiz (capitals from list)
  iQuizSelection := 1;
  QuizSelection(iQuizSelection, aCapitalsList, aAreasList, aInhabitantsList);
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > Neu": Reset form controls }

procedure TfDEQuiz.mQuizNewClick(Sender: TObject);

begin
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > Verlassen": Exit application }

procedure TfDEQuiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Auswahl > Hauptstädte (Liste)": Capitals quiz (selection from list) }

procedure TfDEQuiz.mSelCapitalsListClick(Sender: TObject);

begin
  if not mSelCapitalsList.Checked then begin
    mSelCapitalsList.Checked := True; mSelCapitalsManual.Checked := False;
    mSelAreas.Checked := False; mSelInhabitants.Checked := False;
    mSelFlags.Checked := False;
    iQuizSelection := 1;
    QuizSelection(iQuizSelection, aCapitalsList, aAreasList, aInhabitantsList);
  end;
end;

{ Menu item "Auswahl > Hauptstädte (manuell)": Capitals quiz (manual entry) }

procedure TfDEQuiz.mSelCapitalsManualClick(Sender: TObject);

begin
  if not mSelCapitalsManual.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := True;
    mSelAreas.Checked := False; mSelInhabitants.Checked := False;
    mSelFlags.Checked := False;
    iQuizSelection := 2;
    QuizSelection(iQuizSelection, aCapitalsList, aAreasList, aInhabitantsList);
  end;
end;

{ Menu item "Auswahl > Landesflächen": Land areas quiz (selection from group-like list) }

procedure TfDEQuiz.mSelAreaClick(Sender: TObject);

begin
  if not mSelAreas.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False;
    mSelAreas.Checked := True; mSelInhabitants.Checked := False;
    mSelFlags.Checked := False;
    iQuizSelection := 3;
    QuizSelection(iQuizSelection, aCapitalsList, aAreasList, aInhabitantsList);
  end;
end;

{ Menu item "Auswahl > Einwohner": Inhabitants quiz (selection from group-like list) }

procedure TfDEQuiz.mSelInhabitantsClick(Sender: TObject);

begin
  if not mSelInhabitants.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False;
    mSelAreas.Checked := False; mSelInhabitants.Checked := True;
    mSelFlags.Checked := False;
    iQuizSelection := 4;
    QuizSelection(iQuizSelection, aCapitalsList, aAreasList, aInhabitantsList);
  end;
end;

{ Menu item "Auswahl > Flaggen": Flag quiz (flag image selection) }

procedure TfDEQuiz.mSelFlagsClick(Sender: TObject);

begin
  if not mSelFlags.Checked then begin
    mSelCapitalsList.Checked := False; mSelCapitalsManual.Checked := False;
    mSelAreas.Checked := False;  mSelInhabitants.Checked := False;
    mSelFlags.Checked := True;
    iQuizSelection := 5;
    QuizSelection(iQuizSelection, aCapitalsList, aAreasList, aInhabitantsList);
  end;
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfDEQuiz.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Deutschland-Quiz:' + LineEnding;
  S += 'Bundesland Hauptstadt, Landesfläche, Einwohner, Bundesland-Flagge.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April 2021.';
  MessageDlg('Über "DeutschlandQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Frage/Antwort": Do the quiz (display question or check user's answer) }

procedure TfDEQuiz.btQuestionClick(Sender: TObject);

var
  Percent, I: Integer;
  CorrectAnswer, UserAnswer: string;

begin
  // Button "Frage": Display the quiz question
  if btQuestion.Caption = 'Frage' then begin
    Inc(iQuestion);
    // Not yet all lands done: continue the quiz
    if iQuestion <= NLands then begin
      if iQuestion = 1 then begin
        for I := 1 to NLands do
          aLandsDone[I] := False;                                              // at start of quiz, set all lands to "not done"...
        mSelect.Enabled := False;                                              // ...disable the quiz selection menu
      end;
      // Get a random land
      repeat
        iLand := Random(NLands) + 1;
      until not aLandsDone[iLand];                                             // the random land must not be already done
      aLandsDone[iLand] := True;                                               // mark the land as "done"
      // Display land and clear all answer fields
      edLand.Text := Lands[iLand];
      case iQuizSelection of
        1, 3, 4: cobQuizItem.Text := '';
           2, 5: edQuizItem.Text  := '';
      end;
      edEval.Text := '';
      if iQuizSelection = 2 then
        edQuizItem.SetFocus;
      btQuestion.Caption := 'Antwort';                                          // next button action will be to check user's answer
      btFlags.Enabled := True;                                                  // (re)give user access to "Flagge" button
    end;
  end
  // Button "Antwort": Check user answer
  else begin
    // Get user answer from form
    case iQuizSelection of
      1, 3, 4: UserAnswer := cobQuizItem.Text;                                 // answer retrieved from edit field
         2, 5: UserAnswer := edQuizItem.Text;                                  // answer retrieved from combobox
    end;
    // Get answer for actual quiz and actual land
    QuizAnswer(iQuizSelection, iLand, Capitals, aAreas, aInhabitants, CorrectAnswer);
    // Check user answer
    if UserAnswer = CorrectAnswer then begin
      // Answer is correct
      Inc(iCorrect);
      edEval.Font.Color := clLime;
      edEval.Text := 'Richtig!';
      edCorrect.Text := IntToStr(iCorrect);
    end
    else begin
      // Answer is false
      edEval.Font.Color := clRed;
      if iQuizSelection in [3, 4] then begin
        CorrectAnswer := StringReplace(CorrectAnswer, ' ', '', [rfReplaceAll]);
        CorrectAnswer := StringReplace(CorrectAnswer, '-', ' - ', [rfReplaceAll]);
        if iQuizSelection = 3 then
          CorrectAnswer := StringReplace(CorrectAnswer, 'km²', ' km²', [rfReplaceAll])
        else
        CorrectAnswer := StringReplace(CorrectAnswer, 'Millionen', ' Millionen', [rfReplaceAll]);
      end;
      edEval.Text := 'Falsch! Richtige Antwort = ' + CorrectAnswer;
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
    btQuestion.Caption := 'Frage';                                             // next button action will be to display a question
    // All lands done: Terminate the quiz
    if iQuestion = NLands then begin
      MessageDlg('Deutschland-Quiz','Alle ' + IntToStr(NLands) + ' Bundesländer abgefragt. Ende von diesem Deutschland-Quiz.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // disable button (until next "New" command)
    end;
  end;
end;

{ Button "Flagge": Open the flag selection window and retrieve user flag-quiz answer}

procedure TfDEQuiz.btFlagsClick(Sender: TObject);

begin
  fDEFlags.iQuiz := iQuizSelection;
  fDEFlags.ShowModal;
  if fDEFlags.iLand > 0 then
    edQuizItem.Text := IntToStr(fDEFlags.iLand) + '.jpg';                      // use flag filename (= lands index) as user answer
  btFlags.Enabled := False;                                                    // disable button (to avoid that user chooses another flag)
end;

end.

