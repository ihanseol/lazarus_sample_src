{*********************************************}
{* Main unit for Verkehrszeichen application *}
{*********************************************}

unit verkehrszeichen_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, LazUTF8, verkehrszeichen_u2;

type
  TSignal = record
    Signal, List: string;
    CorrectAnswer1, CorrectAnswer2: string;
    FalseAnswers: array of string;
  end;
  TSignals = array of TSignal;
  {*******************}
  { TfVerkehrszeichen }
  {*******************}
  TfVerkehrszeichen = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsSignals, mSettingsSignalsDE, mSettingsSignalsEU: TMenuItem;
    mSettingsProposals, mSettingsProposals3, mSettingsProposals4, mSettingsAllWrong: TMenuItem;
    MenuItem1, mSettingsEval, mSettingsEval1, mSettingsEval2, mSettingsEval3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imSignal: TImage;
    Memo1: TMemo;
    Label1, laQuestion: TLabel;
    rbAnswer1, rbAnswer2, rbAnswer3, rbAnswer4: TRadioButton;
    edEval: TEdit;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btAnswer: TButton;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsSignalsDEClick(Sender: TObject);
    procedure mSettingsSignalsEUClick(Sender: TObject);
    procedure mSettingsProposals3Click(Sender: TObject);
    procedure mSettingsProposals4Click(Sender: TObject);
    procedure mSettingsAllWrongClick(Sender: TObject);
    procedure mSettingsEval1Click(Sender: TObject);
    procedure mSettingsEval2Click(Sender: TObject);
    procedure mSettingsEval3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
  private
    iMaxDE, iMaxEU, iQuestionsTemp, iQuestions, iProposalsTemp, iProposals, iEvalTemp, iEval: Integer;
    iSignal, iCorrectAnswer, iQuestion, iCorrect, iFalse, iNoAnswer: Integer;
    sListTemp, sList: string;
    bAllFalseTemp, bAllFalse: Boolean;
    aSignals: TSignals;
    aSignalsDone: array of Boolean;
    rbAnswers: array[1..4] of TRadioButton;
  end;

var
  fVerkehrszeichen: TfVerkehrszeichen;

implementation

{$R *.lfm}

{ Format numbers for string grid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  if N < 0 then
    SN := IntToStr(N)
  else
    SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN := SN + S;                                                              // percent sign
  Result := SN;
end;

{ Read signal data from text files }

procedure ReadSignals(out Signals: TSignals; out MaxDE, MaxEU: Integer);

var
  N, I: Integer;
  Line, Signal, List, Answer: string;
  InFile: Text;

begin
  // Read correct answers file
  Assign(InFile, 'verkehrszeichen.txt'); Reset(InFile);
  N := 0; MaxDE := 0; MaxEU := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      if LeftStr(Line, 3) <> '   ' then begin
        // Start of a new signal
        Signal := LeftStr(Line, 3);                                            // 3-digits number = filename = index of signals array
        List := Copy(Line, 5, 1);                                              // D/d = Germany, E = other; - = not included into test
        if List <> '-' then begin
          Inc(MaxEU);                                                          // number of used items in complete list
          if UpperCase(List) = 'D' then begin
            List := 'DE';
            Inc(MaxDE);                                                        // number of used items in Germany list
          end
          else begin
            List := 'EU';
          end;
        end;
        // Add signal data to array
        Inc(N); SetLength(Signals, N);
        Signals[N - 1].Signal := Signal;
        Signals[N - 1].List := List;
      end;
      // First correct answer
      Answer := UTF8Trim(UTF8Copy(Line, 7, UTF8Length(Line)));
      // Optional second correct answer (otherwise set equal to first)
      if LeftStr(Line, 3) <> '   ' then begin
        Signals[N - 1].CorrectAnswer1 := Answer;
        Signals[N - 1].CorrectAnswer2 := Signals[N - 1].CorrectAnswer1;
      end
      else begin
        Signals[N - 1].CorrectAnswer2 := Answer;
      end;
    end;
  end;
  Close(InFile);
  // Read false answers file
  Assign(InFile, 'verkehrszeichen2.txt'); Reset(InFile);
  N := 0; I := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      if LeftStr(Line, 3) <> '   ' then begin
        // Start of a new signal: Point to next array position and reset false answers counter (index for that array)
        Inc(N);
        I := 0;
      end;
      // Fill in false answers for this signal
      Inc(I); SetLength(Signals[N - 1].FalseAnswers, I);
      Signals[N - 1].FalseAnswers[I - 1] := UTF8Trim(UTF8Copy(Line, 5, UTF8Length(Line)));
    end;
  end;
  Close(InFile);
end;

{ Calculate actual success values and display all evaluation values in the grid }

procedure EvalUpdate(Eval, Question, Correct, Flse, NoAnswer: Integer);

var
  Points, Percent: Integer;

begin
  fVerkehrszeichen.sgEval.Cells[1, 0] := GFormat(Question, '');
  fVerkehrszeichen.sgEval.Cells[1, 1] := GFormat(Correct, '');
  fVerkehrszeichen.sgEval.Cells[1, 2] := GFormat(Flse, '');
  fVerkehrszeichen.sgEval.Cells[1, 3] := GFormat(NoAnswer, '');
  case Eval of
    // Success points calculation depending on user settings
    1: Points := Correct;
    2: Points := Correct - (Flse + NoAnswer);
    3: Points := Correct - Flse;
  end;
  fVerkehrszeichen.sgEval.Cells[1, 4] := GFormat(Points, '');
  // Calculate success percentage
  Percent := Round(100 * Points / Question);
  if Percent < 0 then
    Percent := 0;
  fVerkehrszeichen.sgEval.Cells[1, 5] := GFormat(Percent, '%');
end;

{ All signals done (end of test) }

procedure EndOfTest;

begin
  MessageDlg('Verkehrszeichen', 'Alle Verkehrszeichen abgefragt. Ende des Tests.', mtInformation, [mbOK], 0);
  fVerkehrszeichen.btQuestion.Enabled := False; fVerkehrszeichen.btAnswer.Enabled := False;
end;

{*******************}
{ TfVerkehrszeichen }
{*******************}

{ Application start: Initialisation }

procedure TfVerkehrszeichen.FormCreate(Sender: TObject);

begin
  // Create array with answer radio buttons
  rbAnswers[1] := rbAnswer1; rbAnswers[2] := rbAnswer2;
  rbAnswers[3] := rbAnswer3; rbAnswers[4] := rbAnswer4;
  // Read signal data from file
  ReadSignals(aSignals, iMaxDE, iMaxEU);
  SetLength(aSignalsDone, Length(aSignals));
  // Init variables (startup defaults)
  iQuestionsTemp := 20; iProposalsTemp := 3; iEvalTemp := 1;
  sListTemp := 'de'; bAllFalseTemp := False;
  // Start random number generator
  Randomize;
  // Start a new test
  mTestNew.Click;
end;

{ Menu item "Test > Neu": Start a new test }

procedure TfVerkehrszeichen.mTestNewClick(Sender: TObject);

var
  I: Integer;
  Filename: string;

begin
  iQuestions := iQuestionsTemp; iProposals := iProposalsTemp; iEval := iEvalTemp;
  sList := sListTemp; bAllfalse := bAllFalseTemp;
  if (sList = 'DE') and (iQuestions > iMaxDE) then
    // Be sure that the number of quiz questions doesn't exceed the number of signals available in list
    iQuestions := iMaxDE;
  // Hide 4th answers radio button if not used
  if iProposals = 3 then
    rbAnswer4.Visible := False
  else
    rbAnswer4.Visible := True;
  // Init radio buttons
  for I := 1 to iProposals do begin
    rbAnswers[I].Caption := ' Antwort ' + IntToStr(I);
    rbAnswers[I].Checked := False;
  end;
  // Reset evaluation counters
  iQuestion := 0; iCorrect := 0; iFalse := 0; iNoAnswer := 0;
  // Set all signals as "not yet done"
  for I := 0 to Length(aSignalsDone) - 1 do
    aSignalsDone[I] := False;
  // Init question label and picture
  laQuestion.Caption := 'Frage:';
  Filename := './pics/136.jpg'; DoDirSeparators(Filename);
  imSignal.Picture.LoadFromFile(Filename);
  // Clear evaluation grid
  edEval.Text := ''; edEval.Color := clDefault;
  for I := 0 to 5 do
    sgEval.Cells[1, I] := '';
  // Set buttons properties for start of test
  btQuestion.Enabled := True; btAnswer.Enabled := False;
end;

{ Menu item "Test > Verlassen": Exit application }

procedure TfVerkehrszeichen.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Anzahl der Fragen ...": Select number of test questions }

procedure TfVerkehrszeichen.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Verkehrszeichen', 'Anzahl der Fragen', IntToStr(iQuestionsTemp));
  if S <> '' then begin
    iQuestionsTemp := StrToInt(S);
    if iQuestionsTemp > iMaxEU then
      iQuestionsTemp := iMaxEU                                                 // max. number of questions = total number of signals
    else if iQuestionsTemp < 10 then
      iQuestionsTemp := 10;                                                    // min number of questions = 10 (arbitrarily fixed)
  end;
end;

{ Menu items "Einstellungen > Verkehrszeichen Liste > ...": Select signals list (Germany or full list) }

procedure TfVerkehrszeichen.mSettingsSignalsDEClick(Sender: TObject);

begin
  mSettingsSignalsDE.Checked := True; mSettingsSignalsEU.Checked := False;
  sListTemp := 'DE';
end;

procedure TfVerkehrszeichen.mSettingsSignalsEUClick(Sender: TObject);

begin
  mSettingsSignalsDE.Checked := False; mSettingsSignalsEU.Checked := True;
  sListTemp := 'EU';
end;

{ Menu items "Einstellungen > Angezeigte Antworten > ...": Select number of answer proposals (3 or 4) }

procedure TfVerkehrszeichen.mSettingsProposals3Click(Sender: TObject);

begin
  mSettingsProposals3.Checked := True; mSettingsProposals4.Checked := False;
  iProposalsTemp := 3;
  mSettingsAllWrong.Checked := False; bAllFalseTemp := mSettingsAllWrong.Checked; mSettingsAllWrong.Enabled := False;
end;

procedure TfVerkehrszeichen.mSettingsProposals4Click(Sender: TObject);

begin
  mSettingsProposals3.Checked := False; mSettingsProposals4.Checked := True;
  iProposalsTemp := 4;
  mSettingsAllWrong.Enabled := True; bAllFalseTemp := mSettingsAllWrong.Checked;
end;

{ Menu items "Einstellungen > 'Alle falsch' Vorschlag": Toggle to use or not the 'all wrong' proposal }

procedure TfVerkehrszeichen.mSettingsAllWrongClick(Sender: TObject);

begin
  if mSettingsAllWrong.Checked then
    mSettingsAllWrong.Checked := False
  else
    mSettingsAllWrong.Checked := True;
  bAllFalseTemp := mSettingsAllWrong.Checked;
end;

{ Menu items "Einstellungen > Benotung > ...": Select evaluation method (1,0; 1,-1; 1,-1,0) }

procedure TfVerkehrszeichen.mSettingsEval1Click(Sender: TObject);

begin
  mSettingsEval1.Checked := True; mSettingsEval2.Checked := False; mSettingsEval3.Checked := False;
  iEvalTemp := 1;
end;

procedure TfVerkehrszeichen.mSettingsEval2Click(Sender: TObject);

begin
  mSettingsEval1.Checked := False; mSettingsEval2.Checked := True; mSettingsEval3.Checked := False;
  iEvalTemp := 2;
end;

procedure TfVerkehrszeichen.mSettingsEval3Click(Sender: TObject);

begin
  mSettingsEval1.Checked := False; mSettingsEval2.Checked := False; mSettingsEval3.Checked := True;
  iEvalTemp := 3;
end;

{ Open window with application help text }

procedure TfVerkehrszeichen.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Display application about }

procedure TfVerkehrszeichen.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Vorbereitung auf die Führerscheinprüfung:' + LineEnding;
  S += 'Quiz zum Erkennen der Verkehrszeichen.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2021.';
  MessageDlg('Über "Verkehrszeichen"', S, mtInformation, [mbOK], 0);
end;

{ Button "Frage": Generate a new question }

procedure TfVerkehrszeichen.btQuestionClick(Sender: TObject);

var
  I, J, K, IX: Integer;
  Filename, CorrectAnswer, FalseAnswer: string;
  OK: Boolean;

begin
  if btAnswer.Enabled then begin
    // If user did not answer previous question, count this one as "not answered"
    Inc(iNoAnswer);
    EvalUpdate(iEval, iQuestion, iCorrect, iFalse, iNoAnswer);
  end;
  // Proceed if there are questions left
  if iQuestion < iQuestions then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Frage ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ':';
    // Select a random signal (among those from the actual list and not already done)
    repeat
      OK := True;
      iSignal := Random(Length(aSignals));
      if mSettingsSignalsDE.Checked and (aSignals[iSignal].List <> 'DE') then
        OK := False
      else if mSettingsSignalsEU.Checked and (aSignals[iSignal].List = '-') then
        OK := False
      else if aSignalsDone[iSignal] then
        OK := False;
    until OK;
    aSignalsDone[iSignal] := True;                                             // set this signal as 'done'
    // Display the signal's picture
    Filename := './pics/' + aSignals[iSignal].Signal + '.jpg'; DoDirSeparators(Filename);
    imSignal.Picture.LoadFromFile(Filename);
    // Select random radio button for correct answer
    iCorrectAnswer := Random(iProposals) + 1;
    // Randomly choose one of the two correct answers given in the file
    if Random(2) = 0 then
      CorrectAnswer := aSignals[iSignal].CorrectAnswer1
    else
      CorrectAnswer := aSignals[iSignal].CorrectAnswer2;
    // Fill-in the answers radio buttons' labels
    for I := 1 to iProposals do
      rbAnswers[I].Caption := '';
    for I := 1 to iProposals do begin
      if I = iCorrectAnswer then begin
        // Correct answer
        rbAnswers[I].Caption := ' ' + CorrectAnswer;
      end
      else begin
        // Random false answer
        repeat
          OK := True;
          K := Random(Length(aSignals[iSignal].FalseAnswers));
          FalseAnswer := ' ' + aSignals[iSignal].FalseAnswers[K];
          for J := 1 to iProposals do begin
            // Answer must be used only once
            if FalseAnswer = rbAnswers[J].Caption then
              OK := False;
          end;
        until OK;
        rbAnswers[I].Caption := FalseAnswer;
      end;
    end;
    // If "all wrong" answer is selected...
    if mSettingsAllWrong.Checked then begin
      if Random(2) = 0 then begin                                              // 50% of the questions with an 'all wrong' answer
        if Random(3) = 0 then                                                  // 33% of these will be the correct answer to check by user
          IX := iCorrectAnswer
        else begin                                                             // 66% of these will not be the correct answer to check by user
          repeat
            // Random false answer replaced by 'all wrong' item
            IX := Random(iProposals) + 1;
          until IX <> iCorrectAnswer;
        end;
        rbAnswers[IX].Caption := ' Keine der angezeigten Antworten ist richtig...';
      end;
    end;
    // Clear form controls
    edEval.Text := '';  edEval.Color := clDefault;
    for I := 1 to 4 do
      rbAnswers[I].Checked := False;
    // Set buttons properties for user giving an answer (or asking for a new question)
    btAnswer.Enabled := True; btAnswer.SetFocus;
  end
  // All questions have been done
  else begin
    EndOfTest;
  end;
end;

{ Button "Antwort": Check user answer }

procedure TfVerkehrszeichen.btAnswerClick(Sender: TObject);

var
  IX, I: Integer;
  Correct, NoAnswer: Boolean;

begin
  IX := 0; Correct := False; NoAnswer := False;
  // Find radio button selected by user
  for I := 1 to 4 do begin
    if rbAnswers[I].Checked then
      IX := I;
  end;
  if IX = 0 then
    // No radio button selected = no answer given
    NoAnswer := True
  else begin
    // If a radio button selected, check if it is the one corr. to the correct answer
    if IX = iCorrectAnswer then
      Correct := True;
  end;
  // Evaluation
  if Correct then begin
    // Correct answer
    Inc(iCorrect);
    edEval.Text := 'Diese Antwort ist richtig!';
    edEval.Color := clLime;
  end
  else if NoAnswer then begin
    // No answer
    Inc(iNoAnswer);
    edEval.Text := 'Richtige Anwort ist: Antwort ' + IntToStr(iCorrectAnswer) + '.';
    if mSettingsEval2.Checked then
      edEval.Color := clRed
    else
      edEval.Color := clYellow;
  end
  else begin
    // False answer
    Inc(iFalse);
    edEval.Text := 'Falsch! Die richtige Antwort ist: Antwort ' + IntToStr(iCorrectAnswer) + '.';
    if mSettingsEval1.Checked then
      edEval.Color := clYellow
    else
      edEval.Color := clRed;
  end;
  EvalUpdate(iEval, iQuestion, iCorrect, iFalse, iNoAnswer);                   // update evaluation grid
  // Set buttons properties for user asking for a new question
  btAnswer.Enabled := False; btQuestion.SetFocus;
  if iQuestion = iQuestions then begin
    // All questions have been done
    EndOfTest;
  end;

end;

end.

