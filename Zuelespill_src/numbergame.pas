{******************************************************}
{* Main unit for Zuelespill (Number game) application *}
{******************************************************}

unit numbergame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, PopupNotifier;

type
  { TfNumberGame }
  TfNumberGame = class(TForm)
    mMenu: TMainMenu;
    mGame: TMenuItem;
    mGameNew, mGameExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsNumbers: TMenuItem;
    mSettingsNumbers4, mSettingsNumbers5, mSettingsNumbers6, mSettingsNumbers8: TMenuItem;
    mSettingsMax: TMenuItem;
    mSettingsMax10, mSettingsMax20, mSettingsMax50, mSettingsMax100: TMenuItem;
    mSettingsOperators: TMenuItem;
    mSettingsOperators1, mSettingsOperators2, mSettingsOperators3: TMenuItem;
    mHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    memoRules: TMemo;
    Label1, Label2, Label3: TLabel;
    btNumber1, btNumber2, btNumber3, btNumber4: TButton;
    btNumber5, btNumber6, btNumber7, btNumber8: TButton;
    btPlus, btMinus, btMult, btDiv: TButton;
    btCE, btCA: TButton;
    laSolution1: TLabel;
    laSolution2: TLabel;
    laSolution3: TLabel;
    laSolution4: TLabel;
    laSolution5: TLabel;
    laSolution6: TLabel;
    laSolution7: TLabel;
    edSolution1: TEdit;
    edSolution2: TEdit;
    edSolution3: TEdit;
    edSolution4: TEdit;
    edSolution5: TEdit;
    edSolution6: TEdit;
    edSolution7: TEdit;
    edResult: TEdit;
    Label4: TLabel;
    edCorrect: TEdit;
    btQuestion, btAnswer: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsNumbers4Click(Sender: TObject);
    procedure mSettingsNumbers5Click(Sender: TObject);
    procedure mSettingsNumbers6Click(Sender: TObject);
    procedure mSettingsNumbers8Click(Sender: TObject);
    procedure mSettingsMax10Click(Sender: TObject);
    procedure mSettingsMax20Click(Sender: TObject);
    procedure mSettingsMax50Click(Sender: TObject);
    procedure mSettingsMax100Click(Sender: TObject);
    procedure mSettingsOperators1Click(Sender: TObject);
    procedure mSettingsOperators2Click(Sender: TObject);
    procedure mSettingsOperators3Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
    procedure btNumber1Click(Sender: TObject);
    procedure btNumber2Click(Sender: TObject);
    procedure btNumber3Click(Sender: TObject);
    procedure btNumber4Click(Sender: TObject);
    procedure btNumber5Click(Sender: TObject);
    procedure btNumber6Click(Sender: TObject);
    procedure btNumber7Click(Sender: TObject);
    procedure btNumber8Click(Sender: TObject);
    procedure btPlusClick(Sender: TObject);
    procedure btMinusClick(Sender: TObject);
    procedure btMultClick(Sender: TObject);
    procedure btDivClick(Sender: TObject);
    procedure btCEClick(Sender: TObject);
    procedure btCAClick(Sender: TObject);
  private
    iNumbers, iMaximum, iOperatorsAvailable, iQuestions, iCorrect: Integer;
    ixNumber, ixOperator, ixResult, ixSolution: Integer;
    cOperator: Char;
    bFirstDone, bOperatorDone: Boolean;
    aOperatorsAvailable: array[0..3] of Char;
    aNumbers, aResults, aUserNumbers, aUserResults: array of Integer;
    aOperators, aUserOperators: array of Char;
    btNumber: array[1..8] of TButton;
    laSolution: array[1..7] of TLabel;
    edSolution: array[1..7] of TEdit;
  end;

const
  OperatorsMinimum: array[0..3] of Char = ('+', '-', ' ', ' ');
  OperatorsDefault: array[0..3] of Char = ('+', '-', 'x', ' ');
  OperatorsAll:     array[0..3] of Char = ('+', '-', 'x', ':');

var
  fNumberGame: TfNumberGame;

implementation

{$R *.lfm}

{ Clear (some) form controls }

procedure FormClear;

var
  I: Integer;

begin
  for I := 1 to 8 do
    fNumberGame.btNumber[I].Caption := '';
  for I := 1 to 7 do
    fNumberGame.edSolution[I].Text := '';
end;

{ Update (some) form controls }

procedure FormUpdate(Numbers: Integer);

var
  I: Integer;

// Buttons and solution labels/textfields visible depend on number of numbers given selected

begin
  for I := 5 to Numbers do
    fNumberGame.btNumber[I].Visible := True;
  for I := 8 downto Numbers + 1 do
    fNumberGame.btNumber[I].Visible := False;
  for I := 4 to Numbers - 1 do begin
    fNumberGame.edSolution[I].Visible := True;
    fNumberGame.laSolution[I].Visible := True;
  end;
  for I := 7 downto Numbers do begin
    fNumberGame.edSolution[I].Visible := False;
    fNumberGame.laSolution[I].Visible := False;
  end;
end;

{ "Number button pushed" handling routine }

procedure NumberButton(Button, N: Integer; Numbers: array of Integer; Oprator: char; var xNumber, xResult, xSolution: Integer;
  var FirstDone, OperatorDone: Boolean; var UserNumbers, UserResults: array of Integer);

var
  N1, N2, UResult, I: Integer;
  OK: Boolean;

begin
  // If there are numbers left (not yet included in calculations)
  if xNumber < N then begin
    // If the first number has nor yet been entered
    if not FirstDone then begin
      Inc(xNumber);
      N1 := StrToInt(fNumberGame.btNumber[Button].Caption);                    // first number = the one written on button pressed
      UserNumbers[xNumber] := N1;                                              // store this number into Numbers array
      fNumberGame.btNumber[Button].Enabled := False;                           // disable the button (as this number has been used)
      fNumberGame.edSolution[xSolution].Text := IntToStr(N1) + ' ';            // start fill in the solution edit field
      FirstDone := True; OperatorDone := False;
    end
    // If the operator has already been entered
    else if OperatorDone then begin
      // Get the 2 operands
      Inc(xNumber);
      if xResult = 0 then
        N1 := UserNumbers[1]                                                   // for first operation, first operand = 1st number pressed
      else
        N1 := UserResults[xResult];                                            // for further operations, first operand = result from operation before
      N2 := StrToInt(fNumberGame.btNumber[Button].Caption);                    // second number = the one written on button pressed
      UserNumbers[xNumber] := N2;                                              // store this number into Numbers array
      // Do the operation
      case Oprator of
        '+': UResult := N1 + N2;
        '-': UResult := N1 - N2;
        'x': UResult := N1 * N2;
        ':': UResult := N1 div N2;
      end;
      // Check if intermediate result not equal to any previous int. result
      I := 1; OK := True;
      while (I <= xResult) and OK do begin
        if UResult = UserResults[I] then
          OK := False;
        Inc(I);
      end;
      if OK then begin
        // Check if intermediate result not equal to any of the numbers given
        I := 1;
        while (I <= N) and OK do begin
          if UResult = Numbers[I] then
            OK := False;
          Inc(I);
        end;
        // If this operation is allowed (according to game rules), auto-fill in values
        if OK then begin
          Inc(xResult);
          UserResults[xResult] := UResult;                                     // store intermediate result into Results array
          fNumberGame.btNumber[Button].Enabled := False;                       // disable the button (as this number has been used)
          // Display the operation in solution edit field
          fNumberGame.edSolution[xSolution].Text := fNumberGame.edSolution[xSolution].Text + fNumberGame.btNumber[Button].Caption + ' = ' + IntToStr(UserResults[xResult]);
          Inc(xSolution);                                                      // next solution edit field
          if xSolution <= N - 1 then begin
            // Display intermediate result as first operand of new operation (in following edit field)
            fNumberGame.edSolution[xSolution].Text := IntToStr(UserResults[xResult]) + ' ';
            FirstDone := True; OperatorDone := False;
          end;
        end
        // Operation not allowed
        else begin
          MessageDlg('Ongëlteg Rechnung', 'D''Zwëschenergebnis ' + IntToStr(UResult) + ' wir identisch matt enger vun den Zuelen!', mtError, [mbOK], 0);
          Dec(xNumber);                                                        // decrement here, as has been incremented before
        end;
      end
      // Operation not allowed
      else begin
        MessageDlg('Ongëlteg Rechnung', 'D''Zwëschenergebnis ' + IntToStr(UResult) + ' wir identisch matt engem vu virdrunn!', mtError, [mbOK], 0);
        Dec(xNumber);                                                          // decrement here, as has been incremented before
      end;
    end
    // User subsequently entered 2 numbers (no operator)
    else
      MessageDlg('Fehler', 'Dir musst fir d''éischt een Opérateur aginn!', mtError, [mbOK], 0);
  end;
end;

{ "Operator button pushed" handling routine }

procedure OperatorButton(var Oprator: Char; N: Integer; var xOperator: Integer; xSolution: Integer;
  FirstDone: Boolean; var OperatorDone: Boolean; var UserOperators: array of Char);

begin
  // If there are operators left (not yet included in calculations)
  if xOperator < N then begin
    // No first number has been entered
    if not FirstDone then
      MessageDlg('Fehler', 'Dir musst fir d''éischt eng Zuel aginn!', mtError, [mbOK], 0)
    // First number has been entered: handle the operator
    else begin
      if not OperatorDone then begin
        Inc(xOperator);
        UserOperators[xOperator] := Oprator;                                   // store operator into Operators array
        fNumberGame.edSolution[xSolution].Text := fNumberGame.edSolution[xSolution].Text + Oprator + ' ';
        OperatorDone := True;
      end
      // User subsequently entered 2 operators
      else begin
        MessageDlg('Fehler', 'Dir huet schon een Opérateur aginn!', mtError, [mbOK], 0);
        Oprator := UserOperators[xOperator];                                   // reset last operator to the one displayed on previous button preseed
      end;
    end;
  end;
end;

{ Display calculations (with intermediate and final results) }

procedure ShowResults(N: Integer; Operators: array of Char; Numbers, Results: array of Integer);

var
  N1, N2, I: Integer;

begin
  for I := 1 to N - 1 do begin
    if I = 1 then
      N1 := Numbers[1]
    else
      N1 := Results[I - 1];
    N2 := Numbers[I + 1];
    fNumberGame.edSolution[I].Text := IntToStr(N1) + ' ' + Operators[I] + ' ' + IntToStr(N2) + ' = ' + IntToStr(Results[I]);
  end;
end;

{ Display percentage of correct answers }

procedure DisplayCorrect(Questions, Correct: Integer);

var
  P: Real;

begin
  P := 100 * (Correct / Questions);
  P := Round(100 * P) / 100;
  fNumberGame.edCorrect.Text := FloatToStr(P) + ' %';
  if P < 50 then
    fNumberGame.edCorrect.Color := clRed
  else if P < 60 then
    fNumberGame.edCorrect.Color := clYellow
  else
    fNumberGame.edCorrect.Color := clLime;
end;

{ End of round: User has entered all operations (all numbers given having been used) }

procedure EndOfRound(N: Integer; Operators: array of Char; Numbers, Results, UserResults: array of Integer; Questions: Integer; var Correct: Integer);

begin
  // Result of last operator is equal to the result given as exercise value
  if UserResults[N - 1] = Results[N - 1] then begin
    MessageDlg('Resultat', 'Gudd! Dat ass alles richteg!', mtInformation, [mbOK], 0);
    Inc(Correct);
  end
  // Result of last operator is different from the result given as exercise value
  else begin
    MessageDlg('Resultat', 'Nee, nee! Dat Resultat stëmmt net!', mtWarning, [mbOK], 0);
  end;
  DisplayCorrect(Questions, Correct);
  // If bad answer, display the solutions
  if UserResults[N - 1] <> Results[N - 1] then
    ShowResults(N, Operators, Numbers, Results);
  fNumberGame.btQuestion.Enabled := True; fNumberGame.btAnswer.Enabled := False;
end;

{**************}
{ TfNumberGame }
{**************}

{ Application start: Initialisation }

procedure TfNumberGame.FormCreate(Sender: TObject);

begin
  SetLength(aNumbers, 0); SetLength(aOperators, 0);
  // Create array with "Numbers" buttons
  btNumber[1] := btNumber1; btNumber[2] := btNumber2;
  btNumber[3] := btNumber3; btNumber[4] := btNumber4;
  btNumber[5] := btNumber5; btNumber[6] := btNumber6;
  btNumber[7] := btNumber7; btNumber[8] := btNumber8;
  // Create array with "Solutions" labels and edit fields
  edSolution[1] := edSolution1; edSolution[2] := edSolution2;
  edSolution[3] := edSolution3; edSolution[4] := edSolution4;
  edSolution[5] := edSolution5; edSolution[6] := edSolution6;
  edSolution[7] := edSolution7;
  laSolution[1] := laSolution1; laSolution[2] := laSolution2;
  laSolution[3] := laSolution3; laSolution[4] := laSolution4;
  laSolution[5] := laSolution5; laSolution[6] := laSolution6;
  // Initialize variables
  laSolution[7] := laSolution7;
  iNumbers := 5; iMaximum := 20;
  iQuestions := 0; iCorrect := 0;
  iOperatorsAvailable := 3; aOperatorsAvailable := OperatorsDefault;
  // Start random number generator
  Randomize;
end;

{ Menu item "Spill > Neit Spill": Reset for new game }

procedure TfNumberGame.mGameNewClick(Sender: TObject);

var
  I: Integer;

begin
  FormClear;
  mSettings.Enabled := True;                                                   // enable access to Settings menu items
  for I := 1 to 8 do
    btNumber[I].Enabled := True;
  btQuestion.Enabled := True; btAnswer.Enabled := False;
  iQuestions := 0; iCorrect := 0;
  edCorrect.Text := ''; edCorrect.Color := clDefault;
end;

{ Menu item "Spill > Programm verloossen": Exit application }

procedure TfNumberGame.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Astellungen > Zuelen > ...": Choose number of numbers given }

procedure TfNumberGame.mSettingsNumbers4Click(Sender: TObject);

begin
  if not mSettingsNumbers4.Checked then begin
    mSettingsNumbers4.Checked := True;  mSettingsNumbers5.Checked := False;
    mSettingsNumbers6.Checked := False; mSettingsNumbers8.Checked := False;
    iNumbers := 4;
    FormUpdate(iNumbers);
  end;
end;

procedure TfNumberGame.mSettingsNumbers5Click(Sender: TObject);

begin
  if not mSettingsNumbers5.Checked then begin
    mSettingsNumbers4.Checked := False; mSettingsNumbers5.Checked := True;
    mSettingsNumbers6.Checked := False; mSettingsNumbers8.Checked := False;
    iNumbers := 5;
    FormUpdate(iNumbers);
  end;
end;

procedure TfNumberGame.mSettingsNumbers6Click(Sender: TObject);

begin
  if not mSettingsNumbers6.Checked then begin
    mSettingsNumbers4.Checked := False; mSettingsNumbers5.Checked := False;
    mSettingsNumbers6.Checked := True;  mSettingsNumbers8.Checked := False;
    iNumbers := 6;
    FormUpdate(iNumbers);
  end;
end;

procedure TfNumberGame.mSettingsNumbers8Click(Sender: TObject);

begin
  if not mSettingsNumbers8.Checked then begin
    mSettingsNumbers4.Checked := False; mSettingsNumbers5.Checked := False;
    mSettingsNumbers6.Checked := False; mSettingsNumbers8.Checked := True;
    iNumbers := 8;
    FormUpdate(iNumbers);
  end;
end;

{ Menu items "Astellungen > Maximum > ...": Choose maximum value of numbers given }

procedure TfNumberGame.mSettingsMax10Click(Sender: TObject);

begin
  if not mSettingsMax10.Checked then begin
    mSettingsMax10.Checked := True;  mSettingsMax20.Checked := False;
    mSettingsMax50.Checked := False; mSettingsMax100.Checked := False;
    iMaximum := 10;
  end;
end;

procedure TfNumberGame.mSettingsMax20Click(Sender: TObject);

begin
  if not mSettingsMax20.Checked then begin
    mSettingsMax10.Checked := False; mSettingsMax20.Checked := True;
    mSettingsMax50.Checked := False; mSettingsMax100.Checked := False;
    iMaximum := 20;
  end;
end;

procedure TfNumberGame.mSettingsMax50Click(Sender: TObject);

begin
  if not mSettingsMax50.Checked then begin
    mSettingsMax10.Checked := False; mSettingsMax20.Checked := False;
    mSettingsMax50.Checked := True;  mSettingsMax100.Checked := False;
    iMaximum := 50;
  end;
end;

procedure TfNumberGame.mSettingsMax100Click(Sender: TObject);

begin
  if not mSettingsMax100.Checked then begin
    mSettingsMax10.Checked := False; mSettingsMax20.Checked := False;
    mSettingsMax50.Checked := False; mSettingsMax100.Checked := True;
    iMaximum := 100;
  end;
end;

{ Menu items "Astellungen > Opérateuren > ...": Choose operators to be used }

procedure TfNumberGame.mSettingsOperators1Click(Sender: TObject);

begin
  if not mSettingsOperators1.Checked then begin
    mSettingsOperators1.Checked := True;  mSettingsOperators2.Checked := False; mSettingsOperators3.Checked := False;
    iOperatorsAvailable := 2; aOperatorsAvailable := OperatorsMinimum;
    btMult.Enabled := False; btDiv.Enabled := False;
  end;
end;

procedure TfNumberGame.mSettingsOperators2Click(Sender: TObject);

begin
  if not mSettingsOperators2.Checked then begin
    mSettingsOperators1.Checked := False; mSettingsOperators2.Checked := True; mSettingsOperators3.Checked := False;
    iOperatorsAvailable := 3; aOperatorsAvailable := OperatorsDefault;
    btMult.Enabled := True; btDiv.Enabled := False;
  end;
end;

procedure TfNumberGame.mSettingsOperators3Click(Sender: TObject);

begin
  if not mSettingsOperators3.Checked then begin
    mSettingsOperators1.Checked := False; mSettingsOperators2.Checked := False; mSettingsOperators3.Checked := True;
    iOperatorsAvailable := 4; aOperatorsAvailable := OperatorsAll;
    btMult.Enabled := True; btDiv.Enabled := True;
  end;
end;

{ Menu item "Hëllef > Info": Display program about }

procedure TfNumberGame.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Visible := False
  else begin
    S := 'Zuelerechespill, inspiréiert vun der "Des chiffres et des lettres" ';
    S += 'Spillshow am franséische Fernseh.' + Chr(13) + Chr(13);
    S += 'Versioun 1.0, © allu, Juni, 2018.';
    pnAbout.Text := S;
    pnAbout.Visible := True;
  end;
end;

{ Button "Fro" pushed: Generate the exercise }

procedure TfNumberGame.btQuestionClick(Sender: TObject);

var
  NOperators, N1, N2, R, I, J, Temp: Integer;
  OK, OKAll: Boolean;
  Numbers: array of Integer;

begin
  FormClear;
  mSettings.Enabled := False;                                                  // disable Settings menu during game
  NOperators := iNumbers - 1;
  SetLength(aNumbers, iNumbers + 1); SetLength(aOperators, NOperators + 1); SetLength(aResults, NOperators + 1);
  // Search random numbers and operators until exercise is coherent with game rules
  repeat
    OKAll := True;
    for I := 0 to iNumbers do
      aNumbers[I] := 0;
    for I := 0 to NOperators do
      aResults[I] := 0;
    // Random operators (from those being selected)
    for I := 1 to NOperators do
      aOperators[I] := aOperatorsAvailable[Random(iOperatorsAvailable)];
    // Random numbers (within limits selected)
    for I := 1 to iNumbers do begin
      repeat
        OK := True;
        R := Random(iMaximum) + 1;
        // All numbers must have different value
        J := 1;
        while (J < I) and OK do begin
          if R = aNumbers[J] then
            OK := False;
          Inc(J);
        end;
      until OK;
      aNumbers[I] := R;
    end;
    // Compute intermediate results (last of them being final result)
    for I := 1 to NOperators do begin
      if I = 1 then
        N1 := aNumbers[1]                                                      // first operation: first operand is first number
      else
        N1 := aResults[I - 1];                                                 // other operation: first operand is result from before
      N2 := aNumbers[I + 1];
      // Calculate intermediate result
      case aOperators[I] of
        '+': aResults[I] := N1 + N2;
        '-': if N1 > N2 then
               aResults[I] := N1 - N2
             else                                                              // exclude negative intermediate results
               OKAll := False;
        'x': aResults[I] := N1 * N2;
        ':': if N1 mod N2 = 0 then
               aResults[I] := N1 div N2
             else                                                              // exclude non-integer intermediate results
               OKAll := False;
      end;
      if OKAll then begin
        // All intermediate results must have different values
        J := 1;
        while (J < I) and OKAll do begin
          if aResults[I] = aResults[J] then
            OKAll := False;
          Inc(J);
        end;
      end;
      if OKAll then begin
        // Intermediate results must be different from numbers given
        J := 1;
        while (J <= iNumbers) and OKAll do begin
          if aResults[I] = aNumbers[J] then
            OKAll := False;
          Inc(J);
        end;
      end;
    end;
    // Limit final result to 10 times the maximum selected for numbers given values
    if aResults[NOperators] > 10 * iMaximum then
      OKAll := False;
  until OKAll;
  SetLength(Numbers, iNumbers + 1);
  // Sort the numbers in order to display them ordered onto the Number buttons
  for I := 1 to iNumbers do
    Numbers[I] := aNumbers[I];
  for I := 1 to iNumbers - 1 do begin
    for J := I to iNumbers do begin
      if Numbers[J] < Numbers[I] then begin
        Temp := Numbers[I]; Numbers[I] := Numbers[J]; Numbers[J] := Temp;
      end;
    end;
  end;
  // Display the numbers onto the buttons
  for I := 1 to iNumbers do
    btNumber[I].Caption := IntToStr(Numbers[I]);
  // Enable the buttons used (as many as numbers given selected)
  for I := 1 to 8 do
    btNumber[I].Enabled := True;
  // Display the result to be found
  edResult.Text := IntToStr(aResults[NOperators]);
  // Set variables for game continuation
  ixNumber := 0; ixOperator := 0; ixResult := 0; ixSolution := 1;
  bFirstDone := False; bOperatorDone := False;
  SetLength(aUserNumbers, iNumbers + 1); SetLength(aUserResults, NOperators + 1); SetLength(aUserOperators, NOperators + 1);
  Inc(iQuestions);
  btQuestion.Enabled := False; btAnswer.Enabled := True;
end;

{ Button "Äntwert" pushed: Display results and percentage of correct answers }

procedure TfNumberGame.btAnswerClick(Sender: TObject);

begin
  ShowResults(iNumbers, aOperators, aNumbers, aResults);
  DisplayCorrect(iQuestions, iCorrect);
  btQuestion.Enabled := True; btAnswer.Enabled := False;
end;

{ "Number" buttons pushed: Compute user solutions }

procedure TfNumberGame.btNumber1Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(1, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber2Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(2, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber3Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(3, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber4Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(4, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber5Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(5, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber6Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(6, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber7Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(7, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

procedure TfNumberGame.btNumber8Click(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    NumberButton(8, iNumbers, aNumbers, cOperator, ixNumber, ixresult, ixSolution, bFirstDone, bOperatorDone, aUserNumbers, aUserResults);
    if ixNumber = iNumbers then
      EndOfRound(iNumbers, aOperators, aNumbers, aResults, aUserResults, iQuestions, iCorrect);
  end;
end;

{ "Operator" buttons pushed: Compute user solutions }

procedure TfNumberGame.btPlusClick(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    cOperator := '+';
    OperatorButton(cOperator, iNumbers - 1, ixOperator, ixSolution, bFirstDone, bOperatorDone, aUserOperators);
  end;
end;

procedure TfNumberGame.btMinusClick(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    cOperator := '-';
    OperatorButton(cOperator, iNumbers - 1, ixOperator, ixSolution, bFirstDone, bOperatorDone, aUserOperators);
  end;
end;

procedure TfNumberGame.btMultClick(Sender: TObject);

begin
  if btAnswer.Enabled then begin
    cOperator := 'x';
    OperatorButton(cOperator, iNumbers - 1, ixOperator, ixSolution, bFirstDone, bOperatorDone, aUserOperators);
  end;
end;

procedure TfNumberGame.btDivClick(Sender: TObject);

begin
  if btAnswer.Enabled and (ixNumber < iNumbers) then begin
    cOperator := ':';
    OperatorButton(cOperator, iNumbers - 1, ixOperator, ixSolution, bFirstDone, bOperatorDone, aUserOperators);
  end;
end;

{ Button "CA" pushed: Clear all }

procedure TfNumberGame.btCAClick(Sender: TObject);

var
  I: Integer;

begin
  if btAnswer.Enabled then begin
    for I := 1 to iNumbers do begin
      btNumber[I].Enabled := True;                                             // re-enable "Numbers" buttons
      edSolution[I].Text := '';
    end;
    // Reset variables
    ixNumber := 0; ixOperator := 0; ixResult := 0; ixSolution := 1;
    bFirstDone := False; bOperatorDone := False;
  end;
end;

{ Button "CE" pushed: Clear last entry }

procedure TfNumberGame.btCEClick(Sender: TObject);

var
  P, I: Integer;

begin
  if btAnswer.Enabled and (ixNumber < iNumbers) then begin
    // If last entry was an operator, remove this operator from the solution string
    if bOperatorDone then begin
      edSolution[ixSolution].Text := LeftStr(edSolution[ixSolution].Text, Length(edSolution[ixSolution].Text) - 2);
      Dec(ixOperator);
      bOperatorDone := False;
    end
    // If last entry was a number, clear current solution string and second operand from previous solution string
    else begin
      edSolution[ixSolution].Text := '';
      // Solution is not the first one: do as described above
      if ixSolution > 1 then begin
        Dec(ixSolution);
        // Find position of operator in order to clear everything behind
        P := Pos(aUserOperators[ixOperator], edSolution[ixSolution].Text);
        edSolution[ixSolution].Text := LeftStr(edSolution[ixSolution].Text, P + 1);
        Dec(ixResult);
        // Re-enable the button corresponding to last number entered
        for I := 1 to iNumbers do begin
          if StrToInt(btNumber[I].Caption) = aUserNumbers[ixNumber] then
            btNumber[I].Enabled := True;
        end;
        Dec(ixNumber);
        bOperatorDone := True;
      end
      // First solution: clear all
      else begin
        edSolution[ixSolution].Text := '';
        ixNumber := 0; ixOperator := 0; ixResult := 0;
        bFirstDone := False; bOperatorDone := False;
        // Re-enable all buttons
        for I := 1 to iNumbers do
          btNumber[I].Enabled := True;
      end;
    end;
  end;
end;

end.

