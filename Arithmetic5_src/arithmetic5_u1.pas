{*****************************************}
{* Main unit for Arithmetic5 application *}
{*****************************************}

unit arithmetic5_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus;

type
  {**********}
  { TfArith5 }
  {**********}
  TfArith5 = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestCalc, mTestOperation, mTestConversion, mTestExit: TMenuItem;
    mSettings, mSettingsNumbers, mSettingsPercent: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laTest: TLabel;
    edDescription: TMemo;
    laQuestion: TLabel;
    edQuestion: TEdit;
    edAnswer: TEdit;
    edEvaluation: TEdit;
    btStart: TButton;
    btSolve: TButton;
    btReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestCalcClick(Sender: TObject);
    procedure mTestOperationClick(Sender: TObject);
    procedure mTestConversionClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsNumbersClick(Sender: TObject);
    procedure mSettingsPercentClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btSolveClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
  private
    iTest, iQuestion, iCorrect, iMaxNumber, iMaxPercent: Integer;
    sQuestion, sAnswer: string;
  end;

var
  fArith5: TfArith5;

implementation

{$R *.lfm}

{ Greatest Common Divisor }

function GCD (N1, N2: Integer): Integer;

var
  N0, NR : INTEGER;

begin
  N0 := 1;
  repeat
    NR := N1 mod N2;
    if NR = 0 then
      N0 := N2
    else begin
      N1 := N2;
      N2 := NR;
    end;
  until NR = 0;
  GCD := N0;
end;

{ Start a new test }

procedure NewTest(Test: Integer; EvalText: string; EvalColor: TColor; out Question, Correct: Integer);

const
  Tests: array[1..3] of string = (
    'Percentage calculation', 'Percentage operations', 'Percentage conversion'
  );
  Descriptions: array[1..3, 1..2] of string = (
    ( 'Calculate a given percentage of a given number.',
      'Ex: Calculate 5% of 60; result = 3.'
    ),
    ( 'Add/subtract a given percentage to/from a given number.',
      'Ex: Subtract 5% from 60; result = 57.'
    ),
    ( 'Convert a percentage into a fraction or vice-versa. Reduce the fraction as much as possible.',
      'Ex: Convert 5% to a fraction; result = 1/20.'
    )
  );

begin
  fArith5.laTest.Caption := Tests[Test];
  fArith5.edDescription.Lines.Clear;
  fArith5.edDescription.Lines.AddText(Descriptions[Test, 1]);
  fArith5.edDescription.Lines.AddText(Descriptions[Test, 2]);
  fArith5.laQuestion.Caption := 'Question';
  fArith5.edQuestion.Text := '';
  fArith5.edAnswer.Text := '';
  fArith5.edEvaluation.Text := EvalText;
  fArith5.edEvaluation.Color := EvalColor;
  fArith5.btStart.Caption := 'Start';
  fArith5.btStart.Enabled := True;
  fArith5.btSolve.Enabled := False;
  fArith5.btReset.Enabled := False;
  fArith5.btReset.Enabled := False;
  Question := 0; Correct := 0;
end;

{ Generate "Percentage calculation" question }

procedure QuestionCalculation(MaxNumber, MaxPercent: Integer; out Question, Answer: string);

var
  N, P, R: Integer;
  Result: Real;
  OK: Boolean;

begin
  R := Random(5);                                                              // percentages greater than 100 (if enabled)
  repeat
    // Generate number and percentage
    OK := True;
    N := Random(MaxNumber) + 1;                                                // the number to calculate % of
    if MaxPercent = 100 then
      P := Random(100) + 1                                                     // 1 - 100 %
    else begin
      if R = 0 then
        P := 100 + 25 * (Random((MaxPercent - 100) div 25) + 1)                // 125 - 400 % (steps of 25)
      else
        P := Random(100) + 1;
    end;
    Result := N * (P / 100);
    // Exclude "non-wanted" values
    if (N = 100) or (P = 100) or (P = 50)then                                  // exclude number = 100 and 50% and 100%
      OK := False
    else if Result > MaxNumber then                                            // result should not exceed maximum number
      OK := False
    else if Result <> Int(Result) then                                         // result should be an integer
      OK := False;
  until OK;
  Question := 'Calculate ' + IntToStr(P) + '% of ' + IntToStr(N);
  Answer := FloatToStr(Result);
end;

{ Generate "Percentage operation" question }

procedure QuestionOperation(MaxNumber, MaxPercent: Integer; out Question, Answer: string);

var
  O, N, P, R: Integer;
  Result: Real;
  OK: Boolean;

begin
  R := Random(5);                                                              // percentages greater than 100 (if enabled)
  repeat
    // Generate number and percentage
    OK := True;
    N := Random(MaxNumber) + 1;                                                // the number to calculate % of
    if MaxPercent = 100 then
      P := Random(100) + 1                                                     // 1 .. 100 %
    else begin
      if R = 0 then
        P := 100 + 25 * (Random((MaxPercent - 100) div 25) + 1)                // 125 - 400 % (steps of 25)
      else
        P := Random(100) + 1;
    end;
    O := Random(2);                                                            // addition or subtraction
    if O = 0 then
      Result := N + N * (P / 100)
    else
      Result := N - N * (P / 100);
    // Exclude "non-wanted" values
    if (N = 100) or (P = 100) then                                             // exclude number = 100 and 100%
      OK := False
    else if Result > MaxNumber then                                            // result should not excced maximum number
      OK := False
    else if Result < 0 then                                                    // result should be positive
      OK := False
    else if Result <> Int(Result) then                                         // result should be an integer
      OK := False;
  until OK;
  if O = 0 then
    Question := 'Add ' + IntToStr(P) + '% to ' + IntToStr(N)
  else
    Question := 'Subtract ' + IntToStr(P) + '% from ' + IntToStr(N);
  Answer := FloatToStr(Result);
end;

{ Generate "Percentage conversion" question }

procedure QuestionConversion(out Question, Answer: string);

var
  F1, F2, FD, P, R: Integer;
  Fraction: string;
  OK: Boolean;

begin
  repeat
    // Generate percentage
    OK := True;
    P := Random(99) + 1;
    // Fraction = P / 100: Reduce it!
    FD := GCD(P, 100);
    F1 := P div FD;
    F2 := 100 div FD;
    // Exclude "non-wanted" values
    if (F2 = 100) or (F2 = 50) then                                            // exclude fraction denominator 50 and 100
      OK := False
  until OK;
  Fraction := IntToStr(F1) + '/' + IntToStr(F2);
  // Choose between percentage-fraction and fraction-percentage conversion
  R := Random(2);
  if R = 0 then begin
    Question := 'Convert ' + IntToStr(P) + '% into a fraction';
    Answer := Fraction;
  end
  else begin
    Question := 'Convert ' + Fraction + ' into a percentage';
    Answer := IntToStr(P) + '%';
  end;
end;

{**********}
{ TfArith5 }
{**********}

{ Application start: Initialisation }

procedure TfArith5.FormCreate(Sender: TObject);

begin
  iTest := 1; iMaxNumber := 1000; iMaxPercent := 400;
  NewTest(iTest, '', clForm, iQuestion, iCorrect);
  Randomize;
end;

{ Menu item "Test > Percentage calculation": Start a new "Percentage calculation" test }

procedure TfArith5.mTestCalcClick(Sender: TObject);

begin
  iTest := 1;
    NewTest(iTest, '', clForm, iQuestion, iCorrect);
end;

{ Menu item "Test > Percentage operation": Start a new "Percentage operation" test }

procedure TfArith5.mTestOperationClick(Sender: TObject);

begin
  iTest := 2;
    NewTest(iTest, '', clForm, iQuestion, iCorrect);
end;

{ Menu item "Test > Percentage conversion": Start a new "Percentage conversion" test }

procedure TfArith5.mTestConversionClick(Sender: TObject);

begin
  iTest := 3;
  NewTest(iTest, '', clForm, iQuestion, iCorrect);
end;

{ Menu item "Test > Exit": Exit application }

procedure TfArith5.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Limit numbers to 100": Choose maximum number }

procedure TfArith5.mSettingsNumbersClick(Sender: TObject);

begin
  if mSettingsNumbers.Checked then begin
    mSettingsNumbers.Checked := False;
    iMaxNumber := 1000;                                                        // unchecked default = 1000
  end
  else begin
    mSettingsNumbers.Checked := True;
    iMaxNumber := 100;                                                         // checed = 100
  end;
end;

{ Menu item "Settings > Limit Percentages to 100": Choose maximum percentage }

procedure TfArith5.mSettingsPercentClick(Sender: TObject);

begin
  if mSettingsPercent.Checked then begin
    mSettingsPercent.Checked := False;
    iMaxPercent := 400;                                                        // unchecked default = 400
  end
  else begin
    mSettingsPercent.Checked := True;
    iMaxPercent := 100;                                                        // checked = 100
  end;
end;

{ Menu item "Help > About": Display program about }

procedure TfArith5.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics trainer: Generation of arithmetic exercises concerning percentages.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2019.';
  MessageDlg('About "Arithmetic5"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Generate question }

procedure TfArith5.btStartClick(Sender: TObject);

begin
  Inc(iQuestion);
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion);
  edAnswer.Text := '';
  edEvaluation.Text := ''; edEvaluation.Color := clForm;
  case iTest of
    // Generate question type depending on test launched
    1: QuestionCalculation(iMaxNumber, iMaxPercent, sQuestion, sAnswer);
    2: QuestionOperation(iMaxNumber, iMaxPercent, sQuestion, sAnswer);
    3: QuestionConversion(sQuestion, sAnswer);
  end;
  edQuestion.Text := sQuestion;
  edAnswer.SetFocus;
  btStart.Caption := 'Next';
  btSolve.Enabled := True;
  if iQuestion > 5 then
    btReset.Enabled := True;                                                   // no sense to evaluate when less than 5 questions...
end;

{ Button "Solve": Check the answer entered by the user}

procedure TfArith5.btSolveClick(Sender: TObject);

var
  UserAnswer: string;

begin
  UserAnswer := edAnswer.Text;
  UserAnswer := StringReplace(UserAnswer, ' ', '', [rfReplaceAll]);            // remove all spaces
  if iTest = 3 then begin
    if (RightStr(sAnswer, 1) = '%') and (RightStr(UserAnswer, 1) <> '%') then  // add a % sign for fraction conversion questions (if missing)
    UserAnswer += '%';
  end;
  if UserAnswer = sAnswer then begin                                           // user entry is correct answer
    // Correct answer
    edEvaluation.Color := clLime;
    edEvaluation.Text := 'Your answer is correct!';                            // display message (on green background)
    Inc(iCorrect);
  end
  else begin                                                                    // user entry is false answer
    // False answer
    edEvaluation.Color := clRed;
    edEvaluation.Text := 'False! Correct answer is: ' + sAnswer;                // display correct answer (on red background)
  end;
  btSolve.Enabled := False;
  if iQuestion >= 5 then
    btReset.Enabled := True;
  btStart.SetFocus;
end;

{ Button "Reset": Evaluate user answers; reset for new test }

procedure TfArith5.btResetClick(Sender: TObject);

var
  PCorrect: Integer;
  Eval: string;
  Colour: TColor;

begin
  // Calculate and display success percentage
  PCorrect :=  Round(100 * (iCorrect / iQuestion));
  if PCorrect >= 60 then
    Colour := clLime
  else if PCorrect >= 50 then
    Colour := clYellow
  else
    Colour := clRed;
  Eval := 'Correct answers = ' + IntToStr(iCorrect) + ' / ' + IntToStr(iQuestion);
  Eval += '; Success = ' + IntToStr(PCorrect) + '%';
  // Reset for new test
  NewTest(iTest, Eval, Colour, iQuestion, iCorrect);
end;

end.

