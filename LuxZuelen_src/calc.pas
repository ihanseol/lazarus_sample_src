{***************************************}
{* Main unit for LuxZuelen application *}
{***************************************}

unit calc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, PopupNotifier, LazUTF8;

type
  TOperators = array of Char;
  {**********}
  {* TfCalc *}
  {**********}
  TfCalc = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    Label1: TLabel;
    edQuestions: TEdit;
    Label2: TLabel;
    cbPlus: TCheckBox;
    cbMinus: TCheckBox;
    cbMultiply: TCheckBox;
    cbDivide: TCheckBox;
    edHelp: TEdit;
    laQuestion: TLabel;
    edQuestion: TEdit;
    edAnswer: TEdit;
    edEval: TEdit;
    Label3, Label4, Label5: TLabel;
    edExQuestion: TEdit;
    edExCorrect: TEdit;
    edExMarks: TEdit;
    btQuestion: TButton;
    btAe, btOe, btUe, btAccAigu, btTrema: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAeClick(Sender: TObject);
    procedure btOeClick(Sender: TObject);
    procedure btUeClick(Sender: TObject);
    procedure btAccAiguClick(Sender: TObject);
    procedure btTremaClick(Sender: TObject);
  private
    iQuestions, iQuestion, iCorrect, iNumber1, iNumber2, iAnswer: Integer;
    cOperator: Char;
    aOperators: TOperators;
  end;

var
  fCalc: TfCalc;

implementation

{$R *.lfm}

{ Function to check if a letter is one of those where the Eiffel rule says to drop the "n" preceding it }

function IsEiffelChar(Letter: string): Boolean;

const
  EiffelChars: array[1 .. 16] of string = (
    'b', 'c', 'f', 'g', 'j', 'k', 'l', 'm', 'p', 'q', 'r', 's', 'v', 'w', 'x', 'y'
  );

var
  I: Integer;
  IsEiffel: Boolean;

begin
  IsEiffel := False;
  for I := 1 to 16 do
    if Letter = EiffelChars[I] then
      IsEiffel := True;
  IsEiffelChar := IsEiffel;
end;

{ Luxembourgish orthography: Eiffel rule }

function AdjustEiffel(Word1, Word2: string): string;

{ If a word ends in -n and the following word starts with a consonant other than d,t,z,h,n than the "n" is dropped }

begin
  if RightStr(Word1, 1) = 'n' then begin
    if IsEiffelChar(UTF8Copy(Word2, 1, 1)) then begin
      Word1 := UTF8Copy(Word1, 1, UTF8Length(Word1) - 1);
      if RightStr(Word1, 1) = 'n' then                                         // if there was a second "n", drop this one, too
        Word1 := UTF8Copy(Word1, 1, UTF8Length(Word1) - 1);
    end;
  end;
  AdjustEiffel := Word1;
end;

{ Luxembourgish name of number (0 - 100)}

function LuxNumber(N: Integer): string;

const Numbers: array[0..19] of string = (
  'null', 'eent', 'zwee', 'dräi', 'véier', 'fënnef', 'sechs', 'siwen', 'aacht', 'néng',
  'zéng', 'elef', 'zwielef', 'dräizéng', 'véierzéng', 'fofzéng', 'siechzéng', 'siwenzéng', 'uechtzéng', 'nonzéng'
);
const Numbers10: array[2..10] of string = (
  'zwanzeg', 'drësseg', 'véierzeg', 'fofzeg', 'sechzeg', 'siwenzeg', 'achtzeg', 'nonzeg', 'honnert'
);

var
  N1, N2: Integer;
  SN, SN1, SN2, LuxAnd: string;

begin
  // "Special" names for numbers between 0 and 19
  if N <= 19 then
    SN := Numbers[N]
  // Numbers between 20 and 100: combinations of multiples of 10 and unit numbers
  else begin
    N1 := N div 10; N2 := N mod 10;
    // Name of multiple of 10
    SN1 := Numbers10[N1];
    // There are no units: name of number is name of multiple of 10
    if N2 = 0 then
      SN := SN1
    // There are units: name is a compound word: unit name + "an" + multiple of 10 name
    else begin
      // Unit name: "eent" becomes "een" if used within another number
      if N2 = 1 then
        SN2 := 'een'
      // Unit name: other numbers
      else
        SN2 := Numbers[N2];
      // The Eiffel rule has to be applied within the compound word: "an" has eventually to be adapted
      LuxAnd := AdjustEiffel('an', SN1);
      SN := SN2 + LuxAnd;
      // Special case for 60s and 70s: the 's' has to be doubled
      if N1 in [6, 7] then
        SN += 's';
      // Final compound word number name
      SN += SN1;
    end;
  end;
  LuxNumber := SN;
end;

{ Luxembourgish name of maths operators }

function LuxOperator(Op: Char): string;

var
  LuxOp: string;

begin
  case Op of
    '+': LuxOp := 'plus';
    '-': LuxOp := 'minus';
    'x': LuxOp := 'mol';
    ':': LuxOp := 'gedeelt duurch';
  end;
  LuxOperator := LuxOp;
end;

{ Preparation of new exercise: Clear form fields and counters; apply selected settings }

procedure NewExercise(out Questions, Question, QCorrect: Integer; out Operators: TOperators);

begin
  // Get number of exercise questions (as entered by user)
  if fCalc.edQuestions.Text = '' then
    Questions := 0
  else
    Questions := StrToInt(fCalc.edQuestions.Text);
  if Questions < 5 then                                                        // minimum of questions arbitrarily fixed to 5
    MessageDlg('Rechnen op lëtzebuergesch', 'Fehler: D''Zuel vun de Froe muss mindestens 5 sinn!', mtError, [mbOK], 0)
  else begin
    // Clear counters
    Question := 0; QCorrect := 0;
    // Create array with operators to be used for exercise (as selected by user)
    SetLength(Operators, 0);
    if fCalc.cbPlus.Checked then begin
      SetLength(Operators, 1);
      Operators[0] := '+';
    end;
    if fCalc.cbMinus.Checked then begin
      SetLength(Operators, Length(Operators) + 1);
      Operators[Length(Operators) - 1] := '-';
    end;
    if fCalc.cbMultiply.Checked then begin
      SetLength(Operators, Length(Operators) + 1);
      Operators[Length(Operators) - 1] := 'x';
    end;
    if fCalc.cbDivide.Checked then begin
      SetLength(Operators, Length(Operators) + 1);
      Operators[Length(Operators) - 1] := ':';
    end;
    if Length(Operators) = 0 then
      MessageDlg('Rechnen op lëtzebuergesch', 'Fehler: ''t muss mindestens 1 Operateur ausgewielt sinn!', mtError, [mbOK], 0);
  end;
  // Clear form fields
  fCalc.laQuestion.Caption := 'Fro';
  fCalc.edQuestion.Text := '';
  fCalc.edAnswer.Text := '';
  fCalc.edEval.Text := '';
  fCalc.edEval.Color := clDefault;
  fCalc.edExQuestion.Text := '';
  fCalc.edExCorrect.Text := '';
  fCalc.edExMarks.Text := '';
  fCalc.edExMarks.Color := clDefault;
  fCalc.btQuestion.Caption := 'Fro';
end;

{ Get random operands (2 integers between 1 and 100)}

procedure GetOperands(Op: Char; out N1, N2, N: Integer);

var
  OK: Boolean;

begin
  repeat
    OK := True;
    N1 := Random(100) + 1; N2 := Random(100) + 1;                              // random numbers between 1 and 100
    case Op of
      '+': N := N1 + N2;
      '-': N := N1 - N2;
      'x': N := N1 * N2;
      ':': if N2 = 0 then                                                      // can't divide by 0
             OK := False
           else
             N := N1 div N2;
    end;
    if (N < 0) or (N > 100) then                                               // result must be between 0 and 100
      OK := False
    else if N1 / N2 <> Int(N1 / N2) then                                       // result must be an integer
      OK := False
    else if ((Op = 'x') or (Op = ':')) and ((N1 = 1) or (N2 = 1)) then         // exclude operations like N * 1, N / 1 (answer is in question)
      OK := False
    else if (Op = ':') and (N1 = N2) then                                      // exclude operations N / N (may be generated to often)
      OK := False
  until OK;
end;

{**********}
{* TfCalc *}
{**********}

{ Application start: Initialisation }

procedure TfCalc.FormCreate(Sender: TObject);

begin
  SetLength(aOperators, 0);
  NewExercise(iQuestions, iQuestion, iCorrect, aOperators);
  Randomize;
end;

{ Menu item "Exercice > Neien Exercice": Prepare for new exercise }

procedure TfCalc.mExerciseNewClick(Sender: TObject);

begin
  NewExercise(iQuestions, iQuestion, iCorrect, aOperators);
end;

{ Menu item "Exercice > Verloossen": Exit application }

procedure TfCalc.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Hëllef > Iwwer": Display program about }

procedure TfCalc.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'LuxZuelen ass een einfache Recheprogramm, mat de Basisoperatioune vun Zuelen zwëschent 1 an 100, ';
    S += 'woubäi de lëtzebuergesche Numm vun den Zuele benotzt gëtt.' + Chr(13);
    S += 'Versioun 1.0, © allu, Oktober-November, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Fro/Äntwert": Generate new question resp. check user answer }

procedure TfCalc.btQuestionClick(Sender: TObject);

var
  P: Integer;

begin
  // Button "Fro": Generate new exercise question
  if btQuestion.Caption = 'Fro' then begin
    // Do this only if settings are correct and not yet all questions done
    if (Length(aOperators) > 0) and (iQuestions >= 5) and (iQuestion < iQuestions) then begin
      Inc(iQuestion);
      laQuestion.Caption := 'Fro ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
      // Random operator
      cOperator := aOperators[Random(Length(aOperators))];
      // Random operands
      GetOperands(cOperator, iNumber1, iNumber2, iAnswer);
      // Question text (with luxembourgish name of numbers and operator)
      edQuestion.Text := LuxNumber(iNumber1) + ' ' + LuxOperator(cOperator) + ' ' + LuxNumber(iNumber2);
      // Clear answer and evaluation fields; set focus for user answer
      edAnswer.Text := ''; edEval.Text := ''; edEval.Color := clDefault;
      edAnswer.SetFocus;
      // Change button caption (next push will be an answer)
      btQuestion.Caption := 'Äntwert';
    end;
  end
  // Button "Äntwert": Check user answer and update evaluation counters
  else begin
    if iQuestion <= iQuestions then begin
      // Correct answer
      if edAnswer.Text = LuxNumber(iAnswer) then begin
        Inc(iCorrect);
        edEval.Text := 'Dat ass richteg!';
        edEval.Color := clDefault;
      end
      // False answer
      else begin
        //edEval.Text := 'Dat ass net richteg!';
        edEval.Text := LuxNumber(iAnswer);                                     // display correct answer
        edEval.Color := clRed;
      end;
      // Update counters
      edExQuestion.Text := IntToStr(iQuestion);
      edExCorrect.Text := IntToStr(iCorrect);
      // Evaluation: Calculate marks as is done at school in Luxembourg (maximum = 60)
      P := Round(60 * (iCorrect / iQuestion));
      edExMarks.Text := IntToStr(P);
      // Different colors for different marks ranges
      if 100 * (P / 60) >= 60 then
        edExMarks.Color := clLime
      else if 100 * (P / 60) >= 50 then
        edExMarks.Color := clYellow
      else
        edExMarks.Color := clRed;
    end;
    // Change button caption (next push will be another question)
    btQuestion.Caption := 'Fro';
    // If all questions have been done, display message
    if iQuestion = iQuestions then begin
      MessageDlg('Rechnen op lëtzebuergesch', 'Enn vum Exercice...', mtInformation, [mbOK], 0);
      Inc(iQuestion);                                                          // this avoids that message is displayed more than once
    end;
  end;
end;

{ Non-English letters buttons: Include letter in user answer }

procedure TfCalc.btAeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ä';
end;

procedure TfCalc.btOeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ö';
end;

procedure TfCalc.btUeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ü';
end;

procedure TfCalc.btAccAiguClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'é';
end;

procedure TfCalc.btTremaClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ë';
end;

end.

