{*****************************************}
{* Main unit for Arithmetic4 application *}
{*****************************************}

unit arithmetic4_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TOperators = array[1 .. 4] of Char;
  { TForm1 }
  TForm1 = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;
    Addition: TCheckBox;
    Subtraction: TCheckBox;
    Multiplication: TCheckBox;
    Division: TCheckBox;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    NOperation: TEdit;
    Operation: TEdit;
    Result: TEdit;
    Evaluation: TEdit;
    ButtonStart: TButton;
    ButtonSolve: TButton;
    ButtonReset: TButton;
    ButtonQuit: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonSolveClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
  private
    iOperators, iQuestion, iCorrect, iNumerator, iDenominator: Integer;
    aOperators: TOperators;
    sOperation: string;
  end;

var
  Form1: TForm1;

procedure ClearForm(var Question, QCorrect: Integer);
procedure OperatorsList (var N: Integer; var Operators: TOperators);
procedure GenerateOperation (Operators: TOperators; NOperators: Integer; var Operation: string; var Numerator, Denominator: Integer);
function GCD (N1, N2: Integer): Integer;

implementation

{$R *.lfm}

{ Clear form controls and variables }

procedure ClearForm(var Question, QCorrect: Integer);

begin
  // Clear form controls
  Form1.NOperation.Text := '';
  Form1.Operation.Text := '';
  Form1.Result.Text := ''; Form1.Result.ReadOnly := True; Form1.Result.Tabstop := False;
  // Enable operators choice
  Form1.Addition.Enabled := True;
  Form1.Subtraction.Enabled := True;
  Form1.Multiplication.Enabled := True;
  Form1.Division.Enabled := True;
  // Enable/disable buttons
  Form1.ButtonStart.Caption := 'Start';
  Form1.ButtonReset.Enabled := False;
  Form1.ButtonSolve.Enabled := False;
  // Reset variables
  Question := 0; QCorrect := 0;
end;

{ Create operators list (for this training session }

procedure OperatorsList (var N: Integer; var Operators: TOperators);

var
  I: Integer;

begin
  N := 0;
  for I := 1 to 4 do
    Operators[I] := ' ';
  // Add operators to list as selected on application form
  if Form1.Addition.Checked then begin
    Inc(N); Operators[N] := '+';
  end;
  if Form1.Subtraction.Checked then begin
    Inc(N); Operators[N] := '-';
  end;
  if Form1.Multiplication.Checked then begin
    Inc(N); Operators[N] := 'x';
  end;
  if Form1.Division.Checked then begin
    Inc(N); Operators[N] := ':';
  end;
end;

{ Generate operation }

procedure GenerateOperation (Operators: TOperators; NOperators: Integer; var Operation: string; var Numerator, Denominator: Integer);

var
  MaxOperand, MaxResult, Operand1, Operand2, G, R: Integer;
  Result: Real;
  Oprator: Char;
  OperationOk: Boolean;

begin
  MaxOperand := 20; Maxresult := 100;
  // Random operator (from those selected)
  Oprator := Operators[Random(NOperators) + 1];
  // Repeat operation generation until "valid operation found"
  repeat
    OperationOk := True;
    // Generate operands (between 1 and 20; first division operand between 1 and 100 )
    if Oprator = ':' then
      Operand1 := Random(MaxResult) + 1
    else
      Operand1 := Random(MaxOperand) + 1;
    Operand2 := Random(MaxOperand) + 1;
    // At least 1 operand should be negative
    R := Random(3);
    case R of
      0: Operand1 := -Operand1;
      1: Operand2 := -Operand2;
      2: begin Operand1 := -Operand1; Operand2 := -Operand2; end;
    end;
    // Operation depending on current (random) operator
    if Oprator = '+' then
      // Addition
      Result := Operand1 + Operand2
    else if Oprator = '-' then
      // Subtraction
      Result := Operand1 - Operand2
    else if Oprator = 'x' then begin
      // Multiplication
      if (Abs(Operand1) = 1) or (Abs(Operand2) = 1) then                       // no multiplication with "1" or "-1" operand
        OperationOk := False
      else
        Result := Operand1 * Operand2;
    end
    else if Oprator = ':' then begin
      // Division
      if (Abs(Operand1) = 1) or (Abs(Operand2) = 1) then                       // no division with "1" or "-1" operand
        OperationOk := False
      else if Operand1 = Operand2 then                                         // no division with equal operands
        OperationOk := False
      else
        Result := Operand1 / Operand2;
    end;
    if OperationOk then
      if Abs(Result) > MaxResult then                                          // check if result is less than maximum imposed
        OperationOk := False;
    if OperationOk then begin
      // Transform result to fraction
      if Result = Round(Result) then begin
        // Result is an integer number
        Numerator := Round(Result); Denominator := 1;
      end
      else begin
        // Result is a fractional number: apply fraction reduction
        G := GCD(Abs(Operand1), Abs(Operand2));
        Numerator := Operand1 div G;
        Denominator := Operand2 div G;
      end;
      if (Numerator = Operand1) and (Denominator = Operand2) then              // fractional parts of result should be different from operands!
        OperationOk := False;
    end;
  until OperationOk;
  // Adapt signs of numerator and denominator
  if (Numerator < 0) and (Denominator < 0) then begin
    // Result is positive
    Numerator := Abs(Numerator); Denominator := Abs(Denominator);
  end
  else if Denominator < 0 then begin
    // Result is positive: Always set minus sign on numerator
    Numerator := -Numerator; Denominator := -Denominator;
  end;
  // Construct the operation string (negative numbers within brackets)
  Operation := '';
  if Operand1 < 0 then
    Operation := '(';
  Operation += IntToStr(Operand1);
  if Operand1 < 0 then
    Operation += ')';
  Operation += ' ' + Oprator + ' ';
  if Operand2 < 0 then
    Operation += '(';
  Operation += IntToStr(Operand2);
  if Operand2 < 0 then
    Operation += ')';
end;

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

{**********}
{* TForm1 *}
{**********}

{ Application start: Initialisation }

procedure TForm1.FormCreate(Sender: TObject);

begin
  ClearForm(iQuestion, iCorrect);
  Randomize;
end;

{ Button 'Start/Next': Generate arithmetic operation }

procedure TForm1.ButtonStartClick(Sender: TObject);

begin
  // Get operators list for this session
  if (ButtonStart.Caption = 'Start') then begin
    OperatorsList(iOperators, aOperators);
  end;
  // Procced only if at least 1 operator has been selected
  if iOperators > 0 then begin
    Evaluation.Text := ''; Evaluation.Color := clDefault; Evaluation.Font.Color := clDefault;
    if (ButtonStart.Caption = 'Start') then begin
      // Disable operators selection
      Addition.Enabled := False;
      Subtraction.Enabled := False;
      Multiplication.Enabled := False;
      Division.Enabled := False;
      ButtonStart.Caption := 'Next';
    end;
    // Generate exercise
    Inc(iQuestion);
    GenerateOperation(aOperators, iOperators, sOperation, iNumerator, iDenominator);
    // Set values of form fields as required
    NOperation.Text := 'Operation ' + IntToStr(iQuestion);
    Operation.Text := sOperation + ' = ?';
    Result.Text := ''; Evaluation.Text := '';
    Result.ReadOnly := False; Result.TabStop := True; Result.SetFocus;
    ButtonSolve.Enabled := True;
    ButtonReset.Enabled := True;
  end
  // No operator selected: Error message
  else
    MessageDlg('Invalid selection', 'You must choose at least 1 operator!', mtError, [mbOK], 0);
end;

{ Button 'Solve': Check the operation result entered by the user}

procedure TForm1.ButtonSolveClick(Sender: TObject);

var
  UserResult, UserNumerator, UserDenominator, P, C: Integer;
  SUserResult, SUserResult1, SUserResult2, S: string;
  Correct: Boolean;

begin
  Correct := False;
  // Check user entry against operation result
  SUserResult := Result.Text;
  Val(SUserResult, UserResult, C);
  if (C = 0) and (UserResult = Round(UserResult)) then begin
    // User entered an integer number
    UserNumerator := UserResult; UserDenominator := 1;
    if (UserNumerator = iNumerator) and (UserDenominator = iDenominator) then
      // User entry is correct answer
      Correct := True;
  end
  else begin
    // User entered something else
    P := Pos('/', SUserResult);
    if P <> 0 then begin
      // User entry contains a '/' indicating a fraction: analyze it!
      SUserResult1 := LeftStr(SUserResult, P - 1); SUserResult2 := '';
      if Length(SUserResult) > P then
        SUserResult2 := RightStr(SUserResult, Length(SUserResult) - P);
      Val(SUserResult1, UserNumerator, C);
      if (C = 0) and (UserNumerator = Round(UserNumerator)) then begin
        Val(SUserResult2, UserDenominator, C);
        if (C = 0) and (UserDenominator = Round(UserDenominator)) then begin
          // User entry is effectively a fraction
          if (UserNumerator = iNumerator) and (UserDenominator = iDenominator) then
            // User entry is correct answer
            Correct := True;
        end;
      end;
    end;
  end;
  if Correct then begin
    // User answer is correct
    Evaluation.Font.Color := clLime;
    Evaluation.Text := 'Your solution is correct!';                            // display message (in green)
    Inc(iCorrect);
  end
  else begin
    // User answer is false
    Evaluation.Font.Color := clRed;
    S := 'Correct solution is: ' + IntToStr(iNumerator);
    if iDenominator <> 1 then
      S += '/' + IntToStr(iDenominator);
    Evaluation.Text := S;                                                      // display correct result (in red)
  end;
  // Enable/disable form buttons
  ButtonSolve.Enabled := False;
  ButtonReset.Enabled := True;
  ButtonStart.SetFocus;
end;

{ Button 'Reset': Evaluate results; reset application }

procedure TForm1.ButtonResetClick(Sender: TObject);

var
  S: string;
  PCorrect: Integer;

begin
  // Evaluate user answer (if there were at least 5 operations to solve)
  S := '';
  if iQuestion >= 5 then begin
    PCorrect :=  100 * iCorrect div iQuestion;
    if PCorrect < 50 then
      Evaluation.Color := clRed
    else if PCorrect < 60 then
      Evaluation.Color := clYellow
    else
      Evaluation.Color := clLime;
    S := 'Correct answers = ' + IntToStr(iCorrect) + ' / ' + IntToStr(iQuestion);
    S += '; Success = ' + IntToStr(PCorrect) + '%';
    Evaluation.Text := S; Evaluation.Font.Color := clDefault;
  end;
  ClearForm(iQuestion, iCorrect);
end;

{ Button 'Quit' : Terminate the program}

procedure TForm1.ButtonQuitClick(Sender: TObject);

begin
  close();
end;

end.

