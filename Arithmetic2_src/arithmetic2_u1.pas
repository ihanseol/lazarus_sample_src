//
// Arithmetic2 main unit
//

unit arithmetic2_u1;

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
    Label2: TLabel;
    Label3: TLabel;
    Addition: TCheckBox;
    Subtraction: TCheckBox;
    Multiplication: TCheckBox;
    Division: TCheckBox;
    DisplayFraction: TRadioButton;
    DisplayMixed: TRadioButton;
    NOperation: TEdit;
    Operation: TEdit;
    Result: TEdit;
    Evaluation: TEdit;
    ButtonStart: TButton;
    ButtonSolve: TButton;
    ButtonReset: TButton;
    ButtonQuit: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonSolveClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
  end;

var
  N, Correct, NOperators, IntOperations: Integer;
  OpOperation, OpResult: string;
  Operators: TOperators;
  Form1: TForm1;

implementation

{$R *.lfm}

{ Create operators list (for this training session) }

procedure OperatorsList (var N: Integer; var Op: TOperators);

begin
  N := 0;
  // Add operators to list as selected on application form
  if Form1.Addition.Checked then begin
    Inc(N); Op[N] := '+';
  end;
  if Form1.Subtraction.Checked then begin
    Inc(N); Op[N] := '-';
  end;
  if Form1.Multiplication.Checked then begin
    Inc(N); Op[N] := 'x';
  end;
  if Form1.Division.Checked then begin
    Inc(N); Op[N] := ':';
  end;
end;

{ Construct operand string value (fraction) }

function OperandString(O1, O2: Integer): string;

var
  N, R : Integer;
  Op : string;

begin
  if (O1 mod O2) = 0 then
    //Non-fractional values as integer number
    Op := IntToStr(O1 div O2)
  else begin
    if Form1.DisplayFraction.Checked then
      // Fractional values as fraction
      Op := IntToStr(O1) + '/' + IntToStr(O2)
    else begin
      // Fractional values as mixed numbers
      if O1 < O2 then
        Op := IntToStr(O1) + '/' + IntToStr(O2)                           // a simple fraction in this case
      else begin
        N := O1 div O2;
        R := O1 mod O2;
        Op := IntToStr(N) + ' ';                                          // the integer part of the mixed number
        Op := Op + IntToStr(R) + '/' + IntToStr(O2);                      // the fractional part of the mixed number
      end;
    end;
  end;
  OperandString := Op;
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

{ Least Common Multiple }

function LCM (N1, N2: Integer): Integer;

var
  G : Integer;

begin
  G := GCD(N1, N2);
  LCM := (N1 * N2) div G;
end;

{ Addition of two fractions }

procedure FractionAdd(O11, O12, O21, O22: Integer; var R1, R2: Integer);

begin
  R2 := LCM(O12, O22);
  R1 := (O11 * (R2 div O12)) + (O21 * (R2 div O22));
end;

{ Subtraction of two fractions }

procedure FractionSubtract(O11, O12, O21, O22: Integer; var R1, R2: Integer);

begin
  R2 := LCM(O12, O22);
  R1 := (O11 * (R2 div O12)) - (O21 * (R2 div O22));
end;

{ Multiplication of two fractions }

procedure FractionMultiply(O11, O12, O21, O22: Integer; var R1, R2: Integer);

begin
  R1 := O11 * O21;
  R2 := O12 * O22;
end;

{ Division of two fractions }

procedure FractionDivide(O11, O12, O21, O22: Integer; var R1, R2: Integer);

begin
  R1 := O11 * O22;
  R2 := O12 * O21;
end;

{ Generate arithmetic operation (fractions) }

procedure GenerateOperation (Ops: TOperators; var Op, R: string);

var
  Oprator : Char;
  O11, O12, O21, O22, R1, R2, G : Integer;
  OperationOk : Boolean;

begin
  Oprator := Ops[Random(NOperators) + 1];                                 // random operator (from those selected)
  repeat
    OperationOk := True;
    O11 := Random(11); O21 := Random(11);                                 // random numerators (between 0 and 10)
    O12 := Random(10) + 1; O22 := Random(10) + 1;                         // random denominators (between 1 and 10)
    if (O11 = 0) or (O21 = 0) then                                        // no operations with "0" operands
      OperationOk := False
    else begin
      if ((O11 mod O12) = 0) and ((O21 mod O22) = 0) then                 // no operations with integers only
        OperationOk := False
      else if ((O11 mod O12) = 0) or ((O21 mod O22) = 0) then begin
        if IntOperations > (N div 10) then                                // limit operations with integer operand to 10%
          OperationOk := False
        else
          Inc(IntOperations);
      end;
    end;
    if OperationOk then begin
      R1 := 0; R2 := 1;
      // Operation depending on current operator
      if Oprator = '+' then
        // Addition
        FractionAdd(O11, O12, O21, O22, R1, R2)
      else if Oprator = '-' then begin
        // Subtraction
        if (O11 / O12) < (O21 / O22) then                                 // no subtraction with negative results
          OperationOk := False
        else if (O11 = O21) and (O12 = O22) then                          // no subtraction with equal operands
          OperationOk := False
        else
          FractionSubtract(O11, O12, O21, O22, R1, R2);
      end
      else if Oprator = 'x' then begin
        // Multiplication
        if (O11 = O12) or (O21 = O22) then                                // no multiplication with "1" operands
            OperationOk := False
        else
          FractionMultiply(O11, O12, O21, O22, R1, R2);
      end
      else if Oprator = ':' then begin
        // Division
        if (O11 = O12) or (O21 = O22) then                                // no division with "1" operands
            OperationOk := False
        else if (O11 = O21) and (O12 = O22) then                          // no division with equal operands
          OperationOk := False
        else
          FractionDivide(O11, O12, O21, O22, R1, R2);
      end;
    end;
  until OperationOk;
  // Reduction of the result
  G := GCD(R1, R2);
  R1 := R1 div G;
  R2 := R2 div G;
  // Construct the string value for this operation
  Op := OperandString(O11, O12);
  Op := Op + '  ' + Oprator + '  ';
  Op := Op + OperandString(O21, O22);
  R  := OperandString(R1, R2);
end;

{ TForm1 }

{ Button 'Start/Next': Init application; generate arithmetic operation }

procedure TForm1.ButtonStartClick(Sender: TObject);

begin
  if (ButtonStart.Caption = 'Start') then
    OperatorsList(NOperators, Operators);                                 // operators list for this session
  if NOperators > 0 then begin                                            // at least 1 operator must be selected
    if (ButtonStart.Caption = 'Start') then begin
      // Initialize the application
      Randomize;
      N := 0; Correct := 0; IntOperations := 0;
      // Set startup values of form fields as required
      Addition.Enabled := False;
      Subtraction.Enabled := False;
      Multiplication.Enabled := False;
      Division.Enabled := False;
      DisplayFraction.Enabled := False;
      DisplayMixed.Enabled := False;
      Result.ReadOnly := True;
      Result.Tabstop := False;
      ButtonStart.Caption := 'Next';
    end;
    // Do this for all operations
    N := N + 1;
    GenerateOperation(Operators, OpOperation, OpResult);                  // generate a random operation
    // Set values of form fields as required
    NOperation.Text := 'Operation ' + IntToStr(N);
    Operation.Text := OpOperation + '  =  ?';
    Result.Text := '';
    Evaluation.Text := '';
    Result.ReadOnly := False;
    Result.TabStop := True;
    Result.SetFocus;
    ButtonSolve.Enabled := True;
    ButtonReset.Enabled := True;
  end;
end;

{ Button 'Solve': Check the operation result entered by the user}

procedure TForm1.ButtonSolveClick(Sender: TObject);

var
  UserResult, S : string;

begin
  // Check user entry against operation result
  UserResult := Result.Text;
  if UserResult = OpResult then begin                                     // user entry is correct answer
    Evaluation.Font.Color := clLime;
    Evaluation.Text := 'Your solution is correct!';                       // display message (in green)
    Inc(Correct);
  end
  else begin                                                              // user entry is not numeric or false answer
    Evaluation.Font.Color := clRed;
    S := 'Correct solution is: ';
    S := S + OpResult;
    Evaluation.Text := S;                                                 // display correct result (in red)
  end;
  ButtonSolve.Enabled := False;
  ButtonReset.Enabled := True;
  ButtonStart.SetFocus;
end;

{ Button 'Reset': Evaluate results; re-init application }

procedure TForm1.ButtonResetClick(Sender: TObject);

var
  S: string;
  PCorrect: Integer;

begin
  // Evaluate user answers (if there were at least 5 operations to solve)
  S := '';
  if N >= 5 then begin
    PCorrect :=  100 * Correct div N;
    if PCorrect >= 50 then
      Evaluation.Font.Color := clLime
    else
      Evaluation.Font.Color := clRed;
    S := 'Correct answers = ' + IntToStr(Correct) + ' / ' + IntToStr(N);
    S := S + '; Success = ' + IntToStr(PCorrect) + '%';
  end;
  // Reset the form fields
  Addition.Enabled := True;
  Subtraction.Enabled := True;
  Multiplication.Enabled := True;
  Division.Enabled := True;
  DisplayFraction.Enabled := True;
  DisplayMixed.Enabled := True;
  NOperation.Text := '';
  Operation.Text := '';
  Evaluation.Text := S;
  Result.Text := '';
  Result.ReadOnly := True;
  Result.Tabstop := False;
  ButtonStart.Caption := 'Start';
  ButtonStart.SetFocus;
  ButtonSolve.Enabled := False;
  ButtonReset.Enabled := False;
end;

{ Button 'Quit' : Terminate the program}

procedure TForm1.ButtonQuitClick(Sender: TObject);

begin
  close();
end;

end.

