//
// Arithmetic1 main unit
//

unit arithmetic1_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TOperators = array[1 .. 7] of Char;
  { TForm1 }
  TForm1 = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;
    Max10: TRadioButton;
    Max100: TRadioButton;
    Max1000: TRadioButton;
    ResultLimit: TCheckBox;
    Label2: TLabel;
    Addition: TCheckBox;
    Subtraction: TCheckBox;
    Multiplication: TCheckBox;
    Division: TCheckBox;
    Label3: TLabel;
    MultiAdd: TCheckBox;
    MultiSub: TCheckBox;
    MultiAddSub: TCheckBox;
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
    procedure Max10Change(Sender: TObject);
    procedure Max100Change(Sender: TObject);
    procedure Max1000Change(Sender: TObject);
  end;

var
  N, Correct, NOperators, OpResult: Integer;
  OpOperation: string;
  Operators: TOperators;
  Add0, Sub0, SubEqual, Mul0, Mul1, Div0, Div1, DivEqual: Boolean;
  Form1: TForm1;

implementation

{$R *.lfm}

{ Create operators list (for this training session }

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
  if Form1.MultiAdd.Checked then begin
    Inc(N); Op[N] := 'a';
  end;
  if Form1.MultiSub.Checked then begin
    Inc(N); Op[N] := 's';
  end;
  if Form1.MultiAddSub.Checked then begin
    Inc(N); Op[N] := 'b';
  end;
end;

{ Generate single-operator operation }

procedure GenerateSingle(O: Char; M: Integer; MU: Boolean; MO: Integer; var Op: string; var R: Integer);

var
  O1, O2: Integer;
  OperationOk: Boolean;

begin
  repeat
    OperationOk := True;
    // Generate operands
    if MU then                                                  // part of multiple operators operation (except 1st one!)
      O1 := MO                                                  // 1st operand is result of previous simple operation
    else                                                        // simple operation or 1st part of multiple operators operation
      O1 := Random(M + 1);                                      // 1st operand as random integer
    O2 := Random(M + 1);                                        // 2nd operand (always) as random integer
    R := 0;
    // Operation depending on current (random) operator
    if O = '+' then                                             // addition
      begin
        if Add0 and ((O1 = 0) or (O2 = 0)) then                 // addition with "0" operand only once/not at all
          OperationOk := False
        else begin
          R := O1 + O2;
          if (O1 = 0) or (O2 = 0) then
            Add0 := True;
        end;
      end
    else if O = '-' then                                        // subtraction
      begin
        if not Mu then begin                                    // for multiple operators operations, test operators validity in "GenerateMulti" (not here)
          if O1 < O2 then                                       // discard operations with negative results
            OperationOk := False;
        end;
        if OperationOk then begin
          if Sub0 and ((O1 = 0) or (O2 = 0)) then               // subtraction with "0" operand only once/not at all
            OperationOk := False
          else begin
            if (O1 = 0) or (O2 = 0) then
              Sub0 := True;
          end;
          if SubEqual and (O1 = O2) then                        // subtraction with same operands only once/not at all
            OperationOk := False
          else begin
            if O1 = O2 then
              SubEqual := True;
          end;
        end;
        if OperationOk then
          R := O1 - O2;
      end
    else if O = 'x' then                                        // multiplication
      begin
        if Mul0 and ((O1 = 0) or (O2 = 0)) then                 // multiplication with "0" operand only once/not at all
          OperationOk := False
        else begin
          if (O1 = 0) or (O2 = 0) then
            Mul0 := True;
        end;
        if Mul1 and ((O1 = 1) or (O2 = 1)) then                 // multiplication with "1" operand only once/not at all
          OperationOk := False
        else begin
          if (O1 = 1) or (O2 = 1) then
            Mul1 := True;
        end;
        if OperationOk then
          R := O1 * O2
      end
    else if O = ':' then                                        // division
      begin
        if O2 = 0 then                                          // can't divide by 0
          OperationOk := False
        else if O1 mod O2 <> 0 then                             // discard operations with fractional results
          OperationOk := False
        else begin
          if Div0 and (O1 = 0) then                             // division with "0" operand only once/not at all
            OperationOk := False
          else begin
            if O1 = 0 then
              Div0 := True;
          end;
          if Div1 and ((O1 = 1) or (O2 = 1)) then               // division with "1" operand only once/not at all
            OperationOk := False
          else begin
            if (O1 = 1) or (O2 = 1) then
              Div1 := True;
          end;
          if DivEqual and (O1 = O2) then                        // division with equal operands only once/not at all
            OperationOk := False
          else begin
            if O1 = O2 then
              DivEqual := True;
          end;
        end;
        if OperationOk then
          R := O1 div O2;
      end;
    if not Mu then begin                                        // for multiple operators operations, test result validity in "GenerateMulti" (not here)
      if Form1.ResultLimit.Checked and (R > M) then             // check if result is less than limit selected
        OperationOk := False;
    end;
  until OperationOk;
  Op := IntToStr(O1) + ' ' + O + ' ' + IntToStr(O2);            // construct the string value for this operation
end;

{ Generate multi-operator operation }

procedure GenerateMulti(O: Char; M: Integer; var Op: string; var R: Integer);

var
  NOperators, Result, Operand1, Oprator: Integer;
  NPlus, NMinus, I, P: Integer;
  Operation: string;
  Operators: TOperators;
  Multi, OperatorOk, ResultOk: Boolean;

begin
  NOperators := Random(3) + 2;                                  // 2, 3 or 4 operators
  // Create operator list
  repeat
    OperatorOk := True;
    NPlus := 0; NMinus := 0;
    for I := 1 to NOperators do begin
      if O = 'a' then                                           // multiple additions
        Operators[I] := '+'
      else if O = 's' then                                      // multiple subtractions
        Operators[I] := '-'
      else begin                                                // multiple additions and subtractions
        Oprator := Random(2);                                   // random operator
        if Oprator = 0 then begin                               // addition
          Operators[I] := '+';
          Inc(NPlus);
        end
        else begin
          Operators[I] := '-';                                  // subtraction
          Inc(NMinus);
        end;
      end;
    end;
    if (O = 'b') and ((NPlus = 0) or (NMinus = 0)) then         // multiple add/sub must be additions AND subtractions
      OperatorOk := False;
  until OperatorOk;
  // Consider multiple operators operation as sequene of single operators operations
  repeat
    ResultOk := True; Operation := ''; Result := 0;
    for I := 1 to NOperators do begin                           // for each operator in the list
      if I = 1 then begin                                       // first operation (of sequence)
        Multi := False;                                         // treat the same way as single operator operation
        Operand1 := 0;                                          // dummy value
      end
      else begin                                                // all other operations (of sequence)
        Multi := True;                                          // treat in a different way than single operator operation
        Operand1 := Result;                                     // second operand of single operator operation is result from previous operation
      end;
      // Generate the single operator operation
      GenerateSingle(Operators[I], M, Multi, Operand1, Operation, Result);
      // Construct the string value for this operation
      if I = 1 then                                             // first operation (of sequence)
        Op := Operation                                         // the string is the value of the single operator operation
      else begin                                                // all other operations (of sequence)
        P := Pos(Operators[I], Operation);
        Op := Op + ' ' + Copy(Operation, P, Length(Operation) - P + 1); // the string is the operator and the 2nd operand of the single operator operation
      end;
    end;
    // Global result is result of last operation (in sequence)
    R := Result;
    // Check result validity (result limits)
    // This check has to be done here (and not in "GenerateSingle", because (except for the 1st operation) the first operand is a fix value, i.e. the
    // result of the previous operation. If this value equals the result limits (0 resp. maximum selected), it's not possible to find a second (random)
    // operator and the program would hang in an infinite loop!
    if (R < 0) or (Form1.ResultLimit.Checked and (R > M)) then
      ResultOk := False;
  until ResultOk;
end;

{ Generate basic arithmetic operation }

procedure GenerateOperation (Ops: TOperators; var Op: string; var R: Integer);

var
  Max : Integer;
  Oprator: Char;

begin
  Oprator := Ops[Random(NOperators) + 1];                  // random operator (from those selected)
  if Form1.Max10.Checked then                              // operand maximum (as selected)
    Max := 10
  else if Form1.Max100.Checked then
    Max := 100
  else
    Max := 1000;
  if Oprator in ['a', 's', 'b'] then                       // operation with multiple operators
    GenerateMulti(Oprator, Max, Op, R)
  else                                                     // operation with one single operator
    GenerateSingle(Oprator, Max, False, 0, Op, R);
end;

{ TForm1 }

{ Button 'Start/Next': Init application; generate arithmetic operation }

procedure TForm1.ButtonStartClick(Sender: TObject);

begin
  if (ButtonStart.Caption = 'Start') then
    OperatorsList(NOperators, Operators);                       // operators list for this session
  if NOperators > 0 then begin                                  // at least 1 operator must be selected
    if (ButtonStart.Caption = 'Start') then begin
      // Initialize the application
      Randomize;
      N := 0; Correct := 0;
      if Max10.Checked then begin                               // if max = 10, allow 1 "simple" operation
        Add0 := False; Sub0 := False; SubEqual := False;
        Mul0 := False; Mul1 := False;
        Div0 := False; Div1:= False; DivEqual := False;
      end
      else begin                                                // if max <> 10, don't allow any "simple" operation
        Add0 := True; Sub0 := True; SubEqual := True;
        Mul0 := True; Mul1 := True;
        Div0 := True; Div1:= True; DivEqual := True;
      end;
      // Set startup values of form fields as required
      Max10.Enabled := False;
      Max100.Enabled := False;
      Max1000.Enabled := False;
      ResultLimit.Enabled := False;
      Addition.Enabled := False;
      Subtraction.Enabled := False;
      Multiplication.Enabled := False;
      Division.Enabled := False;
      MultiAdd.Enabled := False;
      MultiSub.Enabled := False;
      MultiAddSub.Enabled := False;
      Result.ReadOnly := True;
      Result.Tabstop := False;
      ButtonStart.Caption := 'Next';
    end;
    // Do this for all operations
    N := N + 1;                                                 // actual operation
    GenerateOperation(Operators, OpOperation, OpResult);        // generate a random operation
    // Set values of form fields as required
    NOperation.Text := 'Operation ' + IntToStr(N);              // actual operation
    Operation.Text := OpOperation + ' = ?';                     // operation string value
    Result.Text := '';                                          // operation result (to be entered by the user)
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
  UserResult, C: Integer;
  S: string;

begin
  // Check user entry against operation result
  S := Result.Text;
  Val(S, UserResult, C);
  if (C = 0) and (UserResult = OpResult) then begin             // user entry is correct answer
    Evaluation.Font.Color := clLime;
    Evaluation.Text := 'Your solution is correct!';             // display message (in green)
    Inc(Correct);
  end
  else begin                                                    // user entry is not numeric or false answer
    Evaluation.Font.Color := clRed;
    S := 'Correct solution is: ';
    S := S + IntToStr(OpResult);
    Evaluation.Text := S;                                       // display correct result (in red)
  end;
  // Enable disable form buttons
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
  // Evaluate user answer (if there were at least 5 operations to solve)
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
  Max10.Enabled := True;
  Max100.Enabled := True;
  Max1000.Enabled := True;
  ResultLimit.Enabled := True;
  Addition.Enabled := True;
  Subtraction.Enabled := True;
  Multiplication.Enabled := True;
  Division.Enabled := True;
  if Max10.Checked then begin
    MultiAdd.Enabled := False;
    MultiSub.Enabled := False;
    MultiAddSub.Enabled := False;
    ResultLimit.Enabled := False;
  end
  else begin
    MultiAdd.Enabled := True;
    MultiSub.Enabled := True;
    MultiAddSub.Enabled := True;
    ResultLimit.Enabled := True;
  end;
  NOperation.Text := '';
  Operation.Text := '';
  Evaluation.Text := S;
  Result.Text := '';
  Result.ReadOnly := True;
  Result.Tabstop := False;
  ButtonStart.Caption := 'Start';
  ButtonStart.SetFocus;
  ButtonReset.Enabled := False;
  ButtonSolve.Enabled := False;
end;

{ Button 'Quit' : Terminate the program}

procedure TForm1.ButtonQuitClick(Sender: TObject);

begin
  close();
end;

{ Disable multi-operators and result limited operations for operand maximum = 10 }

procedure TForm1.Max10Change(Sender: TObject);

begin
  if Max10.Checked then begin
    MultiAdd.Enabled := False;
    MultiSub.Enabled := False;
    MultiAddSub.Enabled := False;
    Resultlimit.Checked := False;
    ResultLimit.Enabled := False;
  end;
end;

{ Enable multi-operators and result limited operations for operand maximum = 100 }

procedure TForm1.Max100Change(Sender: TObject);

begin
  if Max100.Checked then begin
    MultiAdd.Enabled := True;
    MultiSub.Enabled := True;
    MultiAddSub.Enabled := True;
    ResultLimit.Enabled := True;
  end;
end;

{ Enable multi-operators and result limited operations for operand maximum = 1000 }

procedure TForm1.Max1000Change(Sender: TObject);

begin
  if Max1000.Checked then begin
    MultiAdd.Enabled := True;
    MultiSub.Enabled := True;
    MultiAddSub.Enabled := True;
    ResultLimit.Enabled := True;
  end;
end;

end.

