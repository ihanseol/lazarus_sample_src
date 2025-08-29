{****************************************}
{* Main unit for Equations4 application *}
{****************************************}

unit equations4_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, Math;

type
  TEquationResult = record
    Result, Mess: string;
  end;
  {**************}
  { TfEquations4 }
  {**************}
  TfEquations4 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsLevel, mOptionsLevel1, mOptionsLevel2, mOptionsLevel3: TMenuItem;
    mOptionsResult, mOptionsResultFraction, mOptionsResultDecimal: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    Label1, Label2, Label3: TLabel;
    edQuestions: TEdit;
    edQuestion: TEdit;
    edEquation: TEdit;
    rbSolOne, rbSolTwo, edSolNone: TRadioButton;
    edAnswer1, edAnswer21, edAnswer22: TEdit;
    edEval: TEdit;
    laResult: TLabel;
    btStart: TButton;
    btSolve: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsLevel1Click(Sender: TObject);
    procedure mOptionsLevel2Click(Sender: TObject);
    procedure mOptionsLevel3Click(Sender: TObject);
    procedure mOptionsResultFractionClick(Sender: TObject);
    procedure mOptionsResultDecimalClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btSolveClick(Sender: TObject);
    procedure edAnswer1Change(Sender: TObject);
    procedure edAnswer21Change(Sender: TObject);
    procedure edAnswer22Change(Sender: TObject);
  private
    iQuestions, iLevel, iQuestion, iCorrect: Integer;
    sEquation, sResult, sResult2, sAnswer: string;
  end;

const
  SUB_Digits: array[0..9] of string = (
     #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84, #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  fEquations4: TfEquations4;

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

{ "Pretty" format equation (ax + b)}

function FormatEquation(A, B : Integer): string;

var
  Eq : string;

begin
  Eq := '';
  if A = 0 then begin                                                          // print '0' instead of '0x'
    if B = 0 then
      Eq := '0'
    else
      Eq := IntToStr(B);
  end
  else begin
    if A = 1 then                                                              // print 'x' instead of '1x'
      Eq := 'x'
    else if A = -1 then                                                        // print '-x' instead of '-1x'
      Eq := '-x'
    else if A <> 0 then
      Eq := IntToStr(A) + 'x';
    if B > 0 then                                                              // "pretty" display for +/-/0 constant
      Eq := Eq + ' + ' + IntToStr(B)
    else if B < 0 then
      Eq := Eq + ' - ' + IntToStr(Abs(B))
  end;
  FormatEquation := Eq;
end;

{ Format number (as fraction or decimal)}

function FormatNumber(N, D: Integer; AlwaysDec: Boolean): string;

var
  NumberN, NumberD, NumberGCD: Integer;
  Number: string;

// The arguments N and D are the nominator resp. denominator of a fraction

begin
  if N / D = Int(N / D) then                                                   // display integers just as integers
    Number := FloatToStrF(N / D, ffFixed, 0, 0)
  else begin
    // Display number as decimal
    if AlwaysDec or fEquations4.mOptionsResultDecimal.Checked then begin
      if 10 * (N / D) = Int(10 * (N / D)) then                                 // 1 significant decimal digit
        Number := FloatToStrF(N / D, ffFixed, 0, 1)
      else if 100 * (N / D) = Int(100 * (N / D)) then                          // 2 significant decimal digits
        Number := FloatToStrF(N / D, ffFixed, 0, 2)
      else                                                                     // all other cases: rounded to 3 decimals
        Number := FloatToStrF(N / D, ffFixed, 0, 3);
    end
    // Display number as reduced fraction
    else begin
      // Reduce the fraction
      NumberGCD := GCD(N, D);
      NumberN := N div NumberGCD;
      NumberD := D div NumberGCD;
      // Do not use negative denominators
      if NumberD < 0 then begin
        NumberN := -NumberN; NumberD := -NumberD;
      end;
      // Construct the fraction string
      Number := IntToStr(NumberN) + '/' + IntToStr(NumberD);
    end;
  end;
  FormatNumber := Number;
end;

{ Remove non-significant zeros from user answer }

function RemoveZeros(EqAnswer: string): string;

var
  P: Integer;
  EqAnsw: string;

begin
  EqAnsw := EqAnswer;
  // Do only if results are decimal numbers
  if fEquations4.mOptionsResultDecimal.Checked then begin
    P := Pos(',', EqAnsw);
    if P = 0 then
      P := Pos('.', EqAnsw);
    if P <> 0 then begin
      if RightStr(EqAnsw, 3) = '000' then
        EqAnsw := LeftStr(EqAnsw, Length(EqAnsw) - 3)
      else if RightStr(EqAnsw, 2) = '00' then
        EqAnsw := LeftStr(EqAnsw, Length(EqAnsw) - 2)
      else if RightStr(EqAnsw, 1) = '0' then
        EqAnsw := LeftStr(EqAnsw, Length(EqAnsw) - 1);
      if (RightStr(EqAnsw, 1) = ',') or (RightStr(EqAnsw, 1) = '.') then       // remove decimal separator
        EqAnsw := LeftStr(EqAnsw, Length(EqAnsw) - 1);
    end;
  end;
  RemoveZeros := EqAnsw;
end;

{ Solve 1st degree equation }

function SolveEquation1(A, B: Integer): TEquationResult;

var
  Res, Mess: string;

// The function returns a TEquationResult record:
//   .Result = equation result
//   .Mess   = filled if no solution or infinity of solutions (used to eliminate such equations)

begin
  Res := ''; Mess := '';
  if A = 0 then begin
    if B = 0 then begin
      Res := 'infinity of solutions'; Mess := Res;
    end
    else begin
      Res := 'no solution'; Mess := Res;
    end;
  end
  else
    Res := FormatNumber(B, A, False);                                          // root as rounded decimal or reduced fraction
  SolveEquation1.Result := Res;
  SolveEquation1.Mess := Mess;
end;

{ Solve 2nd degree equation }

function SolveEquation2(A, B, C: Integer): TEquationResult;

var
  Delta: Real;
  Res, Mess: string;

// The function returns a TEquationResult record:
//   .Result = equation result (if it actually is a 2nd degree equation and if there are real roots)
//   .Mess   = filled if it is a 1st degree equation, if there are no real roots or if Delta is not an integer (used to eliminate such equations)

begin
  Res := ''; Mess := '';
  if A = 0 then
    Mess := 'no 2nd degree equation'
  else begin
    Delta := Sqr(B) - 4 * A * C;
    if Delta < 0 then
      Mess := 'no real solution'
    else begin
      if Sqrt(Delta) <> Int(Sqrt(Delta)) then begin
        Res := FormatNumber(-B + Round(Sqrt(Delta)), 2 * A, True);             // 1 root as rounded decimal
        if Delta > 0 then
          Res += '; ' + FormatNumber(-B - Round(Sqrt(Delta)), 2 * A, True);    // 2 roots as rounded decimals separated by "; "
        Mess := 'no integer square root';
      end
      else begin
        Res := FormatNumber(-B + Round(Sqrt(Delta)), 2 * A, False);            // 1 root as rounded decimal or fraction
        if Delta > 0 then
          Res += '; ' + FormatNumber(-B - Round(Sqrt(Delta)), 2 * A, False);   // 2 roots as rounded decimals or fractions separated by "; "
      end;
    end;
  end;
  SolveEquation2.Result := Res;
  SolveEquation2.Mess := Mess;
end;

{ Randomly generate equation "log(ax + b)" term }

procedure GenerateLogTerm(out LogTerm: string; var Base: Integer; out A, B: Integer; RandomBase: Boolean);

var
  OK: Boolean;

begin
  if RandomBase then
    Base := Random(8) + 2;                                                     // logarithm base: between 2 and 10
  repeat
    OK := True;
    A := Random(21) - 10; B := Random(21) - 10;                                // a and b: integers between -10 and 10
    if (A = 0) and (B <= 0) then
      OK := False;                                                             // logarithm argument can't be negative
  until OK;
  LogTerm := 'log';
  if Base <> 10 then
    LogTerm += SUB_Digits[Base];                                               // add logarithm base as subscript
  LogTerm += '(' + FormatEquation(A, B) + ')';
end;

{ Randomly generate equation constant term }

procedure GenerateConstTerm(Base: Integer; out C: Integer);

begin
  repeat
    C := Random(10) + 1;
  until intpower(Base, C) <= 100;                                              // logarithm-base ^ constant should be <= 100
end;

{ Generate logarithm equation }
{ --------------------------- }

procedure GenerateEquation (Level, Questions: Integer; out Equation, Result: string);

var
  EqType, Base, A1, B1, A2, B2, A3, B3, C, ResOK, R, R1, R2, I, P: Integer;
  Res: Real;
  LogTerm: string;
  OK, AllowNoSolution: Boolean;
  Results: array[1..2] of string;
  EqResult: TEquationResult;

begin
  A1 := 0; A2 := 0; A3 := 0; B1 := 0; B2 := 0; B3 := 0; Base := 1;
  // Equation type (random) variables
  R := Random(11); R1 := Random(3); R2 := Random(2);
  // Only a given number (1/5 of questions) should have no solution
  AllowNoSolution := False;
  if Random(Questions) <= Round(Questions / 5) then
    AllowNoSolution := True;
  // Main equation type (depending on level)
  case Level of
    1: EqType := 1;
    2: begin
         if R <= 2 then
           EqType := 1
         else
           EqType := 2;
       end;
    3: begin
         if R = 0 then
           EqType := 1
         else if R <= 2 then
           EqType := 2
         else if R <= 6 then
           EqType := 3
         else
           EqType := 4;
       end;
  end;
  // Repeat generating an equation until a valid (wanted) one has been found
  repeat
    OK := True;
    repeat
      OK := True;
      // Equation type: log(a1x + b1) = log(a2x + b2)
      if EqType = 1 then begin
        GenerateLogTerm(LogTerm, Base, A1, B1, True);
        Equation := LogTerm;
        GenerateLogTerm(LogTerm, Base, A2, B2, False);
        Equation += ' = ' + LogTerm;
        EqResult := SolveEquation1(A1 - A2, B2 - B1);
      end
      // Equation type: log(a1x + b1) = c
      else if EqType = 2 then begin
        GenerateLogTerm(LogTerm, Base, A1, B1, True);
        GenerateConstTerm(Base, C);
        Equation := LogTerm + ' = ' + IntToStr(C);
        EqResult := SolveEquation1(A1, Round(intpower(Base, C)) - B1);
      end
      // Equations with logarithm addition and subtraction
      else if EqType = 3 then begin
        GenerateLogTerm(LogTerm, Base, A1, B1, True);
        Equation := LogTerm;
        if R1 = 0 then begin
          Equation += ' = ';
          // Equation type: log(a1x + b1) = log(a2x + b2) - log(a3x + b3)
          if R2 = 0 then begin
            GenerateLogTerm(LogTerm, Base, A2, B2, False);
            Equation += LogTerm + ' - ';
            GenerateLogTerm(LogTerm, Base, A3, B3, False);
            Equation += LogTerm;
            EqResult := SolveEquation2(A1 * A3, A1 * B3 + B1 * A3 - A2, B1 * B3 - B2);
          end
          // Equation type: log(a1x + b1) = log(a2x + b2) + log(a3x + b3)
          else begin
            GenerateLogTerm(LogTerm, Base, A2, B2, False);
            Equation += LogTerm + ' + ';
            GenerateLogTerm(LogTerm, Base, A3, B3, False);
            Equation += LogTerm;
            EqResult := SolveEquation2(A2 * A3, A2 * B3 + B2 * A3 - A1, B2 * B3 - B1);
          end;
        end
        // Equation type: log(a1x + b1) + log(a2x + b2) = log(a3x + b3)
        else if R1 = 1 then begin
          Equation += ' + ';
          GenerateLogTerm(LogTerm, Base, A2, B2, False);
          Equation += LogTerm + ' = ';
          GenerateLogTerm(LogTerm, Base, A3, B3, False);
          Equation += LogTerm;
          EqResult := SolveEquation2(A1 * A2, A1 * B2 + B1 * A2 - A3, B1 * B2 - B3);
        end
        // Equation type: log(a1x + b1) - log(a2x + b2) = log(a3x + b3)
        else begin
          Equation += ' - ';
          GenerateLogTerm(LogTerm, Base, A2, B2, False);
          Equation += LogTerm + ' = ';
          GenerateLogTerm(LogTerm, Base, A3, B3, False);
          Equation += LogTerm;
          EqResult := SolveEquation2(A2 * A3, A2 * B3 + B2 * A3 - A1, B2 * B3 - B1);
        end;
      end
      // Equation type: log(a1x + b1) + log(a2x + b2) = c and log(a1x + b1) - log(a2x + b2) = c
      else begin
        GenerateLogTerm(LogTerm, Base, A1, B1, True);
        Equation := LogTerm;

        if R2 = 0 then
          Equation += ' + '
        else
          Equation += ' - ';
        GenerateLogTerm(LogTerm, Base, A2, B2, False);
        Equation += LogTerm;
        GenerateConstTerm(Base, C);
        Equation += ' = ' + IntToStr(C);
        if R2 = 0 then
          EqResult := SolveEquation2(A1 * A2, A1 * B2 + B1 * A2, B1 * B2 - Round(intpower(Base, C)))
        else
          EqResult := SolveEquation1(A1 - A2 * Round(intpower(Base, C)), B2 * Round(intpower(Base, C)) - B1);
      end;
      // If all ok, get equation result
      if EqResult.Mess = '' then
        Result := EqResult.Result
      else
        // If "some problem", generate a new equation
        OK := False;
    until OK;
    // Valid equation found: Determine valid solutions
    P := Pos('; ', Result);
    if P = 0 then begin
      Results[1] := Result; Results[2] := '';                                  // actually 1 solution found
    end
    else begin
      Results[1] := LeftStr(Result, P - 1); Results[2] := Copy(Result, P + 2, Length(Result));  // actually 2 solutions found
    end;
    ResOK := 0;
    // Check how many of the solutions found are valid (i.e. do not give the log of a number <= 0)
    for I := 1 to 2 do begin
      // Transform result string to number
      if Results[I] <> '' then begin
        P := Pos('/', Results[I]);
        if P = 0 then
          Res := StrToFloat(Results[I])
        else
          Res := StrToFloat(LeftStr(Results[I], P - 1)) / StrToFloat(Copy(Results[I], P + 1, Length(Results[I])));
        // Check for log of number <= 0
        if not ((A1 * Res + B1 <= 0) or (((A2 <> 0) or (B2 <> 0)) and (A2 * Res + B2 <= 0)) or (((A3 <> 0) or (B3 <> 0)) and (A3 * Res + B3 <= 0))) then
          // ResOK will contain all info necessary to only retain the valid solution(s)
          ResOK += I;
      end;
    end;
    if ResOK = 0 then
      // ResOK was not incremented at all; thus: always negative value -> equation has no solution
      Result := 'no solution'
    else if ResOK in [1, 2] then
      // ResOK was incremented once; thus: solution 1 or solution 2 (but not both) don't give negative value -> equation has one solution
      Result := Results[ResOK]
    else
      // ResOK = 3 (was incremented twice): never negative value -> equation has two solutions
      Result := Results[1] + '; ' + Results[2];
    // As only a limited "no solution" equations are wanted, check if keep it or generate a new equation
    if Result = 'no solution' then begin
      if not AllowNoSolution then
        OK := False;
    end;
  until OK;
end;

{**************}
{ TfEquations4 }
{**************}

{ Application start: Initialisation }

procedure TfEquations4.FormCreate(Sender: TObject);

begin
  Randomize;
end;

{ Menu item "File > Exit": Exit application }

procedure TfEquations4.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Level > ...": Choose difficulty level (type of equations) }

procedure TfEquations4.mOptionsLevel1Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := True;
  mOptionsLevel2.Checked := False;
  mOptionsLevel3.Checked := False;
end;

procedure TfEquations4.mOptionsLevel2Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False;
  mOptionsLevel2.Checked := True;
  mOptionsLevel3.Checked := False;
end;

procedure TfEquations4.mOptionsLevel3Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False;
  mOptionsLevel2.Checked := False;
  mOptionsLevel3.Checked := True;
end;

{ Menu item "Options > Result > ...": Choose how to display results (fractions or decimals) }

procedure TfEquations4.mOptionsResultFractionClick(Sender: TObject);

begin
  mOptionsResultFraction.Checked := True;
  mOptionsResultDecimal.Checked := False;
  laResult.Caption := 'Results as reduced fractions!';
end;

procedure TfEquations4.mOptionsResultDecimalClick(Sender: TObject);

begin
  mOptionsResultFraction.Checked := False;
  mOptionsResultDecimal.Checked := True;
  laResult.Caption := 'Results as numbers rounded to 3 decimals!';
end;

{ Menu item "Help > About": Display application about }

procedure TfEquations4.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Maths trainer: Logarithm equations.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March 2019.';
  MessageDlg('About "Equations4"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Restart/Next/Evaluation": Generate equation resp. evaluate answers }

procedure TfEquations4.btStartClick(Sender: TObject);

var
  PCorrect: Integer;
  S: string;

begin
  // Button "Start" or "Restart": Start a new test
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Restart') then begin
    // Get number of test questions
    if edQuestions.Text = '' then
      iQuestions := 0
    else
      iQuestions := StrToInt(edQuestions.Text);
    if iQuestions > 0 then begin
      // Get difficulty level
      if mOptionsLevel1.Checked then
        iLevel := 1
      else if mOptionsLevel2.Checked then
        iLevel := 2
      else
        iLevel := 3;
      // Reset evaluation counters
      iQuestion := 0; iCorrect := 0;
      // Next push on button will be for next question
      btStart.Caption := 'Next';
      btSolve.Enabled := False;
    end;
  end;
  // Clear answer and evaluation fileds
  edAnswer1.Text := ''; edAnswer21.Text := ''; edAnswer22.Text := '';
  edEval.Text := ''; edEval.Font.Color := clDefault;
  rbSolOne.Checked := False; rbSolTwo.Checked := False; edSolNone.Checked := False;
  Inc(iQuestion);
  // If there are still questions left: Generate logarithm equation
  if iQuestion <= iQuestions then begin
    edQuestion.Text := 'Equation ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    GenerateEquation(iLevel, iQuestions, sEquation, sResult);
    edEquation.Text := sEquation;
    btSolve.Enabled := True;                                                   // enable "Solve" button
    edAnswer1.SetFocus;
  end
  // If all questions have been done: Display evaluation
  else begin
    PCorrect :=  100 * iCorrect div iQuestions;
    if PCorrect >= 60 then
      edEval.Font.Color := clLime
    else if PCorrect >= 50 then
      edEval.Font.Color := clYellow
    else
      edEval.Font.Color := clRed;
    S := 'Correct answers = ' + IntToStr(iCorrect);
    S := S + '; Success = ' + IntToStr(PCorrect) + '%';
    edEval.Text := S;
    // Prepare to start a new test
    edQuestion.Text := 'Equation 0 / 0';
    edEquation.Text := '';
    btStart.Caption := 'Restart';
  end;
end;

{ Button "Solve": Check user answer }

procedure TfEquations4.btSolveClick(Sender: TObject);

var
  P: Integer;

begin
  // Get user answer
  if rbSolOne.Checked then
    // "One solution" checked: Get answer from corresp. edit field
    sAnswer := RemoveZeros(edAnswer1.Text)
  else if rbSolTwo.Checked then
    // "Two solutions" checked: Construct answer string from corresp. edit fields
    sAnswer := RemoveZeros(edAnswer21.Text) + '; ' + RemoveZeros(edAnswer22.Text)
  else if edSolNone.Checked then
    // "No solution" checked: Set answer to "no solution"
    sAnswer := 'no solution'
  else
    // Nothing checked: This is always false, of course
    sAnswer := '?';
  // Create a second result string with the 2 equation results inverted
  // This gives the user the possibility to enter them in the order she wants
  sResult2 := sResult;
  P := Pos('; ', sResult);
  if P <> 0 then
    sResult2 := Copy(sResult, P + 2, Length(sResult)) + '; ' + LeftStr(sResult, P - 1);
  // Check the user's answer against the 2 result strings
  if (sAnswer = sResult) or (sAnswer = sResult2) then begin
    // Correct answer
    edEval.Font.Color := clLime;
    edEval.Text := 'Your answer is correct!';
    Inc(iCorrect);
  end
  else begin
    // False answer
    edEval.Font.Color := clRed;
    if sResult <> 'no solution' then begin
      sResult := 'x = ' + sResult;
      sResult := StringReplace(sResult, '; ', ' or x = ', []);
    end;
    edEval.Text := 'False! Correct answer: ' + sResult;;
  end;
  // Disable "Solve" button
  btSolve.Enabled := False;
  // If all questions have been done set button capture to "Evaluation" (instead of "Next")
  if iQuestion = iQuestions then
    btStart.Caption := 'Evaluation';
end;

{ Auto-check "One solution", if user enters a one-solution result }

procedure TfEquations4.edAnswer1Change(Sender: TObject);

begin
  rbSolOne.Checked := True;
end;

{ Auto-check "Two solutions", if user enters a two-solutions result }

procedure TfEquations4.edAnswer21Change(Sender: TObject);

begin
  rbSolTwo.Checked := True;
end;

procedure TfEquations4.edAnswer22Change(Sender: TObject);

begin
  rbSolTwo.Checked := True;
end;

end.

