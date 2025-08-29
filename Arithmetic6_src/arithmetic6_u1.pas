{*****************************************}
{* Main unit for Arithmetic6 application *}
{*****************************************}

unit arithmetic6_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UComplex;

type
  TOperators = array[1 .. 4] of Char;
  {********}
  { TForm1 }
  {********}
  TForm1 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    Label1: TLabel;
    cbAddition, cbSubtraction, cbMultiplication, cbDivision: TCheckBox;
    edOperation, edQuestion, edAnswer, edEvaluation: TEdit;
    btStart: TButton;
    btSolve: TButton;
    btReset: TButton;
    btQuit: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btSolveClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
    procedure ButtonQuitClick(Sender: TObject);
  private
    iOperators, iQuestion, iCorrect: Integer;
    sOperation: string;
    cplxResult: complex;
    aOperators: TOperators;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ Convert complex number to string }

function ComplexToStr(CplxNumber: Complex): string;

// As a difference with "CStr" of the UComplex unit, this function returns a properly formatted string representation of a complex number
// Important: Real/imaginary values with an absolute value < 1E-7 are considered to be 0!

var
  L: Integer;
  SComplex, SReal, SImag: string;

begin
  // Complex z = 0: return '0'
  if (Abs(CplxNumber.Re) < 1E-7) and (Abs(CplxNumber.Im) < 1E-7) then
    SComplex := '0'
  // Complex z <> 0: return formatted string
  else begin
    // Round real and decimal part to 7 digits and remove all non-significant zeros
    Str(CplxNumber.Re:0:7, SReal); Str(Abs(CplxNumber.Im):0:7, SImag);
    L := Length(SReal);
    while SReal[L] = '0' do
      Dec(L);                                                                  // non-significant zeros
    if SReal[L] = '.' then
      Dec(L);                                                                  // decimal separator (without fractional digits)
    SReal := LeftStr(SReal, L);
    L := Length(SImag);
    while SImag[L] = '0' do
      Dec(L);
    if SImag[L] = '.' then
      Dec(L);
    SImag := LeftStr(SImag, L);
    SComplex := '';
    // Keep real part only if it is <> 0
    if Abs(CplxNumber.Re) >= 1E-7 then
      SComplex := SReal;
    // Keep imaginary part only if <> 0
    if Abs(CplxNumber.Im) >= 1E-7 then begin
      // Complex with real and imaginary part <> 0
      if Abs(CplxNumber.Re) >= 1E-7 then begin
        if CplxNumber.Im < 0 then
          SComplex += '-'
        else
          SComplex += '+';
      end
      // Complex with imaginary part only <> 0
      else begin
        if CplxNumber.Im < 0 then
          SComplex += '-'                                                      // minus sign has to be displayed (plus sign has not)
      end;
      if Abs(CplxNumber.Im) <> 1 then                                          // use "i" instead of "1i"
        SComplex += SImag;
      SComplex += 'i';
    end;
  end;
  Result := SComplex;
end;

{ Transform string to complex number }

function StrToComplex(SNumber: string): Complex;

// The string is supposed to be properly formatted (as the one returned by the ComplexToStr function)

var
  Sign, P: Integer;
  NumberRe, NumberIm: Real;

begin
  // To avoid problems, be sure that the imaginary part is given as a number + "i"
  if SNumber = 'i' then
    SNumber := '1i'
  else if SNumber = '-i' then
    SNumber := '-1i';
  // Complex number contains an imaginary part
  if RightStr(SNumber, 1) = 'i' then begin
    SNumber := LeftStr(SNumber, Length(SNumber) - 1);
    Sign := 1;
    // This "-" is the sign of the real part (if this one <> 0) or the sign of the imaginary part (if there is no real part)
    if LeftStr(SNumber, 1) = '-' then begin
      SNumber := RightStr(SNumber, Length(SNumber) - 1);
      Sign := -1;
    end;
    // This "+" resp. "-" sign starts the imaginary part; if both are absent, the real part = 0 (cf. ComplexToStr function)
    P := Pos('+', SNumber);
    if P = 0 then
      P := Pos('-', SNumber);
    if P = 0 then begin
      NumberRe := 0; Val(SNumber, NumberIm); NumberIm *= Sign;
    end
    else begin
      Val(LeftStr(SNumber, P - 1), NumberRe); NumberRe *= Sign;
      Val(RightStr(SNumber, Length(SNumber) - P + 1), NumberIm);
    end;
  end
  // Complex number contains no imaginary part
  else begin
    Val(SNumber, NumberRe); NumberIm := 0;
  end;
  Result := cinit(NumberRe, NumberIm);
end;

{ Clear form controls and variables }

procedure ClearForm(out Question, QCorrect: Integer);

begin
  Form1.edOperation.Text := '';
  Form1.edQuestion.Text := '';
  Form1.edAnswer.Text := '';
  Form1.edEvaluation.Text := ''; Form1.edEvaluation.Font.Color := clDefault;
  // Enable operators choice
  Form1.cbAddition.Enabled := True;
  Form1.cbSubtraction.Enabled := True;
  Form1.cbMultiplication.Enabled := True;
  Form1.cbDivision.Enabled := True;
  // Enable/disable buttons
  Form1.btStart.Caption := 'Start';
  Form1.btReset.Enabled := False;
  Form1.btSolve.Enabled := False;
  // Reset variables
  Question := 0; QCorrect := 0;
end;

{ Create operators list (for this exercise }

procedure OperatorsList (out N: Integer; out Operators: TOperators);

var
  I: Integer;

begin
  N := 0;
  for I := 1 to 4 do
    Operators[I] := ' ';
  // Add operators to list as selected on application form
  if Form1.cbAddition.Checked then begin
    Inc(N); Operators[N] := '+';
  end;
  if Form1.cbSubtraction.Checked then begin
    Inc(N); Operators[N] := '-';
  end;
  if Form1.cbMultiplication.Checked then begin
    Inc(N); Operators[N] := 'x';
  end;
  if Form1.cbDivision.Checked then begin
    Inc(N); Operators[N] := ':';
  end;
end;

{ Generate exercise question }

procedure GenerateOperation (N: Integer; Operators: TOperators; out Question: string; out Result: complex);

const
  MaxOperand = 20; MaxResult = 100;
  Signs: array[0..1] of Integer = (1, -1);

var
  Operand_Re, Operand_Im: Integer;
  Oprator: Char;
  OperationOk: Boolean;
  Operand1, Operand2: complex;

begin
  // Random operator (from those selected)
  Oprator := Operators[Random(N) + 1];
  // Repeat exercise generation until "valid operation found"
  repeat
    OperationOk := True;
    // Generate operands (all numbers between 1 and "max operand")
    Operand_Re := Signs[Random(2)] * (Random(MaxOperand) + 1);
    Operand_Im := Signs[Random(2)] * (Random(MaxOperand) + 1);
    Operand1 := cinit(Operand_Re, Operand_Im);
    Operand_Re := Signs[Random(2)] * (Random(MaxOperand) + 1);
    Operand_Im := Signs[Random(2)] * (Random(MaxOperand) + 1);
    Operand2 := cinit(Operand_Re, Operand_Im);
    // cbAddition
    if Oprator = '+' then
      Result := Operand1 + Operand2
    // cbSubtraction
    else if Oprator = '-' then
      Result := Operand1 - Operand2
    // cbMultiplication
    else if Oprator = 'x' then begin
      if csamevalue(Operand1, 1) or csamevalue(Operand1, -1) or csamevalue(Operand2, 1) or csamevalue(Operand2, -1) then
        // No cbMultiplication by "1" or "-1"
        OperationOk := False
      else
        Result := Operand1 * Operand2;
    end
    // cbDivision
    else if Oprator = ':' then begin
      if csamevalue(Operand2, 1) or csamevalue(Operand2, -1) then
        // No cbDivision by "1" or "-1"
        OperationOk := False
      else if csamevalue(Operand1, Operand2) then
        // No cbDivision with equal operands
        OperationOk := False
      else
        Result := Operand1 / Operand2;
    end;
    if OperationOk then
      // Check if real and imaginary part of result is less than "max result"
      if (Abs(Result.re) > MaxResult) or (Abs(Result.im) > MaxResult) then
        OperationOk := False;
  until OperationOk;
  // Create the question (operation) string
  Question := '(' + ComplexToStr(Operand1) + ') ' + Oprator + ' (' + ComplexToStr(Operand2) + ')';
end;

{********}
{ TForm1 }
{********}

{ Application start: Initialisation }

procedure TForm1.FormCreate(Sender: TObject);

begin
  ClearForm(iQuestion, iCorrect);
  Randomize;
end;

{ Button "Start/Next": Generate exercise (basic complex operation) }

procedure TForm1.btStartClick(Sender: TObject);

begin
  // Get operators list for this exercise session
  if (btStart.Caption = 'Start') then begin
    OperatorsList(iOperators, aOperators);
  end;
  // Procced only if at least 1 operator has been selected
  if iOperators > 0 then begin
    edEvaluation.Text := ''; edEvaluation.Color := clDefault; edEvaluation.Font.Color := clDefault;
    if (btStart.Caption = 'Start') then begin
      // Disable operators selection
      cbAddition.Enabled := False;        cbSubtraction.Enabled := False;
      cbMultiplication.Enabled := False;  cbDivision.Enabled := False;
      btStart.Caption := 'Next';
    end;
    // Generate exercise
    Inc(iQuestion);
    GenerateOperation(iOperators, aOperators, sOperation, cplxResult);
    // Set values of form fields as required
    edOperation.Text := 'Operation ' + IntToStr(iQuestion);
    edQuestion.Text := sOperation + ' = ?';
    edAnswer.Text := ''; edEvaluation.Text := '';
    edAnswer.SetFocus;
    btSolve.Enabled := True;
    btReset.Enabled := True;
  end
  // No operator selected: Error message
  else
    MessageDlg('Invalid selection', 'You must choose at least 1 operator!', mtError, [mbOK], 0);
end;

{ Button "Solve": Check the edQuestion edAnswer entered by the user}

procedure TForm1.btSolveClick(Sender: TObject);

var
  SUserResult: string;
  Result_Re, Result_Im, UResult_Re, UResult_Im: Real;
  UserResult: complex;

begin
  SUserResult := Trim(edAnswer.Text);
  // Transform decimal separator (if necessary)
  SUserResult := StringReplace(SUserResult, ',', '.', [rfReplaceAll]);
  // Give user some flexibility in formatting the complex entered
  SUserResult := StringReplace(SUserResult, ' ', '', [rfReplaceAll]);
  SUserResult := StringReplace(SUserResult, '0+', '', [rfReplaceAll]);
  SUserResult := StringReplace(SUserResult, '0-', '', [rfReplaceAll]);
  SUserResult := StringReplace(SUserResult, '+0i', '', [rfReplaceAll]);
  SUserResult := StringReplace(SUserResult, '-0i', '', [rfReplaceAll]);
  SUserResult := StringReplace(SUserResult, '+1i', '+i', [rfReplaceAll]);
  SUserResult := StringReplace(SUserResult, '-1i', '-i', [rfReplaceAll]);
  UserResult := StrToComplex(SUserResult);
  // Results with 3 significant decimal digits
  Result_Re := Round(1000 * cplxResult.re) / 1000;
  Result_Im := Round(1000 * cplxResult.im) / 1000;
  cplxResult := cinit(Result_Re, Result_Im);
  UResult_Re := Round(1000 * UserResult.re) / 1000;
  UResult_Im := Round(1000 * UserResult.im) / 1000;
  if (UResult_Re = Result_Re) and (UResult_Im = Result_Im) then begin
    // User answer is correct
    edEvaluation.Font.Color := clLime;
    edEvaluation.Text := 'Your solution is correct!';                          // display message (in green)
    Inc(iCorrect);
  end
  else begin
    // User answer is false
    edEvaluation.Font.Color := clRed;
    edEvaluation.Text := 'False! Correct solution is: ' + ComplexToStr(cplxResult);  // display correct edAnswer (in red)
  end;
  // Enable/disable form buttons
  btSolve.Enabled := False;
  btReset.Enabled := True;
  btStart.SetFocus;
end;

{ Button "Reset": Evaluate results; reset application }

procedure TForm1.btResetClick(Sender: TObject);

var
  Q, C: Integer;
  S: string;
  PCorrect: Integer;

begin
  Q := iQuestion; C := iCorrect; ClearForm(iQuestion, iCorrect);
  // Evaluate user answer (if there were at least 5 operations to solve)
  if Q >= 5 then begin
    S := '';
    PCorrect :=  100 * C div Q;
    if PCorrect < 50 then
      edEvaluation.Color := clRed
    else if PCorrect < 60 then
      edEvaluation.Color := clYellow
    else
      edEvaluation.Color := clLime;
    S := 'Correct answers = ' + IntToStr(C) + ' / ' + IntToStr(Q);
    S += '; Success = ' + IntToStr(PCorrect) + '%';
    edEvaluation.Text := S; edEvaluation.Font.Color := clDefault;
  end;
end;

{ Button "Quit": Exit the application }

procedure TForm1.ButtonQuitClick(Sender: TObject);

begin
  Close;
end;

end.
