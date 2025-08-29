//
// Maths Trainer (Equations1) main unit
//

unit equations1_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TForm1 }
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EquTotal: TEdit;
    EquNumber: TEdit;
    EquEquation: TEdit;
    SolOne: TRadioButton;
    SolNone: TRadioButton;
    SolInfinity: TRadioButton;
    SolSolution: TEdit;
    SolCheck: TEdit;
    ButtonStart: TButton;
    ButtonSolve: TButton;
    ButtonClose: TButton;
    procedure ButtonSolveClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure SolSolutionChange(Sender: TObject);
  end;

var
  N, I, C : Integer;
  S, Equation : string;
  Result, UResult, Correct, PCorrect : Integer;
  Form1: TForm1;

implementation

{$R *.lfm}

{ Generate equation }

procedure GenerateEquation (var E : string; var R : Integer);

var
  T, M, X1, X2, X0, C1, C2, C0 : Integer;

// Pretty format equation

function FormatEquation(X, C : Integer): string;

var
  E : string;

begin
  E := '';
  if X = 0 then begin                       // print '0' instead of '0x'
    if C = 0 then
      E := '0'
    else
      E := IntToStr(C);
  end
  else begin
    if X = 1 then                           // print 'x' instead of '1x'
      E := 'x'
    else if X = -1 then                     // print '-x' instead of '-1x'
      E := '-x'
    else if X <> 0 then
      E := IntToStr(X) + 'x';
    if C > 0 then                           // pretty display for +/-/0 constant
      E := E + ' + ' + IntToStr(C)
    else if C < 0 then
      E := E + ' - ' + IntToStr(abs(C))
  end;
  FormatEquation := E;
end;

begin
  T := Random (2);
  if T = 0 then                             // equation type 'a1x + b1'
    M := 1
  else
    M := Random(11) - 5;                    // equation type 'm(a1x + b1)'
  if M = 0 then
    M := 1;
  // Generate random values until they are 'ok'. In particular until
  // result is an integer or undefined. Also eliminate equations
  // '0x = a2x + b2' and 'a1x = b1x'
  repeat
    X1 := Random(21) - 10; X2 := Random(21) - 10;
    C1 := Random(21) - 10; C2 := Random(21) - 10;
    if (C1 = 0) or (X1 = 0) then            // eliminate m*ax and m*c
      M := 1;
    X0 := M * X1 - X2; C0 := C2 - M * C1;
  until ((X0 = 0) or (C0 mod X0 = 0))
    and ((X1 <> 0) and ((C1 <> 0) or (C2 <> 0) or (X2 = 0)));
  E := '';
  if M = -1 then                            // pretty display...
    E := '-('
  else if M <> 1 then
    E := IntToStr(M) + '(';
  E := E + FormatEquation(X1, C1);          // and even prettier...
  if M <> 1 then
    E := E + ')';
  E := E + ' = ' + FormatEquation(X2, C2);
  if X0 = 0 then begin
    if C0 = 0 then
      R := MaxInt                           // MaxInt used as 'infinity of solutions'
    else
      R := -MaxInt                          // -MaxInt used as 'no solution'
    end
    else
      R := C0 div X0;                       // one real value result
end;

{ TForm1 }

{ Button 'Start/Restart' resp. 'Next/Evaluate':
  Init program; generate equation; evaluate the answers given }

procedure TForm1.ButtonStartClick(Sender: TObject);

begin
  if (ButtonStart.Caption = 'Start') or
     (ButtonStart.Caption = 'Restart') then begin
    // Initialize the program
    Randomize;
    N := StrToInt(EquTotal.Text);           // number of equations as user entry
    if N > 0 then begin
      I := 0; Correct := 0;
      EquTotal.ReadOnly := True;
      EquTotal.TabStop := False;
      EquTotal.Color := clForm;
      ButtonStart.Caption := 'Next';
      ButtonSolve.Enabled := False;
      SolSolution.TabStop := False;
    end;
  end;
  SolCheck.Text := '';
  SolSolution.Text := '';
  SolOne.Checked := False;
  SolNone.Checked := False;
  SolInfinity.Checked := False;
  I := I + 1;                               // increment actual equation number
  if I <= N then begin                      // still equation(s) remaining
    EquNumber.Text := 'Equation ' + IntToStr(I) + ' / ' + IntToStr(N);
    GenerateEquation(Equation, Result);     // generate a random equation
    EquEquation.Text := Equation;
    ButtonSolve.Enabled := True;
    SolSolution.TabStop := True;
    SolSolution.SetFocus;
  end
  else begin                                // all done; evaluate the answers
    PCorrect :=  100 * Correct div N;
    if PCorrect >= 50 then
      SolCheck.Font.Color := clLime
    else
      SolCheck.Font.Color := clRed;
    S := 'Correct answers = ' + IntToStr(Correct);
    S := S + '; Success = ' + IntToStr(PCorrect) + '%';
    SolCheck.Text := S;
    EquTotal.ReadOnly := False;
    EquTotal.TabStop := True;
    EquTotal.Color := clDefault;
    EquTotal.SetFocus;
    EquNumber.Text := 'Equation 0 / 0';
    EquEquation.Text := '';
    ButtonStart.Caption := 'Restart';
  end;
end;

{ Button 'Solve': Check the equation solution proposed by the user}

procedure TForm1.ButtonSolveClick(Sender: TObject);
begin
  if SolOne.Checked = True then begin       // one real value solution
    S := SolSolution.Text;
    Val(S, UResult, C);
    if C <> 0 then                          // non-numeric entry
      UResult := MaxInt div 2;              // use MaxInt/2 as 'always false'
  end
  else if SolNone.Checked = True then       // no solution
    UResult := -MaxInt
  else if SolInfinity.Checked = True then   // infinity of solutions
    UResult := MaxInt
  else                                      // nothing entered at all
    UResult := MaxInt div 2;
  if UResult = Result then begin            // correct answer
    SolCheck.Font.Color := clLime;
    SolCheck.Text := 'Your solution is correct';
    Inc(Correct);
  end
  else begin                                // false answer
    SolCheck.Font.Color := clRed;
    S := 'Correct solution is:';
    if Result = -MaxInt then
      S := S + ' No solution'
    else if Result = MaxInt then
      S := S + ' Infinity of solutions'
    else
      S := S + ' x = ' + IntToStr(Result);
    SolCheck.Text := S;
  end;
  ButtonSolve.Enabled := False;
  SolSolution.TabStop := False;
  if I = N then
    ButtonStart.Caption := 'Evaluation';
end;

{ Button 'Close' : Terminate the program}

procedure TForm1.ButtonCloseClick(Sender: TObject);

begin
  close;
end;

{ Auto-check 'one' solution, if user enters a result proposal }

procedure TForm1.SolSolutionChange(Sender: TObject);

begin
  SolOne.Checked := True;
end;

end.

