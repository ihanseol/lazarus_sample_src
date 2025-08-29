{****************************************}
{* Maths Trainer (Equations3) main unit *}
{****************************************}

unit equations3_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TfEquations3 }
  TfEquations3 = class(TForm)
    edSolutionZ: TEdit;
    Label1, Label2, Label3: TLabel;
    edNEquations: TEdit;
    EquNumber: TEdit;
    edEquation1: TEdit;
    edEquation2: TEdit;
    edEquation3: TEdit;
    Label4, Label5, Label6: TLabel;
    rgSolution: TRadioGroup;
    edSolutionX: TEdit;
    edSolutionY: TEdit;
    edSolCheck: TEdit;
    btStart: TButton;
    btSolve: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btSolveClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure edSolutionXChange(Sender: TObject);
    procedure edSolutionYChange(Sender: TObject);
    procedure edSolutionZChange(Sender: TObject);
  private
    iEquations, iEquation, iCorrect, iNotUnique: Integer;
    iResultX, iResultY, iResultZ, iUResultX, iUResultY, iUResultZ: Integer;
    sEquation1, sEquation2, sEquation3: string;
  end;

var
  fEquations3: TfEquations3;

implementation

{$R *.lfm}

{ Calculate 2x2 determinant }

function Determinant2(A, B, C, D: Integer): Real;

begin
  Determinant2 := A * D - B * C;
end;

{ Calculate 3x3 determinant }

function Determinant3(A, B, C, D, E, F, G, H, I: Integer): Real;

begin
  Determinant3 := A * Determinant2(E, F, H, I) - B * Determinant2(D, F, G, I) + C * Determinant2(D, E, G, H);
end;

{ Pretty format equation }

function FormatEquation(X, Y, Z, C: Integer): string;

var
  Equ : string;

begin
  Equ := '';
  // Format x coefficient and 'x'
  if X <> 0 then begin                                                                        // don't display if y coefficient = 0
    if X = 1 then                                                                             // display 'x' instead of '1x'
      Equ := 'x'
    else if X = -1 then                                                                       // display '-x' instead of '-1x'
      Equ := '-x'
    else
      Equ := IntToStr(X) + 'x';
    if Y > 0 then                                                                             // pretty display for +/- operator
      Equ += ' + '
    else if Y < 0 then
      Equ += ' - ';
  end;
  // Format y coefficient and 'y'
  if Y <> 0 then begin                                                                        // don't display if y coefficient = 0
    if X <> 0 then begin                                                                      // pretty display for y coefficient (x <> 0)
      if Abs(Y) = 1 then
        Equ += 'y'
      else
        Equ += IntToStr(Abs(Y)) + 'y';
    end
    else begin                                                                                // pretty display for y coefficient (x = 0)
      if Y = 1 then
        Equ += 'y'
      else if Y = -1 then
        Equ += '-y'
      else
        Equ += IntToStr(Y) + 'y';
    end;
  end;
  if Z > 0 then                                                                               // pretty display for +/-
    Equ += ' + '
  else if Z < 0 then
    Equ += ' - ';
  // Format z coefficient and 'z'
  if Z <> 0 then begin                                                                        // don't display if z coefficient = 0
    if Abs(Z) = 1 then                                                                        // pretty display for z coefficient
      Equ += 'z'
    else
      Equ += IntToStr(Abs(Z)) + 'z';
  end;
  // Display constant
  Equ += ' = ' + IntToStr(C);
  FormatEquation := Equ;
end;

{ Generate equation system }

procedure GenerateEquations (var Equ1, Equ2, Equ3 : string; var X, Y, Z, NotUnique: Integer);

var
  A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3: Integer;
  Det, DetX, DetY, DetZ: Real;

begin
  // System of the 3 equations: a1x + b1y + c1z = d1,  a2x + b2y + c2z = d2 and a3x + b3y + c3z = d3
  // Generate random values until they are 'ok'. In particular until result is an integer or undefined.
  // Eliminate systems with less than 3 variables, where in any equation 2 coefficients are 'missing'.
  // Limit number of "no solution/infinity of solutions" exercises.
  repeat
    // Generate random coefficient values
    A1 := Random(21) - 10; B1 := Random(21) - 10;  C1 := Random(21) - 10; D1 := Random(21) - 10;
    A2 := Random(21) - 10; B2 := Random(21) - 10;  C2 := Random(21) - 10; D2 := Random(21) - 10;
    A3 := Random(21) - 10; B3 := Random(21) - 10;  C3 := Random(21) - 10; D3 := Random(21) - 10;
    // Calculate determinants
    Det  := Determinant3(A1, B1, C1, A2, B2, C2, A3, B3, C3);
    DetX := Determinant3(D1, B1, C1, D2, B2, C2, D3, B3, C3);
    DetY := Determinant3(A1, D1, C1, A2, D2, C2, A3, D3, C3);
    DetZ := Determinant3(A1, B1, D1, A2, B2, D2, A3, B3, D3);
    // Keep only if they are 'ok' (as described above)
  until (((A1 <> 0) and (B1 <> 0)) or ((A1 <> 0) and (C1 <> 0)) or ((B1 <> 0) and (C1 <> 0)))
    and (((A2 <> 0) and (B2 <> 0)) or ((A2 <> 0) and (C2 <> 0)) or ((B2 <> 0) and (C2 <> 0)))
    and (((A3 <> 0) and (B3 <> 0)) or ((A3 <> 0) and (C3 <> 0)) or ((B3 <> 0) and (C3 <> 0)))
    and ((A1 <> 0) or (A2 <> 0) or (A3 <> 0))
    and ((B1 <> 0) or (B2 <> 0) or (B3 <> 0))
    and ((C1 <> 0) or (C2 <> 0) or (C3 <> 0))
    and ((Det = 0) or ((DetX / Det = Round(DetX / Det)) and (DetY / Det = Round(DetY / Det)) and (DetZ / Det = Round(DetZ / Det))))
    and ((Det <> 0) or ((Det = 0) and (NotUnique > 0)));
  // Build equation strings
  Equ1 := FormatEquation(A1, B1, C1, D1);                                                     // pretty format the equations
  Equ2 := FormatEquation(A2, B2, C2, D2);
  Equ3 := FormatEquation(A3, B3, C3, D3);
  // Solution of the equation system using Cramer's rule (determinants)
  if Det = 0 then begin
    if (DetX <> 0) or (DetY <> 0) or (DetZ <> 0) then begin
      X := -MaxInt; Y := -MaxInt; Z := -MaxInt;                                               // -MaxInt used as 'no solution'
    end
    else begin
      X := MaxInt; Y := MaxInt; Z := MaxInt;                                                  // MaxInt used as 'infinity of solutions'
    end;
    Dec(NotUnique);
  end
  else begin
    X := Round(DetX / Det);                                                                   // unique solution for x, y and z
    Y := Round(DetY / Det);
    Z := Round(DetZ / Det);
  end;
end;

{****************}
{* TfEquations3 *}
{****************}

{ Application start: Initialisation }

procedure TfEquations3.FormCreate(Sender: TObject);

begin
  iEquations := 10; iCorrect := 0;
  Randomize;
end;

{ Button 'Start/Restart' resp. 'Next/Evaluate': Generate equations resp. evaluate the answers given }

procedure TfEquations3.btStartClick(Sender: TObject);

var
  PCorrect: Real;
  S: string;

begin
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Restart') then begin
    // Reset variables
    iEquations := StrToInt(edNEquations.Text);                                                // number of equations as user entry
    iNotUnique := iEquations div 10 + 1;
    if iEquations > 0 then begin
      iEquation := 0; iCorrect := 0;
      edNEquations.ReadOnly := True;
      edNEquations.TabStop := False;
      edNEquations.Color := clForm;
      btStart.Caption := 'Next';
      btSolve.Enabled := False;
      edSolutionX.TabStop := False;
      edSolutionY.TabStop := False;
      edSolutionZ.TabStop := False;
    end;
  end;
  edSolutionX.Text := ''; edSolutionY.Text := ''; edSolutionZ.Text := '';
  edSolCheck.Text := '';
  rgSolution.ItemIndex := -1;                                                                 // uncheck all radio buttons
  // Generate equations
  Inc(iEquation);                                                                             // increment actual equation number
  if iEquation <= iEquations then begin                                                       // still equation(s) remaining
    EquNumber.Text := 'Equation ' + IntToStr(iEquation) + ' / ' + IntToStr(iEquations);
    GenerateEquations(sEquation1, sEquation2, sEquation3, iResultX, iResultY, iResultZ, iNotUnique);  // generate a random equation system
    edEquation1.Text := sEquation1;
    edEquation2.Text := sEquation2;
    edEquation3.Text := sEquation3;
    edSolutionX.TabStop := True; edSolutionY.TabStop := True; edSolutionZ.TabStop := True;
    btSolve.Enabled := True;
    edSolutionX.SetFocus;
  end
  // Evaluate the answers
  else begin
    PCorrect := 100 * iCorrect / iEquations;
    PCorrect := Round(100 * PCorrect) / 100;                                                  // round to 2 decimal digits
    if PCorrect >= 50 then
      edSolCheck.Font.Color := clLime
    else
      edSolCheck.Font.Color := clRed;
    S := 'Correct answers = ' + IntToStr(iCorrect);
    S := S + '; Success = ' + FloatToStr(PCorrect) + '%';
    edSolCheck.Text := S;
    edNEquations.ReadOnly := False;
    edNEquations.TabStop := True;
    edNEquations.Color := clDefault;
    EquNumber.Text := 'System 0 / 0';
    edEquation1.Text := ''; edEquation2.Text := ''; edEquation3.Text := '';
    btStart.Caption := 'Restart';
    edNEquations.SetFocus;
  end;
end;

{ Button 'Solve': Check the equations solution proposed by the user }

procedure TfEquations3.btSolveClick(Sender: TObject);

var
  C: Integer;
  S: string;

begin
  // Get user answer from form
  if rgSolution.ItemIndex = 0 then begin                                                      // one real value solution
    S := edSolutionX.Text;
    Val(S, iUResultX, C);
    if C <> 0 then                                                                            // non-numeric entry
      iUResultX := MaxInt div 2;                                                              // use MaxInt/2 as 'always false'
    S := edSolutionY.Text;
    Val(S, iUResultY, C);
    if C <> 0 then
      iUResultY := MaxInt div 2;
    S := edSolutionZ.Text;
    Val(S, iUResultZ, C);
    if C <> 0 then
      iUResultZ := MaxInt div 2;
  end
  else if rgSolution.ItemIndex = 1 then begin                                                 // no solution
    iUResultX := -MaxInt; iUResultY := -MaxInt; iUResultZ := -MaxInt;
  end
  else if rgSolution.ItemIndex = 2 then begin                                                 // infinity of solutions
    iUResultX := MaxInt; iUResultY := MaxInt;  iUResultZ := MaxInt;
  end
  else begin                                                                                  // nothing entered at all
    iUResultX := MaxInt div 2; iUResultY := MaxInt div 2; iUResultZ := MaxInt div 2;
  end;
  // Correct answer
  if (iUResultX = iResultX) and (iUResultY = iResultY) and (iUResultZ = iResultZ) then begin
    edSolCheck.Font.Color := clLime;
    edSolCheck.Text := 'Your solution is correct';                                            // display message (in lime)
    Inc(iCorrect);
  end
  // False answer
  else begin
    edSolCheck.Font.Color := clRed;
    S := 'Correct solution is:';
    if iResultX = -MaxInt then
      S += ' No solution'
    else if iResultX = MaxInt then
      S += ' Infinity of solutions'
    else begin
      S += ' x = ' + IntToStr(iResultX);
      S += ';  y = ' + IntToStr(iResultY);
      S += ';  z = ' + IntToStr(iResultZ);
    end;
    edSolCheck.Text := S;                                                                     // display correct solution (in red)
  end;
  btSolve.Enabled := False;
  edSolutionX.TabStop := False;
  edSolutionY.TabStop := False;
  edSolutionZ.TabStop := False;
  if iEquation = iEquations then
    btStart.Caption := 'Evaluation';
  btStart.SetFocus;
end;

{ Button 'Close' : Terminate the program}

procedure TfEquations3.btCloseClick(Sender: TObject);

begin
  close;
end;

{ Auto-check 'one solution' radiobutton, if user enters a result proposal }

procedure TfEquations3.edSolutionXChange(Sender: TObject);

begin
  rgSolution.ItemIndex := 0;
end;

procedure TfEquations3.edSolutionYChange(Sender: TObject);

begin
  rgSolution.ItemIndex := 0;
end;

procedure TfEquations3.edSolutionZChange(Sender: TObject);

begin
  rgSolution.ItemIndex := 0;
end;

end.

