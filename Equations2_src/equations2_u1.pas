{****************************************}
{* Maths Trainer (Equations2) main unit *}
{****************************************}

unit equations2_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TfEquations2 }
  TfEquations2 = class(TForm)
    Label1, Label2, Label3: TLabel;
    edNEquations: TEdit;
    EquNumber: TEdit;
    edEquation1: TEdit;
    edEquation2: TEdit;
    Label4, Label5: TLabel;
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
  private
    iEquations, iEquation, iCorrect: Integer;
    iResultX, iResultY, iUResultX, iUResultY: Integer;
    sEquation1, sEquation2: string;
  end;

var
  fEquations2: TfEquations2;

implementation

{$R *.lfm}

{ Generate equation system }

procedure GenerateEquations (var Equ1, Equ2 : string; var X, Y: Integer);

var
  A, B, C, D, E, F, Det, DetX, DetY: Integer;

{ Pretty format equation }

function FormatEquation(X, Y, C: Integer): string;

var
  Equ : string;

begin
  Equ := '';
  if X <> 0 then begin                                                         // don't print '0x'
    if X = 1 then                                                              // print 'x' instead of '1x'
      Equ := 'x'
    else if X = -1 then                                                        // print '-x' instead of '-1x'
      Equ := '-x'
    else
      Equ := IntToStr(X) + 'x';
    if Y > 0 then                                                              // pretty display for +/-
      Equ += ' + '
    else if Y < 0 then
      Equ += ' - '
  end;
  if Y <> 0 then begin
    if (RightStr(Equ, 3) = ' + ') or (RightStr(Equ, 3) = ' - ') then begin     // pretty display for y (x <> 0)
      if Abs(Y) = 1 then
        Equ += 'y'
      else
        Equ += IntToStr(Abs(Y)) + 'y';
    end
    else begin                                                                 // pretty display for y/-y (x = 0)
      if Y = 1 then
        Equ += 'y'
      else if Y = -1 then
        Equ += '-y'
      else
        Equ += IntToStr(Y) + 'y';
    end;
  end;
  Equ += ' = ' + IntToStr(C);
  FormatEquation := Equ;
end;

begin
  // System of the 2 equations: ax + by = e and cx + dy = f
  // Generate random values until they are 'ok'. In particular until result
  // is an integer or undefined. Eliminate equations where x or y 'missing'
  // in both equations as well as equations like 0x + 0y = k or 1x + 1y = k
  repeat
    A := Random(21) - 10; B := Random(21) - 10;                                // all coefficients between -10 and 10
    C := Random(21) - 10; D := Random(21) - 10;
    E := Random(21) - 10; F := Random(21) - 10;
    Det  := A * D - B * C;
    DetX := E * D - B * F;
    DetY := A * F - E * C;
  until ((Det = 0) or ((DetX mod Det = 0) and (DetY mod Det = 0)))
    and ((A <> 0) or (C <> 0)) and ((B <> 0) or (D <> 0))
    and ((A <> 0) or (B <> 0)) and ((C <> 0) or (D <> 0))
    and ((A <> 1) or (B <> 1)) and ((C <> 1) or (D <> 1));
  Equ1 := FormatEquation(A, B, E);                                             // pretty format the equations
  Equ2 := FormatEquation(C, D, F);
  // Solution of the equation system using determinants and Cramer's rule
  if Det = 0 then begin
    if (DetX <> 0) or (DetY <> 0)then begin
      X := -MaxInt; Y := -MaxInt;                                              // -MaxInt used as 'no solution'
    end
    else begin
      X := MaxInt; Y := MaxInt;                                                // MaxInt used as 'infinity of solutions'
    end;
  end
  else begin
    X := DetX div Det;
    Y := DetY div Det;
  end;
end;

{****************}
{* TfEquations2 *}
{****************}

{ Application start: Initialisation }

procedure TfEquations2.FormCreate(Sender: TObject);

begin
  iEquations := 10; iCorrect := 0;
  Randomize;
end;

{ Button 'Start/Restart' resp. 'Next/Evaluate': Generate equations resp. evaluate the answers given }

procedure TfEquations2.btStartClick(Sender: TObject);

var
  PCorrect: Real;
  S: string;

begin
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Restart') then begin
    // Reset variables
    iEquations := StrToInt(edNEquations.Text);                                 // number of equations as user entry
    if iEquations > 0 then begin
      iEquation := 0; iCorrect := 0;
      edNEquations.ReadOnly := True;
      edNEquations.TabStop := False;
      edNEquations.Color := clForm;
      btStart.Caption := 'Next';
      btSolve.Enabled := False;
      edSolutionX.TabStop := False;
      edSolutionY.TabStop := False;
    end;
  end;
  edSolutionX.Text := ''; edSolutionY.Text := '';
  edSolCheck.Text := '';
  rgSolution.ItemIndex := -1;                                                  // uncheck all radio buttons
  // Generate equations
  Inc(iEquation);                                                              // increment actual equation number
  if iEquation <= iEquations then begin                                        // still equation(s) remaining
    EquNumber.Text := 'Equation ' + IntToStr(iEquation) + ' / ' + IntToStr(iEquations);
    GenerateEquations(sEquation1, sEquation2, iResultX, iResultY);             // generate a random equation system
    edEquation1.Text := sEquation1;
    edEquation2.Text := sEquation2;
    edSolutionX.TabStop := True; edSolutionY.TabStop := True;
    btSolve.Enabled := True;
    edSolutionX.SetFocus;
  end
  // Evaluate the answers
  else begin
    PCorrect := 100 * iCorrect / iEquations;
    PCorrect := Round(100 * PCorrect) / 100;                                   // round to 2 decimal digits
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
    edEquation1.Text := ''; edEquation2.Text := '';
    btStart.Caption := 'Restart';
    edNEquations.SetFocus;
  end;
end;

{ Button 'Solve': Check the equations solution proposed by the user }

procedure TfEquations2.btSolveClick(Sender: TObject);

var
  C: Integer;
  S: string;

begin
  // Get user answer from form
  if rgSolution.ItemIndex = 0 then begin                                       // one real value solution
    S := edSolutionX.Text;
    Val(S, iUResultX, C);
    if C <> 0 then                                                             // non-numeric entry
      iUResultX := MaxInt div 2;                                               // use MaxInt/2 as 'always false'
    S := edSolutionY.Text;
    Val(S, iUResultY, C);
    if C <> 0 then
      iUResultY := MaxInt div 2;
  end
  else if rgSolution.ItemIndex = 1 then begin                                  // no solution
    iUResultX := -MaxInt; iUResultY := -MaxInt
  end
  else if rgSolution.ItemIndex = 2 then begin                                  // infinity of solutions
    iUResultX := MaxInt; iUResultY := MaxInt
  end
  else begin                                                                        // nothing entered at all
    iUResultX := MaxInt div 2; iUResultY := MaxInt div 2;
  end;
  // Correct answer
  if (iUResultX = iResultX) and (iUResultY = iResultY) then begin
    edSolCheck.Font.Color := clLime;
    edSolCheck.Text := 'Your solution is correct';
    Inc(iCorrect);
  end
  // False answer
  else begin
    edSolCheck.Font.Color := clRed;
    S := 'Correct solution is:';
    if iResultX = -MaxInt then
      S += ' No solution;'
    else if iResultX = MaxInt then
      S += ' Infinity of solutions'
    else begin
      S += ' x = ' + IntToStr(iResultX);
      S += '; y = ' + IntToStr(iResultY);
    end;
    edSolCheck.Text := S;
  end;
  btSolve.Enabled := False;
  edSolutionX.TabStop := False;
  edSolutionY.TabStop := False;
  if iEquation = iEquations then
    btStart.Caption := 'Evaluation';
  btStart.SetFocus;
end;

{ Button 'Close' : Terminate the program}

procedure TfEquations2.btCloseClick(Sender: TObject);

begin
  close;
end;

{ Auto-check 'one solution' radiobutton, if user enters a result proposal }

procedure TfEquations2.edSolutionXChange(Sender: TObject);

begin
  rgSolution.ItemIndex := 0;
end;

procedure TfEquations2.edSolutionYChange(Sender: TObject);

begin
  rgSolution.ItemIndex := 0;
end;

end.

