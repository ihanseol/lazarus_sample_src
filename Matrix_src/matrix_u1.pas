{************************************}
{* Main unit for Matrix application *}
{************************************}

unit matrix_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, matrix_u2, help;

type
  TMatrixEdit = array[0..3, 0..3] of TEdit;
  TAllMatrixEdit = array[1..3] of TMatrixEdit;
  {**********}
  { TfMatrix }
  {**********}
  TfMatrix = class(TForm)
    mMenu: TMainMenu;
    mCalc, mCalcArith, mCalcRow, mCalcExit: TMenuItem;
    mOptions, mOptionsRandom, mOptionsRandomMin, mOptionsRandomMax, mOptionsRandomInt: TMenuItem;
    mOptionsSeq, mOptionsSeqStart, mOptionsSeqStep: TMenuItem;
    mOptionsSpecial, mOptionsSpecialRandom, mOptionsSpecialSeq, mOptionsSpecialUser: TMenuItem;
    mHelpHelp, mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    rbAdd, rbSub, rbScalMult, rbMult, rbInv: TRadioButton;
    rbTrans, rbRowSwitch, rbRowMult, rbRowAdd, rbRowSub: TRadioButton;
    laMatrixA, laSizeA, laSizeAOp, laMatrixB, laSizeB: TLabel;
    laSizeBOp, laMatrixRes, laSizeRes, laSizeResOp: TLabel;
    edSizeARows, edSizeACols, edSizeBRows, edSizeBCols, edSizeResRows, edSizeResCols: TEdit;
    cobMatrixA, cobMatrixB: TComboBox;
    edMatrixA1, edMatrixA2, edMatrixA3, edMatrixA4, edMatrixA5, edMatrixA6, edMatrixA7, edMatrixA8: TEdit;
    edMatrixA9, edMatrixA10, edMatrixA11, edMatrixA12, edMatrixA13, edMatrixA14, edMatrixA15, edMatrixA16: TEdit;
    edMatrixB1, edMatrixB2, edMatrixB3, edMatrixB4, edMatrixB5, edMatrixB6, edMatrixB7, edMatrixB8: TEdit;
    edMatrixB9, edMatrixB10, edMatrixB11, edMatrixB12, edMatrixB13, edMatrixB14, edMatrixB15, edMatrixB16: TEdit;
    edMatrixR1, edMatrixR2, edMatrixR3, edMatrixR4, edMatrixR5, edMatrixR6, edMatrixR7, edMatrixR8: TEdit;
    edMatrixR9, edMatrixR10, edMatrixR11, edMatrixR12, edMatrixR13, edMatrixR14, edMatrixR15, edMatrixR16: TEdit;
    laScalar, laRowI, laRowJ: TLabel;
    edScalar, edRowI, edRowJ: TEdit;
    shBracketBLeft, shBracketRLeft, shBracketBLeft2, shBracketBLeft3, shBracketARight, shBracketALeft2: TShape;
    shBracketALeft, shBracketALeft3, shBracketRLeft2, shBracketRLeft3, shBracketBRight, shBracketARight2: TShape;
    shBracketARight3, shBracketRRight, shBracketBRight2, shBracketBRight3, shBracketRRight2, shBracketRRight3: TShape;
    btCalc1: TButton;
    btCalc2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mCalcArithClick(Sender: TObject);
    procedure mCalcRowClick(Sender: TObject);
    procedure mCalcExitClick(Sender: TObject);
    procedure mOptionsRandomMinClick(Sender: TObject);
    procedure mOptionsRandomMaxClick(Sender: TObject);
    procedure mOptionsRandomIntClick(Sender: TObject);
    procedure mOptionsSeqStartClick(Sender: TObject);
    procedure mOptionsSeqStepClick(Sender: TObject);
    procedure mOptionsSpecialRandomClick(Sender: TObject);
    procedure mOptionsSpecialSeqClick(Sender: TObject);
    procedure mOptionsSpecialUserClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbAddChange(Sender: TObject);
    procedure rbSubChange(Sender: TObject);
    procedure rbScalMultChange(Sender: TObject);
    procedure rbMultChange(Sender: TObject);
    procedure rbInvChange(Sender: TObject);
    procedure rbTransChange(Sender: TObject);
    procedure rbRowSwitchChange(Sender: TObject);
    procedure rbRowMultChange(Sender: TObject);
    procedure rbRowAddChange(Sender: TObject);
    procedure rbRowSubChange(Sender: TObject);
    procedure cobMatrixAChange(Sender: TObject);
    procedure cobMatrixBChange(Sender: TObject);
    procedure btCalc1Click(Sender: TObject);
    procedure btCalc2Click(Sender: TObject);
    procedure edSizeARowsEditingDone(Sender: TObject);
    procedure edSizeAColsEditingDone(Sender: TObject);
    procedure edSizeBRowsEditingDone(Sender: TObject);
    procedure edSizeBColsEditingDone(Sender: TObject);
  private
    rRandomMin, rRandomMax, rSeqStart, rSeqStep: Real;
    sSpecialFill: string;
    bRandomIntOnly: Boolean;
    aMatrixA, aMatrixB, aMatrixR: TMatrix;
    rdSizeA, rdSizeB, rdSizeR: TMatrixSize;
    edMatrices: TAllMatrixEdit;
    shBrackets: array[0..17] of TShape;
  end;

const
  SUP_MINUS = #$E2#$81#$BB;
  SUP_1 = #$C2#$B9;
  SUP_T = 'ᵀ';

var
  fMatrix: TfMatrix;

implementation

{$R *.lfm}

{ Get nth power of a real number (n = integer) }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Format real number display with f decimal digits }

function RFormat(R: Real; F: Integer): string;

 var
   R0: Real;
   SR: string;

 begin
   if R = 0 then
     SR := '0'
   else begin
     SR := '';
     if F >= 0 then begin
       R0 := Round(R * Power(10, F)) / Power(10, F);
       if Abs(R0) < Power(10, -F) then
         // If number to small to be displayed with f decimal digits, use exponential format
         SR := FloatToStrF(R, ffExponent, F, 0)
       else
         // If f can be rounded to f decimal digits, use floating point format (with f decimal digits)
         SR := FloatToStr(R0);
     end;
   end;
   Result := SR;
 end;

{ Clear one or more matrices (on the form) }

procedure ClearMatrices(var MatrixEdt: TAllMatrixEdit; ClearA, ClearB, ClearR: Boolean);

var
  I, R, C: Integer;

begin
  for I := 1 to 3 do begin
    for R := 0 to 3 do begin
      for C := 0 to 3 do begin
        if ((I = 1) and ClearA) or ((I = 2) and ClearB) or ((I = 3) and ClearR) then
          MatrixEdt[I][R, C].Text := '';
      end;
    end;
  end;
end;

{ Show the second matrix (2 operands operations) }

procedure MatrixShow(var MatrixEdt: TMatrixEdit; Size: TMatrixSize);

var
  R, C: Integer;

begin
  for R := 0 to 3 do begin
    for C := 0 to 3 do begin
      // Show or hide edit fields (depending on matrix size)
      if (R < Size.Rows) and (C < Size.Cols) then
        MatrixEdt[R, C].Visible := True
      else
        MatrixEdt[R, C].Visible := False;
    end;
  end;
  // Show all matrix related controls
  fMatrix.laMatrixB.Visible := True;
  fMatrix.laSizeB.Visible := True; fMatrix.laSizeBOp.Visible := True;
  fMatrix.edSizeBRows.Visible := True; fMatrix.edSizeBCols.Visible := True;
  fMatrix.shBracketBLeft.Visible := True; fMatrix.shBracketBLeft2.Visible := True; fMatrix.shBracketBLeft3.Visible := True;
  fMatrix.shBracketBRight.Visible := True; fMatrix.shBracketBRight2.Visible := True; fMatrix.shBracketBRight3.Visible := True;
end;

{ Hide the second matrix (1 operand operations) }

procedure MatrixHide(var MatrixEdt: TMatrixEdit);

var
  R, C: Integer;

begin
  for R := 0 to 3 do begin
    for C := 0 to 3 do
      // Hide edit fields
      MatrixEdt[R, C].Visible := False;
  end;
  // Hide all matrix related controls
  fMatrix.laMatrixB.Visible := False;
  fMatrix.laSizeB.Visible := False; fMatrix.laSizeBOp.Visible := False;
  fMatrix.edSizeBRows.Visible := False; fMatrix.edSizeBCols.Visible := False;
  fMatrix.shBracketBLeft.Visible := False; fMatrix.shBracketBLeft2.Visible := False; fMatrix.shBracketBLeft3.Visible := False;
  fMatrix.shBracketBRight.Visible := False; fMatrix.shBracketBRight2.Visible := False; fMatrix.shBracketBRight3.Visible := False;
end;

{ Adapt matrix display for given size }

procedure AdaptMatrix(var MatrixEdt: TMatrixEdit; var BracketShp: array of TShape; Matrix, Rows, Cols: Integer);

var
  I0, R, C: Integer;

begin
  // Show or hide matrix edit fields
  for R := 0 to 3 do begin
    for C := 0 to 3 do begin
      if (R < Rows) and (C < Cols) then
        MatrixEdt[R, C].Visible := True
      else
        MatrixEdt[R, C].Visible := False;
    end;
  end;
  // Adapt matrix delimiter shapes length and position
  I0 := (Matrix - 1) * 6;
  BracketShp[I0].Height := 175 - (4 - Rows) * 40; BracketShp[I0 + 3].Height := 175 - (4 - Rows) * 40;
  BracketShp[I0 + 2].Top := 364 - (4 - Rows) * 40; BracketShp[I0 + 5].Top := 364 - (4 - Rows) * 40;
  BracketShp[I0 + 3].Left := BracketShp[I0].Left + 350 - (4 - Cols) * 85;
  BracketShp[I0 + 4].Left := BracketShp[I0 + 3].Left - 10; BracketShp[I0 + 5].Left := BracketShp[I0 + 3].Left - 10;
  if Matrix = 1 then
    fMatrix.cobMatrixA.ItemIndex := 0
  else if Matrix = 2 then
    fMatrix.cobMatrixB.ItemIndex := 0;
end;

{ Read matrix elements from form (user input) }

procedure ReadMatrix(var MatrixEdt: TMatrixEdit; Size: TMatrixSize; out Matrix: TMatrix; out Mess: string);

var
  R, C: Integer;

begin
  Mess := '';
  for R := 0 to Size.Rows - 1 do begin
    for C := 0 to Size.Cols - 1 do begin
      if MatrixEdt[R, C].Text = '' then begin
        // Error message if user forgot to fill in a matrix element
        if Mess = '' then begin
          Mess := 'Matrix X: Missing values!';
          MatrixEdt[R, C].SetFocus;
        end;
      end
      else begin
        // Get value of matrix element from form
        Matrix[R, C] := StrToFloat(MatrixEdt[R, C].Text);
      end;
    end;
  end;
end;

{ Write matrix elements (into the form's matrix edit fields) }

procedure WriteMatrix(var MatrixEdt: TMatrixEdit; Size: TMatrixSize; var Matrix: TMatrix);

var
  R, C: Integer;

begin
  for R := 0 to Size.Rows - 1 do begin
    for C := 0 to Size.Cols - 1 do begin
      MatrixEdt[R, C].Text := RFormat(Matrix[R, C], 2);
    end;
  end;
end;

{ 1-operand matrix operations }

procedure MatrixOperation1(Operation: string; MatrixA: TMatrix; SizeA: TMatrixSize; out MatrixR: TMatrix; out Mess: string);

var
  I, J: Integer;
  K: Real;

begin
  Mess := '';
  // Scalar multiplication
  if Operation = 'scalar multiplication' then begin
    if fMatrix.edScalar.Text = '' then
      // Error message if multiplier scalar is not filled in
      Mess := 'Invalid scalar value'
    else begin
      // Scalar multiplication calculation (with K read from form)
      K := StrToFloat(fMatrix.edScalar.Text);
      MatrixR := MatrixScalarMultiplication(MatrixA, SizeA, K);
    end;
  end
  // Matrix inversion
  else if Operation = 'inversion' then begin
    if SizeA.Rows  <> SizeA.Cols then
      // Matrix inverse only defined for square matrices
      Mess := 'Only square matrices may be inverted'
    else if SizeA.Rows > 2 then
      // Application limitation: calculation only done for 1x1 and 2x2 matrices
      Mess := 'Sorry, application calculates inverse only for 1x1 and 2x2 matrices'
    else if (SizeA.Rows = 1) and (MatrixA[0, 0] = 0) then
      // 1/0 is not defined
      Mess := 'The inverse of this matrix does not exist'
    else if (SizeA.Rows = 2) and (MatrixDeterminant2(MatrixA, SizeA) = 0) then
      // If detA = 0, inverse matrix does not exist
      Mess := 'The inverse of this matrix does not exist'
    else begin
      // Matrix inverse calculation
      if SizeA.Rows = 1 then
        // 1x1 matrix actually is a number
        MatrixR[0, 0] := 1 / MatrixA[0, 0]
      else
        // 2x2 matrix inverse calculation
        MatrixR := MatrixInversion2(MatrixA, SizeA);
    end;
  end
  // Matrix transposition
  else if Operation = 'transposition' then begin
    // Matrix transpose calculation
    MatrixR := MatrixTransposition(MatrixA, SizeA);
  end
  // Row switching
  else if Operation = 'row switching' then begin
    if SizeA.Rows = 1 then
      // A 1-row matrix can't be switched
      Mess := 'Row switching is not possible with a one-row matrix'
    else begin
      if fMatrix.edRowI.Text = '' then
        I := -1
      else
        I := StrToInt(fMatrix.edRowI.Text) - 1;
      if fMatrix.edRowJ.Text = '' then
        J := -1
      else
        J := StrToInt(fMatrix.edRowJ.Text) - 1;
      // I and J must be valid row values and they must have different values
      if (I < 0) or (I > SizeA.Rows - 1) then
        Mess := 'Invalid row i'
      else if (J < 0) or (J > SizeA.Rows - 1) then
        Mess := 'Invalid row j'
      else if I = J then
        Mess := 'Rows i and j must be different'
      else
        // Row switching calculation
        MatrixR := MatrixRowSwitching(MatrixA, SizeA, I, J);
    end;
  end
  // Row multiplication
  else if Operation = 'row multiplication' then begin
    if SizeA.Rows = 1 then
      // Row-multiplication is not possible with a 1-row matrix
      Mess := 'Row multiplication is not possible with a one-row matrix'
    else begin
      if fMatrix.edRowI.Text = '' then
        I := -1
      else
        I := StrToInt(fMatrix.edRowI.Text) - 1;
      // The scalar must be a numeric value different from zero; I must be a valid row value
      if (fMatrix.edScalar.Text = '') or (StrToFloat(fMatrix.edScalar.Text) = 0) then
        Mess := 'Invalid scalar value'
      else if (I < 0) or (I > SizeA.Rows - 1) then
        Mess := 'Invalid row i'
      else begin
        // Row multiplication calculation
        K := StrToFloat(fMatrix.edScalar.Text);
        MatrixR := MatrixRowMultiplication(MatrixA, SizeA, I, K);
      end;
    end;
  end
  // Row addition and row subtraction
  else if (Operation = 'row addition') or (Operation = 'row subtraction') then begin
    if SizeA.Rows = 1 then begin
      // Row addition/subtraction is not possible with a 1-row matrix
      if Operation = 'row addition' then
        Mess := 'Row addition is not possible with a one-row matrix'
      else
        Mess := 'Row subtraction is not possible with a one-row matrix';
    end
    else begin
      if fMatrix.edRowI.Text = '' then
        I := -1
      else
        I := StrToInt(fMatrix.edRowI.Text) - 1;
      if fMatrix.edRowJ.Text = '' then
        J := -1
      else
        J := StrToInt(fMatrix.edRowJ.Text) - 1;
      // The scalar must be a numeric value different from zero; I and J must be a valid row values and be different
      if (fMatrix.edScalar.Text = '') or (StrToFloat(fMatrix.edScalar.Text) = 0) then
        Mess := 'Invalid scalar value'
      else if (I < 0) or (I > SizeA.Rows - 1) then
        Mess := 'Invalid row i'
      else if (J < 0) or (J > SizeA.Rows - 1) then
        Mess := 'Invalid row j'
      else if I = J then
        Mess := 'Rows i and j must be different'
      else begin
        // Row addition/subtraction calculation
        K := StrToFloat(fMatrix.edScalar.Text);
        if Operation = 'row subtraction' then
          K := -K;
        MatrixR := MatrixRowAddition(MatrixA, SizeA, I, J, K);
      end;
    end;
  end
end;

{ 2-operands matrix operations }

procedure MatrixOperation2(Operation: string; MatrixA, MatrixB: TMatrix; SizeA, SizeB: TMatrixSize; out MatrixR: TMatrix);

begin
  // Matrix addition and subtraction
  if (Operation = 'addition') or (Operation = 'subtraction') then begin
    if Operation = 'subtraction' then
      MatrixB := MatrixNegation(MatrixB, SizeB);
    MatrixR := MatrixAddition(MatrixA, MatrixB, SizeA);
  end
  // Matrix multiplication
  else begin
    MatrixR := MatrixMultiplication(MatrixA, MatrixB, SizeA, SizeB);
  end;
end;

{ Random matrix generation }

procedure RandomMatrix(var MatrixEdt: TMatrixEdit; Size: TMatrixSize; Min, Max: Real; IntOnly: Boolean);

var
  R, C, D: Integer;
  V: Real;

begin
  for R := 0 to Size.Rows - 1 do begin;
    for C := 0 to Size.Cols - 1 do begin
      repeat
        V := Random(Round(Int(Max - Min + 1))) + Round(Int(Min));
        if not IntOnly then begin
          // Gernerate decimal values
          D := Random(6);
          if D = 0 then
            V += Random(100) / 100
          else if D <= 2 then
            V += Random(10) / 10;
        end;
      until (V >= Min) and (V <= Max);                                         // value must be within [min, max] interval
      MatrixEdt[R, C].Text := RFormat(V, 2);
    end;
  end;
end;

{ Sequence value matrix generation }

procedure SequenceMatrix(var MatrixEdt: TMatrixEdit; Size: TMatrixSize; Start, Step: Real);

var
  R, C: Integer;
  V: Real;

begin
  V := Start;
  for R := 0 to Size.Rows - 1 do begin;
    for C := 0 to Size.Cols - 1 do begin
      MatrixEdt[R, C].Text := RFormat(V, 2);
      V += Step;
    end;
  end;
end;

{ Triangular matrix generation }

procedure TriangularMatrix(TType: string; var MatrixEdt: TMatrixEdit; Size: TMatrixSize; Fill: string; N, M: Real; IntOnly: Boolean);

var
  Min, Max, R, C, D: Integer;
  Start, Step, V: Real;

begin
  if Fill = 'random' then begin
    Min := Round(N); Max := Round(M);
  end
  else if Fill = 'sequence' then begin
    Start := N; Step := M; V := Start - Step;
  end;
  for R := 0 to Size.Rows - 1 do begin;
    for C := 0 to Size.Cols - 1 do begin
      MatrixEdt[R, C].ReadOnly := True;                                        // set all fields to read-only here; reset later if must be writable
      if ((TType = 'lower') and (C <= R)) or ((TType = 'upper') and (C >= R)) or ((TType = 'diagonal') and (C = R)) then begin
        // Fill the non-zero elements of the matrix
        if Fill = 'user' then begin
          // Custom matrix: Make edit fields writable for user to entry the matrix elements
          MatrixEdt[R, C].ReadOnly := False;
          MatrixEdt[R, C].Text := '';
        end
        else begin
          if Fill = 'random' then begin
            // Set the matrix elements to random values
            repeat
              V := Random(Round(Int(Max - Min + 1))) + Round(Int(Min));
              if not IntOnly then begin
                // Generate decimal values
                D := Random(6);
                if D = 0 then
                  V += Random(100) / 100
                else if D <= 2 then
                  V += Random(10) / 10;
              end;
            until (V >= Min) and (V <= Max) and (V <> 0);                      // value must be in [min, max] interval and arbitrarily chosen <> 0
          end
          else if Fill = 'sequence' then begin
            // Set the matrix elements to sequence values
            V += Step;
          end;
          // Set the matrix element to determined value
          MatrixEdt[R, C].Text := RFormat(V, 2);
        end;
      end
      else
        // Set other matrix elements to 0
        MatrixEdt[R, C].Text := '0';
    end;
  end;
end;

{ Generate symmetric/skew-symmetric matrix }

procedure SymmetricMatrix(SType: string; var MatrixEdt: TMatrixEdit; Size: TMatrixSize; Fill: string; N, M: Real; IntOnly: Boolean);

var
  Min, Max, R, C, D: Integer;
  Start, Step, V: Real;

begin
  if Fill = 'random' then begin
    Min := Round(N); Max := Round(M);
  end
  else if Fill = 'sequence' then begin
    Start := N; Step := M; V := Start - Step;
  end;
  for R := 0 to Size.Rows - 1 do begin;
    for C := 0 to Size.Cols - 1 do begin
      MatrixEdt[R, C].ReadOnly := True;
      if C >= R then begin
        // Fill in upper half of matrix (incl. the diagonal)
        if (SType = 'skewsym') and (R = C) then
          // Diagonal of a skew-symmetric matrix is all zeros
          MatrixEdt[R, C].Text := '0'
        else begin
          // Set matrix elements to random values
          if Fill = 'random' then begin
            repeat
              V := Random(Round(Int(Max - Min + 1))) + Round(Int(Min));
              if not IntOnly then begin
                // Generate decimal values
                D := Random(6);
                if D = 0 then
                  V += Random(100) / 100
                else if D <= 2 then
                  V += Random(10) / 10;
              end;
            until (V >= Min) and (V <= Max) and (V <> 0);                      // value must be in [min, max] interval and arbitrarily chosen <> 0
          end
          // Set matrix elements to sequence values
          else begin
            V += Step;
          end;
          MatrixEdt[R, C].Text := RFormat(V, 2);
        end;
      end
      else begin
        // Fill in lower half of matrix: (i, j) = (j, i) resp -(j, i)
        if SType = 'sym' then
          MatrixEdt[R, C].Text := MatrixEdt[C, R].Text
        else
          MatrixEdt[R, C].Text := RFormat(-StrToFloat(MatrixEdt[C, R].Text), 2);
      end;
    end;
  end;
end;

{**********}
{ TfMatrix }
{**********}

{ Application start: Initialisation }

procedure TfMatrix.FormCreate(Sender: TObject);

begin
  // Move row operations radio buttons to where they belong
  rbTrans.Top := rbAdd.Top; rbRowSwitch.Top := rbAdd.Top; rbRowMult.Top := rbAdd.Top;
  rbRowAdd.Top := rbAdd.Top; rbRowSub.Top := rbAdd.Top;
  laScalar.Left := 919; edScalar.Left := 987;
  // Create array of 2-dimensional array of the 3 matrices edit fields
  edMatrices[1][0, 0] := edMatrixA1;  edMatrices[1][0, 1] := edMatrixA2;  edMatrices[1][0, 2] := edMatrixA3;  edMatrices[1][0, 3] := edMatrixA4;
  edMatrices[1][1, 0] := edMatrixA5;  edMatrices[1][1, 1] := edMatrixA6;  edMatrices[1][1, 2] := edMatrixA7;  edMatrices[1][1, 3] := edMatrixA8;
  edMatrices[1][2, 0] := edMatrixA9;  edMatrices[1][2, 1] := edMatrixA10; edMatrices[1][2, 2] := edMatrixA11; edMatrices[1][2, 3] := edMatrixA12;
  edMatrices[1][3, 0] := edMatrixA13; edMatrices[1][3, 1] := edMatrixA14; edMatrices[1][3, 2] := edMatrixA15; edMatrices[1][3, 3] := edMatrixA16;
  edMatrices[2][0, 0] := edMatrixB1;  edMatrices[2][0, 1] := edMatrixB2;  edMatrices[2][0, 2] := edMatrixB3;  edMatrices[2][0, 3] := edMatrixB4;
  edMatrices[2][1, 0] := edMatrixB5;  edMatrices[2][1, 1] := edMatrixB6;  edMatrices[2][1, 2] := edMatrixB7;  edMatrices[2][1, 3] := edMatrixB8;
  edMatrices[2][2, 0] := edMatrixB9;  edMatrices[2][2, 1] := edMatrixB10; edMatrices[2][2, 2] := edMatrixB11; edMatrices[2][2, 3] := edMatrixB12;
  edMatrices[2][3, 0] := edMatrixB13; edMatrices[2][3, 1] := edMatrixB14; edMatrices[2][3, 2] := edMatrixB15; edMatrices[2][3, 3] := edMatrixB16;
  edMatrices[3][0, 0] := edMatrixR1;  edMatrices[3][0, 1] := edMatrixR2;  edMatrices[3][0, 2] := edMatrixR3;  edMatrices[3][0, 3] := edMatrixR4;
  edMatrices[3][1, 0] := edMatrixR5;  edMatrices[3][1, 1] := edMatrixR6;  edMatrices[3][1, 2] := edMatrixR7;  edMatrices[3][1, 3] := edMatrixR8;
  edMatrices[3][2, 0] := edMatrixR9;  edMatrices[3][2, 1] := edMatrixR10; edMatrices[3][2, 2] := edMatrixR11; edMatrices[3][2, 3] := edMatrixR12;
  edMatrices[3][3, 0] := edMatrixR13; edMatrices[3][3, 1] := edMatrixR14; edMatrices[3][3, 2] := edMatrixR15; edMatrices[3][3, 3] := edMatrixR16;
  // Create array with the matrix delimiter shapes
  shBrackets[0] := shBracketALeft; shBrackets[1] := shBracketALeft2; shBrackets[2] := shBracketALeft3;
  shBrackets[3] := shBracketARight; shBrackets[4] := shBracketARight2; shBrackets[5] := shBracketARight3;
  shBrackets[6] := shBracketBLeft; shBrackets[7] := shBracketBLeft2; shBrackets[8] := shBracketBLeft3;
  shBrackets[9] := shBracketBRight; shBrackets[10] := shBracketBRight2; shBrackets[11] := shBracketBRight3;
  shBrackets[12] := shBracketRLeft; shBrackets[13] := shBracketRLeft2; shBrackets[14] := shBracketRLeft3;
  shBrackets[15] := shBracketRRight; shBrackets[16] := shBracketRRight2; shBrackets[17] := shBracketRRight3;
  // Start-up values
  rdSizeA.Rows := 4; rdSizeA.Cols := 4;
  rdSizeB.Rows := 4; rdSizeB.Cols := 4;
  rdSizeR.Rows := 4; rdSizeR.Cols := 4;
  rRandomMin := -10; rRandomMax := 10; bRandomIntOnly := True; rSeqStart := 1; rSeqStep := 1;
  sSpecialFill := 'random';
  Randomize;
end;

{ Menu item "Calculations > Arithmetic operations": Display radio buttons for arithmetic operations }

procedure TfMatrix.mCalcArithClick(Sender: TObject);

begin
  stTitle.Caption := 'Arithmetic matrix operations.';
  rbAdd.Visible := True; rbSub.Visible := True; rbScalMult.Visible := True; rbMult.Visible := True; rbInv.Visible := True;
  rbTrans.Visible := False; rbRowSwitch.Visible := False; rbRowMult.Visible := False; rbRowAdd.Visible := False; rbRowSub.Visible := False;
  rbAdd.Checked := True;
end;

{ Menu item "Calculations > Matrix row operations": Display radio buttons for matrix row operations }

procedure TfMatrix.mCalcRowClick(Sender: TObject);

begin
  stTitle.Caption := 'Basic matrix row operations.';
  rbAdd.Visible := False; rbSub.Visible := False; rbScalMult.Visible := False; rbMult.Visible := False; rbInv.Visible := False;
  rbTrans.Visible := True; rbRowSwitch.Visible := True; rbRowMult.Visible := True; rbRowAdd.Visible := True; rbRowSub.Visible := True;
  rbTrans.Checked := True;
end;

{ Menu item "Calculations > Exit": Exit application }

procedure TfMatrix.mCalcExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Random matrices > Minimum...": User input of minimum for random values generation }

procedure TfMatrix.mOptionsRandomMinClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Random values', 'Random values minimum', FloatToStr(rRandomMin));
  if S <> '' then begin
    if StrToFloat(S) <= rRandomMax then
      rRandomMin := StrToFloat(S)
    else
      MessageDlg('Invalid minimum', 'Minimum must be less than or equal to maximum value', mtError, [mbOK], 0);
  end;
end;

{ Menu item "Options > Random matrices > Maximum...": User input maximum for random values generation }

procedure TfMatrix.mOptionsRandomMaxClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Random values', 'Random values maximum', FloatToStr(rRandomMax));
  if S <> '' then begin
    if StrToFloat(S) >= rRandomMin then
      rRandomMax := StrToFloat(S)
    else
      MessageDlg('Invalid maximum', 'Maximum must be greater than or equal to minimum value', mtError, [mbOK], 0);
  end;
end;

{ Menu item "Options > Random matrices > Integers only": Toggle between integer and decimal random values }

procedure TfMatrix.mOptionsRandomIntClick(Sender: TObject);

begin
  if mOptionsRandomInt.Checked then
    mOptionsRandomInt.Checked := False
  else
    mOptionsRandomInt.Checked := True;
  bRandomIntOnly := mOptionsRandomInt.Checked;
end;

{ Menu item "Options > Sequence matrices > Start...": User input of start value for value sequence generation }

procedure TfMatrix.mOptionsSeqStartClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Sequence values', 'Sequence values start', FloatToStr(rSeqStart));
  if S <> '' then
    rSeqStart := StrToFloat(S);
end;

{ Menu item "Options > Sequence matrices > Step...": User input of step value for value sequence generation }

procedure TfMatrix.mOptionsSeqStepClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Sequence values', 'Sequence values step', FloatToStr(rSeqStep));
  if S <> '' then
    rSeqStep := StrToFloat(S);
end;

{ Menu items "Options > Special matrices > ...": Select how special matrices should be filled }

procedure TfMatrix.mOptionsSpecialRandomClick(Sender: TObject);

begin
  mOptionsSpecialRandom.Checked := True; mOptionsSpecialSeq.Checked := False; mOptionsSpecialUser.Checked := False;
  sSpecialFill := 'random';
end;

procedure TfMatrix.mOptionsSpecialSeqClick(Sender: TObject);

begin
  mOptionsSpecialRandom.Checked := False; mOptionsSpecialSeq.Checked := True; mOptionsSpecialUser.Checked := False;
  sSpecialFill := 'sequence';
end;

procedure TfMatrix.mOptionsSpecialUserClick(Sender: TObject);

begin
  mOptionsSpecialRandom.Checked := False; mOptionsSpecialSeq.Checked := False; mOptionsSpecialUser.Checked := True;
  sSpecialFill := 'user';
end;

{ Menu item "Help > Help": Display application help }

procedure TfMatrix.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfMatrix.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics: Matrix calculations.' + LineEnding;
  S += 'Arithmetic and row operations on 1x1 to 4x4 sized matrices' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, November 2021 - March 2022.';
  MessageDlg('About "Matrix"', S, mtInformation, [mbOK], 0);
end;

{ Operation radio button selection (by user): Adapt form controls for corresponding operation }

procedure TfMatrix.rbAddChange(Sender: TObject);

begin
  if rbAdd.Checked then begin
    laMatrixRes.Caption := 'Matrix A + B';
    btCalc1.Caption := 'A + B'; btCalc1.Visible := True; btCalc2.Caption := 'B + A';
    ClearMatrices(edMatrices, False, False, True);
    MatrixShow(edMatrices[2], rdSizeB); cobMatrixB.Visible := True;
    laScalar.Visible := False; edScalar.Visible := False;
    laRowI.Visible := False; edRowI.Visible := False;
    laRowJ.Visible := False; edRowJ.Visible := False;
  end;
end;

procedure TfMatrix.rbSubChange(Sender: TObject);

begin
  if rbSub.Checked then begin
    laMatrixRes.Caption := 'Matrix A - B';
    btCalc1.Caption := 'A - B'; btCalc1.Visible := True; btCalc2.Caption := 'B - A';
    ClearMatrices(edMatrices, False, False, True);
    MatrixShow(edMatrices[2], rdSizeB); cobMatrixB.Visible := True;
    laScalar.Visible := False; edScalar.Visible := False;
    laRowI.Visible := False; edRowI.Visible := False;
    laRowJ.Visible := False; edRowJ.Visible := False;
  end;
end;

procedure TfMatrix.rbScalMultChange(Sender: TObject);

begin
  if rbScalMult.Checked then begin
    laMatrixRes.Caption := 'Matrix k ∙ A';
    btCalc2.Caption := 'k ∙ A'; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := True; edScalar.Visible := True;
    laRowI.Visible := False; edRowI.Visible := False;
    laRowJ.Visible := False; edRowJ.Visible := False;
  end;
end;

procedure TfMatrix.rbMultChange(Sender: TObject);

begin
  if rbMult.Checked then begin
    laMatrixRes.Caption := 'Matrix A ∙ B';
    btCalc1.Caption := 'A ∙ B'; btCalc1.Visible := True; btCalc2.Caption := 'B ∙ A';
    ClearMatrices(edMatrices, False, False, True);
    MatrixShow(edMatrices[2], rdSizeB); cobMatrixB.Visible := True;
    laScalar.Visible := False; edScalar.Visible := False;
    laRowI.Visible := False; edRowI.Visible := False;
    laRowJ.Visible := False; edRowJ.Visible := False;
  end;
end;

procedure TfMatrix.rbInvChange(Sender: TObject);

begin
  if rbInv.Checked then begin
    laMatrixRes.Caption := 'Matrix A' + SUP_Minus + SUP_1;
    btCalc2.Caption := 'A' + SUP_Minus + SUP_1; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := False; edScalar.Visible := False;
    laRowI.Visible := False; edRowI.Visible := False;
    laRowJ.Visible := False; edRowJ.Visible := False;
  end;
end;

procedure TfMatrix.rbTransChange(Sender: TObject);

begin
  if rbTrans.Checked then begin
    laMatrixRes.Caption := 'Matrix A' + SUP_T;
    btCalc2.Caption := 'A' + SUP_T; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := False; edScalar.Visible := False;
    laRowI.Visible := False; edRowI.Visible := False;
    laRowJ.Visible := False; edRowJ.Visible := False;
  end;
end;

procedure TfMatrix.rbRowSwitchChange(Sender: TObject);

begin
  if rbRowSwitch.Checked then begin
    laMatrixRes.Caption := 'Result matrix';
    btCalc2.Caption := 'Switch'; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := False; edScalar.Visible := False;
    laRowI.Visible := True; edRowI.Visible := True;
    laRowJ.Visible := True; edRowJ.Visible := True;
    laRowJ.Left := 934; edRowJ.Left := 987;
    laRowI.Left := laRowJ.Left - 146; edRowI.Left := edRowJ.Left - 148;
  end;
end;

procedure TfMatrix.rbRowMultChange(Sender: TObject);

begin
  if rbRowMult.Checked then begin
    laMatrixRes.Caption := 'Result matrix';
    btCalc2.Caption := 'Multiply'; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := True; edScalar.Visible := True;
    laRowI.Visible := True; edRowI.Visible := True;
    laRowJ.Visible := False; edRowJ.Visible := False;
    laRowI.Left := laScalar.Left - 145; edRowI.Left := edScalar.Left - 163;
  end;
end;

procedure TfMatrix.rbRowAddChange(Sender: TObject);

begin
  if rbRowAdd.Checked then begin
    laMatrixRes.Caption := 'Result matrix';
    btCalc2.Caption := 'Add'; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := True; edScalar.Visible := True;
    laRowI.Visible := True; edRowI.Visible := True;
    laRowJ.Visible := True; edRowJ.Visible := True;
    laRowJ.Left := laScalar.Left - 145; edRowJ.Left := edScalar.Left - 163;
    laRowI.Left := laRowJ.Left - 146; edRowI.Left := edRowJ.Left - 148;
  end;
end;

procedure TfMatrix.rbRowSubChange(Sender: TObject);

begin
  if rbRowSub.Checked then begin
    laMatrixRes.Caption := 'Result matrix';
    btCalc2.Caption := 'Subtract'; btCalc1.Visible := False;
    ClearMatrices(edMatrices, False, False, True);
    MatrixHide(edMatrices[2]); cobMatrixB.Visible := False;
    laScalar.Visible := True; edScalar.Visible := True;
    laRowI.Visible := True; edRowI.Visible := True;
    laRowJ.Visible := True; edRowJ.Visible := True;
    laRowJ.Left := laScalar.Left - 145; edRowJ.Left := edScalar.Left - 163;
    laRowI.Left := laRowJ.Left - 146; edRowI.Left := edRowJ.Left - 148;
  end;
end;

{ Left calculation button pushed: Perform actual operation (addition, subtraction, multiplication) }

procedure TfMatrix.btCalc1Click(Sender: TObject);

var
  Operation, Mess: string;

begin
  Mess := '';
  if rbAdd.Checked then
    Operation := 'addition'
  else if rbSub.Checked then
    Operation := 'subtraction'
  else
    Operation := 'multiplication';
  if (Operation = 'addition') or (Operation = 'subtraction') then begin
    // Matrix addition and subtraction
    if (rdSizeA.Rows = rdSizeB.Rows) and (rdSizeA.Cols = rdSizeB.Cols) then begin
      // Same size matrices: Result matrix size is this size, too
      rdSizeR.Rows := rdSizeA.Rows;
      rdSizeR.Cols := rdSizeA.Cols;
    end
    else
      // Different size matrices: Addition not defined
      Mess := 'Matrices must be the same size for ' + Operation
  end
  else if Operation = 'multiplication' then begin
    // Matrix multiplication
    if rdSizeA.Cols = rdSizeB.Rows then begin
      // Number of matrix A rows = number of matrix B cols: Determine result matrix size
      rdSizeR.Rows := rdSizeA.Rows;
      rdSizeR.Cols := rdSizeB.Cols;
    end
    else
      // Number of matrix A rows = number of matrix B cols: Multiplication not defined
      Mess := 'Number of matrix A columns must be equal to number of matrix B rows for multiplication';
  end;
  if Mess = '' then begin
    // Operation for actual matrices size is defined
    edSizeResRows.Text := IntToStr(rdSizeR.Rows); edSizeResCols.Text := IntToStr(rdSizeR.Cols);
    AdaptMatrix(edMatrices[3], shBrackets, 3, rdSizeR.Rows, rdSizeR.Cols);     // Adapt result matrix display
    ReadMatrix(edMatrices[1], rdSizeA, aMatrixA, Mess);                        // Read matrix A element values from form
    if Mess <> '' then
      // Invalid data in matrix A
      Mess := StringReplace(Mess, 'X', 'A', [])
    else begin
      // Matrix A data ok: Proceed
      ReadMatrix(edMatrices[2], rdSizeB, aMatrixB, Mess);                      // Read matrix B element values from form
      if Mess <> '' then
        // Invalid data in matrix B
        Mess := StringReplace(Mess, 'X', ' B', [])
      else begin
        // All matrix data ok: Perform the operation and fill the result matrix edit fields
        MatrixOperation2(Operation, aMatrixA, aMatrixB, rdSizeA, rdSizeB, aMatrixR);
        WriteMatrix(edMatrices[3], rdSizeR, aMatrixR);
      end;
    end;
  end;
  // If there has been an error, display message
  if Mess <> '' then
    MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
end;

{ Right calculation button pushed: Perform actual operation (all operations, with B being left operand for 2 operands operations) }

procedure TfMatrix.btCalc2Click(Sender: TObject);

var
  Operation, Mess: string;

begin
  Mess := '';
  if rbAdd.Checked then
    Operation := 'addition'
  else if rbSub.Checked then
    Operation := 'subtraction'
  else if rbMult.Checked then
    Operation := 'multiplication'
  else if rbScalMult.Checked then
    Operation := 'scalar multiplication'
  else if rbInv.Checked then
    Operation := 'inversion'
  else if rbTrans.Checked then
    Operation := 'transposition'
  else if rbRowSwitch.Checked then
    Operation := 'row switching'
  else if rbRowMult.Checked then
    Operation := 'row multiplication'
  else if rbRowAdd.Checked then
    Operation := 'row addition'
  else
    Operation := 'row subtraction';
  // 2-operands operations
  if (Operation = 'addition') or (Operation = 'subtraction') or (Operation = 'multiplication') then begin
    // Addition or subtraction
    if (Operation = 'addition') or (Operation = 'subtraction') then begin
      if (rdSizeA.Rows = rdSizeB.Rows) and (rdSizeA.Cols = rdSizeB.Cols) then begin
        rdSizeR.Rows := rdSizeA.Rows;
        rdSizeR.Cols := rdSizeA.Cols;
      end
      else
        Mess := 'Matrices must be the same size for ' + Operation
    end
    // Multiplication
    else if Operation = 'multiplication' then begin
      if rdSizeB.Cols = rdSizeA.Rows then begin
        rdSizeR.Rows := rdSizeB.Rows;
        rdSizeR.Cols := rdSizeA.Cols;
      end
      else
        Mess := 'Number of matrix A columns must be equal to number of matrix B rows for multiplication';
    end;
    // Matrices sizes ok for actual operation: Proceed
    if Mess = '' then begin
      edSizeResRows.Text := IntToStr(rdSizeR.Rows); edSizeResCols.Text := IntToStr(rdSizeR.Cols);
      AdaptMatrix(edMatrices[3], shBrackets, 3, rdSizeR.Rows, rdSizeR.Cols);   // Adapt display of result matrix
      ReadMatrix(edMatrices[1], rdSizeA, aMatrixA, Mess);                      // Read matrix A elements
      if Mess <> '' then
        Mess := StringReplace(Mess, 'X', 'A', [])
      else begin
        ReadMatrix(edMatrices[2], rdSizeB, aMatrixB, Mess);                    // Read matrix B elements
        if Mess <> '' then
          Mess := StringReplace(Mess, 'X', 'B', [])
        else begin
          // All matrix data ok: Perform the operation and fill the result matrix edit fields
          MatrixOperation2(Operation, aMatrixB, aMatrixA, rdSizeB, rdSizeA, aMatrixR);
          WriteMatrix(edMatrices[3], rdSizeR, aMatrixR);
        end;
      end;
    end;
  end
  // 1-operand operations
  else begin
    // Determine number of rows and columns of result matrix
    if Operation = 'transposition' then begin
      rdSizeR.Rows := rdSizeA.Cols;
      rdSizeR.Cols := rdSizeA.Rows;
    end
    else begin
      rdSizeR.Rows := rdSizeA.Rows;
      rdSizeR.Cols := rdSizeA.Cols;
    end;
    // Read matrix elements from form
    edSizeResRows.Text := IntToStr(rdSizeR.Rows); edSizeResCols.Text := IntToStr(rdSizeR.Cols);
    AdaptMatrix(edMatrices[3], shBrackets, 3, rdSizeR.Rows, rdSizeR.Cols);     // Adapt display of result matrix
    ReadMatrix(edMatrices[1], rdSizeA, aMatrixA, Mess);                        // Read matrix elements
    if Mess <> '' then
      Mess := StringReplace(Mess, 'X', 'A', [])
    else begin
      // Matrix data ok: Perform the operation and fill the result matrix edit fields
      MatrixOperation1(Operation, aMatrixA, rdSizeA, aMatrixR, Mess);
      if Mess = '' then
        WriteMatrix(edMatrices[3], rdSizeR, aMatrixR);
    end;
  end;
  // If there has been an error, display message
  if Mess <> '' then
    MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
end;

{ Matrix A type combobox changes: Fill-in matrix A values (special matrix types) }

procedure TfMatrix.cobMatrixAChange(Sender: TObject);

var
  R, C: Integer;
  N, M: Real;
  SpecialFill, Mess: string;

begin
  Mess := '';
  for R := 0 to rdSizeA.Rows - 1 do begin;
    for C := 0 to rdSizeA.Cols - 1 do
      edMatrices[1][R, C].ReadOnly := False;
  end;
  if cobMatrixA.ItemIndex <> 0 then begin
    // Matrix type = special matrix
    if cobMatrixA.ItemIndex in [3..8] then begin
      // These matrix types are only defined for square matrices
      if rdSizeA.Rows <> rdSizeA.Cols then
        Mess := LowerCase(cobMatrixA.Text) + ' matrix is always a square matrix';
    end;
    if Mess = '' then begin
      // Matrix size = ok: Fill-in matrix elements
      SpecialFill := sSpecialFill;
      if (SpecialFill = 'user') and (cobMatrixA.ItemIndex in [3, 7, 8]) then
        // Inversion, symmetric and skew-symmetric matrices: Use random values instead of custom user input
        SpecialFill := 'random';
      if SpecialFill = 'random' then begin
        N := rRandomMin; M := rRandomMax;
      end
      else if SpecialFill = 'sequence' then begin
        N := rSeqStart; M := rSeqStep;
      end;
      // Fill-in matrix elements for selected matrix type
      case cobMatrixA.ItemIndex of
        1: RandomMatrix(edMatrices[1], rdSizeA, rRandomMin, rRandomMax, bRandomIntOnly);
        2: SequenceMatrix(edMatrices[1], rdSizeA, rSeqStart, rSeqStep);
        3: if SpecialFill = 'random' then
             TriangularMatrix('diagonal', edMatrices[1], rdSizeA, SpecialFill, 1, 1, True)
           else
             TriangularMatrix('diagonal', edMatrices[1], rdSizeA, SpecialFill, 1, 0, True);
        4: TriangularMatrix('diagonal', edMatrices[1], rdSizeA, SpecialFill, N, M, bRandomIntOnly);
        5: TriangularMatrix('lower', edMatrices[1], rdSizeA, SpecialFill, N, M, bRandomIntOnly);
        6: TriangularMatrix('upper', edMatrices[1], rdSizeA, SpecialFill, N, M, bRandomIntOnly);
        7: SymmetricMatrix('sym', edMatrices[1], rdSizeA, SpecialFill, N, M, bRandomIntOnly);
        8: SymmetricMatrix('skewsym', edMatrices[1], rdSizeA, SpecialFill, N, M, bRandomIntOnly);
      end;
    end;
  end;
  if Mess <> '' then begin
    // If there has been an error, display message
    if LeftStr(Mess, 1)[1] in ['i', 'u', 'e', 'o', 'a'] then
      Mess := 'An ' + Mess
    else
      Mess := 'A ' + Mess;
    MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Matrix B type combobox changes: Fill-in matrix A values (special matrix types) }

procedure TfMatrix.cobMatrixBChange(Sender: TObject);

var
  R, C: Integer;
  N, M: Real;
  SpecialFill, Mess: string;

begin
  Mess := '';
  for R := 0 to rdSizeB.Rows - 1 do begin;
    for C := 0 to rdSizeB.Cols - 1 do
      edMatrices[2][R, C].ReadOnly := False;
  end;
  // Matrix type = special matrix
  if cobMatrixB.ItemIndex <> 0 then begin
    if cobMatrixB.ItemIndex in [3..8] then begin
      // These matrix types are only defined for square matrices
      if rdSizeB.Rows <> rdSizeB.Cols then
        Mess := LowerCase(cobMatrixB.Text) + ' matrix is always a square matrix';
    end;
    if Mess = '' then begin
      // Matrix size = ok: Fill-in matrix elements
      SpecialFill := sSpecialFill;
      if (SpecialFill = 'user') and (cobMatrixB.ItemIndex in [3, 7, 8]) then
        // Inversion, symmetric and skew-symmetric matrices: Use random values instead of custom user input
        SpecialFill := 'random';
      if SpecialFill = 'random' then begin
        N := rRandomMin; M := rRandomMax;
      end
      else if SpecialFill = 'sequence' then begin
        N := rSeqStart; M := rSeqStep;
      end;
      // Fill-in matrix elements for selected matrix type
      case cobMatrixB.ItemIndex of
        1: RandomMatrix(edMatrices[2], rdSizeB, rRandomMin, rRandomMax, bRandomIntOnly);
        2: SequenceMatrix(edMatrices[2], rdSizeB, rSeqStart, rSeqStep);
        3: if SpecialFill = 'random' then
             TriangularMatrix('diagonal', edMatrices[2], rdSizeB, SpecialFill, 1, 1, True)
           else
             TriangularMatrix('diagonal', edMatrices[2], rdSizeB, SpecialFill, 1, 0, True);
        4: TriangularMatrix('diagonal', edMatrices[2], rdSizeB, SpecialFill, N, M, bRandomIntOnly);
        5: TriangularMatrix('lower', edMatrices[2], rdSizeB, SpecialFill, N, M, bRandomIntOnly);
        6: TriangularMatrix('upper', edMatrices[2], rdSizeB, SpecialFill, N, M, bRandomIntOnly);
        7: SymmetricMatrix('sym', edMatrices[2], rdSizeB, SpecialFill, N, M, bRandomIntOnly);
        8: SymmetricMatrix('skewsym', edMatrices[2], rdSizeB, SpecialFill, N, M, bRandomIntOnly);
      end;
    end;
  end;
  if Mess <> '' then begin
    // If there has been an error, display message
    if LeftStr(Mess, 1)[1] in ['i', 'u', 'e', 'o', 'a'] then
      Mess := 'An ' + Mess
    else
      Mess := 'A ' + Mess;
    MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Matrix size (Rows or Columns edit fields) changed: Adapt matrix display }

procedure TfMatrix.edSizeARowsEditingDone(Sender: TObject);

var
  R, C: Integer;

begin
  if edSizeARows.Text = '' then
    R := -1
  else
    R := StrToInt(edSizeARows.Text);
  if R in [1..4] then begin
    C := StrToInt(edSizeACols.Text);
    AdaptMatrix(edMatrices[1], shBrackets, 1, R, C);
    rdSizeA.Rows := R;
  end
  else begin
    MessageDlg('Data error', 'Matrix A: Invalid number of rows!', mtError, [mbOK], 0);
    edSizeARows.Text := IntToStr(rdSizeA.Rows);
  end;
end;

procedure TfMatrix.edSizeAColsEditingDone(Sender: TObject);

var
  R, C: Integer;

begin
  if edSizeACols.Text = '' then
    C := -1
  else
    C := StrToInt(edSizeACols.Text);
  if C in [1..4] then begin
    R := StrToInt(edSizeARows.Text);
    AdaptMatrix(edMatrices[1], shBrackets, 1, R, C);
    rdSizeA.Cols := C;
  end
  else begin
    MessageDlg('Data error', 'Matrix A: Invalid number of colums!', mtError, [mbOK], 0);
    edSizeACols.Text := IntToStr(rdSizeA.Cols);
  end;
end;

procedure TfMatrix.edSizeBRowsEditingDone(Sender: TObject);

var
  R, C: Integer;

begin
  if edSizeBRows.Text = '' then
    R := -1
  else
    R := StrToInt(edSizeBRows.Text);
  if R in [1..4] then begin
    C := StrToInt(edSizeBCols.Text);
    AdaptMatrix(edMatrices[2], shBrackets, 2, R, C);
    rdSizeB.Rows := R;
  end
  else begin
    MessageDlg('Data error', 'Matrix B: Invalid number of rows!', mtError, [mbOK], 0);
    edSizeBRows.Text := IntToStr(rdSizeB.Rows);
  end;
end;

procedure TfMatrix.edSizeBColsEditingDone(Sender: TObject);

var
  R, C: Integer;

begin
  if edSizeBCols.Text = '' then
    C := -1
  else
    C := StrToInt(edSizeBCols.Text);
  if C in [1..4] then begin
    R := StrToInt(edSizeBRows.Text);
    AdaptMatrix(edMatrices[2], shBrackets, 2, R, C);
    rdSizeB.Cols := C;
  end
  else begin
    MessageDlg('Data error', 'Matrix B: Invalid number of colums!', mtError, [mbOK], 0);
    edSizeBCols.Text := IntToStr(rdSizeB.Cols);
  end;
end;

end.

