{******************************************************}
{* Matrix calculation routines for Matrix application *}
{******************************************************}

unit matrix_u2;

{$mode objfpc}{$H+}

interface

type
  TMatrix = array[0..3, 0..3] of Real;
  TMatrixSize = record
    Rows, Cols: Integer;
  end;

function MatrixDeterminant2(A: TMatrix; S: TMatrixSize): Real;
function MatrixAdjoint2(A: TMatrix; S: TMatrixSize): TMatrix;
function MatrixNegation(A: TMatrix; S: TMatrixSize): TMatrix;
function MatrixAddition(A, B: TMatrix; S: TMatrixSize): TMatrix;
function MatrixScalarMultiplication(A: TMatrix; S: TMatrixSize; K: Real): TMatrix;
function MatrixMultiplication(A, B: TMatrix; SA, SB: TMatrixSize): TMatrix;
function MatrixInversion2(A: TMatrix; S: TMatrixSize): TMatrix;
function MatrixTransposition(A: TMatrix; S: TMatrixSize): TMatrix;
function MatrixRowSwitching(A: TMatrix; S: TMatrixSize; I, J: Integer): TMatrix;
function MatrixRowMultiplication(A: TMatrix; S: TMatrixSize; I: Integer; K: Real): TMatrix;
function MatrixRowAddition(A: TMatrix; S: TMatrixSize; I, J: Integer; K: Real): TMatrix;

implementation

{ Swap two real values }

procedure RSwap(var R1, R2: Real);

var
  R: Real;

begin
  R := R1; R1 := R2; R2 := R;
end;

{ 2x2 matrix determinant }

function MatrixDeterminant2(A: TMatrix; S: TMatrixSize): Real;

var
  Det: Real;

begin
  Det := 0;
  if (S.Rows = 2) and (S.Cols = 2) then
    Det := A[0, 0] * A[1, 1] - A[0, 1] * A[1, 0];
  Result := Det;
end;

{ 2x2 matrix adjoint }

function MatrixAdjoint2(A: TMatrix; S: TMatrixSize): TMatrix;

var
  Res: TMatrix;

begin
  Res := A;
  if (S.Rows = 2) and (S.Cols = 2) then begin
    RSwap(Res[0, 0], Res[1, 1]);
    Res[0, 1] := -Res[0, 1];
    Res[1, 0] := -Res[1, 0];
  end;
  Result := Res;
end;

{ Matrix negation: A(i,j) -> -A(i,j) }

function MatrixNegation(A: TMatrix; S: TMatrixSize): TMatrix;

var
  R, C: Integer;
  Res: TMatrix;

begin
  for R := 0 to S.Rows - 1 do begin
    for C := 0 to S.Cols - 1 do begin
      Res[R, C] := -A[R, C];
    end;
  end;
  Result := Res;
end;

{ Matrix addition }

function MatrixAddition(A, B: TMatrix; S: TMatrixSize): TMatrix;

var
  R, C: Integer;
  Res: TMatrix;

begin
  for R := 0 to S.Rows - 1 do begin
    for C := 0 to S.Cols - 1 do begin
      Res[R, C] := A[R, C] + B[R, C];
    end;
  end;
  Result := Res;
end;

{ Scalar multiplication }

function MatrixScalarMultiplication(A: TMatrix; S: TMatrixSize; K: Real): TMatrix;

var
  R, C: Integer;
  Res: TMatrix;

begin
  for R := 0 to S.Rows - 1 do begin
    for C := 0 to S.Cols - 1 do begin
      Res[R, C] := K * A[R, C];
    end;
  end;
  Result := Res;
end;

{ Matrix multiplication }

function MatrixMultiplication(A, B: TMatrix; SA, SB: TMatrixSize): TMatrix;

var
  R, C, I: Integer;
  V: Real;
  Res: TMatrix;

begin
  for R := 0 to SA.Rows - 1 do begin
    for C := 0 to SB.Cols - 1 do begin
      V := 0;
      for I := 0 to SA.Cols - 1 do
        V += A[R, I] * B[I, C];
      Res[R, C] := V;
    end;
  end;
  Result := Res;
end;

{ 2x2 matrix inversion }

function MatrixInversion2(A: TMatrix; S: TMatrixSize): TMatrix;

var
  R, C: Integer;
  Det: Real;
  Adj, Res: TMatrix;

begin
  Det := MatrixDeterminant2(A, S);
  Adj := MatrixAdjoint2(A, S);
  for R := 0 to S.Rows - 1 do begin
    for C := 0 to S.Cols - 1 do begin
      Res[R, C] := (1 / Det) * Adj[R, C];
    end;
  end;
  Result := Res;
end;

{ Matrix transposition }

function MatrixTransposition(A: TMatrix; S: TMatrixSize): TMatrix;

var
  R, C: Integer;
  Res: TMatrix;

begin
  for R := 0 to S.Rows - 1 do begin
    for C := 0 to S.Cols - 1 do begin
      Res[C, R] := A[R, C];
    end;
  end;
  Result := Res;
end;

{ Matrix row switching }

function MatrixRowSwitching(A: TMatrix; S: TMatrixSize; I, J: Integer): TMatrix;

var
  C: Integer;
  Res: TMatrix;

begin
  Res := A;
  for C := 0 to S.Cols - 1 do
    RSwap(Res[I, C], Res[J, C]);
  Result := Res;
end;

{ Matrix row multiplication }

function MatrixRowMultiplication(A: TMatrix; S: TMatrixSize; I: Integer; K: Real): TMatrix;

var
  C: Integer;
  Res: TMatrix;

begin
  Res := A;
  for C := 0 to S.Cols - 1 do
    Res[I, C] := K * A[I, C];
  Result := Res;
end;

{ Matrix row addition }

function MatrixRowAddition(A: TMatrix; S: TMatrixSize; I, J: Integer; K: Real): TMatrix;

var
  C: Integer;
  Res: TMatrix;

begin
  Res := A;
  for C := 0 to S.Cols - 1 do
    Res[I, C] := A[I, C] + K * A[J, C];
  Result := Res;
end;

end.

