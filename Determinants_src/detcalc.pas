{********************************************************************}
{* Common determinant calculation unit for Determinants application *}
{********************************************************************}

unit detcalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Public types }

type
  // Determinants (2-dimensional array)
  TDetVals2 = array[1..2, 1..2] of Real;
  TDetVals3 = array[1..3, 1..3] of Real;
  TDetVals4 = array[1..4, 1..4] of Real;
  // Expansion minors (array of determinants)
  TDets2 = array[1..3] of TDetVals2;
  TDets3 = array[1..4] of TDetVals3;
  // Minors multiplicators (array of real)
  TMult2 = array[1..3] of Real;
  TMult3 = array[1..4] of Real;

{ Public functions }

function Determinant2(D2: TDetVals2): Real;
function Determinant4(D4: TDetVals4): Real;
function Determinant3(D3: TDetVals3): Real;
function Det3Multipliers(D3: TDetVals3): TMult2;
function Det3Determinants(D3: TDetVals3): TDets2;
function Det4Multipliers(D4: TDetVals4): TMult3;
function Det4Determinants(D4: TDetVals4): TDets3;
function FloatToString(R: Real): string;

implementation

{ Calculate 2x2 determinant }

function Determinant2(D2: TDetVals2): Real;

begin
  Result := D2[1, 1] * D2[2, 2] - D2[1, 2] * D2[2, 1];
end;

{ Calculate 3x3 determinant }

function Determinant3(D3: TDetVals3): Real;

var
  D2Mult: TMult2;
  D2Dets: TDets2;

begin
  // Determinant expansion: Get multipliers and 2x2 minors
  D2Mult := Det3Multipliers(D3);
  D2Dets := Det3Determinants(D3);
  // Compute the determinant using the expansion values
  Result := D2Mult[1] * Determinant2(D2Dets[1]) - D2Mult[2] * Determinant2(D2Dets[2]) + D2Mult[3] * Determinant2(D2Dets[3]);
end;

{ Calculate 4x4 determinant }

function Determinant4(D4: TDetVals4): Real;

var
  D3Mult: TMult3;
  D3Dets: TDets3;

begin
  // Determinant expansion: Get multipliers and 3x3 minors
  D3Mult := Det4Multipliers(D4);
  D3Dets := Det4Determinants(D4);
  // Compute the determinant using the expansion values
  Result := D3Mult[1] * Determinant3(D3Dets[1]) - D3Mult[2] * Determinant3(D3Dets[2]) + D3Mult[3] * Determinant3(D3Dets[3]) - D3Mult[4] * Determinant3(D3Dets[4]);
end;

{ Get 3x3 determinant multipliors }

function Det3Multipliers(D3: TDetVals3): TMult2;

var
  I: Integer;
  Mult2: TMult2;

// The application always uses the determinant's first line elements as multipliers

begin
  for I := 1 to 3 do
    Mult2[I] := D3[1, I];
  Result := Mult2;
end;

{ Get 3x3 determinant expansion minors }

function Det3Determinants(D3: TDetVals3): TDets2;

var
  N, I, J, X, Y: Integer;
  Dets2: TDets2;

// The minor multipliers are supposed to be 1st row elements

begin
  for N := 1 to 3 do begin
    X := 1; Y := 1;
    for I := 2 to 3 do begin                                                   // all first row elements ignored (as used as multipliers)
      for J := 1 to 3 do begin
        if J <> N then begin                                                   // column ignored if it is the one, that the multiplier belongs to
          Dets2[N][Y, X] := D3[I, J];
          Inc(X);
          if X = 3 then begin                                                  // next row
            X := 1; Inc(Y);
          end;
        end;
      end;
    end;
  end;
  Result := Dets2;
end;

{ Get 4x4 determinant multipliors }

function Det4Multipliers(D4: TDetVals4): TMult3;

var
  I: Integer;
  Mult3: TMult3;

// The application always uses the determinant's first line elements as multipliers

begin
  for I := 1 to 4 do
    Mult3[I] := D4[1, I];
  Result := Mult3;
end;

{ Get 4x4 determinant expansion minors }

function Det4Determinants(D4: TDetVals4): TDets3;

var
  N, I, J, X, Y: Integer;
  Dets3: TDets3;

// The minor multipliers are supposed to be 1st row elements

begin
  for N := 1 to 4 do begin
    X := 1; Y := 1;
    for I := 2 to 4 do begin                                                   // all first row elements ignored (as used as multipliers)
      for J := 1 to 4 do begin
        if J <> N then begin                                                   // column ignored if it is the one, that the multiplier belongs to
          Dets3[N][Y, X] := D4[I, J];
          Inc(X);
          if X = 4 then begin                                                  // next row
            X := 1; Inc(Y);
          end;
        end;
      end;
    end;
  end;
  Result := Dets3;
end;

{ Custom float to string conversion function }

function FloatToString(R: Real): string;

var
  F: Integer;

// Does the same as FloatToStrF(R, ffFixed, ...) but drops unsignificant zeros after the decimal seperator

begin
  if R = Int(R) then
    F := 0
  else if R * 10 = Int(R * 10) then
    F := 1
  else if R * 100 = Int(R * 100) then
    F := 2
  else                                                                         // 3 decimal digits by default
    F := 3;
  Result := FloatToStrF(R, ffFixed, 0, F);
end;

end.

