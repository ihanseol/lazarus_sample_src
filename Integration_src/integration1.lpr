program integration1;

uses
  SysUtils, Integration;

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

function RFormat(R: Real; F: Integer): string;

var
  R0: Real;
  SR: string;

begin
  SR := '';
  if R = 0 then
    SR := '0'
  else begin
    if F >= 0 then begin
      R0 := Round(R * Power(10, F)) / Power(10, F);
      if Abs(R0) < Power(10, -F) then
        SR := FloatToStrF(R, ffExponent, F, 0)
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

function Cube(X: Real): Real;

begin
  Result := X * X * X;
end;

begin
  Writeln ('Integral of x^3 for x in [1.2 ; 4.5] = ', RFormat(Integrate(@Cube, 1.2, 4.5), 3));
  Writeln;
  Write('Enter to terminate... '); Readln;
end.

