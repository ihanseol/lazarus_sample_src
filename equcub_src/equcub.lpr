{***********************************}
{* Cubic equations in one variable *}
{***********************************}

program equcub;

// Version 1.0 (March 2019)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Crt, SysUtils, Math, UComplex;

const
  Key_ESC = #27;

var
  Fract, ValCode: Integer;
  S: string;
  Key: Char;
  A, B, C, D, P, Q, Delta, DeltaR, Z, Y, R, Theta, X1, X2, X3: Real;
  CZ, CY, CX2, CX3: Complex;
  FirstRun, Cplx: Boolean;

{ Math function: Cube of X }

function Cub(X: Real): Real;

begin
  Cub := X * X * X;
end;

{ Math function: Cubic root of X }

function Cubrt(X: Real): Real;

// The Math unit power function (used with 1/3 as exponent) aborts with
// base = 0 and base < 0. Thus can't directly call it in these cases!

var
  Root: Real;

begin
  if X = 0 then
    Root := 0
  else if X > 0 then
    Root := power(X, 1 / 3)
  else
    Root := -power(-X, 1 / 3);
  Cubrt := Root;
end;

{ Formatted output of real number (+ asociated text) }

procedure FWrite(S: string; R: Real; F:Integer; CR: Boolean);

begin
  // Display text (if any)
  if S <> '' then
    Write(S, ' ');
  // Display number with or without decimals
  if Abs(R) < 1E-5 then                                                             // to avoid writing things like -0,00
    Write(0)
  else begin
    R := Round(intpower(10, F + 2) * R) / intpower(10, F + 2);                      // supposing this is precision enough to say it's an integer
    if R = Int(R) then                                                              // for integers: do not display fractional digits
      Write(R:0:0)
    else                                                                            // for non-integers: display fractional digits as given by F argument
      Write(R:0:F);
  end;
  // Display a CR/LF (if CR argument tells so)
  if CR then
    Writeln;
end;

{ Formatted output of a complex number (+ associated text) }

procedure CFWrite(S: string; C: Complex; F: Integer; CR: Boolean);

var
  Im: Real;

begin
  C.re := Round(intpower(10, F + 2) * C.re) / intpower(10, F + 2);                  // supposing this is precision enough
  C.im := Round(intpower(10, F + 2) * C.im) / intpower(10, F + 2);
  Im := C.im;
  // Display the complex number (with formatted real and imaginary part)
  if C.re <> 0 then begin
    // If there is a real part display it, followed by the sign of the imaginary part
    FWrite(S, C.re, F, False);                                                      // formatted display of real part of complex number
    if C.im > 0 then                                                                // sign of imaginary part
      Write(' +')
    else
      Write(' -');
    Im := Abs(Im);                                                                  // as the sign already is displayed, the imaginary part will be displayed as positive number
    S := '';                                                                        // the text already was displayed with the real part of the complex
  end;
  // "Pretty" display of imaginary part is 1 or -1
  if C.im = 1 then
    Write(S, ' i')                                                                  // display "i" instead of 1i
  else if C.im = -1 then begin
    if Im = 1 then
      Write(S, ' i')                                                                // display "r - i" instead of "r - 1i"
    else
      Write(S, ' -i');                                                              // display "-i" instead of "-1i"
  end
  // General case (imagnary part <> 1 and -1; supposing it is not 0)
  else begin
    FWrite(S, Im, F, False);                                                        // formatted display of imaginary part of complex number
    Write('i');                                                                     // imaginary "i"
  end;
  // Display CR/LF (if CR argument tells so)
  if CR then
    Writeln;
end;

{ ------------ }
{ Main program }
{ ------------ }

begin
  FirstRun := True; Cplx := False;
  // Do calculations until user tells to terminate
  repeat
    // Display general information
    ClrScr; TextColor(Yellow);
    Writeln('Cubic equations in one variable');
    Writeln('===============================');
    Writeln;
    TextColor(LightGray); Write('Solving equations of the form ');
    TextColor(Yellow); Writeln('ax^3 + bx^2 + cx + d = 0'); Writeln;
    TextColor(LightGray); Writeln('Calculation done by using the discriminant approach (Cardano''s formula)');
    Writeln;
    // At first run, ask for calculation and display options
    if FirstRun then begin
      // Option: Calculate complex roots or not
      repeat
        Write('Calculate complex roots [y/n]   ? '); Readln(S);
        if S = '' then                                                              // default = no complex calculation
          S := 'n'
        else
          S := LowerCase(S);
      until (S = 'y') or (S = 'n');
      if S = 'y' then
        Cplx := True;
      // Option: Number of fractional digits for display
      repeat
        ValCode := 0;
        Write('Fractional digits (default = 2) ? '); Readln(S);
        if S = '' then                                                              // default = 2 fractional digits
          Fract := 2
        else begin
          Val(S, Fract, ValCode);
        end;
      until (ValCode = 0) and (Fract >= 0) and (Fract <= 7);
      Writeln;
    end;
    // User entry of the equation coefficients
    Writeln('Enter the equation coefficients:');
    repeat
      Write('  a ? '); Readln(S);
      Val(S, A, ValCode)
    until (ValCode = 0) and (A <> 0);                                               // a = 0 (2nd degree equation) not considered here...
    repeat
      Write('  b ? '); Readln(S);
      Val(S, B, ValCode)
    until (ValCode = 0);
    repeat
      Write('  c ? '); Readln(S);
      Val(S, C, ValCode)
    until (ValCode = 0);
    repeat
      Write('  d ? '); Readln(S);
      Val(S, D, ValCode)
    until (ValCode = 0);
    // ------------------------------------------
    // Calculate the equation solutions (Cardano)
    // ------------------------------------------
    P := -Sqr(B) / (3 * Sqr(A)) + C / A;
    Q := 2 * Cub(B) / (27 * Cub(A)) - (B * C / (3 * Sqr(A))) + D / A;
    Delta := Sqr(Q) / 4 + Cub(P) / 27;                                              // 3rd degree equation discriminent
    DeltaR := Round(intpower(10, Fract + 2) * Delta) / intpower(10, Fract + 2);     // supposing this is precision enough for determining Delta = 0
    Writeln; Writeln('Solution:'); Writeln('---------'); Writeln;
    TextColor(LightGreen); FWrite('Delta =', DeltaR, Fract + 2, False);
    Write(': ');
    // Delta >= 0
    // ----------
    if DeltaR >= 0 then begin
      Z := Cubrt(-Q / 2 + Sqrt(Delta));
      if (DeltaR = 0) or (Z = 0) then
        Writeln('Three real roots (at least 2 being equal)')
      else begin
        if Cplx then
          Writeln('One real and two complex conjugate roots')
        else
          Writeln('One real root');
      end;
      Writeln;
      TextColor(LightGray); Writeln('Roots:');
      // Special case when Z = 0 (3 equal real roots)
      if Z = 0 then begin
        Y := -Cubrt(Q);
        X1 := Y - B / (3 * A);
        X2 := X1; X3 := X1;
      end
      else begin
        // General case (Delta >= 0):
        //   1. Calculate the real root
        //   2. Determine the 2 other roots by complex numbers calculations
        //        - if Delta > 0: this gives the 2 complex conjugates
        //        - if Delta = 0: these will be 2 real numbers
        Y := Z - P / (3 * Z);
        X1 := Y - B / (3 * A);
        CZ := Z * cinit(-1 / 2, Sqrt(3) / 2); CY := CZ - P / ( 3 * CZ);
        CX2 := CY - B / (3 * A); X2 := CX2.Re;
        CZ := Z * cinit(-1 / 2, -Sqrt(3) / 2); CY := CZ - P / ( 3 * CZ);
        CX3 := CY - B / (3 * A); X3 := CX3.Re;
      end;
      if (DeltaR = 0) or (Z = 0) then begin
        // Delta = 0 (and special case where Z becomes 0): 3 real roots (2 at least equal)
        FWrite('  X1 =', X1, Fract, True);
        FWrite('  X2 =', X2, Fract, True);
        FWrite('  X3 =', X3, Fract, True);
      end
      else begin
        // Delta > 0 (except special case where Z becomes 0): 1 real and 2 complex conjugate roots
        FWrite('  X1 =', X1, Fract, True);
        if Cplx then begin
          CFWrite('  X2 =', CX2, Fract, True);
          CFWrite('  X3 =', CX3, Fract, True);
        end;
      end;
    end
    // Delta < 0
    // ---------
    else begin
      Writeln('Three distinct real roots'); Writeln;
      TextColor(LightGray); Writeln('Roots:');
      // The formulas used here are obtained by doing several complex calculations/transformations
      R := Sqrt(Abs(P) / 3);
      Theta := ArcCos(3 * Sqrt(3) * Q / (2 * P * Sqrt(-P)));
      Y := (R - P / (3 * R)) * Cos(Theta / 3); X1 := Y - B / (3 * A);
      Y := (R - P / (3 * R)) * Cos(Theta / 3 + DegToRad(120)); X2 := Y - B / (3 * A);
      Y := (R - P / (3 * R)) * Cos(Theta / 3 + DegToRad(240)); X3 := Y - B / (3 * A);
      // Delta < 0: 3 distinct real roots
      FWrite('  X1 =', X1, Fract, True);
      FWrite('  X2 =', X2, Fract, True);
      FWrite('  X3 =', X3, Fract, True);
    end;
    Writeln; Writeln;
    // Wait for user key (ESC key terminating the program)
    Write('ESC to terminate; any other key for next equation ');
    Key := ReadKey; if Key = #0 then Key := ReadKey;
    FirstRun := False;
  until Key = Key_ESC;
end.

