{***************************************************}
{* Second degree complex equations in one variable *}
{***************************************************}

program equcplx2;

// Version history:
//   Version 1.0 (May 2022): Original program
//   Version 1.1 (April 2025):
//     - Improvement of the ComplexToString function, avoiding possible R+0i and R-0i display

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Crt, SysUtils, UComplex;

const
  Key_ESC = #27;

var
  Key: Char;
  A, B, C, Delta, Z1, Z2: Complex;

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
  Result := CInit(NumberRe, NumberIm);
end;

{ Read coefficient (complex number) from keyboard }

procedure ReadCoeff(SCoeff: string; NotNull: Boolean; out Coeff: Complex);

var
  Plus, Minus, DecSep, I: Integer;
  S: string;
  Ok: Boolean;

begin
  repeat
    Write('  ', SCoeff, ' ? '); Readln(S);
    S := StringReplace(S, ',', '.', [rfReplaceAll]);                           // transform decimal separator (if necessary)
    S := StringReplace(S, '+i', '+1i', []);
    S := StringReplace(S, '-i', '-1i', []);
    // Check if user input is valid complex number
    Ok := True; Plus := 0; Minus := 0; DecSep := 0;
    for I := 1 to Length(S) do begin
      if S[I] in ['0'..'9', '.', '+', '-'] then begin
        case S[I] of
          // Count plus signs, minus signs and decimal points
          '+': Inc(Plus);
          '-': Inc(Minus);
          '.': Inc(DecSep);
        end;
      end
      else begin
        Ok := False;
        if (I = Length(S)) and (S[I] = 'i') then                               // real part must be given first (ex: i-3 will not be accepted)
          Ok := True;
      end;
    end;
    if Ok then begin
      // Check number of plus signs, minus signs and decimal points
      if Plus > 1 then begin
        Ok := False;
        if (Plus = 2) and (S[1] = '+') then
          Ok := True;
      end;
      if Minus > 1 then begin
        Ok := False;
        if (Minus = 2) and (S[1] = '-') then
          Ok := True;
      end;
      if DecSep > 1 then
        Ok := False;
    end;
    if Ok then begin
      // Transform user input string to Complex type
      Coeff := StrToComplex(S);
      if NotNull and CSameValue(Coeff, 0) then begin
        // Argument NotNull = True tells that number can't be 0
        Ok := False;
      end;
    end;
  until Ok;
end;

{ User input of equation coefficients }

procedure ReadCoefficients(out A, B, C: Complex);

begin
  Writeln('Please, enter the equation coefficients:');
  ReadCoeff('a', True, A);
  ReadCoeff('b', False, B);
  ReadCoeff('c', False, C);
end;

{ Second degree equation in 1 variable solution }

procedure SolveEquation(A, B, C: Complex; out Delta, Z1, Z2: Complex);

// The solution of the equation is calculated exactly the same way as with real numbers
// Just to note, that there is always a solution (2 roots or 1 double root)
// Operator overloading in the UComplex unit allows to calculate as with real numbers

begin
  Delta := CSqr(B) - 4 * A * C;                                                // discriminant
  Z1 := (-B - CSqrt(Delta)) / (2 * A);
  Z2 := (-B + CSqrt(Delta)) / (2 * A);
end;

{**************}
{ Main program }
{**************}

begin
  // Do calculations until user hits ESC key
  repeat
    // Display general information
    ClrScr;
    TextColor(Yellow);
    Writeln('Second degree complex equations in one variable');
    Writeln('===============================================');
    Writeln;
    TextColor(LightGray); Write('Solving equations of the form ');
    TextColor(Yellow); Write('az^2 + bz + c = 0');
    TextColor(LightGray); Writeln(',');
    Writeln('where z is a complex variable and a, b, c are complex coefficients.');
    Writeln;
    // User input of the equation coefficients
    ReadCoefficients(A, B, C);
    // Calculation of equation solution
    SolveEquation(A, B, C, Delta, Z1, Z2);
    // Display discriminant
    Writeln; Write('Delta = ', ComplexToStr(Delta));
    // Display equation solution
    Writeln; Write('The equation has ');
    if CSameValue(Z1, Z2) then
      Write('one (double) solution: ')
    else
      Write('two solutions: ');
    TextColor(LightGreen);
    if CSameValue(Z1, Z2) then
      Writeln('z = ', ComplexToStr(Z1))
    else
      Writeln('z1 = ', ComplexToStr(Z1), ', z2 = ', ComplexToStr(Z2));
    TextColor(LightGray); Writeln;
    // Wait for user keystroke (program termination with ESC)
    Write('ENTER ESC to terminate the program, any other key for new equation ');
    Key := ReadKey;
    if Key = #00 then
      Key := ReadKey;
  until Key = Key_ESC;
end.

