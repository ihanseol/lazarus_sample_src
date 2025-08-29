{***************************************}
{* Quadratic equations in one variable *}
{***************************************}

program eququad;

// Version history
//   Version 1.0 (January 2018): Original program
//   Version 1.1 (August 2024):
//     - proper number output (ex: 0 instead of -0, -i instead of -1i)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Crt, SysUtils;

type
  TComplex = record
    Re, Im: Real;
  end;

const
  Key_ESC = #27;

var
  Fract, ValCode: Integer;
  S: string;
  Key: Char;
  A, B, C, D, R1, R2: Real;
  Z1, Z2: TComplex;
  FirstRun, Cplx: Boolean;

{ Formatted output of 2 real numbers (with associated) text }

procedure FWrite(S1, S2: string; R1, R2: Real; F:Integer; CR: Boolean);

begin
  // Display 1st number (and associated text)
  Write(S1);
  if R1 = 0 then                                                               // to avoid output as -0
    Write(0)
  else if R1 = Int(R1) then                                                    // for integers: do not display fractional digits
    Write(R1:0:0)
  else                                                                         // for non-integers: display fractional digits as given by F argument
    Write(R1:0:F);
  // Display 2nd number (only if associated text not empty)
  if S2 <> '' then begin
    Write(S2);
    if R2 = 0 then
      Write(0)
    else if R2 = Int(R2) then
      Write(R2:0:0)
    else
      Write(R2:0:F);
  end;
  // Display a CR/LF (if CR argument tells so)
  if CR then
    Writeln;
end;

{ Formatted output of a complex number }

procedure CWrite(Z: TComplex; F: Integer; CR: Boolean);

begin
  // Formatted display of real part of complex number
  if (Z.Re = 0) and (Z.Im = 0) then
    Write(0)
  else begin
    if Z.Re <> 0 then begin
      FWrite('', '', Z.Re, 0, F, False);
      if Z.Im > 0 then
        Write(' + ')
      else if Z.Im < 0 then
        Write(' - ');
    end;
    // Formatted display of imaginary part of complex number
    if Z.Im <> 0 then begin
      if (Z.Re = 0) and (Z.Im < 0) then
        Write('-');
      if (Z.Im <> 1) and (Z.Im <> -1) then
        FWrite('', '', Abs(Z.Im), 0, F, False);
      Write('i');
    end;
  end;
  // Display CR/LF (if CR argument tells so)
  if CR then
    Writeln;
end;

{ Main program }

begin
  FirstRun := True; Cplx := False;
  // Do calculations until user tells to terminate
  repeat
    // Display general information
    ClrScr;
    TextColor(Yellow);
    Writeln('Quadratic equations in one variable');
    Writeln('===================================');
    Writeln;
    TextColor(LightGray); Write('Solving equations of the form ');
    TextColor(Yellow); Writeln('ax^2 + bx + c = 0'); Writeln;
    TextColor(LightGray); Write('Calculation done by calculating the discriminant ');
    TextColor(Yellow); Writeln('Delta = b^2 - 4ac');
    TextColor(LightGray);
    Writeln('  D > 0: two real solutions');
    Writeln('  D = 0: one real solution');
    Writeln('  D < 0: two complex solutions');
    Writeln;
    // At first run, ask for calculation and display options
    if FirstRun then begin
      // Option: Calculate complex roots or not
      repeat
        Write('Calculate complex roots [y/n]   ? '); Readln(S);
        if S = '' then                                                         // default = no complex calculation
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
        if S = '' then                                                         // default = 2 fractional digits
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
      Write('a ? '); Readln(S);
      Val(S, A, ValCode)
    until (ValCode = 0) and (A <> 0);                                          // a = 0 (1st degree equation) not considered here...
    repeat
      Write('b ? '); Readln(S);
      Val(S, B, ValCode)
    until (ValCode = 0);
    repeat
      Write('c ? '); Readln(S);
      Val(S, C, ValCode)
    until (ValCode = 0);
    // Calculate the equation solutions
    D := Sqr(B) - 4 * A * C;                                                   // discriminent
    Writeln; Writeln('Solution:'); Writeln('---------'); Writeln;
    FWrite('Delta = ', '', D, 0, Fract, False);
    // Delta > 0: 2 real roots
    Write(': ');
    if D > 0 then begin
      Writeln('Two real solutions:');
      R1 := (-B - Sqrt(D)) / (2 * A); R2 := (-B + Sqrt(D)) / (2 * A);
      FWrite('x1 = ', ' and x2 = ', R1, R2, Fract, True);
    end
    // Delta = 0: 1 real root
    else if D = 0 then begin
      Writeln('One real solution:');
      R1 := -B  / (2 * A);
      FWrite('x = ', '', R1, 0, Fract, True);
    end
    // Delta < 0: 2 imaginary roots
    else begin
      if Cplx then begin                                                       // if option tells so, do complex calculation...
        Z1.Re := -B / (2 * A); Z1.Im := Sqrt(-D) / (2 * A);
        Z2.Re := -B / (2 * A); Z2.Im := -(Sqrt(-D) / (2 * A));
        Writeln('Two complex solutions:');
        Write('x1 = '); CWrite(Z1, Fract, False);
        Write(' and x2 = '); CWrite(Z2, Fract, True);
      end
      else begin                                                               // ...else just say that there are no real roots
        Writeln('There is no real solution for this equation!');
      end;
    end;
    Writeln; Writeln;
    // Wait for user key (ESC key terminating the program)
    Write('ESC to terminate; any other key for next equation ');
    Key := ReadKey; if Key = #0 then Key := ReadKey;
    FirstRun := False;
  until Key = Key_ESC;
end.

