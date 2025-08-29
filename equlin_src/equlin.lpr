{********************************************}
{* Linear equations in 1, 2 and 3 variables *}
{* Version 1.0, © allu, April, 2018         *}
{* Version 1.1, © allu, July, 2019          *}
{* Version 1.2, © allu, March, 2024         *}
{********************************************}

program equlin;

// Version 1.1 modifications:
//   - Display of the determinant values for 2- and 3-variables equations
// Version 1.2 modifications:
//   - Bug fix: "A"-key (change equation type and number of decimals) was not recognized

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Crt;

const
  Key_ENTER = #13;
  Key_ESC   = #27;

var
  Choice, Fract, ValCode, I: Integer;
  M1, M2, A1, A2, A3, B1, B2, B3, C1, C2, C3, D1, D2, D3, Det, DetX, DetY, DetZ, X, Y, Z: Real;
  S: string;
  Key: Char;
  FirstRun: Boolean;

{ Ask for equation coefficient }

procedure GetCoeff(SCoeff: string; NotNull: Boolean; out Coeff: Real);

var
  ValCode: Integer;
  S: string;
  Ok: Boolean;

begin
  repeat
    Ok := True;
    Write('  ', SCoeff, ' ? '); Readln(S);
    Val(S, Coeff, ValCode);
    if ValCode <> 0 then                                                       // coefficient must be numeric
      Ok := False;
    if NotNull and (Coeff = 0) then                                            // if parameter NotNull is set True, coefficient must not be 0
      Ok := False;
  until Ok;
end;

{ Ask for 1 variable equation coefficient }

procedure GetCoeff1(out M1, A1, D1, M2, A2, D2: Real);

begin
  GetCoeff('M1', True, M1);
  GetCoeff('A1', True, A1);
  GetCoeff('D1', False, D1);
  GetCoeff('M2', False, M2);
  if M2 <> 0 then begin
    GetCoeff('A2', False, A2);
    GetCoeff('D2', False, D2);
  end;
end;

{ Ask for 2 variables equation system coefficients }

procedure GetCoeff2(out A1, B1, D1, A2, B2, D2: Real);

begin
  GetCoeff('A1', True, A1);
  GetCoeff('B1', True, B1);
  GetCoeff('D1', False, D1);
  GetCoeff('A2', True, A2);
  GetCoeff('B2', True, B2);
  GetCoeff('D2', False, D2);
end;

{ Ask for 3 variables equation system coefficients }

procedure GetCoeff3(out A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3: Real);

var
  Ok: Boolean;

begin
  repeat
    for I := 10 to 25 do begin
      GotoXY(1, I);
      ClrEol;
    end;
    GotoXY(1, 10);
    Ok := True;
    GetCoeff('A1', False, A1);
    GetCoeff('B1', False, B1);
    GetCoeff('C1', False, C1);
    if ((A1 = 0) and (B1 = 0)) or ((A1 = 0) and (C1 = 0)) or ((B1 = 0) and (C1 = 0)) then
      // At least 2 coefficients should be <> 0 (equation 1)
      Ok := False;
    if Ok then begin
      GetCoeff('D1', False, D1);
      GetCoeff('A2', False, A2);
      GetCoeff('B2', False, B2);
      GetCoeff('C2', False, C2);
      if ((A2 = 0) and (B2 = 0)) or ((A2 = 0) and (C2 = 0)) or ((B2 = 0) and (C2 = 0)) then
        // At least 2 coefficients should be <> 0 (equation 2)
        Ok := False;
      if Ok then begin
        GetCoeff('D2', False, D2);
        GetCoeff('A3', False, A3);
        GetCoeff('B3', False, B3);
        GetCoeff('C3', False, C3);
        if ((A3 = 0) and (B3 = 0)) or ((A3 = 0) and (C3 = 0)) or ((B3 = 0) and (C3 = 0)) then
          // At least 2 coefficients should be <> 0 (equation 3)
          Ok := False;
        if Ok then begin
          // At lest 1 coefficient for each variable has to be <> 0
          if (A1 = 0) and (A2 = 0) and (A3 = 0) then
            Ok := False;
          if (B1 = 0) and (B2 = 0) and (B3 = 0) then
            Ok := False;
          if (C1 = 0) and (C2 = 0) and (C3 = 0) then
            Ok := False;
          if Ok then
            GetCoeff('D3', False, D3);
        end;
      end;
    end;
  until Ok;
end;

{ Ask for number of fractional digits }

procedure GetFract(out Fract: Integer);

var
  ValCode: Integer;
  S: string;

begin
  repeat
    ValCode := 0;
    Write('Fractional digits (default = 2) ? '); Readln(S);
    if S = '' then                                                             // hitting ENTER corresponds to default = 2
      Fract := 2
    else
      Val(S, Fract, ValCode);
  until (ValCode = 0) and (Fract >= 0) and (Fract <= 7);
  Writeln;
end;

{ Determine coefficient length (= number of characters) ["pretty printing"] }

function CoeffLength(R: Real): Integer;

var
  L: Integer;

begin
  if (R = 0) or (R = 1) then
    L := 0                                                                     // a coefficient of 1 is not displayed
  else if R = -1 then
    L := 1                                                                     // a coefficient of -1 is displayed as '-'
  else
    L := Length(FloatToStr(R));                                                // other coefficients = number of digits (+ decimal point)
  CoeffLength := L;
end;

{ Determine maximal coefficient length (for given variable) ["pretty printing"] }

function MaxLength(L1, L2, L3: Integer): Integer;

var
  L: Integer;

begin
  L := L1;
  if (L2 > L1) and (L2 >= L3) then
    L := L2
  else if (L3 > L1) and (L3 >= L2) then
    L := L3;
  MaxLength := L;
end;

{ Real number formatted output ["pretty printing"] }

procedure FWrite(S: string; R: Real; F: Integer; LF: Boolean);

var
  P: Integer;
  SR: string;

begin
  Write(S);
  if R = 0 then                                                                // avoid 0 being displayed as "-0"
    Write('0')
  else if R = Int(R) then                                                      // display integers without fractional digits
    Write(R:0:0)
  else begin
    // If number of fractional digits is given as parameter, use it
    // otherwise display number of fractional digits as they actually are
    if F = -1 then begin
      SR := FloatToStr(R);
      P := Pos(',', SR);
      if P = 0 then
        P := Pos('.', SR);
      F := Length(SR) - P;
    end;
    Write(R:0:F);
  end;
  // Display or not a line feed / carriage return
  if LF then
    Writeln;
end;

{ Coefficient/variable formatted output ["pretty printing"] }

procedure PWrite(R: Real; S: string; F: Integer; LF, PAbs: Boolean);

begin
  if R <> 0 then begin
    // Display only if coefficient <> 0
    if PAbs then
      // If PAbs is set True, display absolute value
      R := Abs(R);
    if R = -1 then
      // If coefficient = -1, display '-'
      Write('-')
    else if R <> 1 then
      // If coefficient = 1, display nothing,
      // otherwise display coefficient as formatted real number
      FWrite('', R, F, False);
    Write(S);
  end;
  if LF then
    Writeln;
end;

{ Operator formatted output ["pretty printing"] }

procedure OWrite(R: Real);

// The number given as parameter is the number on the right side of the operator

begin
  if R > 0 then
    Write(' + ')
  else if R < 0 then
    Write(' - ');
end;

{ Equation formatted output ["pretty printing"] }

procedure EWrite(A, B, C, D: Real; LA1, LB1, LC1, LA2, LB2, LC2, LA3, LB3, LC3: Integer; Format: Boolean);

var
  PAbs: Boolean;
  F, L, LMax: Integer;

// Subroutine used to pretty print equations of 2 and 3 variables systems
// If Format is set to false, the equation will be printed as is (no formatting);
// this simple output is used to display equations where the coefficient of 1 variable  = 0

begin
  F := -1; PAbs := False;
  Write('  ');
  // Display of x coefficient/variable (if <> 0)
  if A <> 0 then begin
    if Format then begin
      // Display spaces for proper alignment
      LMax := MaxLength(LA1, LA2, LA3);
      L := CoeffLength(A);
      while L < LMax do begin
        Write(' ');
        Inc(L);
      end;
    end;
    // Display x coefficient, followed by 'x'
    PWrite(A, 'x', F, False, PAbs);
  end;
  // Display of y coefficient/variable (if <> 0)
  if B <> 0 then begin
    if A <> 0 then begin
      // If x coefficient <> 0, display operator
      // and set PAbs to True in order to display y coefficient absolute (instead of signed) value
      OWrite(B);
      PAbs := True;
    end;
    if Format then begin
      // Display spaces for proper alignment
      L := CoeffLength(Abs(B));
      LMax := MaxLength(LB1, LB2, LB3);
      while L < LMax do begin
        Write(' ');
        Inc(L);
      end;
    end;
    // Display y coefficient, followed by 'y'
    PWrite(B, 'y', F, False, PAbs);
  end;
  PAbs := True;
  // Display of z coefficient/variable (if <> 0)
  if C <> 0 then begin
    // Display operator
    OWrite(C);
    if Format then begin
      // Display spaces for proper alignment
      L := CoeffLength(Abs(C));
      LMax := MaxLength(LC1, LC2, LC3);
      while L < LMax do begin
        Write(' ');
        Inc(L);
      end;
    end;
    // Display z coefficient, followed by 'z'
    PWrite(C, 'z', F, False, PAbs);
  end;
  // Display equal sign and constant coefficient
  Write(' = ');
  FWrite('', D, F, True);
end;

{ Equation in 1 variable formatted output ["pretty printing"] }

procedure DisplayEq1(M1, A1, D1, M2, A2, D2: Real);

var
  Bracket: Boolean;

begin
  //
  // Display left part of the equation
  //
  Bracket := False;
  if M1 = -1 then
    // Display multiplier of -1 as '-'
    Write('-')
  else if M1 <> 1 then
    // Don't display multiplier of 1; for other values use formatted number display
    FWrite('', M1, -1, False);
  if (M1 <> 1) and ((D1 <> 0) or (A1 < 0)) then begin
    // Display bracket if appropriate
    Write('(');
    Bracket := True;
  end;
  // Display x coefficient, followed by 'x'
  PWrite(A1, 'x', -1, False, False);
  // Display operator and constant coefficient if constant <> 0
  if D1 <> 0 then begin
    OWrite(D1);
    FWrite('', Abs(D1), -1, False);
  end;
  // Display closing bracket (if there were an opening one )
  if (M1 <> 1) and ((D1 <> 0) or (A1 < 0)) then begin
    Write(')');
    Bracket := False;
  end;
  // Display equal sign
  Write(' = ');
  //
  // Display right part of the equation
  //
  if M2 = 0 then
    // If multiplier = 0, just display '0
    Writeln('0')
  else begin
    // Display of multiplier value
    if M2 = -1 then
      // Display multiplier of -1 as '-'
      Write('-')
    else if M2 <> 1 then
      // Don't display multiplier of 1; for other values use formatted number display
      FWrite('', M2, -1, False);
    if (M2 <> 1) and (A2 <> 0) and (D2 <> 0) then begin
      // Display bracket if appropriate
      Write('(');
      Bracket := True;
    end;
    // Display x coefficient, followed by 'x'
    PWrite(A2, 'x', -1, False, False);
    if (A2 <> 0) and (D2 <> 0) then
      // Display operator if x coefficient and constant <> 0
      OWrite(D2);
    if D2 <> 0 then
      // Display constant coefficient if constant <> 0
      FWrite('', Abs(D2), -1, False);
    if Bracket then begin
      // Display closing bracket if there was an opening one
      Write(')');
      Bracket := False;
    end;
    Writeln;
  end;
end;

{ Equation system in 2 variables formatted output ["pretty printing"] }

procedure DisplayEq2(A1, B1, D1, A2, B2, D2: Real);

var
  LA1, LB1, LA2, LB2: Integer;

begin
  // Determine coefficients length (number of characters)
  // Usage of absolute value for y coefficient, as sign is given by the operator, not by the coefficient
  LA1 := CoeffLength(A1); LB1 := CoeffLength(Abs(B1));
  LA2 := CoeffLength(A2); LB2 := CoeffLength(Abs(B2));
  // Display the 2 equations
  EWrite(A1, B1, 0, D1, LA1, LB1, 0, LA2, LB2, 0, 0, 0, 0, True);
  EWrite(A2, B2, 0, D2, LA1, LB1, 0, LA2, LB2, 0, 0, 0, 0, True);
end;

{ Equation system in 3 variables formatted output ["pretty printing"] }

procedure DisplayEq3(A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3: Real);

var
  LA1, LB1, LC1, LA2, LB2, LC2, LA3, LB3, LC3: Integer;
  Format: Boolean;

begin
  // Determine coefficients length (number of characters)
  // Usage of absolute value for y coefficient, as sign is given by the operator, not by the coefficient
  LA1 := CoeffLength(A1); LB1 := CoeffLength(Abs(B1)); LC1 := CoeffLength(Abs(C1));
  LA2 := CoeffLength(A2); LB2 := CoeffLength(Abs(B2)); LC2 := CoeffLength(Abs(C2));
  LA3 := CoeffLength(A3); LB3 := CoeffLength(Abs(B3)); LC3 := CoeffLength(Abs(C3));
  // Do not use formatted output for equations with 2 variables only
  if (A1 = 0) or (A2 = 0) or (A3 = 0) or (B1 = 0) or (B2 = 0) or (B3 = 0) or (C1 = 0) or (C2 = 0) or (C3 = 0) then
    Format := False;
  // Display the 3 equations
  EWrite(A1, B1, C1, D1, LA1, LB1, LC1, LA2, LB2, LC2, LA3, LB3, LC3, Format);
  EWrite(A2, B2, C2, D2, LA1, LB1, LC1, LA2, LB2, LC2, LA3, LB3, LC3, Format);
  EWrite(A3, B3, C3, D3, LA1, LB1, LC1, LA2, LB2, LC2, LA3, LB3, LC3, Format);
end;

{ Calculate 2x2 determinant }

function Determinant2(A, B, C, D: Real): Real;

begin
  Determinant2 := A * D - B * C;
end;

{ Calculate 3x3 determinant }

function Determinant3(A, B, C, D, E, F, G, H, I: Real): Real;

begin
  Determinant3 := A * Determinant2(E, F, H, I) - B * Determinant2(D, F, G, I) + C * Determinant2(D, E, G, H);
end;

{ Solve linear equation in 1 variable }

procedure EquLin1(M1, A1, D1, M2, A2, D2: Real; out X: Real);

// The subroutine returns MaxInt for 'infinity of solutions' and -MaxInt for 'no solution'

begin
  // Global x coefficient = 0
  if M1 * A1 - M2 * A2 = 0 then begin
    if M2 * D2 - M1 * D1 = 0 then
      // Constant = 0: infinity of solutions
      X := MaxInt
    else
      // Constant <> 0: no solution
      X := -MaxInt;
  end
  // Global x coefficient <> 0: unique solution
  else
    X := (M2 * D2 - M1 * D1) / (M1 * A1 - M2 * A2);
end;

{ Solve linear equation system in 2 variables (Cramer's method) }

procedure EquLin2(A1, B1, D1, A2, B2, D2: Real; out Det, DetX, DetY, X, Y: Real);

// The subroutine returns MaxInt for 'infinity of solutions' and -MaxInt for 'no solution'

begin
  // Calculate determinants
  Det  := Determinant2(A1, B1, A2, B2);
  DetX := Determinant2(D1, B1, D2, B2);
  DetY := Determinant2(A1, D1, A2, D2);
  // Equations determinant = 0
  if Det = 0 then begin
    if (DetX = 0) and (DetY = 0) then begin
      // x and y determinant = 0: infinity of solutions
      X := MaxInt; Y := MaxInt;
    end
    else begin
      // x or y determinant <> 0: no solution
      X := -MaxInt; Y := -MaxInt;
    end;
  end
  // Equations determinant <> 0: unique solution
  else begin
    X := DetX / Det; Y := DetY / Det;
  end;
end;

{ Solve linear equation system in 3 variables (Cramer's method) }

procedure EquLin3(A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3: Real; out Det, DetX, DetY, DetZ, X, Y, Z: Real);

// The subroutine returns MaxInt for 'infinity of solutions' and -MaxInt for 'no solution'

begin
  // Calculate determinants
  Det  := Determinant3(A1, B1, C1, A2, B2, C2, A3, B3, C3);
  DetX := Determinant3(D1, B1, C1, D2, B2, C2, D3, B3, C3);
  DetY := Determinant3(A1, D1, C1, A2, D2, C2, A3, D3, C3);
  DetZ := Determinant3(A1, B1, D1, A2, B2, D2, A3, B3, D3);
  // Equations determinant = 0
  if Det = 0 then begin
    if (DetX = 0) and (DetY = 0) and (DetZ = 0) then begin
      // x, y and z determinant = 0: infinity of solutions
      X := MaxInt; Y := MaxInt; Z := MaxInt;
    end
    else begin
      // x, y or z determinant <> 0: no solution
      X := -MaxInt; Y := -MaxInt; Z := MaxInt;
    end;
  end
  // Equations determinant <> 0: unique solution
  else begin
    X := DetX / Det; Y := DetY / Det; Z := DetZ / Det;
  end;
end;

{****************}
{* Main program *}
{****************}

begin
  FirstRun := True; Choice := 0;
  repeat
    Window(1, 1, 80, 50);
    ClrScr;
    TextColor(Yellow);
    Writeln('Linear equations in 1, 2 or 3 variables');
    Writeln('=======================================');
    TextColor(LightGray); Writeln;
    // Ask for equatation type (1, 2 or 3 variables)
    if Choice = 0 then begin
      Writeln('Choose equations type:'); Writeln;
      Writeln('  1 = 1 variable');
      Writeln('  2 = 2 variables system');
      Writeln('  3 = 3 variables system');
      Writeln;
      repeat
        Write('Your choice ? '); Readln(S);
        Val(S, Choice, ValCode);
      until (ValCode = 0) and (Choice in [1..3]);
      Writeln;
    end;
    // Ask for number of fractional digits (result)
    if FirstRun then begin
      GetFract(Fract);
      FirstRun := False;
    end;
    for I := 4 to 25 do begin
      GotoXY(1, I);
      ClrEol
    end;
    GotoXY(1, 4);
    // Display equation (general format)
    if Choice = 1 then
      Writeln('Solving equations of the form')
    else
        Writeln('Solving equation systems of the form');
    TextColor(Yellow);
    case Choice of
      1: Writeln('  M1(A1x + D1) = M2(A2x + D2)');
      2: begin Writeln('  A1x + B1y = D1'); Writeln('  A2x + B2y = D2'); end;
      3: begin Writeln('  A1x + B1y + C1z = D1'); Writeln('  A2x + B2y + C2z = D2'); Writeln('  A3x + B3y + C3z = D3'); end;
    end;
    // Ask for equation coefficints
    TextColor(LightGray); Writeln;
    Write('Enter the equation');
    if (Choice = 2) or (Choice = 3) then
      Write('s');
    Writeln(' coefficients:');
    case Choice of
      1: GetCoeff1(M1, A1, D1, M2, A2, D2);
      2: GetCoeff2(A1, B1, D1, A2, B2, D2);
      3: GetCoeff3(A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3);
    end;
    for I := 4 to 25 do begin
      GotoXY(1, I);
      ClrEol;
    end;
    GotoXY(1, 4);
    // Display equation (with actual coefficients)
    if Choice = 1 then
      Writeln('Solving the linear equation in 1 variable:')
    else
      Writeln('Solving the linear equation system in ', Choice, ' variables:');
    TextColor(Yellow);
    case Choice of
      1: DisplayEq1(M1, A1, D1, M2, A2, D2);
      2: DisplayEq2(A1, B1, D1, A2, B2, D2);
      3: DisplayEq3(A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3);
    end;
    TextColor(LightGray);
    // Calculate equation solution
    case Choice of
      1: EquLin1(M1, A1, D1, M2, A2, D2, X);
      2: EquLin2(A1, B1, D1, A2, B2, D2, Det, DetX, DetY, X, Y);
      3: EquLin3(A1, B1, C1, D1, A2, B2, C2, D2, A3, B3, C3, D3, Det, DetX, DetY, DetZ, X, Y, Z);
    end;
    Writeln;
    // Display equation solution
    if Choice = 1 then
      Write('The equation has ')
    else begin
      Writeln('Determinants:');
      FWrite('  |A|  = ', Det, Fract, True);
      FWrite('  |Ax| = ', DetX, Fract, True);
      FWrite('  |Ay| = ', DetY, Fract, True);
      if Choice = 3 then
        FWrite('  |Az| = ', DetZ, Fract, True);
      Writeln;
      Write('The equation system has ');
    end;
    if Abs(X) = MaxInt then begin
      // MaxInt = infinity of solutions; -MaxInt = no solution
       TextColor(LightGreen);
      if X = -MaxInt then
        Writeln('no solution.')
      else if X = MaxInt then
        Writeln('an infinity of solutions.');
    end
    else begin
      // Unique solution
      Write('a unique solution: ');
      TextColor(LightGreen);
      case Choice of
        1: FWrite('x = ', X, Fract, True);
        2: begin FWrite('x = ', X, Fract, False); FWrite(';  y = ', Y, Fract, True); end;
        3: begin FWrite('x = ', X, Fract, False); FWrite(';  y = ', Y, Fract, False); FWrite(';  z = ', Z, Fract, True); end;
      end;
    end;
    TextColor(LightGray); Writeln;
    // Wait for user keystroke (and continue depending on key pressed)
    Writeln('ENTER = next equation(s) of same type; T = next equation(s) of different type');
    Write('F = change number of fracional digits; A = T + F; ESC = terminate program ');
    repeat
      Key := ReadKey;
      if Key = #00 then
        Key := ReadKey;
    until Key in [Key_ENTER, Key_ESC, 'T', 't', 'F', 'f', 'A', 'a'];
    Writeln;
    if Key in ['F', 'A', 'f', 'a'] then begin
      // Change number of fractional digits
      Writeln;
      GetFract(Fract);
    end;
    if Key in ['T', 'A', 't', 'a'] then
      // Continue with asking for equation type
      Choice := 0;
  until Key = Key_ESC;                                                         // ENTER = next equation (same type); ESC = exit program
end.

