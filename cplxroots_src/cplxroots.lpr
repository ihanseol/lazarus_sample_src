{******************************************************}
{* Determination of all nth roots of a complex number *}
{******************************************************}

program cplxroots;

uses SysUtils, math, Crt;

type
  TComplex = record
    Form: string;
    C1, C2: Real;
  end;
  TComplexRoots = array of TComplex;

const
  Version = '1.0';
  WDate   = 'May - November 2021';
  Forms: array[0..3] of string = (
    'standard', 'Cartesian', 'polar', 'exponential'
  );
  Key_ESC = #27;

var
  DecDigits, N, Q1, Q2, I: Integer;
  Form, Form2, S, S1, S2: string;
  Key: Char;
  R, T: Real;
  Start, AskForm, AskDecDigits: Boolean;
  Z: TComplex;
  Roots: TComplexRoots;

{ Get system decimal separator }

function DecimalSeparator: Char;

var
  S: string;

begin
  S := FloatToStrF(1.2, ffFixed, 0, 1);
  Result := Copy(S, 2, 1)[1];
end;

{ Test if a string contains a positive integer }

function IsPositiveInteger(S: string): Boolean;

var
  I: Integer;
  ItIs: Boolean;

begin
  if S = '' then
    ItIs := False
  else begin
    ItIs := True;
    for I := 1 to Length(S) do begin
      if not (S[I] in ['0'..'9']) then
        ItIs := False;
    end;
  end;
  Result := ItIs;
end;

{ Test if a string is numeric (is a real number) }

function IsRealNumber(S: string): Boolean;

var
  I, DCCount: Integer;
  ItIs: Boolean;

begin
  if S = '' then
    ItIs := False
  else begin
    ItIs := True; DCCount := 0;
    for I := 1 to Length(S) do begin
      if not (S[I] in ['0'..'9', '+', '-', DecimalSeparator]) then
        ItIs := False
      else begin
        if (S[I] in ['+', '-']) and (I > 1) then
          ItIs := False
        else if S[I] = DecimalSeparator then
          Inc(DCCount);
      end;
    end;
    if DCCount > 1 then
      ItIs := False;
  end;
  Result := ItIs;
end;

{ Calculate all nth roots of a complex number }

function CalcComplexRoots(Z: TComplex; N: Integer; Form: string): TComplexRoots;

var
  I: Integer;
  R0, T0: Real;
  Roots: TComplexRoots;

begin
  SetLength(Roots, 0);
  if (Z.Form = 'exponential') and (N >= 2) then begin
    // The complex has to be in exponential form to calculate the roots
    SetLength(Roots, N);                                                       // there are n nth roots
    R0 := Power(Z.C1, 1/N);                                                    // modulus of the root
    T0 := Z.C2 / N;                                                            // base angle of the root
  end;
  for I := 1 to N do begin
    R := R0;                                                                   // mudulus for this root
    T := T0 + (I - 1) * (2 / N) * Pi;                                          // angle for this root
    if (Form = 'standard') or (Form = 'Cartesian') then begin
      // Cartesian form of the root
      Roots[I - 1].C1 := R * Cos(T);
      Roots[I - 1].C2 := R * Sin(T);
    end
    else begin
      // Exponential (or polar) form of the root
      Roots[I - 1].C1 := R;
      Roots[I - 1].C2 := T;
    end;
    Roots[I - 1].Form := Form;
  end;
  Result := Roots;
end;

{ Display program title }

procedure DisplayTitle(Version, WDate: string; Details: Boolean);

begin
  ClrScr;
  TextColor(Yellow);
  Writeln('Determination of all nth roots of a complex number.');
  Writeln('===================================================');
  TextColor(LightGray);
  if Details then begin
    // Display details only if boolean is True (will be at program start)
    Writeln;
    Writeln('Version ', Version, ', (c) allu, ', WDate, '.'); Writeln;
    Writeln('Complex numbers may be entered in Cartesian, polar or exponential form:');
    Writeln('  Cartesian:   z = a + bi');
    Writeln('  Polar:       z = r * [cos(T) + i*sin(T)]');
    Writeln('  Exponential: z = r * e^[iT]');
    Writeln('All numbers are converted to exponential format during calculations.');
  end;
  Writeln;
end;

{ Display complex number (using actual form) }

procedure DisplayComplex(S: string; Z: TComplex; F: Integer; LF: Boolean);

var
  C1, C2: Real;
  SC: string;

begin
  SC := '';
  C1 := StrToFloat(FloatToStrF(Z.C1, ffFixed, 0, F));
  C2 := StrToFloat(FloatToStrF(Z.C2, ffFixed, 0, F));
  if Z.Form = 'standard' then begin
    // Standard display = Cartesian form formatted display: ax + b
    if (C1 = 0) and (C2 = 0) then
      SC := '0'
    else begin
      if C1 <> 0 then
        SC := FloatToStr(C1);
      if C2 <> 0 then begin
        if C2 > 0 then begin
          if C1 <> 0 then
            SC += ' + ';
          if C2 <> 1 then
            SC += FloatToStr(C2);
          SC += 'i';
        end
        else begin
          if C1 = 0 then
            SC += '-'
          else
            SC += ' - ';
          if C2 <> -1 then
            SC += FloatToStr(-C2);
          SC += 'i';
        end;
      end;
    end;
  end
  else if Z.Form = 'Cartesian' then begin
    // Cartesian display = Cartesian form "as is" display: ax + b
    SC := FloatToStr(C1);
    if C2 >= 0 then
      SC += ' + '
    else
      SC += ' - ';
    SC += FloatToStr(Abs(C2)) + 'i';
  end
  else if Z.Form = 'polar' then begin
    // Polar display = polar form "as is" display: r[cos(T) + sin(T)]
    SC := FloatToStr(C1) + '[';
    SC += 'cos(' + FloatToStrF(C2, ffFixed, 0, F) + ')';
    SC += ' + sin(' + FloatToStrF(C2, ffFixed, 0, F) + ')' + 'i';
    SC += ']';
  end
  else begin
    // Exponential display = exponential form "as is" display: re^Ti
    SC := FloatToStr(C1) + 'e^';
    SC += FloatToStr(C2) + 'i';
  end;
  if C1 >= 0 then
    SC := ' ' + SC;                                                            // nicer alignment of root values
  Write(S, SC);
  if LF then
    // End-of-line only if boolean is true
    Writeln;
end;

{**************}
{ Main program }
{**************}

begin
  // Start-up and default values
  Start := True; AskForm := True; AskDecDigits := True;
  Form := Forms[1]; DecDigits := 3;
  Key := ' ';
  // Continue doing calculations, until the ESC key is pressed
  repeat
    DisplayTitle(Version, WDate, Start); Start := False;
    if AskForm then begin
      // User input of complex number form
      repeat
        Writeln('Complex number form:');
        Write('  1 = Cartesian, 2 = polar, 3 = exponential ? '); Readln(S1);
        if S1 = '' then
          S1 := '1';                                                           // default form = Cartesian
      until (Length(S1) = 1) and (S1[1] in ['1', '2', '3']);
      Form := Forms[StrToInt(S1)];
      // User input of root form display
      repeat
        Writeln('Roots display:');
        Write('  0 = standard, 1 = Cartesian, 2 = polar, 3 = exponential ? '); Readln(S1);
        if S1 = '' then
          S1 := '0';                                                           // default display = standard
      until (Length(S1) = 1) and (S1[1] in ['0', '1', '2', '3']);
      Form2 := Forms[StrToInt(S1)];
      Writeln;
      AskForm := False;
    end;
    if AskDecDigits then begin
      // User input of roots' number of decimal digits
      Write('Complex roots number of decimal digits     ? '); Readln(S1);
      if (Length(S1) = 1) and (S1[1] in ['0'..'7']) then                       // let old value (default = 3) if invalid input
        DecDigits := StrToInt(S1);
      Writeln;
      AskDecDigits := False;
    end;
    // User input of which root to calculate
    repeat
      Write('Root (2 = square root, 3 = cubic root ...) ? '); Readln(S1);
    until IsPositiveInteger(S1) and (StrToInt(S1) >= 2);
    N := StrToInt(S1);
    Writeln;
    // User input of complex number (using the actual form)
    Writeln('Complex number (', Form, ' form):');
    for I := 1 to 3 do begin
      if Form = Forms[I] then begin
        repeat
          if I = 1 then begin
            Write('Real part (a)      ? '); Readln(S1);
            Write('Imaginary part (b) ? '); Readln(S2);
          end
          else begin
            Write('Modulus (r) ? '); Readln(S1);
            Write('Angle (T)   ? '); Readln(S2);
          end;
        until IsRealNumber(S1) and IsRealNumber(S2);
        Z.Form := Forms[I];
        Z.C1 := StrToFloat(S1);
        Z.C2 := StrToFloat(S2);
      end;
    end;
    // Calculate and display the roots
    DisplayTitle(Version, WDate, False);
    if Z.Form = 'Cartesian' then
      Z.Form := 'standard';                                                    // always use standard form for display of the complex number
    Write('Calculation of ', N, 'th roots of ');
    DisplayComplex('Z = ', Z, DecDigits, True); Writeln;                       // display the complex number
    if Z.Form = 'standard' then begin
      // If the complex has been entered using Cartesian form, transform it to exponential form
      // Modulus: R = sqrt(a^2 + b^2)
      // Angle: T = atan(b/a), but will have to consider the quadrants in order to get the correct angle!
      R := Sqrt(Sqr(Z.C1) + Sqr(Z.C2));
      if Z.C1 = 0 then begin
        if Z.C2 >= 0 then
          T := Pi / 2
        else
          T := -Pi / 2;
      end
      else if Z.C2 = 0 then begin
        if Z.C1 >= 0 then
          T := 0
        else
          T := Pi;
      end
      else begin
        T := ArcTan(Z.C2 / Z.C1);
        if (Z.C1 > 0) and (Z.C2 > 0) then
          Q1 := 1
        else if (Z.C1 < 0) and (Z.C2 > 0) then
          Q1 := 2
        else if (Z.C1 < 0) and (Z.C2 < 0) then
          Q1 := 3
        else
          Q1 := 4;
        if (T > 0) and (T < Pi / 2) or ((T < -3 * Pi / 2) and (T > -2 * Pi)) then
          Q2 := 1
        else if (T > Pi / 2) and (T < Pi) or ((T < -Pi) and (T > -3 * Pi / 2)) then
          Q2 := 2
        else if ((T > Pi) and (T < 3 * Pi / 2)) or ((T < -Pi / 2) and (T > -Pi)) then
          Q2 := 3
        else if ((T > 3 * Pi / 2) and (T < 2 * Pi)) or ((T < 0) and (T > -Pi / 2)) then
          Q2 := 4;
        if Q2 <> Q1 then begin
          T += (Q1 - Q2) * (Pi / 2);
        end;
      end;
      if T < 0 then
        T += 2 * Pi
      else if T > 2 * Pi then
        T -= 2 * Pi;
      Z.C1 := R; Z.C2 := T;
      Z.Form := 'exponential';
    end;
    Roots := CalcComplexRoots(Z, N, Form2);                                    // calculate the roots
    for I := 1 to N do begin
      // Display the roots
      S := 'Root ' + IntToStr(I) + ': ';
      if (N >= 10) and (I < 10) then
        S += ' ';
      DisplayComplex(S, Roots[I - 1], DecDigits, True);
    end;
    Writeln;
    Writeln('Hit S or R to restart with new settings, C or F to change the complex numbers format,');
    Write('ESC to terminate the program, any other key for new calculation with actual settings... ');
    Key := ReadKey;
    if Key = #0 then
      Key := ReadKey;
    if Key in ['S', 's', 'R', 'r'] then begin
      // If one of these keys has been pushed, reset the program settings
      AskForm := True; AskDecDigits := True;
    end
    else if Key in ['C', 'c', 'F', 'f'] then begin
      // If one of these keys has been pushed, reset the complex and roots form
      AskForm := True;
    end;
  until Key = Key_ESC;                                                         // terminate program with ESC, otherwise continue with new calculation
end.

