{**************************************}
{*     Geometry: Lines in a plane     *}
{* ---------------------------------- *}
{* Parallel lines; lines intersection *}
{* Version 1.0,  © allu, 12/2018      *}
{**************************************}

program lines2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Crt;

const
  Key_ESC = #27;

var
  Cas: Integer;
  A1, B1, C1, M1, P1, A2, B2, C2, M2, P2, XA, YA, Det, DetX, DetY: Real;
  D1, D2, Eq1, Eq2: string;
  POrd1, POrd2: Boolean;
  Key: Char;

{ Format real number for 'nice' display }

function RFormat(R: Real): string;

var
  SR: string;

begin
  // Display integers as integers, reals with 2 (or if to small with 4) decimal digits
  if R = Int(R) then
    SR := FloatToStrF(R, ffFixed, 0, 0)
  else if Abs(R) < 0.01 then
    SR := FloatToStrF(R, ffFixed, 0, 4)
  else
    SR := FloatToStrF(R, ffFixed, 0, 2);
  SR := StringReplace(SR, ',', '.', []);                                       // use '.' as decimal separator
  RFormat := SR;
end;

{ Get coefficients of line equation }

procedure LineCoefficients(D: string; out Eq: string; out POrd: Boolean; out A, B, C, M, P: Real);

var
  Code, Ps: Integer;

// For a "équation cartésienne", relevant coefficients will be A, B and C,
// for a "équation cartésienne réduite", relevant coefficients will be M and P.
// Special case: lines parallel to y-axis:
//  - POrd is set to true
//  - C (for "équation cartésienne") resp. P (for "équation cartésienne réduite") will contain the value of the x-constant

begin
  D := StringReplace(D, ' ', '', [rfReplaceAll]);                              // remove all spaces
  Eq := '?'; POrd := False; A := 0; B := 0; C := 0; M := 0; P := 0;
  // "Equation "cartésienne réduite": y=mx+p or x=k
  if (LeftStr(D, 2) = 'y=') or (LeftStr(D, 2) = 'x=') then begin
    Eq := 'cart. reduite';
    if LeftStr(D, 2) = 'x=' then begin
      // "Equation "cartésienne réduite": x=k
      POrd := True;
      M := 0; Val(RightStr(D, Length(D) - 2), P, Code);                        // the value of k will be returned in variable P
      if Code <> 0 then
        Eq := '?';
    end
    else begin
      // "Equation "cartésienne réduite": y=mx+p
      Delete(D, 1, 2);
      Ps := Pos('x', D);
      if Ps = 1 then
        D := StringReplace(D, 'x', '1x', [])
      else if (Ps = 2) and (D[1] = '-') then
        D := StringReplace(D, '-x', '-1x', []);
      Ps := Pos('x', D);
      if Ps = 0 then begin
        M := 0; Val(D, P, Code);
        if Code <> 0 then
          Eq := '?';
      end
      else begin
        Val(LeftStr(D, Ps - 1), M, Code);
        if Code <> 0 then
          Eq := '?'
        else begin
          if Ps = Length(D) then
            P := 0
          else begin
            Val(RightStr(D, Length(D) - Ps), P, Code);
            if Code <> 0 then
              Eq := '?'
          end;
        end;
      end;
    end;
  end
  else if RightStr(D, 2) = '=0' then begin
    // "Equation "cartésienne": ax+by+c=0 or x+k=0
    Eq := 'cartesienne';
    D := LeftStr(D, Length(D) - 2);
    Ps := Pos('x', D);
    if Ps = 1 then
      D := StringReplace(D, 'x', '1x', [])
    else if (Ps = 2) and (D[1] = '-') then
      D := StringReplace(D, '-x', '-1x', []);
    Ps := Pos('x', D);
    if Ps = 0 then begin
      A := 0;
    end
    else begin
      Val(LeftStr(D, Ps - 1), A, Code);
      if Code <> 0 then
        Eq := '?'
    end;
    if Eq <> '?' then begin
      if Ps = Length(D) then begin
        B := 0; C := 0;
      end
      else begin
        Delete(D, 1, Ps);
        Ps := Pos('y', D);
        if Ps = 1 then
          D := StringReplace(D, 'y', '1y', [])
        else if (Ps = 2) and (D[1] = '-') then
          D := StringReplace(D, '-y', '-1y', []);
        Ps := Pos('y', D);
        if Ps = 0 then begin
          B := 0;
        end
        else begin
          Val(LeftStr(D, Ps - 1), B, Code);
          if Code <> 0 then
            Eq := '?'
        end;
      end;
    end;
    if Eq <> '?' then begin
      if (A = 0) and (B = 0) then                                              // 'x' or 'y' must be present
        Eq := '?'
      else begin
        if Ps = Length(D) then
          C := 0
        else
          Val(RightStr(D, Length(D) - Ps), C, Code);
        if Code <> 0 then
          Eq := '?';
      end;
    end;
    if Eq <> '?' then begin
      if B = 0 then begin
        // If there is no y coefficient, the line is parallel to y-axis
        POrd := True;
        C /= A; A := 1;                                                        // eq. 2x-4=0 <=> x-2=0
      end;
    end;
  end;
end;

{ Get "équation cartésienne" from "équation cartésienne réduite" }

function EqCartesienne(POrd: Boolean; M, P: Real): string;

var
  Eq: string;

begin
  if POrd then begin
    // Line parallel to y-axis
    Eq := 'x';
    if P < 0 then
      Eq += '+'
    else
      Eq += '-';
    Eq += FloatToStr(Abs(P));
  end
  else begin
    // All other lines
    Eq := '';
    if M <> 0 then begin
      Eq := FloatToStr(M) + 'x';
    end;
    Eq += '-y';
    if P <> 0 then begin
      if P > 0 then
        Eq += '+'
      else
        Eq += '-';
      Eq += FloatToStr(Abs(P));
    end;
  end;
  Eq := StringReplace(Eq, ',', '.', [rfReplaceAll]);                           // use '.' as decimal separator
  EqCartesienne := Eq + '=0';
end;

{ Display program title }

procedure DisplayTitle(Title: string);

var
  I: Integer;

begin
  ClrScr;
  TextColor(Yellow);
  Writeln(Title);
  for I := 1 to Length(Title) do
    Write('=');
  Writeln;
  Writeln;
  TextColor(LightGray);
end;

{ Clear lines on screen }

procedure Clear(L: Integer);

var
  I: Integer;

begin
  for I := L to L + 10 do begin
    GotoXY(1, L); ClrEoL;
  end;
  GotoXY(1, L);
end;

{**************}
{ Main program }
{**************}

begin
  repeat
    // Display title and info
    DisplayTitle('Intersection de 2 droites.');
    Writeln('Vous pouvez entrer les equations des droites');
    Writeln('  - soit sous la forme cartesienne: ax+by+c=0');
    Writeln('  - soit sous la forme cart. reduite: y=mx+p');
    Writeln;
    Writeln('Utilisez le point (.) comme separateur decimal, svp!');
    Writeln;
    // Get first line equation
    repeat
      Clear(11);
      Write('Droite D1 ? '); Readln(D1);
      LineCoefficients(D1, Eq1, POrd1, A1, B1, C1, M1, P1);
    until Eq1 <> '?';
    // Get second line equation
    repeat
      Clear(12);
      Write('Droite D2 ? '); Readln(D2);
      LineCoefficients(D2, Eq2, POrd2, A2, B2, C2, M2, P2);
    until Eq2 <> '?';
    Writeln;
    // If equations have different forms transform one of them (to "cartesienne")
    if (Eq1 = 'cartesienne') and (Eq2 <> 'cartesienne') then begin
      Eq2 := 'cartesienne';
      D2 := EqCartesienne(POrd2, M2, P2);
      LineCoefficients(D2, Eq2, POrd2, A2, B2, C2, M2, P2);
    end
    else if (Eq2 = 'cartesienne') and (Eq1 <> 'cartesienne') then begin
      Eq1 := 'cartesienne';
      D1 := EqCartesienne(POrd1, M1, P1);
      LineCoefficients(D1, Eq1, POrd1, A1, B1, C1, M1, P1);
    end;
    // Determine if lines are parallel (distinct or equal) or if there is an intersection point
    if POrd1 and POrd2 then begin
      // Special case where both lines are parallel to y-axis
      if (Eq1 = 'cartesienne') and (C1 = C2) or ((Eq1 = 'cart. reduite') and (P1 = P2)) then
        Cas := 3
      else
        Cas := 2;
    end
    else if (Eq1 = 'cartesienne') then begin
      // Lines given by "équation cartésienne"
      if POrd1 then begin
        // First line parallel to y-axis
        XA := -C1; YA := (-A2 * XA - C2) / B2;
        Cas := 1;
      end
      else if POrd2 then begin
        // Second line parallel to y-axis
        Cas := 1;
        XA := -C2; YA := (-A1 * XA - C1) / B1;
      end
      else begin
        // General case (none of the lines parallel to y-axis)
        Det  := (A1 * B2) - (B1 * A2);
        DetX := (-C1 * B2) - (B1 * -C2);
        DetY := (A1 * -C2) - (-C1 * A2);
        if Det = 0 then begin
          // If the determinant is 0, the lines are parallel
          if (DetX = 0) and (DetY = 0) then
            Cas := 3                                                           // lines are equal
          else
            Cas := 2;                                                          // lines are distinct
        end
        else begin
          // If the determinant is not 0, there is an intersection point
          Cas := 1;
          XA := DetX / Det; YA := DetY / Det;
        end;
      end;
    end
    else begin
      // Lines given by "équation cartésienne réduite"
      if POrd1 then begin
        // First line parallel to y-axis
        Cas := 1;
        XA := P1; YA := M2 * XA + P2;
      end
      else if POrd2 then begin
        // Second line parallel to y-axis
        Cas := 1;
        XA := P2; YA := M1 * XA + P1;
      end
      else if M1 = M2 then begin
        // General case with lines having the same "coefficient directeur": they are parallel
        if P1 = P2 then
          Cas := 3                                                             // lines are equal
        else
          Cas := 2;                                                            // lines are distinct
      end
      else begin
        // General case with lines having a different "coefficient directeur": there is an intersection point
        Cas := 1;
        XA := (P2 - P1) / (M1 - M2); YA := M1 * XA + P1;
      end;
    end;
    // Display results
    case Cas of
      1: begin
           Writeln('D1 et D2 sont secantes.');
           Writeln('Point d''intersection = I(', RFormat(XA), ' ; ', RFormat(YA), ')');
         end;
      2: Writeln('D1 et D2 sont paralleles.');
      3: Writeln('D1 et D2 sont confondues.')
    end;
    Writeln; Writeln;
    // Wait for user to hit a key
    Write('ESC to terminate, any other key to continue... ');
    Key := ReadKey;
    if Key = #0 then
      Key := ReadKey;
  until Key = Key_ESC;                                                         // terminate with ESC key or restart
end.

