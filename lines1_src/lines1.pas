{*********************************}
{*  Geometry:  Lines in a plane  *}
{* ----------------------------- *}
{* Line passing through 2 points *}
{* Version 1.0,  © allu, 12/2018 *}
{*********************************}

program lines1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Crt;

type
  TVector = record
    X, Y: Real;
  end;

const
  Key_ESC = #27;

var
  XA, YA, XB, YB, M, P, A, B, C, Alpha: Real;
  Key: Char;
  V: TVector;
  POrd: Boolean;

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
  RFormat := SR;
end;

{ Format vector for display }

function VFormat(V: TVector): string;

begin
  // Format the vector components for 'nice' display
  VFormat := '(' + RFormat(V.X) + ';' + RFormat(V.Y) + ')';
end;

{ Determine a "vecteur directeur" for line (AB) }

function VecteurDirecteur(XA, YA, XB, YB: Real): TVector;

var
  V: TVector;

begin
  // "Vecteur directeur" chosen = AB
  V.X := XB - XA;
  V.Y := YB - YA;
  VecteurDirecteur := V;
end;

{ Display 'nice' form of "équation cartésienne" defined by A, B, C coefficients }

function EquationCartesienne(POrd: Boolean; A, B, C: Real): string;

var
  Eq: string;

// "Equation cartésienne" of a line: ax + by + c = 0

begin
  if POrd then begin
    // Line parallel to y-axis
    Eq := 'x'; C := -C;
    if C > 0 then
      Eq += ' + ' + RFormat(C)
    else
      Eq += ' - ' + RFormat(-C);
    Eq += ' = 0';
  end
  else begin
    // All other lines
    if A <> 0 then begin
      if A = 1 then
        Eq := 'x'
      else if A = -1 then
        Eq := '-x'
      else
        Eq := RFormat(A) + 'x';
    end;
    if B <> 0 then begin
      if A <> 0 then begin
        if B = 1 then
          Eq += ' + y'
        else if B = -1 then
          Eq += ' - y'
        else begin
          if B > 0 then
            Eq += ' + ' + RFormat(B) + 'y'
          else
            Eq += ' - ' + RFormat(-B) + 'y';
        end;
      end
      else begin
        if B = 1 then
          Eq := 'y'
        else if B = -1 then
          Eq := '-y'
        else
          Eq := RFormat(B) + 'y';
      end;
    end;
    if C <> 0 then begin
      if C > 0 then
        Eq += ' + ' + RFormat(C)
      else
        Eq += ' - ' + RFormat(-C);
    end;
    Eq += ' = 0';
  end;
  EquationCartesienne := Eq;
end;

{ Display 'nice' form of "équation cartésienne réduite" defined by M and P }

function EquationCartesienneReduite(POrd: Boolean; M, P, C: Real): string;

var
  Eq: string;

// "Equation cartésienne réduite" of a line: y = mx + p (except for lines parallel to y-axis: x = c)

begin
  if POrd then
    // Line parallel to y-axis
    Eq := 'x = ' + RFormat(C)
  else begin
    // All other lines
    Eq := 'y = ';
    if (M = 0) and (P = 0) then
      Eq += '0'
    else begin
      if M <> 0 then begin
        if M = 1 then
          Eq += 'x'
        else if M = -1 then
          Eq += '-x'
        else
          Eq += RFormat(M) + 'x';
      end;
      if P <> 0 then begin
        if M <> 0 then begin
          if P > 0 then
            Eq += ' + ' + RFormat(P)
          else
            Eq += ' - ' + RFormat(-P);
        end
        else
          Eq += RFormat(P);
      end;
    end;
  end;
  EquationCartesienneReduite := Eq;
end;

{ Display "équation vectorielle" of a line defined by a "vecteur directeur" and a point }

function EquationVectorielle(V: TVector; XA, YA: Real): string;

begin
  EquationVectorielle := '(x;y) = (' + RFormat(XA) + ';' + RFormat(YA) + ') + k' + VFormat(V);
end;

{ Display 'nice' form of "équations paramétriques" of a line defined by a "vecteur directeur" and a point }

function EquationParametrique(V: TVector; XA, YA: Real): string;

var
  Eq1, Eq2: string;

begin
  // First equation (for x)
  Eq1 := 'x = ';
  if (XA = 0) and (V.X = 0) then
    Eq1 += '0'
  else begin
    if XA <> 0 then begin
      Eq1 += RFormat(XA);
      if V.X <> 0 then begin
        if V.X > 0 then
          Eq1 += ' + '
        else
          Eq1 += ' - ';
      end;
    end;
    if V.X <> 0 then begin
      if XA <> 0 then
        Eq1 += RFormat(Abs(V.X))
      else
        Eq1 += RFormat(V.X);
      Eq1 += 'k';
    end;
  end;
  // Second equation (for y)
  Eq2 := 'y = ';
  if (YA = 0) and (V.Y = 0) then
    Eq2 += '0'
  else begin
    if YA <> 0 then begin
      Eq2 += RFormat(YA);
      if V.Y <> 0 then begin
        if V.Y > 0 then
          Eq2 += ' + '
        else
          Eq2 += ' - ';
      end;
    end;
    if V.Y <> 0 then begin
      if YA <> 0 then
        Eq2 += RFormat(Abs(V.Y))
      else
        Eq2 += RFormat(V.Y);
      Eq2 += 'k';
    end;
  end;
  EquationParametrique := Eq1 + ' et ' + Eq2;
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

{ Main program }

begin
  repeat
    repeat
      DisplayTitle('Droite passant par 2 points.');
      Write('Point A(xa, ya):  xa = ? '); Readln(XA);
      Write('                  ya = ? '); Readln(YA);
      Write('Point B(xb, yb):  xb = ? '); Readln(XB);
      Write('                  yb = ? '); Readln(YB);
    until (XA <> XB) or (YA <> YB);                                            // the 2 points have to be different
    Writeln;
    POrd := False;
    if XA = XB then begin
      // Line parallel to y-axis
      POrd := True;
      C := XA;
      Writeln('Droite parallele a l''axe des ordonnees');
    end
    else begin
      // Normal case
      M := (YB - YA) / (XB - XA);
      P := YA - M * XA;
      A := -(YB - YA);
      B := XB - XA;
      C := -(YA * (XB - XA) - XA * (YB - YA));
      if M = 0 then
        Writeln('Droite parallele a l''axe des abscisses');
    end;
    Writeln('Equation cartesienne:    ', EquationCartesienne(POrd, A, B, C));
    Writeln('Equation cart. reduite:  ', EquationCartesienneReduite(POrd, M, P, C));
    if XA <> XB then begin
      Writeln('Coefficient directeur:   ', RFormat(M));
      Writeln('Ordonnee a l''origine:    ', RFormat(P));
    end;
    Writeln;
    V := VecteurDirecteur(XA, YA, XB, YB);
    Writeln('Vecteur directeur:       ', VFormat(V));
    Writeln('Equation vectorielle:    ', EquationVectorielle(V, XA, YA));
    Writeln('Equation parametrique:   ', EquationParametrique(V, XA, YA));
    Writeln;
    if V.X <> 0 then begin
      Alpha := Arctan(V.Y / V.X);
      if (Alpha > 0) and (Alpha < Pi / 2) then begin
        // Display these values only for angles between 0 and 90 degrees
        Writeln('Pente:                   ', RFormat(100 * (Sin(Alpha) / Cos(Alpha))), ' %');
        Writeln('Declivite:               ', RFormat(100 * Sin(Alpha)), ' %');
        Writeln('Alpha de la pente:       ', RFormat((360 / (2 * Pi)) * Alpha), ' deg');
      end;
    end;
    Writeln; Writeln;
    Write('ESC to terminate, any other key to continue... ');
    Key := ReadKey;
    if Key = #0 then
      Key := ReadKey;
  until Key = Key_ESC;                                                         // terminate with ESC key or restart
end.

