//
// Arithmetic3: "Die Kinder zählen die Autos" (common unit)
//
// This unit contains the question-generation-routines used by the
// GUI application Arithmetics3 and the command line program arith3
//

unit arithmetics3_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  NQuestionTypes = 99;

function RandomPlace: string;
function RandomCars(NumberType: Integer; CheckType: Char; CheckValue: Integer): Integer;
procedure RandomColors(NC: Integer; Ending: string; var C1, C2, C3: string);
procedure QuestionText(QType: Integer; var Txt: string; var N: Integer);
procedure QuestionText0(N, NC, NumberType: Integer; var N1, N2, N3: Integer; var C1, C2, C3, Txt:string);
procedure QuestionText1(Action1, Action2: Char; Color1, Color2: string; Check1, Check2, NumberType: Integer; var N1, N2: Integer; var C1, C2, Txt: string);
procedure QuestionText2(Action2: Char; Colour1, Colour2: string; NumberType: Integer; var F1, F2: Real; var N3: Integer; var Txt: string);
procedure QuestionText3(Action1, Action2: Char; Colour1, Colour2: string; NumberType: Integer; var N1, N2: Integer; var M1, M2: Real; var Txt: string);
procedure QuestionText4(QuestionSubType, NumberType: Integer; var N1, N2: Integer; var F1, F2: Real; var C1, C2, C3, Txt: string);
procedure QuestionText5(Colour1: string; QuestionSubType: Integer; var M2, M3: Real; var C2, C3, Txt: string);
function UpperFirst(S: string): string;

implementation

type
  TFractionsS = array[1..9] of string;
  TFractions = array[1..9] of Integer;
  TSMult = array[1..6] of string;
  TMult = array[1..6] of Integer;

const
  FractionsS: TFractionsS = (
    'die Haelfte', 'ein Drittel', 'zwei Drittel', 'ein Viertel', 'drei Viertel',
    'ein Fuenftel', 'zwei Fuenftel', 'drei Fuenftel', 'vier Fuenftel');
  FractionsN: TFractions = (1, 1, 2, 1, 3, 1, 2, 3, 4);
  FractionsD: TFractions = (2, 3, 3, 4, 4, 5, 5, 5, 5);
  SMult: TSMult = ('ebensoviele', 'halbsoviele', 'doppeltsoviele', 'halbsoviele', 'doppeltsoviele', 'halbsoviele');
  MultN: TMult = (1, 1, 2, 1, 2, 1);
  MultD: TMult = (1, 2, 1, 2, 1, 2);

{ Main question generation routine }

procedure QuestionText(QType: Integer; var Txt: string; var N: Integer);

// Procedure argument: question type (1 - 99)
// Procedure returns:  question text and result

const
  // The '#' sign is used to specify tags which will be replaced by the actual question values
  Questions: array[1..9] of string = (
    'Wieviele Autos sind es zusammen?',
    'Wieviele Autos sind es jetzt?',
    'Wieviele #color# Autos sind es jetzt?',
    'Wieviele Autos fahren weg?',
    'Wieviele #color# Autos fahren weg?',
    'Wieviele Autos kommen insgesamt hinzu?',
    'Wieviele Autos sind #color#?',
    'Wieviele Autos sind #color1# oder #color2#?',
    'Wieviele Autos fahren insgesamt weg?');

var
  N1, N11, N12, N13, N21, N22, N23, NF1, NF2: Integer;
  F21, F22: Real;
  C1, C2, C3, T1, T2: string;

begin
  repeat
    // Repeat generating the question until the (random) car numbers are valid, i.e.
    //   - question result being between 0 and 100
    //   - all car numbers being integers
    N1 := 0; N11 := 0; N12 := 0; N13 := 0; N21 := 0; N22 := 0; N23 := 0;
    F21 := 0; F22 := 0; NF1 := 0; NF2 := 0;
    C1 := ''; C2 := ''; C3 := ''; T1 := ''; T2 := ''; Txt := '';
    // Generate question depending on question type passed as argument
    case QType of
      // Addition/subtraction questions
       1: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            Txt := T1 + ' ' + Questions[1];
            N := N11 + N12;
          end;
       2: begin
            QuestionText0(3, 3, 3, N11, N12, N13, C1, C2, C3, T1);
            Txt := T1 + ' ' + Questions[1];
            N := N11 + N12 + N13;
          end;
       3: begin
            QuestionText0(1, 0, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '_', '', '', 0, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N21;
          end;
       4: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '_', '', '', 0, 0, 1, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - N21;
          end;
       5: begin
            QuestionText0(1, 0, 3, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '+', '', '', 0, 0, 3, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N21 + N22;
          end;
       6: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '-', '', '', 0, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - N21 - N22;
          end;
       7: begin
            QuestionText0(1, 0, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '-', '', '', N11, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N21 - N22;
          end;
       8: begin
            QuestionText0(1, 0, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '+', '', '', -N11, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - N21 + N22;
          end;
       9: begin
            QuestionText0(1, 1, 3, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '+', 'random', 'random', 0, 0, 3, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N21 + N22;
          end;
      10: begin
            QuestionText0(1, 1, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '+', C1, 'random', 0, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + N21;
          end;
      11: begin
            QuestionText0(1, 1, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '+', 'random', C1, 0, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N11 + N22;
          end;
      12: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '_', C1, '', 0, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 - N21;
          end;
      13: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '_', C2, '', 0, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N12 - N21;
          end;
      14: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '+', C1, C2, 0, N12, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + N21;
          end;
      15: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '+', C1, C2, N11, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 + N22;
          end;
      16: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '-', C1, C2, 0, -N12, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 - N21;
          end;
      17: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '-', C1, C2, -N11, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 - N22;
          end;
      18: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '-', C1, C2, 0, -N12, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + N21;
          end;
      19: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('+', '-', C1, C2, N11, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 - N22;
          end;
      20: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '+', C1, C2, 0, N12, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 - N21;
          end;
      21: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText1('-', '+', C1, C2, -N11, 0, 2, N21, N22, C1, C2, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 + N22;
          end;
      // Fractions (type 1) questions
      22: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', '', '', 0, F21, F22, N23, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[4];
            N := Trunc(N11 * F21);
          end;
      23: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', '', '', 0, F21, F22, N23, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - Trunc(N11 * F21);
          end;
      24: begin
            repeat
              QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
              QuestionText2('-', '', '', 2, F21, F22, N23, T2); NF1 := N11;
            until N11 - (Trunc(N11 * F21) + N23) >= 0;                         // cars driving away must be <= total cars
            Txt := T1 + ' ' + T2 + ' ' + Questions[9];
            N := Trunc(N11 * F21) + N23;
          end;
      25: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('-', '', '', 2, F21, F22, N23, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - Trunc(N11 * F21) - N23;
          end;
      26: begin
            QuestionText0(1, 0, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('+', '', '', 2, F21, F22, N23, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[9];
            N := Trunc(N11 * F21);
          end;
      27: begin
            QuestionText0(1, 0, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('+', '', '', 2, F21, F22, N23, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - Trunc(N11 * F21) + N23;
          end;
      28: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C1, '', 0, F21, F22, N23, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[4];
            N := Trunc(N11 * F21);
          end;
      29: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C2, '', 0, F21, F22, N22, T2); NF1 := N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[4];
            N := Trunc(N12 * F21);
          end;
      30: begin
            repeat
              QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1); N1 := N11 + N12;
              QuestionText2('_', C1, '', 0, F21, F22, N23, T2); NF1 := N11;
            until N1 <= 100;                                                   // number of cars should always be <= 100
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N1 - Trunc(N11 * F21);
          end;
      31: begin
            repeat
              QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1); N1 := N11 + N12;
              QuestionText2('_', C2, '', 0, F21, F22, N23, T2); NF1 := N12;
            until N1 <= 100;                                                   // number of cars should always be <= 100
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N1 - Trunc(N12 * F21);
          end;
      32: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C1, C2, 0, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[5], '#color#', C1, [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
      33: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C1, C2, 0, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[5], '#color#', C2, [rfReplaceAll]);
            N := Trunc(N12 * F22);
          end;
      34: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C1, C2, 0, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[3], '#color#', C1, [rfReplaceAll]);
            N := N11 - Trunc(N11 * F21);
          end;
      35: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C1, C2, 0, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[3], '#color#', C2, [rfReplaceAll]);
            N := N12 - Trunc(N12 * F22);
          end;
      36: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText2('_', C1, C2, 0, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[4];
            N := Trunc(N11 * F21) + Trunc(N12 * F22);
          end;
      37: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1); N1 := N11 + N12;
            QuestionText2('_', C1, C2, 0, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N1 - (Trunc(N11 * F21) + Trunc(N12 * F22));
          end;
      38: begin
            repeat
              QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
              QuestionText2('-', C1, C2, 2, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            until N12 - (Trunc(N12 * F22) + N23) >= 0;                         // cars driving away must be <= total cars (actual color)
            Txt := T1 + ' ' + T2 + ' ' + Questions[4];
            N := Trunc(N11 * F21) + Trunc(N12 * F22) + N23;
          end;
      39: begin
            repeat
              QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1); N1 := N11 + N12;
              QuestionText2('-', C1, C2, 2, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            until N12 - (Trunc(N12 * F22) + N23) >= 0;                         // cars driving away must be <= total cars (actual color)
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N1 - Trunc(N11 * F21) - Trunc(N12 * F22) - N23;
          end;
      40: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1); N1 := N11 + N12;
            QuestionText2('+', C1, C2, 2, F21, F22, N23, T2); NF1 := N11; NF2 := N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N1 - Trunc(N11 * F21) - Trunc(N12 * F22) + N23;
          end;
      // Multiples (type 1) questions
      41: begin
            QuestionText0(1, 0, 3, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '_', '', '', 0, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + Trunc(N11 * F21);
          end;
      42: begin
            QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', '', '', 4, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + Trunc(N11 * F21) + N22;
          end;
      43: begin
            QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', '', '', 4, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[6];
            N := Trunc(N11 * F21) + N22;
          end;
      44: begin
            QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '-', '', '', 3, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + Trunc(N11 * F21) - N22;
          end;
      45: begin
            QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('+', '+', '', '', 4, N21, N22, F21, F22, T2); NF2 := N21;
             Txt := T1 + ' ' + T2 + ' ' + Questions[6];
            N := N21 + Trunc(N21 * F22);
          end;
      46: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('-', '-', '', '', 4, N21, N22, F21, F22, T2); NF2 := N21;
            Txt := T1 + ' ' + T2 + ' ' + Questions[9];
            N := N21 + Trunc(N21 * F22);
          end;
      47: begin
            QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('+', '+', '', '', 4, N21, N22, F21, F22, T2); NF2 := N21;
             Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N21 + Trunc(N21 * F22);
          end;
      48: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('-', '-', '', '', 4, N21, N22, F21, F22, T2); NF2 := N21;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - N21 - Trunc(N21 * F22);
          end;
      49: begin
            repeat
              QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('+', '-', '', '', 4, N21, N22, F21, F22, T2); NF2 := N21;
            until N11 + N21 <= 100;                                            // number of cars should always be <= 100
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N21 - Trunc(N21 * F22);
          end;
      50: begin
            repeat
              QuestionText0(1, 0, 4, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('-', '+', '', '', 4, N21, N22, F21, F22, T2); NF2 := N21;
            until N11 - N21 >= 0;                                              // cars driving away must be <= total cars
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 - N21 + Trunc(N21 * F22);
          end;
      51: begin
            QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '_', '', '', 0, N21, N22, F21, F22, T2); NF1 := N11 + N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N12 + Trunc(NF1 * F21);
          end;
      52: begin
            QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '_', C1, '', 0, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N12 + Trunc(N11 * F21);
          end;
      53: begin
            QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '_', C1, '', 0, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + Trunc(N11 * F21);
          end;
      54: begin
            QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '_', C2, '', 0, N21, N22, F21, F22, T2); NF1 := N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 + Trunc(N12 * F21);
          end;
      55: begin
            QuestionText0(2, 2, 5, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', C1, C2, 5, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N12 + Trunc(N11 * F21) + N22;
          end;
      56: begin
            QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', C1, C2, 4, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + Trunc(N11 * F21);
          end;
      57: begin
            QuestionText0(2, 2, 3, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', C1, C2, 4, N21, N22, F21, F22, T2);
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 + N22;
          end;
      58: begin
            QuestionText0(2, 2, 5, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', C1, C2, 5, N21, N22, F21, F22, T2); NF1 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[6];
            N := Trunc(N11 * F21) + N22;
          end;
      59: begin
            QuestionText0(2, 2, 5, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('x', '+', C2, C1, 5, N21, N22, F21, F22, T2); NF1 := N12;
            Txt := T1 + ' ' + T2 + ' ' + Questions[6];
            N := Trunc(N12 * F21) + N22;
          end;
      60: begin
            repeat
              QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('x', '-', C1, C2, 3, N21, N22, F21, F22, T2); NF1 := N11;
            until N12 - N22 >= 0;                                              // cars driving away must be <= total cars (actual color)
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N12 + Trunc(N11 * F21) - N22;
          end;
      61: begin
            repeat
              QuestionText0(2, 2, 4, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('x', '-', C2, C1, 3, N21, N22, F21, F22, T2); NF1 := N12;
            until N11 - N22 >= 0;                                              // cars driving away must be <= total cars (actual color)
            Txt := T1 + ' ' + T2 + ' ' + Questions[2];
            N := N11 + N12 + Trunc(N12 * F21) - N22;
          end;
      62: begin
            QuestionText0(2, 2, 5, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('+', '+', C1, C1, 5, N21, N22, F21, F22, T2); NF2 := N21;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + N21 + Trunc(N21 * F22);
          end;
      63: begin
            QuestionText0(2, 2, 5, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('+', '+', C2, C2, 5, N21, N22, F21, F22, T2); NF2 := N21;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 + N21 + Trunc(N21 * F22);
          end;
      64: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('-', '-', C1, C1, 4, N21, N22, F21, F22, T2); NF2 := N21;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 - N21 - Trunc(N21 * F22);
          end;
      65: begin
            QuestionText0(2, 2, 2, N11, N12, N13, C1, C2, C3, T1);
            QuestionText3('-', '-', C2, C2, 4, N21, N22, F21, F22, T2); NF2 := N21;
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 - N21 - Trunc(N21 * F22);
          end;
      66: begin
            repeat
              QuestionText0(2, 2, 3, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('+', '-', C1, C1, 4, N21, N22, F21, F22, T2); NF2 := N21;
            until N11 + N21 <= 100;                                            // number of cars should always be <= 100
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 + N21 - Trunc(N21 * F22);
          end;
      67: begin
            repeat
              QuestionText0(2, 2, 3, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('+', '-', C2, C2, 4, N21, N22, F21, F22, T2); NF2 := N21;
            until N12 + N21 <= 100;                                            // number of cars should always be <= 100
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 + N21 - Trunc(N21 * F22);
          end;
      68: begin
            repeat
              QuestionText0(2, 2, 3, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('-', '+', C1, C1, 4, N21, N22, F21, F22, T2); NF2 := N21;
            until N11 - N21 >= 0;                                              // cars driving away must be <= total cars
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C1, [rfReplaceAll]);
            N := N11 - N21 + Trunc(N21 * F22);
          end;
      69: begin
            repeat
              QuestionText0(2, 2, 3, N11, N12, N13, C1, C2, C3, T1);
              QuestionText3('-', '+', C2, C2, 4, N21, N22, F21, F22, T2); NF2 := N21;
            until N12 - N21 >= 0;                                              // cars driving away must be <= total cars
            Txt := T1 + ' ' + T2 + ' ' + Questions[3];
            Txt := StringReplace(Txt, '#color#', C2, [rfReplaceAll]);
            N := N12 - N21 + Trunc(N21 * F22);
          end;
      // Fractions (type 2) questions
      70: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(1, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF2 := N11 - N21;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', C2, [rfReplaceAll]);
            N := Trunc((N11 - N21) * F22);
          end;
      71: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(1, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF2 := N11 - N21;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', C3, [rfReplaceAll]);
            N := (N11 - N21) - Trunc((N11 - N21) * F22);
          end;
      72: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(1, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF2 := N11 - N21;
            Txt := T1 + ' ' + T2 + ' ';
            T1 := StringReplace(Questions[8], '#color1#', C2, [rfReplaceAll]);
            Txt += StringReplace(T1, '#color2#', C1, [rfReplaceAll]);
            N := Trunc((N11 - N21) * F22) + N21;
          end;
      73: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(1, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF2 := N11 - N21;
            Txt := T1 + ' ' + T2 + ' ';
            T1 := StringReplace(Questions[8], '#color1#', C3, [rfReplaceAll]);
            Txt += StringReplace(T1, '#color2#', C1, [rfReplaceAll]);
            N := (N11 - N21) - Trunc((N11 - N21) * F22) + N21;
          end;
      74: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(1, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF2 := N11 - N21;
            Txt := T1 + ' ' + T2 + ' ';
            T1 := StringReplace(Questions[8], '#color1#', C2, [rfReplaceAll]);
            Txt += StringReplace(T1, '#color2#', C3, [rfReplaceAll]);
            N := N11 - N21;
          end;
      75: begin
            repeat
              QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
              QuestionText4(2, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11;
            until N11 - Trunc(N11 * F21) - N22 > 0;                            // last group should be at least 1 car
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', C1, [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
      76: begin
            repeat
              QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
              QuestionText4(2, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11;
            until N11 - Trunc(N11 * F21) - N22 > 0;                            // last group should be at least 1 car;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', C3, [rfReplaceAll]);
            N := N11 - Trunc(N11 * F21) - N22;
          end;
      77: begin
            repeat
              QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
              QuestionText4(2, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11;
            until N11 - Trunc(N11 * F21) - N22 > 0;                            // last group should be at least 1 car;
            Txt := T1 + ' ' + T2 + ' ';
            T1 := StringReplace(Questions[8], '#color1#', C1, [rfReplaceAll]);
            Txt += StringReplace(T1, '#color2#', C2, [rfReplaceAll]);
            N := Trunc(N11 * F21) + N22;
          end;
      78: begin
            repeat
              QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
              QuestionText4(2, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11;
            until N11 - Trunc(N11 * F21) - N22 > 0;                            // last group should be at least 1 car;
            Txt := T1 + ' ' + T2 + ' ';
            T1 := StringReplace(Questions[8], '#color1#', C1, [rfReplaceAll]);
            Txt += StringReplace(T1, '#color2#', C3, [rfReplaceAll]);
            N := N11 - N22;
          end;
      79: begin
            repeat
              QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
              QuestionText4(2, 4, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11;
            until N11 - Trunc(N11 * F21) - N22 > 0;                            // last group should be at least 1 car;
            Txt := T1 + ' ' + T2 + ' ';
            T1 := StringReplace(Questions[8], '#color1#', C2, [rfReplaceAll]);
            Txt += StringReplace(T1, '#color2#', C3, [rfReplaceAll]);
            N := N11 - Trunc(N11 * F21);
          end;
      80: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(3, 0, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 - N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', C1, [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
      81: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(3, 0, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 - N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', C3, [rfReplaceAll]);
            N := N11 - Trunc(N11 * F21) - Trunc(NF2 * F22);
          end;
      82: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(3, 0, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11; NF1 := N11; NF2 := Trunc(N11 - N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', C1, [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', C2, [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := Trunc(N11 * F21) + Trunc(NF2 * F22);
          end;
      83: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(3, 0, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11; NF1 := N11; NF2 := Trunc(N11 - N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', C1, [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', C3, [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := N11 - Trunc(NF2 * F22);
          end;
      84: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText4(3, 0, N21, N22, F21, F22, C1, C2, C3, T2); NF1 := N11; NF1 := N11; NF2 := Trunc(N11 - N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', C2, [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', C3, [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := N11 - Trunc(N11 * F21);
          end;
      // Multiples (type 2) questions
      85: begin
            QuestionText0(1, 1, 3, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 1, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11;
            Txt := T1 + ' ' + T2 + ' ' + Questions[1];
            N := N11 + Trunc(N11 * F21) + Trunc(N11 * F22);
          end;
      86: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 2, F21, F22, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + Questions[1];
            N := N11 + Trunc(N11 * F21) + Trunc((N11 * F21) * F22);
          end;
      87: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 3, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11 + Trunc(N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + Questions[1];
            N := N11 + Trunc(N11 * F21) + Trunc((N11 + N11 * F21) * F22);
          end;
      88: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 2, F21, F22, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
      89: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 2, F21, F22, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            N := Trunc((N11 * F21) * F22);
          end;
      90: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 2, F21, F22, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', Copy(C1, 1, Length(C1) - 1), [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := N11 + Trunc(N11 * F21);
          end;
      91: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 2, F21, F22, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', Copy(C1, 1, Length(C1) - 1), [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := N11 + Trunc((N11 * F21) * F22);
          end;
      92: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 2, F21, F22, C2, C3, T2); NF1 := N11; NF2 := Trunc(N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := Trunc(N11 * F21) + Trunc((N11 * F21) * F22);
          end;
      93: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 3, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11 + Trunc(N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
      94: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 3, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11 + Trunc(N11 * F21);
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            N := Trunc((N11 + N11 * F21) * F22);
          end;
      95: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 3, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11 + Trunc(N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', Copy(C1, 1, Length(C1) - 1), [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := N11 + Trunc(N11 * F21);
          end;
      96: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 3, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11 + Trunc(N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', Copy(C1, 1, Length(C1) - 1), [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := N11 + Trunc((N11 + N11 * F21) * F22);
          end;
      97: begin
            QuestionText0(1, 1, 4, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5(C1, 3, F21, F22, C2, C3, T2); NF1 := N11; NF2 := N11 + Trunc(N11 * F21);
            Txt := StringReplace(Questions[8], '#color1#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            Txt := StringReplace(Txt, '#color2#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            Txt := T1 + ' ' + T2 + ' ' + Txt;
            N := Trunc(N11 * F21) + Trunc((N11 + N11 * F21) * F22);
          end;
      98: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5('', 1, F21, F22, C2, C3, T2); NF1 := N11;
            if F21 = 1 then
              F21 := 1 / 2
            else if F21 = 0.5 then
              F21 := 1 / 3
            else
              F21 := 2 / 3;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', Copy(C2, 1, Length(C2) - 1), [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
      99: begin
            QuestionText0(1, 0, 1, N11, N12, N13, C1, C2, C3, T1);
            QuestionText5('', 1, F21, F22, C2, C3, T2); NF1 := N11;
            if F21 = 1 then
              F21 := 1 / 2
            else if F21 = 0.5 then
              F21 := 2 / 3
            else
              F21 := 1 / 3;
            Txt := T1 + ' ' + T2 + ' ' + StringReplace(Questions[7], '#color#', Copy(C3, 1, Length(C3) - 1), [rfReplaceAll]);
            N := Trunc(N11 * F21);
          end;
    end;
  until (N >= 0) and (N <= 100) and ((NF1 = 0) or (F21 * NF1 = Int(F21 * NF1))) and ((NF2 = 0) or (F22 * NF2 = Int(F22 * NF2)));
end;

//
// Question text: Original situation
//

procedure QuestionText0(N, NC, NumberType: Integer; var N1, N2, N3: Integer; var C1, C2, C3, Txt:string);

begin
  N1 := 0; N2 := 0; N3 := 0;
  C1 := ''; C2 := ''; C3 := ''; Txt := '';
  // Total number of cars
  if (N = 1) and (NC = 0) then begin
    N1 := RandomCars(NumberType, '_', 0);
    Txt := 'Insgesamt sind es ' + IntToStr(N1) + ' Autos.';
  end
  // Two or three car groups (colors)
  else begin
    RandomColors(NC, 'e', C1, C2, C3);
    N1 := RandomCars(NumberType, '_', 0);
    Txt := 'Es sind ' + IntToStr(N1) + ' ' + C1;
    if N > 1 then begin
      if N = 2 then
        Txt += ' und '
      else
        Txt += ', ';
      N2 := RandomCars(NumberType, '_', 0);
      Txt += IntToStr(N2) + ' ' + C2;
      if N = 3 then begin
        N3 := RandomCars(NumberType, '_', 0);
        Txt += ' und ' + IntToStr(N3) + ' ' + C3;
      end;
    end;
    Txt += ' Autos.';
  end;
end;

{ Question text: Questions type 1 (Addition/subtraction) }

procedure QuestionText1(Action1, Action2: Char; Color1, Color2: string; Check1, Check2, NumberType: Integer; var N1, N2: Integer; var C1, C2, Txt: string);

var
  NC: Integer;
  C3: string;

begin
  N1 := 0; N2 := 0; NC := 0;
  C1 := ''; C2 := ''; C3 := '';
  if Color1 <> '' then
    Inc(NC);
  if Color2 <> '' then
    Inc(NC);
  RandomColors(NC, 'e', C1, C2, C3);
  // Use fix (instead of random) color if it is specified
  if (Color1 <> '') and (Color1 <> 'random') then
    C1 := Color1;
  if (Color2 <> '') and (Color2 <> 'random') then
    C2 := Color2;
  // Questions with 2 actions (cars arriving and driving away)
  if (Action2 = '+') or (Action2 = '-') then begin
    // Get random number of cars
    // Intermediate results constraints: cv = 0: no constraint; cv > 0: check if
    // result is within limit (<= 100); cv < 0: check if result isn't negative
    if Check1 = 0 then
      N1 := RandomCars(NumberType, '_', 0)
    else begin
      if Check1 > 0 then
        N1 := RandomCars(NumberType, '+', Check1)
      else
        N1 := RandomCars(NumberType, '-', -Check1);
    end;
    if Check2 = 0 then
      N2 := RandomCars(NumberType, '_', 0)
    else begin
      if Check2 > 0 then
        N2 := RandomCars(NumberType, '+', Check2)
      else
        N2 := RandomCars(NumberType, '-', -Check2);
    end;
    // Somewhat different text for same and diffrent actions
    if Action1 = Action2 then begin
      // Two same actions
      Txt := 'Zuerst ';
      if Action1 = '+' then begin
        // Actions = cars arriving
        Txt += 'kommen ' + IntToStr(N1);
        if Color1 <> '' then
          Txt += ' ' + C1;
        Txt += ', dann ' + IntToStr(N2);
        if Color2 <> '' then
          Txt += ' ' + C2;
        Txt += ' Autos hinzu.'
      end
      else if Action1 = '-' then begin
        // Actions = cars driving away
        Txt += 'fahren ' + IntToStr(N1);
        if Color1 <> '' then
          Txt += ' ' + C1;
        Txt += ', dann ' + IntToStr(N2);
        if Color2 <> '' then
          Txt += ' ' + C2;
        Txt += ' Autos weg.'
      end;
    end
    else begin
      // Two different actions
      Txt := IntToStr(N1);
      if Color1 <> '' then
          Txt += ' ' + C1;
      if Action1 = '+' then begin
        // First action = cars arriving (second action = cars driving away)
        Txt += ' Autos kommen hinzu, ';
        Txt += 'dann fahren ' + IntToStr(N2);
        if Color2 <> '' then
          Txt += ' ' + C2;
        Txt += ' Autos weg.'
      end
      else if Action1 = '-' then begin
        // First action = cars driving away (second action = cars arriving)
        Txt += ' Autos fahren weg, ';
        Txt += 'dann kommen ' + IntToStr(N2);
        if Color2 <> '' then
          Txt += ' ' + C2;
        Txt += ' Autos hinzu.'
      end;
    end;
  end
  // Questions with 1 action (cars arriving or driving away)
  else begin
    N1 := RandomCars(NumberType, '_', 0);
    Txt := IntToStr(N1);
    if Color1 <> '' then
      Txt += ' ' + C1;
    if Action1 = '+' then
      // Cars arriving
      Txt += ' Autos kommen hinzu.'
    else if Action1 = '-' then
      // Cars driving away
      Txt += ' Autos fahren weg.';
  end;
end;

{ Question text: Questions type 2 (Fractions 1) }

procedure QuestionText2(Action2: Char; Colour1, Colour2: string; NumberType: Integer; var F1, F2: Real; var N3: Integer; var Txt: string);

var
  FX: Integer;

begin
  F1 := 0; F2 := 0; N3 := 0; Txt := '';
  FX := Random(9) + 1; F1 := FractionsN[FX] / FractionsD[FX];
  // First fraction = all cars or first color cars
  Txt := UpperFirst(FractionsS[FX]) + ' der ';
  if Colour1 <> '' then
    Txt += Colour1 + 'n ';
  Txt += 'Autos ';
  // Second fraction = second color cars (only if specified)
  if Colour2 = '' then begin
    // No second color cars
    if FractionsN[FX] <> 1 then
      Txt += 'fahren weg.'
    else
      Txt += 'faehrt weg.';
  end
  else begin
    // Second color cars fraction
    FX := Random(9) + 1; F2 := FractionsN[FX] / FractionsD[FX];
    Txt += 'und ' + FractionsS[FX] + ' der ';
    Txt += Colour2 + 'n Autos fahren weg.';
  end;
  // Cars arriving / driving away (if action specified)
  if (Action2 = '+') or (Action2 = '-') then begin
    if Action2 = '+' then
      Txt += ' Dann kommen '
    else
      Txt += ' Dann fahren ';
    N3 := RandomCars(NumberType, '_', 0);
    Txt += IntToStr(N3);
    if Colour2 <> '' then
      Txt += ' ' + Colour2;
    Txt += ' Autos ';
    if Action2 = '+' then
      Txt += 'hinzu.'
    else
      Txt += 'weg.';
  end;
end;

{ Question text: Questions type 3 (Multiples 1) }

procedure QuestionText3(Action1, Action2: Char; Colour1, Colour2: string; NumberType: Integer; var N1, N2: Integer; var M1, M2: Real; var Txt: string);

var
  MX: Integer;

begin
  N1 := 0; N2 := 0; M1 := 0; M2 := 0; Txt := '';
  if Action1 = 'x' then begin
    // First group of cars = multiple
    MX := Random(6) + 1; M1 := MultN[MX] / MultD[MX];
    Txt := UpperFirst(SMult[MX]) + ' ';
  end
  else begin
    // First group of cars = number
    N1 := RandomCars(NumberType, '_', 0);
    Txt += IntToStr(N1) + ' ';
  end;
  if Colour1 <> '' then
    Txt += Colour1 + ' ';
  if (Action1 = 'x') or (Action1 = '+') then
    Txt += 'Autos kommen hinzu'
  else
    Txt += 'Autos fahren weg';
  // Second group of cars only if second action tells so
  if (Action2 = '+') or (Action2 = '-') then begin
    if Action2 = '+' then begin
      Txt += ' und dann kommen ';
      if (Action1 = 'x') or (Action1 = '+') then
        Txt += 'noch einmal ';
    end
    else begin
      Txt += ' und dann fahren ';
      if (Action1 = '-') then
        Txt += 'noch einmal ';
    end;
    if Action1 = 'x' then begin
      // Second group of cars = number
      N2 := RandomCars(NumberType, '_', 0);
      Txt += IntToStr(N2);
    end
    else begin
      // Second group of cars = multiple
      MX := Random(6) + 1; M2 := MultN[MX] / MultD[MX];
      Txt += SMult[MX];
    end;
    if Colour2 <> '' then
      Txt += ' ' + Colour2;
    Txt += ' Autos ';
    if Action2 = '+' then
      Txt += 'hinzu'
    else
      Txt += 'weg';
  end;
  Txt += '.';
end;

{ Question text: Questions type 4 (Fractions 2) }

procedure QuestionText4(QuestionSubType, NumberType: Integer; var N1, N2: Integer; var F1, F2: Real; var C1, C2, C3, Txt: string);

var
  FX: Integer;

begin
  N1 := 0; N2 := 0; F1 := 0; F2 := 0;
  C1 := ''; C2 := ''; C3 := ''; Txt := '';
  RandomColors(3, '', C1, C2, C3);
  if QuestionSubType = 1 then begin
    // Question subtype 1: 1st car group = constant, 2nd car group = fraction
    N1 := RandomCars(NumberType, '_', 0);
    Txt := IntToStr(N1) + ' sind ' + C1 + '.';
    Txt += ' Von den uebrigen ';
    FX := Random(9) + 1; F2 := FractionsN[FX] / FractionsD[FX];
    if FractionsN[FX] = 1 then
      Txt += 'ist '
    else
      Txt += 'sind ';
    Txt += FractionsS[FX] + ' ' + C2 + ', ';
  end
  else if QuestionSubType = 2 then begin
    // Question subtype 2: 1st car group = fraction, 2nd car group = constant
    FX := Random(9) + 1; F1 := FractionsN[FX] / FractionsD[FX];
    Txt := UpperFirst(FractionsS[FX]) + ' davon ';
    if FractionsN[FX] = 1 then
      Txt += 'ist '
    else
      Txt += 'sind ';
    Txt += C1  + '.';
    Txt += ' Von den uebrigen sind ';
    N2 := RandomCars(NumberType, '_', 0);
    Txt += IntToStr(N2) + ' ' + C2 + ', ';
  end
  else begin
    // Question subtype 3: both car groups = fractions
    FX := Random(9) + 1; F1 := FractionsN[FX] / FractionsD[FX];
    Txt := UpperFirst(FractionsS[FX]) + ' davon ';
    if FractionsN[FX] = 1 then
      Txt += 'ist '
    else
      Txt += 'sind ';
    Txt += C1  + '.';
    Txt += ' Von den uebrigen ';
    FX := Random(9) + 1; F2 := FractionsN[FX] / FractionsD[FX];
    if FractionsN[FX] = 1 then
      Txt += 'ist '
    else
      Txt += 'sind ';
    Txt += FractionsS[FX] + ' ' + C2 + ', ';
  end;
  Txt += 'der Rest ist ' + C3 + '.';
end;

{ Question text: Questions type 5 (Multiples 2) }

procedure QuestionText5(Colour1: string; QuestionSubType: Integer; var M2, M3: Real; var C2, C3, Txt: string);

var
  MX: Integer;
  C1, Colour: string;

begin
  M2 := 0; M3 := 0; C2 := ''; C3 := ''; Txt := '';
  if Colour1 = '' then begin
    RandomColors(3, 'e', C1, C2, C3);
    C3 := C1;                                                                  // set this, as C3 is returned
  end
  else begin
    C1 := Colour1;
    repeat
      RandomColors(2, 'e', C2, C3, Colour);
    until (C2 <> C1) and (C3 <> C1);
  end;
  if Colour1 = '' then
    Txt := 'Die einen sind ' + Copy(C2, 1, Length(C2) - 1) + ', die anderen sind ' + Copy(C1, 1, Length(C1) - 1) + '. ';
  // First group of cars = multiple of initial cars
  MX := Random(6) + 1; M2 := MultN[MX] / MultD[MX];
  TxT += 'Es sind ' + SMult[MX] + ' ' + C2 + ' wie ' + C1 + ' Autos';
  if Colour1 = '' then
    Txt += '.'
  else begin
    // Second group of cars
    MX := Random(6) + 1; M3 := MultN[MX] / MultD[MX];
    Txt += ' und ' + SMult[MX] + ' ' + C3 + ' Autos wie ';
    if QuestionSubType = 1 then
      // Multiple of initial cars
      Txt += C1 + ' Autos.'
    else if QuestionSubType = 2 then
      // Multiple of first group of cars
      Txt += C2 + ' Autos.'
    else
      // Multiple of initial cars and first group of cars together
      Txt += C1 + ' und ' + C2 + ' Autos zusammen.';
  end;
end;

{ Get a random place }

function RandomPlace: string;

const
  NPlaces = 15;
  Places: array[1..NPlaces] of string = (
    'auf dem grossen Parkplatz', 'in ihrer Strasse', 'vor dem Rathaus', 'vor der Kirche', 'vor dem Einkaufszentrum',
    'vor dem Museum', 'vor der Badeanstalt', 'im Park', 'vor dem Bahnhof', 'vor der Disko', 'vor dem Supermarkt',
    'vor der Sporthalle', 'auf dem Campingplatz', 'vor dem neuen Cafe', 'vor der Oper');

var
  R: Integer;
  P: string;

begin
  R := Random(NPlaces) + 1; P := Places[R];
  R := Random(2);
  if R = 0 then
    P := StringReplace(P, 'vor', 'hinter', [rfReplaceAll]);
  RandomPlace := P;
end;

{ Get random number of cars }

function RandomCars(NumberType: Integer; CheckType: Char; CheckValue: Integer): Integer;

var
  N0: Integer;
  Ok: Boolean;

begin
  N0 := 0;
  repeat
    Ok := True;
    // Get random number of cars (depending on limits specified by number type)
    case NumberType of
      1: N0 := Random(51) + 50;
      2: N0 := Random(56) + 20;
      3: N0 := Random(41) + 10;
      4: N0 := Random(26) + 5;
      5: N0 := Random(16) + 5;
    end;
    // Check constraints for intermediate results
    if (CheckType = '+') then begin
      // Results shouldn't be greater than 100
      if CheckValue + N0 > 100 then
        Ok := False;
    end
    else if (CheckType = '-') then begin
      // Results must not be negative
      if CheckValue - N0 < 0 then
        Ok := False;
    end;
  until Ok;
  RandomCars := N0;
end;

{ Get random colors }

procedure RandomColors(NC: Integer; Ending: string; var C1, C2, C3: string);

const
  NColours = 10;
  Colours: array[1..NColours] of string = (
    'schwarz', 'weiss', 'blau', 'rot', 'gruen', 'gelb', 'braun', 'orangenfarben', 'grau', 'tuerkisfarben');

var
  Colors: array[1..3] of string;
  R, I, J: Integer;
  Ok: Boolean;

begin
  for I := 1 to 3 do
    Colors[I] := '';
  repeat
    // Get random colors
    for I := 1 to NC do begin
      R := Random(NColours) + 1;
      Colors[I] := Colours[R];
    end;
    // Check for uniqueness
    Ok := True;
    for I := 1 to NC - 1 do begin
      for J := I + 1 to NC do begin
        if Colors[I] = Colors[J] then
          Ok := False;
      end;
    end;
  until Ok;
  C1 := Colors[1] + Ending; C2 := Colors[2] + Ending; C3 := Colors[3] + Ending;
end;

{ Transform fisrt character of string to uppercase }

function UpperFirst(S: string): string;

var
  Ch: Char;

begin
  Ch := S[1];
  Delete(S, 1, 1);
  S := UpperCase(Ch) + S;
  UpperFirst := S;
end;

end.

