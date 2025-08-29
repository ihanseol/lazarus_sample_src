{ *************************************************************************** }
{               Morse code list, translation and knowledge test               }
{ *************************************************************************** }

program morse;

uses
  Crt, sysutils;

const
  MaxChars = 60;

type
  TCharacters = array[1 .. MaxChars] of Char;
  TMorseCodes = array[1 .. MaxChars, 1 .. 6] of Char;

var
  Characters : TCharacters;
  MorseCodes : TMorseCodes;
  Letters, Numbers, Punctuation, TotalChars, I, J : Integer;
  S : string;
  Choice : Char;

{ Read characters and morse codes from file (morse.txt) }

procedure ReadMorseCodes(var Chars: TCharacters; var Morses: TMorseCodes; var L, N, P, T: Integer);

var
  C, M, I : Integer;
  S : string;
  Infile : Text;

begin
  Assign(Infile, 'morse.txt');
  Reset(Infile);
  L := 0; N := 0; P := 0; T := 0; C := 0;
  // Read all characters and corresponding morse codes from file
  while (not EoF(Infile)) and (C <= MaxChars) do begin
    Readln(Infile, S);
    Inc(C); M := 0;
    // Count letters, numbers and punctuation symbols
    case S[1] of
      'L' : Inc(L);
      'N' : Inc(N);
      'P' : Inc(P);
    end;
    // Fill characters array
    Chars[C] := S[3];
    // Fill corresponding morse codes array
    for I := 5 to Length(S) do begin
      Inc(M);
      Morses[C, M] := S[I];
    end;
  end;
  // Total characters found in file
  T := L + N + P;
  Close(Infile);
end;

{               Function : Character to morse code translation              }
{ The function returns the morse code corresponding to the character passed }
{ as argument (as a string!) or a message if character invalid/unknown      }

function CharToMorse(Ch: TCharacters; Mo: TMorseCodes; T: Integer; S1: string): string;

var
  C, M, I : Integer;
  S2 : string;

begin
  if Length(S1) = 1 then begin                   // string must be 1 character
    C := 0; S2 := '';
    S1[1] := UpCase(S1[1]);
    for I := 1 to T do                           // search character in characters array
      if S1[1] = Ch[I] then
        C := I;
     if C <> 0 then begin                        // if found, find corresponding morse code
       for M := 1 to 6 do
         if Mo[C, M] <> ' ' then                 // trim spaces at end
           S2 := S2 + Mo[C, M];
     end
     else                                        // character not found
       S2 := 'Unknown character';
  end
  else                                           // string is more than 1 character
    S2 := 'Invalid character';
  CharToMorse := S2;
end;

{               Function : Morse code to character translation              }
{ The function returns the character (as a string!) corresponding to the    }
{ morse code passed as argument or a message if morse code  invalid/unknown }

function MorseToChar(Ch: TCharacters; Mo: TMorseCodes; T: Integer; S1: string): string;

var
  C, M, I : Integer;
  S2 : string;
  MorseValid, MorseOK, CodeOK : Boolean;

begin
  MorseValid := True; MorseOK := True;
  if Length(S1) <= 6 then begin                  // maximum of symbols = 6
    for M := 1 to Length(S1) do                  // must be dots or dashes
      if not (S1[M] in ['.', '-']) then
        MorseValid := False;
    if MorseValid then begin
      if Length(S1) < 6 then begin               // extend to 6 symbols (as in array)
        for I := Length(S1) + 1 to 6 do
          S1 := S1 + ' ';
      end;
      I := 0; C := 0;
      MorseOK := False;
      repeat                                     // search morse codes array
        Inc(I);
        CodeOK := True;
        for M := 1 to 6 do                       // check all dots/dashes/spaces for this code
          if S1[M] <> Mo[I, M] then
            CodeOK := False;
        if CodeOK then begin                     // morse code found
          C := I;
          I := T;
          MorseOK := True;
        end;
      until I = T;
    end;
  end
  else
    MorseOK := False;
  if MorseValid and MorseOK then                 // if morse code found, return character
    S2 := Ch[C]
  else if not MorseValid then                    // to long or invalid symbols
    S2 := 'Invalid morse code'
  else                                           // not found in morse codes array
    S2 := 'Unknown morse code';
  MorseToChar := S2;
end;

{ Option 1: Display morse codes list }

procedure DisplayMorseCodes(Chars: TCharacters; Morses: TMorseCodes; L, N, P, T: Integer);

var
  I, J, K : Integer;
  Key : Char;

begin
  ClrScr;
  GotoXY(32, 1); Writeln('Morse codes list');
  GotoXY(32, 2); Writeln('----------------');
  Writeln;
  K := 0;
  for I := 1 to T do begin
    // Display the character type
    if I = 1 then begin
      Writeln('Letters'); Writeln;
    end
    else if I = L + 1 then begin
      Writeln('Numbers'); Writeln;
    end
    else if I = L + N + 1 then begin
      Writeln('Punctuation'); Writeln;
    end;
    Inc(K);
    // Display character and corresponding morse code
    Write(Chars[I], '  ');
    for J := 1 to 6 do
      Write(Morses[I, J]);
    Write('    ');
    // New line if character type changes
    if (I = L) or (I = L + N) or (I = L + N + P) then begin
      Writeln;  Writeln;
      K := 0;
    end
    // New line all 6 columns
    else if K mod 6 = 0 then
      Writeln;
  end;
  Writeln;
  Write('Hit any key to continue');
  repeat until KeyPressed; Key := ReadKey; if Ord(Key) = 0 then Key := ReadKey;
end;

{ Options 2 and 3: Translate character to morse code (or vice-versa) }

procedure TranslateChar(Chars: TCharacters; Morses: TMorseCodes;
                        T: Integer; Option: string);

var
  Title, Txt, O, S1, S2 : string;

begin
  ClrScr;
  O := Option; O[1] := UpCase(O[1]);
  if Option = 'character' then begin
    Title := 'Translate character to morse code';
    Txt   := 'Enter character (letter, number, punctuation) to translate.';
  end
  else begin
    Title := 'Translate morse code to character';
    Txt   := 'Enter the morse code, using "." as dot and "-" as dash.';
  end;
  GotoXY(24, 1); Writeln(Title);
  GotoXY(24, 2); Writeln('---------------------------------');
  Writeln;
  Write(Txt);
  Writeln(' To terminate, just'); Writeln('hit the ENTER key.');
  repeat
    GotoXY(1, 7); ClrEoL;
    GotoXY(1, 7);
    // Enter character resp. morse code from keyboard
    Write(O, ' ? '); Readln(S1);
    GotoXY(1, 9); ClrEoL;
    GotoXY(1, 9);
    // Call char to morse resp. morse to char function
    if Length(S1) > 0 then begin
      if Option = 'character' then
        S2 := CharToMorse(Chars, Morses, T, S1)
      else
        S2 := MorseToChar(Chars, Morses, T, S1);
      // Display morse code resp. character or error message
      if (Copy(S2, 1, 7) = 'Unknown') or (Copy(S2, 1, 7) = 'Invalid') then
        Writeln(S2, ': ', S1)
      else
        Writeln(S1, ' ==> ', S2);
    end;
  until S1 = '';
end;

{ Options 4 and 5: Translate text to morse codes (or vice-versa) }

procedure TranslateText(Chars: TCharacters; Morses: TMorseCodes;
                       T: Integer; Option: string);

 var
   I, J, N : Integer;
   Title, Symbols, S, S1, S2 : string;
   Key : Char;
   Data : array[1 .. 10] of string[80];

 begin
   ClrScr;
   if Option = 'text' then begin
     Title := 'Translate text to morse codes';
     Symbols := 'characters';
   end
   else begin
     Title := 'Translate morse codes to text';
     Symbols := 'morse symbols';
   end;
   GotoXY(26, 1); Writeln(Title);
   GotoXY(26, 2); Writeln('-----------------------------');
   Writeln;
   Writeln('Enter ', Option, ' on one or several lines, terminated by the ENTER key. Maximum');
   Writeln('of ', Symbols, ' per line = 80; maximum of lines = 10. To terminate your entry,');
   Writeln('enter an empty line (just press ENTER).');
   Writeln;
   if Option = 'morse codes' then begin
     Write('Enter the morse codes, using "." as dot and "-" as dash, ');
     TextColor(Yellow); Writeln('1 space as short gap');
     TextColor(White); Write('(between characters) and ');
     TextColor(Yellow); Write('3 spaces as medium gap');
     TextColor(White); Writeln(' (between words).');
     Writeln;
   end;
   for I := 1 to 10 do
     Data[I] := ' ';
   I := 0;
   repeat
     // Enter text or morse code sequence from keyboard
     Readln(S);
       if Option = 'morse codes' then begin
         // Transform 2 spaces to 1 (small gap), 4 spaces to 3 (medium gap)
         S := StringReplace(S, '    ', '   ', [rfReplaceAll]);
         S := StringReplace(S, '   ', '###', [rfReplaceAll]);
         S := StringReplace(S, '  ', ' ', [rfReplaceAll]);
         S := StringReplace(S, '###', '   ', [rfReplaceAll]);
       end;
       Inc(I); Data[I] := S;
   until (S = '') or (I = 10);                   // max 10 lines or terminated by user
   if I > 0 then begin
     N := I;
     if N = 10 then
       Writeln;
     // N lines of text resp. morse code sequences
     for I := 1 to N do begin
       J := 1;
       // For each char/code in input line, call char to morse resp. morse to char function
       repeat
         if Option = 'text' then begin
           // Text to morse code translation
           S1 := Data[I][J];
           S2 := CharToMorse(Chars, Morses, T, S1);
           if (Copy(S2, 1, 7) = 'Unknown') or (Copy(S2, 1, 7) = 'Invalid') then begin
             if S1 = ' ' then
               Write('  ')                      // gap between 2 words
             else
               Write('? ');                     // unknown character
           end
           else
             Write(S2, ' ');                    // morse code corresponding to this character
         Inc(J);
         end
         else begin
           // Morse code to text translation
           S1 := '';
           repeat
             // Extract morse code from input line
             if Data[I][J] <> ' ' then begin
               S1 := S1 + Data[I][J];
               Inc(J);
             end
             else
               Write(' ');                      // medium gap (between 2 words)
           until (J > Length(Data[I])) or (Data[I][J] = ' ');
           S2 := MorseToChar(Chars, Morses, T, S1);
           // Display character or "?" if invalid/unknown
           if (Copy(S2, 1, 7) = 'Unknown') or (Copy(S2, 1, 7) = 'Invalid') then begin
             if S1 = '' then begin              // small gap (between 2 characters)
               Inc(J);
             end
             else                               // not found in morse code array
              Write('?');
           end
           else                                 // morse code found
             Write(S2);
           Inc(J);
         end;
       until J > Length(Data[I]);
       Writeln;
     end;
   end;
   Writeln;
   Write('Hit any key to continue... ');
   repeat until KeyPressed;
   Key := ReadKey; if Ord(Key) = 0 then Key := ReadKey;
 end;

{ Options 6 and 7: Morse code knowledge test }

procedure MorseTest(Chars: TCharacters; Morses: TMorseCodes; CL, CN, CT: Integer; Option: string);

  var
    T0, N, C, P, R, I : Integer;
    Code1, Code2, Ucode, Choice : string;
    Key : Char;
    Done : array [1 .. MaxChars] of Boolean;

  begin
    N := 0; C := 0;
    for I := 1 to MaxChars do
      Done[I] := False;
    ClrScr;
    GotoXY(33, 1); Writeln('Morse code test');
    GotoXY(33, 2); Writeln('---------------');
    Writeln;
    // Choose what to test
    Writeln('Settings:'); Writeln;
    Writeln('  1 = Letters only');
    Writeln('  2 = Letters and numbers');
    Writeln('  3 = All characters');
    Writeln;
    Write('  Your choice ? '); Readln(Choice); Writeln;
    if Choice = '3' then
      T0 := CT
    else if Choice = '2' then
      T0 := CL + CN
    else
      T0 := CL;
    if Option = 'character' then
      Writeln('Enter the morse code ("." as dot, "-" as dash) for the character displayed.')
    else
      Writeln('Enter the character for the morse code displayed.');
    Writeln('Just hit the ENTER key to terminate...');
    repeat
      UCode := '';
      // Test until user terminates or all answered correctly
      repeat
        R := Random(T0);                         // random character resp. morse code
        if not Done[R] then begin                // check if not already correctly answered
          GotoXY(1, 15); ClrEoL; GotoXY(1, 16); ClrEoL;
          GotoXY(1, 15);
          if Option = 'character' then begin
            // Get corresponding morse code (from array)
            Code1 := Chars[R];
            Code2 := '';
            for I := 1 to 6 do
              if Morses[R, I] <> ' ' then
                Code2 := Code2 + Morses[R, I];
            Writeln('Character  : ', Code1);
            // Get corresponding morse code (from user)
            Write('Morse code ? '); Readln(UCode);
          end
          else begin
            // Get corresponding character (from array)
            Code1 := '';
            for I := 1 to 6 do
              if Morses[R, I] <> ' ' then        // trim trailing spaces
                Code1 := Code1 + Morses[R, I];
            Code2 := Chars[R];
            Writeln('Morse code : ', Code1);
            // Get corresponding character (from user)
            Write('Character  ? '); Readln(UCode);
            UCode := UpCase(UCode);
          end;
          if UCode <> '' then begin
            Inc(N);
            GotoXY(1, 18); ClrEoL;
            GotoXY(1, 18);
            // Check user answer and display adequate message
            if UCode = Code2 then begin
               TextColor(LightGreen);
               Writeln(Code1, ' => ', Code2, ' is correct!');
               TextColor(White);
               Done[R] := True;
               Inc(C);
            end
            else begin
              TextColor(LightRed);
              Writeln('False! Correct answer is: ', Code1, ' => ', Code2);
              TextColor(White);
              end;
          end;
        end;
      until (not Done[R]) or (N = T0);
    until UCode = '';
    // Calculate and display percentage of correct answers
    P := (100 * C) div N;
    if P >= 60 then
      TextColor(LightGreen)
    else if P >= 50 then
      TextColor(Yellow)
    else
      TextColor(LightRed);
    GotoXY(1, 15); ClrEoL; GotoXY(1, 16); ClrEoL; GotoXY(1, 18); ClrEoL;
    GotoXY(1, 15);
    Writeln('Correct answers : ', C, ' / ', N, ' = ', P, ' %');
    TextColor(White);
    Writeln; Write('Hit any key to continue...');
    repeat until KeyPressed; Key := ReadKey; if Ord(Key) = 0 then Key := ReadKey;
  end;

{ Main program }

begin
  Randomize;
  Window(1, 1, 80, 50);
  TextBackground(Black); TextColor(White); ClrScr;
  // Initialize variables
  for I := 1 to MaxChars do
    begin
      Characters[I] := ' ';
      for J := 1 to 6 do
        MorseCodes[I, J] := ' ';
    end;
  Letters := 0; Numbers := 0; Punctuation := 0; TotalChars := 0;
  // Read morse codes from file
  ReadMorseCodes(Characters, MorseCodes, Letters, Numbers, Punctuation, TotalChars);
  // Display, translate, test or exit program
  repeat
    ClrScr;
    GotoXY(34, 1); Writeln('MORSE CODES');
    GotoXY(34, 2); Writeln('===========');
    Writeln;
    Writeln('Program options:'); Writeln;
    Writeln('  1 = Display morse codes list');
    Writeln('  2 = Translate character to morse code');
    Writeln('  3 = Translate morse code to character');
    Writeln('  4 = Translate text to morse codes');
    Writeln('  5 = Translate morse codes to text');
    Writeln('  6 = Knowledge test (character to morse code)');
    Writeln('  7 = Knowledge test (morse code to character)');
    Writeln('  9 = Exit program');
    Writeln;
    Write('  Your choice ? '); Readln(S);
    if (Length(S) = 0) or not (S[1] in ['1' .. '9']) then
      Choice := '9'
    else
      Choice := S[1];
    case Choice of
      '1' : DisplayMorseCodes(Characters, MorseCodes, Letters, Numbers, Punctuation, TotalChars);
      '2' : TranslateChar(Characters, MorseCodes, TotalChars, 'character');
      '3' : TranslateChar(Characters, MorseCodes, TotalChars, 'morse code');
      '4' : TranslateText(Characters, MorseCodes, TotalChars, 'text');
      '5' : TranslateText(Characters, MorseCodes, TotalChars, 'morse codes');
      '6' : MorseTest(Characters, MorseCodes, Letters, Numbers, TotalChars, 'character');
      '7' : MorseTest(Characters, MorseCodes, Letters, Numbers, TotalChars, 'morse code');
    end;
  until Choice = '9';
end.

