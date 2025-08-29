{**************************************************}
{*       Common unit for Morse2 application       *}
{* (contains all morse code relevant programming) *}
{**************************************************}

unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

type
  TMorseAlphabet = array[32 .. 90] of string;

const
  Dot = '.';                                                                   // dot (short signal) symbol
  Dash = '-';                                                                  // dash (long signal) symbol
  CharSeparator = ' ';                                                         // separator between 2 morse characters
  WordSeparator = '/ ';                                                        // separator between 2 (morse code) words
  DotDuration = 100;                                                           // duration of a dot signal (= base for all other durations)
  DashDuration = 3 * DotDuration;                                              // duration of a dash signal
  LDashDuration = 4 * DotDuration;                                             // longer dash signal duration (if selected in "Settings")
  CompSpaceDuration = DotDuration;                                             // pause between components of morse character (equal to dot signal duration)
  CharSpaceDuration = 3 * CompSpaceDuration;                                   // pause between 2 morse characters
  LCharSpaceDuration = 4 * CompSpaceDuration;                                  // longer pause between 2 characters (if 'longer dash duration' selected in "Settings")
  WordSpaceDuration = 7 * CompSpaceDuration;                                   // pause between 2 (morse code) words
  LWordSpaceDuration = 10 * CompSpaceDuration;                                 // longer pause between 2 (morse code) words (if selected in Settings)
  Frequency = 900;                                                             // frequancy for the Windows Beep command

function MorseToChar(S: string; var MorseAlphabet: TMorseAlphabet): Char;
function CharToMorse(Ch: Char; var MorseAlphabet: TMorseAlphabet): string;
function TextToMorse(Txt: AnsiString; var MorseAlphabet: TMorseAlphabet): AnsiString;
function MorseToText(Mrse: AnsiString; var MorseAlphabet: TMorseAlphabet): AnsiString;
procedure AdjustMorseCode(var MorseCode: AnsiString; out Mess: string);
procedure PlayMorseChar(var MorseCode: AnsiString; LDash, LWordSep: Boolean; out Pause: Integer);

implementation

{ Text character to morse code character conversion }

function CharToMorse(Ch: Char; var MorseAlphabet: TMorseAlphabet): string;

var
  S: string;

begin
  S := '?';
  Ch := UpperCase(Ch)[1];
  if (Ord(Ch) >= 32) and (Ord(Ch) <= 90) then
    S := MorseAlphabet[Ord(Ch)];
  CharToMorse := S;
end;

{ Morse code character to text character conversion }

function MorseToChar(S: string; var MorseAlphabet: TMorseAlphabet): Char;

var
  I: Integer;
  Ch: Char;
  OK: Boolean;

begin
  Ch := '?'; OK := True;
  for I := 1 to Length(S) do begin
    if (S[I] <> '.') and (S[I] <> '-') then
      OK := False;
  end;
  if OK then begin
    for I := 32 to 90 do begin
      if S = MorseAlphabet[I] then
        Ch := Chr(I);
    end;
  end;
  MorseToChar := Ch;
end;

{ Text to morse code translation }

function TextToMorse(Txt: AnsiString; var MorseAlphabet: TMorseAlphabet): AnsiString;

// Multi-line translation, conserving original linebreaks

var
  P, I: Integer;
  Mrse: AnsiString;

begin
  // Remove linebreaks and spaces at beginning of text
  repeat
    P := Pos(LineEnding, Txt);
    if P = 1 then
      Txt := StringReplace(Txt, LineEnding, '', []);
  until P <> 1;
  repeat
    P := Pos(' ', Txt);
    if P = 1 then
      Txt := StringReplace(Txt, ' ', '', []);
  until P <> 1;
  // Eliminate empty lines
  Txt := StringReplace(Txt, ' ' + LineEnding, LineEnding, [rfReplaceAll]);
  // Be sure there is only 1 space between words
  Txt := StringReplace(Txt, '  ', ' ', [rfReplaceAll]);
  // Use '#' to code for linebreak
  Txt := StringReplace(Txt, LineEnding, '#', [rfReplaceAll]);
  Mrse := '';
  for I := 1 to Length(Txt) do begin
    if Txt[I] = '#' then
      // Linebreak
      Mrse += WordSeparator + LineEnding
    else if Txt[I] = ' ' then begin
      // Word separator
      Mrse += WordSeparator;
    end
    else
      // Text character (to convert to morse code character; will give '?' if unknown)
      Mrse += CharToMorse(Txt[I], MorseAlphabet) + ' ';
  end;
  // Add word separator at end of morse code string
  if RightStr(Mrse, Length(WordSeparator)) <> WordSeparator then
    Mrse += WordSeparator;
  // Remove some inadequate characters
  Mrse := StringReplace(Mrse, WordSeparator + WordSeparator, WordSeparator, [rfReplaceAll]);
  Mrse := StringReplace(Mrse, LineEnding + WordSeparator, '', [rfReplaceAll]);
  TextToMorse := Mrse;
end;

{ Morse code to text conversion }

function MorseToText(Mrse: AnsiString; var MorseAlphabet: TMorseAlphabet): AnsiString;

var
  P: Integer;
  Mrse0, Mrse1, Mrse2, WordSpace, Mess: string;
  Txt: AnsiString;

begin
  // Be sure morse code has format as expected by the routine
  AdjustMorseCode(Mrse, Mess);
  // Use word separator without spaces ('/ ' -> '/')
  Txt := ''; WordSpace := StringReplace(WordSeparator, ' ', '', [rfReplaceAll]);
  // Parse the morse code until all dots/dashes/separators have been done
  repeat
    // 1. Search for lines
    P := Pos('#', Mrse);
    if P = 0 then begin
      Mrse1 := Mrse; Mrse := '';
    end
    else begin
      Mrse1 := LeftStr(Mrse, P - 1); Delete(Mrse, 1, P);
    end;
    repeat
      // 2. In a line, search for words
      P := Pos(WordSpace, Mrse1);
      if P = 0 then begin
        Mrse2 := Mrse1; Mrse1 := '';
      end
      else begin
        Mrse2 := LeftStr(Mrse1, P - 1); Delete(Mrse1, 1, P);
      end;
      repeat
        // 3. In a word, search for morse code characters
        Mrse2 := StringReplace(Mrse2, '  ', ' ', [rfReplaceAll]);              // this allows to correctly convert if user used 2 instead of 1 space
        P := Pos(CharSeparator, Mrse2);
        if P = 0 then begin
          Mrse0 := Mrse2; Mrse2 := '';
        end
        else begin
          Mrse0 := LeftStr(Mrse2, P - 1); Delete(Mrse2, 1, P);
        end;
        // Convert the morse code character and add letter/number/punctuation/? to text
        Txt += MorseToChar(Mrse0, MorseAlphabet);
      until Mrse2 = '';
      Txt += ' ';
    until Mrse1 = '';
    Txt += LineEnding;
  until Mrse = '';
  MorseToText := Txt;
end;

{ Check user morse code and give it the format expected by the MorseToText and PlayMorseChar routines }

procedure AdjustMorseCode(var MorseCode: AnsiString; out Mess: string);

// If morse code invalid, Mess will contain error message (otherwise empty)

var
  P, I: Integer;
  WordSpace, Ch: string;
  OK: Boolean;

begin
  WordSpace := StringReplace(WordSeparator, ' ', '', [rfReplaceAll]); Mess := '';
  // Remove linebreaks, word separators and spaces at beginning of text
  repeat
    P := Pos(LineEnding, MorseCode);
    if P = 1 then
      MorseCode := StringReplace(MorseCode, LineEnding, '', []);
  until P <> 1;
  repeat
    P := Pos(WordSeparator, MorseCode);
    if P = 1 then
      MorseCode := StringReplace(MorseCode, WordSeparator, '', []);
  until P <> 1;
  repeat
    P := Pos(' ', MorseCode);
    if P = 1 then
      MorseCode := StringReplace(MorseCode, ' ', '', []);
  until P <> 1;
  // Format the morse code (in particular, use '#' for line-ending)
  MorseCode := StringReplace(MorseCode, LineEnding, WordSeparator + '#', [rfReplaceAll]);
  MorseCode := StringReplace(MorseCode, WordSeparator + WordSeparator, WordSeparator, [rfReplaceAll]);
  MorseCode := StringReplace(MorseCode, '  ', ' ', [rfReplaceAll]);
  I := 0;
  // Check the morse code string
  repeat
    Inc(I);
    Ch := Copy(MorseCode, I, 1);
    if (Ch = Dot) or (Ch = Dash) or (Ch = CharSeparator) then
      // Dot, dash or character separator
      OK := True
    else begin
      OK := False;
      if ((Ch = WordSpace) and (Copy(MorseCode, I, Length(WordSeparator)) = WordSeparator)) or (Ch = '#') then
        // Word separator ('/ ') or end of line code ('#')
        OK := True;
    end;
  until (I >= Length(MorseCode)) or not OK;
  // Use word separator without spaces ('/ ' -> '/')
  MorseCode := StringReplace(MorseCode, WordSeparator, WordSpace, [rfReplaceAll]);
  if not OK then
    Mess := 'Morse code contains invalid characters!';
end;

{ Play (audio output beep signal) morse code symbol (dot or dash) }

procedure PlayMorseChar(var MorseCode: AnsiString; LDash, LWordSep: Boolean; out Pause: Integer);

var
  DDash, DCharSpace, DWordSpace: Integer;
  WordSpace, Ch: string;

// The routine plays the first dot/dash of the complete morse code given as argument. The symbol played, plus eventual separators are
// removed from the string and the pause, as applicable, is returned to the calling routine. This allows to play any length multi-line
// morse code, using a simple timer routine (cf. 'translation' unit).

begin
  DDash := DashDuration; DCharSpace := CharSpaceDuration; DWordSpace := WordSpaceDuration;
  WordSpace := StringReplace(WordSeparator, ' ', '', [rfReplaceAll]);
  // Adapt dash duration (and with it morse separation duration) and word separation duration (if these options have been selected)
  if LDash then begin
    DDash := LDashDuration;
    DCharSpace := LCharSpaceDuration;
  end;
  if LWordSep then
    DWordSpace := LWordSpaceDuration;
  // Play dot/dash and determine pause duration
  if Length(MorseCode) >= 1 then begin
    Ch := LeftStr(MorseCode, 1); Delete(MorseCode, 1, 1);
    if Ch = Dot then
      // Play a dot
      Beep(Frequency, DotDuration)
    else if Ch = Dash then
      // Play a dash
      Beep(Frequency, DDash);
    if Length(MorseCode) >= 1 then begin
      Ch := LeftStr(MorseCode, 1);
      if (Ch = CharSeparator) or (Ch = WordSpace) then begin
        // If character following the dot/dash is a character or word separator, set the corr. pause
        if Ch = CharSeparator then
          Pause := DCharSpace
        else
          Pause := DWordSpace;
        Delete(MorseCode, 1, 1);
      end
      else
        // No separator, thus stay in same morse character and pause is the one between 2 character components
        Pause := CompSpaceDuration;
    end;
  end;
end;

end.

