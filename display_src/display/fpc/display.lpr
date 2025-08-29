{*******************************************************}
{* DLL "display": Some useful cdecl display procedures *}
{* Version 1.0, (c) allu, April 2024                   *}
{*******************************************************}

library display;

{$mode objfpc}{$H+}

uses
  SysUtils, Crt, Strings, ctypes;

{ Clear the screen }

procedure ClearScreen; cdecl;

// Arguments: none

begin
  ClrScr;
end;

{ Clear end of line }

procedure ClearEol; cdecl;

// Arguments: none

begin
  ClrEol;
end;

{ Display end of line character = new line }

procedure WriteEol; cdecl;

// Arguments: none

begin
  Write(LineEnding);
end;

{ Repeated character display }

procedure WriteChar(C: Char; Count: cint32); cdecl;

// Arguments:
//   character to be displayed (Char)
//   repeat factor = count (CInt32)

var
  I: Integer;

begin
  for I := 1 to Count do
    Write(C);
end;

{ Formattded string display }

procedure WriteString(S: PChar; Align: Char = 'L'; Width: cint32 = 0); cdecl;

// Arguments:
//   string to be displayed (PChar)
//   alignment (Char): L (left; default), R (right), C (center)
//   display area width (CInt32); left/right/left+right padding with spaces; default = 0 (no padding)

var
  Len, Spaces, Spaces2: Integer;

begin
  Align := Uppercase(Align)[1];
  Len := StrLen(S);
  if Width >= 0 then begin
    Spaces := Width - Len;
    if Align = 'L' then begin
      // Left alignment
      Write(S);
      WriteChar(' ', Spaces);
    end
    else if Align = 'R' then begin
      // Right alignment
      WriteChar(' ', Spaces);
      Write(S);
    end
    else if Align = 'C' then begin
      // Centered
      Spaces2 := Spaces div 2;
      WriteChar(' ', Spaces2);
      Write(S);
      WriteChar(' ', Spaces2);
      if Spaces mod 2 = 1 then
        Write(' ');
    end;
  end;
end;

{ Formatted integer display }

procedure WriteInt(N: cint32; Width: cint32 = 0); cdecl;

// Arguments:
//   integer to be displayed (CInt32)
//   display area width (CInt32); left padding with spaces; default = 0 (no padding)

begin
  if Width >= 0 then
    Write(N:Width);
end;

{ Formatted float (real) display }

procedure WriteFloat(R: cfloat; Width: cint32 = 0; Digits: cint32 = 0); cdecl;

// Arguments:
//   float to be displayed (CFloat)
//   display area width (CInt32); left padding with spaces; default = 0 (no padding)
//   number of decimal digits (CInt32); default = 0

begin
  if (Width >= 0) and (Digits >= 0) then
    Write(R:Width:Digits);
end;

{ Set foreground (text) and background colors }

procedure SetColor(FG: cint32; BG: cint32 = Black); cdecl;

// Arguments:
//   foreground color (CInt32); Crt unit text color (0 - 15)
//   background color (CInt32); Crt unit background color (0 - 7); default = 0 (black)

begin
  if (FG >= 0) and (FG <= 15) and (BG >= 0) and (BG <= 7) then begin
    TextColor(FG); TextBackground(BG);
  end;
end;

{ Reset foreground (text) and background colors }

procedure ResetColor; cdecl;

// Arguments: none
// Text color is set to LightGray (7), background color to Black (0)

begin
  TextColor(LightGray); TextBackground(Black);
end;

{ Colored string display }

procedure WriteColor(S: PChar; FG: cint32; BG: cint32 = Black);  cdecl;

// Arguments:
//   string to be displayed (PChar)
//   foreground color (CInt32); Crt unit text color (0 - 15)
//   background color (CInt32); Crt unit background color (0 - 7); default = 0 (black)

begin
  SetColor(FG, BG);
  Write(S);
  ResetColor;
end;

{ Set cursor position }

procedure SetPos(Y, X: cint32); cdecl;

// Arguments:
//   screen row = Y (CInt32)
//   screen column = X (CInt32)

begin
  if (X > 0) and (Y > 0) then
    GotoXY(X, Y);
end;

{ Positional string display }

procedure WritePos(S: PChar; Y, X: cint32);  cdecl;

// Arguments:
//   string to be displayed (PChar)
//   screen position: row = Y (CInt32)
//   screen position: column = X (CInt32)

begin
  SetPos(Y, X);
  Write(S);
end;

{ Export the procedures (!) }

exports
  ClearScreen, ClearEol, WriteEol, WriteChar, WriteString, WriteInt, WriteFloat,
  SetColor, ResetColor, WriteColor, SetPos, WritePos;

end.

