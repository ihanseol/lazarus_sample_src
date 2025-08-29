{***************************************************}
{* DLL test: Call some procedures of DLL "display" *}
{***************************************************}

program test_display;

{$mode objfpc}{$H+}

{$LINKLIB display}

uses
  ctypes;

const
  Black = 0; Yellow = 14; DarkGray = 8;
  Title: PChar = 'Test DISPLAY unit.';
  EndOfProg: PChar = 'Enter to terminate ';
  Name: PChar = 'Aly';
  Arr: array[0..9] of Real = (
    1, 1.25, 1.50, 2, 2.25, 2.50, 3, 3.25, 3.50, 4
  );

var
  I: Integer;

{ Declaration of the external (DLL) procedures used }

procedure ClearScreen; cdecl; external;
procedure WriteEol; cdecl; external;
procedure SetColor(FG: cint32; BG: cint32 = Black); cdecl; external;
procedure ResetColor; cdecl; external;
procedure SetPos(Y, X: cint32); cdecl; external;
procedure WriteString(S: PChar; Align: Char = 'l'; Width: cint32 = 0); cdecl; external;
procedure WriteChar(C: Char; Count: cint32); cdecl; external;
procedure WriteFloat(R: cfloat; Width: cint32 = 0; Digits: cint32 = 0); cdecl; external;
procedure WriteColor(S: PChar; FG: cint32; BG: cint32 = Black);  cdecl; external;
procedure WritePos(S: PChar; Y, X: cint32);  cdecl; external;

{ Main program }

begin
  ClearScreen;
  // Display colored text (underlined with '=' signs) at given screen position
  SetColor(Yellow);
  WritePos(Title, 1, 21);
  SetPos(2, 21);
  WriteChar('=', 18);
  ResetColor; WriteEol; WriteEol;
  // String alignment (left, right, centered)
  WriteChar('-', 10); WriteEol;
  WriteString(Name); WriteEol;
  WriteString(Name, 'r', 10); WriteEol;
  WriteString(Name, 'c', 10); WriteEol; WriteEol;
  // Formatted display of real numbers
  for I := 0 to 9 do begin
    WriteFloat(Arr[I], 0, 2);
    WriteChar(' ', 2);
  end;
  WriteEol; WriteEol;
  // Colored string display
  WriteColor(EndOfProg, DarkGray);
  // Wait for user hitting ENTER key (and terminate the program)
  Readln;
end.

