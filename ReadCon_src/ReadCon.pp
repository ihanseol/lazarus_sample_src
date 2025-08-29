{********************************************************************}
{*                     Read from console unit                       *}
{* ---------------------------------------------------------------- *}
{* Simple procedures to read a string, real or integer from console *}
{* The routines may in particular be used to replace Readln in OS/2 *}
{* programs using the CRT unit (that does not work correctly)       *}
{* Version 1.0, (c) allu, July 2023                                 *}
{********************************************************************}

unit ReadCon;

{$mode objfpc}{$H+}

interface

uses
  Crt;

const
  Key_NULL  = #0;                           // Null character
  Key_BS    = #8;                           // Backspace key
  Key_ENTER = #13;                          // Enter key
  Key_ESC   = #27;                          // Escape key

procedure ReadString(out S: string);
procedure ReadReal(out R: Real; out Empty: Boolean; out Err: string);
procedure ReadInteger(out I: Integer; out Empty: Boolean; out Err: string);

implementation

const
  ErrMess: array[0..1] of string = (
    'Invalid number',
    'Invalid integer'
  );

{* ------------------------ *}
{* Read string from console *}
{* ------------------------ *}

procedure ReadString(out S: string);

// The procedure reads a string terminated by ENTER (as with Readln, the cursor passes to column 1 of the next line).
// Characters accepted are ASCII 32 to 126. They are displayed with current text attributes at the current cursor position.
// The BACKSPACE key may be used to clear the last character entered. The ESCAPE key may be used to abort the program.

var
  Count: Integer;
  Key: Char;

begin
  S := ''; Count := 0;
  repeat
    // Do this until the ENTER key (or the ESCAPE key) has been pressed
    Key := #255;
    if KeyPressed then begin
      // Do this if a key has been pressed on the keyboard
      Key := ReadKey;                       // read the key that has been pressed
      if Key = Key_NULL then                // check if it's a control key
        Key := ReadKey                      // if yes, read second byte for this key
      else begin
        // Do this for regular (non control keys)
        if Ord(Key) in [32..126] then begin
          // Key is a (printable) character: Print it out and add it to the input string
          Write(Key);
          S := S + Key;
          Inc(Count);                       // count characters that have been entered (string length)
        end
        else if Key = Key_ENTER then begin
          // Key is ENTER: Move the cursor to column 1 of next line (this will also terminate input)
          Writeln;
        end
        else if Key = Key_BS then begin
          // Key is BACKSPACE: Clear last character entered
          if Count > 0 then begin
            // This may only be done if there has been some character(s) entered
            GotoXY(WhereX - 1, WhereY);     // move cursor one position to the left
            Write(' ');                     // print out a space
            GotoXY(WhereX - 1, WhereY);     // cursor one position to the left again
            Delete(S, Length(S), 1);        // remove last character from the input string
            Dec(Count);                     // adjust number of characters entered (string length)
          end;
        end
        else if Key = Key_ESC then begin
          // Key is ESCAPE: Abort the program (may be used instead of CTRL+BREAK)
          Halt;
        end;
      end;
    end;
  until Key = Key_Enter;
end;

{* ----------------------------- *}
{* Read real number from console *}
{* ----------------------------- *}

procedure ReadReal(out R: Real; out Empty: Boolean; out Err: string);

// The procedure calls ReadString to read a string terminated by ENTER (the ESCAPE key terminating the program).
// If the string is empty (ENTER has been pressed without entering any character), the boolean "Empty" is set to True.
// This allows to use the procedure with no value entered (for example to use a default value in this case).
// The procedure then checks if the input is a number; if not the error message variable "Err" is filled with "ErrMess[0]".

var
  E: Integer;
  S: string;

begin
  R := 0; Empty := False; Err := '';
  // Read input as a string
  ReadString(S);
  // Check if input string is empty (ENTER pressed without entering any character)
  if S = '' then begin
    // Empty string: Set "Empty" variable to True
    Empty := True;
  end
  // Check if input string is numeric (is a real number)
  else begin
    Val(S, R, E);
    if E <> 0 then begin
      // Non-numeric string: Fill error message variable
      Err := ErrMess[0];
    end;
  end;
end;

{* -------------------------------- *}
{* Read integer number from console *}
{* -------------------------------- *}

procedure ReadInteger(out I: Integer; out Empty: Boolean; out Err: string);

// The procedure calls ReadReal to read a real number terminated by ENTER (the ESCAPE key terminating the program).
// If nothing has been entered, the boolean "Empty" has been set to True, if the input isn't a number the error
// message variable "Err" has been filled with "ErrMess[0]". If neither of this is the case, the procedure checks
// if the number entered is well an integer. If yes, it is returned; if not, "Err" is set to "ErrMess[0]".

var
  R: Real;

begin
  I := 0; Empty := False; Err := '';
  // Read input as a real number
  ReadReal(R, Empty, Err);
  if (Err = '') and (not Empty) then begin
    // If the input is neither empty, nor a non-nueric value, check if the number is an integer
    if R = Int(R) then begin
      // Input number is an integer: return it
      I := Round(R);
    end
    else begin
      // Input number isn't an integer: Fill error message variable
      Err := ErrMess[1];
    end;
  end;
end;

end.
