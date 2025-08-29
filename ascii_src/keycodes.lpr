{*************************************************}
{* Display ASCII code of key pressed on keyboard *}
{*************************************************}

program keycodes;

uses Crt;

const
  Key_NULL = #0;
  Key_ESC  = #27;

var
  Key: Char;

begin
  ClrScr;
  Writeln('Display ASCII code of key pressed on keyboard');
  Writeln('---------------------------------------------');
  Writeln;
  Writeln('Hit the key to display the code of or ESC to terminate');
  Writeln;
  repeat
    Key := ReadKey;
    Write('ASCII code of key pressed is : ');
    if Key = Key_NULL then begin
      Write('0 + ');
      Key := ReadKey;
    end;
    Writeln(Ord(Key));
  until Key = Key_ESC;
end.

