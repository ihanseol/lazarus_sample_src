{*********************************************}
{* Print ASCII table (ASCII codes 32 to 126) *}
{*********************************************}

program ascii;

var
  I, C : Integer;

begin
  C := 0;
  Writeln;
  Writeln('ASCII codes table');
  Writeln('=================');
  Writeln;
  for I:= 32 to 126 do
    begin
      if I < 100 then                                                          // 3-digit numbers
        Write('0');
      Write(I, ' ', Chr(I));
      Inc(C);
      if C = 8 then                                                            // 8 colums per line
        begin
          Writeln;
          C := 0;
        end
      else
        Write('     ');
    end;
  Writeln; Writeln;
  Write('Hit ENTER to terminate the program'); Readln;
end.

