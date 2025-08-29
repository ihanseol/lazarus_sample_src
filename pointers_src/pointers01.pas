program pointers01;

var
  N: Integer;
  IPtr: ^Integer;
  
begin
  N := 24;
  IPtr := @N;
  Writeln(N, ' ', IPtr^);
  Writeln; Write('ENTER to terminate '); Readln;
end.
