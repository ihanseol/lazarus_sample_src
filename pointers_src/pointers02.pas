program pointers02;

var
  N1, N2: Integer;
  IPtr, IPtr1, IPtr2: ^Integer;

begin
  N1 := 100; N2 := 200;
  IPtr1 := @N1; IPtr2 := @N2;
  Writeln(IPtr1^, ' ', IPtr2^);
  IPtr := IPtr1;
  IPtr^ += IPtr2^;
  Writeln(IPtr1^, ' ', IPtr2^, ' ', IPtr^);
  IPtr^ := 500;
  Writeln(IPtr1^, ' ', IPtr2^, ' ', IPtr^);
  IPtr := IPtr2;
  Writeln(IPtr1^, ' ', IPtr2^, ' ', IPtr^);
  Writeln(N1, ' ', N2);
  Writeln; Write('ENTER to terminate '); Readln;
end.
