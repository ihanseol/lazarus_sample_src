program pointers03;

var
  IPtr, IPtr1, IPtr2: ^Integer;

begin
  New(IPtr); New(IPtr1); New(IPtr2);
  IPtr1^ := 100; IPtr2^ := 200;
  Writeln(IPtr1^, ' ', IPtr2^);
  IPtr := IPtr1;
  IPtr^ += IPtr2^;
  Writeln(IPtr1^, ' ', IPtr2^, ' ', IPtr^);
  IPtr^ := 500;
  Writeln(IPtr1^, ' ', IPtr2^, ' ', IPtr^);
  IPtr := IPtr2;
  Writeln(IPtr1^, ' ', IPtr2^, ' ', IPtr^);
  Writeln; Write('ENTER to terminate '); Readln;
end.
