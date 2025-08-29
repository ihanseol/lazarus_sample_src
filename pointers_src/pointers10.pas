program pointers10;

type
  TIptr  = ^Integer;
  TPIptr = ^TIptr;

var
  N: Integer;
  IPtr: TIptr;
  PIptr: TPIptr;

begin
  N := 200;
  IPtr   := @N;
  PIPtr := @IPtr;
  IPtr^ := IPtr^ div 2;
  Writeln('N = ', N, ', IPtr^ = ', IPtr^, ', PIPtr^^ = ', PIPtr^^);
  Writeln; Write('ENTER to terminate '); Readln;
end.
