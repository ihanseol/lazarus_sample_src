program pointers15;

type
  TArrayPtr = ^TArray;
  TArray = Array[0..25000] of LongInt;

var
  M, N, R, I: Integer;
  Start, Alloc: Boolean;
  APtr: TArrayPtr;

begin
  Randomize;
  Start := True; Alloc := False;
  repeat
    Write('Random numbers from 0 to 99 - how many do you want? '); Readln(M);
    if M > 0 then begin
      N := M;
      if Start then begin
        GetMem(APtr, N * SizeOf(LongInt));
        Start := False;
      end
      else begin
        ReallocMem(APtr, N * SizeOf(LongInt));
      end;
      Alloc := True;
      for I := 0 to N - 1 do
        APtr^[I] := Random(100);
      if N > 20 then begin
        Writeln('The 20 first random numbers are: ');
        R := 20;
      end
      else begin
        Writeln('The random numbers are: ');
        R := N;
      end;
      for I := 0 to R - 1 do
        Write (APtr^[I], ' ');
      Writeln;
    end;
  until M <= 0;
  if Alloc then
    FreeMem(APtr, N * SizeOf(LongInt));
  Writeln; Write('ENTER to terminate '); Readln;
end.
