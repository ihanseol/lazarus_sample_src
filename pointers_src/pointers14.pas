program pointers14;

type
  TIPtr = ^Integer;

var
  I, J: Integer;
  IPtr: TIptr;

// Random numbers function
function RandomNumbers(N: Integer): TIPtr;

var
  I: Integer;
  Numbers: array of Integer;

begin
  SetLength(Numbers, N);
  for I := 0 to N - 1 do
    Numbers[I] := Random(100);
  RandomNumbers := @Numbers[0];
end;

// Main program
begin
  Randomize;
  for J := 1 to 3 do begin
    IPtr := RandomNumbers(J * 10);
    for I := 1 to J * 10 do begin
      Write(IPtr^:2, ' ');
      Inc(IPtr);
    end;
    Writeln;
  end;
  Writeln; Write('ENTER to terminate'); Readln;
end.
