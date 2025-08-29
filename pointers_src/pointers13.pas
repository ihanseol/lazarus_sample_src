program pointers13;

type
  TIPtr = ^Integer;

var
  I: Integer;
  Numbers: array[1..10] of Integer;
  IPtr: TIptr;

// Print procedure
procedure PrintNumbers(PI: TIPtr);
begin
  for I := 1 to 10 do begin
    Write(PI^, ' ');
    Inc(PI);
  end;
end;

// Main program
begin
  for I := 1 to 10 do
    Numbers[I] := 2 * I;
  IPtr := @Numbers[1];
  PrintNumbers(IPtr);
  Writeln; Write('ENTER to terminate'); Readln;
end.
