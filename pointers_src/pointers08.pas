program pointers08;

type
  TArray = Array[0..9] of Integer;

var
  I: Integer;
  APtr: ^TArray;

begin
  New (APtr);
  for I := 0 to 9 do
    APtr^[I] := 200 * I;
  for I := 0 to 9 do
    Write (APtr^[I] div 2, ' ');
  Dispose (APtr);
  Writeln; Write('ENTER to terminate '); Readln;
end.
