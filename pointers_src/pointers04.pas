program pointers04;

var
  I: Integer;
  D: array[1..5] of Double = (1.1, 2.2, 3.3, 4.4, 5.5);
  DPtr: array[1..5] of ^Double;

begin
  // Fill the pointer array with the addresses of the array elements
  for I := 1 to 5 do
    DPtr[I] := @D[I];
  // Print out of the values using the pointer array
  for I := 1 to 5 do
    Writeln(' Value of D[', I, '] = ' , DPtr[I]^ );
  Writeln; Write('ENTER to terminate '); Readln;
end.
