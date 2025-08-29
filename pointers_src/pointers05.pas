program pointers05;

var
  I: Integer;
  D: array[1..5] of Double = (1.1, 2.2, 3.3, 4.4, 5.5);
  DPtr: ^Double;

begin
  // Point to the first element of the array
  DPtr := @D[1];
  // Print out of the values using the pointer incremented at each iteration
  for I := 1 to 5 do begin
    Writeln(' Value of D[', I, '] = ' , DPtr^ );
    Inc(DPtr);
  end;
  Writeln; Write('ENTER to terminate '); Readln;
end.
