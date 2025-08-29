program pointers06;

var
  I: Integer;
  D: array[1..5] of Double = (1.1, 2.2, 3.3, 4.4, 5.5);
  DPtr: ^Double;

begin
  // Point to the last element of the array
  DPtr := @D[5];
  // Print out of the values using the pointer decremented at each iteration by 2
  I := 5;
  while I >= 1 do begin
    Writeln(' Value of D[', I, '] = ' , DPtr^ );
    I -= 2;
    Dec(DPtr, 2);
  end;
  Writeln; Write('ENTER to terminate '); Readln;
end.
