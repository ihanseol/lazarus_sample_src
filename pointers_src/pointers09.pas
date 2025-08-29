program pointers09;

var
    SPtr: ^string;

begin
  New (SPtr);
  SPtr^ := 'abcdefghijklmnopqrstuvwxyz';
  Writeln('First letter of string = ', SPtr^[1]);
  Writeln('10th  letter of string = ', SPtr^[10]);
  Writeln('Last  letter of string = ', SPtr^[Length(SPtr^)]);
  Dispose (SPtr);
  Writeln; Write('ENTER to terminate '); Readln;
end.
