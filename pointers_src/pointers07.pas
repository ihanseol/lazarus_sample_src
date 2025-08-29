program pointers07;

type
  TEmployee = record
    Name, FirstName: string;
    Salary: Single;
  end;

var
  RPtr: ^TEmployee;

begin
  New (RPtr);
  RPtr^.Name := 'Flintstone';
  RPtr^.FirstName := 'Fred';
  RPtr^.Salary := 2400;
  Writeln('The salary of ', RPtr^.FirstName, ' ', RPtr^.Name, ' is ', RPtr^.Salary:0:2, '$');
  Dispose (RPtr);
  Writeln; Write('ENTER to terminate '); Readln;
end.
