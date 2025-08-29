program integration2;

uses
  Integration;

var
  A, B: Real;
  SFunction: string;
  OK: Boolean;

begin
  SFunction := 'sin(x)'; A := -2*Pi; B := 2*Pi;
  OK := IntegralArea(SFunction, A, B);
  if OK then
    Writeln('Graph successfully created in plot.png')
  else
    Writeln('Graph creation failed!');
  Writeln;
  Write('Enter to terminate... '); Readln;
end.

