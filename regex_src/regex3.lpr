program regex3;

uses
  SysUtils, RegExpr;

var
  N: Integer;
  Expr, Mess: string;
  Regex: TRegExpr;

procedure Calculate(Op: string; var N1: Integer; N2: Integer; out Mess: string);

begin
  Mess := '';
  case Op[1] of
    '+': N1 += N2;
    '-': N1 -= N2;
    '*': N1 *= N2;
    '/': begin
      if N2 = 0 then
        Mess := 'Division by zero'
      else
        N1 := N1 div N2
    end;
  end;
end;

begin
  Regex := TRegExpr.Create;
  Writeln; Writeln('Parse an arithmetic expression (positive integers).'); Writeln;
  repeat
    Write('Enter expression (ENTER to terminate)? '); Readln(Expr);
    Regex.Expression := '^(\d+)([\+\-\*\/]\d+)+$';
    if Regex.Exec(Expr) then begin
      N := StrToInt(Regex.Match[1]);
      Regex.Expression := '([\+\-\*\/])(\d+)';
      if Regex.Exec(Expr) then begin
        Calculate(Regex.Match[1], N, StrToInt(Regex.Match[2]), Mess);
        while (Mess = '') and Regex.ExecNext do
          Calculate(Regex.Match[1], N, StrToInt(Regex.Match[2]), Mess);
      end;
      if Mess = '' then
        Writeln('  Result of arithmetic expression is ', N)
      else
        Writeln('  Input error: ', Mess, '!');
    end
    else begin
      if Expr <> '' then
        Writeln('  This is not a positive integer arithmetic expression!');
    end;
    Writeln;
  until Expr = '';
  Regex.Free;
end.

