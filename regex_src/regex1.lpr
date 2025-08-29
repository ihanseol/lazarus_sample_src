program regex1;

uses
  SysUtils, RegExpr;

const
  Strings: array[0..9] of string = (
    '123', '-123', '123-', '--123', '12.3', '12,3', '1,2,3', '-12.3', '-12,3', '-123,'
  );

var
  I: Integer;
  IsInteger, IsPosInteger, IsFloat: Boolean;
  Regex: TRegExpr;

function DecimalSeparator: Char;

var
  S: string;

begin
  S := FloatToStr(1.2);
  Result := Copy(S, 2, 1)[1];
end;

begin
  Regex := TRegExpr.Create;
  Writeln; Writeln('Test if an expression is numeric.');
  for I := 0 to 9 do begin
    Regex.Expression := '^-?\d+$';
    IsInteger := Regex.Exec(Strings[I]);
    if IsInteger then begin
      Regex.Expression := '^\d+$';
      IsPosInteger := Regex.Exec(Strings[I]);
    end
    else begin
      Regex.Expression := '^-?\d+' + DecimalSeparator + '\d+$';
      IsFloat := Regex.Exec(Strings[I]);
    end;
    Write(Strings[I]:7);
    if IsInteger or IsFloat then begin
      Write('  is a number; ');
      if IsInteger then begin
        if IsPosInteger then
          Writeln('it is a positive integer.')
        else
          Writeln('it is a (positive or negative) integer.')
      end
      else begin
        Writeln('it is a floating point number.');
      end;
    end
    else begin
      Writeln('  is NOT a valid number!');
    end;
  end;
  Regex.Free;
end.

