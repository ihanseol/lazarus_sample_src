program regex2;

uses
  SysUtils, RegExpr;

const
  Words: array[0..8] of string = (
    'Any', 'any', 'many', 'NY', 'Ali', 'Bali', 'aua', 'AUA', 'Aua'
  );

var
  I: Integer;
  Contains, Upper, First, Last, All, AllUpper: Boolean;
  Regex: TRegExpr;

begin
  Regex := TRegExpr.Create;
  Writeln; Writeln('Test if an expression contains vowels.');
  for I := 0 to 8 do begin
    Regex.Expression := '(?i)[iueoa]';
    Contains := Regex.Exec(Words[I]);
    if Contains then begin
      Regex.Expression := '^(?i)[iueoa]';
      First := Regex.Exec(Words[I]);
      Regex.Expression := '(?i)[iueoa]$';
      Last := Regex.Exec(Words[I]);
      Regex.Expression := '^(?i)[iueoa]+$';
      All := Regex.Exec(Words[I]);
      Regex.Expression := '[IUEOA]';
      Upper := Regex.Exec(Words[I]);
      if Upper then begin
        Regex.Expression := '^[IUEOA]+$';
        AllUpper := Regex.Exec(Words[I]);
      end;
    end;
    Write(Words[I]:6);
    if Contains then begin
      if All then begin
        if AllUpper then
          Writeln('  contains exclusively uppercase vowels.')
        else if Upper then
          Writeln('  contains exclusively (uppercase and lowercase) vowels.')
        else
          Writeln('  contains exclusively lowercase vowels.');
      end
      else begin
        if Upper then
          Write('  contains one or more (uppercase or lowercase) vowels; ')
        else
          Write('  contains one or more (lowercase) vowels; ');
        if First and Last then
          Writeln('the first and last letter are vowels.')
        else if First then
          Writeln('the first letter is a vowel.')
        else if Last then
          Writeln('the last letter is a vowel.')
        else
          Writeln('neither the first, nor the last letter are vowels.');
      end;
    end
    else begin
      Writeln('  does NOT contain any vowels.');
    end;
  end;
  Regex.Free;
end.


