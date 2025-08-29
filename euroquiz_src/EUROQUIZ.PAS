program euroquiz;

uses
  Crt, Mouse;

const
  Countries: array[0..47] of string = (
    'Albania', 'Andorra', 'Armenia', 'Austria', 'Azerbaijan', 'Belarus', 'Belgium',
    'Bosnia and Herzegovina', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic',
    'Denmark', 'Estonia', 'Finland', 'France', 'Georgia', 'Germany', 'Greece',
    'Holy See', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Kazakhstan', 'Latvia',
    'Liechtenstein', 'Lithuania', 'Luxembourg', 'Malta', 'Moldova', 'Monaco',
    'Montenegro', 'Netherlands', 'North Macedonia', 'Norway', 'Poland', 'Portugal',
    'Romania', 'San Marino', 'Serbia', 'Slovakia', 'Slovenia',
    'Spain', 'Sweden', 'Switzerland', 'Ukraine', 'United Kingdom'
  );
  Capitals: array[0..47] of string = (
    'Tirana', 'Andorra la Vella', 'Yerevan', 'Vienna', 'Baku', 'Minsk', 'Brussels',
    'Sarajevo', 'Sofia', 'Zagreb', 'Nicosia', 'Prague',
    'Copenhagen', 'Tallinn', 'Helsinki', 'Paris', 'Tbilisi', 'Berlin', 'Athens',
    'Vatican City', 'Budapest', 'Reykjavik', 'Dublin', 'Rome', 'Nur-Sultan', 'Riga',
    'Vaduz', 'Vilnius', 'Luxembourg', 'Valletta', 'Chisinau', 'Monaco',
    'Podgorica', 'Amsterdam', 'Skopje', 'Oslo', 'Warsaw', 'Lisbon',
    'Bucharest', 'City of San Marino', 'Belgrade', 'Bratislava', 'Ljubljana',
    'Madrid', 'Stockholm', 'Berne', 'Kyiv', 'London'
  );

var
  Q, C, R, M, N, I: Integer;
  Buttons: Byte;
  X, Y: Word;
  Country, Capital: string;
  ButtonPushed, PExit: Boolean;
  CountriesDone, CapitalsDone: array[0..46] of Boolean;

begin
  ClrScr; CursorOff;
  TextColor(Yellow);
  Writeln('European capitals quiz.');
  Writeln('=======================');
  Writeln;
  TextColor(LightGray);
  Randomize;
  InitMouse;
  Buttons := DetectMouse;
  if Buttons = 0 then
    Writeln('Error: No mouse found!')
  else begin
    ShowMouse;
    GotoXY(1, 6); Write('Answer:   ');
    TextColor(Yellow);
    TextBackground(Blue); Write('Yes');
    TextBackground(Black); Write('   ');
    TextBackground(Blue); Write('No');
    TextBackground(Black); Write('   ');
    TextBackground(Blue); Write('Next');
    TextBackground(Black); Write('   ');
    TextBackground(Blue); Write('Exit');
    TextBackground(Black); TextColor(LightGray);
    Q := 0; C := 0; PExit := False;
    for I := 0 to 46 do begin
      CountriesDone[I] := False; CapitalsDone[I] := False;
    end;
    while (Q < 20) and (not PExit) do begin
      Inc(Q);
      repeat
        M := Random(46);
      until not CountriesDone[M];
      Country := Countries[M]; CountriesDone[M] := True;
      R := Random(2);
      if R = 0 then begin
        if CapitalsDone[M] then
          R := 1
        else
          Capital := Capitals[M];
      end;
      if R = 1 then begin
        repeat
          N := Random(46);
        until (N <> M) and (not CapitalsDone[N]);
        Capital := Capitals[N]; CapitalsDone[N] := True;
      end;
      GotoXY(1, 4); ClrEoL;
      Write('Question ', Q, ': The capital of ', Country, ' is ', Capital, '?');
      GotoXY(1, 8); ClrEoL;
      ButtonPushed := False;
      repeat
        repeat
        until GetMouseButtons = MouseLeftButton;
        X := GetMouseX; Y := GetMouseY;
        repeat
        until GetMouseButtons <> MouseLeftButton;
        if (Y = 6) and (X in [29..32]) then begin
          ButtonPushed := True; PExit := True;
        end
        else if (Y = 6) and (X in [11..13, 17..18]) then begin
          ButtonPushed := True;
          GotoXY(1, 8); Write('This answer is ');
          if X in [11..14] then begin
            if R = 0 then begin
              TextColor(LightGreen); Write('correct!');
              Inc(C);
            end
            else begin
              TextColor(LightRed); Write('false!');
            end;
          end
          else begin
            if R = 1 then begin
              TextColor(LightGreen); Write('correct!');
              Inc(C);
            end
            else begin
              TextColor(LightRed); Write('false!');
            end;
          end;
          TextColor(LightGray);
          if Q < 20 then begin
            repeat
              repeat
              until GetMouseButtons = MouseLeftButton;
              X := GetMouseX; Y := GetMouseY;
              repeat
              until GetMouseButtons <> MouseLeftButton;
            until (Y = 6) and (X in [22..25, 29..32]);
            if X in [29..32] then
              PExit := True;
          end;
        end;
      until ButtonPushed;
    end;
  end;
  DoneMouse;
  GotoXY(1, 10);
  if not PExit then begin
    Writeln('You got ', C, ' correct answers out of 20.');
    Writeln('That is a success rate of ', 100*(C/Q):0:2, '%.');
    Writeln;
  end;
  CursorOn;
end.
