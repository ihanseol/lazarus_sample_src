{ **************************************************************************** }
{                             Simple SNAKE program                             }
{                        (version 1.0, allu 02-06.2016)                        }
{ **************************************************************************** }

program Snake;

uses Crt;

// Constants (global use)

const
  LengthMin = 5;
  LengthMax = 40;
  SpeedMax  = 6;
  GoodMin   = 5;
  GoodMax   = 10;
  BadMax    = 20;

// Snake and meals type declarations

type
  TSnake = record
    Speed    : Integer;
    Length   : Integer;
    Elements : array[1 .. LengthMax] of record
      X, Y   : Integer;
      Color  : Integer;
    end;
  end;

  TMeal = record
    Effect  : Integer;
    X, Y    : Integer;
    IsEaten : Boolean;
  end;

  TMeals = record
    Good, Bad, GoodLeft : Integer;
    Meals : array[1 .. GoodMax + BadMax] of TMeal;
  end;

// Variables (local use)

var
  Speed   : Integer;
  MySnake : TSnake;
  MyMeals : TMeals;
  EndOfGame, ClearTail, ClearWindow : Boolean;

{ --------------------------- }
{ Set the title window active }
{ --------------------------- }

procedure TitleWindow;

  begin
    Window(1, 1, 80, 3);
    TextBackground(White);
    TextColor(Blue);
    ClrScr;
  end;

{ -------------------------- }
{ Set the game window active }
{ -------------------------- }

procedure GameWindow(Clear: Boolean);

  begin
    Window(2, 4, 79, 48);
    TextBackground(Black);
    TextColor(White);
    // Clear the game window if needed so
    if Clear then
      ClrScr;
  end;

{ -------------------------- }
{ Print the actual game info }
{ -------------------------- }

procedure GameInfo(S: TSnake; M: TMeals);

  var
    X0 : Integer;

  begin
    TitleWindow;
    GotoXY(35, 1); Write('S N A K E');
    GotoXY(2, 3); Write('Snake speed: ', S.Speed);
    // X-position of info depending on snake length and number of meals/meals left
    X0 := 32;
    if S.Length > 9 then
      Dec(X0);
    GotoXY(X0, 3); Write('Snake length: ', S.Length);
    X0 := 70;
    if M.Good > 9 then begin
      Dec(X0);
      if M.GoodLeft > 9 then;
        Dec(X0);
    end;
    GotoXY(X0, 3); Write('Meals: ', M.GoodLeft, '/', M.Good);
  end;

{ -------------- }
{ Init the snake }
{ -------------- }

procedure InitSnake(var S: TSnake; V, L: Integer);

  var
    I, C : Integer;

  begin
    // Init variable (set all to null)
    for I := 1 to LengthMax do begin
      S.Elements[I].X := 0;
      S.Elements[I].Y := 0;
      S.Elements[I].Color := 0;
    end;
    S.Speed := V;
    S.Length := L; C := Green;
    // Snake elements' color alters green and white
    for I := 1 to S.Length do begin
      S.Elements[I].Color := C;
      if C = Green then
        C := White
      else
        C := Green;
    end;
  end;

{ --------------- }
{ Reset the snake }
{ --------------- }

procedure ResetSnake(var S: TSnake);

  var
    I : Integer;

  begin
    // The snake starts at the left end (X) and the middle of the game window (Y)
    for I := 1 to S.Length do begin
      S.Elements[I].X := (S.Length + 1) - I;
      S.Elements[I].Y := 23;
    end;
  end;

{ -------------- }
{ Init the meals }
{ -------------- }

procedure InitMeals(var M: TMeals; GMin, GMax, BMax: Integer);

  var
    I, J, X, Y : Integer;
    MealOk : Boolean;

  begin
    // Init the variable (set all to null)
    for I := 1 to GMax + BMax do begin
      M.Meals[I].X := 0;
      M.Meals[I].Y := 0;
      M.Meals[I].Effect := 0;
      M.Meals[I].IsEaten := False;
    end;
    // Number of good/bad meals
    M.Good := Random(GMax - GMin + 1) + GMin;
    M.GoodLeft:= M.Good;
    M.Bad  := BMax;
    // Bad meals (position on game window), ignoring if 2 or more are at same position
    for I := M.Good + 1 to M.Good + M.Bad do begin
      X := Random(78) + 1;
      // Exlude "space directly around" the snake
      repeat
        Y := Random(45) + 1;
      until (Y < 22) or (Y > 24);
      M.Meals[I].X := X;
      M.Meals[I].Y := Y;
      M.Meals[I].Effect := 2;
      M.Meals[I].IsEaten := False;
    end;
    // Good meals (position on game window and type)
    for I := M.Good downto 1 do begin
      repeat
        MealOk := True;
        X := Random(78) + 1;
        repeat
          Y := Random(45) + 1;
        until (Y < 22) or (Y > 24);
        // Check if this position (and those directly around) is still empty
        for J := M.Good + M.Bad downto I + 1 do begin
          if (X = M.Meals[J].X) and (Abs(Y - M.Meals[J].Y) < 2) or (Y = M.Meals[J].Y)
            and (Abs(X - M.Meals[J].X) < 2) then
            MealOk := False;
        end;
      until MealOk;
      M.Meals[I].X := X;
      M.Meals[I].Y := Y;
      M.Meals[I].Effect := Random(2);
      M.Meals[I].IsEaten := False;
    end;
  end;

{ -------------- }
{ Draw the snake }
{ -------------- }

procedure DrawSnake(S: TSnake; TX, TY: Integer; CW, CT: Boolean);

  var
    I : Integer;

  begin
    GameWindow(CW);
    // Draw actual snake position (as green or white spaces)
    for I := 1 to S.Length do begin
      GotoXY(S.Elements[I].X, S.Elements[I].Y);
      TextBackground(S.Elements[I].Color);
      Write(' ');
    end;
    TextBackground(Black);
    // Clear position left by the snake (position where its tail was before the move)
    if CT then begin
      GotoXY(TX, TY);
      Write(' ');
    end;
  end;

{ -------------- }
{ Draw the meals }
{ -------------- }

procedure DrawMeals(M: TMeals);

  const
    Colors : array[0 .. 2] of Integer = (LightCyan, LightBlue, LightRed);

  var
    I : Integer;

  begin
    GameWindow(False);
    // Draw meals positions (as spaces with color corresponding to their effect)
    for I := 1 to M.Good + M.Bad do begin
      GotoXY(M.Meals[I].X, M.Meals[I].Y);
      TextBackground(Colors[M.Meals[I].Effect]);
      Write(' ');
    end;
    TextBackground(Black);
  end;

{ ----------- }
{ End of game }
{ ----------- }

procedure PEndOfGame(S: TSnake);

  var
    I : Integer;

  begin
    GameWindow(False);
    // Snake is maximum length (game won) : Blinking text message
    if S.Length = LengthMax then begin
      TextColor(Yellow);
      repeat
        GotoXY(23, 22); Write('*******************************');
        GotoXY(23, 23); Write('C O N G R A T U L A T I O N S !');
        GotoXY(23, 24); Write('*******************************');
        Delay(750);
        GotoXY(23, 22); ClrEoL;
        GotoXY(23, 23); ClrEoL;
        GotoXY(23, 24); ClrEoL;
        Delay(750);
      until KeyPressed;
    end
    // Snake hits borders or eats a bad meal : Blinking snake and message
    else begin
      repeat
        // Display message + turn snake on (-> draw it)
        DrawSnake(S, 0, 0, False, False);
        TextColor(LightMagenta);
        GotoXY(29, 22); Write('*******************');
        GotoXY(29, 23); Write('G A M E   O V E R !');
        GotoXY(29, 24); Write('*******************');
        Delay(750);
        // Clear message and turn snake off (-> display black spaces on black background)
        TextColor(Black);
        for I := 1 to S.Length do begin
          GotoXY(S.Elements[I].X, S.Elements[I].Y);
          Write(' ');
        end;
        GotoXY(29, 22); ClrEoL;
        GotoXY(29, 23); ClrEoL;
        GotoXY(29, 24); ClrEoL;
        Delay(750);
      until KeyPressed;
    end;
    TextColor(White);
  end;

{ -------------- }
{ Move the snake }
{ -------------- }

procedure MoveSnake(var S: TSnake; var M: TMeals; X, Y, TailX, TailY: Integer; var CT, EoG: Boolean);

  var
    I : Integer;

  begin
    // Game over if the snake hits the borders
    if (S.Elements[1].X + X < 1) or (S.Elements[1].X + X  > 78) or
       (S.Elements[1].Y + Y < 1) or (S.Elements[1].Y + Y > 45) then
      EoG := True
    // Game over if the snake hits a bad meal
    else begin
      for I := M.Good + 1 to M.Good + M.Bad do begin
        if (S.Elements[1].X + X = M.Meals[I].X) and (S.Elements[1].Y + Y = M.Meals[I].Y) then
          EoG := True;
      end;
    end;
    // Normal continuation of the game : move the snake by 1 position
    if not EoG then begin
      // Element's position is equal to previous position of preceding element
      for I := S.Length downto 2 do begin
        S.Elements[I].X := S.Elements[I - 1].X;
        S.Elements[I].Y := S.Elements[I - 1].Y;
      end;
      // First element (head) is at new position (determined by the actual move)
      S.Elements[1].X := S.Elements[1].X + X;
      S.Elements[1].Y := S.Elements[1].Y + Y;
      // Normally clear the position where the snake's tail was before
      CT := True;
      // Check if there is a good meal at the new position
      for I := 1 to M.Good do begin
        if (S.Elements[1].X = M.Meals[I].X) and
           (S.Elements[1].Y = M.Meals[I].Y) and
           not M.Meals[I].IsEaten then begin
          M.Meals[I].IsEaten := True;
          M.GoodLeft := M.GoodLeft - 1;
          // Increase snake's length if head encounters a blue meal
          if M.Meals[I].Effect = 1 then begin
            // If the snake grows, do not clear previous tail element's position
            CT := False;
            // Snake grows at the tail
            S.Length := S.Length + 1;
            S.Elements[S.Length].X := TailX;
            S.Elements[S.Length].Y := TailY;
            // Color of the new element
            if S.Elements[S.Length - 1].Color = Green then
              S.Elements[S.Length].Color := White
            else
              S.Elements[S.Length].Color := Green;
            // Game won when the snake has reached maximum length
            if S.Length = LengthMax then
              EoG := True;
          end;
        end;
      end;
    end;
  end;

{ -------------------------------- }
{ Play the game, round after round }
{ -------------------------------- }

procedure PlayRound(var S:TSnake; var M: TMeals; var CT, EoG: Boolean);

  const
    Key_ESC  = 27;
    Key_Left = 75; Key_Right = 77; Key_Up = 72; Key_Down = 80;
    Key_F10  = 68; Key_Ctrl_F12  = 138;

  var
    TailX, TailY, LastX, LastY, X, Y, D : Integer;
    DT : Real;
    Key : Char;
    Key_Null, CW : Boolean;

  begin
    Key_Null := False; Key := ' ';
    // Wait for F10 to start the game (or ESC to exit)
    repeat
      Key := ReadKey;
      if Ord(Key) = 0 then begin
        Key_Null := True;
        Key := ReadKey;
      end;
    until (Ord(Key) = Key_ESC) or (Key_Null and (Ord(Key) = Key_F10));
    if Ord(Key) = Key_ESC then
      EoG := True
    // Play this round (until all good meals have been eaten)
    else begin
      X := 1; Y := 0;
      D := 0; CW := False;
      // Delay factor depending on snake's speed
      DT := 2 * Int(10 * (1 / S.Speed));
      // Move the snake if key pressed or delay is over
      repeat
        if (Ord(Key) = 255) or (D = DT) then begin
          // Remember position of the snakes's tail element (to clear it when moving)
          TailX := S.Elements[S.Length].X;
          TailY := S.Elements[S.Length].Y;
          // Move the snake by 1 position + draw new position onto game window
          MoveSnake(S, M, X, Y, TailX, TailY, CT, EoG);
          DrawSnake(S, TailX, TailY, CW, CT);
          GameInfo(S, M);
          // Remember X- and Y-direction of last move (to prevent direct move in opposite direction)
          LastX := X; LastY := Y;
          if D = DT then
            D := 0;
        end;
        if not EoG then begin
          Delay(20);
          D := D + 1;
          Key := ' ';
          // Check if user pressed a key and do corresponding action
          if KeyPressed then begin
            Key := ReadKey;
            if Ord(Key) = 0 then begin
              Key_Null := True;
              Key := ReadKey;
            end;
            // ESC exits the game
            if Ord(Key) = Key_ESC then
              EoG := True
            else if Key_Null then begin
              case Ord(Key) of
                // Arrow keys move the snake by 1 position
                Key_Left : if LastX <> 1 then begin X := -1; Y := 0; Key := Chr(255); end;
                Key_Right : if LastX <> -1 then begin X := 1; Y := 0; Key := Chr(255); end;
                Key_Up : if LastY <> 1 then begin X := 0; Y := -1; Key := Chr(255); end;
                Key_Down : if LastY <> -1 then begin X := 0; Y := 1; Key := Chr(255); end;
                // F10 pauses the game
                Key_F10 : begin repeat until KeyPressed; Key := ReadKey; if Ord(Key) = 0 then Key := ReadKey; end;
                // Cheat key: CTRL+F12 allows to pass to next round (no need to eat cyan meals)
                Key_Ctrl_F12 : M.GoodLeft := 0;
              end;
            end;
          end;
        end;
      until (M.GoodLeft = 0) or EoG;
    end;
    // End of game
    if EoG then
      if not (Ord(Key) = Key_ESC) then
        PEndOfGame(S);
  end;

{ ------------ }
{ Main program }
{ ------------ }

begin
  ClrScr;
  TextColor(Yellow);
  GotoXY(35 , 1); Writeln('S N A K E');
  GotoXY(35 , 2); Writeln('=========');
  TextColor(White);
  Writeln; Writeln('Simple snake game (version 1.0, allu, Feb-Jun 2016).');
  Writeln;
  Writeln('Move your snake around and let it grow by eating the meals it encounters.');
  Writeln('To move the snake, use the arrow keys. Press F10 to start a round (during the');
  Writeln('round, F10 pauses the game; hit any key to resume). To quit the game at any');
  Writeln('moment, press the ESC key.');
  Writeln;
  Writeln('Effects of the objects met are as follows:');
  TextColor(LightCyan); Write('  Cyan'); TextColor(White);
  Writeln('  meals which have no effect (but must be eaten).');
  TextColor(LightBlue); Write('  Blue'); TextColor(White);
  Writeln('  meals which grow the snake by 1 element.');
  TextColor(LightRed); Write('  Red'); TextColor(White);
  Writeln('   rocks which kill the snake when it touches them!');
  Writeln;
  Writeln('The snake also dies when the snake''s head touches the borders! The game is');
  Writeln('won when the snake''s length is ', LengthMax, ' elements...');
  repeat
    GotoXY(1, 19); ClrEoL;
    GotoXY(1, 19); Write('Snake speed [1 - ', SpeedMax, '] ? ');
    Readln(Speed);
  until Speed in [0 .. SpeedMax];
  if Speed > 0 then begin
    Randomize;
    Window(1, 1, 80, 50); TextBackground(White); TextColor(Black); ClrScr; cursoroff;
    InitSnake(MySnake, Speed, LengthMin);
    EndOfGame := False;
    // Play round after round until game over or you won...
    repeat
      ClearWindow := True; ClearTail := False;
      ResetSnake(MySnake);
      DrawSnake(MySnake, 0, 0, ClearWindow, ClearTail);
      Initmeals(MyMeals, GoodMin, GoodMax, BadMax);
      Drawmeals(MyMeals);
      GameInfo(MySnake, MyMeals);
      PlayRound(MySnake, MyMeals, ClearTail, EndOfGame);
    until EndOfGame;
    Window(1, 1, 80, 50); TextBackground(Black); TextColor(White); ClrScr; cursoron;
  end;
end.

