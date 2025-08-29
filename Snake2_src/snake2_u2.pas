{***************************************************}
{* Play the Snake game unit for Snake2 application *}
{***************************************************}

unit snake2_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLType, snake2_u3;

const
  SnakeLengthMin = 5;  SnakeLengthMax = 40;
  GoodMealsMin   = 10; GoodMealsMax   = 20;
  BadMealsMin    = 15; BadMealsMax    = 25;

type
  TSnake = record
    Speed    : Integer;
    Length   : Integer;
    HasEaten : Integer;
    Elements : array[1 .. SnakeLengthMax] of record
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
    Meals : array[1 .. GoodMealsMax + BadMealsMax] of TMeal;
  end;
  {********}
  { TfGame }
  {********}
  TfGame = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edRound, edSpeed, edMeals, edLength: TEdit;
    shGameWindow: TShape;
    btStart: TButton;
    btCancel: TButton;
    tiSnakeMove: TTimer;
    tiEndOfGame: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure tiSnakeMoveTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tiEndOfGameTimer(Sender: TObject);
  private
    iRound, iDirX, iDirY, iDirLastX, iDirLastY, iEoGCount: Integer;
    bEndOfGame, bKeybEvent: Boolean;
    sEoGTxt: string;
    clEoGColor: TColor;
    rdSnake: TSnake;                                                           // the snake as a TSnake record
    rdMeals: TMeals;                                                           // the meals as an array of TMeal records
    shSnake: array[1..SnakeLengthMax] of TShape;                               // rectangle shapes, one for each snake element
    shMeals: array[1..GoodMealsMax + BadMealsMax] of TShape;                   // rectangle shapes, one for each meal
  public
    // Game level, snake maximal length and speed, set by main unit
    iLevel, iLengthMax, iSpeed: Integer;
  end;

var
  fGame: TfGame;

implementation

{$R *.lfm}

{ Init the snake record }

procedure InitSnake(var S: TSnake; V, L: Integer);

var
  I, Colour: Integer;

begin
  // Init snake's elements (set all to null)
  for I := 1 to SnakeLengthMax do begin
    S.Elements[I].X := 0;
    S.Elements[I].Y := 0;
    S.Elements[I].Color := 0;
  end;
  // Snake's speed and initial length
  S.Speed := V; S.Length := L;
  // Reset number of blue meals eaten
  S.HasEaten := 0;
  // Snake elements' color (alternating green and white)
  Colour := clLime;
  for I := 1 to S.Length do begin
    S.Elements[I].Color := Colour;
    if Colour = clLime then
      Colour := clWhite
    else
      Colour := clLime;
  end;
end;

{ Reset the snake (at beginning of each round) }

procedure ResetSnake(var S: TSnake);

var
  I: Integer;

begin
  // The snake starts at the left end (X) and the middle of the game window (Y)
  for I := 1 to S.Length do begin
    S.Elements[I].X := (S.Length + 1) - I;
    S.Elements[I].Y := 25;
  end;
  // Reset the number of blue meals eaten
  S.HasEaten := 0;
end;

{ Generate random meals (at beginning of each round) }

procedure ResetMeals(Level: Integer; var M: TMeals);

var
  I, J, X, Y, Count, MagentaMeals: Integer;
  MealOk: Boolean;

begin
  // Init the meals array (set all elements' variables to null)
  for I := 1 to GoodMealsMax + BadMealsMax do begin
    M.Meals[I].X := 0;
    M.Meals[I].Y := 0;
    M.Meals[I].Effect := 0;
    M.Meals[I].IsEaten := False;
  end;
  // Number of good/bad meals (not including the magenta ones!)
  M.Good := Random(GoodMealsMax div 2 - GoodMealsMin div 2 + 1) + GoodMealsMin div 2;
  M.GoodLeft:= M.Good;
  M.Bad  := Random(BadMealsMax - BadMealsMin + 1) + BadMealsMin;
  // Bad meals position on game window (ignoring if 2 or more are at same position)
  for I := M.Good + 1 to M.Good + M.Bad do begin
    X := Random(80) + 1;
    // Exlude "space directly around" the snake
    repeat
      Y := Random(50) + 1;
    until (Y < 24) or (Y > 26);
    M.Meals[I].X := X;  M.Meals[I].Y := Y;
    M.Meals[I].Effect := 2;                                                    // rock (bad meal): effect = 2
    M.Meals[I].IsEaten := False;
  end;
  // Good meals position on game window and meal type (cyan or blue)
  Count := 0;
  for I := M.Good downto 1 do begin
    repeat
      MealOk := True;
      X := Random(80) + 1;
      repeat
        Y := Random(50) + 1;
      until (Y < 24) or (Y > 26);
      // Check if this position (and squares directly around) is still empty
      for J := M.Good + M.Bad downto I + 1 do begin
        if (X = M.Meals[J].X) and (Abs(Y - M.Meals[J].Y) < 2) or (Y = M.Meals[J].Y)
          and (Abs(X - M.Meals[J].X) < 2) then
          MealOk := False;
      end;
    until MealOk;
    M.Meals[I].X := X; M.Meals[I].Y := Y;
    M.Meals[I].Effect := Random(2);                                            // cyan/blue meals: effect = 0 resp. 1
    if M.Meals[I].Effect = 1 then
      Inc(Count);                                                              // count blue meals
    M.Meals[I].IsEaten := False;
  end;
  // Magenta meals position on game window
  if Level > 1 then begin
    MagentaMeals := Random(Count div 2) + Count div 2 + 1;                     // number of magenta meals depending on number of blue meals
    for I := M.Good + M.Bad + 1 to M.Good + M.Bad + MagentaMeals + 1 do begin  // add them to (at the end of) the meals array
      repeat
        MealOk := True;
        X := Random(80) + 1;
        repeat
          Y := Random(50) + 1;
        until (Y < 24) or (Y > 26);
        // Check if this position (and squares directly around) is still empty
        for J := 1 to I - 1 do begin
          if ((X = M.Meals[J].X) and (Abs(Y - M.Meals[J].Y) < 2)) or ((Y = M.Meals[J].Y) and (Abs(X - M.Meals[J].X) < 2)) then
            MealOk := False;
        end;
      until MealOk;
      M.Meals[I].X := X; M.Meals[I].Y := Y;
      M.Meals[I].Effect := -1;                                                 // magenta meals: effect = -1
      M.Meals[I].IsEaten := False;
    end;
    M.Good += MagentaMeals; M.GoodLeft += MagentaMeals;                        // adjust number of good meals (now including the magenta ones)
  end;
end;

{ Clear game window (by making shapes corresponding to snake elements and meals invisible) }

procedure ClearGameWindow;

var
  I: Integer;

begin
  for I := 1 to SnakeLengthMax do
    fGame.shSnake[I].Visible := False;
  for I := 1 to GoodMealsMax + BadMealsMax do
    fGame.shMeals[I].Visible := False;
end;

{ Display game information }

procedure GameInfo(R: Integer; S: TSnake; M: TMeals);

begin
  fGame.edRound.Text := IntToStr(R);
  fGame.edSpeed.Text := IntToStr(S.Speed);
  fGame.edMeals.Text := IntToStr(M.GoodLeft);
  fGame.edLength.Text := IntToStr(S.Length);
end;

{ Draw the snake (at actual position in the game window) }

procedure DrawSnake(S: TSnake; Clear: Boolean);

var
  I : Integer;

begin
  if Clear then
    ClearGameWindow;                                                           // this will be done at beginning of round
  for I := 1 to S.Length do begin
    // Show a green/white rectangle shape at position of each snake element
    fGame.shSnake[I].Left := (S.Elements[I].X - 1) * 10 + 20;
    fGame.shSnake[I].Top := (S.Elements[I].Y - 1) * 10 + 110;
    fGame.shSnake[I].Brush.Color := S.Elements[I].Color;
    fGame.shSnake[I].Visible := True;
  end;
end;

{ Draw the meals (at actual position in the game window) }

procedure DrawMeals(M: TMeals; HideRocks: Boolean);

const
  Colors : array[0 .. 3] of Integer = (clFuchsia, clAqua, clBlue, clRed);

var
  CX, I: Integer;

begin
  // Show a magenta/cyan/blue/red rectangle shape at position of each meal
  for I := 1 to M.Good + M.Bad do begin
    CX := M.Meals[I].Effect + 1;
    if (CX = 3) and HideRocks then
      CX := 1;                                                                 // dispaying rocks as cyan meals
    fGame.shMeals[I].Left := (M.Meals[I].X - 1) * 10 + 20;
    fGame.shMeals[I].Top := (M.Meals[I].Y - 1) * 10 + 110;
    fGame.shMeals[I].Brush.Color := Colors[CX];
    fGame.shMeals[I].Visible := True;
  end;
end;

{ New game round initialisation }

procedure NewRound(Level: Integer; var R: Integer; var S: TSnake; var M: TMeals);

// This only initializes the new round; the round is actually started in the TfGame.btStartClick routine

begin
  fGame.tiSnakeMove.Enabled := False; fGame.bKeybEvent := False;
  Inc(R);
  ResetSnake(S); DrawSnake(S, True);
  ResetMeals(Level, M); DrawMeals(M, False);                                   // rocks always drawn in red (so that player can remember their positions)
  GameInfo(R, S, M);
  fGame.btStart.Caption := 'Start';
end;

{ Move the snake by 1 position }

procedure MoveSnake(var S: TSnake; var M: TMeals; X, Y, TailX, TailY: Integer; var EoR, EoG: Boolean);

var
  I : Integer;

// This routine, called by the TfGame.tiSnakeMoveTimer method, moves the snake by 1 position.
//  - if the snake hits the borders or if this position contains a rock, the end-of-game variable is set to True
//  - if this position contains a blue meal, the snake length is increased by 1
//      - if the snake has reached its maximum length, the end-of-game variable is set to True
//  - if this position contains a magenta meal
//      - if the snake has eaten a blue meal before, nothing happens
//      - otherwise, its length is decreased by 1 element; if length < 3, the end-of-game variable is set to True
//  - if there was a meal and all meals have been eaten now, the end-of-round variable is set to True

begin
  EoR := False;
  // Game over if the snake hits the borders
  if (S.Elements[1].X + X < 1) or (S.Elements[1].X + X  > 80) or
     (S.Elements[1].Y + Y < 1) or (S.Elements[1].Y + Y > 50) then
    EoG := True
  // Game over if the snake hits a rock (bad meal)
  else begin
    for I := 1 to M.Good + M.Bad do begin
      if (S.Elements[1].X + X = M.Meals[I].X) and (S.Elements[1].Y + Y = M.Meals[I].Y) and (M.Meals[I].Effect = 2) then
        EoG := True;
    end;
  end;
  // Normal continuation of the game : Move the snake by 1 position
  if not EoG then begin
    // The element's new position is equal to the previous position of the preceding element
    for I := S.Length downto 2 do begin
      S.Elements[I].X := S.Elements[I - 1].X;
      S.Elements[I].Y := S.Elements[I - 1].Y;
    end;
    // The first element (head) is at the new position (determined by the actual move)
    S.Elements[1].X := S.Elements[1].X + X;
    S.Elements[1].Y := S.Elements[1].Y + Y;
    // Check if there is a (good) meal at the new position
    for I := 1 to M.Good + M.Bad do begin
      if (S.Elements[1].X = M.Meals[I].X) and (S.Elements[1].Y = M.Meals[I].Y) and not M.Meals[I].IsEaten then begin
        // Remove this meal from game
        M.Meals[I].IsEaten := True;
        M.GoodLeft := M.GoodLeft - 1;
        fGame.shMeals[I].Visible := False;
        // Increase snake's length if its head encounters a blue meal
        if M.Meals[I].Effect = 1 then begin
          // Snake grows at the tail
          Inc(S.Length);
          S.Elements[S.Length].X := TailX; S.Elements[S.Length].Y := TailY;
          // Color of the new element
          if S.Elements[S.Length - 1].Color = clLime then
            S.Elements[S.Length].Color := clWhite
          else
            S.Elements[S.Length].Color := clLime;
          // Increment number of blue meals eaten
          Inc(S.HasEaten);
          // Game won when the snake has reached maximum length
          if S.Length = fGame.iLengthMax then
            EoG := True;
        end
        // Decrease snake's length if its head encounters a magenta meal (except if the snake has eaten a blue meal before)
        else if M.Meals[I].Effect = -1 then begin
          if S.HasEaten > 0 then begin
            // No effect of magenta meal, if snake has eaten
            Dec(S.HasEaten);                                                   // decrease number of blue meals eaten
          end
          else begin
            // Otherwise, decrease of the snake's length
            Dec(S.Length);
            if S.Length < 3 then                                               // end of the game if the snake's length becomes less than 3 elements
              EoG := True;
          end;
        end;
        if M.GoodLeft = 0 then                                                 // end of the round if all meals have been eaten
          EoR := True;
      end;
    end;
  end;
end;

{ Display end of game message }

procedure EndOfGameMessage(S: TSnake);

begin
  // Set message depending on user wins/looses
  if S.Length = fGame.iLengthMax then begin
    // User wins
    fGameOver.imGameOver.Picture.LoadFromFile('winner.png');
    fGame.sEoGTxt := 'Y O U   W I N ! ';
    fGame.clEoGColor := clLime;
  end
  else begin
    // User looses
    fGameOver.imGameOver.Picture.LoadFromFile('looser.png');
    fGame.sEoGTxt := 'Y O U   L O O S E ! ';
    fGame.clEoGColor := clRed;
  end;
  // Start the end-of-game timer (it's the timer routine that will display the blinking end-of-game message)
  fGame.iEoGCount := 0;
  fGame.tiEndOfGame.Enabled := True;
end;

{********}
{ TfGame }
{********}

{ Application start: Initialisation }

procedure TfGame.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create (maximum number of) snake shapes
  for I := 1 to SnakeLengthMax do begin
    fGame.shSnake[I] := TShape.Create(fGame.shSnake[I]);
    fGame.shSnake[I].Parent := fGame;
    fGame.shSnake[I].Shape := stRectangle;
    fGame.shSnake[I].Width := 10;
    fGame.shSnake[I].Height := 10;
    fGame.shSnake[I].Visible := False;
  end;
  // Create (maximum number of) meals shapes
  for I := 1 to GoodMealsMax + BadMealsMax do begin
    fGame.shMeals[I] := TShape.Create(fGame.shMeals[I]);
    fGame.shMeals[I].Parent := fGame;
    fGame.shMeals[I].Shape := stRectangle;
    fGame.shMeals[I].Width := 10;
    fGame.shMeals[I].Height := 10;
    fGame.shMeals[I].Visible := False;
  end;
  // Start random number generator
  Randomize;
end;

{ Form activation: Game round initialisation }

procedure TfGame.FormActivate(Sender: TObject);

begin
  iRound := 0;
  InitSnake(rdSnake, iSpeed, SnakeLengthMin);                                  // create initial snake record
  bEndOfGame := False;
  NewRound(iLevel, iRound, rdSnake, rdMeals);                                  // new game round initialisation
  btStart.SetFocus;
end;

{ Button "Start/Pause/Resume": Start, pause, resp. resume current game round }

procedure TfGame.btStartClick(Sender: TObject);

begin
  // Round start or resume after pause
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Resume') then begin
    if btStart.Caption = 'Start' then begin
      // Redraw the meals (Level 3)
      if iLevel = 3 then
        DrawMeals(rdMeals, True);                                              // hidden rocks (= drawn as cyan meals)
      // Snake initial movement = rightwards
      iDirX := 1; iDirY := 0; iDirLastX := 0; iDirLastY := 0;
    end;
    // Enable snake movement timer
    tiSnakeMove.Interval := 20 * (11 - iSpeed);
    tiSnakeMove.Enabled := True;
    // Enable keyboard event capture
    bKeybEvent := True;
    // "Start" button now used to pause the game
    btStart.Caption := 'Pause';
  end
  // Round pause
  else begin
    // Disable timer
    tiSnakeMove.Enabled := False;
    // Disable keyboard event capture
    bKeybEvent := False;
    // "Start" button now used to resume the game
    btStart.Caption := 'Resume';
  end;
end;

{ Button "Cancel": Quit actual game and close the game window }

procedure TfGame.btCancelClick(Sender: TObject);

begin
  // Disable timer
  tiSnakeMove.Enabled := False;
  // Disable keyboard event capture
  bKeybEvent := False;
  // Close game window
  Close;
end;

{ Snake movement timer: Move the snake }

procedure TfGame.tiSnakeMoveTimer(Sender: TObject);

var
  TailX, TailY: Integer;
  EndOfRound: Boolean;

begin
  EndOfRound := False;
  // No more meals left (all eaten): End of round
  if rdMeals.GoodLeft = 0 then
    EndOfRound := True
  // Still meals to eat: Move the snake
  else begin
    // Remember position of the snakes's tail element
    TailX := rdSnake.Elements[rdSnake.Length].X; TailY := rdSnake.Elements[rdSnake.Length].Y;
    // Move the snake by 1 position
    MoveSnake(rdSnake, rdMeals, iDirX, iDirY, TailX, TailY, EndOfRound, bEndOfGame);
    // If the snake length has decreased (magenta meal), remove the (previously) tail element (by making the corr. shape invisible)
    if (rdSnake.Elements[rdSnake.Length].X <> TailX) or (rdSnake.Elements[rdSnake.Length].Y <> TailY) then
      shSnake[rdSnake.Length + 1].Visible := False;
    // Draw the snake at its new position in the game window
    DrawSnake(rdSnake, False);
    GameInfo(iRound, rdSnake, rdMeals);
  end;
  // End of game
  if bEndOfGame then begin
    tiSnakeMove.Enabled := False;                                              // disable timer
    bKeybEvent := False;                                                       // disable keyboard capture
    EndOfGameMessage(rdSnake);                                                 // display end of game message
  end
  // End of round
  else if EndOfRound then
    NewRound(iLevel, iRound, rdSnake, rdMeals)                                 // init new round (and wait for user to push the "Start" button)
  // Normal round continuation
  else begin
    // Remember X- and Y-direction of last move
    iDirLastX := iDirX; iDirLastY := iDirY;
  end;
end;

{ Keyboard event capture: Take action depending on keyboard key pressed }

procedure TfGame.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

begin
  // Do only if keyboard capture is enabled (i.e. when the snake is moving)
  if bKeybEvent then begin
    // Key pressed = left arrow
    if Key = VK_LEFT then begin
      if iDirLastX <> 1 then begin                                             // prevent direct move in opposite direction
        // Set snake direction to leftwards
        iDirX := -1;
        iDirY := 0;
      end;
    end
    // Key pressed = right arrow
    else if Key = VK_RIGHT then begin
      if iDirLastX <> -1 then begin
        // Set snake direction to rightwards
        iDirX := 1;
        iDirY := 0;
      end;
    end
    // Key pressed = up arrow
    else if Key = VK_UP then begin
      if iDirLastY <> 1 then begin
        // Set snake direction to upwards
        iDirX := 0;
        iDirY := -1;
      end;
    end
    // Key pressed = down arrow
    else if Key = VK_DOWN then begin
      if iDirLastY <> -1 then begin
        // Set snake direction to downwards
        iDirX := 0;
        iDirY := 1;
      end;
    end
    // Key pressed = CTRL + F12
    else if (Key = VK_F12) and (ssCtrl in Shift) then
      // Cheat key: CTRL+F12 allows to pass to next round (without having to eat the resting meals)
      rdMeals.GoodLeft := 0;
    // Save the snake's actual position
    iDirLastX := iDirX; iDirLastY := iDirY;
  end;
end;

{ End of game timer: Display blinking message and close the game window }

procedure TfGame.tiEndOfGameTimer(Sender: TObject);

begin
  Inc(iEoGCount);
  if iEoGCount = 1 then
    fGameOver.Show;
  // Alternately display message and "nothing"
  if iEoGCount mod 2 = 1 then begin
    fGameOver.stGameOver2.Caption := sEoGTxt;
    fGameOver.stGameOver2.Font.Color := clEoGColor;
  end
  else begin
    fGameOver.stGameOver2.Caption := '';
  end;
  // After 5 blink periods, close the game window
  if iEoGCount = 10 then begin
    tiEndOfGame.Enabled := False;
    fGameOver.Hide;
    Close;
  end;
end;

end.

