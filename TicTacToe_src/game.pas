{***************************************}
{* Main unit for TicTacToe application *}
{***************************************}

unit game;

// The Tic-Tac-Toe algorithm routines used are based on the C++ program published at the "Geeks for Geeks" website:
// https://www.geeksforgeeks.org/minimax-algorithm-in-game-theory-set-3-tic-tac-toe-ai-finding-optimal-move/

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type
  TField = record
    Row, Col: Integer;
  end;
  TBoard = array[0..2, 0..2] of Char;
  TBoardLabels = array[0..2, 0..2] of TStaticText;
  {*************}
  { TfTicTacToe }
  {*************}
  TfTicTacToe = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsPlayerMark, mOptionsFirstMove: TMenuItem;
    mOptionsPlayerMarkOBlue, mOptionsPlayerMarkORed, mOptionsPlayerMarkXBlue, mOptionsPlayerMarkXRed: TMenuItem;
    mOptionsFirstMovePlayer, mOptionsFirstMoveComputer, mOptionsFirstMoveAlternate: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3: TLabel;
    shField1, shField2, shField3, shField4, shField5: TShape;
    shField6, shField7, shField8, shField9: TShape;
    stField1, stField2, stField3, stField4, stField5: TStaticText;
    stField6, stField7, stField8, stField9: TStaticText;
    edPlayer, edComputer: TEdit;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsPlayerMarkOBlueClick(Sender: TObject);
    procedure mOptionsPlayerMarkORedClick(Sender: TObject);
    procedure mOptionsPlayerMarkXBlueClick(Sender: TObject);
    procedure mOptionsPlayerMarkXRedClick(Sender: TObject);
    procedure mOptionsFirstMovePlayerClick(Sender: TObject);
    procedure mOptionsFirstMoveComputerClick(Sender: TObject);
    procedure mOptionsFirstMoveAlternateClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure shField1MouseDown(Sender: TObject);
    procedure shField2MouseDown(Sender: TObject);
    procedure shField3MouseDown(Sender: TObject);
    procedure shField4MouseDown(Sender: TObject);
    procedure shField5MouseDown(Sender: TObject);
    procedure shField6MouseDown(Sender: TObject);
    procedure shField7MouseDown(Sender: TObject);
    procedure shField8MouseDown(Sender: TObject);
    procedure shField9MouseDown(Sender: TObject);
    procedure stField1Click(Sender: TObject);
    procedure stField2Click(Sender: TObject);
    procedure stField3Click(Sender: TObject);
    procedure stField4Click(Sender: TObject);
    procedure stField5Click(Sender: TObject);
    procedure stField6Click(Sender: TObject);
    procedure stField7Click(Sender: TObject);
    procedure stField8Click(Sender: TObject);
    procedure stField9Click(Sender: TObject);
    procedure btStartClick(Sender: TObject);
  private
    sActualPlayer, sActualPlayer0: string;
    cPlayerMark, cComputerMark: Char;
    iPlayerColor, iComputerColor: TColor;
    aBoard: TBoard;
    rField: TField;
    stBoard: TBoardLabels;
  end;

var
  fTicTacToe: TfTicTacToe;

implementation

{$R *.lfm}

{ Calculate maximum of 2 integer values }

function Max(I, J: Integer): Integer;

var
  M: Integer;

begin
  if I >= J then
    M := I
  else
    M := J;
  Result := M;
end;

{ Calculate minimum of 2 integer values }

function Min(I, J: Integer): Integer;

var
  M: Integer;

begin
  if I <= J then
    M := I
  else
    M := J;
  Result := M;
end;

{ Reset the board }

procedure BoardReset(var Board: TBoard; var BoardLabels: TBoardLabels);

var
  I, J: Integer;

begin
  for I := 0 to 2 do begin
    for J := 0 to 2 do begin
      Board[I, J] := '_';
      BoardLabels[I, J].Visible := False;
    end;
  end;
end;

{ Check if there are still moves left }

function MovesLeft(var Board: TBoard): Boolean;

var
  I, J: Integer;
  IsLeft: Boolean;

begin
  IsLeft := False;
  for I := 0 to 2 do begin
    for J := 0 to 2 do begin
      if Board[I, J] = '_' then                                                // the underscore marks empty fields
        IsLeft := True;
    end;
  end;
  Result := IsLeft;
end;

{ Evaluation: Check if computer will win or loose, or if the game will end in a tie }

function Evaluate(PlayerMark, ComputerMark: Char; var Board: TBoard): Integer;

// Function return: +10 for winning, -10 for loosing, 0 for tie

var
  Row, Col, Eval: Integer;
  Row0, Col0: Integer;

begin
  Row0 := -1; Col0 := -1;
  Eval := 0;                                                                   // if no player wins, return = 0 (tie)
  // Checking rows for victory
  for Row := 0 to 2 do begin
    if (Row0 = -1) and (Col0 = -1) then begin
      if (Board[Row, 0] = Board[Row, 1]) and (Board[Row, 1] = Board[Row, 2]) then begin
        Row0 := Row; Col0 := 0;
      end;
    end;
  end;
  // Checking columns for victory
  for Col := 0 to 2 do begin
    if (Row0 = -1) and (Col0 = -1) then begin
      if (Board[0, Col] = Board[1, Col]) and (Board[1, Col] = Board[2, Col]) then begin
        Row0 := 0; Col0 := Col;
      end;
    end;
  end;
  // Checking diagonals for victory
  if (Row0 = -1) and (Col0 = -1) then begin
    if (Board[0, 0] = Board[1, 1]) and (Board[1, 1]= Board[2, 2]) then begin
      Row0 := 0; Col0 := 0;
    end
    else if (Board[0, 2] = Board[1, 1]) and (Board[1, 1] = Board[2, 0]) then begin
      Row0 := 0; Col0 := 2;
    end;
  end;
  if (Row0 <> -1) and (Col0 <> -1) then begin
    // One of the players wins
    if Board[Row0, Col0] = ComputerMark then
      Eval := +10                                                              // if computer wins, return = +10
    else if Board[Row0, Col0] = PlayerMark then
      Eval := -10;                                                             // if player wins, return = -10
  end;
  Result := Eval;
end;

{ Minimax algorithm }

function Minimax(PlayerMark, ComputerMark: Char; var Board: TBoard; Depth: Integer; IsMax: Boolean): Integer;

// The function considers all the possible ways the game can go and returns the best score value for the actual board

var
  Score, Best, I, J: Integer;

begin
  Score := Evaluate(PlayerMark, ComputerMark, Board);                          // check if there is a winner
  if Abs(Score) = 10 then begin
    // If the maximizer or the minimizer will win the game, return the score
    // Subtracting/adding the depth from/to the +10/-10 value, lets the computer choose the move
    // that results in the fastest victory or in the slowest loss (if there are several winning/loosing moves)
    if Score = +10 then                                                        // computer wins
      Score -= Depth
    else                                                                       // computer looses
      Score += Depth;
    Best := Score;
  end
  else if not MovesLeft(Board) then begin
    // If there is no winner and there are no more moves, then it is a tie
    Best := 0;
  end
  else begin
    // If there is no winner and there are still moves left, continue evaluation (recursive call of Minimax function)
    if IsMax then begin
      // Maximizer's move
      // ----------------
      Best := -1000;
      for I := 0 to 2 do begin
        for J := 0 to 2 do begin
          // Check if the cell is empty
          if Board[I, J] = '_' then begin
            // Do the move
            Board[I, J] := ComputerMark;
            // Call Minimax recursively and keep the new evaluation value if it is greater than the actual one
            Best := Max(Best, Minimax(PlayerMark, ComputerMark, Board, Depth + 1, not IsMax));
            // Undo the move
            Board[I, J] := '_';
          end;
        end;
      end;
    end
    else begin
      // Minimizer's move
      // ----------------
      Best := 1000;
      for I := 0 to 2 do begin
        for J := 0 to 2 do begin
          // Check if the cell is empty
          if Board[I, J] = '_' then begin
            // Make the move
            Board[I, J] := PlayerMark;
            // Call Minimax recursively and keep the new evaluation value if it is less than the actual one
            Best := Min(Best, Minimax(PlayerMark, ComputerMark, Board, Depth + 1, not IsMax));
            // Undo the move
            Board[I, J] := '_';
          end;
        end;
      end;
    end;
  end;
  // Return best score value for actual board
  Result := Best;
end;

{ Find the best move possible }

function FindBestMove(PlayerMark, ComputerMark: Char; var Board: TBoard): TField;

// The function traverses all cells, evaluating the minimax function for
// all empty fields and returns the field with the optimal evaluation value

var
  BestVal, MoveVal, I, J: Integer;
  BestMove: TField;

begin
  BestVal := -1000;
  BestMove.Row := -1; BestMove.Col := -1;
  for I := 0 to 2 do begin
    for J := 0 to 2 do begin
      // Empty fields only
      if Board[I, J] = '_' then begin
        // Do the move
        Board[I, J] := ComputerMark;
        // Compute evaluation function for this move
        MoveVal := Minimax(PlayerMark, ComputerMark, Board, 0, False);
        // Undo the move
        Board[I, J] := '_';
        // If the value for the current move is greater than the best value so far, update best values
        if MoveVal > BestVal then begin
          BestMove.Row := I; BestMove.Col := J;                                // board field with optimal value
          BestVal := MoveVal;                                                  // new best value
        end;
      end;
    end;
  end;
  // return the field with the optimal evaluation value
  Result := BestMove;
end;

{ Computer move (automatically done, when player has done their move) }

procedure ComputerMove(var ActualPlayer: string; PlayerMark, ComputerMark: Char; Colour: TColor;
  var Board: TBoard; var BoardLabels: TBoardLabels; RandomMove: Boolean);

var
  Eval, R, C: Integer;
  Mess: string;
  Move: TField;

begin
  if RandomMove then begin
    // Random move (actually just its first move, when being the first to play)
    repeat
      R := Random(3); C := Random(3);
    until Board[R, C] = '_';
    Move.Row := R; Move.Col := C;
  end
  else begin
    // Optimal move (in actual program version, the computer will never loose...)
    Move := FindBestMove(PlayerMark, ComputerMark, Board);
  end;
  // Update the board
  Board[Move.Row, Move.Col] := ComputerMark;
  BoardLabels[Move.Row, Move.Col].Visible := True;
  BoardLabels[Move.Row, Move.Col].Caption := ComputerMark;
  BoardLabels[Move.Row, Move.Col].Font.Color := Colour;
  // Check if game is over
  Eval := Evaluate(PlayerMark, ComputerMark, Board);
  if (Eval = +10) or ((Eval = 0) and not (MovesLeft(Board))) then begin
    // Game over
    if Eval = +10 then begin
      // There are three computer marks aligned: Computer wins
      Mess := 'Computer wins!';
      fTicTacToe.edComputer.Text := IntToStr(StrToInt(fTicTacToe.edComputer.Text) + 3);
    end
    else begin
      // There are no more moves left (and nobody wins): Tie
      Mess := 'Game ends in a tie.';
      fTicTacToe.edComputer.Text := IntToStr(StrToInt(fTicTacToe.edComputer.Text) + 1);
      fTicTacToe.edPlayer.Text := IntToStr(StrToInt(fTicTacToe.edPlayer.Text) + 1);
    end;
    MessageDlg('Game over', Mess, mtInformation, [mbOK], 0);
    ActualPlayer := '';
    fTicTacToe.btStart.Enabled := True;                                        // enable "Next" button to start a new game
  end
  else begin
    // Game continuing with player having to move
    ActualPlayer := 'Player';
  end;
end;

{ Player move (done, when player clicks on a board field) }

procedure PlayerMove(var ActualPlayer: string; PlayerMark, ComputerMark: Char; PlayerColor, ComputerColor: TColor; Field: TField;
  var Board: TBoard; var BoardLabels: TBoardLabels);

// After the player has moved (and the game isn't over), the ComputerMove procedure is called to automatically do the computer play

var
  Eval, R, C: Integer;
  Mess: string;

begin
  Mess := '';
  // React only on clicks onto the board fields, if it actually is the player's turn
  if ActualPlayer = 'Player' then begin
    R := Field.Row; C := Field.Col;                                            // row and column of field that has been clicked
    if Board[R, C] = '_' then begin
      // Empty field: Mark it
      Board[R, C] := PlayerMark;
      BoardLabels[R, C].Visible := True;
      BoardLabels[R, C].Caption := PlayerMark; BoardLabels[R, C].Font.Color := PlayerColor;
    end
    else begin
      // Marked field: Invalid move
      Mess := 'This field is already marked!';
    end;
    // Field clicked was a valid one
    if Mess = '' then begin
      // Check if game is over
      Eval := Evaluate(PlayerMark, ComputerMark, Board);
      if (Eval = -10) or ((Eval = 0) and not (MovesLeft(Board))) then begin
        // Game over
        if Eval = -10 then begin
          // There are three player marks aligned: Player wins (never happens in actual version...)
          Mess := 'Player wins!';
          fTicTacToe.edPlayer.Text := IntToStr(StrToInt(fTicTacToe.edPlayer.Text) + 3);
        end
        else begin
          // There are no more moves left (and nobody wins): Tie
          Mess := 'Game ends in a tie.';
          fTicTacToe.edPlayer.Text := IntToStr(StrToInt(fTicTacToe.edPlayer.Text) + 1);
          fTicTacToe.edComputer.Text := IntToStr(StrToInt(fTicTacToe.edComputer.Text) + 1);
        end;
        MessageDlg('Game over', Mess, mtInformation, [mbOK], 0);
        ActualPlayer := '';
        fTicTacToe.btStart.Enabled := True;
      end
      else begin
        // Game continuing with computer (immediately) moving
        ActualPlayer := 'Computer';
        ComputerMove(ActualPlayer, PlayerMark, ComputerMark, ComputerColor, Board, BoardLabels, False);
      end;
    end
    // Field clicked was an invalid one
    else begin
      MessageDlg('Invalid move', Mess, mtError, [mbOK], 0);
    end;
  end;
end;

{*************}
{ TfTicTacToe }
{*************}

{ Application start: Initiaisation }

procedure TfTicTacToe.FormCreate(Sender: TObject);

begin
  stBoard[0, 0] := stField1; stBoard[0, 1] := stField2; stBoard[0, 2] := stField3;
  stBoard[1, 0] := stField4; stBoard[1, 1] := stField5; stBoard[1, 2] := stField6;
  stBoard[2, 0] := stField7; stBoard[2, 1] := stField8; stBoard[2, 2] := stField9;
  Randomize;
  mGameNew.Click;
end;

{ Menu "Game > New": Start a new game series (with reset of player scores) }

procedure TfTicTacToe.mGameNewClick(Sender: TObject);

begin
  sActualPlayer0 := ''; sActualPlayer := '';
  BoardReset(aBoard, stBoard);                                                 // clear the board
  // Set player/computer mark and color for this game series
  if mOptionsPlayerMarkOBlue.Checked then begin
    cPlayerMark := 'O'; cComputerMark := 'X';
    iPlayerColor := clBlue; iComputerColor := clRed;
  end
  else if mOptionsPlayerMarkORed.Checked then begin
    cPlayerMark := 'O'; cComputerMark := 'X';
    iPlayerColor := clRed; iComputerColor := clBlue;
  end
  else if mOptionsPlayerMarkXBlue.Checked then begin
    cPlayerMark := 'X'; cComputerMark := 'O';
    iPlayerColor := clBlue; iComputerColor := clRed;
  end
  else begin
    cPlayerMark := 'X'; cComputerMark := 'O';
    iPlayerColor := clRed; iComputerColor := clBlue;
  end;
  // Reset the game scores
  edPlayer.Text := '0'; edComputer.Text := '0';
  // Make sure the button is enabled and has the correct caption
  btStart.Caption := 'Start'; btStart.Enabled := True;
end;

{ Menu "Game > Exit": Exit application }

procedure TfTicTacToe.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Player mark > ...": Choose player's mark and color }

procedure TfTicTacToe.mOptionsPlayerMarkOBlueClick(Sender: TObject);

begin
  mOptionsPlayerMarkOBlue.Checked := True;  mOptionsPlayerMarkORed.Checked := False;
  mOptionsPlayerMarkXBlue.Checked := False; mOptionsPlayerMarkXRed.Checked := False;
end;

procedure TfTicTacToe.mOptionsPlayerMarkORedClick(Sender: TObject);

begin
  mOptionsPlayerMarkOBlue.Checked := False; mOptionsPlayerMarkORed.Checked := True;
  mOptionsPlayerMarkXBlue.Checked := False; mOptionsPlayerMarkXRed.Checked := False;
end;

procedure TfTicTacToe.mOptionsPlayerMarkXBlueClick(Sender: TObject);

begin
  mOptionsPlayerMarkOBlue.Checked := False; mOptionsPlayerMarkORed.Checked := False;
  mOptionsPlayerMarkXBlue.Checked := True;  mOptionsPlayerMarkXRed.Checked := False;
end;

procedure TfTicTacToe.mOptionsPlayerMarkXRedClick(Sender: TObject);

begin
  mOptionsPlayerMarkOBlue.Checked := False; mOptionsPlayerMarkORed.Checked := False;
  mOptionsPlayerMarkXBlue.Checked := False; mOptionsPlayerMarkXRed.Checked := True;
end;

{ Menu items "Options > First move > ...": Choose who should move first }

procedure TfTicTacToe.mOptionsFirstMovePlayerClick(Sender: TObject);

begin
  mOptionsFirstMovePlayer.Checked := True; mOptionsFirstMoveComputer.Checked := False; mOptionsFirstMoveAlternate.Checked := False;
end;

procedure TfTicTacToe.mOptionsFirstMoveComputerClick(Sender: TObject);

begin
  mOptionsFirstMovePlayer.Checked := False; mOptionsFirstMoveComputer.Checked := True; mOptionsFirstMoveAlternate.Checked := False;
end;

procedure TfTicTacToe.mOptionsFirstMoveAlternateClick(Sender: TObject);

begin
  mOptionsFirstMovePlayer.Checked := False; mOptionsFirstMoveComputer.Checked := False; mOptionsFirstMoveAlternate.Checked := True;
end;

{ Menu item "Help > Help": Display (short) application help text }

procedure TfTicTacToe.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'The aim of the Tic-Tac-Toe game is to align 3 of your own marks on a horizontal, vertical or diagonal line. ';
  S += 'To play the game push the "Start/Next" button to start the game. Click on a board field to mark it. The computer ';
  S += 'moves as soon as you have done. Use "New" in the "Game" menu to reset the scores.';
  MessageDlg('"TicTacToe" Help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfTicTacToe.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Tic-Tac-Toe.' + LineEnding;
  S += 'Free PC version of the classic Tic-Tac-Toe game.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2021.';
  MessageDlg('About "TicTacToe"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Start new game }

procedure TfTicTacToe.btStartClick(Sender: TObject);

begin
  BoardReset(aBoard, stBoard);                                                 // clear the board
  if btStart.Caption = 'Start' then begin
    // Some initialisations to do if it is the first game of a game series
    if mOptionsFirstMovePlayer.Checked then
      sActualPlayer0 := 'Player'
    else if mOptionsFirstMoveComputer.Checked then
      sActualPlayer0 := 'Computer'
    else begin
      // In alternate play mode, randomly choose who has to begin the series
      if sActualPlayer0 = '' then begin
        if Random(2) = 0 then
          sActualPlayer0 := 'Player'
        else
          sActualPlayer0 := 'Computer';
      end;
    end;
    // Set button caption to next for subsequent games of this series
    btStart.Caption := 'Next';
  end
  else begin
    // In alternate play mode, let begin players in alternate way
    if mOptionsFirstMoveAlternate.Checked then begin
      if sActualPlayer0 = 'Player' then
        sActualPlayer0 := 'Computer'
      else
        sActualPlayer0 := 'Player';
    end;
  end;
  sActualPlayer := sActualPlayer0;
  if sActualPlayer = 'Computer' then begin
    // If the computer has to play first, let it do a random move (Boolean argument set to True)
    ComputerMove(sActualPlayer, cPlayerMark, cComputerMark, iComputerColor, aBoard, stBoard, True);
  end;
  // Disable "Next" button, until this game is over (game is played by user clicking the board fields)
  btStart.Enabled := False;
end;

{ User click on board fields (shapes or labels): Do player move for this field, followed by computer response }

procedure TfTicTacToe.stField1Click(Sender: TObject);

begin
  rField.Row := 0; rField.Col := 0;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField2Click(Sender: TObject);

begin
  rField.Row := 0; rField.Col := 1;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField3Click(Sender: TObject);

begin
  rField.Row := 0; rField.Col := 2;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField4Click(Sender: TObject);

begin
  rField.Row := 1; rField.Col := 0;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField5Click(Sender: TObject);

begin
  rField.Row := 1; rField.Col := 1;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField6Click(Sender: TObject);

begin
  rField.Row := 1; rField.Col := 2;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField7Click(Sender: TObject);

begin
  rField.Row := 2; rField.Col := 0;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField8Click(Sender: TObject);

begin
  rField.Row := 2; rField.Col := 1;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.stField9Click(Sender: TObject);

begin
  rField.Row := 2; rField.Col := 2;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField1MouseDown(Sender: TObject);

begin
  rField.Row := 0; rField.Col := 0;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField2MouseDown(Sender: TObject);

begin
  rField.Row := 0; rField.Col := 1;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField3MouseDown(Sender: TObject);

begin
  rField.Row := 0; rField.Col := 2;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField4MouseDown(Sender: TObject);

begin
  rField.Row := 1; rField.Col := 0;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField5MouseDown(Sender: TObject);

begin
  rField.Row := 1; rField.Col := 1;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField6MouseDown(Sender: TObject);

begin
  rField.Row := 1; rField.Col := 2;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField7MouseDown(Sender: TObject);

begin
  rField.Row := 2; rField.Col := 0;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField8MouseDown(Sender: TObject);

begin
  rField.Row := 2; rField.Col := 1;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

procedure TfTicTacToe.shField9MouseDown(Sender: TObject);

begin
  rField.Row := 2; rField.Col := 2;
  PlayerMove(sActualPlayer, cPlayerMark, cComputerMark, iPlayerColor, iComputerColor, rField, aBoard, stBoard);
end;

end.

