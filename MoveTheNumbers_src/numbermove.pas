{********************************************}
{* Main unit for MoveTheNumbers application *}
{********************************************}

unit numbermove;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type
  TArray2 = array[1..4, 1..4] of Integer;
  TMoves = array of record
    Row, Col: Integer;
  end;
  TShArray2 = array[1..4, 1..4] of TShape;
  TStArray2 = array[1..4, 1..4] of TStaticText;
  {**************}
  { TfNumberMove }
  {**************}
  TfNumberMove = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsLevel, mOptionsLevel1, mOptionsLevel2, mOptionsLevel3: TMenuItem;
    mOptionsInterval, mOptionsInterval5, mOptionsInterval4, mOptionsInterval3, mOptionsInterval2, mOptionsInterval1: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laMoves, laMove: TLabel;
    shGrid11, shGrid12, shGrid13, shGrid14, shGrid21, shGrid22, shGrid23, shGrid24: TShape;
    shGrid31, shGrid32, shGrid33, shGrid34, shGrid41, shGrid42, shGrid43, shGrid44: TShape;
    stGrid11, stGrid12, stGrid13, stGrid14, stGrid21, stGrid22, stGrid23, stGrid24: TStaticText;
    stGrid31, stGrid32, stGrid33, stGrid34, stGrid41, stGrid42, stGrid43, stGrid44: TStaticText;
    btStart: TButton;
    tiShow: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsLevel1Click(Sender: TObject);
    procedure mOptionsLevel2Click(Sender: TObject);
    procedure mOptionsLevel3Click(Sender: TObject);
    procedure mOptionsInterval1Click(Sender: TObject);
    procedure mOptionsInterval2Click(Sender: TObject);
    procedure mOptionsInterval3Click(Sender: TObject);
    procedure mOptionsInterval4Click(Sender: TObject);
    procedure mOptionsInterval5Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure stGrid11Click(Sender: TObject);
    procedure stGrid12Click(Sender: TObject);
    procedure stGrid13Click(Sender: TObject);
    procedure stGrid14Click(Sender: TObject);
    procedure stGrid21Click(Sender: TObject);
    procedure stGrid22Click(Sender: TObject);
    procedure stGrid23Click(Sender: TObject);
    procedure stGrid24Click(Sender: TObject);
    procedure stGrid31Click(Sender: TObject);
    procedure stGrid32Click(Sender: TObject);
    procedure stGrid33Click(Sender: TObject);
    procedure stGrid34Click(Sender: TObject);
    procedure stGrid41Click(Sender: TObject);
    procedure stGrid42Click(Sender: TObject);
    procedure stGrid43Click(Sender: TObject);
    procedure stGrid44Click(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tiShowTimer(Sender: TObject);
  private
    iLevel, iLevel0, iInterval, iFreeRow, iFreeCol, iMove: Integer;
    aGrid, aGrid0: TArray2;
    aMoves: TMoves;
    shGrid: TShArray2;
    stGrid: TStArray2;
  end;

var
  fNumberMove: TfNumberMove;

implementation

{$R *.lfm}

{ Reset the grid array with numbers in sequence }

procedure GridReset(var Grid: TArray2);

var
  N, I, J: Integer;

begin
  N := 0;
  for I := 1 to 4 do begin
    for J := 1 to 4 do begin
      Inc(N); Grid[I, J] := N;
    end;
  end;
  Grid[4, 4] := -1;                                                            // -1 is used for the empty field
end;

{ Create random grid array }

procedure GridCreate(Level: Integer; var Grid: TArray2; out Moves: TMoves);

var
  Min, Max, R, OldR, C, OldC, FR, FC, N: Integer;
  OK: Boolean;

begin
  // Number of moves: level limits
  case Level of
    1: begin
      Min := 10; Max := 20;
    end;
    2: begin
      Min := 20; Max := 40;
    end;
    3: begin
      Min := 40; Max := 80;
    end;
  end;
  // Repeat whole procedure until the number of moves is between level limits
  repeat
    GridReset(Grid);
    FR := 4; FC := 4;
    N := 0; OldR := 0; OldC := 0;
    SetLength(Moves, 0);
    // Starting from the sequence situation, randomly move the tiles around
    // until the last field of the grid has become an empty field again
    repeat
      Inc(N);
      // Random move (left, right, top, bottom)
      repeat
        OK := True;
        R := Random(3) - 1; C := Random(3) - 1;
        // Eliminate invalid and unwanted moves
        if (FR + R < 1) or (FR + R > 4) then                                   // can't move horizontally out of the grid
          OK := False
        else if (FC + C < 1) or (FC + C > 4) then                              // can't move vartically out of the grid
          OK := False
        else if (R = 0) and (C = 0) then                                       // can't move onto itself
          OK := False
        else if (R <> 0) and (C <> 0) then                                     // can't move diagonally
          OK := False
        else if (R = -OldR) and (C = -OldC) then                               // do not allow to move back from where it came
          OK := False;
      until OK;
      // Save the move made
      SetLength(Moves, N);
      Moves[N - 1].Row := FR;
      Moves[N - 1].Col := FC;
      // Update the grid array
      Grid[FR, FC] := Grid[FR + R, FC + C];
      Grid[FR + R, FC + C] := -1;
      // New coordinates of empty field
      FR += R; FC += C;
      // Save displacements values (needed to code for: disallow next move to come back to previous position)
      OldR := R; OldC := C;
    until (FR = 4) and (FC = 4);
  until (N >= Min) and (N <= Max);
end;

{ Display the tile grid }

procedure GridDisplay(var Grid: TArray2; var ShGrid: TShArray2; var StGrid: TStArray2);

var
  I, J: Integer;

begin
  for I := 1 to 4 do begin
    for J := 1 to 4 do begin
      if Grid[I, J] = -1 then begin
        // Empty field
        ShGrid[I, J].Brush.Color := clWhite;
        StGrid[I, J].Caption := '';
      end
      else begin
        // Tile
        ShGrid[I, J].Brush.Color := clYellow;
        StGrid[I, J].Caption := IntToStr(Grid[I, J]);
      end;
    end;
  end;
end;

{ Tile moving (user click on given tile) }

procedure NumberMove(Row, Col: Integer; var FreeRow, FreeCol, Move: Integer; var Grid: TArray2; var ShGrid: TShArray2; var StGrid: TStArray2);

var
  MR, MC: Integer;

begin
  if fNumberMove.btStart.Caption = 'Done' then begin
    // Only do if there is a game running
    MR := Row - FreeRow; MC := Col - FreeCol;
    if ((Abs(MR) = 1) and (MC = 0)) or ((Abs(MC) = 1) and (MR = 0)) then begin
      // The only tiles that may be moved are the 4 ones around the empty field
      Grid[FreeRow, FreeCol] := Grid[Row, Col];                                // move tile to empty field
      Grid[Row, Col] := -1;                                                    // tile field is now empty
      FreeRow := Row; FreeCol := Col;                                          // new empty field coordinates
      GridDisplay(Grid, ShGrid, StGrid);                                       // display the grid
      Inc(Move);
      fNumberMove.laMove.Caption := 'Actual move: ' + IntToStr(Move);
    end;
  end;
end;

{**************}
{ TfNumberMove }
{**************}

{ Application start: Initialization }

procedure TfNumberMove.FormCreate(Sender: TObject);

begin
  // Create array with tile shapes
  shGrid[1, 1] := shGrid11; shGrid[1, 2] := shGrid12; shGrid[1, 3] := shGrid13; shGrid[1, 4] := shGrid14;
  shGrid[2, 1] := shGrid21; shGrid[2, 2] := shGrid22; shGrid[2, 3] := shGrid23; shGrid[2, 4] := shGrid24;
  shGrid[3, 1] := shGrid31; shGrid[3, 2] := shGrid32; shGrid[3, 3] := shGrid33; shGrid[3, 4] := shGrid34;
  shGrid[4, 1] := shGrid41; shGrid[4, 2] := shGrid42; shGrid[4, 3] := shGrid43; shGrid[4, 4] := shGrid44;
  // Create array with tile static texts (numbers)
  stGrid[1, 1] := stGrid11; stGrid[1, 2] := stGrid12; stGrid[1, 3] := stGrid13; stGrid[1, 4] := stGrid14;
  stGrid[2, 1] := stGrid21; stGrid[2, 2] := stGrid22; stGrid[2, 3] := stGrid23; stGrid[2, 4] := stGrid24;
  stGrid[3, 1] := stGrid31; stGrid[3, 2] := stGrid32; stGrid[3, 3] := stGrid33; stGrid[3, 4] := stGrid34;
  stGrid[4, 1] := stGrid41; stGrid[4, 2] := stGrid42; stGrid[4, 3] := stGrid43; stGrid[4, 4] := stGrid44;
  // Startup options
  iLevel0 := 1; iInterval := 500;
  // Start random number generator
  Randomize;
end;

{ Menu item "Game > New": Start a new game }

procedure TfNumberMove.mGameNewClick(Sender: TObject);

begin
  tiShow.Enabled := False;                                                     // stop timer (that may eventually be active)
  iLevel := iLevel0;                                                           // level setting becomes active now
  GridCreate(iLevel, aGrid, aMoves);                                           // generate random grid
  aGrid0 := aGrid;                                                             // save initial grid (needed for "show" feature)
  GridDisplay(aGrid0, shGrid, stGrid);                                         // display the grid
  iFreeRow := 4; iFreeCol := 4;                                                // last field is the empty one
  iMove := 0;
  laMoves.Caption := 'Maximum moves: ' + IntToStr(Length(aMoves));
  laMove.Caption := 'Actual move:';
  btStart.Caption := 'Done';
end;

{ Menu item "Game > Exit": Exit application }

procedure TfNumberMove.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Maximum moves > ...": User selection of the game level }

procedure TfNumberMove.mOptionsLevel1Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := True; mOptionsLevel2.Checked := False; mOptionsLevel3.Checked := False;
  iLevel0 := 1;
end;

procedure TfNumberMove.mOptionsLevel2Click(Sender: TObject);

begin
  mOptionsLevel2.Checked := True; mOptionsLevel1.Checked := False; mOptionsLevel3.Checked := False;
  iLevel0 := 2;
end;

procedure TfNumberMove.mOptionsLevel3Click(Sender: TObject);

begin
  mOptionsLevel3.Checked := True; mOptionsLevel1.Checked := False; mOptionsLevel2.Checked := False;
  iLevel0 := 3;
end;

{ Menu item "Options > Show interval > ...": User selection of the "show feature" speed }

procedure TfNumberMove.mOptionsInterval1Click(Sender: TObject);

begin
  mOptionsInterval1.Checked := True; mOptionsInterval2.Checked := False; mOptionsInterval3.Checked := False;
  mOptionsInterval4.Checked := False; mOptionsInterval5.Checked := False;
  iInterval := 500;
end;

procedure TfNumberMove.mOptionsInterval2Click(Sender: TObject);

begin
  mOptionsInterval1.Checked := False; mOptionsInterval2.Checked := True; mOptionsInterval3.Checked := False;
  mOptionsInterval4.Checked := False; mOptionsInterval5.Checked := False;
  iInterval := 1000;
end;

procedure TfNumberMove.mOptionsInterval3Click(Sender: TObject);

begin
  mOptionsInterval1.Checked := False; mOptionsInterval2.Checked := False; mOptionsInterval3.Checked := True;
  mOptionsInterval4.Checked := False; mOptionsInterval5.Checked := False;
  iInterval := 1500;
end;

procedure TfNumberMove.mOptionsInterval4Click(Sender: TObject);

begin
  mOptionsInterval1.Checked := False; mOptionsInterval2.Checked := False; mOptionsInterval3.Checked := False;
  mOptionsInterval4.Checked := True; mOptionsInterval5.Checked := False;
  iInterval := 2000;
end;

procedure TfNumberMove.mOptionsInterval5Click(Sender: TObject);

begin
  mOptionsInterval1.Checked := False; mOptionsInterval2.Checked := False; mOptionsInterval3.Checked := False;
  mOptionsInterval4.Checked := False; mOptionsInterval5.Checked := True;
  iInterval := 2500;
end;

{ Menu item "Help > Help": Display application help }

procedure TfNumberMove.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'The aim of the game is to move the tiles in a way that the numbers form an ascending sequence ';
  S += '(from left to right and from top to bottom). A tile can be moved if there is a free space at its ';
  S += 'left, right, top or bottom side. To move a tile, just click it; when finished, push the "Done" button.';
  S += 'When the button caption is "Show", you can view the computer moving the tiles in a number of moves equal ';
  S += 'to the one indicated. Use "New" in the "Game" menu to make new level settings active and start a new game. Enjoy!';
  MessageDlg('"MoveTheNumbers" Help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfNumberMove.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Game: Move the tiles until the numbers form a sequence from 1 to 15.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March 2023.';
  MessageDlg('About "MoveTheNumbers"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Done/Show/Stop": Perform corresponding action }

procedure TfNumberMove.btStartClick(Sender: TObject);

var
  N, I, J: Integer;
  OK: Boolean;

begin
  // Button "Start": Start a new game (only used at application start)
  if btStart.Caption = 'Start' then begin
    mGameNew.Click;
    btStart.Caption := 'Done';
  end
  // Button "Done": Check if tiles are well in sequence
  else if btStart.Caption = 'Done' then begin
    OK := True; N := 0;
    for I := 1 to 4 do begin
      for J := 1 to 4 do begin
        Inc(N);
        if stGrid[I, J].Caption <> '' then begin
          if StrToInt(stGrid[I, J].Caption) <> N then
            OK := False;
        end;
      end;
    end;
    // Display message if puzzle has been solved or not
    if OK then
      MessageDlg('Move the Numbers', 'Congratulations! You have successfully ordered the numbers!', mtInformation, [mbOK], 0)
    else
      MessageDlg('Move the Numbers', 'Sorry. But, this is not the correct number sequence!', mtInformation, [mbOK], 0);
    btStart.Caption := 'Show';
  end
  // Button "Show": Play back the moves (as computer did when generating the grid)
  // To note that this is a solution, but there may be faster ones!
  else if btStart.Caption = 'Show' then begin
    aGrid := aGrid0;
    GridDisplay(aGrid, shGrid,stGrid);
    laMove.Caption := 'Actual move:';
    iFreeRow := 4; iFreeCol := 4;
    iMove := Length(aMoves) - 1;
    tiShow.Interval := iInterval;
    tiShow.Enabled := True;
    btStart.Caption := 'Stop';
  end
  // Button "Stop": Stop move playback
  else begin
    tiShow.Enabled := False;
    btStart.Caption := 'Show';
  end;
end;

{ Tile clicked by user: Move it (if it may be moved) }

procedure TfNumberMove.stGrid11Click(Sender: TObject);

begin
  NumberMove(1, 1, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid12Click(Sender: TObject);

begin
  NumberMove(1, 2, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid13Click(Sender: TObject);

begin
  NumberMove(1, 3, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid14Click(Sender: TObject);

begin
  NumberMove(1, 4, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid21Click(Sender: TObject);

begin
  NumberMove(2, 1, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid22Click(Sender: TObject);

begin
  NumberMove(2, 2, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid23Click(Sender: TObject);

begin
  NumberMove(2, 3, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid24Click(Sender: TObject);

begin
  NumberMove(2, 4, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid31Click(Sender: TObject);

begin
  NumberMove(3, 1, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid32Click(Sender: TObject);

begin
  NumberMove(3, 2, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid33Click(Sender: TObject);

begin
  NumberMove(3, 3, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid34Click(Sender: TObject);

begin
  NumberMove(3, 4, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid41Click(Sender: TObject);

begin
  NumberMove(4, 1, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid42Click(Sender: TObject);

begin
  NumberMove(4, 2, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid43Click(Sender: TObject);

begin
  NumberMove(4, 3, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

procedure TfNumberMove.stGrid44Click(Sender: TObject);

begin
  NumberMove(4, 4, iFreeRow, iFreeCol, iMove, aGrid, shGrid, stGrid);
end;

{ Timer routine for move playback }

procedure TfNumberMove.tiShowTimer(Sender: TObject);

begin
  // Move tile to empty field
  aGrid[iFreeRow, iFreeCol] := aGrid[aMoves[iMove].Row, aMoves[iMove].Col];
  // Field with moved tile is the empty field noe
  aGrid[aMoves[iMove].Row, aMoves[iMove].Col] := -1;
  // New empty field coordinates
  iFreeRow := aMoves[iMove].Row; iFreeCol := aMoves[iMove].Col;
  // Display the grid
  GridDisplay(aGrid, shGrid,stGrid);
  // If there are moves left, set moves array index to next element (in fact previous one, as playback goes from end to begin of array)
  if iMove >= 0 then begin
    laMove.Caption := 'Actual move: ' + IntToStr(Length(aMoves) - iMove);
    Dec(iMove);
  end;
  // If all moves have been done, terminate the "show feature" by disabling the timer
  if iMove < 0 then begin
    tiShow.Enabled := False;
    btStart.Caption := 'Show';
  end;
end;

end.

