{***************************************}
{* Main unit for Marienbad application *}
{***************************************}

unit mbnim;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LCLIntf;

type
  {*************}
  { TfMarienbad }
  {*************}
  TfMarienbad = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsVariant, mOptionsVariantNim, mOptionsVariantMisere: TMenuItem;
    mOptionsStrength, mOptionsStrength1, mOptionsStrength2, mOptionsStrength3: TMenuItem;
    mOptionsFirstMove, mOptionsFirstMoveComputer, mOptionsFirstMovePlayer, mOptionsFirstMoveRandom: TMenuItem;
    MenuItem1, mOptionsMatches, mOptionsMatchesRemove, mOptionsMatchesStrikethrough: TMenuItem;
    mHelp, mHelpApplication, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    imMatch1, imMatch2, imMatch3, imMatch4, imMatch5: TImage;
    imMatch6, imMatch7, imMatch8, imMatch9, imMatch10: TImage;
    imMatch11, imMatch12, imMatch13, imMatch14, imMatch15, imMatch16: TImage;
    shMatch1, shMatch2, shMatch3, shMatch4, shMatch5: TShape;
    shMatch6, shMatch7, shMatch8, shMatch9, shMatch10: TShape;
    shMatch11, shMatch12, shMatch13, shMatch14, shMatch15, shMatch16: TShape;
    laComputer: TLabel;
    edPlayer, edPlayer1, edPlayer2, edPlayer3, edPlayer4: TEdit;
    edComputer1, edComputer2, edComputer3, edComputer4: TEdit;
    btAction: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsVariantNimClick(Sender: TObject);
    procedure mOptionsVariantMisereClick(Sender: TObject);
    procedure mOptionsStrength1Click(Sender: TObject);
    procedure mOptionsStrength2Click(Sender: TObject);
    procedure mOptionsStrength3Click(Sender: TObject);
    procedure mOptionsFirstMoveComputerClick(Sender: TObject);
    procedure mOptionsFirstMovePlayerClick(Sender: TObject);
    procedure mOptionsFirstMoveRandomClick(Sender: TObject);
    procedure mOptionsMatchesRemoveClick(Sender: TObject);
    procedure mOptionsMatchesStrikethroughClick(Sender: TObject);
    procedure mHelpApplicationClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure imMatch1Click(Sender: TObject);
    procedure imMatch2Click(Sender: TObject);
    procedure imMatch3Click(Sender: TObject);
    procedure imMatch4Click(Sender: TObject);
    procedure imMatch5Click(Sender: TObject);
    procedure imMatch6Click(Sender: TObject);
    procedure imMatch7Click(Sender: TObject);
    procedure imMatch8Click(Sender: TObject);
    procedure imMatch9Click(Sender: TObject);
    procedure imMatch10Click(Sender: TObject);
    procedure imMatch11Click(Sender: TObject);
    procedure imMatch12Click(Sender: TObject);
    procedure imMatch13Click(Sender: TObject);
    procedure imMatch14Click(Sender: TObject);
    procedure imMatch15Click(Sender: TObject);
    procedure imMatch16Click(Sender: TObject);
  private
    iGameLevel, iGameLevel0, iCountTaken: Integer;
    sGameVariant, sMoveFirst, sGameVariant0, sMoveFirst0, sActualPlayer: string;
    aMatches: array[0..3] of Integer;
    aMatchesTaken: array[0..3] of Boolean;
    imMatches: array[0..15] of TImage;
    shMatches: array[0..15] of TShape;
    edPlayerMatches, edComputerMatches: array[0..3] of TEdit;
  end;

var
  fMarienbad: TfMarienbad;

implementation

{$R *.lfm}

{ Reset player controls on the form}

procedure ResetPlayers(ActualPlayer: string; var ComputerEdt, PlayerEdt: array of TEdit;
  var MatchShapes: array of TShape; var Taken: array of Boolean; Clear: Boolean);

var
  I: Integer;

begin
  // Highlighting of actual player
  fMarienbad.laComputer.Color := clNone; fMarienbad.edPlayer.Color := clDefault;
  if ActualPlayer = 'computer' then
    fMarienbad.laComputer.Color := clAqua
  else
    fMarienbad.edPlayer.Color := clAqua;
  // Reset of all player strike-through bars color to black
  for I := 0 to 15 do begin
    if MatchShapes[I].Brush.Color = clRed then
      MatchShapes[I].Brush.Color := clBlack;
  end;
  // Clear of players matches taken edit fields (if this has to be done)
  if Clear then begin
    for I := 0 to 3 do begin
      ComputerEdt[I].Text := '';
      PlayerEdt[I].Text := '';
      Taken[I] := False;
    end;
  end;
end;

{ Removing of the match, the player has clicked on }

procedure RemoveMatch(var MatchPics: array of TImage; var MatchShapes: array of TShape; var Matches: array of Integer;
  var ComputerEdt, PlayerEdt: array of TEdit; var Taken: array of Boolean; ActualPlayer: string; Heap, Match: Integer; var Count: Integer);

var
  I: Integer;
  OK: Boolean;

begin
  // If it's the first match clicked on for this turn, reset the player controls on the form
  if Count = 0 then
    ResetPlayers(ActualPlayer, ComputerEdt, PlayerEdt, MatchShapes, Taken, True);
  // If the click is on a match removed during this turn, undo the removal action
  if fMarienbad.mOptionsMatchesStrikethrough.Checked and MatchShapes[Match].Visible and (MatchShapes[Match].Brush.Color = clRed) then begin
    MatchShapes[Match].Visible := False;
    Inc(Matches[Heap]); Dec(Count);
    if Count = 0 then
      PlayerEdt[Heap].Text := ''
    else
      PlayerEdt[Heap].Text := IntToStr(Count);
    if Count = 0 then
      Taken[Heap] := False;
  end
  // Normal case: Click on the match -> remove this match from the game
  else begin
    if fMarienbad.mOptionsMatchesStrikethrough.Checked and MatchShapes[Match].Visible then
      MessageDlg('Marienbad', 'This match has already been removed!', mtError, [mbOK], 0)
    // Remove the match. If it is on the same heap as the matches already removed during this turn!
    else begin
      OK := True;
      for I := 0 to 3 do begin
        // Illegal move: There was a match taken from another heap before
        if (I <> Heap) and Taken[I] then
          OK := False;
      end;
      // Remove the match
      if OK then begin
        if fMarienbad.mOptionsMatchesRemove.Checked then
          MatchPics[Match].Visible := False
        else begin
          MatchShapes[Match].Visible := True;
          fMarienbad.shMatches[Match].Brush.Color := clRed;
        end;
        Dec(Matches[Heap]);                                                    // remaining number of matches in this row
        Inc(Count);                                                            // number of matches taken by player during this turn
        PlayerEdt[Heap].Text := IntToStr(Count);
        Taken[Heap] := True;                                                   // mark this row as the one, from where a match has been taken
      end
      // Illegal move error message
      else
        MessageDlg('Marienbad', 'All matches must be taken from the same row!', mtError, [mbOK], 0);
    end;
  end;
end;

{ Computer random move }

procedure ComputerRandomMove(var Matches: array of Integer; out Heap, Take: Integer);

var
  R: Integer;

begin
  // Choose a ranom row
  repeat
    Heap := Random(4);
  until Matches[Heap] > 0;
  // Choose a random number of matches (preference for 1 or 2 matches)
  R := Random(3);
  if R = 0 then
    // Take 1 match
    Take := 1
  else if R = 1 then begin
    // Take 2 matches
    if Matches[Heap] < 2 then
      Take := 1                                                                // can't take 2 matches if there is only 1!
    else
      Take := 2;
  end
  else begin
    // Randomly take between 1 and 5 matches
    repeat
      Take := Random(5) + 1;
    until Take <= Matches[Heap];                                               // can't take more matches than there are in the row!
  end;
  Take := -Take;                                                               // random moves returned as negative number allows to see what kind of move the program made
end;

{ Computer winning move }

procedure ComputerWinningMove(GameVariant: string; var Matches: array of Integer; out Heap, Take: Integer);

// If no winning move can be found, the procedure calls ComputerRandomMove for this move

var
  N, Ones, MoreThanOnes, Loop, I, J: Integer;
  EndGame, Win: Boolean;
  Matches2: array[0..3] of Integer;
  MatchesBin: array[0..3, 0..2] of Byte;
  NimSums: array[0..2] of Byte;

begin
  // If the Misere variant is played, check if the standard or the end-of-game algorithm has to be used
  EndGame := False;
  if GameVariant = 'Misere' then begin
    MoreThanOnes := 0; Ones := 0;
    for I := 0 to 3 do begin
      if Matches[I] > 1 then
        Inc(MoreThanOnes);                                                     // rows with more than 1 match
      if Matches[I] = 1 then
        Inc(Ones);                                                             // rows with exactly 1 match (used by the end-of-game algorithm)
    end;
    // The end-of-game algorithm has to be used if there is only 1 row with more than 1 match left
    if MoreThanOnes <= 1 then
      EndGame := True;
  end;
  // End-of-game algorithm (to be used only with Misere variant)
  if EndGame then begin
    Heap := -1;
    // Take the matches from the row with more than 1 match
    for I := 0 to 3 do begin
      if Matches[I] > 1 then
        Heap := I;
    end;
    if Heap = -1 then begin
      // If there isn't such a row, choose a random one and take the 1 match left in that row
      repeat
        Heap := Random(4);
      until Matches[Heap] > 0;
      Take := 1;
    end
    else begin
      // In the row with more than 1 match:
      //  - Take all matches, if the number of rows with 1 match is odd
      //  - Take all matches, but 1, if the number of rows with 1 match is even
      if Ones mod 2 = 0 then
        Take := Matches[Heap] - 1
      else
        Take := Matches[Heap];
    end;
  end
  // Standard algorithm (always applicable with Nim variant)
  else begin
    // For code simplicity, the algorithm takes a random number of matches from a random row and checks if this is a winning move
    // This procedure is continued until a winning move has been found or if the loop counter exceeds 1000 (play of random move in this case)
    Loop := 0;
    repeat
      Inc(Loop);
      for I := 0 to 3 do
        Matches2[I] := Matches[I];
      for I := 0 to 3 do begin
        for J := 0 to 2 do
          MatchesBin[I, J] := 0;
      end;
      for I := 0 to 2 do
        NimSums[I] := 0;
      repeat
        Heap := Random(4);                                                     // random row (must contain at least 1 match)
      until Matches2[Heap] > 0;
      Take := Random(Matches2[Heap]) + 1;                                      // random number of matches (between 1 and total in the row)
      // Standard Nim algrithm:
      // For the matches left after this move has been made, transform the number of matches per row to binary (x*4 + y*2 + z*1),
      // then calculate the Nim sums for 1, 2 and 4
      // If all Nim sums are even numbers, this move will win the game
      Matches2[Heap] -= Take;                                                  // matches present, after (!) this move has been made
      // Transform number of matches to binary
      for I := 0 to 3 do begin
        N := Matches2[I];
        if N >= 4 then begin
          MatchesBin[I, 0] := 1;
          N -= 4;
        end;
        if N >= 2 then begin
          MatchesBin[I, 1] := 1;
          N -= 2;
        end;
        if N = 1 then begin
          MatchesBin[I, 2] := 1;
        end;
      end;
      // Calculate the Nim sums
      for J := 0 to 2 do begin
        for I := 0 to 3 do begin
          if MatchesBin[I, J] = 1 then
            Inc(NimSums[J]);
        end;
      end;
      // Check if the move is a winning one
      Win := True;
      for I := 0 to 2 do begin
        if NimSums[I] mod 2 = 1 then                                           // if there is one Nim sum that is an odd number, this move does not win
          Win := False;
      end;
    until Win or (Loop >= 1000);                                               // try to find a winning move for a maximum number of times
    // If there has no winning move been found, suppose there isn't any and make a random move
    // This is perhaps not 100% sure, because theoratically even after 1000 trials, the winning move could be missed
    // To get rid of this theoratically possible issue, checking if there actually is a winning move would have to be done
    if Loop >= 1000 then
      ComputerRandomMove(Matches, Heap, Take);
  end;
end;

{*************}
{ TfMarienbad }
{*************}

{ Application start: Initialisation }

procedure TfMarienbad.FormCreate(Sender: TObject);

begin
  // Create arrays with match images and strike-through shapes
  imMatches[0]  := imMatch1;  imMatches[1]  := imMatch2;  imMatches[2]  := imMatch3;  imMatches[3]  := imMatch4;
  imMatches[4]  := imMatch5;  imMatches[5]  := imMatch6;  imMatches[6]  := imMatch7;  imMatches[7]  := imMatch8;
  imMatches[8]  := imMatch9;  imMatches[9]  := imMatch10; imMatches[10] := imMatch11; imMatches[11] := imMatch12;
  imMatches[12] := imMatch13; imMatches[13] := imMatch14; imMatches[14] := imMatch15; imMatches[15] := imMatch16;
  shMatches[0]  := shMatch1;  shMatches[1]  := shMatch2;  shMatches[2]  := shMatch3;  shMatches[3]  := shMatch4;
  shMatches[4]  := shMatch5;  shMatches[5]  := shMatch6;  shMatches[6]  := shMatch7;  shMatches[7]  := shMatch8;
  shMatches[8]  := shMatch9;  shMatches[9]  := shMatch10; shMatches[10] := shMatch11; shMatches[11] := shMatch12;
  shMatches[12] := shMatch13; shMatches[13] := shMatch14; shMatches[14] := shMatch15; shMatches[15] := shMatch16;
  // Create arrays with computer and player edit fields
  edPlayerMatches[0] := edPlayer1; edPlayerMatches[1] := edPlayer2;
  edPlayerMatches[2] := edPlayer3; edPlayerMatches[3] := edPlayer4;
  edComputerMatches[0] := edComputer1; edComputerMatches[1] := edComputer2;
  edComputerMatches[2] := edComputer3; edComputerMatches[3] := edComputer4;
  // Default game settings at application start
  sGameVariant0 := 'Nim'; iGameLevel0 := 2; sMoveFirst0 := 'computer';
  // Start random number generator
  Randomize;
  // Start a new game (by simulation the selection of "New" in the "Game" menu
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new Marienbad game }

procedure TfMarienbad.mGameNewClick(Sender: TObject);

var
  I: Integer;

begin
  sGameVariant := sGameVariant0; iGameLevel := iGameLevel0; sMoveFirst := sMoveFirst0;  // game options now becoming active
  stTitle.Caption := 'Marienbad game (' + sGameVariant + ' variant).';
  for I := 0 to 15 do begin
    imMatches[I].Visible := True;                                              // show all matches
    shMatches[I].Visible := False;                                             // hide all strike-through bars
  end;
  laComputer.Color := clNone; edPlayer.Color := clDefault;
  // Determine who plays first
  if (sMoveFirst = 'computer') or (sMoveFirst = 'player') then
    sActualPlayer := sMoveFirst
  else if sMoveFirst = 'random' then begin
    // Random first move player
    if Random(2) = 0 then
      sActualPlayer := 'computer'
    else
      sActualPlayer := 'player';
  end;
  // Initialise the game
  aMatches[0] := 1; aMatches[1] := 3; aMatches[2] := 5; aMatches[3] := 7; iCountTaken := 0;
  ResetPlayers(sActualPlayer, edComputerMatches, edPlayerMatches, shMatches, aMatchesTaken, True);
  // Set button caption to "Start" if computer begins
  if sActualPlayer = 'computer' then
    btAction.Caption := 'Start'
  else
    btAction.Caption := 'Nim';
  btAction.Enabled := True;
end;

{ Menu item "Game > Exit": Exit application }

procedure TfMarienbad.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Game variant > ...": Select Nim or Misere variant }

procedure TfMarienbad.mOptionsVariantNimClick(Sender: TObject);

begin
  mOptionsVariantNim.Checked := True; mOptionsVariantMisere.Checked := False;
  sGameVariant0 := 'Nim';
end;

procedure TfMarienbad.mOptionsVariantMisereClick(Sender: TObject);

begin
  mOptionsVariantNim.Checked := False; mOptionsVariantMisere.Checked := True;
  sGameVariant0 := 'Misere';
end;

{ Menu items "Options > Computer strength > ...": Select computer strength: Beginner, Intermediate, Expert }

procedure TfMarienbad.mOptionsStrength1Click(Sender: TObject);

begin
  mOptionsStrength1.Checked := True; mOptionsStrength2.Checked := False; mOptionsStrength3.Checked := False;
  iGameLevel0 := 1;
end;

procedure TfMarienbad.mOptionsStrength2Click(Sender: TObject);

begin
  mOptionsStrength1.Checked := False; mOptionsStrength2.Checked := True; mOptionsStrength3.Checked := False;
  iGameLevel0 := 2;
end;

procedure TfMarienbad.mOptionsStrength3Click(Sender: TObject);

begin
  mOptionsStrength1.Checked := False; mOptionsStrength2.Checked := False; mOptionsStrength3.Checked := True;
  iGameLevel0 := 3;
end;

{ Menu items "Options > First move > ...": Select who should move first: Computer, player, or randomly chosen }

procedure TfMarienbad.mOptionsFirstMoveComputerClick(Sender: TObject);

begin
  mOptionsFirstMoveComputer.Checked := True; mOptionsFirstMovePlayer.Checked := False;
  mOptionsFirstMoveRandom.Checked := False;
  sMoveFirst0 := 'computer';
end;

procedure TfMarienbad.mOptionsFirstMovePlayerClick(Sender: TObject);

begin
  mOptionsFirstMoveComputer.Checked := False; mOptionsFirstMovePlayer.Checked := True;
  mOptionsFirstMoveRandom.Checked := False;
  sMoveFirst0 := 'player';
end;

procedure TfMarienbad.mOptionsFirstMoveRandomClick(Sender: TObject);

begin
  mOptionsFirstMoveComputer.Checked := False; mOptionsFirstMovePlayer.Checked := False;
  mOptionsFirstMoveRandom.Checked := True;
  sMoveFirst0 := 'random';
end;

{ Menu items "Options > Matches removal > ...": Select if removes matches should be removed or stroked through }

procedure TfMarienbad.mOptionsMatchesRemoveClick(Sender: TObject);

begin
  mOptionsMatchesRemove.Checked := True; mOptionsMatchesStrikethrough.Checked := False;
end;

procedure TfMarienbad.mOptionsMatchesStrikethroughClick(Sender: TObject);

begin
  mOptionsMatchesRemove.Checked := False; mOptionsMatchesStrikethrough.Checked := True;
end;

{ Menu item "Help > Help": Open application help text in web browser }

procedure TfMarienbad.mHelpApplicationClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Help > About": Display application about }

procedure TfMarienbad.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Marienbad game.' + LineEnding;
  S += 'Free PC version of the Marienbad variant of the classic Nim game.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2019.';
  MessageDlg('About "Marienbad"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Nim" pushed: Play the game }

procedure TfMarienbad.btActionClick(Sender: TObject);

var
  Heap, Matches, MatchRows, OneMatchRows, First, Max, I, J: Integer;
  EndOfGame, Err: Boolean;

begin
  EndOfGame := False; Err := False;
  // Player play (if the computer begins the game, this part of the code is skipped)
  if (sActualPlayer = 'player') then begin
    if iCountTaken = 0 then begin
      // Possible illegal move (player did take no match at all)
      MessageDlg('Marienbad', 'You must take at least one match!', mtError, [mbOK], 0);
      Err := True;
    end
    else begin
      // The player has taken the matches at this point, so nothing else remaining to do than to check if there are matches left or not
      EndOfGame := True;
      for I := 0 to 3 do begin
        if aMatches[I] <> 0 then                                               // row with at least 1 match remaining
          EndOfGame := False;
      end;
      if EndOfGame then begin
        // If the last match has been taken, determine who is the winner
        if sGameVariant = 'Nim' then begin
          // Nim variant: Player wins by taking last match
          MessageDlg('Marienbad', 'Congratulations! You win!', mtInformation, [mbOK], 0);
        end
        else begin
          // Misere variant: Player looses by taking last match
          MessageDlg('Marienbad', 'Sorry! But you loose this game!', mtInformation, [mbOK], 0);
        end;
        sActualPlayer := '';
        btAction.Enabled := False;                                             // disable button until a new game has been started
      end;
    end;
  end;
  // Computer play (computer plays immediately after the player has played; no further button push...),
  // except if the game is over or the player move was illegal
  if not EndOfGame and not Err then begin
    sActualPlayer := 'computer';
    ResetPlayers(sActualPlayer, edComputerMatches, edPlayerMatches, shMatches, aMatchesTaken, False);
    // Computer move, depending on playing strength selected
    if iGameLevel = 3 then begin
      // Expert: Always playing the winning move (if there is any, of course)
      ComputerWinningMove(sGameVariant, aMatches, Heap, Matches);
    end
    else begin
      // Computer not playing at full strength (making mistakes by making more or less often a random move)
      MatchRows := 0; OneMatchRows := 0;
      for I := 0 to 3 do begin
        if aMatches[I] <> 0 then begin
          Inc(MatchRows);                                                      // rows with 1 or more matches left
          if aMatches[I] = 1 then
            Inc(OneMatchRows);                                                 // rows with exactly 1 match left
        end;
      end;
      if iGameLevel = 1 then begin
        // Beginner: Random move, except if there is one single row with one or more matches left
        if MatchRows = 1 then
          ComputerWinningMove(sGameVariant, aMatches, Heap, Matches)
        else
          ComputerRandomMove(aMatches, Heap, Matches);
      end
      else begin
        // Intermediate: 50% random moves at the begin of the game, full strength play at the end of the game
        if (MatchRows <= 2) or (MatchRows = OneMatchRows) then begin
          // End of the game, if there are only 1 or 2 rows left OR if all rows contain one single match
          ComputerWinningMove(sGameVariant, aMatches, Heap, Matches);
        end
        else begin
          // Not end of the game: Random choice between winning and random move
          if Random(2) = 0 then
            ComputerRandomMove(aMatches, Heap, Matches)
          else
            ComputerWinningMove(sGameVariant, aMatches, Heap, Matches);
        end;
      end;
    end;
    if Matches < 0 then begin                                                  // random moves return negative number
      //edComputerMatches[Heap].Font.Color := clRed;                           // during test phase: possibility to see if program played winning or random move
      Matches := -Matches;
    end
    else begin
      edComputerMatches[Heap].Font.Color := clDefault;
    end;
    // Calculate new number of matches for row, from where the computer took the matches
    aMatches[Heap] -= Matches; edComputerMatches[Heap].Text := IntToStr(Matches);
    // Determine index of first match and maximum number of matches for the row, from where the computer took the matches
    case Heap of
      0: begin First := 0; Max := 1; end;
      1: begin First := 1; Max := 3; end;
      2: begin First := 4; Max := 5; end;
      3: begin First := 9; Max := 7; end;
    end;
    // Remove the matches that the computer has taken
    J := First + Max - 1;
    for I := 1 to Matches do begin
      while shMatches[J].Visible or not imMatches[J].Visible do begin
        // These matches have already been taken...
        Dec(J);
      end;
      if mOptionsMatchesRemove.Checked then begin
        // Remove the match (making it invisible)
        imMatches[J].Visible := False;
      end
      else begin
        // Strike the match through (making the strike-through shape visible)
        shMatches[J].Visible := True;
        shMatches[J].Brush.Color := clBlue;
      end;
      Dec(J);
    end;
    // Check if there are matches left; if not determine the winner of the game
    EndOfGame := True;
    for I := 0 to 3 do begin
      if aMatches[I] <> 0 then                                                 // there remains at least 1 match in this row
        EndOfGame := False;
    end;
    // No matches left: Game is over
    if EndOfGame then begin
      if sGameVariant = 'Nim' then begin
        // Nim variant: Player looses if computer takes last match
        MessageDlg('Marienbad', 'Sorry! But you loose this game!', mtInformation, [mbOK], 0);
      end
      else begin
        // Misere variant: Player wins if computer takes last match
        MessageDlg('Marienbad', 'Congratulations! You win!', mtInformation, [mbOK], 0);
      end;
      sActualPlayer := '';
      btAction.Enabled := False;                                               // disable button until a new game is started
    end
    // Still matches left: Next turn (with player having to take some matches)
    else begin
      sActualPlayer := 'player';
      ResetPlayers(sActualPlayer, edComputerMatches, edPlayerMatches, shMatches, aMatchesTaken, False);
      iCountTaken := 0;                                                        // not yet any matches taken for this turn
      btAction.Caption := 'Nim';
    end;
  end
end;

{ Click on one of the match images: Remove this match (or...) }

procedure TfMarienbad.imMatch1Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 0, 0, iCountTaken);
end;

procedure TfMarienbad.imMatch2Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 1, 1, iCountTaken);
end;

procedure TfMarienbad.imMatch3Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 1, 2, iCountTaken);
end;

procedure TfMarienbad.imMatch4Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 1, 3, iCountTaken);
end;

procedure TfMarienbad.imMatch5Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 2, 4, iCountTaken);
end;

procedure TfMarienbad.imMatch6Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 2, 5, iCountTaken);
end;

procedure TfMarienbad.imMatch7Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 2, 6, iCountTaken);
end;

procedure TfMarienbad.imMatch8Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 2, 7, iCountTaken);
end;

procedure TfMarienbad.imMatch9Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 2, 8, iCountTaken);
end;

procedure TfMarienbad.imMatch10Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 9, iCountTaken);
end;

procedure TfMarienbad.imMatch11Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 10, iCountTaken);
end;

procedure TfMarienbad.imMatch12Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 11, iCountTaken);
end;

procedure TfMarienbad.imMatch13Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 12, iCountTaken);
end;

procedure TfMarienbad.imMatch14Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 13, iCountTaken);
end;

procedure TfMarienbad.imMatch15Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 14, iCountTaken);
end;

procedure TfMarienbad.imMatch16Click(Sender: TObject);

begin
  if sActualPlayer = 'player' then
    RemoveMatch(imMatches, shMatches, aMatches, edComputerMatches, edPlayerMatches, aMatchesTaken, sActualPlayer, 3, 15, iCountTaken);
end;

end.

