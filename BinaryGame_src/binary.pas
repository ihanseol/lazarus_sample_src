{****************************************}
{* Main unit for BinaryGame application *}
{****************************************}

unit binary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls;

type
  TGridShapes = array[1..8, 1..8] of TShape;
  TGridLabels = array[1..8, 1..8] of TStaticText;
  {**********}
  { TfBinary }
  {**********}
  TfBinary = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    shGrid11, shGrid12, shGrid13, shGrid14, shGrid15, shGrid16, shGrid17, shGrid18: TShape;
    shGrid81, shGrid82, shGrid83, shGrid84, shGrid85, shGrid86, shGrid87, shGrid88: TShape;
    shGrid71, shGrid72, shGrid73, shGrid74, shGrid75, shGrid76, shGrid77, shGrid78: TShape;
    shGrid61, shGrid62, shGrid63, shGrid64, shGrid65, shGrid66, shGrid67, shGrid68: TShape;
    shGrid51, shGrid52, shGrid53, shGrid54, shGrid55, shGrid56, shGrid41, shGrid42: TShape;
    shGrid43, shGrid44, shGrid31, shGrid32, shGrid21, shGrid22, shGrid23, shGrid24: TShape;
    shGrid25, shGrid26, shGrid27, shGrid28, shGrid33, shGrid34, shGrid35, shGrid36: TShape;
    shGrid37, shGrid38, shGrid45, shGrid46, shGrid47, shGrid48, shGrid57, shGrid58: TShape;
    stGrid11, stGrid12, stGrid13, stGrid14, stGrid15, stGrid16, stGrid17, stGrid18: TStaticText;
    stGrid82, stGrid83, stGrid84, stGrid85, stGrid86, stGrid87, stGrid88, stGrid81: TStaticText;
    stGrid72, stGrid73, stGrid74, stGrid75, stGrid76, stGrid77, stGrid78, stGrid71: TStaticText;
    stGrid62, stGrid63, stGrid64, stGrid65, stGrid66, stGrid67, stGrid68, stGrid61: TStaticText;
    stGrid52, stGrid53, stGrid54, stGrid55, stGrid56, stGrid57, stGrid58, stGrid51: TStaticText;
    stGrid42, stGrid43, stGrid44, stGrid45, stGrid46, stGrid47, stGrid41, stGrid38: TStaticText;
    stGrid37, stGrid36, stGrid35, stGrid34, stGrid33, stGrid32, stGrid31, stGrid28: TStaticText;
    stGrid27, stGrid26, stGrid25, stGrid24, stGrid23, stGrid22, stGrid21, stGrid48: TStaticText;
    Memo1: TMemo;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure stGrid11MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid12MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid13MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid14MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid15MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid16MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid17MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid18MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid21MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid22MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid23MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid24MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid25MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid26MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid27MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid28MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid31MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid32MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid33MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid34MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid35MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid36MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid37MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid38MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid41MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid42MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid43MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid44MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid45MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid46MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid47MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid48MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid51MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid52MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid53MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid54MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid55MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid56MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid57MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid58MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid61MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid62MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid63MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid64MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid65MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid66MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid67MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid68MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid71MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid72MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid73MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid74MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid75MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid76MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid77MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid78MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid81MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid82MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid83MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid84MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid85MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid86MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid87MouseDown(Sender: TObject; Button: TMouseButton);
    procedure stGrid88MouseDown(Sender: TObject; Button: TMouseButton);
    procedure btDoneClick(Sender: TObject);
  private
    shGrid: TGridShapes;
    stGrid: TGridLabels;
  end;

var
  fBinary: TfBinary;

implementation

{$R *.lfm}

{ Write number to grid cell resp. clear grid cell }

procedure GridSet(var GridShapes: TGridShapes; var GridLabels: TGridLabels; Button: TMouseButton; Row, Col: Integer);

begin
  if GridShapes[Row, Col].Brush.Color <> clYellow then begin
    // User cannot change cells filled in by the application (these cells are colored in yellow)
    if GridLabels[Row, Col].Caption = '' then begin
      // Free cell: left mouse button = 0, right mouse button = 1
      if Button = mbLeft then
        GridLabels[Row, Col].Caption := '0'
      else
        GridLabels[Row, Col].Caption := '1';
      GridShapes[Row, Col].Brush.Color := clLime;                              // color filled-in cells in green
    end
    else begin
      // Cell contains number: Clear the cell
      GridLabels[Row, Col].Caption := '';
      GridShapes[Row, Col].Brush.Color := clWhite;                             // empty cells colored in white
    end;
  end;
end;

{**********}
{ TfBinary }
{**********}

{ Application start: Initializations }

procedure TfBinary.FormCreate(Sender: TObject);

begin
  // Create two-dimensional array with grid shapes
  shGrid[1, 1] := shGrid11; shGrid[1, 2] := shGrid12; shGrid[1, 3] := shGrid13; shGrid[1, 4] := shGrid14;
  shGrid[1, 5] := shGrid15; shGrid[1, 6] := shGrid16; shGrid[1, 7] := shGrid17; shGrid[1, 8] := shGrid18;
  shGrid[2, 1] := shGrid21; shGrid[2, 2] := shGrid22; shGrid[2, 3] := shGrid23; shGrid[2, 4] := shGrid24;
  shGrid[2, 5] := shGrid25; shGrid[2, 6] := shGrid26; shGrid[2, 7] := shGrid27; shGrid[2, 8] := shGrid28;
  shGrid[3, 1] := shGrid31; shGrid[3, 2] := shGrid32; shGrid[3, 3] := shGrid33; shGrid[3, 4] := shGrid34;
  shGrid[3, 5] := shGrid35; shGrid[3, 6] := shGrid36; shGrid[3, 7] := shGrid37; shGrid[3, 8] := shGrid38;
  shGrid[4, 1] := shGrid41; shGrid[4, 2] := shGrid42; shGrid[4, 3] := shGrid43; shGrid[4, 4] := shGrid44;
  shGrid[4, 5] := shGrid45; shGrid[4, 6] := shGrid46; shGrid[4, 7] := shGrid47; shGrid[4, 8] := shGrid48;
  shGrid[5, 1] := shGrid51; shGrid[5, 2] := shGrid52; shGrid[5, 3] := shGrid53; shGrid[5, 4] := shGrid54;
  shGrid[5, 5] := shGrid55; shGrid[5, 6] := shGrid56; shGrid[5, 7] := shGrid57; shGrid[5, 8] := shGrid58;
  shGrid[6, 1] := shGrid61; shGrid[6, 2] := shGrid62; shGrid[6, 3] := shGrid63; shGrid[6, 4] := shGrid64;
  shGrid[6, 5] := shGrid65; shGrid[6, 6] := shGrid66; shGrid[6, 7] := shGrid67; shGrid[6, 8] := shGrid68;
  shGrid[7, 1] := shGrid71; shGrid[7, 2] := shGrid72; shGrid[7, 3] := shGrid73; shGrid[7, 4] := shGrid74;
  shGrid[7, 5] := shGrid75; shGrid[7, 6] := shGrid76; shGrid[7, 7] := shGrid77; shGrid[7, 8] := shGrid78;
  shGrid[8, 1] := shGrid81; shGrid[8, 2] := shGrid82; shGrid[8, 3] := shGrid83; shGrid[8, 4] := shGrid84;
  shGrid[8, 5] := shGrid85; shGrid[8, 6] := shGrid86; shGrid[8, 7] := shGrid87; shGrid[8, 8] := shGrid88;
  // Create two-dimensional array with grid labels (statictexts)
  stGrid[1, 1] := stGrid11; stGrid[1, 2] := stGrid12; stGrid[1, 3] := stGrid13; stGrid[1, 4] := stGrid14;
  stGrid[1, 5] := stGrid15; stGrid[1, 6] := stGrid16; stGrid[1, 7] := stGrid17; stGrid[1, 8] := stGrid18;
  stGrid[2, 1] := stGrid21; stGrid[2, 2] := stGrid22; stGrid[2, 3] := stGrid23; stGrid[2, 4] := stGrid24;
  stGrid[2, 5] := stGrid25; stGrid[2, 6] := stGrid26; stGrid[2, 7] := stGrid27; stGrid[2, 8] := stGrid28;
  stGrid[3, 1] := stGrid31; stGrid[3, 2] := stGrid32; stGrid[3, 3] := stGrid33; stGrid[3, 4] := stGrid34;
  stGrid[3, 5] := stGrid35; stGrid[3, 6] := stGrid36; stGrid[3, 7] := stGrid37; stGrid[3, 8] := stGrid38;
  stGrid[4, 1] := stGrid41; stGrid[4, 2] := stGrid42; stGrid[4, 3] := stGrid43; stGrid[4, 4] := stGrid44;
  stGrid[4, 5] := stGrid45; stGrid[4, 6] := stGrid46; stGrid[4, 7] := stGrid47; stGrid[4, 8] := stGrid48;
  stGrid[5, 1] := stGrid51; stGrid[5, 2] := stGrid52; stGrid[5, 3] := stGrid53; stGrid[5, 4] := stGrid54;
  stGrid[5, 5] := stGrid55; stGrid[5, 6] := stGrid56; stGrid[5, 7] := stGrid57; stGrid[5, 8] := stGrid58;
  stGrid[6, 1] := stGrid61; stGrid[6, 2] := stGrid62; stGrid[6, 3] := stGrid63; stGrid[6, 4] := stGrid64;
  stGrid[6, 5] := stGrid65; stGrid[6, 6] := stGrid66; stGrid[6, 7] := stGrid67; stGrid[6, 8] := stGrid68;
  stGrid[7, 1] := stGrid71; stGrid[7, 2] := stGrid72; stGrid[7, 3] := stGrid73; stGrid[7, 4] := stGrid74;
  stGrid[7, 5] := stGrid75; stGrid[7, 6] := stGrid76; stGrid[7, 7] := stGrid77; stGrid[7, 8] := stGrid78;
  stGrid[8, 1] := stGrid81; stGrid[8, 2] := stGrid82; stGrid[8, 3] := stGrid83; stGrid[8, 4] := stGrid84;
  stGrid[8, 5] := stGrid85; stGrid[8, 6] := stGrid86; stGrid[8, 7] := stGrid87; stGrid[8, 8] := stGrid88;
  // Start random generator
  Randomize;
  // Start new game (simulating click on "Game > New" menu item)
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new game }

procedure TfBinary.mGameNewClick(Sender: TObject);

var
  N, I, J: Integer;
  OK: Boolean;
  Grid: array[1..8, 1..8] of Integer;
  RCounts, CCounts: array[0..1, 1..8] of Integer;

begin
  // Reset the grid shapes and labels
  for I := 1 to 8 do begin
    for J := 1 to 8 do begin
      shGrid[I, J].Brush.Color := clWhite;
      stGrid[I, J].Caption := '';
    end;
  end;
  // Generate the puzzle
  repeat
    // Repeate puzzle generation until the puzzle is conform to the game rules
    OK := True;
    // Reset
    for I := 1 to 8 do begin
      for J := 1 to 8 do
        Grid[I, J] := -1;
    end;
    for I := 0 to 1 do begin
      for J := 1 to 8 do begin
        RCounts[I, J] := 0;
        CCounts[I, J] := 0;
      end;
    end;
    // Randomly fill in the first 7 rows
    for I := 1 to 7 do begin
      if OK then begin
        for J := 1 to 8 do begin
          if OK then begin
            // There have to be 4 ones and 4 zeros in each row and each column
            if (RCounts[0, I] = 4) and (CCounts[1, J] = 4) then
              OK := False
            else if (RCounts[1, I] = 4) and (CCounts[0, J] = 4) then
              OK := False
            else if (RCounts[0, I] = 4) or (CCounts[0, J] = 4) then
              N := 1
            else if (RCounts[1, I] = 4) or (CCounts[1, J] = 4) then
              N := 0
            else
              N := Random(2);
            // There must not be more than 2 consecutive ones or zeros
            if (I > 2) and (Grid[I - 1, J] = N) and (Grid[I - 2, J] = N) then
              OK := False
            else if (J > 2) and (Grid[I, J - 1] = N) and (Grid[I, J - 2] = N) then
              OK := False;
            if OK then begin
              // Insert 0 or 1 into grid cell; increment corr. row and column counters
              Grid[I, J] := N;
              Inc(RCounts[N, I]);
              Inc(CCounts[N, J]);
            end;
          end;
        end;
      end;
    end;
    if OK then begin
      // Fill in the last row (based on values of column counters)
      for J := 1 to 8 do begin
        if CCounts[0, J] = 4 then
          Grid[8, J] := 1
        else
          Grid[8, J] := 0;
      end;
      // There must not be more than 2 consecutive ones or zeros in this row
      for J := 3 to 8 do begin
        if OK then begin
          for N := 0 to 1 do begin
            if (Grid[8, J] = N) and (Grid[8, J - 1] = N) and (Grid[8, J - 2] = N) then
              OK := False;
          end;
        end;
      end;
    end;
  until OK;
  // Randomly choose 15 to 25 cells with value filled in by the application
  for N := 1 to 15 + Random(11) do begin
    // Cell must be empty
    repeat
      I := Random(8) + 1; J := Random(8) + 1;
    until stGrid[I, J].Caption = '';
    // Fill in the number (0 or 1) and color the cell in yellow
    stGrid[I, J].Caption := IntToStr(Grid[I, J]);
    shGrid[I, J].Brush.Color := clYellow;
  end;
  // Enable the "Done" button
  btDone.Enabled := True;
end;

{ Menu item "Game > Exit": Exit application }

procedure TfBinary.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display application help }

procedure TfBinary.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Binary Game: A logic game with ones and zeros.' + LineEnding;
  S += 'Fill in the grid with ones and zeros. There must not be more than 2 consecutive ';
  S += 'ones or zeros. In each row and in each column, there must be the same number of ones and zeros.';
  MessageDlg('"BinaryGame" help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfBinary.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Binary Game: A logic game with ones and zeros.' + LineEnding;
  S += 'The game is based on a kind of puzzle, that I found in the free Luxembourgish newspaper "L''essentiel".' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2023.';
  MessageDlg('About "BinaryGame"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done" pushed: Check user solution }

procedure TfBinary.btDoneClick(Sender: TObject);

// The routine does not simply compare the user grid with the grid generated,
// but checks it accordingly to the game rules. In fact, there may be several solutions.

var
  Count, N, I, J: Integer;
  Mess: string;
  OK: Boolean;
  UGrid: array[1..8, 1..8] of Integer;

begin
  OK := True;
  // Copy grid values from form to an array
  for I := 1 to 8 do begin
    for J := 1 to 8 do begin
      if stGrid[I, J].Caption = '' then
        OK := False
      else
        UGrid[I, J] := StrToInt(stGrid[I, J].Caption);
    end;
  end;
  // Error message if grid isn't completely filled in
  if not OK then begin
    Mess := 'The grid must be completely filled in!';
  end
  // Continuation of validity check, otherwise
  else begin
    // Check if all rows contain same number of ones and zeros
    for I := 1 to 8 do begin
      Count := 0;
      for J := 1 to 8 do
        Count += UGrid[I, J];
      if Count <> 4 then
        OK := False;
    end;
  end;
  // Error message if one or several rows don't contain same number of ones and zeros
  if not OK then begin
    if Mess = '' then
      Mess := 'All rows must contain the same number of ones and zeros!';
  end
  // Continuation of validity check, otherwise
  else begin
    // Check if all columns contain same number of ones and zeros
    for J := 1 to 8 do begin
      Count := 0;
      for I := 1 to 8 do
        Count += UGrid[I, J];
      if Count <> 4 then
        OK := False;
    end;
  end;
  // Error message if one or several columns don't contain same number of ones and zeros
  if not OK then begin
    if Mess = '' then
      Mess := 'All columns must contain the same number of ones and zeros!';
  end
  // Continuation of validity check, otherwise
  else begin
    // Check if there aren't any more than 2 consecutive ones or zeros
    for I := 1 to 8 do begin
      for J := 1 to 8 do begin
        for N := 0 to 1 do begin
          if (I > 2) and (UGrid[I, J] = N) and (UGrid[I - 1, J] = N) and (UGrid[I - 2, J] = N) then
            OK := False
          else if (J > 2) and (UGrid[I, J] = N) and (UGrid[I, J - 1] = N) and (UGrid[I, J - 2] = N) then
            OK := False;
        end;
      end;
    end;
  end;
  // Error message if there are any more than 2 consecutive ones or zeros
  if not OK then begin
    if Mess = '' then
      Mess := 'There must not be more than 2 consecutive ones or zeros!';
  end;
  // Puzzle has been successfully solved
  if OK then begin
    Mess := 'Congratulations! You successfully solved the puzzle!';
  end
  // Puzzle has not been solved
  else begin
    Mess := 'Sorry, this is a bad solution: ' + Mess;
  end;
  // Display success resp. failure message
  MessageDlg('Binary Game', Mess, mtInformation, [mbOK], 0);
  // Disable "Done" button (user must click "Game > New" to start a new game)
  btDone.Enabled := False;
end;

{ Left of right mouse click on grid cells: Fill in 0 or 1 resp. clear the cell }

procedure TfBinary.stGrid11MouseDown(Sender: TObject; Button: TMouseButton);


begin
  GridSet(shGrid, stGrid, Button, 1, 1);
end;

procedure TfBinary.stGrid12MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 2);
end;

procedure TfBinary.stGrid13MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 3);
end;

procedure TfBinary.stGrid14MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 4);
end;

procedure TfBinary.stGrid15MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 5);
end;

procedure TfBinary.stGrid16MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 6);
end;

procedure TfBinary.stGrid17MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 7);
end;

procedure TfBinary.stGrid18MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 1, 8);
end;

procedure TfBinary.stGrid21MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 1);
end;

procedure TfBinary.stGrid22MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 2);
end;

procedure TfBinary.stGrid23MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 3);
end;

procedure TfBinary.stGrid24MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 4);
end;

procedure TfBinary.stGrid25MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 5);
end;

procedure TfBinary.stGrid26MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 6);
end;

procedure TfBinary.stGrid27MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 7);
end;

procedure TfBinary.stGrid28MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 2, 8);
end;

procedure TfBinary.stGrid31MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 1);
end;

procedure TfBinary.stGrid32MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 2);
end;

procedure TfBinary.stGrid33MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 3);
end;

procedure TfBinary.stGrid34MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 4);
end;

procedure TfBinary.stGrid35MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 5);
end;

procedure TfBinary.stGrid36MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 6);
end;

procedure TfBinary.stGrid37MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 7);
end;

procedure TfBinary.stGrid38MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 3, 8);
end;

procedure TfBinary.stGrid41MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 1);
end;

procedure TfBinary.stGrid42MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 2);
end;

procedure TfBinary.stGrid43MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 3);
end;

procedure TfBinary.stGrid44MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 4);
end;

procedure TfBinary.stGrid45MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 5);
end;

procedure TfBinary.stGrid46MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 6);
end;

procedure TfBinary.stGrid47MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 7);
end;

procedure TfBinary.stGrid48MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 4, 8);
end;

procedure TfBinary.stGrid51MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 1);
end;

procedure TfBinary.stGrid52MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 2);
end;

procedure TfBinary.stGrid53MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 3);
end;

procedure TfBinary.stGrid54MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 4);
end;

procedure TfBinary.stGrid55MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 5);
end;

procedure TfBinary.stGrid56MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 6);
end;

procedure TfBinary.stGrid57MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 7);
end;

procedure TfBinary.stGrid58MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 5, 8);
end;

procedure TfBinary.stGrid61MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 1);
end;

procedure TfBinary.stGrid62MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 2);
end;

procedure TfBinary.stGrid63MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 3);
end;

procedure TfBinary.stGrid64MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 4);
end;

procedure TfBinary.stGrid65MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 5);
end;

procedure TfBinary.stGrid66MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 6);
end;

procedure TfBinary.stGrid67MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 7);
end;

procedure TfBinary.stGrid68MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 6, 8);
end;

procedure TfBinary.stGrid71MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 1);
end;

procedure TfBinary.stGrid72MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 2);
end;

procedure TfBinary.stGrid73MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 3);
end;

procedure TfBinary.stGrid74MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 4);
end;

procedure TfBinary.stGrid75MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 5);
end;

procedure TfBinary.stGrid76MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 6);
end;

procedure TfBinary.stGrid77MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 7);
end;

procedure TfBinary.stGrid78MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 7, 8);
end;

procedure TfBinary.stGrid81MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 1);
end;

procedure TfBinary.stGrid82MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 2);
end;

procedure TfBinary.stGrid83MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 3);
end;

procedure TfBinary.stGrid84MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 4);
end;

procedure TfBinary.stGrid85MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 5);
end;

procedure TfBinary.stGrid86MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 6);
end;

procedure TfBinary.stGrid87MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 7);
end;

procedure TfBinary.stGrid88MouseDown(Sender: TObject; Button: TMouseButton);

begin
  GridSet(shGrid, stGrid, Button, 8, 8);
end;

end.

