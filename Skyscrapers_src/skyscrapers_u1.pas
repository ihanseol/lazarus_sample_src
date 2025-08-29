{*****************************************}
{* Main unit for Skyscrapers application *}
{*****************************************}

unit skyscrapers_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, skyscrapers_u2;

type
  TGrid = array[1..5, 1..5] of Byte;
  {***************}
  { TfSkyscrapers }
  {***************}
  TfSkyscrapers = class(TForm)
    mMenu: TMainMenu;
    mSkyscrapers, mSkyscrapers4, mSkyscrapers5, mSkyscrapersCancel, mSkyscrapersExit: TMenuItem;
    mOptions, mOptionsHints, mOptionsHighlight: TMenuItem;
    mHelp, mHelpRules, mHelpHelp, mHelpCopyright, mHelpAbout: TMenuItem;
    Square11, Square12, Square13, Square14, Square15: TEdit;
    Square21, Square22, Square23, Square24, Square25: TEdit;
    Square31, Square32, Square33, Square34, Square35: TEdit;
    Square41, Square42, Square43, Square44, Square45: TEdit;
    Square51, Square52, Square53, Square54, Square55: TEdit;
    CountT1, CountT2, CountT3, CountT4, CountT5: TEdit;
    CountL1, CountL2, CountL3, CountL4, CountL5: TEdit;
    CountR1, CountR2, CountR3, CountR4, CountR5: TEdit;
    CountB1, CountB2, CountB3, CountB4, CountB5: TEdit;
    btCheck: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mSkyscrapers4Click(Sender: TObject);
    procedure mSkyscrapers5Click(Sender: TObject);
    procedure mSkyscrapersCancelClick(Sender: TObject);
    procedure mSkyscrapersExitClick(Sender: TObject);
    procedure mOptionsHintsClick(Sender: TObject);
    procedure mOptionsHighlightClick(Sender: TObject);
    procedure mHelpRulesClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpCopyrightClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure Square11Change(Sender: TObject);
    procedure Square12Change(Sender: TObject);
    procedure Square13Change(Sender: TObject);
    procedure Square14Change(Sender: TObject);
    procedure Square15Change(Sender: TObject);
    procedure Square21Change(Sender: TObject);
    procedure Square22Change(Sender: TObject);
    procedure Square23Change(Sender: TObject);
    procedure Square24Change(Sender: TObject);
    procedure Square25Change(Sender: TObject);
    procedure Square31Change(Sender: TObject);
    procedure Square32Change(Sender: TObject);
    procedure Square33Change(Sender: TObject);
    procedure Square34Change(Sender: TObject);
    procedure Square35Change(Sender: TObject);
    procedure Square41Change(Sender: TObject);
    procedure Square42Change(Sender: TObject);
    procedure Square43Change(Sender: TObject);
    procedure Square44Change(Sender: TObject);
    procedure Square45Change(Sender: TObject);
    procedure Square51Change(Sender: TObject);
    procedure Square52Change(Sender: TObject);
    procedure Square53Change(Sender: TObject);
    procedure Square54Change(Sender: TObject);
    procedure Square55Change(Sender: TObject);
    procedure btCheckClick(Sender: TObject);
  private
    iSquares, iHints: Integer;
    bDone, bCancel: Boolean;
    Grid, UGrid: TGrid;
    Squares: array[1..5, 1..5] of TEdit;
    Clues: array[1..4, 1..5] of TEdit;
  end;

var
  fSkyscrapers: TfSkyscrapers;

implementation

{$R *.lfm}

{ Count buildings viewed from a given point outside the grid }

function BuildingsCount(G: TGrid; N, CA, CI: Integer): Integer;

// Point of view is given by given element in given "clues bar"
// CA = "clues bar" (top, left, right, bottom); CI = bar element number

var
  BC, B0, I: Integer;

begin
  BC := 1;
  case CA of
    // Point of view from positions at top of grid
    1: begin
         B0 := G[1][CI];
         for I := 2 to N do begin
           if G[I][CI] > B0 then begin
             Inc(BC);
             B0 := G[I][CI];
           end;
         end;
       end;
    // Point of view from positions at left of grid
    2: begin
         B0 := G[CI][1];
         for I := 2 to N do begin
           if G[CI][I] > B0 then begin
             Inc(BC);
             B0 := G[CI][I];
           end;
         end;
       end;
    // Point of view from positions at right of grid
    3: begin
         B0 := G[CI][N];
         for I := N - 1 downto 1 do begin
           if G[CI][I] > B0 then begin
             Inc(BC);
             B0 := G[CI][I];
           end;
         end;
       end;
    // Point of view from positions at bottom of grid
    4: begin
         B0 := G[N][CI];
         for I := N - 1 downto 1 do begin
           if G[I][CI] > B0 then begin
             Inc(BC);
             B0 := G[I][CI];
           end;
         end;
       end;
  end;
  Result := BC;
end;

{ Clear Skyscrapers grid and clues bars }

procedure ClearGrid(var G: TGrid);

var
  I, J: Integer;

begin
  // Clear the grid
  for I := 1 to 5 do begin
    for J := 1 to 5 do begin
      fSkyscrapers.Squares[I][J].Clear;
      fSkyscrapers.Squares[I][J].Color := clDefault;
    end;
  end;
  // Clear the clues bars
  for I := 1 to 4 do begin
    for J := 1 to 5 do
      fSkyscrapers.Clues[I][J].Clear;
  end;
  // Set all elements of grid array to 0
  for I := 1 to 5 do begin
    for J := 1 to 5 do
      G[I][J] := 0;
  end;
end;

{ Start new Skyscrapers puzzle }

procedure NewPuzzle(N: Integer; var Done, Cancel: Boolean; var Hints: Integer; var G, UG: TGrid);

var
  C, B, I, J, I1, J1: Integer;
  OK, Available: Boolean;

begin
  Done := False; Cancel := False;
  // Clear user grid
  ClearGrid(UG);
  // Init variables (depending on number of grid fields)
  Hints := 0;
  if N = 5 then begin
    Available := True;
    if fSkyscrapers.mOptionsHints.Checked then
      Hints := 4;
  end
  else begin
    Available := False;
    if fSkyscrapers.mOptionsHints.Checked then
      Hints := 2;
  end;
  // Show resp. hide 5th row and column grid fields
  for I := 1 to 5 do begin
    fSkyscrapers.Squares[I][5].Enabled := Available;
    fSkyscrapers.Squares[5][I].Enabled := Available;
  end;
  // Show resp. hide 5th row and column clues bars fields
  for I := 1 to 4 do
    fSkyscrapers.Clues[I][5].Visible := Available;
  // Create random Skyscrapers grid
  // ------------------------------
  for I := 1 to N do begin
    repeat
      OK := True;
      for J := 1 to N do begin
        repeat
          B := Random(N) + 1; OK := True;
          if J > 1 then begin
            for J1 := 1 to J - 1 do begin
              if B = G[I][J1] then
                OK := False;
            end;
          end;
        until OK;
        G[I][J] := B;
      end;
      if I > 1 then begin
        for I1 := 1 to I - 1 do
          for J := 1 to N do begin
            if G[I][J] = G[I1][J] then
              OK := False;
          end;
      end;
    until OK;
  end;
  // Determine number of buildings seen (clues fields values)
  for I := 1 to 4 do begin
    for J := 1 to N do begin
      fSkyscrapers.Clues[I, J].Text := IntToStr(BuildingsCount(G, N, I, J));             // calculate number of buildings seen from this point
      fSkyscrapers.Clues[I, J].Font.Color := clSilver;                                   // use background color to hide the clue (for now...)
    end;
  end;
  // Display clues: 10 for 4x4 grid, 14 for 5x5 grid,
  // Possible extensions: Variable number of clues shown; difficulty level (number of "1" and "4" resp. "5" clues shown...)
  if N = 4 then
    C := 10
  else
    C := 14;
  for I := 1 to C do begin
    repeat
      I1 := Random(4) + 1; J1 := Random(N) + 1;                                          // choose a random one
    until fSkyscrapers.Clues[I1, J1].Font.Color = clSilver;                              // must not have been chosen before
    fSkyscrapers.Clues[I1, J1].Font.Color := clDefault;                                  // display the clue (using foreground color)
  end;
  // Disable "Options" menu and "Check" button
  fSkyscrapers.mOptions.Enabled := False;
  fSkyscrapers.btCheck.Enabled := False;
end;

{ Buildings entry by user (sub called when the value of one of the grid edit fields has changed) }

procedure BuildingsEntry(I, J, N: Integer; Done, Cancel: Boolean; var PGrid, UGrid: TGrid; var Hints: Integer);

var
  B0, B, NB, K: Integer;
  S: string;
  Ch: Char;
  OK: Boolean;

begin
  B0 := UGrid[I][J];                                                                     // old grid field value
  if Done or Cancel then begin
    // Disable action on grid if puzzle terminated: Reset the grid field value
    if B0 = 0 then
      fSkyscrapers.Squares[I][J].Clear
    else
      fSkyscrapers.Squares[I][J].Text := IntToStr(B0);
  end
  else begin
    // Action on grid enabled: Get user entry from grid field
    if fSkyscrapers.Squares[I][J].Text = '' then
      B := 0
    else begin
      S := fSkyscrapers.Squares[I][J].Text; Ch := S[1];
      // Numeric entry
      if Ch in ['0'..'9'] then
        B := StrToInt(Ch)
      // Non-numeric entry
      else begin
        // Entering "H" = asking for a hint
        if UpperCase(Ch) = 'H' then begin
          if fSkyscrapers.mOptionsHints.Checked and (Hints > 0) then begin
            B := PGrid[I][J];                                                            // get value from program grid
            Dec(Hints);                                                                  // one hint less left
          end
          else
            B := -1;                                                                     // entry considered as invalid
        end
        // Other non numeric character
        else
          B := -1;                                                                       // invalid entry
      end;
    end;
    // Invalid numeric entry
    if (B < 0) or (B > N) then
      B := B0                                                                            // reset grid field to what it was
    // Valid numeric entry (different from 0, which is clearing the field)
    else if B <> 0 then begin
      OK := True;
      // Check if value entered (building height) not yet present in actual row
      for K := 1 to N do
        if (K <> J) and (UGrid[I][K] = B) then
          OK := False;
      if not OK then
        MessageDlg('Invalid entry','All buildings in a row must be different height!', mtError, [mbOK], 0)
      else begin
        // Check if value entered (building height) not yet present in actual column
        for K := 1 to N do
          if (K <> I) and (UGrid[K][J] = B) then
            OK := False;
        if not OK then
          MessageDlg('Invalid entry','All buildings in a column must be different height!', mtError, [mbOK], 0);
      end;
      // Value entered isn't conform to game rules
      if not OK then
        B := B0                                                                          // reset grid field to what it was
    end;
    // Update user grid
    UGrid[I][J] := B;
    // Update the grid on the form
    if B = 0 then begin
      // Clear the grid field
      fSkyscrapers.Squares[I][J].Clear;
      fSkyscrapers.Squares[I][J].Color := clDefault;
    end
    else begin
      // Display grid field value
      fSkyscrapers.Squares[I][J].Text := IntToStr(B);
      fSkyscrapers.Squares[I][J].Color := clDefault;
      if (UpperCase(Ch) = 'H') and fSkyscrapers.mOptionsHints.Checked and (Hints >= 0) then
        // Mark fields filled-in using the Hint feature
        fSkyscrapers.Squares[I][J].Color := clAqua
      else if UGrid[I][J] = PGrid[I][J] then begin
        // Mark fields containing same value as the one in the program grid (if this option has been selected)
        if fSkyscrapers.mOptionsHighlight.Checked and (fSkyscrapers.Squares[I][J].Color <> clAqua) then
          fSkyscrapers.Squares[I][J].Color := clLime;
      end;
    end;
    // Count number of grid fields filled-in
    NB := 0;
    for I := 1 to N do begin
      for J := 1 to N do begin
        if UGrid[I][J] <> 0 then
          Inc(NB);
      end;
    end;
    // Enable check button if all fields have been filled-in
    if NB < N * N then
      fSkyscrapers.btCheck.Enabled := False
    else
      fSkyscrapers.btCheck.Enabled := True;
  end;
end;

{*****************}
{* TfSkyscrapers *}
{*****************}

{ Application start: Initialisation }

procedure TfSkyscrapers.FormCreate(Sender: TObject);

begin
  // Create array with building fields
  Squares[1][1] := fSkyscrapers.Square11; Squares[1][2] := fSkyscrapers.Square12; Squares[1][3] := fSkyscrapers.Square13; Squares[1][4] := fSkyscrapers.Square14;
  Squares[1][5] := fSkyscrapers.Square15; Squares[2][1] := fSkyscrapers.Square21; Squares[2][2] := fSkyscrapers.Square22; Squares[2][3] := fSkyscrapers.Square23;
  Squares[2][4] := fSkyscrapers.Square24; Squares[2][5] := fSkyscrapers.Square25; Squares[3][1] := fSkyscrapers.Square31; Squares[3][2] := fSkyscrapers.Square32;
  Squares[3][3] := fSkyscrapers.Square33; Squares[3][4] := fSkyscrapers.Square34; Squares[3][5] := fSkyscrapers.Square35; Squares[4][1] := fSkyscrapers.Square41;
  Squares[4][2] := fSkyscrapers.Square42; Squares[4][3] := fSkyscrapers.Square43; Squares[4][4] := fSkyscrapers.Square44; Squares[4][5] := fSkyscrapers.Square45;
  Squares[5][1] := fSkyscrapers.Square51; Squares[5][2] := fSkyscrapers.Square52; Squares[5][3] := fSkyscrapers.Square53; Squares[5][4] := fSkyscrapers.Square54;
  Squares[5][5] := fSkyscrapers.Square55;
  // Create array with clues fields
  Clues[1][1] := fSkyscrapers.CountT1; Clues[1][2] := fSkyscrapers.CountT2; Clues[1][3] := fSkyscrapers.CountT3; Clues[1][4] := fSkyscrapers.CountT4;
  Clues[1][5] := fSkyscrapers.CountT5; Clues[2][1] := fSkyscrapers.CountL1; Clues[2][2] := fSkyscrapers.CountL2; Clues[2][3] := fSkyscrapers.CountL3;
  Clues[2][4] := fSkyscrapers.CountL4; Clues[2][5] := fSkyscrapers.CountL5; Clues[3][1] := fSkyscrapers.CountR1; Clues[3][2] := fSkyscrapers.CountR2;
  Clues[3][3] := fSkyscrapers.CountR3; Clues[3][4] := fSkyscrapers.CountR4; Clues[3][5] := fSkyscrapers.CountR5; Clues[4][1] := fSkyscrapers.CountB1;
  Clues[4][2] := fSkyscrapers.CountB2; Clues[4][3] := fSkyscrapers.CountB3; Clues[4][4] := fSkyscrapers.CountB4; Clues[4][5] := fSkyscrapers.CountB5;
  // Start random number generator
  Randomize;
  // Start new Skyscrapers puzzle
  mSkyscrapers5.Click;
end;

{ Menu item "Skyscrapers > New 4x4 grid": Start new 4x4 grid puzzle }

procedure TfSkyscrapers.mSkyscrapers4Click(Sender: TObject);

begin
  iSquares := 4;
  NewPuzzle(iSquares, bDone, bCancel, iHints, Grid, UGrid);
end;

{ Menu item "Skyscrapers > New 5x5 grid": Start new 5x5 grid puzzle }

procedure TfSkyscrapers.mSkyscrapers5Click(Sender: TObject);

begin
  iSquares := 5;
  NewPuzzle(iSquares, bDone, bCancel, iHints, Grid, UGrid);
end;

{ Menu item "Skyscrapers > Cancel": Cancel actual puzzle }

procedure TfSkyscrapers.mSkyscrapersCancelClick(Sender: TObject);

begin
  MessageDlg('Skyscrapers', 'Skyscrapers puzzle canceled by user!', mtInformation, [mbOK], 0);
  bCancel := True;                                                                       // user buidings entry disabled until new puzzle is started
  mOptions.Enabled := True;                                                              // make "Options" menu available again
end;

{ Menu item "Skyscrapers > Exit": Exit application }

procedure TfSkyscrapers.mSkyscrapersExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Allow fill-in help (hints)": Enable/disable hints }

procedure TfSkyscrapers.mOptionsHintsClick(Sender: TObject);

begin
  if mOptionsHints.Checked then
    mOptionsHints.Checked := False
  else
    mOptionsHints.Checked := True;
end;

{ Menu item "Options > Highlight correct entries": Enable/disable highlighting }

procedure TfSkyscrapers.mOptionsHighlightClick(Sender: TObject);

begin
  if mOptionsHighlight.Checked then
    mOptionsHighlight.Checked := False
  else
    mOptionsHighlight.Checked := True;
end;

{ Menu item "Help > Rules": Display Skyscrapers rules }

procedure TfSkyscrapers.mHelpRulesClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.stTitle.Caption := 'Skyscrapers puzzle rules.';
  fHelp.edHelp.Lines.LoadFromFile('rules.txt');
  fHelp.Show;
end;

{ Menu item "Help > Hekp": Display application help }

procedure TfSkyscrapers.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.stTitle.Caption := 'Skyscrapers application help.';
  fHelp.edHelp.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > Copyright": Display Skyscrapers puzzle copyright }

procedure TfSkyscrapers.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'Skyscrapers is a classic logic game with regular puzzles in magazines and with online puzzles on several websites. The "Rätsel und Puzzles" ';
  S += 'website www.janko.at states the author of the game as unknown and I suppose that it is public domain. Concerning my "Skyscrapers" PC ';
  S += 'application itself, it is freeware/open source, as all programs on www.streetinfo.lu.';
  MessageDlg('Skyscrapers copyright', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfSkyscrapers.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Skyscrapers.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2018 - December 2020.' + LineEnding;
  S += 'See also: Copyright.';
  MessageDlg('About "Skyscrapers"', S, mtInformation, [mbOK], 0);
end;

{ Button "Check" pressed: Check user puzzle solution }

procedure TfSkyscrapers.btCheckClick(Sender: TObject);

var
  I, J: Integer;
  S: string;
  Solved: Boolean;

begin
  if not bDone then begin
    // Do only, if puzzle has not been terminated (completely done or canceled)
    Solved := True;
    for I := 1 to 4 do begin
      for J := 1 to iSquares do begin
        if Clues[I, J].Font.Color = clDefault then begin
          // The only clues to be considered for a correct solution are those actually displayed!
          if BuildingsCount(Ugrid, iSquares, I, J) <> StrToInt(Clues[I, J].Text) then
            // If the count of buildings seen in the user grid doesn't equal the clue number, this row/column contains an error
            Solved := False;
        end;
      end;
    end;
    if Solved then
      S := 'You have successfully solved this Skyscrapers puzzle!'
    else
      S := 'Sorry, this is no valid solution of this Skyscrapers puzzle!';
    MessageDlg('Skyscrapers', S, mtInformation, [mbOK], 0);
    bDone := True;
    mOptions.Enabled := True;                                                            // make "Options" menu available again
  end;
end;

{ User buildings entry (change of the grid edit fields content) }

procedure TfSkyscrapers.Square11Change(Sender: TObject);

begin
  BuildingsEntry(1, 1, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square12Change(Sender: TObject);

begin
  BuildingsEntry(1, 2, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square13Change(Sender: TObject);

begin
  BuildingsEntry(1, 3, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square14Change(Sender: TObject);

begin
  BuildingsEntry(1, 4, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square15Change(Sender: TObject);

begin
  BuildingsEntry(1, 5, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square21Change(Sender: TObject);

begin
  BuildingsEntry(2, 1, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square22Change(Sender: TObject);

begin
  BuildingsEntry(2, 2, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square23Change(Sender: TObject);

begin
  BuildingsEntry(2, 3, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square24Change(Sender: TObject);

begin
  BuildingsEntry(2, 4, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square25Change(Sender: TObject);

begin
  BuildingsEntry(2, 5, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square31Change(Sender: TObject);

begin
  BuildingsEntry(3, 1, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square32Change(Sender: TObject);

begin
  BuildingsEntry(3, 2, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square33Change(Sender: TObject);

begin
  BuildingsEntry(3, 3, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square34Change(Sender: TObject);

begin
  BuildingsEntry(3, 4, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square35Change(Sender: TObject);

begin
  BuildingsEntry(3, 5, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square41Change(Sender: TObject);

begin
  BuildingsEntry(4, 1, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square42Change(Sender: TObject);

begin
  BuildingsEntry(4, 2, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square43Change(Sender: TObject);

begin
  BuildingsEntry(4, 3, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square44Change(Sender: TObject);

begin
  BuildingsEntry(4, 4, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square45Change(Sender: TObject);

begin
  BuildingsEntry(4, 5, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square51Change(Sender: TObject);

begin
  BuildingsEntry(5, 1, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square52Change(Sender: TObject);

begin
  BuildingsEntry(5, 2, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square53Change(Sender: TObject);

begin
  BuildingsEntry(5, 3, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square54Change(Sender: TObject);

begin
  BuildingsEntry(5, 4, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

procedure TfSkyscrapers.Square55Change(Sender: TObject);

begin
  BuildingsEntry(5, 5, iSquares, bDone, bCancel, Grid, UGrid, iHints);
end;

end.

