{*****************************************}
{* Main unit for NumberCross application *}
{*****************************************}

unit nc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TSums = array[0..2, 0..5] of Integer;
  TGridShapes = array[0..5, 0..5] of TShape;
  TGridLabels = array[0..5, 0..5] of TStaticText;
  TSumLabels  = array[0..1, 0..5] of TStaticText;
  {******}
  { TfNC }
  {******}
  TfNC = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize4, mOptionsSize5, mOptionsSize6: TMenuItem;
    mOptionsSelection, mOptionsSelectionExcluded, mOptionsSelectionIncluded: TMenuItem;
    mHelp, mHelpCopyright, mHelpWebsite, MenuItem1, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edRules: TMemo;
    shGrid00, shGrid01, shGrid02, shGrid03, shGrid04, shGrid05: TShape;
    shGrid10, shGrid11, shGrid12, shGrid13, shGrid14, shGrid15: TShape;
    shGrid20, shGrid21, shGrid22, shGrid23, shGrid24, shGrid25: TShape;
    shGrid30, shGrid31, shGrid32, shGrid33, shGrid34, shGrid35: TShape;
    shGrid40, shGrid41, shGrid42, shGrid43, shGrid44, shGrid45: TShape;
    shGrid50, shGrid51, shGrid52, shGrid53, shGrid54, shGrid55: TShape;
    laGrid00, laGrid01, laGrid02, laGrid03, laGrid04, laGrid05: TStaticText;
    laGrid10, laGrid11, laGrid12, laGrid13, laGrid14, laGrid15: TStaticText;
    laGrid20, laGrid21, laGrid22, laGrid23, laGrid24, laGrid25: TStaticText;
    laGrid30, laGrid31, laGrid32, laGrid33, laGrid34, laGrid35: TStaticText;
    laGrid40, laGrid41, laGrid42, laGrid43, laGrid44, laGrid45: TStaticText;
    laGrid50, laGrid51, laGrid52, laGrid53, laGrid54, laGrid55: TStaticText;
    laSum00, laSum01, laSum02, laSum03, laSum04, laSum05: TStaticText;
    laSum10, laSum11, laSum12, laSum13, laSum14, laSum15: TStaticText;
    edHelp: TMemo;
    btShow: TButton;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mHelpCopyrightClick(Sender: TObject);
    procedure mHelpWebsiteClick(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mOptionsSize4Click(Sender: TObject);
    procedure mOptionsSize5Click(Sender: TObject);
    procedure mOptionsSize6Click(Sender: TObject);
    procedure mOptionsSelectionExcludedClick(Sender: TObject);
    procedure mOptionsSelectionIncludedClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure laGrid00Click(Sender: TObject);
    procedure laGrid01Click(Sender: TObject);
    procedure laGrid02Click(Sender: TObject);
    procedure laGrid03Click(Sender: TObject);
    procedure laGrid04Click(Sender: TObject);
    procedure laGrid05Click(Sender: TObject);
    procedure laGrid10Click(Sender: TObject);
    procedure laGrid11Click(Sender: TObject);
    procedure laGrid12Click(Sender: TObject);
    procedure laGrid13Click(Sender: TObject);
    procedure laGrid14Click(Sender: TObject);
    procedure laGrid15Click(Sender: TObject);
    procedure laGrid20Click(Sender: TObject);
    procedure laGrid21Click(Sender: TObject);
    procedure laGrid22Click(Sender: TObject);
    procedure laGrid23Click(Sender: TObject);
    procedure laGrid24Click(Sender: TObject);
    procedure laGrid25Click(Sender: TObject);
    procedure laGrid30Click(Sender: TObject);
    procedure laGrid31Click(Sender: TObject);
    procedure laGrid32Click(Sender: TObject);
    procedure laGrid33Click(Sender: TObject);
    procedure laGrid34Click(Sender: TObject);
    procedure laGrid35Click(Sender: TObject);
    procedure laGrid40Click(Sender: TObject);
    procedure laGrid41Click(Sender: TObject);
    procedure laGrid42Click(Sender: TObject);
    procedure laGrid43Click(Sender: TObject);
    procedure laGrid44Click(Sender: TObject);
    procedure laGrid45Click(Sender: TObject);
    procedure laGrid50Click(Sender: TObject);
    procedure laGrid51Click(Sender: TObject);
    procedure laGrid52Click(Sender: TObject);
    procedure laGrid53Click(Sender: TObject);
    procedure laGrid54Click(Sender: TObject);
    procedure laGrid55Click(Sender: TObject);
  private
    iSizeTemp, iSize: Integer;
    sSelectTemp, sSelect: string;
    aNumbers: TNumbers;
    aSums, aCounts: TSums;
    aGridShapes: TGridShapes;
    aGridLabels: TGridLabels;
    aSumLabels : TSumLabels;
  end;

var
  fNC: TfNC;

implementation

{$R *.lfm}

{ Selection of a number at given grid position (user click at this number) }

procedure NumberSelect(var GridShapes: TGridShapes; Row, Col: Integer; Select: string);

begin
  if GridShapes[Row, Col].Brush.Color = clWhite then begin
    // Number is not yet selected: select it (using highlighting color)
    if Select = 'excluded' then
      // Numbers to be excluded from selection highlighted in red
      GridShapes[Row, Col].Brush.Color := clRed
    else
      // Numbers to be included into selection highlighted in lime
      GridShapes[Row, Col].Brush.Color := clLime;
  end
  else
    // Number is already unselected: select it (resetting color to white)
    GridShapes[Row, Col].Brush.Color := clWhite;
end;

{******}
{ TfNC }
{******}

{ Application start: Initialisations }

procedure TfNC.FormCreate(Sender: TObject);

begin
  // Create arrays with number shapes and statictexts
  aGridShapes[0, 0] := shGrid00; aGridShapes[0, 1] := shGrid01; aGridShapes[0, 2] := shGrid02;
  aGridShapes[0, 3] := shGrid03; aGridShapes[0, 4] := shGrid04; aGridShapes[0, 5] := shGrid05;
  aGridShapes[1, 0] := shGrid10; aGridShapes[1, 1] := shGrid11; aGridShapes[1, 2] := shGrid12;
  aGridShapes[1, 3] := shGrid13; aGridShapes[1, 4] := shGrid14; aGridShapes[1, 5] := shGrid15;
  aGridShapes[2, 0] := shGrid20; aGridShapes[2, 1] := shGrid21; aGridShapes[2, 2] := shGrid22;
  aGridShapes[2, 3] := shGrid23; aGridShapes[2, 4] := shGrid24; aGridShapes[2, 5] := shGrid25;
  aGridShapes[3, 0] := shGrid30; aGridShapes[3, 1] := shGrid31; aGridShapes[3, 2] := shGrid32;
  aGridShapes[3, 3] := shGrid33; aGridShapes[3, 4] := shGrid34; aGridShapes[3, 5] := shGrid35;
  aGridShapes[4, 0] := shGrid40; aGridShapes[4, 1] := shGrid41; aGridShapes[4, 2] := shGrid42;
  aGridShapes[4, 3] := shGrid43; aGridShapes[4, 4] := shGrid44; aGridShapes[4, 5] := shGrid45;
  aGridShapes[5, 0] := shGrid50; aGridShapes[5, 1] := shGrid51; aGridShapes[5, 2] := shGrid52;
  aGridShapes[5, 3] := shGrid53; aGridShapes[5, 4] := shGrid54; aGridShapes[5, 5] := shGrid55;
  aGridLabels[0, 0] := laGrid00; aGridLabels[0, 1] := laGrid01; aGridLabels[0, 2] := laGrid02;
  aGridLabels[0, 3] := laGrid03; aGridLabels[0, 4] := laGrid04; aGridLabels[0, 5] := laGrid05;
  aGridLabels[1, 0] := laGrid10; aGridLabels[1, 1] := laGrid11; aGridLabels[1, 2] := laGrid12;
  aGridLabels[1, 3] := laGrid13; aGridLabels[1, 4] := laGrid14; aGridLabels[1, 5] := laGrid15;
  aGridLabels[2, 0] := laGrid20; aGridLabels[2, 1] := laGrid21; aGridLabels[2, 2] := laGrid22;
  aGridLabels[2, 3] := laGrid23; aGridLabels[2, 4] := laGrid24; aGridLabels[2, 5] := laGrid25;
  aGridLabels[3, 0] := laGrid30; aGridLabels[3, 1] := laGrid31; aGridLabels[3, 2] := laGrid32;
  aGridLabels[3, 3] := laGrid33; aGridLabels[3, 4] := laGrid34; aGridLabels[3, 5] := laGrid35;
  aGridLabels[4, 0] := laGrid40; aGridLabels[4, 1] := laGrid41; aGridLabels[4, 2] := laGrid42;
  aGridLabels[4, 3] := laGrid43; aGridLabels[4, 4] := laGrid44; aGridLabels[4, 5] := laGrid45;
  aGridLabels[5, 0] := laGrid50; aGridLabels[5, 1] := laGrid51; aGridLabels[5, 2] := laGrid52;
  aGridLabels[5, 3] := laGrid53; aGridLabels[5, 4] := laGrid54; aGridLabels[5, 5] := laGrid55;
  // Create array with row/column sum statictexts
  // Elements [0, n] = row sums; elements [1, n] = column sums
  aSumLabels[0, 0] := laSum00; aSumLabels[0, 1] := laSum01; aSumLabels[0, 2] := laSum02;
  aSumLabels[0, 3] := laSum03; aSumLabels[0, 4] := laSum04; aSumLabels[0, 5] := laSum05;
  aSumLabels[1, 0] := laSum10; aSumLabels[1, 1] := laSum11; aSumLabels[1, 2] := laSum12;
  aSumLabels[1, 3] := laSum13; aSumLabels[1, 4] := laSum14; aSumLabels[1, 5] := laSum15;
  // Start random number generator
  Randomize;
  // Application start-up options
  iSizeTemp := 5; sSelectTemp := 'excluded';
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfNC.mPuzzleNewClick(Sender: TObject);

const
  Help0: array[0.. 6] of Ansistring = (
    'NumberCross help:' + LineEnding + LineEnding,
    'Click a number, that you want to exclude from the selection; the corresponding field will be highlighted in red',
    ' (to unselect it, click it again). ',
    'The not selected numbers (white fields) will be used to calculate the row and column sums.' + LineEnding + LineEnding,
    'When you have selected all your numbers, push the "Done" button to check your solution.',
    'Click a number, that you want to include into the selection; the corresponding field will be highlighted in green',
    'The selected numbers (green fields) will be used to calculate the row and column sums.' + LineEnding + LineEnding
  );
var
  I, J: Integer;
  Help: array[0.. 4] of Ansistring;

begin
  // Set user option selections active now
  iSize := iSizeTemp; sSelect := sSelectTemp;
  // Adapt help text
  for I := 0 to 4 do
    Help[I] := Help0[I];
  if sSelect = 'included' then begin
    Help[1] := Help0[5]; Help[3] := Help0[6];
  end;
  edHelp.Text := '';
  for I := 0 to 4 do
    edHelp.Text := edHelp.Text + Help[I];
  // Set row/col sums to 0; hide the corr. statictexts, if not part of actual puzzle
  for I := 0 to 1 do begin
    for J := 0 to 5 do begin
      aSums[I, J] := 0; aCounts[I, J] := 0;
      if J > iSize - 1 then
        aSumLabels[I, J].Visible := False
      else
        aSumLabels[I, J].Visible := True;
    end;
  end;
  // Generate a random number for each (actual) grid position
  for I := 0 to 5 do begin
    for J := 0 to 5 do begin
      // Hide number shapes and statictexts, if not part of actual puzzle
      if (I > iSize - 1) or (J > iSize - 1) then begin
        aGridShapes[I, J].Visible := False;
        aGridLabels[I, J].Visible := False;
      end
      // For those being part of the puzzle, fill-in random number between 1 and 9
      else begin
        aNumbers[I, J] := Random(9) + 1;
        aGridShapes[I, J].Visible := True; aGridShapes[I, J].Brush.Color := clWhite;
        aGridLabels[I, J].Visible := True;
        aGridLabels[I, J].Caption := IntToStr(aNumbers[I, J]);
      end;
    end;
  end;
  // Randomly choose the numbers being part of the selection
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if Random(2) = 1 then
        aNumbers[I, J] := -aNumbers[I, J];                                     // numbers being set negative here, will be those NOT part of the selection
    end;
  end;
  // For the (puzzle-size - 1) first rows, count number of selected numbers
  for I := 0 to iSize - 2 do begin
    for J := 0 to iSize - 2 do begin
      if aNumbers[I, J] > 0 then begin
        aCounts[0, I] += 1;
      end;
    end;
  end;
  // For the (puzzle-size - 1) first columns, count number of selected numbers
  for I := 0 to iSize - 2 do begin
    for J := 0 to iSize - 2 do begin
      if aNumbers[I, J] > 0 then begin
        aCounts[1, J] += 1;
      end;
    end;
  end;
  // Adapt (puzzle-size)th row and (puzzle-sizeth column value, depending on the counts, calculated before
  // This is a simple way to avoid rows/columns with zero or all numbers being part of the selection
  for I := 0 to iSize - 2 do begin
    // Adapt (puzzle-size)th column values
    if (aCounts[0, I] = 0) or (aCounts[0, I] = 1) then
      // Be sure, at least 1 number per row is part of selection
      // Avoid "as much as possible" rows with only 1 number being part of selection
      aNumbers[I, iSize - 1] := Abs(aNumbers[I, iSize - 1])
    else if aCounts[0, I] = iSize then
      // Do not allow rows with all numbers being part of selection
      aNumbers[I, iSize - 1] := -Abs(aNumbers[I, iSize - 1]);
  end;
  for I := 0 to iSize - 2 do begin
    // Adapt (puzzle-size)th row values
    if (aCounts[1, I] = 0) or (aCounts[1, I] = 1) then
      // Make sure, at least 1 number per column is part of selection
      // Avoid "as much as possible" columns with only 1 number being part of selection
      aNumbers[iSize - 1, I] := Abs(aNumbers[iSize - 1, I])
    else if aCounts[1, I] = iSize then
      // Do not allow columns with all numbers being part of selection
      aNumbers[iSize - 1, I] := -Abs(aNumbers[iSize - 1, I]);
  end;
  // Count number of selected numbers in (puzzle-size)th row
  for I := 0 to iSize - 1 do begin
    if aNumbers[I, iSize - 1] > 0 then begin
      aCounts[0, iSize - 1] += 1;
    end;
  end;
  // Count number of selected numbers in (puzzle-size)th column
  for I := 0 to iSize - 1 do begin
    if aNumbers[iSize - 1, I] > 0 then begin
      aCounts[1, iSize - 1] += 1;
    end;
  end;
  // Adapt value at (puzzle-size)th row and (puzzle-size)th column value, depending on the counts, calculated before
  // This that the last row or last column doesn't contain any numbers being part of the selection
  if (aCounts[0, iSize - 1] = 0) or (aCounts[1, iSize - 1] = 0) then
    // Make sure, at least 1 number of (puzzle-size)th row and column is part of selection
    aNumbers[iSize - 1, iSize - 1] := Abs(aNumbers[iSize - 1, iSize - 1])
  else if (aCounts[0, iSize - 1] = iSize) or (aCounts[1, iSize - 1] = iSize) then
    // If possible, avoid that all numbers of (puzzle-size)th row or column are part of selection
    aNumbers[iSize - 1, iSize - 1] := -Abs(aNumbers[iSize - 1, iSize - 1]);
  // For each row, calculate sum of selected numbers
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if aNumbers[I, J] > 0 then begin
        aSums[0, I] += Abs(aNumbers[I, J]);
      end;
    end;
  end;
  // For each column, calculate sum of selected numbers
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if aNumbers[I, J] > 0 then begin
        aSums[1, J] += Abs(aNumbers[I, J]);
      end;
    end;
  end;
  // Fill sum values into fields at the top and the left of the grid
  for I := 0 to 1 do begin
    for J := 0 to iSize - 1 do begin
      aSumLabels[I, J].Caption := IntToStr(aSums[I, J]);
    end;
  end;
  // Re-enable the "Done" button
  btDone.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfNC.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Puzzle size > ...": Select size of the puzzle (4x4, 5x5, 6x6) }

procedure TfNC.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize5.Checked := False; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                                        // option becomes only active when "New puzzle" is selected
end;

procedure TfNC.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 5;
end;

procedure TfNC.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu item "Options > Number selection > ...": Choose if user-clicked numbers are those to be excluded from or included into the selection }

procedure TfNC.mOptionsSelectionExcludedClick(Sender: TObject);

begin
  mOptionsSelectionExcluded.Checked := True; mOptionsSelectionIncluded.Checked := False;
  sSelectTemp := 'excluded';
end;

procedure TfNC.mOptionsSelectionIncludedClick(Sender: TObject);

begin
  mOptionsSelectionExcluded.Checked := False; mOptionsSelectionIncluded.Checked := True;
  sSelectTemp := 'included';
end;

{ Menu item "Help > Source and copyright": Display source and copyright info }

procedure TfNC.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Number Cross" at https://www.janko.at/ (puzzle name: "Zahlenkreuz"). It may have been invented by Otto Janko ';
  S += 'and by this may underly the Creative Commons 3.0 licence: quotation of the author''s name; no commercial usage; distribution only ';
  S += 'under the same conditions.';
  MessageDlg('NumberCross licence', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel and Puzzles' website": Open browser at 'Rätsel and Puzzles' website }

procedure TfNC.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > About": Display application about }

procedure TfNC.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Number Cross.' + LineEnding;
  S += 'Find the numbers that added per row resp. column equal a given row/column sum.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June 2020.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "NumberCross"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid values and check if this is a correct solution }

procedure TfNC.btDoneClick(Sender: TObject);

var
  I, J: Integer;
  Solved: Boolean;
  UserNumbers: array[0..5, 0..5] of Integer;
  UserSums: array[0..1, 0..5] of Integer;

begin
  // Get user values from form grid and insert them into an array, similar to the one created, when generating the puzzle
  // In this array, mark all not selected numbers by setting them negative
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      UserNumbers[I, J] := StrToInt(aGridLabels[I, J].Caption);
      if (sSelect = 'excluded') and (aGridShapes[I, J].Brush.Color = clRed) then
        // If numbers to be excluded are highlighted, highlighted numbers are not part of selection
        UserNumbers[I, J] := -UserNumbers[I, J]
      else if (sSelect = 'included') and (aGridShapes[I, J].Brush.Color = clWhite) then
        // If numbers to be included are highlighted, non highlighted numbers are not part of selection
        UserNumbers[I, J] := -UserNumbers[I, J];
    end;
  end;
  // Calculate row and column sums of the "user grid"
  for I := 0 to 1 do begin
    for J := 0 to iSize - 1 do
      UserSums[I, J] := 0;
  end;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if UserNumbers[I, J] > 0 then begin
        UserSums[0, I] += Abs(UserNumbers[I, J]);
      end;
    end;
  end;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if UserNumbers[I, J] > 0 then begin
        UserSums[1, J] += Abs(UserNumbers[I, J]);
      end;
    end;
  end;
  // Compare the "user sums" with the values, that they have to be
  // If the value pairs are different, the puzzle has not been solved
  Solved := True;
  for I := 0 to 1 do begin
    for J := 0 to iSize - 1 do begin
      if UserSums[I, J] <> aSums[I, J] then
        Solved := False;
    end;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Number Cross', 'You have successfully solved this Number Cross puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Number Cross', 'Sorry. Your numbers are no valid solution of this Number Cross puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" button (until "New puzzle" is chosen)
  btDone.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfNC.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize -1 do begin
    for J := 0 to iSize - 1 do begin
      // Set shape color to "nothing"
      aGridShapes[I, J].Brush.Color := clWhite;
      // Highlight excluded numbers
      if (sSelect = 'excluded') and (aNumbers[I, J] < 0) then
        aGridShapes[I, J].Brush.Color := clRed
      // Highlight included numbers
      else if (sSelect = 'included') and (aNumbers[I, J] > 0) then
        aGridShapes[I, J].Brush.Color := clLime;
    end;
  end;
end;

{ User click on any of the 6x6 statictexts fields: Highlight/un-highlight corresponding shape }

procedure TfNC.laGrid00Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 0, 0, sSelect);
end;

procedure TfNC.laGrid01Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 0, 1, sSelect);
end;

procedure TfNC.laGrid02Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 0, 2, sSelect);
end;

procedure TfNC.laGrid03Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 0, 3, sSelect);
end;

procedure TfNC.laGrid04Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 0, 4, sSelect);
end;

procedure TfNC.laGrid05Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 0, 5, sSelect);
end;

procedure TfNC.laGrid10Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 1, 0, sSelect);
end;

procedure TfNC.laGrid11Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 1, 1, sSelect);
end;

procedure TfNC.laGrid12Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 1, 2, sSelect);
end;

procedure TfNC.laGrid13Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 1, 3, sSelect);
end;

procedure TfNC.laGrid14Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 1, 4, sSelect);
end;

procedure TfNC.laGrid15Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 1, 5, sSelect);
end;

procedure TfNC.laGrid20Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 2, 0, sSelect);
end;

procedure TfNC.laGrid21Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 2, 1, sSelect);
end;

procedure TfNC.laGrid22Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 2, 2, sSelect);
end;

procedure TfNC.laGrid23Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 2, 3, sSelect);
end;

procedure TfNC.laGrid24Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 2, 4, sSelect);
end;

procedure TfNC.laGrid25Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 2, 5, sSelect);
end;

procedure TfNC.laGrid30Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 3, 0, sSelect);
end;

procedure TfNC.laGrid31Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 3, 1, sSelect);
end;

procedure TfNC.laGrid32Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 3, 2, sSelect);
end;

procedure TfNC.laGrid33Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 3, 3, sSelect);
end;

procedure TfNC.laGrid34Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 3, 4, sSelect);
end;

procedure TfNC.laGrid35Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 3, 5, sSelect);
end;

procedure TfNC.laGrid40Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 4, 0, sSelect);
end;

procedure TfNC.laGrid41Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 4, 1, sSelect);
end;

procedure TfNC.laGrid42Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 4, 2, sSelect);
end;

procedure TfNC.laGrid43Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 4, 3, sSelect);
end;

procedure TfNC.laGrid44Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 4, 4, sSelect);
end;

procedure TfNC.laGrid45Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 4, 5, sSelect);
end;

procedure TfNC.laGrid50Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 5, 0, sSelect);
end;

procedure TfNC.laGrid51Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 5, 1, sSelect);
end;

procedure TfNC.laGrid52Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 5, 2, sSelect);
end;

procedure TfNC.laGrid53Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 5, 3, sSelect);
end;

procedure TfNC.laGrid54Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 5, 4, sSelect);
end;

procedure TfNC.laGrid55Click(Sender: TObject);

begin
  NumberSelect(aGridShapes, 5, 5, sSelect);
end;

end.

