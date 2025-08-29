{************************************}
{* Main unit for Bricks application *}
{************************************}

unit bricks_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TNumberShapes = array[0..5, 0..5] of TShape;
  TNumberLabels = array[0..5, 0..5] of TStaticText;
  TColors = array[0..5, 0..5] of TColor;
  {**********}
  { TfBricks }
  {**********}
  TfBricks = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize4, mOptionsSize6: TMenuItem;
    mHelp, mHelpCopyright, mHelpWebsite, MenuItem1, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
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
    edHelp, edRules: TMemo;
    btDone, btShow, btClearAll: TButton;
    bt1, bt2, bt3, bt4, bt5: TButton;
    bt6, btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mOptionsSize4Click(Sender: TObject);
    procedure mOptionsSize6Click(Sender: TObject);
    procedure mHelpCopyrightClick(Sender: TObject);
    procedure mHelpWebsiteClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btClearAllClick(Sender: TObject);
    procedure bt1Click(Sender: TObject);
    procedure bt2Click(Sender: TObject);
    procedure bt3Click(Sender: TObject);
    procedure bt4Click(Sender: TObject);
    procedure bt5Click(Sender: TObject);
    procedure bt6Click(Sender: TObject);
    procedure btClearClick(Sender: TObject);
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
    iSizeTemp, iSize, iRow, iCol: Integer;
    aGridNumbers, aBrick: TNumbers;
    aGridColors: TColors;
    shGridShapes: TNumberShapes;
    stGridNumbers: TNumberLabels;
    aSameBrick: array[0..5, 0..5] of Integer;
  end;

var
  fBricks: TfBricks;

implementation

{$R *.lfm}

{ Field selection at given grid position (user click at this filed) }

procedure SelectField(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels; var GridColors: TColors;
  Row, Col: Integer; var RowSave, ColSave: Integer);

begin
  if fBricks.btDone.Enabled and (GridNumbers[Row, Col].Font.Color <> clBlue) then begin
    // Click mustn't have any effect on fields, that have been filled in by the program (blue font statictexts)
    if GridShapes[Row, Col].Brush.Color = clBlack then begin
      // Number is already selected: unselect it (resetting color)
      GridShapes[Row, Col].Brush.Color := GridColors[Row, Col];
      GridNumbers[Row, Col].Font.Color := clBlack;
      RowSave := -1; ColSave := -1;                                            // no field actually selected
    end
    else begin
      // Field is not yet selected: select it (using highlighting color)
      if (RowSave <> -1) and (ColSave <> -1) then begin
        // Unselect previously selected field
        GridShapes[RowSave, ColSave].Brush.Color := GridColors[RowSave, ColSave];
        GridNumbers[RowSave, ColSave].Font.Color := clBlack;
      end;
      GridShapes[Row, Col].Brush.Color := clBlack;
      GridNumbers[Row, Col].Font.Color := clWhite;
      RowSave := Row; ColSave := Col;                                          // remember selected field (to place number there, when user pushes number button)
    end;
  end;
end;

{ Write given number into given grid field (user click on corr. number button) }

procedure WriteNumber(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels;
  var GridColors: TColors; Number: string; Row, Col: Integer);

begin
  if fBricks.btDone.Enabled then begin
    if (Row <> -1) and (Col <> -1) and (GridShapes[Row, Col].Brush.Color = clBlack) then begin
      // Do only if there is a field selcted
      GridNumbers[Row, Col].Caption := Number;                                 // write the number (or empty string if "Clear" button)
      GridShapes[Row, Col].Brush.Color := GridColors[Row, Col];
      GridNumbers[Row, Col].Font.Color := clBlack;
    end;
  end;
end;

{**********}
{ TfBricks }
{**********}

{ Application start: Initialisation }

procedure TfBricks.FormCreate(Sender: TObject);

begin
  // Create arrays with number shapes and statictexts
  shGridShapes[0, 0] := shGrid00; shGridShapes[0, 1] := shGrid01; shGridShapes[0, 2] := shGrid02; shGridShapes[0, 3] := shGrid03;
  shGridShapes[0, 4] := shGrid04; shGridShapes[0, 5] := shGrid05;
  shGridShapes[1, 0] := shGrid10; shGridShapes[1, 1] := shGrid11; shGridShapes[1, 2] := shGrid12; shGridShapes[1, 3] := shGrid13;
  shGridShapes[1, 4] := shGrid14; shGridShapes[1, 5] := shGrid15;
  shGridShapes[2, 0] := shGrid20; shGridShapes[2, 1] := shGrid21; shGridShapes[2, 2] := shGrid22; shGridShapes[2, 3] := shGrid23;
  shGridShapes[2, 4] := shGrid24; shGridShapes[2, 5] := shGrid25;
  shGridShapes[3, 0] := shGrid30; shGridShapes[3, 1] := shGrid31; shGridShapes[3, 2] := shGrid32; shGridShapes[3, 3] := shGrid33;
  shGridShapes[3, 4] := shGrid34; shGridShapes[3, 5] := shGrid35;
  shGridShapes[4, 0] := shGrid40; shGridShapes[4, 1] := shGrid41; shGridShapes[4, 2] := shGrid42; shGridShapes[4, 3] := shGrid43;
  shGridShapes[4, 4] := shGrid44; shGridShapes[4, 5] := shGrid45;
  shGridShapes[5, 0] := shGrid50; shGridShapes[5, 1] := shGrid51; shGridShapes[5, 2] := shGrid52; shGridShapes[5, 3] := shGrid53;
  shGridShapes[5, 4] := shGrid54; shGridShapes[5, 5] := shGrid55;
  stGridNumbers[0, 0] := laGrid00; stGridNumbers[0, 1] := laGrid01; stGridNumbers[0, 2] := laGrid02; stGridNumbers[0, 3] := laGrid03;
  stGridNumbers[0, 4] := laGrid04; stGridNumbers[0, 5] := laGrid05;
  stGridNumbers[1, 0] := laGrid10; stGridNumbers[1, 1] := laGrid11; stGridNumbers[1, 2] := laGrid12; stGridNumbers[1, 3] := laGrid13;
  stGridNumbers[1, 4] := laGrid14; stGridNumbers[1, 5] := laGrid15;
  stGridNumbers[2, 0] := laGrid20; stGridNumbers[2, 1] := laGrid21; stGridNumbers[2, 2] := laGrid22; stGridNumbers[2, 3] := laGrid23;
  stGridNumbers[2, 4] := laGrid24; stGridNumbers[2, 5] := laGrid25;
  stGridNumbers[3, 0] := laGrid30; stGridNumbers[3, 1] := laGrid31; stGridNumbers[3, 2] := laGrid32; stGridNumbers[3, 3] := laGrid33;
  stGridNumbers[3, 4] := laGrid34; stGridNumbers[3, 5] := laGrid35;
  stGridNumbers[4, 0] := laGrid40; stGridNumbers[4, 1] := laGrid41; stGridNumbers[4, 2] := laGrid42; stGridNumbers[4, 3] := laGrid43;
  stGridNumbers[4, 4] := laGrid44; stGridNumbers[4, 5] := laGrid45;
  stGridNumbers[5, 0] := laGrid50; stGridNumbers[5, 1] := laGrid51; stGridNumbers[5, 2] := laGrid52; stGridNumbers[5, 3] := laGrid53;
  stGridNumbers[5, 4] := laGrid54; stGridNumbers[5, 5] := laGrid55;
  // Start random number generator
  Randomize;
  // Application start-up parameters
  iSizeTemp := 6;
  iRow := -1; iCol := -1;
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfBricks.mPuzzleNewClick(Sender: TObject);

const
  Colors: array[0..8] of TColor = (
    $D7EBFA, $F0FFF0, $98FB98, $00D7FF, $00FFFF, $507FFF,
    $CBC0FF, $FAE6E6, $FACE87
  );
  LineSums: array[0..2] of Integer = (
    10, 15, 21
  );

var
  Col, N, D, I, J, K: Integer;
  OK, OK2, Display: Boolean;
  Displayed: array[1..3] of Integer;

begin
  // Set user selected puzzle size active now
  iSize := iSizeTemp;
  // Hide actually unused fields
  for I := 0 to 5 do begin
    for J := 0 to 5 do begin
      if (I > iSize - 1) or (J > iSize - 1) then begin
        // Actually unused fields
        shGridShapes[I, J].Visible := False; stGridNumbers[I, J].Visible := False;
      end
      else begin
        // Actually used fields
        shGridShapes[I, J].Visible := True; stGridNumbers[I, J].Visible := True;
      end;
    end;
  end;
  // Create random bricks
  N := 0;
  for I := 0 to iSize - 1 do begin
    // Craete row after row
    if Random(3) = 0 then begin
      // Overlapping bricks
      aBrick[I, 0] := N; aBrick[I, iSize - 1] := N;                            // each 2-field brick has number identifier, also used for color
      aSameBrick[I, 0] := iSize - 1; aSameBrick[I, iSize - 1] := 0;            // for each field, save the field, that is part of the same brick
      for J := 1 to iSize - 2 do begin
        if J mod 2 = 1 then begin
          K := J;
          Inc(N);
        end;
        aBrick[I, J] := N;
        if J mod 2 = 0 then begin
          aSameBrick[I, J] := K; aSameBrick[I, K] := J;
        end;
      end;
    end
    else begin
      // No overlapping bricks
      for J := 0 to iSize - 1 do begin
        if J mod 2 = 0 then begin
          K := J;
          Inc(N);
        end;
        aBrick[I, J] := N;
        if J mod 2 = 1 then begin
          aSameBrick[I, J] := K; aSameBrick[I, K] := J;
        end;
      end;
    end;
    Inc(N);
  end;
  // Generate random numbers at random grid position, according to the game rules
  repeat
    OK := True;
    // Clear the numbers array
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do
        aGridNumbers[I, J] := 0;
    end;
    // Generate the numbers
    for I := 0 to iSize - 2 do begin
      if OK then begin
        // Do row after row (except the last one: filled-in below)
        for K := 1 to iSize do begin
          // Place N numbers in each row
          repeat
            // For each of these numbers, choose random column in actual row (from those not yet filled in)
            // Choose other color, if the actual one would result in two even/odd numbers in same brick
            OK2 := True;
            Col := Random(iSize);
            if aGridNumbers[I, Col] <> 0 then
              OK2 := False
            else if (aGridNumbers[I, aSameBrick[I, Col]] <> 0) and (K mod 2 = aGridNumbers[I, aSameBrick[I, Col]] mod 2) then
              OK2 := False;
          until OK2;
          // Check if this column doesn't already contain this number
          for J := 0 to iSize - 1 do begin
            if aGridNumbers[J, Col] = K then                                   // if column already contains this number...
              OK := False;                                                     // ...start over
          end;
          if OK then
            aGridNumbers[I, Col] := K;                                         // store number into this row of numbers array
        end;
      end;
    end;
    // Fill-in last row, using the number not yet used in this column; this more than significantly speeds
    // up the procedure (in comparison with filling the line randomly, as done above for the other rows)
    for J := 0 to iSize - 1 do begin
      K := 0;
      for I := 0 to iSize - 2 do
        K += aGridNumbers[I, J];
      aGridNumbers[iSize - 1, J] := LineSums[iSize - 4] - K;
    end;
    // If in the last row, there are two even/odd numbers in the same brick, start over
    for J := 0 to iSize - 1 do begin
      if aGridNumbers[iSize - 1, J] mod 2 = aGridNumbers[iSize - 1, aSameBrick[iSize - 1, J]] mod 2 then
        OK := False;
    end;
  until OK;
  // Display bricks (by coloring the shapes) and some of the numbers
  for I := 0 to iSize - 1 do begin
    // Count of numbers displayed in this row
    if iSize = 4 then
      D := Random(2) + 1
    else
      D := Random(3) + 1;
    // Random columns to be used for displayed numbers
    for K := 1 to D do
      Displayed[K] := Random(iSize - 1);
    // For each filed in actual row, color the brick and (if has to be done for this field) display the field number
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := Colors[aBrick[I, J] mod 9];            // brick color on the form
      aGridColors[I, J] := Colors[aBrick[I, J] mod 9];                         // brick color save to array
      // Display or not the number in actual field
      Display := False;
      for K := 1 to D do begin
        if J = Displayed[K] then
          Display := True;
      end;
      if Display then begin
        stGridNumbers[I, J].Font.Color := clBlue;                              // blue font for numbers given by application
        stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J])
      end
      else begin
        stGridNumbers[I, J].Font.Color := clDefault;                           // black font for numbers to be entered by user
        stGridNumbers[I, J].Caption := '';
      end;
    end;
  end;
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfBricks.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Puzzle size > ...": Select size of the puzzle (4x4 or 6x6) }

procedure TfBricks.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfBricks.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu item "Help > Source and copyright": Display source and copyright of Bricks puzzle }

procedure TfBricks.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Bricks" at https://www.janko.at/ (puzzle name: "Ziegelmauer"). It may have been invented by Otto Janko ';
  S += 'and by this may underly the Creative Commons 3.0 licence: quotation of the author''s name; no commercial usage; ';
  S += 'distribution only under the same conditions. Concerning my "Bricks" PC application, itself, it is freeware/open ';
  S += 'source, as all programs at www.streetinfo.lu.';
  MessageDlg('Bricks', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel und Puzzles' website": Point web browser to janko.at }

procedure TfBricks.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > About": Display application about }

procedure TfBricks.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Bricks.' + LineEnding;
  S += 'Fill the grid fields with numbers from 1 to N, in a way, that each 2-field brick contains one even and one odd number.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "Bricks"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid and check if this is a correct solution }

procedure TfBricks.btDoneClick(Sender: TObject);

// Solution validity check is done by checking the user values and their uniqueness per row and column, and by checking if each brick
// contains one even and one odd number, and not by comparison of the user numbers and the generated grid numbers; in fact, there
// usually are several correct solutions...

var
  I, J, I1, J1: Integer;
  Solved: Boolean;
  UserNumbers: TNumbers;

begin
  // Disable field number entry (by setting actual field to none)
  iRow := -1; iCol := -1;
  // Get user values from form grid and insert them into an array, similar to the one created, when generating the puzzle
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if stGridNumbers[I, J].Caption = '' then
        UserNumbers[I, J] := -1
      else
        UserNumbers[I, J] := StrToInt(stGridNumbers[I, J].Caption);
    end;
  end;
  // Check values and uniqueness of row and column numbers
  Solved := True;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if (UserNumbers[I, J] < 1) or (UserNumbers[I, J] > iSize) then
        // Invalid number (ouside range 1..N)
        Solved := False
      else begin
        for I1 := 0 to iSize - 1 do begin
          if (I1 <> I) and (UserNumbers[I1, J] = UserNumbers[I, J]) then
            // Number more than once in a row
            Solved := False;
        end;
        for J1 := 0 to iSize - 1 do begin
          if (J1 <> J) and (UserNumbers[I, J1] = UserNumbers[I, J]) then
            // Number more than once in a column
            Solved := False;
        end;
      end;
    end;
  end;
  // Check if each brick has one odd and one even number
  if Solved then begin
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        if UserNumbers[I, J] mod 2 = UserNumbers[I, aSameBrick[I, J]] mod 2 then
          Solved := False;
      end;
    end;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Bricks', 'You have successfully solved this Bricks puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Bricks', 'Sorry. Your numbers are no valid solution of this Bricks puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
  // This will also disable any action, when a grid field or number button is clicked
  btDone.Enabled := False; btClearAll.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfBricks.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if stGridNumbers[I, J].Font.Color <> clBlue then begin
        // User fields only (other do never change)
        shGridShapes[I, J].Brush.Color := aGridColors[I, J];
        stGridNumbers[I, J].Font.Color := clBlack;
        stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J]);
      end;
    end;
  end;
  btShow.Enabled := False;
end;

{ Number buttons: Write corr. number into actually selected grid field }

procedure TfBricks.bt1Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '1', iRow, iCol);
end;

procedure TfBricks.bt2Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '2', iRow, iCol);
end;

procedure TfBricks.bt3Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '3', iRow, iCol);
end;

procedure TfBricks.bt4Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '4', iRow, iCol);
end;

procedure TfBricks.bt5Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '5', iRow, iCol);
end;

procedure TfBricks.bt6Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '6', iRow, iCol);
end;

{ Button "Clear": Clear number in actually selected grid field }

procedure TfBricks.btClearClick(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, '', iRow, iCol);
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfBricks.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if stGridNumbers[I, J].Font.Color <> clBlue then begin
        // User fields only
        shGridShapes[I, J].Brush.Color := aGridColors[I, J];
        stGridNumbers[I, J].Font.Color := clBlack;
        stGridNumbers[I, J].Caption := '';
      end;
    end;
  end;
  iRow := -1; iCol := -1;                                                      // no field actually selected
end;

{ User click on any of the 6x6 statictext fields: Highlight/unhighlight corresponding shape }

procedure TfBricks.laGrid00Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 0, 0, iRow, iCol);
end;

procedure TfBricks.laGrid01Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 0, 1, iRow, iCol);
end;

procedure TfBricks.laGrid02Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 0, 2, iRow, iCol);
end;

procedure TfBricks.laGrid03Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 0, 3, iRow, iCol);
end;

procedure TfBricks.laGrid04Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 0, 4, iRow, iCol);
end;

procedure TfBricks.laGrid05Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 0, 5, iRow, iCol);
end;

procedure TfBricks.laGrid10Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 1, 0, iRow, iCol);
end;

procedure TfBricks.laGrid11Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 1, 1, iRow, iCol);
end;

procedure TfBricks.laGrid12Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 1, 2, iRow, iCol);
end;

procedure TfBricks.laGrid13Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 1, 3, iRow, iCol);
end;

procedure TfBricks.laGrid14Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 1, 4, iRow, iCol);
end;

procedure TfBricks.laGrid15Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 1, 5, iRow, iCol);
end;

procedure TfBricks.laGrid20Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 2, 0, iRow, iCol);
end;

procedure TfBricks.laGrid21Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 2, 1, iRow, iCol);
end;

procedure TfBricks.laGrid22Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 2, 2, iRow, iCol);
end;

procedure TfBricks.laGrid23Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 2, 3, iRow, iCol);
end;

procedure TfBricks.laGrid24Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 2, 4, iRow, iCol);
end;

procedure TfBricks.laGrid25Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 2, 5, iRow, iCol);
end;

procedure TfBricks.laGrid30Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 3, 0, iRow, iCol);
end;

procedure TfBricks.laGrid31Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 3, 1, iRow, iCol);
end;

procedure TfBricks.laGrid32Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 3, 2, iRow, iCol);
end;

procedure TfBricks.laGrid33Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 3, 3, iRow, iCol);
end;

procedure TfBricks.laGrid34Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 3, 4, iRow, iCol);
end;

procedure TfBricks.laGrid35Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 3, 5, iRow, iCol);
end;

procedure TfBricks.laGrid40Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 4, 0, iRow, iCol);
end;

procedure TfBricks.laGrid41Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 4, 1, iRow, iCol);
end;

procedure TfBricks.laGrid42Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 4, 2, iRow, iCol);
end;

procedure TfBricks.laGrid43Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 4, 3, iRow, iCol);
end;

procedure TfBricks.laGrid44Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 4, 4, iRow, iCol);
end;

procedure TfBricks.laGrid45Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 4, 5, iRow, iCol);
end;

procedure TfBricks.laGrid50Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 5, 0, iRow, iCol);
end;

procedure TfBricks.laGrid51Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 5, 1, iRow, iCol);
end;

procedure TfBricks.laGrid52Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 5, 2, iRow, iCol);
end;

procedure TfBricks.laGrid53Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 5, 3, iRow, iCol);
end;

procedure TfBricks.laGrid54Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 5, 4, iRow, iCol);
end;

procedure TfBricks.laGrid55Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, 5, 5, iRow, iCol);
end;

end.

