{*****************************************}
{* Main unit for DoubleBlock application *}
{*****************************************}

unit dbleblock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TSums = array[0..5] of Integer;
  TNumberShapes = array[0..5, 0..5] of TShape;
  TNumberLabels = array[0..5, 0..5] of TStaticText;
  TSumShapes = array[0..5] of TShape;
  TSumLabels = array[0..5] of TStaticText;
  {***************}
  { TfDoubleBlock }
  {***************}
  TfDoubleBlock = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize4, mOptionsSize5, mOptionsSize6: TMenuItem;
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
    shRCSum, shRSum0, shRSum1, shRSum2, shRSum3, shRSum4, shRSum5: TShape;
    shCSum0, shCSum1, shCSum2, shCSum3, shCSum4, shCSum5: TShape;
    laRSum0, laRSum1, laRSum2, laRSum3, laRSum4, laRSum5: TStaticText;
    laCSum0, laCSum1, laCSum2, laCSum3, laCSum4, laCSum5: TStaticText;
    edHelp, edRules: TMemo;
    btDone, btShow, btClearAll: TButton;
    bt1, bt2, bt3, bt4: TButton;
    btColor, btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mOptionsSize4Click(Sender: TObject);
    procedure mOptionsSize5Click(Sender: TObject);
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
    procedure btColorClick(Sender: TObject);
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
    iOldColor: TColor;
    aGridNumbers: TNumbers;
    aRowSums, aColSums: TSums;
    shGridShapes: TNumberShapes;
    stGridNumbers: TNumberLabels;
    shRowSums, shColSums: TSumShapes;
    stRowSums, stColSums: TSumLabels;
  end;

var
  fDoubleBlock: TfDoubleBlock;

implementation

{$R *.lfm}

{ Field selection at given grid position (user click at this filed) }

procedure SelectField(var GridShapes: TNumberShapes; Row, Col: Integer; var RowSave, ColSave: Integer; var OldColor: TColor);

begin
  // Number is already selected: unselect it (resetting color)
  if GridShapes[Row, Col].Brush.Color = clRed then begin
    // Unselect the field
    GridShapes[Row, Col].Brush.Color := OldColor;
    RowSave := -1; ColSave := -1;                                              // no field actually selected
  end
  // Field is not yet selected: select it (using highlighting color)
  else begin
  // Unselect previously selected field
    if (RowSave <> -1) and (ColSave <> -1) then begin
      GridShapes[RowSave, ColSave].Brush.Color := OldColor;                    // restore color as it was before selection
    end;
    // Select the field
    OldColor := GridShapes[Row, Col].Brush.Color;                              // remember actual field color
    GridShapes[Row, Col].Brush.Color := clRed;
    RowSave := Row; ColSave := Col;                                            // remember which field is selected (to color it or place number there)
  end;
end;

{ Write given number into given grid field (user click on corr. number button) }

procedure WriteNumber(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels; Number: string; Row, Col: Integer; var OldColor: TColor);

// This procedure is also called, when the "Color" or "Clear" button is pushed

begin
  // Do only if there is a field selcted
  if (Row <> -1) and (Col <> -1) then begin
    if Number = 'color' then begin
      // User pushed "Color" button
      GridShapes[Row, Col].Brush.Color := clBlack;                             // color the field in black
      GridNumbers[Row, Col].Caption := '';
    end
    else if Number = 'clear' then begin
      // User pushed "Clear" button
      GridShapes[Row, Col].Brush.Color := clWhite;                             // reset the field color
      GridNumbers[Row, Col].Caption := '';                                     // remove the number
    end
    else begin
      // User clicked number button
      GridShapes[Row, Col].Brush.Color := clWhite;
      GridNumbers[Row, Col].Caption := Number;                                 // write the number clicked into field
    end;
    OldColor := GridShapes[Row, Col].Brush.Color;                              // save actual field color
  end;
end;

{***************}
{ TfDoubleBlock }
{***************}

{ Application start: Initialisations }

procedure TfDoubleBlock.FormCreate(Sender: TObject);

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
  // Create arrays with sums shapes and statictexts
  shRowSums[0] := shRSum0; shRowSums[1] := shRSum1; shRowSums[2] := shRSum2;
  shRowSums[3] := shRSum3; shRowSums[4] := shRSum4; shRowSums[5] := shRSum5;
  shColSums[0] := shCSum0; shColSums[1] := shCSum1; shColSums[2] := shCSum2;
  shColSums[3] := shCSum3; shColSums[4] := shCSum4; shColSums[5] := shCSum5;
  stRowSums[0] := laRSum0; stRowSums[1] := laRSum1; stRowSums[2] := laRSum2;
  stRowSums[3] := laRSum3; stRowSums[4] := laRSum4; stRowSums[5] := laRSum5;
  stColSums[0] := laCSum0; stColSums[1] := laCSum1; stColSums[2] := laCSum2;
  stColSums[3] := laCSum3; stColSums[4] := laCSum4; stColSums[5] := laCSum5;
  // Start random number generator
  Randomize;
  // Application start-up parameters
  iSizeTemp := 6;
  iRow := -1; iCol := -1;
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfDoubleBlock.mPuzzleNewClick(Sender: TObject);

const
  LineSums: array[0..2] of Integer = (
    10, 15, 21
  );

var
  Col, I, J, K: Integer;
  OK: Boolean;

begin
  iRow := -1; iCol := -1; iOldColor := clWhite;
  iSize := iSizeTemp;                                                          // set user selected puzzle size active now
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
  // Hide actually unused sum fields
  for I := 0 to 5 do begin
    if I > iSize - 1 then begin
      shRowSums[I].Visible := False; stRowSums[I].Visible := False;
    end
    else begin
      shRowSums[I].Visible := True; stRowSums[I].Visible := True;
    end;
  end;
  for J := 0 to 5 do begin
    if J > iSize - 1 then begin
      shColSums[J].Visible := False; stColSums[J].Visible := False;
    end
    else begin
      shColSums[J].Visible := True; stColSums[J].Visible := True;
    end;
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
      // Do row after row (except the last one: filled-in below)
      // For the moment fill all fields with numbers
      for K := 1 to iSize do begin
        // Place N numbers in each row
        repeat
          // For each of these numbers, choose random column in actual row
          Col := Random(iSize);
        until aGridNumbers[I, Col] = 0;                                        // field must be empty, of course
        // Check if this column doesn't already contain this number
        for J := 0 to iSize - 1 do begin
          if aGridNumbers[J, Col] = K then                                     // if column already contains this number...
            OK := False;                                                       // ...start over
        end;
        if OK then
          aGridNumbers[I, Col] := K;                                           // store number into this row of numbers array
      end;
    end;
  until OK;
  // Fill-in last row, using the number not yet used in this column
  // This more than significatively speeds up the procedure (in comparison with filling the line randomly, as done above for the other rows)
  for J := 0 to iSize - 1 do begin
    K := 0;
    for I := 0 to iSize - 2 do
      K += aGridNumbers[I, J];
    aGridNumbers[iSize - 1, J] := LineSums[iSize - 4] - K;
  end;
  // Finally, remove the "invalid" numbers, setting these array elements to 0 (meaning: this is a colored field )
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize -1 do begin
      if aGridNumbers[I, J] > iSize - 2 then
        aGridNumbers[I, J] := 0;
    end;
  end;
  // Calculate the sums of the numbers between the two colored row-fields (using array-element = 0 as indicator for colored field)
  for I := 0 to iSize - 1 do begin
    J := 0;
    while aGridNumbers[I, J] <> 0 do
      Inc(J);
    aRowSums[I] := 0;
    repeat
      if aGridNumbers[I, J] <> 0 then
        aRowSums[I] += aGridNumbers[I, J];
      Inc(J);
    until aGridNumbers[I, J] = 0;
  end;
  // Calculate the sums of the numbers betwwen the two colored column-fields
  for J := 0 to iSize - 1 do begin
    I := 0;
    while aGridNumbers[I, J] <> 0 do
      Inc(I);
    aColSums[J] := 0;
    repeat
      if aGridNumbers[I, J] <> 0 then
        aColSums[J] += aGridNumbers[I, J];
      Inc(I);
    until aGridNumbers[I, J] = 0;
  end;
  // Clear the number fields
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := clWhite; stGridNumbers[I, J].Caption := '';
    end;
  end;
  // Display the sums
  for I := 0 to iSize - 1 do
    stRowSums[I].Caption := IntToStr(aRowSums[I]);
  for J := 0 to iSize - 1 do
    stColSums[J].Caption := IntToStr(aColSums[J]);
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfDoubleBlock.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Puzzle size > ...": Select size of the puzzle (4x4, 5x5, 6x6) }

procedure TfDoubleBlock.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize5.Checked := False; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfDoubleBlock.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 5;
end;

procedure TfDoubleBlock.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu item "Help > Source and Copyright": Display puzzle origin and copyright info }

procedure TfDoubleBlock.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Double Block" at https://www.janko.at/ (puzzle name: "Doppelblock"). It seems not have been invented by Otto Janko, ';
  S += 'but found by himself at "Croco Puzzle" and is probably public domain. Concerning my "DoubleBlock" PC application, it is ';
  S += 'freeware/open source, as all my programs at www.streetinfo.lu.';
  MessageDlg('Double Block', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > Rätsel und Puzzles website": Point browser to Otto Janko's website }

procedure TfDoubleBlock.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > About": Display application about }

procedure TfDoubleBlock.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Double Block.' + LineEnding;
  S += 'Color some fields and write numbers into the other ones. The numbers at the margins indicate the sums of the numbers ';
  S += 'between two colored fields in the corresponding row resp. column.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, November 2020 - November 2022.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "DoubleBlock"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid values and check if this is a correct solution }

procedure TfDoubleBlock.btDoneClick(Sender: TObject);

// Solution validity check is done by checking the user values and their uniqueness per row and column, and by calculating the sums of
// the numbers between two colored fieds and not by comparison of the user and generated grid numbers; in fact, there might be several
// correct solutions...

var
  Sum, I, J, I1, J1: Integer;
  Solved: Boolean;
  UserNumbers: TNumbers;

begin
  // Disable field number entry (by setting actual field to none)
  iRow := -1; iCol := -1; iOldColor := clWhite;
  // Get user values from form grid and insert them into an array, similar to the one created, when generating the puzzle
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if shGridShapes[I, J].Brush.Color = clBlack then
        UserNumbers[I, J] := 0                                                 // use 0 as colored field indicator
      else begin
        if stGridNumbers[I, J].Caption = '' then
          UserNumbers[I, J] := -1                                              // no number written to white field: bad user solution
        else
          UserNumbers[I, J] := StrToInt(stGridNumbers[I, J].Caption);          // number written by user
      end;
    end;
  end;
  // Check values and uniqueness of row and column numbers
  Solved := True;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if shGridShapes[I, J].Brush.Color = clWhite then begin
        // Applies only to fields with numbers (not to the colored fields)
        if (UserNumbers[I, J] < 1) or (UserNumbers[I, J] > iSize - 2) then
          // Invalid number (ouside range 1 .. N-2)
          Solved := False
        else begin
          for I1 := 0 to iSize - 1 do begin
            if (shGridShapes[I1, J].Brush.Color = clWhite) and (I1 <> I) and (UserNumbers[I1, J] = UserNumbers[I, J]) then
              // Number more than once in a row
              Solved := False;
          end;
          for J1 := 0 to iSize - 1 do begin
            if (shGridShapes[I, J1].Brush.Color = clWhite) and (J1 <> J) and (UserNumbers[I, J1] = UserNumbers[I, J]) then
              // Number more than once in a column
              Solved := False;
          end;
        end;
      end;
    end;
  end;
  // Calculate sums of numbers between two colored fields and check if they equal the sums displayed at the left/top margins
  if Solved then begin
    // Row sums
    for I := 0 to iSize - 1 do begin
      J := 0;
      while shGridShapes[I, J].Brush.Color <> clBlack do
        Inc(J);
      Sum := 0;
      repeat
        if shGridShapes[I, J].Brush.Color <> clBlack then
          Sum += StrToInt(stGridNumbers[I, J].Caption);
        Inc(J);
      until shGridShapes[I, J].Brush.Color = clBlack;
      if Sum <> aRowSums[I] then
        Solved := False;
    end;
    // Column sums
    if Solved then begin
      for J := 0 to iSize - 1 do begin
        I := 0;
        while shGridShapes[I, J].Brush.Color <> clBlack do
          Inc(I);
        Sum := 0;
        repeat
          if shGridShapes[I, J].Brush.Color <> clBlack then
            Sum += StrToInt(stGridNumbers[I, J].Caption);
          Inc(I);
        until shGridShapes[I, J].Brush.Color = clBlack;
        if Sum <> aColSums[J] then
          Solved := False;
      end;
    end;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Double Block', 'You have successfully solved this Double Bock puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Double Block', 'Sorry. Your colored fields and numbers are no valid solution of this Double Block puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
  btDone.Enabled := False; btClearAll.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfDoubleBlock.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if aGridNumbers[I, J] = 0 then begin                                     // array-element = 0 indicates a colored field
        shGridShapes[I, J].Brush.Color := clBlack;
      end
      else begin
        shGridShapes[I, J].Brush.Color := clWhite;
        stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J]);
      end;
    end;
  end;
  btShow.Enabled := False;
end;

{ Number buttons: Write corr. number into actually selected grid field }

procedure TfDoubleBlock.bt1Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '1', iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.bt2Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '2', iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.bt3Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '3', iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.bt4Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '4', iRow, iCol, iOldColor);
end;


{ Button "Color": Color actually selected grid field }

procedure TfDoubleBlock.btColorClick(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, 'color', iRow, iCol, iOldColor);
end;


{ Button "Clear": Clear actually selected grid field }

procedure TfDoubleBlock.btClearClick(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, 'clear', iRow, iCol, iOldColor);
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfDoubleBlock.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := clWhite;
      stGridNumbers[I, J].Caption := '';
    end;
  end;
  iRow := -1; iCol := -1; iOldColor := clWhite;
end;

{ User click on any of the 6x6 statictext fields: Highlight/unhighlight corresponding shape }

procedure TfDoubleBlock.laGrid00Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 0, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid01Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 1, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid02Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 2, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid03Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 3, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid04Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 4, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid05Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 5, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid10Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 0, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid11Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 1, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid12Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 2, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid13Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 3, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid14Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 4, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid15Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 5, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid20Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 0, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid21Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 1, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid22Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 2, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid23Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 3, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid24Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 4, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid25Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 5, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid30Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 0, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid31Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 1, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid32Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 2, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid33Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 3, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid34Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 4, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid35Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 5, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid40Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 0, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid41Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 1, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid42Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 2, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid43Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 3, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid44Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 4, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid45Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 5, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid50Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 0, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid51Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 1, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid52Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 2, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid53Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 3, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid54Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 4, iRow, iCol, iOldColor);
end;

procedure TfDoubleBlock.laGrid55Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 5, iRow, iCol, iOldColor);
end;

end.

