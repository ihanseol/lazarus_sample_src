{************************************}
{* Main unit for Yakuso application *}
{************************************}

unit yakuso_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TShowNumbers = array[0..5, 0..5] of Boolean;
  TNumberShapes = array[0..5, 0..5] of TShape;
  TNumberLabels = array[0..5, 0..5] of TStaticText;
  {**********}
  { TfYakuso }
  {**********}
  TfYakuso = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize3, mOptionsSize4, mOptionsSize5: TMenuItem;
    mHelp, mHelpCopyright, mHelpWebsite, mHelpWebsite2, MenuItem1, mHelpAbout: TMenuItem;
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
    bt0, bt1, bt2, bt3, bt4, bt5: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mOptionsSize3Click(Sender: TObject);
    procedure mOptionsSize4Click(Sender: TObject);
    procedure mOptionsSize5Click(Sender: TObject);
    procedure mHelpCopyrightClick(Sender: TObject);
    procedure mHelpWebsiteClick(Sender: TObject);
    procedure mHelpWebsite2Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btClearAllClick(Sender: TObject);
    procedure bt0Click(Sender: TObject);
    procedure bt1Click(Sender: TObject);
    procedure bt2Click(Sender: TObject);
    procedure bt3Click(Sender: TObject);
    procedure bt4Click(Sender: TObject);
    procedure bt5Click(Sender: TObject);
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
  private
    iSizeTemp, iSize, iRow, iCol: Integer;
    aGridNumbers: TNumbers;
    aShowNumbers: TShowNumbers;
    shGridShapes: TNumberShapes;
    stGridNumbers: TNumberLabels;
  end;

var
  fYakuso: TfYakuso;

implementation

{$R *.lfm}

{ Field selection at given grid position (user click at this filed) }

procedure SelectField(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels; Row, Col: Integer; var RowSave, ColSave: Integer);

begin
  if GridShapes[Row, Col].Brush.Color <> clYellow then begin                   // can't modify numbers given by the application
    // Number is already selected: unselect it (resetting color)
    if GridShapes[Row, Col].Brush.Color = clRed then begin
      // Unselect the field
      if GridNumbers[Row, Col].Caption = '' then
        GridShapes[Row, Col].Brush.Color := clWhite                            // empty field
      else
        GridShapes[Row, Col].Brush.Color := clLime;                            // field, containing a number
      RowSave := -1; ColSave := -1;                                            // no field actually selected
    end
    // Field is not yet selected: select it (using highlighting color)
    else begin
      // Unselect previously selected field
      if (RowSave <> -1) and (ColSave <> -1) then begin
        if GridNumbers[RowSave, ColSave].Caption = '' then
          GridShapes[RowSave, ColSave].Brush.Color := clWhite                  // empty field
        else
          GridShapes[RowSave, ColSave].Brush.Color := clLime;                  // field, containing a number
      end;
      // Select the field
      GridShapes[Row, Col].Brush.Color := clRed;
      // Remember which field was selected (to place number there, when user pushes a number button)
      RowSave := Row; ColSave := Col;
    end;
  end;
end;

{ Write given number into given grid field (user click on corr. number button) }

procedure WriteNumber(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels; Number: string; Row, Col: Integer);

begin
  // Do only if there is a field selcted
  if (Row <> -1) and (Col <> -1) then begin
    GridNumbers[Row, Col].Caption := Number;                                   // write the number (or empty string if "Clear" button)
    if Number = '' then
      GridShapes[Row, Col].Brush.Color := clWhite                              // field number was cleared
    else
      GridShapes[Row, Col].Brush.Color := clLime;                              // field number was set
  end;
end;

{**********}
{ TfYakuso }
{**********}

{ Application start: Initialisations }

procedure TfYakuso.FormCreate(Sender: TObject);

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
  iSizeTemp := 4;
  iRow := -1; iCol := -1;                                                      // no grid field actually selected
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfYakuso.mPuzzleNewClick(Sender: TObject);

var
  Col, M, N, N1, N2, I, J, K, L: Integer;
  OK: Boolean;
  NumbersUsed: array[1..5] of Boolean;

begin
  // Set user option selection active now
  iSize := iSizeTemp;
  // Hide actually unused fields
  for I := 0 to 5 do begin
    for J := 0 to 5 do begin
      if (I > iSize) or (J > iSize) then begin
        // Actually unused fields
        shGridShapes[I, J].Visible := False; stGridNumbers[I, J].Visible := False;
      end
      else begin
        // Actually used fields
        shGridShapes[I, J].Visible := True; stGridNumbers[I, J].Visible := True;
        if I = iSize then
          // Sum fields
          shGridShapes[I, J].Brush.Color := clCream
        else
          // Number fields
          shGridShapes[I, J].Brush.Color := clWhite;
      end;
    end;
  end;
  // Disable number buttons not used
  bt4.Enabled := False; bt5.Enabled := False;
  if iSize > 3 then
    bt4.Enabled := True;
  if iSize > 4 then
    bt5.Enabled := True;
  // Generate the numbers according to the Yakuso game rules
  repeat
    OK := True;
    // Clear the numbers array
    for I := 0 to iSize do begin
      for J := 0 to iSize do begin
        aGridNumbers[I, J] := 0;
        if I = iSize then
          aShowNumbers[I, J] := True
        else
          aShowNumbers[I, J] := False;
      end;
    end;
    // Clear the "numbers used" array
    for I := 1 to iSize do
      NumbersUsed[I] := False;
    // Generate the numbers (row after row)
    for I := 0 to iSize - 1 do begin
      // Choose random number from 1 to N (must not have been used before)
      repeat
        M := Random(iSize) + 1;
      until not NumbersUsed[M];
      NumbersUsed[M] := True;
      // Place the number M in random free fields of current row (exactly M times)
      for J := 1 to M do begin
        repeat
          Col := Random(iSize + 1);
        until aGridNumbers[I, Col] = 0;                                        // field must be empty, of course
        aGridNumbers[I, Col] := M;
      end;
    end;
    // Calculate the sums
    for J := 0 to iSize do begin
      aGridNumbers[iSize, J] := 0;
      for I := 0 to iSize - 1 do
        aGridNumbers[iSize, J] += aGridNumbers[I, J]
    end;
    // Do not allow columns without numbers (all zeros)
    for J := 0 to iSize do begin
      if aGridNumbers[iSize, J] = 0 then
        OK := False;
    end;
  until OK;
  // Zeros to show
  L := Random(iSize);                                                          // no zeros to show in this row
  for I := 0 to iSize - 1 do begin
    if I <> L then begin
      // Row is one, where some zeros will be shown
      K := 0;
      // Count zeros actually in this row
      for J := 0 to iSize do begin
        if aGridNumbers[I, J] = 0 then
          Inc(K);
      end;
      // Randomly show between 1 and all zeros in this row
      for N := 1 to Random(K) + 1 do begin
        repeat
          Col := Random(iSize + 1);
        until (aGridNumbers[I, Col] = 0) and not aShowNumbers[I, Col];
        aShowNumbers[I, Col] := True;
      end;
    end;
  end;
  // Sums to hide
  N1 := 1;
  N2 := Random(1 + (iSize - 3)) + 1;
  for J := N1 to N2 do begin
    repeat
      Col := Random(iSize + 1);
    until aShowNumbers[iSize, Col];
    aShowNumbers[iSize, Col] := False;
  end;
  // Fill in the grid numbers and sums (those that will actually be shown)
  for I := 0 to iSize do begin
    for J := 0 to iSize do begin
      if aShowNumbers[I, J] then begin
        // Numbers (sums) to be shown
        stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J]);           // write number into the grid
        if I < iSize then
          // Number rows
          shGridShapes[I, J].Brush.Color := clYellow;
      end
      else begin
        // Numbers (sums) to be hidden
        stGridNumbers[I, J].Caption := '';
        if I < iSize then
          shGridShapes[I, J].Brush.Color := clWhite;
      end;
    end;
  end;
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfYakuso.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Puzzle size > ...": Select size of the puzzle (3x4, 4x5, 5x6) }

procedure TfYakuso.mOptionsSize3Click(Sender: TObject);

begin
  mOptionsSize3.Checked := True; mOptionsSize4.Checked := False; mOptionsSize5.Checked := False;
  iSizeTemp := 3;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfYakuso.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize3.Checked := False; mOptionsSize4.Checked := True; mOptionsSize5.Checked := False;
  iSizeTemp := 4;
end;

procedure TfYakuso.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize3.Checked := False; mOptionsSize4.Checked := False; mOptionsSize5.Checked := True;
  iSizeTemp := 5;
end;

{ Menu item "Help > Source and copyright": Display license information }

procedure TfYakuso.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Yakuso" at https://www.janko.at/. The game has been originally published by Bertrand Leplay ';
  S += '(http://bertrandleplay.wixsite.com/yakuso) in the magazine "Multilogic" (December 2009). It (probably) underlies the ';
  S += 'Creative Commons 3.0 licence: quotation of the author''s name; no commercial usage; distribution only under the same conditions.';
  MessageDlg('Yakuso license', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel und Puzzles' website": Point browser to 'Rätsel und Puzzles' website }

procedure TfYakuso.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > Yakuso website of Bertrand Leplay": Point browser to Bertrand Leplay's website }

procedure TfYakuso.mHelpWebsite2Click(Sender: TObject);

begin
  OpenDocument('http://bertrandleplay.wixsite.com/yakuso');
end;

{ Menu item "Help > About": Display application about }

procedure TfYakuso.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Yakuso.' + LineEnding;
  S += 'Fill the grid fields with numbers from 1 to N or 0, according to the Yakuso game rules.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September 2020.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "Yakuso"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from grid fields and check if this is a correct solution }

procedure TfYakuso.btDoneClick(Sender: TObject);

// Solution validity check is done by checking the user values (all identical numbers M in a row and present M times)
// and by calculating the column sums; in fact, there might be several correct solutions...

var
  Sum, M, N, I, J: Integer;
  Solved: Boolean;
  UserNumbers: TNumbers;

begin
  // Disable field number entry (by setting actual field to none)
  iRow := -1; iCol := -1;
  // Get user values from form grid and insert them into an array, similar to the one created, when generating the puzzle
  for I := 0 to iSize do begin
    for J := 0 to iSize do begin
      if stGridNumbers[I, J].Caption = '' then
        UserNumbers[I, J] := -1
      else
        UserNumbers[I, J] := StrToInt(stGridNumbers[I, J].Caption);
    end;
  end;
  // Check user numbers validity
  Solved := True;
  for I := 0 to iSize - 1 do begin
    // Check if all numbers in a row are identical
    if Solved then begin                                                       // no need to continue, if there was a user error in grid row before
      // Get number M for this row: the one shown, if any, otherwise the first non zero number
      M := 0;
      for J := 0 to iSize do begin
        if (UserNumbers[I, J] <> 0) and (M = 0) then
          M := UserNumbers[I, J];
        if (UserNumbers[I, J] <> 0) and (shGridShapes[I, J].Brush.Color = clYellow) then
          M := UserNumbers[I, J];
      end;
      // Check if all numbers in the row are equal to M (or to 0)
      for J := 0 to iSize do begin
        if shGridShapes[I, J].Brush.Color = clWhite then begin
          if (UserNumbers[I, J] <> 0) and (UserNumbers[I, J] <> M) then
            Solved := False;
        end;
      end;
      // Check if number M is present M times
      if Solved then begin
        N := 0;
        for J := 0 to iSize do begin
          if UserNumbers[I, J] = M then
            Inc(N);
        end;
        if N <> M then
          Solved := False;
      end;
    end;
  end;
  // Check the column sums (only for those shown!)
  if Solved then begin
    for J := 0 to iSize do begin
      if aShowNumbers[iSize, J] then begin
        // If user solution is different from program solution, not shown sums may differ!
        Sum := 0;
        for I := 0 to iSize - 1 do begin
          Sum += UserNumbers[I, J];
        end;
        if Sum <> aGridNumbers[iSize, J] then
          Solved := False;
      end;
    end;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Kakkuru', 'You have successfully solved this Kakkuru puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Kakkuru', 'Sorry. Your numbers are no valid solution of this Kakkuru puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
  btDone.Enabled := False; btClearAll.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfYakuso.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize do begin
      if shGridShapes[I, J].Brush.Color <> clYellow then begin
        // Do for "not given by application" numbers, only
        shGridShapes[I, J].Brush.Color := clLime;
        stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J]);
      end;
    end;
  end;
  btShow.Enabled := False;
end;

{ Number buttons: Write corr. number into actually selected grid field }

procedure TfYakuso.bt0Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '0', iRow, iCol);
end;

procedure TfYakuso.bt1Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '1', iRow, iCol);
end;

procedure TfYakuso.bt2Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '2', iRow, iCol);
end;

procedure TfYakuso.bt3Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '3', iRow, iCol);
end;

procedure TfYakuso.bt4Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '4', iRow, iCol);
end;

procedure TfYakuso.bt5Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '5', iRow, iCol);
end;

{ Button "Clear": Clear number in actually selected grid field }

procedure TfYakuso.btClearClick(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '', iRow, iCol);
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfYakuso.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize do begin
      if shGridShapes[I, J].Brush.Color <> clYellow then begin
        shGridShapes[I, J].Brush.Color := clWhite;
        stGridNumbers[I, J].Caption := '';
      end;
    end;
  end;
  iRow := -1; iCol := -1;                                                      // no field actually selected
end;

{ User click on any of the 5x6 static-text fields: Highlight/unhighlight corresponding shape }

procedure TfYakuso.laGrid00Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 0, iRow, iCol);
end;

procedure TfYakuso.laGrid01Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 1, iRow, iCol);
end;

procedure TfYakuso.laGrid02Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 2, iRow, iCol);
end;

procedure TfYakuso.laGrid03Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 3, iRow, iCol);
end;

procedure TfYakuso.laGrid04Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 4, iRow, iCol);
end;

procedure TfYakuso.laGrid05Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 5, iRow, iCol);
end;

procedure TfYakuso.laGrid10Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 0, iRow, iCol);
end;

procedure TfYakuso.laGrid11Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 1, iRow, iCol);
end;

procedure TfYakuso.laGrid12Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 2, iRow, iCol);
end;

procedure TfYakuso.laGrid13Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 3, iRow, iCol);
end;

procedure TfYakuso.laGrid14Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 4, iRow, iCol);
end;

procedure TfYakuso.laGrid15Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 5, iRow, iCol);
end;

procedure TfYakuso.laGrid20Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 0, iRow, iCol);
end;

procedure TfYakuso.laGrid21Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 1, iRow, iCol);
end;

procedure TfYakuso.laGrid22Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 2, iRow, iCol);
end;

procedure TfYakuso.laGrid23Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 3, iRow, iCol);
end;

procedure TfYakuso.laGrid24Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 4, iRow, iCol);
end;

procedure TfYakuso.laGrid25Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 5, iRow, iCol);
end;

procedure TfYakuso.laGrid30Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 0, iRow, iCol);
end;

procedure TfYakuso.laGrid31Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 1, iRow, iCol);
end;

procedure TfYakuso.laGrid32Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 2, iRow, iCol);
end;

procedure TfYakuso.laGrid33Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 3, iRow, iCol);
end;

procedure TfYakuso.laGrid34Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 4, iRow, iCol);
end;

procedure TfYakuso.laGrid35Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 5, iRow, iCol);
end;

procedure TfYakuso.laGrid40Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 0, iRow, iCol);
end;

procedure TfYakuso.laGrid41Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 1, iRow, iCol);
end;

procedure TfYakuso.laGrid42Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 2, iRow, iCol);
end;

procedure TfYakuso.laGrid43Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 3, iRow, iCol);
end;

procedure TfYakuso.laGrid44Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 4, iRow, iCol);
end;

procedure TfYakuso.laGrid45Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 5, iRow, iCol);
end;

end.

