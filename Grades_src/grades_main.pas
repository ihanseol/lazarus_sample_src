{************************************}
{* Main unit for Grades application *}
{************************************}

unit grades_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5] of Integer;
  TGridNumbers = array[0..5, 0..5] of Integer;
  TNumberLabels = array[0..5] of TStaticText;
  TGridShapes = array[0..5, 0..5] of TShape;
  TGridLabels = array[0..5, 0..5] of TStaticText;
  {**********}
  { TfGrades }
  {**********}
  TfGrades = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize4, mOptionsSize5, mOptionsSize6: TMenuItem;
    mHelp, mHelpCopyright, mHelpWebsite, MenuItem1, mHelpRules, mHelpAbout: TMenuItem;
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
    laLeft0, laLeft1, laLeft2, laLeft3, laLeft4, laLeft5: TStaticText;
    laTop0, laTop1, laTop2, laTop3, laTop4, laTop5: TStaticText;
    laRight0, laRight1, laRight2, laRight3, laRight4, laRight5: TStaticText;
    laBottom0, laBottom1, laBottom2, laBottom3, laBottom4, laBottom5: TStaticText;
    edHelp, edRules: TMemo;
    btDone, btShow, btClearAll: TButton;
    bt1, bt2, bt3, bt4, bt5: TButton;
    bt6, bt7, bt8, bt9, btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mHelpCopyrightClick(Sender: TObject);
    procedure mHelpWebsiteClick(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mOptionsSize4Click(Sender: TObject);
    procedure mOptionsSize5Click(Sender: TObject);
    procedure mOptionsSize6Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpRulesClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btClearAllClick(Sender: TObject);
    procedure bt1Click(Sender: TObject);
    procedure bt2Click(Sender: TObject);
    procedure bt3Click(Sender: TObject);
    procedure bt4Click(Sender: TObject);
    procedure bt5Click(Sender: TObject);
    procedure bt6Click(Sender: TObject);
    procedure bt7Click(Sender: TObject);
    procedure bt8Click(Sender: TObject);
    procedure bt9Click(Sender: TObject);
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
    aGridNumbers: TGridNumbers;
    aRowCount, aColumnCount, aRowSum, aColumnSum : TNumbers;
    shGridShapes: TGridShapes;
    stGridNumbers: TGridLabels;
    stRowCount, stColumnCount, stRowSum, stColumnSum : TNumberLabels;
  end;

var
  fGrades: TfGrades;

implementation

{$R *.lfm}

{ Field selection at given grid position (user click at this filed) }

procedure SelectField(var GridShapes: TGridShapes; var GridNumbers: TGridLabels; Row, Col: Integer; var RowSave, ColSave: Integer);

begin
  // Number is already selected: unselect it (resetting color)
  if GridShapes[Row, Col].Brush.Color = clYellow then begin
    // Unselect the field
    if GridNumbers[Row, Col].Caption = '' then
      GridShapes[Row, Col].Brush.Color := clWhite                              // empty field
    else
      GridShapes[Row, Col].Brush.Color := clLime;                              // number field
    RowSave := -1; ColSave := -1;                                              // no field actually selected
  end
  // Field is not yet selected: select it (using highlighting color)
  else begin
    // Unselect previously selected field
    if (RowSave <> -1) and (ColSave <> -1) then begin
      if GridNumbers[RowSave, ColSave].Caption = '' then
        GridShapes[RowSave, ColSave].Brush.Color := clWhite                    // empty field
      else
        GridShapes[RowSave, ColSave].Brush.Color := clLime;                    // number field
    end;
    // Select the field
    GridShapes[Row, Col].Brush.Color := clYellow;
    // Remember which field was selected (to place number there, when user pushes a number button)
    RowSave := Row; ColSave := Col;
  end;
end;

{ Write given number into given grid field (user click on corr. number button) }

procedure WriteNumber(var GridShapes: TGridShapes; var GridNumbers: TGridLabels; Number: string; Row, Col: Integer);

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
{ TfGrades }
{**********}

{ Application start: Initialisations }

procedure TfGrades.FormCreate(Sender: TObject);

begin
  // Create arrays with number shapes and statictexts
  shGridShapes[0, 0] := shGrid00; shGridShapes[0, 1] := shGrid01; shGridShapes[0, 2] := shGrid02;
  shGridShapes[0, 3] := shGrid03; shGridShapes[0, 4] := shGrid04; shGridShapes[0, 5] := shGrid05;
  shGridShapes[1, 0] := shGrid10; shGridShapes[1, 1] := shGrid11; shGridShapes[1, 2] := shGrid12;
  shGridShapes[1, 3] := shGrid13; shGridShapes[1, 4] := shGrid14; shGridShapes[1, 5] := shGrid15;
  shGridShapes[2, 0] := shGrid20; shGridShapes[2, 1] := shGrid21; shGridShapes[2, 2] := shGrid22;
  shGridShapes[2, 3] := shGrid23; shGridShapes[2, 4] := shGrid24; shGridShapes[2, 5] := shGrid25;
  shGridShapes[3, 0] := shGrid30; shGridShapes[3, 1] := shGrid31; shGridShapes[3, 2] := shGrid32;
  shGridShapes[3, 3] := shGrid33; shGridShapes[3, 4] := shGrid34; shGridShapes[3, 5] := shGrid35;
  shGridShapes[4, 0] := shGrid40; shGridShapes[4, 1] := shGrid41; shGridShapes[4, 2] := shGrid42;
  shGridShapes[4, 3] := shGrid43; shGridShapes[4, 4] := shGrid44; shGridShapes[4, 5] := shGrid45;
  shGridShapes[5, 0] := shGrid50; shGridShapes[5, 1] := shGrid51; shGridShapes[5, 2] := shGrid52;
  shGridShapes[5, 3] := shGrid53; shGridShapes[5, 4] := shGrid54; shGridShapes[5, 5] := shGrid55;
  stGridNumbers[0, 0] := laGrid00; stGridNumbers[0, 1] := laGrid01; stGridNumbers[0, 2] := laGrid02;
  stGridNumbers[0, 3] := laGrid03; stGridNumbers[0, 4] := laGrid04; stGridNumbers[0, 5] := laGrid05;
  stGridNumbers[1, 0] := laGrid10; stGridNumbers[1, 1] := laGrid11; stGridNumbers[1, 2] := laGrid12;
  stGridNumbers[1, 3] := laGrid13; stGridNumbers[1, 4] := laGrid14; stGridNumbers[1, 5] := laGrid15;
  stGridNumbers[2, 0] := laGrid20; stGridNumbers[2, 1] := laGrid21; stGridNumbers[2, 2] := laGrid22;
  stGridNumbers[2, 3] := laGrid23; stGridNumbers[2, 4] := laGrid24; stGridNumbers[2, 5] := laGrid25;
  stGridNumbers[3, 0] := laGrid30; stGridNumbers[3, 1] := laGrid31; stGridNumbers[3, 2] := laGrid32;
  stGridNumbers[3, 3] := laGrid33; stGridNumbers[3, 4] := laGrid34; stGridNumbers[3, 5] := laGrid35;
  stGridNumbers[4, 0] := laGrid40; stGridNumbers[4, 1] := laGrid41; stGridNumbers[4, 2] := laGrid42;
  stGridNumbers[4, 3] := laGrid43; stGridNumbers[4, 4] := laGrid44; stGridNumbers[4, 5] := laGrid45;
  stGridNumbers[5, 0] := laGrid50; stGridNumbers[5, 1] := laGrid51; stGridNumbers[5, 2] := laGrid52;
  stGridNumbers[5, 3] := laGrid53; stGridNumbers[5, 4] := laGrid54; stGridNumbers[5, 5] := laGrid55;
  // Create array with row/column counts and sum statictexts
  stRowCount[0] := laLeft0; stRowCount[1] := laLeft1; stRowCount[2] := laLeft2;
  stRowCount[3] := laLeft3; stRowCount[4] := laLeft4; stRowCount[5] := laLeft5;
  stColumnCount[0] := laTop0; stColumnCount[1] := laTop1; stColumnCount[2] := laTop2;
  stColumnCount[3] := laTop3; stColumnCount[4] := laTop4; stColumnCount[5] := laTop5;
  stRowSum[0] := laRight0; stRowSum[1] := laRight1; stRowSum[2] := laRight2;
  stRowSum[3] := laRight3; stRowSum[4] := laRight4; stRowSum[5] := laRight5;
  stColumnSum[0] := laBottom0; stColumnSum[1] := laBottom1; stColumnSum[2] := laBottom2;
  stColumnSum[3] := laBottom3; stColumnSum[4] := laBottom4; stColumnSum[5] := laBottom5;
  // Start random number generator
  Randomize;
  // Application start-up parameters
  iSizeTemp := 5; iRow := -1; iCol := -1;
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfGrades.mPuzzleNewClick(Sender: TObject);

var
  N, Row, Column, Max, I, J: Integer;
  OK: Boolean;

begin
  // Set user option selections active now
  iSize := iSizeTemp;
  // Clear the grid; hide actually unused fields
  for I := 0 to 5 do begin
    for J := 0 to 5 do begin
      aGridNumbers[I, J] := 0;
      stGridNumbers[I, J].Caption := ''; shGridShapes[I, J].Brush.Color := clWhite;
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
  // Set row/col counts and sums to 0; hide the corr. statictexts, if not part of actual puzzle
  for I := 0 to 5 do begin
    aRowCount[I] := 0; aColumnCount[I] := 0;
    aRowSum[I] := 0; aColumnSum[I] := 0;
    if I > iSize - 1 then begin
      // Actually not used rows and columns
      stRowCount[I].Visible := False; stColumnCount[I].Visible := False;
      stRowSum[I].Visible := False; stColumnSum[I].Visible := False;
    end
    else begin
      // Actually used rows and columns
      stRowCount[I].Visible := True; stColumnCount[I].Visible := True;
      stRowSum[I].Visible := TRue; stColumnSum[I].Visible := True;
    end;
  end;
  // Adapt position of sum labels (nicer display...)
  for I := 0 to iSize - 1 do begin
    stRowSum[I].Left := 426 - (60 * (6 - iSize));                              // 426 = statictext's Left for 6x6 puzzle
    stColumnSum[I].Top := 656 -(60 * (6 - iSize));                             // 656 = statictext's Top for 6x6 puzzle
  end;
  // Generate random numbers at random grid position
  // The actual version doesn't care if number fields touch each other
  // (as tell the rules in the orginal game)!
  N := Random((iSize - 3) * 2) + iSize + 1;                                    // actual number of number fields (seems ok with this code)
  repeat                                                                       // continue generating numbers, until all is as wanted
    // Clear the actual grid
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        aGridNumbers[I, J] := 0; stGridNumbers[I, J].Caption := '';
      end;
    end;
    // Clear the counts and sums
    for I := 0 to iSize - 1 do begin
      aRowCount[I] := 0; aColumnCount[I] := 0;
      aRowSum[I] := 0; aColumnSum[I] := 0;
    end;
    // Generate the numbers
    OK := True;
    for I := 1 to N do begin
      repeat
        Row := Random(iSize); Column := Random(iSize);
      until aGridNumbers[Row, Column] = 0;                                     // field has to be empty!
      aGridNumbers[Row, Column] := Random(9) + 1;
    end;
    // Calculate row counts and sums
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        if aGridNumbers[I, J] > 0 then begin
          aRowCount[I] += 1; aRowSum[I] += aGridNumbers[I, J];
        end;
      end;
    end;
    // Calculate column counts and sums
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        if aGridNumbers[I, J] > 0 then begin
          aColumnCount[J] += 1; aColumnSum[J] += aGridNumbers[I, J];
        end;
      end;
    end;
    // Force maximum of number fields per row/col to 2 (4x4 puzzle) resp. 3 (other puzzles)
    if iSize = 4 then
      Max := 2
    else
      Max := 3;
    for I := 0 to iSize - 1 do begin
      if (aRowCount[I] > Max) or (aColumnCount[I] > Max) then
        OK := False;
    end;
  until OK;                                                                    // regenerate the whole if numbers are not as "should be"
  // Fill in row/column counts and sums
  for I := 0 to iSize - 1 do begin
    stRowCount[I].Caption := IntToStr(aRowCount[I]);
    stColumnCount[I].Caption := IntToStr(aColumnCount[I]);
    stRowSum[I].Caption := IntToStr(aRowSum[I]);
    stColumnSum[I].Caption := IntToStr(aColumnSum[I]);
  end;
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfGrades.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Puzzle size > ...": Select size of the puzzle (4x4, 5x5, 6x6) }

procedure TfGrades.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize5.Checked := False; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfGrades.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 5;
end;

procedure TfGrades.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu item "Help > Source and copyright": Display source and copyright info }

procedure TfGrades.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Grades" at https://www.janko.at/ (puzzle name: "Grades"). It may have been invented by Otto Janko ';
  S += 'and by this may underly the Creative Commons 3.0 licence: quotation of the author''s name; no commercial usage; distribution only ';
  S += 'under the same conditions.';
  MessageDlg('Grades licence', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel und Puzzles' website": Open browser at 'Rätsel und Puzzles' website }

procedure TfGrades.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > Original game": Display application/original game rule difference }

procedure TfGrades.mHelpRulesClick(Sender: TObject);

var
  S: string;

begin
  S := 'The game rules used within the "Grades" application are somewhat simpler than those of the original game. ';
  S += 'In fact, "Grades" does not consider the condition, that the number fields must not touch each other.';
  MessageDlg('Grades rules', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfGrades.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Grades.' + LineEnding;
  S += 'Find the numbers (and the fields, where to place them) in order to verify that the row and column ';
  S += 'counts and sums equal the corresponding values, displayed at the borders of the grid.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, July 2020.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "Grades"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid values and check if this is a correct solution }

procedure TfGrades.btDoneClick(Sender: TObject);

// Solution validity check is done by calculating the row/column counts and sums and comparing these to the displayed values,
// not by comparison of the user and generated grid numbers; in fact, there might be several correct solutions...

var
  I, J: Integer;
  Solved: Boolean;
  UserNumbers: TGridNumbers;
  UserRowCount, UserColCount, UserRowSum, UserColSum: TNumbers;

begin
  // Disable field number entry (by setting actual field to none)
  iRow := -1; iCol := -1;
  // Get user values from form grid and insert them into an array, similar to the one created, when generating the puzzle
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if stGridNumbers[I, J].Caption = '' then
        // Empty fields
        UserNumbers[I, J] := 0
      else
        // Number fields
        UserNumbers[I, J] := StrToInt(stGridNumbers[I, J].Caption);
    end;
  end;
  // Calculate row and column counts and sums of the "user grid"
  for I := 0 to iSize - 1 do begin
    UserRowCount[I] := 0; UserRowSum[I] := 0;
    UserColCount[I] := 0; UserColSum[I] := 0;
  end;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if UserNumbers[I, J] > 0 then begin
        UserRowCount[I] += 1; UserRowSum[I] += UserNumbers[I, J];
      end;
    end;
  end;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if UserNumbers[I, J] > 0 then begin
        UserColCount[J] += 1; UserColSum[J] += UserNumbers[I, J];
      end;
    end;
  end;
  // Compare the "user counts and sums" with the values, that they have to be
  // If one of these value pairs differs, the puzzle has not been solved
  Solved := True;
  for I := 0 to iSize - 1 do begin
    if (UserRowCount[I] <> aRowCount[I]) or (UserColCount[I] <> aColumnCount[I]) or
       (UserRowSum[I] <> aRowSum[I]) or (UserColSum[I] <> aColumnSum[I]) then
      Solved := False;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Grades', 'You have successfully solved this Grades puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Grades', 'Sorry. Your numbers are no valid solution of this Grades puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
  btDone.Enabled := False; btClearAll.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfGrades.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize -1 do begin
    for J := 0 to iSize - 1 do begin
      if aGridNumbers[I, J] = 0 then begin
        // Empty field
        shGridShapes[I, J].Brush.Color := clWhite;
        stGridNumbers[I, J].Caption := '';
      end
      else begin
        // Number field
        shGridShapes[I, J].Brush.Color := clLime;
        stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J]);
      end;
    end;
  end;
end;

{ Number buttons: Write corr. number into actually selected grid field }

procedure TfGrades.bt1Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '1', iRow, iCol);
end;

procedure TfGrades.bt2Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '2', iRow, iCol);
end;

procedure TfGrades.bt3Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '3', iRow, iCol);
end;

procedure TfGrades.bt4Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '4', iRow, iCol);
end;

procedure TfGrades.bt5Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '5', iRow, iCol);
end;

procedure TfGrades.bt6Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '6', iRow, iCol);
end;

procedure TfGrades.bt7Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '7', iRow, iCol);
end;

procedure TfGrades.bt8Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '8', iRow, iCol);
end;

procedure TfGrades.bt9Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '9', iRow, iCol);
end;

{ Buttons "Clear": Clear number in actually selected grid field }

procedure TfGrades.btClearClick(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, '', iRow, iCol);
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfGrades.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  iRow := -1; iCol := -1;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := clWhite;
      stGridNumbers[I, J].Caption := '';
    end;
  end;
end;

{ User click on any of the 6x6 statictexts fields: Highlight/un-highlight corresponding shape }

procedure TfGrades.laGrid00Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 0, iRow, iCol);
end;

procedure TfGrades.laGrid01Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 1, iRow, iCol);
end;

procedure TfGrades.laGrid02Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 2, iRow, iCol);
end;

procedure TfGrades.laGrid03Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 3, iRow, iCol);
end;

procedure TfGrades.laGrid04Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 4, iRow, iCol);
end;

procedure TfGrades.laGrid05Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 0, 5, iRow, iCol);
end;

procedure TfGrades.laGrid10Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 0, iRow, iCol);
end;

procedure TfGrades.laGrid11Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 1, iRow, iCol);
end;

procedure TfGrades.laGrid12Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 2, iRow, iCol);
end;

procedure TfGrades.laGrid13Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 3, iRow, iCol);
end;

procedure TfGrades.laGrid14Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 4, iRow, iCol);
end;

procedure TfGrades.laGrid15Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 1, 5, iRow, iCol);
end;

procedure TfGrades.laGrid20Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 0, iRow, iCol);
end;

procedure TfGrades.laGrid21Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 1, iRow, iCol);
end;

procedure TfGrades.laGrid22Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 2, iRow, iCol);
end;

procedure TfGrades.laGrid23Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 3, iRow, iCol);
end;

procedure TfGrades.laGrid24Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 4, iRow, iCol);
end;

procedure TfGrades.laGrid25Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 2, 5, iRow, iCol);
end;

procedure TfGrades.laGrid30Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 0, iRow, iCol);
end;

procedure TfGrades.laGrid31Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 1, iRow, iCol);
end;

procedure TfGrades.laGrid32Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 2, iRow, iCol);
end;

procedure TfGrades.laGrid33Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 3, iRow, iCol);
end;

procedure TfGrades.laGrid34Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 4, iRow, iCol);
end;

procedure TfGrades.laGrid35Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 3, 5, iRow, iCol);
end;

procedure TfGrades.laGrid40Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 0, iRow, iCol);
end;

procedure TfGrades.laGrid41Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 1, iRow, iCol);
end;

procedure TfGrades.laGrid42Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 2, iRow, iCol);
end;

procedure TfGrades.laGrid43Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 3, iRow, iCol);
end;

procedure TfGrades.laGrid44Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 4, iRow, iCol);
end;

procedure TfGrades.laGrid45Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 4, 5, iRow, iCol);
end;

procedure TfGrades.laGrid50Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 5, 0, iRow, iCol);
end;

procedure TfGrades.laGrid51Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 5, 1, iRow, iCol);
end;

procedure TfGrades.laGrid52Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 5, 2, iRow, iCol);
end;

procedure TfGrades.laGrid53Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 5, 3, iRow, iCol);
end;

procedure TfGrades.laGrid54Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 5, 4, iRow, iCol);
end;

procedure TfGrades.laGrid55Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, 5, 5, iRow, iCol);
end;

end.

