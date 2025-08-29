{*************************************}
{* Main unit for Mathrax application *}
{*************************************}

unit mathrax_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TNumberShapes = array[0..5, 0..5] of TShape;
  TNumberLabels = array[0..5, 0..5] of TStaticText;
  TOperatorShapes = array[0..4, 0..4] of TShape;
  TOperatorLabels = array[0..4, 0..4] of TStaticText;
  TOperator = record
    Oprator: string;
    Result: Integer;
  end;
  TOperators = array[0..4, 0..4] of TOperator;
  {***********}
  { TfMathrax }
  {***********}
  TfMathrax = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize4, mOptionsSize5, mOptionsSize6: TMenuItem;
    mOptionsLevel, mOptionsLevel1, mOptionsLevel2: TMenuItem;
    mHelp, mHelpCopyright, mHelpWebsite, MenuItem1, mHelpAbout: TMenuItem;
    StaticText1, stOperator: TStaticText;
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
    bt1, bt2, bt3, bt4, bt5, bt6, btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mOptionsSize4Click(Sender: TObject);
    procedure mOptionsSize5Click(Sender: TObject);
    procedure mOptionsSize6Click(Sender: TObject);
    procedure mOptionsLevel1Click(Sender: TObject);
    procedure mOptionsLevel2Click(Sender: TObject);
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
    iSizeTemp, iSize, iLevelTemp, ilevel, iRow, iCol: Integer;
    aGridNumbers: TNumbers;
    aOperators: TOperators;
    shGridShapes: TNumberShapes;
    stGridNumbers: TNumberLabels;
    shOperatorShapes: TOperatorShapes;
    stOperatorNumbers: TOperatorLabels;
  end;

var
  fMathrax: TfMathrax;

implementation

{$R *.lfm}

{ Field selection at given grid position (user click at this filed) }

procedure SelectField(var GridShapes: TNumberShapes; Row, Col: Integer; var RowSave, ColSave: Integer);

begin
  if fMathrax.btDone.Enabled then begin
    // Proceed only during the game
    if GridShapes[Row, Col].Brush.Color = clRed then begin
      // Number is already selected: Unselect it (using white as shape color)
      GridShapes[Row, Col].Brush.Color := clWhite;
      RowSave := -1; ColSave := -1;                                            // no field actually selected
    end
    else begin
      // Field is not yet selected: Select it (using highlighting = red shape color)
      if (RowSave <> -1) and (ColSave <> -1) then begin
        // Unselect previously selected field
        GridShapes[RowSave, ColSave].Brush.Color := clWhite;
      end;
      GridShapes[Row, Col].Brush.Color := clRed;
      RowSave := Row; ColSave := Col;                                          // remember selected field (to place number there, when user pushes number button)
    end;
  end;
end;

{ Write given number into given grid field (user click on corr. number button) }

procedure WriteNumber(var GridNumbers: TNumberLabels; Number: string; Row, Col: Integer);

begin
  if fMathrax.btDone.Enabled then begin
    // Proceed only during the game
    if (Row <> -1) and (Col <> -1) then begin
      // Do only if there is a field selected
      GridNumbers[Row, Col].Caption := Number;                                 // write the number (or empty string if "Clear" button)
    end;
  end;
end;

{***********}
{ TfMathrax }
{***********}

{ Application start: Initialisation }

procedure TfMathrax.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  // Create arrays with number shapes and static texts
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
  // Create operator shapes and static texts
  for I := 0 to 4 do begin
    for J := 0 to 4 do begin
      shOperatorShapes[I, J] := TShape.Create(shOperatorShapes[I, J]);
      shOperatorShapes[I, J].Parent := Self;
      shOperatorShapes[I, J].Shape := stCircle;
      shOperatorShapes[I, J].Width := 40;
      shOperatorShapes[I, J].Height := 40;
      shOperatorShapes[I, J].Brush.Color := clYellow;
      shOperatorShapes[I, J].Left := shGridShapes[I, J].Left + 40;
      shOperatorShapes[I, J].Top := shGridShapes[I, J].Top + 40;
      shOperatorShapes[I, J].Visible := False;
      stOperatorNumbers[I, J] := TStaticText.Create(stOperatorNumbers[I, J]);
      stOperatorNumbers[I, J].Parent := Self;
      stOperatorNumbers[I, J].Width := 36;
      stOperatorNumbers[I, J].Alignment := taCenter;
      stOperatorNumbers[I, J].Font.Size := stOperator.Font.Size;
      stOperatorNumbers[I, J].Left := shGridShapes[I, J].Left + 42;
      stOperatorNumbers[I, J].Top := shGridShapes[I, J].Top + 48;
      stOperatorNumbers[I, J].Visible := False;
    end;
  end;
  // Start random number generator
  Randomize;
  // Application start-up parameters
  iSizeTemp := 6; iLevelTemp := 1;
  iRow := -1; iCol := -1;
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfMathrax.mPuzzleNewClick(Sender: TObject);

const
  LineSums: array[0..2] of Integer = (
    10, 15, 21
  );

var
  Col, N, N0, I, J, K: Integer;
  OK: Boolean;

begin
  // Set user selected puzzle size and game level active now
  iSize := iSizeTemp; iLevel := iLevelTemp;
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
  // Hide all operator shapes and static texts
  for I := 0 to 4 do begin
    for J := 0 to 4 do begin
      shOperatorShapes[I, J].Visible := False;
      stOperatorNumbers[I, J].Visible := False;
    end;
  end;
  // Generate random numbers at random grid position, according to the game rules
  repeat
    OK := True;
    repeat
      OK := True;
      // Clear the numbers array
      for I := 0 to iSize - 1 do begin
        for J := 0 to iSize - 1 do
          aGridNumbers[I, J] := 0;
      end;
      // Clear operators array
      for I := 0 to 4 do begin
        for J := 0 to 4 do begin
          aOperators[I, J].Oprator := '';
        end;
      end;
      // Generate the numbers
      for I := 0 to iSize - 2 do begin
        if OK then begin
          // Do row after row (except the last one: filled-in below)
          for K := 1 to iSize do begin
            // Place N numbers in each row
            repeat
              // For each of these numbers, choose random column in actual row
              Col := Random(iSize);
            until aGridNumbers[I, Col] = 0;                                    // field must be empty, of course
            // Check if this column doesn't already contain this number
            for J := 0 to iSize - 1 do begin
              if aGridNumbers[J, Col] = K then                                 // if column already contains this number...
                OK := False;                                                   // ...start over
            end;
            if OK then
              aGridNumbers[I, Col] := K;                                       // store number into this row of numbers array
          end;
        end;
      end;
    until OK;
    // Fill-in last row, using the number not yet used in this column; this more than significantly speeds
    // up the procedure (in comparison with filling the line randomly, as done above for the other rows)
    for J := 0 to iSize - 1 do begin
      K := 0;
      for I := 0 to iSize - 2 do
        K += aGridNumbers[I, J];
      aGridNumbers[iSize - 1, J] := LineSums[iSize - 4] - K;
    end;
    // Generate the operators
    // Check for each field of the grid, if the 4 neighbor numbers are such that one of the possible operations applies
    // If so, store operator and operation result in corr. element of the operators array (a part of them will be eliminated later)
    N := 0;
    for I := 0 to iSize - 2 do begin
      for J := 0 to iSize - 2 do begin
        if aGridNumbers[I, J] / aGridNumbers[I + 1, J + 1] = aGridNumbers[I + 1, J] / aGridNumbers[I, J + 1] then begin
          if aGridNumbers[I, J] mod aGridNumbers[I + 1, J + 1] = 0 then begin
            aOperators[I, J].Oprator := '÷';
            aOperators[I, J].Result := aGridNumbers[I, J] div aGridNumbers[I + 1, J + 1];
          end
        end
        else if aGridNumbers[I, J] * aGridNumbers[I + 1, J + 1] = aGridNumbers[I + 1, J] * aGridNumbers[I, J + 1] then begin
          aOperators[I, J].Oprator := '*';
          aOperators[I, J].Result := aGridNumbers[I, J] * aGridNumbers[I + 1, J + 1];
        end
        else if Abs(aGridNumbers[I, J] - aGridNumbers[I + 1, J + 1]) = Abs(aGridNumbers[I + 1, J] - aGridNumbers[I, J + 1]) then begin
          aOperators[I, J].Oprator := '-';
          aOperators[I, J].Result := Abs(aGridNumbers[I, J] - aGridNumbers[I + 1, J + 1]);
        end
        else if aGridNumbers[I, J] + aGridNumbers[I + 1, J + 1] = aGridNumbers[I + 1, J] + aGridNumbers[I, J + 1] then begin
          aOperators[I, J].Oprator := '+';
          aOperators[I, J].Result := aGridNumbers[I, J] + aGridNumbers[I + 1, J + 1];
        end
        else if (aGridNumbers[I, J] mod 2 = 0) and (aGridNumbers[I + 1, J + 1] mod 2 = 0)
        and (aGridNumbers[I + 1, J] mod 2 = 0) and  (aGridNumbers[I, J + 1] mod 2 = 0) then begin
          aOperators[I, J].Oprator := 'e';
        end
        else if (aGridNumbers[I, J] mod 2 = 1) and (aGridNumbers[I + 1, J + 1] mod 2 = 1)
        and (aGridNumbers[I + 1, J] mod 2 = 1) and  (aGridNumbers[I, J + 1] mod 2 = 1) then begin
          aOperators[I, J].Oprator := 'o';
        end;
        if aOperators[I, J].Oprator <> '' then
          Inc(N);
      end;
    end;
    if ((iLevel = 1) and (N < 3)) or ((iLevel = 2) and (N < iSize)) then begin
      // If number of applicable operators is less than an arbitrarily chosen minimum, redo number grid generation from the beginning
      OK := False;
    end;
  until OK;
  // Randomly selected number of operators (depends on level and puzzle size)
  if iLevel = 1 then begin
    if iSize = 6 then
      N0 := Random(3) + 3
    else
      N0 := Random(iSize - 2) + iSize - 2;
  end
  else
    N0 := Random(iSize - 1) + iSize;
  // If there are actually more operators than the value selected, randomly remove some of them
  if N > N0 then begin
    for K := 1 to N - N0 do begin
      repeat
        I := Random(iSize - 1); J := Random(iSize - 1);
      until aOperators[I, J].Oprator <> '';
      aOperators[I, J].Oprator := '';
    end;
  end;
  // Display the grid (all fields empty)
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := clWhite;
      stGridNumbers[I, J].Caption := '';
    end;
  end;
  // Display the operators
  for I := 0 to iSize - 2 do begin
    for J := 0 to iSize - 2 do begin
      if aOperators[I, J].Oprator <> '' then begin
        shOperatorShapes[I, J].Visible := True; stOperatorNumbers[I, J].Visible := True;
        if (aOperators[I, J].Oprator = 'e') or (aOperators[I, J].Oprator = 'o') then
          stOperatorNumbers[I, J].Caption := aOperators[I, J].Oprator
        else
          stOperatorNumbers[I, J].Caption := IntToStr(aOperators[I, J].Result) + '/' + aOperators[I, J].Oprator;
      end;
    end;
  end;
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfMathrax.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Puzzle size > ...": Select size of the puzzle (4x4, 5x5, 6x6) }

procedure TfMathrax.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize5.Checked := False; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfMathrax.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 5;
end;

procedure TfMathrax.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu items "Options > Game level > ...": Select game level (1 or 2) }

procedure TfMathrax.mOptionsLevel1Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := True; mOptionsLevel2.Checked := False;
  iLevelTemp := 1;                                                             // option will become active when "New puzzle" is selected
end;

procedure TfMathrax.mOptionsLevel2Click(Sender: TObject);

begin
  mOptionsLevel1.Checked := False; mOptionsLevel2.Checked := True;
  iLevelTemp := 2;
end;

{ Menu item "Help > Source and copyright": Display source and copyright of Mathrax puzzle }

procedure TfMathrax.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found Mathrax at https://www.janko.at/. It may have been invented by Otto Janko and by this may underly the Creative Commons ';
  S += '3.0 licence: quotation of the author''s name; no commercial usage; distribution only under the same conditions.';
  S += 'Concerning my "Mathrax" PC application itself, it is freeware/open source, as all puzzle and other applications at www.streetinfo.lu.';
  MessageDlg('Mathrax', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel und Puzzles' website": Point web browser to janko.at }

procedure TfMathrax.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > About": Display application about }

procedure TfMathrax.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Mathrax.' + LineEnding;
  S += 'Fill the grid fields with numbers from 1 to N, in a way, that all diagonal operations are fulfilled.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2022.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "Mathrax"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid and check if this is a correct solution }

procedure TfMathrax.btDoneClick(Sender: TObject);

// Solution validity check is done by checking the user values and their uniqueness per row and column, and by checking if all diagonal
// operations are fulfilled, and not by comparison of the user and generated grid numbers; in fact, there may be several correct solutions...

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
  // Check diagonal operations
  if Solved then begin
    for I := 0 to iSize - 2 do begin
      for J := 0 to iSize - 2 do begin
        if aOperators[I, J].Oprator <> '' then begin
          case aOperators[I, J].Oprator of
            '+': if UserNumbers[I, J] + UserNumbers[I + 1, J + 1] <> UserNumbers[I + 1, J] + UserNumbers[I, J + 1] then
                   Solved := False;
            '-': if Abs(UserNumbers[I, J] - UserNumbers[I + 1, J + 1]) <> Abs(UserNumbers[I + 1, J] - UserNumbers[I, J + 1]) then
                   Solved := False;
            '*': if UserNumbers[I, J] * UserNumbers[I + 1, J + 1] <> UserNumbers[I + 1, J] * UserNumbers[I, J + 1] then
                   Solved := False;
            '÷': if UserNumbers[I, J] / UserNumbers[I + 1, J + 1] <> UserNumbers[I + 1, J] / UserNumbers[I, J + 1] then
                   Solved := False;
            'e': if (aGridNumbers[I, J] mod 2 = 1) or (aGridNumbers[I + 1, J + 1] mod 2 = 1)
                 or (aGridNumbers[I + 1, J] mod 2 = 1) or (aGridNumbers[I, J + 1] mod 2 = 1) then
                   Solved := False;
            'o': if (aGridNumbers[I, J] mod 2 = 0) or (aGridNumbers[I + 1, J + 1] mod 2 = 0)
                 or (aGridNumbers[I + 1, J] mod 2 = 0) or (aGridNumbers[I, J + 1] mod 2 = 0) then
                   Solved := False;
          end;
        end;
      end;
    end;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Mathrax', 'You have successfully solved this Mathrax puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Mathrax', 'Sorry. Your numbers are no valid solution of this Mathrax puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
  // This will also disable any action, when a grid field or number button is clicked
  btDone.Enabled := False; btClearAll.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfMathrax.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := clWhite;
      stGridNumbers[I, J].Caption := IntToStr(Abs(aGridNumbers[I, J]));
    end;
  end;
  btShow.Enabled := False;
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfMathrax.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := clWhite;
      stGridNumbers[I, J].Caption := '';
    end;
  end;
  iRow := -1; iCol := -1;                                                      // no field actually selected
end;

{ Number buttons: Write corr. number into actually selected grid field }

procedure TfMathrax.bt1Click(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '1', iRow, iCol);
end;

procedure TfMathrax.bt2Click(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '2', iRow, iCol);
end;

procedure TfMathrax.bt3Click(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '3', iRow, iCol);
end;

procedure TfMathrax.bt4Click(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '4', iRow, iCol);
end;

procedure TfMathrax.bt5Click(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '5', iRow, iCol);
end;

procedure TfMathrax.bt6Click(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '6', iRow, iCol);
end;

{ Button "Clear": Clear number in actually selected grid field }

procedure TfMathrax.btClearClick(Sender: TObject);

begin
  WriteNumber(stGridNumbers, '', iRow, iCol);
end;

{ User click on any of the 6x6 static text fields: Highlight/unhighlight corresponding shape }

procedure TfMathrax.laGrid00Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 0, iRow, iCol);
end;

procedure TfMathrax.laGrid01Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 1, iRow, iCol);
end;

procedure TfMathrax.laGrid02Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 2, iRow, iCol);
end;

procedure TfMathrax.laGrid03Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 3, iRow, iCol);
end;

procedure TfMathrax.laGrid04Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 4, iRow, iCol);
end;

procedure TfMathrax.laGrid05Click(Sender: TObject);

begin
  SelectField(shGridShapes, 0, 5, iRow, iCol);
end;

procedure TfMathrax.laGrid10Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 0, iRow, iCol);
end;

procedure TfMathrax.laGrid11Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 1, iRow, iCol);
end;

procedure TfMathrax.laGrid12Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 2, iRow, iCol);
end;

procedure TfMathrax.laGrid13Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 3, iRow, iCol);
end;

procedure TfMathrax.laGrid14Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 4, iRow, iCol);
end;

procedure TfMathrax.laGrid15Click(Sender: TObject);

begin
  SelectField(shGridShapes, 1, 5, iRow, iCol);
end;

procedure TfMathrax.laGrid20Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 0, iRow, iCol);
end;

procedure TfMathrax.laGrid21Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 1, iRow, iCol);
end;

procedure TfMathrax.laGrid22Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 2, iRow, iCol);
end;

procedure TfMathrax.laGrid23Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 3, iRow, iCol);
end;

procedure TfMathrax.laGrid24Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 4, iRow, iCol);
end;

procedure TfMathrax.laGrid25Click(Sender: TObject);

begin
  SelectField(shGridShapes, 2, 5, iRow, iCol);
end;

procedure TfMathrax.laGrid30Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 0, iRow, iCol);
end;

procedure TfMathrax.laGrid31Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 1, iRow, iCol);
end;

procedure TfMathrax.laGrid32Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 2, iRow, iCol);
end;

procedure TfMathrax.laGrid33Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 3, iRow, iCol);
end;

procedure TfMathrax.laGrid34Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 4, iRow, iCol);
end;

procedure TfMathrax.laGrid35Click(Sender: TObject);

begin
  SelectField(shGridShapes, 3, 5, iRow, iCol);
end;

procedure TfMathrax.laGrid40Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 0, iRow, iCol);
end;

procedure TfMathrax.laGrid41Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 1, iRow, iCol);
end;

procedure TfMathrax.laGrid42Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 2, iRow, iCol);
end;

procedure TfMathrax.laGrid43Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 3, iRow, iCol);
end;

procedure TfMathrax.laGrid44Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 4, iRow, iCol);
end;

procedure TfMathrax.laGrid45Click(Sender: TObject);

begin
  SelectField(shGridShapes, 4, 5, iRow, iCol);
end;

procedure TfMathrax.laGrid50Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 0, iRow, iCol);
end;

procedure TfMathrax.laGrid51Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 1, iRow, iCol);
end;

procedure TfMathrax.laGrid52Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 2, iRow, iCol);
end;

procedure TfMathrax.laGrid53Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 3, iRow, iCol);
end;

procedure TfMathrax.laGrid54Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 4, iRow, iCol);
end;

procedure TfMathrax.laGrid55Click(Sender: TObject);

begin
  SelectField(shGridShapes, 5, 5, iRow, iCol);
end;

end.

