{*************************************}
{* Main unit for Factors application *}
{*************************************}

unit factors_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TNumberShapes = array[0..5, 0..5] of TShape;
  TNumberLabels = array[0..5, 0..5] of TStaticText;
  TProductLabels = array[0..5, 0..5] of TLabel;
  TColors = array[0..5, 0..5] of TColor;
  {***********}
  { TfFactors }
  {***********}
  TfFactors = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleExit: TMenuItem;
    mOptions, mOptionsSize, mOptionsSize4, mOptionsSize5, mOptionsSize6, mOptionsUniqueDiags: TMenuItem;
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
    laProduct: TLabel;                                                         // hidden label, used to set font of labels created during runtime
    procedure FormCreate(Sender: TObject);
    procedure mOptionsUniqueDiagsClick(Sender: TObject);
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
    aProducts: array of Integer;
    aGridNumbers, aArea: TNumbers;
    aGridColors: TColors;
    shGridShapes: TNumberShapes;
    stGridNumbers: TNumberLabels;
    laProducts: TProductLabels;
  end;

var
  fFactors: TfFactors;

implementation

{$R *.lfm}

{ Field selection at given grid position (user click at this filed) }

procedure SelectField(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels; var GridColors: TColors; var ProdLabels: TProductLabels;
  Row, Col: Integer; var RowSave, ColSave: Integer);

begin
  if fFactors.btDone.Enabled then begin
    if GridShapes[Row, Col].Brush.Color = clBlack then begin
      // Number is already selected: unselect it (resetting color)
      GridShapes[Row, Col].Brush.Color := GridColors[Row, Col];
      GridNumbers[Row, Col].Font.Color := clBlack; ProdLabels[Row, Col].Font.Color := clBlack;
      RowSave := -1; ColSave := -1;                                            // no field actually selected
    end
    else begin
      // Field is not yet selected: select it (using highlighting color)
      if (RowSave <> -1) and (ColSave <> -1) then begin
        // Unselect previously selected field
        GridShapes[RowSave, ColSave].Brush.Color := GridColors[RowSave, ColSave];
        GridNumbers[RowSave, ColSave].Font.Color := clBlack; ProdLabels[RowSave, ColSave].Font.Color := clBlack;
      end;
      GridShapes[Row, Col].Brush.Color := clBlack;
      GridNumbers[Row, Col].Font.Color := clWhite; ProdLabels[Row, Col].Font.Color := clWhite;
      RowSave := Row; ColSave := Col;                                          // remember selected field (to place number there, when user pushes number button)
    end;
  end;
end;

{ Write given number into given grid field (user click on corr. number button) }

procedure WriteNumber(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels;
  var GridColors: TColors; var ProdLabels: TProductLabels; Number: string; Row, Col: Integer);

begin
  if fFactors.btDone.Enabled then begin
    if (Row <> -1) and (Col <> -1) then begin
      // Do only if there is a field selcted
      GridNumbers[Row, Col].Caption := Number;                                 // write the number (or empty string if "Clear" button)
      GridShapes[Row, Col].Brush.Color := GridColors[Row, Col];
      GridNumbers[Row, Col].Font.Color := clBlack; ProdLabels[Row, Col].Font.Color := clBlack;
    end;
  end;
end;

{***********}
{ TfFactors }
{***********}

{ Application start: Initialisations }

procedure TfFactors.FormCreate(Sender: TObject);

var
  I, J: Integer;

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
  // Generate product labels
  for I := 0 to 5 do begin
    for J := 0 to 5 do begin
      laProducts[I, J] := TLabel.Create(laProducts[I, J]);
      laProducts[I, J].Parent := Self;
      laProducts[I, J].Font.Size := laProduct.Font.Size;
      laProducts[I, J].Left := shGridShapes[I, J].Left + 2;
      laProducts[I, J].Top := shGridShapes[I, J].Top + 2;
      laProducts[I, J].Visible := False;
    end;
  end;
  // Start random number generator
  Randomize;
  // Application start-up parameters
  iSizeTemp := 5;
  iRow := -1; iCol := -1;
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfFactors.mPuzzleNewClick(Sender: TObject);

const
  Colors: array[0..14] of TColor = (
    $D7EBFA, $F0FFF0, $98FB98, $2FFFAD, $8CE6F0, $00D7FF, $00FFFF, $507FFF,
    $CBC0FF, $D8BFD8, $FAE6E6, $EEEEAF, $E6E0B0, $FACE87, $DCDCDC
  );
  LineSums: array[0..2] of Integer = (
    10, 15, 21
  );

var
  Area, AreaMaxFields, Col, R, I, J, K, I0, J0: Integer;
  AreasOK, First, OK: Boolean;
  Counts: array of Integer;
  Done: array of Boolean;

begin
  // Set user option selections active now
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
      // Hide all area products
      laProducts[I, J].Visible := False;
    end;
  end;
  // Maximum number of fields per area (may be higher in reality, because areas will be "corrected" later)
  if iSize = 4 then
    AreaMaxFields := 2
  else
    AreaMaxFields := 3;
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
  // Create areas
  repeat
    AreasOK := True;
    // Clear areas array
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        aArea[I, J] := -1;
      end;
    end;
    // Random area creation
    Area := 0; SetLength(Counts, Area + 1); Counts[Area] := 0;
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        if aArea[I, J] = -1 then begin
          // If actual field doesn't belong to any area yet, make it part of current area
          aArea[I, J] := Area; Inc(Counts[Area]);
          // Add one (or two) further field(s) to current area
          R := Random(3);
          if R = 0 then begin
            // Add same row, next column field to area; except if last column,
            // add next row, same column field to area
            if (J <= iSize - 2) and (aArea[I, J + 1] = -1) then begin
              aArea[I, J + 1] := Area; Inc(Counts[Area]);
            end
            else if (I <= iSize - 2) and (aArea[I + 1, J] = -1) then begin
              aArea[I + 1, J] := Area; Inc(Counts[Area]);
            end;
          end
          else begin
            // Add next row, same column field to area; except if last row,
            // add same row, next column field to area
            if (I <= iSize - 2) and (aArea[I + 1, J] = -1) then begin
              aArea[I + 1, J] := Area; Inc(Counts[Area]);
              if R = 1 then begin
                // Add a further field of the same column to area
                // This increases number of vertical areas -> grid more "balanced" and interesting
                if (I <= iSize - 3) and (aArea[I + 2, J] = -1) then begin
                  aArea[I + 2, J] := Area; Inc(Counts[Area]);
                end;
              end;
            end
            else if (J <= iSize - 2) and (aArea[I, J + 1] = -1) then begin
              aArea[I, J + 1] := Area; Inc(Counts[Area]);
            end;
          end;
          // Continue adding fields to actual area, except
          //   - if actual column is last one (areas can't exceed end of line)
          //   - if the max of fields per area has been reached
          //   - randomly (1/3 of times): this avoids to much to large areas
          if (J = iSize - 1) or (Random(3) = 0) or (Counts[aArea[I, J]] >= AreaMaxFields) then begin
            Inc(Area); SetLength(Counts, Area + 1); Counts[Area] := 0;
          end;
        end;
      end;
    end;
    if Length(Counts) > 15 then
      // Total number of areas limited to 15
      AreasOK := False
    else begin
      // Correction to be done: elimination of areas with one single field (field will be added to neighbour area)
      repeat
        OK := True;
        for I := 0 to iSize - 1 do begin
          for J := 0 to iSize - 1 do begin
            if Counts[aArea[I, J]] < 2 then begin
              OK := False;
              R := Random(3);
              if (R = 0) and (J >= 1) then
                aArea[I, J] := aArea[I, J - 1]
              else if (R = 1) and (J <= iSize - 2) then
                aArea[I, J] := aArea[I, J + 1]
              else
                aArea[I, J] := aArea[I - 1, J];
              Inc(Counts[aArea[I, J]]);
            end;
          end;
        end;
      until OK;
      // In some (rather rare) cases, the different area fields have no horizontal or vertical contact
      // If this is the case, restart the area generation procedure
      for K := 0 to Length(Counts) - 1 do begin
        First := True;
        for I := 0 to iSize - 1 do begin
          for J := 0 to iSize - 1 do begin
            if aArea[I, J] = K then begin
              if First then begin
                I0 := I; J0 := J;
                First := False;
              end
              else begin
                if not (((Abs(I0 - I) = 1) and (J0 = J)) or ((Abs(J0 - J) = 1) and (I0 = I))) then
                  AreasOK := False;
                I0 := I; J0 := J;
              end;
            end;
          end;
        end;
      end;
    end;
  until AreasOK;
  // Display areas (by coloring the shapes) and area products
  SetLength(aProducts, Length(Counts)); SetLength(Done, Length(Counts));
  for I := 0 to Length(aProducts) - 1 do begin
    aProducts[I] := 1; Done[I] := False;
  end;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := Colors[aArea[I, J]];
      aGridColors[I, J] := Colors[aArea[I, J]];
      stGridNumbers[I, J].Caption := '';
      aProducts[aArea[I, J]] *= aGridNumbers[I, J];                                // calculate area products
    end;
  end;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if not Done[aArea[I, J]] then begin
        // The Done array is used to display the area products only once (in 1st field of area)
        laProducts[I, J].Visible := True; laProducts[I, J].Caption := IntToStr(aProducts[aArea[I, J]]);
        Done[aArea[I, J]] := True;
      end;
    end;
  end;
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfFactors.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Puzzle size > ...": Select size of the puzzle (4x4, 5x5, 6x6) }

procedure TfFactors.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize5.Checked := False; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfFactors.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 5;
end;

procedure TfFactors.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu item "Options > Unique numbers on diagonals > ...": Toggle use unique numbers on diagonals rule on/off }

procedure TfFactors.mOptionsUniqueDiagsClick(Sender: TObject);

// Option not implemented in v1.0 (menu item disabled)

begin
  if mOptionsUniqueDiags.Checked then
    mOptionsUniqueDiags.Checked := False
  else
    mOptionsUniqueDiags.Checked := True;
end;

{ Menu item "Help > Source and copyright": Display source and copyright of Factors puzzle }

procedure TfFactors.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Factors" at https://www.janko.at/. (puzzle name: "Factors"). They think, that "Factors" was first published in the ';
  S += 'Japanese puzzle magazine "Communications Nikoli", Nr. 92 (Sep. 2009).  The author, Yano Ryuou, is probably the inventor of ';
  S += 'this kind of puzzle and the puzzle game is probably public domain. Concerning my "Factors" PC application, itself, it is ';
  S += 'freeware/open source, as all puzzle and other applications on www.streetinfo.lu.';
  MessageDlg('Factors', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel und Puzzles' website": Point web browser to janko.at }

procedure TfFactors.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > About": Display application about }

procedure TfFactors.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Factors.' + LineEnding;
  S += 'Fill the grid fields with numbers from 1 to N, in a way, that the product of the numbers in each area ';
  S += 'equals the number given for this area.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March 2021.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About "Factors"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid values and check if this is a correct solution }

procedure TfFactors.btDoneClick(Sender: TObject);

// Solution validity check is done by checking the user values and their uniqueness per row and column, and by calculating the products
// of the numbers in the different areas, and not by comparison of the user and generated grid numbers; in fact, there usually are several
// correct solutions...

var
  I, J, I1, J1: Integer;
  Solved: Boolean;
  UserNumbers: TNumbers;
  UserProducts: array of Integer;

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
  // Calculate the products of numbers in the different areas and compare with the products given by the application (written in the grid areas)
  if Solved then begin
    SetLength(UserProducts, Length(aProducts));
    for I := 0 to Length(UserProducts) - 1 do
      UserProducts[I] := 1;
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do
        UserProducts[aArea[I, J]] *= UserNumbers[I, J];
    end;
    for I := 0 to Length(UserProducts) - 1 do begin
      if UserProducts[I] <> aProducts[I] then
        Solved := False;
    end;
  end;
  // Display "success or not" message
  if Solved then
    MessageDlg('Factors', 'You have successfully solved this Factors puzzle!', mtInformation, [mbOK], 0)
  else begin
    MessageDlg('Factors', 'Sorry. Your numbers are no valid solution of this Factors puzzle !', mtInformation, [mbOK], 0);
    btShow.Enabled := True;                                                    // give user possibility to view correct solution
  end;
  // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
  // This will also disable any action, when a grid field or number button is clicked
  btDone.Enabled := False; btClearAll.Enabled := False;
end;

{ Button "Show": Show puzzle solution }

procedure TfFactors.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := aGridColors[I, J];
      stGridNumbers[I, J].Font.Color := clBlack; laProducts[I, J].Font.Color := clBlack;
      stGridNumbers[I, J].Caption := IntToStr(aGridNumbers[I, J]);
    end;
  end;
  btShow.Enabled := False;
end;

{ Number buttons: Write corr. number into actually selected grid field }

procedure TfFactors.bt1Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '1', iRow, iCol);
end;

procedure TfFactors.bt2Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '2', iRow, iCol);
end;

procedure TfFactors.bt3Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '3', iRow, iCol);
end;

procedure TfFactors.bt4Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '4', iRow, iCol);
end;

procedure TfFactors.bt5Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '5', iRow, iCol);
end;

procedure TfFactors.bt6Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '6', iRow, iCol);
end;

procedure TfFactors.bt7Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '7', iRow, iCol);
end;

procedure TfFactors.bt8Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '8', iRow, iCol);
end;

procedure TfFactors.bt9Click(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '9', iRow, iCol);
end;

{ Button "Clear": Clear number in actually selected grid field }

procedure TfFactors.btClearClick(Sender: TObject);

begin
  WriteNumber(shGridShapes, stGridNumbers, aGridColors, laProducts, '', iRow, iCol);
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfFactors.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := aGridColors[I, J];
      stGridNumbers[I, J].Font.Color := clBlack; laProducts[I, J].Font.Color := clBlack;
      stGridNumbers[I, J].Caption := '';
    end;
  end;
  iRow := -1; iCol := -1;                                                      // no field actually selected
end;

{ User click on any of the 6x6 statictexts fields: Highlight/unhighlight corresponding shape }

procedure TfFactors.laGrid00Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 0, 0, iRow, iCol);
end;

procedure TfFactors.laGrid01Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 0, 1, iRow, iCol);
end;

procedure TfFactors.laGrid02Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 0, 2, iRow, iCol);
end;

procedure TfFactors.laGrid03Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 0, 3, iRow, iCol);
end;

procedure TfFactors.laGrid04Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 0, 4, iRow, iCol);
end;

procedure TfFactors.laGrid05Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 0, 5, iRow, iCol);
end;

procedure TfFactors.laGrid10Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 1, 0, iRow, iCol);
end;

procedure TfFactors.laGrid11Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 1, 1, iRow, iCol);
end;

procedure TfFactors.laGrid12Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 1, 2, iRow, iCol);
end;

procedure TfFactors.laGrid13Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 1, 3, iRow, iCol);
end;

procedure TfFactors.laGrid14Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 1, 4, iRow, iCol);
end;

procedure TfFactors.laGrid15Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 1, 5, iRow, iCol);
end;

procedure TfFactors.laGrid20Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 2, 0, iRow, iCol);
end;

procedure TfFactors.laGrid21Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 2, 1, iRow, iCol);
end;

procedure TfFactors.laGrid22Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 2, 2, iRow, iCol);
end;

procedure TfFactors.laGrid23Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 2, 3, iRow, iCol);
end;

procedure TfFactors.laGrid24Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 2, 4, iRow, iCol);
end;

procedure TfFactors.laGrid25Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 2, 5, iRow, iCol);
end;

procedure TfFactors.laGrid30Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 3, 0, iRow, iCol);
end;

procedure TfFactors.laGrid31Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 3, 1, iRow, iCol);
end;

procedure TfFactors.laGrid32Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 3, 2, iRow, iCol);
end;

procedure TfFactors.laGrid33Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 3, 3, iRow, iCol);
end;

procedure TfFactors.laGrid34Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 3, 4, iRow, iCol);
end;

procedure TfFactors.laGrid35Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 3, 5, iRow, iCol);
end;

procedure TfFactors.laGrid40Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 4, 0, iRow, iCol);
end;

procedure TfFactors.laGrid41Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 4, 1, iRow, iCol);
end;

procedure TfFactors.laGrid42Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 4, 2, iRow, iCol);
end;

procedure TfFactors.laGrid43Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 4, 3, iRow, iCol);
end;

procedure TfFactors.laGrid44Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 4, 4, iRow, iCol);
end;

procedure TfFactors.laGrid45Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 4, 5, iRow, iCol);
end;

procedure TfFactors.laGrid50Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 5, 0, iRow, iCol);
end;

procedure TfFactors.laGrid51Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 5, 1, iRow, iCol);
end;

procedure TfFactors.laGrid52Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 5, 2, iRow, iCol);
end;

procedure TfFactors.laGrid53Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 5, 3, iRow, iCol);
end;

procedure TfFactors.laGrid54Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 5, 4, iRow, iCol);
end;

procedure TfFactors.laGrid55Click(Sender: TObject);

begin
  SelectField(shGridShapes, stGridNumbers, aGridColors, laProducts, 5, 5, iRow, iCol);
end;

end.

