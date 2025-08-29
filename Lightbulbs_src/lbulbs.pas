{****************************************}
{* Main unit for Lightbulbs application *}
{****************************************}

unit lbulbs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf;

type
  TNumbers = array[0..5, 0..5] of Integer;
  TNumberShapes = array[0..5, 0..5] of TShape;
  TNumberLabels = array[0..5, 0..5] of TStaticText;
  {**************}
  { TfLightbulbs }
  {**************}
  TfLightbulbs = class(TForm)
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
    edHelp, edRules: TMemo;
    btDone, btShow, btClearAll: TButton;
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
    aGridNumbers: TNumbers;
    shGridShapes: TNumberShapes;
    stGridNumbers: TNumberLabels;
  end;

var
  fLightbulbs: TfLightbulbs;

implementation

{$R *.lfm}

{ Toggle color of given grid field (user click on this field) }

procedure ColorField(var GridShapes: TNumberShapes; var GridNumbers: TNumberLabels; Row, Col: Integer);

begin
  // Set field color
  if GridShapes[Row, Col].Brush.Color = cl3DLight then
    GridShapes[Row, Col].Brush.Color := clWhite                                // gray to white
  else if GridShapes[Row, Col].Brush.Color = clWhite then
    GridShapes[Row, Col].Brush.Color := clBlack                                // white to black
  else
    GridShapes[Row, Col].Brush.Color := cl3DLight;                             // black to gray again
  // Adapt font color (number)
  if GridShapes[Row, Col].Brush.Color = clBlack then
    GridNumbers[Row, Col].Font.Color := clWhite
  else
    GridNumbers[Row, Col].Font.Color := clBlack;
end;

{**************}
{ TfLightbulbs }
{**************}

{ Application start: Initialisation }

procedure TfLightbulbs.FormCreate(Sender: TObject);

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
  iSizeTemp := 5;
  iRow := -1; iCol := -1;
  // Generate a new puzzle
  mPuzzleNew.Click;
end;

{ Menu item "Puzzle > New": Generate new puzzle }

procedure TfLightbulbs.mPuzzleNewClick(Sender: TObject);

var
  Count, White, NotWhite, NotWhiteMax, I, J, K1, K2, K3, K4: Integer;

begin
  // Set user option selection active now
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
  // Randomly color the fields (white or black)
  repeat
    Count := 0;
    for I := 0 to iSize - 1 do begin
      for J := 0 to iSize - 1 do begin
        if Random(3) = 0 then begin
          aGridNumbers[I, J] := -1;                                            // -1 indicating a black field
        end
        else begin
          aGridNumbers[I, J] := 1;                                             // 1 indicating a white field
          Inc(Count);
        end;
      end;
    end;
  until Count > 2 * Sqr(iSize) / 3;                                            // 2/3 of the fields should be white
  // For each field, determine number of white neighbours and fill in the numbers array
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      // Count white neighbours
      White := 0;
      for K1 := I - 1 to I + 1 do begin
        for K2 := J - 1 to J + 1 do begin
          if (K1 >= 0) and (K1 <= iSize - 1) and (K2 >= 0) and (K2 <= iSize - 1) then begin
            if aGridNumbers[K1, K2] > 0 then                                   // positive number = white field
              Inc(White);
          end;
        end;
      end;
      // Do not allow white field without white neighbours (to avoid puzzles with 1 only white field being a possible solution)
      if White = 1 then begin
        K3 := 0; K4 := 0;
        // If there is such a field, color a black neigbour white
        for K1 := I - 1 to I + 1 do begin
          for K2 := J - 1 to J + 1 do begin
            if (K1 >= 0) and (K1 <= iSize - 1) and (K2 >= 0) and (K2 <= iSize - 1) then begin
              if aGridNumbers[K1, K2] > 0 then begin
                K3 := K1; K4 := K2;
              end;
            end;
          end;
        end;
        aGridNumbers[K3, K4] := -aGridNumbers[K3, K4];
      end;
      // Write number for this filed
      if aGridNumbers[I, J] > 0 then begin
        // Actual field = white: fill in the number of white neighbours
        aGridNumbers[I, J] := White;
      end
      else begin
        // Actual field = black: fill in a random number different from the number of white neighbours
        repeat
          NotWhiteMax := 9;
          // Limit the number in black fields, depending on field position
          if (I = 0) or (I = iSize - 1) then
            NotWhiteMax -= 3;
          if (J = 0) or (J = iSize - 1) then
            NotWhiteMax -= 3;
          // Random number from 2 ... the limit calculated above
          NotWhite := Random(NotWhiteMax - 1) + 2;
        until NotWhite <> White;                                               // number written must be <> from number of white neighbours
        aGridNumbers[I, J] := -NotWhite;                                       // numbers in black field marked negative
      end;
    end;
  end;
  // Write numbers to the grid
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      stGridNumbers[I, J].Caption := IntToStr(Abs(aGridNumbers[I, J]));
      shGridShapes[I, J].Brush.Color := cl3DLight;                             // all fields colored gray
      stGridNumbers[I, J].Font.Color := clBlack;                               // must set font to black (as maybe white because of black field before)
    end;
  end;
  // Adapt button availability
  btDone.Enabled := True; btClearAll.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfLightbulbs.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Puzzle size > ...": Select size of the puzzle (4x4, 5x5, 6x6) }

procedure TfLightbulbs.mOptionsSize4Click(Sender: TObject);

begin
  mOptionsSize4.Checked := True; mOptionsSize5.Checked := False; mOptionsSize6.Checked := False;
  iSizeTemp := 4;                                                              // option will become active when "New puzzle" is selected
end;

procedure TfLightbulbs.mOptionsSize5Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := True; mOptionsSize6.Checked := False;
  iSizeTemp := 5;
end;

procedure TfLightbulbs.mOptionsSize6Click(Sender: TObject);

begin
  mOptionsSize4.Checked := False; mOptionsSize5.Checked := False; mOptionsSize6.Checked := True;
  iSizeTemp := 6;
end;

{ Menu item "Help > Source and copyright": Dispaly game source (inventor) and application copyright }

procedure TfLightbulbs.mHelpCopyrightClick(Sender: TObject);

var
  S: string;

begin
  S := 'I found "Lightbulbs" at https://www.janko.at/ (puzzle name: "Lampions"). The game has been invented by Erich Friedman and I suppose ';
  S += 'that it is public domain. Concerning my "Lightbulbs" PC application itself, it is freeware/open source, as all my programs on this site.';
  MessageDlg('Lightbulbs copyright', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > 'Rätsel und Puzzles' website": Point webbrowser to 'Rätsel und Puzzles' website }

procedure TfLightbulbs.mHelpWebsiteClick(Sender: TObject);

begin
  OpenDocument('https://www.janko.at/Raetsel/');
end;

{ Menu item "Help > About": Display application about }

procedure TfLightbulbs.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Lightbulbs.' + LineEnding;
  S += 'Color the grid fields white or black, knowing that a number in a white field indicates how many white neighbours it has ';
  S += 'and that a number in a black field must not indicate the number of its white neighbours.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September - October 2020.' + LineEnding;
  S += 'See also: Source and copyright.';
  MessageDlg('About Lightbulbs', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Read user solution from form grid values and check if this is a correct solution }

procedure TfLightbulbs.btDoneClick(Sender: TObject);

// Solution validity check is done by determining the number of white neighbours of each field and then checking if the user correctly
// colored this field and not by comparison of the user colors with the actual colors of the generated grid; in fact, there might be
// several correct solutions...

var
  Count, White, I, J, K1, K2: Integer;
  Solved: Boolean;

begin
  // Check if all fields have been colored and that there is at least one white field
  Count := 0; White := 0;
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if (shGridShapes[I, J].Brush.Color = clWhite) or (shGridShapes[I, J].Brush.Color = clBlack) then begin
        Inc(Count);
        if shGridShapes[I, J].Brush.Color = clWhite then
          Inc(White);
      end;
    end;
  end;
  // All fields have been colored
  if Count = Sqr(iSize) then begin
    // Disable field number entry (by setting actual field to "none")
    iRow := -1; iCol := -1;
    // Check user solution
    Solved := True;
    if White = 0 then begin
      // No white field
      Solved := False;
    end
    else begin
      // Check user color for each grid field
      for I := 0 to iSize - 1 do begin
        for J := 0 to iSize - 1 do begin
          White := 0;
          // Count number of white neigbours
          for K1 := I - 1 to I + 1 do begin
            for K2 := J - 1 to J + 1 do begin
              if (K1 >= 0) and (K1 <= iSize - 1) and (K2 >= 0) and (K2 <= iSize - 1) then begin
                if shGridShapes[K1, K2].Brush.Color = clWhite then
                  Inc(White);
              end;
            end;
          end;
          // Check actual field's color
          if (shGridShapes[I, J].Brush.Color = clWhite) and (White <> Abs(aGridNumbers[I, J])) then
            // If user colored it white, its number must equal the number of white neigbours
            Solved := False;
          if (shGridShapes[I, J].Brush.Color = clBlack) and (White = Abs(aGridNumbers[I, J])) then
            // If user colored it black, its number must NOT equal the number of white neigbours
            Solved := False;
        end;
      end;
    end;
    // Display "success or not" message
    if Solved then
      MessageDlg('Lightbulbs', 'You have successfully solved this Lightbulbs puzzle!', mtInformation, [mbOK], 0)
    else begin
      MessageDlg('Lightbulbs', 'Sorry. Your numbers are no valid solution of this Lightbulbs puzzle!', mtInformation, [mbOK], 0);
      btShow.Enabled := True;                                                  // give user possibility to view correct solution
    end;
    // Disable the "Done" and "Clear all" buttons (until "New puzzle" is chosen)
    btDone.Enabled := False; btClearAll.Enabled := False;
  end
  // User didn't color all grid fields: tell her to do so
  else
    MessageDlg('Lightbulbs', 'Puzzle not completed: You have to color all fields!', mtError, [mbOK], 0);
end;

{ Button "Show": Show puzzle solution }

procedure TfLightbulbs.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      if aGridNumbers[I, J] > 0 then begin
        // A positive number in the array indicates a white field
        shGridShapes[I, J].Brush.Color := clWhite;
        stGridNumbers[I, J].Font.Color := clBlack;
      end
      else begin
        // A negative number in the array indicates a black field
        shGridShapes[I, J].Brush.Color := clBlack;
        stGridNumbers[I, J].Font.Color := clWhite;
      end;
    end;
  end;
  btShow.Enabled := False;
end;

{ Button "Clear all": Clear all grid fields (if user wants to start over...) }

procedure TfLightbulbs.btClearAllClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to iSize - 1 do begin
    for J := 0 to iSize - 1 do begin
      shGridShapes[I, J].Brush.Color := cl3DLight;
      stGridNumbers[I, J].Font.Color := clBlack;
    end;
  end;
end;

{ User click on any of the 6x6 statictexts fields: Toggle color of corresponding shape }

procedure TfLightbulbs.laGrid00Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 0, 0);
end;

procedure TfLightbulbs.laGrid01Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 0, 1);
end;

procedure TfLightbulbs.laGrid02Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 0, 2);
end;

procedure TfLightbulbs.laGrid03Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 0, 3);
end;

procedure TfLightbulbs.laGrid04Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 0, 4);
end;

procedure TfLightbulbs.laGrid05Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 0, 5);
end;

procedure TfLightbulbs.laGrid10Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 1, 0);
end;

procedure TfLightbulbs.laGrid11Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 1, 1);
end;

procedure TfLightbulbs.laGrid12Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 1, 2);
end;

procedure TfLightbulbs.laGrid13Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 1, 3);
end;

procedure TfLightbulbs.laGrid14Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 1, 4);
end;

procedure TfLightbulbs.laGrid15Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 1, 5);
end;

procedure TfLightbulbs.laGrid20Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 2, 0);
end;

procedure TfLightbulbs.laGrid21Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 2, 1);
end;

procedure TfLightbulbs.laGrid22Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 2, 2);
end;

procedure TfLightbulbs.laGrid23Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 2, 3);
end;

procedure TfLightbulbs.laGrid24Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 2, 4);
end;

procedure TfLightbulbs.laGrid25Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 2, 5);
end;

procedure TfLightbulbs.laGrid30Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 3, 0);
end;

procedure TfLightbulbs.laGrid31Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 3, 1);
end;

procedure TfLightbulbs.laGrid32Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 3, 2);
end;

procedure TfLightbulbs.laGrid33Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 3, 3);
end;

procedure TfLightbulbs.laGrid34Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 3, 4);
end;

procedure TfLightbulbs.laGrid35Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 3, 5);
end;

procedure TfLightbulbs.laGrid40Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 4, 0);
end;

procedure TfLightbulbs.laGrid41Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 4, 1);
end;

procedure TfLightbulbs.laGrid42Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 4, 2);
end;

procedure TfLightbulbs.laGrid43Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 4, 3);
end;

procedure TfLightbulbs.laGrid44Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 4, 4);
end;

procedure TfLightbulbs.laGrid45Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 4, 5);
end;

procedure TfLightbulbs.laGrid50Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 5, 0);
end;

procedure TfLightbulbs.laGrid51Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 5, 1);
end;

procedure TfLightbulbs.laGrid52Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 5, 2);
end;

procedure TfLightbulbs.laGrid53Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 5, 3);
end;

procedure TfLightbulbs.laGrid54Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 5, 4);
end;

procedure TfLightbulbs.laGrid55Click(Sender: TObject);

begin
  ColorField(shGridShapes, stGridNumbers, 5, 5);
end;

end.

