{**************************************}
{* Main unit for Dividers application *}
{**************************************}

unit divider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, Grids, help;

type
  TGrid = array[1 .. 8, 1 .. 8] of TShape;
  TGridNumbers = array[1 .. 8, 1 .. 8] of TLabel;
  TNumbers = array[1 .. 8, 1 .. 8] of Integer;
  TAnswer = array of Integer;
  TExclusions = array of Integer;
  {************}
  { TfDividers }
  {************}
  TfDividers = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameSimple, mGameAddition, mGameSubtraction, mGameExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsGrid, mSettingsGrid3, mSettingsGrid4, mSettingsGrid5, mSettingsGrid6, mSettingsGrid8: TMenuItem;
    mSettingsNMax, mSettingsNMax100, mSettingsNMax1000: TMenuItem;
    mSettingsMax, mSettingsMax10, mSettingsMax20, mSettingsMax50, mSettingsMax100: TMenuItem;
    mSettingsExclusions: TMenuItem;
    mSettingsExclusions2, mSettingsExclusions5, mSettingsExclusions10, mSettingsExclusions20: TMenuItem;
    mSettingsExclusions25, mSettingsExclusions50, mSettingsExclusions100: TMenuItem;
    mSettingsTime, mSettingsTime20, mSettingsTime30, mSettingsTime60, mSettingsTime120, mSettingsTimeUnlimited: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    laDivider, laNumber, laTime: TLabel;
    edDivider, edNumber, edTime: TEdit;
    shGrid11, shGrid12, shGrid13, shGrid14, shGrid15, shGrid16, shGrid17, shGrid18: TShape;
    shGrid21, shGrid22, shGrid23, shGrid24, shGrid25, shGrid26, shGrid27, shGrid28: TShape;
    shGrid31, shGrid32, shGrid33, shGrid34, shGrid35, shGrid36, shGrid37, shGrid38: TShape;
    shGrid41, shGrid42, shGrid43, shGrid44, shGrid45, shGrid46, shGrid47, shGrid48: TShape;
    shGrid51, shGrid52, shGrid53, shGrid54, shGrid55, shGrid56, shGrid57, shGrid58: TShape;
    shGrid61, shGrid62, shGrid63, shGrid64, shGrid65, shGrid66, shGrid67, shGrid68: TShape;
    shGrid71, shGrid72, shGrid73, shGrid74, shGrid75, shGrid76, shGrid77, shGrid78: TShape;
    shGrid81, shGrid82, shGrid83, shGrid84, shGrid85, shGrid86, shGrid87, shGrid88: TShape;
    stTitle: TStaticText;
    edInstructions, edInstructions2: TMemo;
    StaticText1: TStaticText;
    sgEval: TStringGrid;
    imEval: TImage;
    btQuestion: TButton;
    tiDivider: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameSimpleClick(Sender: TObject);
    procedure mGameAdditionClick(Sender: TObject);
    procedure mGameSubtractionClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsGrid3Click(Sender: TObject);
    procedure mSettingsGrid4Click(Sender: TObject);
    procedure mSettingsGrid5Click(Sender: TObject);
    procedure mSettingsGrid6Click(Sender: TObject);
    procedure mSettingsGrid8Click(Sender: TObject);
    procedure mSettingsNMax100Click(Sender: TObject);
    procedure mSettingsNMax1000Click(Sender: TObject);
    procedure mSettingsMax10Click(Sender: TObject);
    procedure mSettingsMax20Click(Sender: TObject);
    procedure mSettingsMax50Click(Sender: TObject);
    procedure mSettingsMax100Click(Sender: TObject);
    procedure mSettingsExclusions2Click(Sender: TObject);
    procedure mSettingsExclusions5Click(Sender: TObject);
    procedure mSettingsExclusions10Click(Sender: TObject);
    procedure mSettingsExclusions20Click(Sender: TObject);
    procedure mSettingsExclusions25Click(Sender: TObject);
    procedure mSettingsExclusions50Click(Sender: TObject);
    procedure mSettingsExclusions100Click(Sender: TObject);
    procedure mSettingsTime20Click(Sender: TObject);
    procedure mSettingsTime30Click(Sender: TObject);
    procedure mSettingsTime60Click(Sender: TObject);
    procedure mSettingsTime120Click(Sender: TObject);
    procedure mSettingsTimeUnlimitedClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure shGrid11MouseDown(Sender: TObject);
    procedure shGrid12MouseDown(Sender: TObject);
    procedure shGrid13MouseDown(Sender: TObject);
    procedure shGrid14MouseDown(Sender: TObject);
    procedure shGrid15MouseDown(Sender: TObject);
    procedure shGrid16MouseDown(Sender: TObject);
    procedure shGrid17MouseDown(Sender: TObject);
    procedure shGrid18MouseDown(Sender: TObject);
    procedure shGrid21MouseDown(Sender: TObject);
    procedure shGrid22MouseDown(Sender: TObject);
    procedure shGrid23MouseDown(Sender: TObject);
    procedure shGrid24MouseDown(Sender: TObject);
    procedure shGrid25MouseDown(Sender: TObject);
    procedure shGrid26MouseDown(Sender: TObject);
    procedure shGrid27MouseDown(Sender: TObject);
    procedure shGrid28MouseDown(Sender: TObject);
    procedure shGrid31MouseDown(Sender: TObject);
    procedure shGrid32MouseDown(Sender: TObject);
    procedure shGrid33MouseDown(Sender: TObject);
    procedure shGrid34MouseDown(Sender: TObject);
    procedure shGrid35MouseDown(Sender: TObject);
    procedure shGrid36MouseDown(Sender: TObject);
    procedure shGrid37MouseDown(Sender: TObject);
    procedure shGrid38MouseDown(Sender: TObject);
    procedure shGrid41MouseDown(Sender: TObject);
    procedure shGrid42MouseDown(Sender: TObject);
    procedure shGrid43MouseDown(Sender: TObject);
    procedure shGrid44MouseDown(Sender: TObject);
    procedure shGrid45MouseDown(Sender: TObject);
    procedure shGrid46MouseDown(Sender: TObject);
    procedure shGrid47MouseDown(Sender: TObject);
    procedure shGrid48MouseDown(Sender: TObject);
    procedure shGrid51MouseDown(Sender: TObject);
    procedure shGrid52MouseDown(Sender: TObject);
    procedure shGrid53MouseDown(Sender: TObject);
    procedure shGrid54MouseDown(Sender: TObject);
    procedure shGrid55MouseDown(Sender: TObject);
    procedure shGrid56MouseDown(Sender: TObject);
    procedure shGrid57MouseDown(Sender: TObject);
    procedure shGrid58MouseDown(Sender: TObject);
    procedure shGrid61MouseDown(Sender: TObject);
    procedure shGrid62MouseDown(Sender: TObject);
    procedure shGrid63MouseDown(Sender: TObject);
    procedure shGrid64MouseDown(Sender: TObject);
    procedure shGrid65MouseDown(Sender: TObject);
    procedure shGrid66MouseDown(Sender: TObject);
    procedure shGrid67MouseDown(Sender: TObject);
    procedure shGrid68MouseDown(Sender: TObject);
    procedure shGrid71MouseDown(Sender: TObject);
    procedure shGrid72MouseDown(Sender: TObject);
    procedure shGrid73MouseDown(Sender: TObject);
    procedure shGrid74MouseDown(Sender: TObject);
    procedure shGrid75MouseDown(Sender: TObject);
    procedure shGrid76MouseDown(Sender: TObject);
    procedure shGrid77MouseDown(Sender: TObject);
    procedure shGrid78MouseDown(Sender: TObject);
    procedure shGrid81MouseDown(Sender: TObject);
    procedure shGrid82MouseDown(Sender: TObject);
    procedure shGrid83MouseDown(Sender: TObject);
    procedure shGrid84MouseDown(Sender: TObject);
    procedure shGrid85MouseDown(Sender: TObject);
    procedure shGrid86MouseDown(Sender: TObject);
    procedure shGrid87MouseDown(Sender: TObject);
    procedure shGrid88MouseDown(Sender: TObject);
    procedure tiDividerTimer(Sender: TObject);
  private
    iGridSize, iGridSizeTemp, iNMax, iNMaxTemp, iMax, iMaxTemp, iTime, iGameTime: Integer;
    iDivider, iNumber, iQuestion, iCorrect, iFalse: Integer;
    sGame, sTimeTemp, sTime: string;
    aGrid: TGrid;
    aGridNumbers: TGridNumbers;
    aNumbers: TNumbers;
    aExclusions, aExclusionsTemp: TExclusions;
    aAnswer, aUserAnswer: TAnswer;
  end;

var
  fDividers: TfDividers;

implementation

{$R *.lfm}

{ Format numbers in evaluation grid (right-alignment) }

function GridFormat(N: Integer; S: string): string;

var
  GF: string;

begin
  GF := IntToStr(N);
  if N < 10 then
    GF := '  ' + GF
  else if N < 100 then
    GF := ' ' + GF;
  if S = '' then
    GF := ' ' + GF
  else
    GF += S;
  GridFormat := GF;
end;

{ Create the numbers grid }

procedure CreateGrid(GridSize: Integer; Grid: TGrid; var Numbers: TNumbers; GridNumbers: TGridNumbers);

var
  I, J: Integer;

begin
  for J := 1 to 8 do begin
    for I := 1 to 8 do begin
      // Do this for squares included in actual grid
      if (J <= GridSize) and (I <= GridSize) then begin
        // No number actually (is the case when a new game has been selected)
        if Numbers[J, I] = 0 then
          GridNumbers[J, I].Caption := ''
        // Normal case
        else begin
          GridNumbers[J, I].Caption := IntToStr(Numbers[J, I]);
          // Add spaces as needed to center the number
          if Numbers[J, I] < 10 then
            GridNumbers[J, I].Caption := '   ' + GridNumbers[J, I].Caption
          else if Numbers[J, I] < 100 then
            GridNumbers[J, I].Caption := '  ' + GridNumbers[J, I].Caption;
        end;
        Grid[J, I].Visible := True;
        Grid[J, I].Brush.Color := clLime;                                      // lime squares indicate not selected numbers
        GridNumbers[J, I].Visible := True;
      end
      // Do this for squares not included in actual grid
      else begin
        Grid[J, I].Visible := False;
        GridNumbers[J, I].Visible := False;                                    // hide all squares outside the actual grid
      end;
    end;
  end;
end;

{ Reset all numbers (for given grid) to 0; clear the numbers lables }

procedure ResetNumbers(N: Integer; out Numbers: TNumbers; GridNumbers: TGridNumbers);

var
  I, J: Integer;

begin
  for J := 1 to N do begin
    for I := 1 to N do begin
      Numbers[J, I] := 0;
      GridNumbers[J, I].Caption := '';
    end;
  end;
end;

{ Prepare for starting a new game }

procedure NewGame(GridSizeTemp, NMaxTemp, MaxTemp: Integer; STimeTemp: string; ExclusionsTemp: TExclusions;
  out Question, QCorrect, QFalse, GridSize, NMax, Max, GTime: Integer; out SGTime: string;
  out Numbers: TNumbers; out Exclusions: TExclusions; Grid: TGrid; GridNumbers: TGridNumbers);

var
  I: Integer;

begin
  fDividers.tiDivider.Enabled := False;
  GridSize := GridSizeTemp; Max := MaxTemp; NMax := NMaxTemp;                  // game parameters chosen now becoming active
  SGTime := STimeTemp;
  Exclusions := ExclusionsTemp;
  Question := 0; QCorrect := 0; QFalse := 0;
  fDividers.imEval.Visible := False;
  ResetNumbers(8, Numbers, GridNumbers);                                       // set all numbers to 0
  CreateGrid(GridSize, Grid, Numbers, GridNumbers);                            // create the (empty) grid
  fDividers.edDivider.Text := ''; fDividers.edNumber.Text := '';
  // Show (enable) timer value or not; make max. answer time active (or ignore it)
  if STimeTemp = 'unlimited' then begin
    GTime := -1;
    fDividers.laTime.Enabled := False;
    fDividers.edTime.Text := '';
    fDividers.edTime.Enabled := False;
  end
  else begin
    STimeTemp := StringReplace(STimeTemp, ' sec', '', []);
    STimeTemp := StringReplace(STimeTemp, ' min', '', []);
    GTime := StrToInt(STimeTemp);
    if RightStr(SGTime, 3) = 'min' then
      GTime *= 60;
    fDividers.laTime.Enabled := True;
    fDividers.edTime.Enabled := True;
    fDividers.edTime.Color := clBlack;
    fDividers.edTime.Text := SGTime;
  end;
  // Clear evaluation grid
  for I := 0 to 3 do
    fDividers.sgEval.Cells[1, I] := '';
  fDividers.btQuestion.Caption := 'Question';
end;

{ Update list of dividers to be excluded }

procedure ExcludeDivider(Divider: Integer; Exclude: Boolean; var Exclusions: TExclusions);

// Variable Exclude=True means that divider is to exclude (= to be added to the list); Exclude=False means, it must be removed from the list

var
  I: Integer;
  IsInList: Boolean;
  OldExclusions: TExclusions;

begin
  OldExclusions := Exclusions; IsInList := False;
  // Check if divider is actually in the exclusions list
  for I := 0 to Length(OldExclusions) - 1 do begin
    if Divider = OldExclusions[I] then
      IsInList := True;
  end;
  // If devider not to be excluded is in the list, remove it
  if not Exclude and IsInList then begin
    SetLength(Exclusions, 0);
    for I := 0 to Length(OldExclusions) - 1 do begin
      if OldExclusions[I] <> Divider then begin
        SetLength(Exclusions, Length(Exclusions) + 1);
        Exclusions[Length(Exclusions) - 1] := OldExclusions[I];
      end;
    end;
  end
  // If devider to be excluded isn't in the list, add it
  else if Exclude and not IsInList then begin
    SetLength(Exclusions, Length(Exclusions) + 1);
    Exclusions[Length(Exclusions) - 1] := Divider;
  end;
end;

{ Select number (as indicated by the square that has been clicked by user) }

procedure NumberSelect(Y, X: Integer; Grid: TGrid);

begin
  // Squares having only to "react" if a user answer is awaited
  if fDividers.btQuestion.Caption = 'Answer' then begin
    // Select the square, unless it already has been selected
    if Grid[Y, X].Brush.Color = clLime then
      // Highlight the selected square (yellow)
      Grid[Y, X].Brush.Color := clYellow
    // Unselect the square, if it already has been selected
    else
      // 'De-highlight' the unselected square (default color = lime)
      Grid[Y, X].Brush.Color := clLime;
  end;
end;

{************}
{ TfDividers }
{************}

{ Application start: Initialisation }

procedure TfDividers.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  // Create array with grid squares
  aGrid[1, 1] := shGrid11; aGrid[1, 2] := shGrid12; aGrid[1, 3] := shGrid13; aGrid[1, 4] := shGrid14;
  aGrid[1, 5] := shGrid15; aGrid[1, 6] := shGrid16; aGrid[1, 7] := shGrid17; aGrid[1, 8] := shGrid18;
  aGrid[2, 1] := shGrid21; aGrid[2, 2] := shGrid22; aGrid[2, 3] := shGrid23; aGrid[2, 4] := shGrid24;
  aGrid[2, 5] := shGrid25; aGrid[2, 6] := shGrid26; aGrid[2, 7] := shGrid27; aGrid[2, 8] := shGrid28;
  aGrid[3, 1] := shGrid31; aGrid[3, 2] := shGrid32; aGrid[3, 3] := shGrid33; aGrid[3, 4] := shGrid34;
  aGrid[3, 5] := shGrid35; aGrid[3, 6] := shGrid36; aGrid[3, 7] := shGrid37; aGrid[3, 8] := shGrid38;
  aGrid[4, 1] := shGrid41; aGrid[4, 2] := shGrid42; aGrid[4, 3] := shGrid43; aGrid[4, 4] := shGrid44;
  aGrid[4, 5] := shGrid45; aGrid[4, 6] := shGrid46; aGrid[4, 7] := shGrid47; aGrid[4, 8] := shGrid48;
  aGrid[5, 1] := shGrid51; aGrid[5, 2] := shGrid52; aGrid[5, 3] := shGrid53; aGrid[5, 4] := shGrid54;
  aGrid[5, 5] := shGrid55; aGrid[5, 6] := shGrid56; aGrid[5, 7] := shGrid57; aGrid[5, 8] := shGrid58;
  aGrid[6, 1] := shGrid61; aGrid[6, 2] := shGrid62; aGrid[6, 3] := shGrid63; aGrid[6, 4] := shGrid64;
  aGrid[6, 5] := shGrid65; aGrid[6, 6] := shGrid66; aGrid[6, 7] := shGrid67; aGrid[6, 8] := shGrid68;
  aGrid[7, 1] := shGrid71; aGrid[7, 2] := shGrid72; aGrid[7, 3] := shGrid73; aGrid[7, 4] := shGrid74;
  aGrid[7, 5] := shGrid75; aGrid[7, 6] := shGrid76; aGrid[7, 7] := shGrid77; aGrid[7, 8] := shGrid78;
  aGrid[8, 1] := shGrid81; aGrid[8, 2] := shGrid82; aGrid[8, 3] := shGrid83; aGrid[8, 4] := shGrid84;
  aGrid[8, 5] := shGrid85; aGrid[8, 6] := shGrid86; aGrid[8, 7] := shGrid87; aGrid[8, 8] := shGrid88;
  // Create array with newly created label objects
  for J := 1 to 8 do begin
    for I := 1 to 8 do begin
      aGridNumbers[J, I] := TLabel.Create(aGridNumbers[J, I]);
      aGridNumbers[J, I].Parent := Self;
      aGridNumbers[J, I].Font.Size := 9;
      aGridNumbers[J, I].Caption := '';
      aGridNumbers[J, I].Left := aGrid[J, I].Left + 14;
      aGridNumbers[J, I].Top := aGrid[J, I].Top + 17;
    end;
  end;
  // Default startup values
  sGame := 'simple';
  iGridSizeTemp := 5; iNMaxTemp := 100; iMaxTemp := 10; sTimeTemp := '30 sec';
  SetLength(aExclusionsTemp, 0);
  SetLength(aAnswer, 0);
  // Start random number generator
  Randomize;
  // Prepare new "simple" game
  mGameSimple.Click;
end;

{ Menu item "Game > Simple game" : Prepare for a new simple game }

procedure TfDividers.mGameSimpleClick(Sender: TObject);

begin
  sGame := 'simple';
  stTitle.Caption := 'Maths game' + LineEnding + 'Dividers (simple)';
  laNumber.Font.Style := []; laNumber.Caption := 'Operation number';
  laNumber.Enabled := False; edNumber.Enabled := False;
  edInstructions.Lines.Clear;
  edInstructions.Lines.AddText('Among the numbers in the grid, find all those, that have a given divider!');
  NewGame(iGridSizeTemp, iNMaxTemp, iMaxTemp, sTimeTemp, aExclusionsTemp, iQuestion, iCorrect, iFalse, iGridSize, iNMax, iMax, iTime, sTime,
    aNumbers, aExclusions, aGrid, aGridNumbers);
end;

{ Menu item "Game > Addition game" : Prepare for a new addition game }

procedure TfDividers.mGameAdditionClick(Sender: TObject);

begin
  sGame := 'addition';
  stTitle.Caption := 'Maths game' + LineEnding + 'Dividers (addition)';
  laNumber.Font.Style := [fsBold]; laNumber.Caption := 'Addition number';
  laNumber.Enabled := True; edNumber.Enabled := True;
  edInstructions.Lines.Clear;
  edInstructions.Lines.AddText('Find all numbers in the grid that plus a given number have a given divider!');
  NewGame(iGridSizeTemp, iNMaxTemp, iMaxTemp, sTimeTemp, aExclusionsTemp, iQuestion, iCorrect, iFalse, iGridSize, iNMax, iMax, iTime, sTime,
    aNumbers, aExclusions, aGrid, aGridNumbers);
end;

{ Menu item "Game > Subtraction game" : Prepare for a new subtraction game }

procedure TfDividers.mGameSubtractionClick(Sender: TObject);

begin
  sGame := 'subtraction';
  stTitle.Caption := 'Maths game' + LineEnding + 'Dividers (subtraction)';
  edInstructions.Lines.Clear;
  edInstructions.Lines.AddText('Find all numbers in the grid that minus a given number have a given divider!');
  laNumber.Font.Style := [fsBold]; laNumber.Caption := 'Subtraction number';
  laNumber.Enabled := True; edNumber.Enabled := True;
  NewGame(iGridSizeTemp, iNMaxTemp, iMaxTemp, sTimeTemp, aExclusionsTemp, iQuestion, iCorrect, iFalse, iGridSize, iNMax, iMax, iTime, sTime,
    aNumbers, aExclusions, aGrid, aGridNumbers);
end;

{ Menu item "Game > Exit" : Exit application }

procedure TfDividers.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Grid size > ...": Choose grid size (number of numbers displayed) }

procedure TfDividers.mSettingsGrid3Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := True;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 3;
end;

procedure TfDividers.mSettingsGrid4Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := True;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 4;
end;

procedure TfDividers.mSettingsGrid5Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := True;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 5;
end;

procedure TfDividers.mSettingsGrid6Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := True;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 6;
end;

procedure TfDividers.mSettingsGrid8Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := True;
  iGridSizeTemp := 8;
end;

{ Menu items "Settings > Max. number > ...": Choose maximum value for grid numbers }

procedure TfDividers.mSettingsNMax100Click(Sender: TObject);

begin
  mSettingsNMax100.Checked := True;
  mSettingsNMax1000.Checked := False;
  iNMaxTemp := 100;
  if mSettingsMax50.Checked or mSettingsMax100.Checked then begin
    // Using dividers up to 50/100 for max. number = 100 makes no sense
    MessageDlg('Invalid settings', 'Actual value of max. divider can''t be used with max. number = 100! Max. divider will be reset to 20.', mtWarning, [mbOK], 0);
    mSettingsMax50.Checked := False; mSettingsMax100.Checked := False;
    mSettingsMax20.Click;                                                      // this not only checks max = 20, but also disables not accurate exclusions
  end;
  mSettingsMax50.Enabled := False; mSettingsMax100.Enabled := False;
end;

procedure TfDividers.mSettingsNMax1000Click(Sender: TObject);

begin
  mSettingsNMax100.Checked := False;
  mSettingsNMax1000.Checked := True;
  iNMaxTemp := 1000;
  mSettingsMax50.Enabled := True; mSettingsMax100.Enabled := True;
end;

{ Menu items "Settings > Max. divider > ...": Choose maximum value for divider }

procedure TfDividers.mSettingsMax10Click(Sender: TObject);

begin
  mSettingsMax10.Checked := True;
  mSettingsMax20.Checked := False;
  mSettingsMax50.Checked := False;
  mSettingsMax100.Checked := False;
  iMaxTemp := 10;
  // Disable inaccurate exclusions
  mSettingsExclusions20.Checked := False;
  mSettingsExclusions25.Checked := False;
  mSettingsExclusions50.Checked := False;
  mSettingsExclusions100.Checked := False;
  mSettingsExclusions20.Enabled := False;
  mSettingsExclusions25.Enabled := False;
  mSettingsExclusions50.Enabled := False;
  mSettingsExclusions100.Enabled := False;
end;

procedure TfDividers.mSettingsMax20Click(Sender: TObject);

begin
  mSettingsMax10.Checked := False;
  mSettingsMax20.Checked := True;
  mSettingsMax50.Checked := False;
  mSettingsMax100.Checked := False;
  iMaxTemp := 20;
  // Enable/disable exclusions (as are acuurate or not)
  mSettingsExclusions25.Checked := False;
  mSettingsExclusions50.Checked := False;
  mSettingsExclusions100.Checked := False;
  mSettingsExclusions20.Enabled := True;
  mSettingsExclusions25.Enabled := False;
  mSettingsExclusions50.Enabled := False;
  mSettingsExclusions100.Enabled := False;
end;

procedure TfDividers.mSettingsMax50Click(Sender: TObject);

begin
  mSettingsMax10.Checked := False;
  mSettingsMax20.Checked := False;
  mSettingsMax50.Checked := True;
  mSettingsMax100.Checked := False;
  iMaxTemp := 50;
  // Enable/disable exclusions (as are acuurate or not)
  mSettingsExclusions100.Checked := False;
  mSettingsExclusions20.Enabled := True;
  mSettingsExclusions25.Enabled := True;
  mSettingsExclusions50.Enabled := True;
  mSettingsExclusions100.Enabled := False;
end;

procedure TfDividers.mSettingsMax100Click(Sender: TObject);

begin
  mSettingsMax10.Checked := False;
  mSettingsMax20.Checked := False;
  mSettingsMax50.Checked := False;
  mSettingsMax100.Checked := True;
  iMaxTemp := 100;
  // Enable/disable exclusions (as are acuurate or not)
  mSettingsExclusions20.Enabled := True;
  mSettingsExclusions25.Enabled := True;
  mSettingsExclusions50.Enabled := True;
  mSettingsExclusions100.Enabled := TRue;
end;

{ Menu items "Settings > Exclusions > ...": Choose which dividers should be excluded (as to easy...) }

procedure TfDividers.mSettingsExclusions2Click(Sender: TObject);

begin
  if mSettingsExclusions2.Checked then
    mSettingsExclusions2.Checked := False
  else
    mSettingsExclusions2.Checked := True;
  ExcludeDivider(2, mSettingsExclusions2.Checked, aExclusionsTemp);
end;

procedure TfDividers.mSettingsExclusions5Click(Sender: TObject);

begin
  if mSettingsExclusions5.Checked then
    mSettingsExclusions5.Checked := False
  else
    mSettingsExclusions5.Checked := True;
  ExcludeDivider(5, mSettingsExclusions5.Checked, aExclusionsTemp);
end;

procedure TfDividers.mSettingsExclusions10Click(Sender: TObject);

begin
  if mSettingsExclusions10.Checked then
    mSettingsExclusions10.Checked := False
  else
    mSettingsExclusions10.Checked := True;
  ExcludeDivider(10, mSettingsExclusions10.Checked, aExclusionsTemp);
end;

procedure TfDividers.mSettingsExclusions20Click(Sender: TObject);

begin
  if mSettingsExclusions20.Checked then
    mSettingsExclusions20.Checked := False
  else
    mSettingsExclusions20.Checked := True;
  ExcludeDivider(20, mSettingsExclusions20.Checked, aExclusionsTemp);
end;

procedure TfDividers.mSettingsExclusions25Click(Sender: TObject);

begin
  if mSettingsExclusions25.Checked then
    mSettingsExclusions25.Checked := False
  else
    mSettingsExclusions25.Checked := True;
  ExcludeDivider(25, mSettingsExclusions25.Checked, aExclusionsTemp);
end;

procedure TfDividers.mSettingsExclusions50Click(Sender: TObject);

begin
  if mSettingsExclusions50.Checked then
    mSettingsExclusions50.Checked := False
  else
    mSettingsExclusions50.Checked := True;
  ExcludeDivider(50, mSettingsExclusions50.Checked, aExclusionsTemp);
end;

procedure TfDividers.mSettingsExclusions100Click(Sender: TObject);

begin
  if mSettingsExclusions100.Checked then
    mSettingsExclusions100.Checked := False
  else
    mSettingsExclusions100.Checked := True;
  ExcludeDivider(100, mSettingsExclusions100.Checked, aExclusionsTemp);
end;

{ Menu items "Settings > Max. time > ...": Choose maximum answer time }

procedure TfDividers.mSettingsTime20Click(Sender: TObject);

begin
  mSettingsTime20.Checked := True;
  mSettingsTime30.Checked := False;
  mSettingsTime60.Checked := False;
  mSettingsTime120.Checked := False;
  mSettingsTimeUnlimited.Checked := False;
  sTimeTemp := '20 sec';
end;

procedure TfDividers.mSettingsTime30Click(Sender: TObject);

begin
  mSettingsTime20.Checked := False;
  mSettingsTime30.Checked := True;
  mSettingsTime60.Checked := False;
  mSettingsTime120.Checked := False;
  mSettingsTimeUnlimited.Checked := False;
  sTimeTemp := '30 sec';
end;

procedure TfDividers.mSettingsTime60Click(Sender: TObject);

begin
  mSettingsTime20.Checked := False;
  mSettingsTime30.Checked := False;
  mSettingsTime60.Checked := True;
  mSettingsTime120.Checked := False;
  mSettingsTimeUnlimited.Checked := False;
  sTimeTemp := '1 min';
end;

procedure TfDividers.mSettingsTime120Click(Sender: TObject);

begin
  mSettingsTime20.Checked := False;
  mSettingsTime30.Checked := False;
  mSettingsTime60.Checked := False;
  mSettingsTime120.Checked := True;
  mSettingsTimeUnlimited.Checked := False;
  sTimeTemp := '2 min';
end;

procedure TfDividers.mSettingsTimeUnlimitedClick(Sender: TObject);

begin
  mSettingsTime20.Checked := False;
  mSettingsTime30.Checked := False;
  mSettingsTime60.Checked := False;
  mSettingsTime120.Checked := False;
  mSettingsTimeUnlimited.Checked := True;
  sTimeTemp := 'unlimited';
end;

{ Menu item "Help > Help": Display application help }

procedure TfDividers.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfDividers.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics game: Find numbers with a given divider.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2019.';
  MessageDlg('About "Dividers"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new number grid resp. check user answer}

procedure TfDividers.btQuestionClick(Sender: TObject);

var
  I, J: Integer;
  Ok, OkAll, IsAnswer: Boolean;

begin
  // Button "Question": Generate new number grid
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    imEval.Visible := False;
    ResetNumbers(iGridSize, aNumbers, aGridNumbers);
    SetLength(aAnswer, 0);
    // Generate divider
    repeat
      OK := True;
      iDivider := Random(iMax - 1) + 2;
      // Divider must not be one of those excluded
      for I := 0 to Length(aExclusions) - 1 do begin
        if iDivider = aExclusions[I] then
          OK := False;
      end;
    until OK;
    edDivider.Text := IntToStr(iDivider);
    if sGame <> 'simple' then begin
      // Generate addition/subtraction number
      iNumber := Random(10) + 1;
      edNumber.Text := IntToStr(iNumber);
    end;
    // Generate random numbers to fill the number grid
    for J := 1 to iGridSize do begin
      for I := 1 to iGridSize do begin
        aNumbers[J, I] := Random(iNMax - iDivider + 1) + iDivider;
        // Check if number generated is part of solution and if so, add it to the answer array
        IsAnswer := False;
        if (sGame = 'simple') and (aNumbers[J, I] mod iDivider = 0) then
          IsAnswer := True
        else if (sGame = 'addition') and ((aNumbers[J, I] + iNumber) mod iDivider = 0) then
          IsAnswer := True
        else if (sGame = 'subtraction') and ((aNumbers[J, I] - iNumber) mod iDivider = 0) then
          IsAnswer := True;
        if IsAnswer then begin
          // Add number to answer array
          SetLength(aAnswer, Length(aAnswer) + 1);
          aAnswer[Length(aAnswer) - 1] := aNumbers[J, I];
        end;
      end;
    end;
    // Create the number grid with the generated numbers
    CreateGrid(iGridSize, aGrid, aNumbers, aGridNumbers);
    if sTime <> 'unlimited' then begin
      // Start game timer (unless time is chosen 'unlimited')
      iGameTime := iTime; tiDivider.Enabled := True;
    end;
    // Next button push will be the user's answer
    btQuestion.Caption := 'Answer';
  end
  // Button "Answer": Check user answer
  else begin
    tiDivider.Enabled := False;                                                // disable the timer
    // Get user answer from number grid and store into an array
    SetLength(aUserAnswer, 0);
    for J := 1 to 8 do begin
      for I := 1 to 8 do begin
        // A yellow square means a number that has been selected by the user as part of the solution
        if aGrid[J, I].Brush.Color = clYellow then begin
          // Add number to user answer array
          SetLength(aUserAnswer, Length(aUserAnswer) + 1);
          aUserAnswer[Length(aUserAnswer) - 1] := aNumbers[J, I];
        end;
      end;
    end;
    OKAll := True;
    if Length(aAnswer) <> Length(aUserAnswer) then
      // If the 2 arrays have different length, the user has not found all numbers or has found incorrect numbers
      OkAll := False
    else begin
      // If the 2 arrays have same length, check if they have the same element values
      for I := 1 to Length(aUserAnswer) - 1 do begin
        Ok := False;
        for J := 0 to Length(aAnswer) - 1 do begin
          if aUserAnswer[I] = aAnswer[J] then
            Ok := True;
        end;
        if not Ok then
          // If one eleement differs, the solution is false
          OkAll := False;
      end;
    end;
    if OkAll then begin
      // User's answer is correct
      Inc(iCorrect);
      imEval.Picture.LoadFromFile('correct.png');
    end
    else begin
      // User's answer isn't correct
      Inc(iFalse);
      imEval.Picture.LoadFromFile('false.png');
    end;
    imEval.Visible := True;
    // Update evaluation
    sgEval.Cells[1, 0] := GridFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GridFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GridFormat(iFalse, '');
    sgEval.Cells[1, 3] := GridFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be another question
    btQuestion.Caption := 'Question';
  end;
end;

{ Methods handling click on squares 1,1 to 8,8 }

procedure TfDividers.shGrid11MouseDown(Sender: TObject);

begin
  NumberSelect(1, 1, aGrid);
end;

procedure TfDividers.shGrid12MouseDown(Sender: TObject);

begin
  NumberSelect(1, 2, aGrid);
end;

procedure TfDividers.shGrid13MouseDown(Sender: TObject);

begin
  NumberSelect(1, 3, aGrid);
end;

procedure TfDividers.shGrid14MouseDown(Sender: TObject);

begin
  NumberSelect(1, 4, aGrid);
end;

procedure TfDividers.shGrid15MouseDown(Sender: TObject);

begin
  NumberSelect(1, 5, aGrid);
end;

procedure TfDividers.shGrid16MouseDown(Sender: TObject);

begin
  NumberSelect(1, 6, aGrid);
end;

procedure TfDividers.shGrid17MouseDown(Sender: TObject);

begin
  NumberSelect(1, 7, aGrid);
end;

procedure TfDividers.shGrid18MouseDown(Sender: TObject);

begin
  NumberSelect(1, 8, aGrid);
end;

procedure TfDividers.shGrid21MouseDown(Sender: TObject);

begin
  NumberSelect(2, 1, aGrid);
end;

procedure TfDividers.shGrid22MouseDown(Sender: TObject);

begin
  NumberSelect(2, 2, aGrid);
end;

procedure TfDividers.shGrid23MouseDown(Sender: TObject);

begin
  NumberSelect(2, 3, aGrid);
end;

procedure TfDividers.shGrid24MouseDown(Sender: TObject);

begin
  NumberSelect(2, 4, aGrid);
end;

procedure TfDividers.shGrid25MouseDown(Sender: TObject);

begin
  NumberSelect(2, 5, aGrid);
end;

procedure TfDividers.shGrid26MouseDown(Sender: TObject);

begin
  NumberSelect(2, 6, aGrid);
end;

procedure TfDividers.shGrid27MouseDown(Sender: TObject);

begin
  NumberSelect(2, 7, aGrid);
end;

procedure TfDividers.shGrid28MouseDown(Sender: TObject);

begin
  NumberSelect(2, 8, aGrid);
end;

procedure TfDividers.shGrid31MouseDown(Sender: TObject);

begin
  NumberSelect(3, 1, aGrid);
end;

procedure TfDividers.shGrid32MouseDown(Sender: TObject);

begin
  NumberSelect(3, 2, aGrid);
end;

procedure TfDividers.shGrid33MouseDown(Sender: TObject);

begin
  NumberSelect(3, 3, aGrid);
end;

procedure TfDividers.shGrid34MouseDown(Sender: TObject);

begin
  NumberSelect(3, 4, aGrid);
end;

procedure TfDividers.shGrid35MouseDown(Sender: TObject);

begin
  NumberSelect(3, 5, aGrid);
end;

procedure TfDividers.shGrid36MouseDown(Sender: TObject);

begin
  NumberSelect(3, 6, aGrid);
end;

procedure TfDividers.shGrid37MouseDown(Sender: TObject);

begin
  NumberSelect(3, 7, aGrid);
end;

procedure TfDividers.shGrid38MouseDown(Sender: TObject);

begin
  NumberSelect(3, 8, aGrid);
end;

procedure TfDividers.shGrid41MouseDown(Sender: TObject);

begin
  NumberSelect(4, 1, aGrid);
end;

procedure TfDividers.shGrid42MouseDown(Sender: TObject);

begin
  NumberSelect(4, 2, aGrid);
end;

procedure TfDividers.shGrid43MouseDown(Sender: TObject);

begin
  NumberSelect(4, 3, aGrid);
end;

procedure TfDividers.shGrid44MouseDown(Sender: TObject);

begin
  NumberSelect(4, 4, aGrid);
end;

procedure TfDividers.shGrid45MouseDown(Sender: TObject);

begin
  NumberSelect(4, 5, aGrid);
end;

procedure TfDividers.shGrid46MouseDown(Sender: TObject);

begin
  NumberSelect(4, 6, aGrid);
end;

procedure TfDividers.shGrid47MouseDown(Sender: TObject);

begin
  NumberSelect(4, 7, aGrid);
end;

procedure TfDividers.shGrid48MouseDown(Sender: TObject);

begin
  NumberSelect(4, 8, aGrid);
end;

procedure TfDividers.shGrid51MouseDown(Sender: TObject);

begin
  NumberSelect(5, 1, aGrid);
end;

procedure TfDividers.shGrid52MouseDown(Sender: TObject);

begin
  NumberSelect(5, 2, aGrid);
end;

procedure TfDividers.shGrid53MouseDown(Sender: TObject);

begin
  NumberSelect(5, 3, aGrid);
end;

procedure TfDividers.shGrid54MouseDown(Sender: TObject);

begin
  NumberSelect(5, 4, aGrid);
end;

procedure TfDividers.shGrid55MouseDown(Sender: TObject);

begin
  NumberSelect(5, 5, aGrid);
end;

procedure TfDividers.shGrid56MouseDown(Sender: TObject);

begin
  NumberSelect(5, 6, aGrid);
end;

procedure TfDividers.shGrid57MouseDown(Sender: TObject);

begin
  NumberSelect(5, 7, aGrid);
end;

procedure TfDividers.shGrid58MouseDown(Sender: TObject);

begin
  NumberSelect(5, 8, aGrid);
end;

procedure TfDividers.shGrid61MouseDown(Sender: TObject);

begin
  NumberSelect(6, 1, aGrid);
end;

procedure TfDividers.shGrid62MouseDown(Sender: TObject);

begin
  NumberSelect(6, 2, aGrid);
end;

procedure TfDividers.shGrid63MouseDown(Sender: TObject);

begin
  NumberSelect(6, 3, aGrid);
end;

procedure TfDividers.shGrid64MouseDown(Sender: TObject);

begin
  NumberSelect(6, 4, aGrid);
end;

procedure TfDividers.shGrid65MouseDown(Sender: TObject);

begin
  NumberSelect(6, 5, aGrid);
end;

procedure TfDividers.shGrid66MouseDown(Sender: TObject);

begin
  NumberSelect(6, 6, aGrid);
end;

procedure TfDividers.shGrid67MouseDown(Sender: TObject);

begin
  NumberSelect(6, 7, aGrid);
end;

procedure TfDividers.shGrid68MouseDown(Sender: TObject);

begin
  NumberSelect(6, 8, aGrid);
end;

procedure TfDividers.shGrid71MouseDown(Sender: TObject);

begin
  NumberSelect(7, 1, aGrid);
end;

procedure TfDividers.shGrid72MouseDown(Sender: TObject);

begin
  NumberSelect(7, 2, aGrid);
end;

procedure TfDividers.shGrid73MouseDown(Sender: TObject);

begin
  NumberSelect(7, 3, aGrid);
end;

procedure TfDividers.shGrid74MouseDown(Sender: TObject);

begin
  NumberSelect(7, 4, aGrid);
end;

procedure TfDividers.shGrid75MouseDown(Sender: TObject);

begin
  NumberSelect(7, 5, aGrid);
end;

procedure TfDividers.shGrid76MouseDown(Sender: TObject);

begin
  NumberSelect(7, 6, aGrid);
end;

procedure TfDividers.shGrid77MouseDown(Sender: TObject);

begin
  NumberSelect(7, 7, aGrid);
end;

procedure TfDividers.shGrid78MouseDown(Sender: TObject);

begin
  NumberSelect(7, 8, aGrid);
end;

procedure TfDividers.shGrid81MouseDown(Sender: TObject);

begin
  NumberSelect(8, 1, aGrid);
end;

procedure TfDividers.shGrid82MouseDown(Sender: TObject);

begin
  NumberSelect(8, 2, aGrid);
end;

procedure TfDividers.shGrid83MouseDown(Sender: TObject);

begin
  NumberSelect(8, 3, aGrid);
end;

procedure TfDividers.shGrid84MouseDown(Sender: TObject);

begin
  NumberSelect(8, 4, aGrid);
end;

procedure TfDividers.shGrid85MouseDown(Sender: TObject);

begin
  NumberSelect(8, 5, aGrid);
end;

procedure TfDividers.shGrid86MouseDown(Sender: TObject);

begin
  NumberSelect(8, 6, aGrid);
end;

procedure TfDividers.shGrid87MouseDown(Sender: TObject);

begin
  NumberSelect(8, 7, aGrid);
end;

procedure TfDividers.shGrid88MouseDown(Sender: TObject);

begin
  NumberSelect(8, 8, aGrid);
end;

{ Game timer routine }

procedure TfDividers.tiDividerTimer(Sender: TObject);

begin
  Dec(iGameTime);                                                              // decrease time value by 1 sec
  edTime.Text := IntToStr(iGameTime) + ' sec';
  // Time over
  if iGameTime = 0 then begin
    // Stop timer
    tiDivider.Enabled := False;
    // Display message
    MessageDlg('Time over', 'Your answer time of ' + sTime + ' is over!', mtError, [mbOK], 0);
    // Update evaluation
    Inc(iFalse);
    sgEval.Cells[1, 0] := GridFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GridFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GridFormat(iFalse, '');
    sgEval.Cells[1, 3] := GridFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be another question
    btQuestion.Caption := 'Question';
  end;
end;

end.

