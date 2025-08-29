{******************************************}
{* Main unit for FiveDiceGrid application *}
{******************************************}

unit fivedice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, help, scores;

type
  TGrid = array[0..7, 0..9] of Integer;
  TTaken = array[0..4] of record
    Row, Col, Value: Integer;
  end;
  TGridImages = array[0..7, 0..9] of TImage;
  TSelectShapes = array[0..4, 0..3] of TShape;
  {************}
  { TfFiveDice }
  {************}
  TfFiveDice = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameReset, mGameExit: TMenuItem;
    mOptions, mOptionsRemove: TMenuItem;
    mHelp, mHelpHelp, mHelpScores, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edScore, edScoreHigh, edScoring: TEdit;
    Shape1: TShape;
    imDice1, imDice10, imDice11, imDice12, imDice13, imDice14, imDice15, imDice16, imDice17, imDice18: TImage;
    imDice19, imDice2, imDice20, imDice21, imDice22, imDice23, imDice24, imDice25, imDice26, imDice27: TImage;
    imDice28, imDice29, imDice3, imDice30, imDice31, imDice32, imDice33, imDice34, imDice35, imDice36: TImage;
    imDice37, imDice38, imDice39, imDice4, imDice40, imDice41, imDice42, imDice43, imDice44, imDice45: TImage;
    imDice46, imDice47, imDice48, imDice49, imDice5, imDice50, imDice51, imDice52, imDice53, imDice54: TImage;
    imDice55, imDice56, imDice57, imDice58, imDice59, imDice6, imDice60, imDice61, imDice62, imDice63: TImage;
    imDice64, imDice65, imDice66, imDice67, imDice68, imDice69, imDice7, imDice70, imDice71, imDice72: TImage;
    imDice73, imDice74, imDice75, imDice76, imDice77, imDice78, imDice79, imDice8, imDice80, imDice9: TImage;
    btAction: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameResetClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsRemoveClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpScoresClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure imDice10Click(Sender: TObject);
    procedure imDice11Click(Sender: TObject);
    procedure imDice12Click(Sender: TObject);
    procedure imDice13Click(Sender: TObject);
    procedure imDice14Click(Sender: TObject);
    procedure imDice15Click(Sender: TObject);
    procedure imDice16Click(Sender: TObject);
    procedure imDice17Click(Sender: TObject);
    procedure imDice18Click(Sender: TObject);
    procedure imDice19Click(Sender: TObject);
    procedure imDice1Click(Sender: TObject);
    procedure imDice20Click(Sender: TObject);
    procedure imDice21Click(Sender: TObject);
    procedure imDice22Click(Sender: TObject);
    procedure imDice23Click(Sender: TObject);
    procedure imDice24Click(Sender: TObject);
    procedure imDice25Click(Sender: TObject);
    procedure imDice26Click(Sender: TObject);
    procedure imDice27Click(Sender: TObject);
    procedure imDice28Click(Sender: TObject);
    procedure imDice29Click(Sender: TObject);
    procedure imDice2Click(Sender: TObject);
    procedure imDice30Click(Sender: TObject);
    procedure imDice31Click(Sender: TObject);
    procedure imDice32Click(Sender: TObject);
    procedure imDice33Click(Sender: TObject);
    procedure imDice34Click(Sender: TObject);
    procedure imDice35Click(Sender: TObject);
    procedure imDice36Click(Sender: TObject);
    procedure imDice37Click(Sender: TObject);
    procedure imDice38Click(Sender: TObject);
    procedure imDice39Click(Sender: TObject);
    procedure imDice3Click(Sender: TObject);
    procedure imDice40Click(Sender: TObject);
    procedure imDice41Click(Sender: TObject);
    procedure imDice42Click(Sender: TObject);
    procedure imDice43Click(Sender: TObject);
    procedure imDice44Click(Sender: TObject);
    procedure imDice45Click(Sender: TObject);
    procedure imDice46Click(Sender: TObject);
    procedure imDice47Click(Sender: TObject);
    procedure imDice48Click(Sender: TObject);
    procedure imDice49Click(Sender: TObject);
    procedure imDice4Click(Sender: TObject);
    procedure imDice50Click(Sender: TObject);
    procedure imDice51Click(Sender: TObject);
    procedure imDice52Click(Sender: TObject);
    procedure imDice53Click(Sender: TObject);
    procedure imDice54Click(Sender: TObject);
    procedure imDice55Click(Sender: TObject);
    procedure imDice56Click(Sender: TObject);
    procedure imDice57Click(Sender: TObject);
    procedure imDice58Click(Sender: TObject);
    procedure imDice59Click(Sender: TObject);
    procedure imDice5Click(Sender: TObject);
    procedure imDice60Click(Sender: TObject);
    procedure imDice61Click(Sender: TObject);
    procedure imDice62Click(Sender: TObject);
    procedure imDice63Click(Sender: TObject);
    procedure imDice64Click(Sender: TObject);
    procedure imDice65Click(Sender: TObject);
    procedure imDice66Click(Sender: TObject);
    procedure imDice67Click(Sender: TObject);
    procedure imDice68Click(Sender: TObject);
    procedure imDice69Click(Sender: TObject);
    procedure imDice6Click(Sender: TObject);
    procedure imDice70Click(Sender: TObject);
    procedure imDice71Click(Sender: TObject);
    procedure imDice72Click(Sender: TObject);
    procedure imDice73Click(Sender: TObject);
    procedure imDice74Click(Sender: TObject);
    procedure imDice75Click(Sender: TObject);
    procedure imDice76Click(Sender: TObject);
    procedure imDice77Click(Sender: TObject);
    procedure imDice78Click(Sender: TObject);
    procedure imDice79Click(Sender: TObject);
    procedure imDice7Click(Sender: TObject);
    procedure imDice80Click(Sender: TObject);
    procedure imDice8Click(Sender: TObject);
    procedure imDice9Click(Sender: TObject);
    procedure btActionClick(Sender: TObject);
  private
    iScore, iScoreHigh, iTaken: Integer;
    bStart: Boolean;
    aScores: array[0..7] of Integer;
    aCategories: array[0..7] of string;
    aGrid, aGridSave, aGridTaken: TGrid;
    aTaken: TTaken;
    imGrid: TGridImages;
    shSelect: TSelectShapes;
  end;

var
  fFiveDice: TfFiveDice;

implementation

{$R *.lfm}

{ Read scores data from text file }

procedure ReadScores(var Categories: array of string; var Scores: array of Integer; out Mess: string);

// There is no validation of the file data done. Thus, if the user messes up the file when changing
// the scoring values, the application will (mostly) abort with a runtime error or untreated exception

var
  N: Integer;
  S, Score: string;
  InFile: Text;

begin
  Mess := '';
  Assign(InFile, 'Scores.txt'); Reset(Infile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, S); S := Trim(S);
    if S <> '' then begin
      Inc(N);
      Categories[N - 1] := Trim(Copy(S, 1, 19));                               // category (combination) name
      Score := Trim(Copy(S, 20, 3));
      Scores[N - 1] := StrToInt(Score);                                        // category scoring value
      if Scores[N - 1] < 10 then
        Score := '  ' + Score
      else if Scores[N - 1] < 100 then
        Score := ' ' + Score;
      Score := ' ' + Score;
      fScores.stScores.Cells[1, N] := Score;                                   // fill in table value on fScores form
    end;
  end;
  Close(InFile);
  if N <> 8 then
    Mess := 'Missing score category!';
end;

{ Load dice pictures (as defined by array elements = dice values) }

procedure GridLoad(var GridImages: TGridImages; var Grid: TGrid);

var
  I, J: Integer;
  DiceImg: string;

begin
  for I := 0 to 7 do begin
    for J := 0 to 9 do begin
      DiceImg := GetCurrentDir + '/dices/' + 'dice' + IntToStr(Grid[I, J]) + '.jpg'; DoDirSeparators(DiceImg);
      GridImages[I, J].Picture.LoadFromFile(DiceImg);
      GridImages[I, J].Visible := True;
    end;
  end;
end;

{ Generate a random grid (random dice values) }

procedure GridGenerate(var GridImages: TGridImages; var Grid: TGrid);

var
  I, J: Integer;

begin
  for I := 0 to 7 do begin
    for J := 0 to 9 do
      Grid[I, J] := Random(6) + 1;
  end;
  GridLoad(GridImages, Grid);                                                  // Display the dice pictures
end;

{ Reset the user-takes-values arrays (for next turn) }

procedure TakenReset(var Count: Integer; var Taken: TTaken; var GridTaken: TGrid; var SelectShapes: TSelectShapes);

var
  I, J: Integer;

begin
  for I := 0 to 7 do begin
    for J := 0 to 9 do begin
      GridTaken[I, J] := 0;
    end;
  end;
  for I := 0 to 4 do begin
    Taken[I].Row := -1; Taken[I].Col := -1;
    Taken[I].Value := 0;
  end;
  Count := 0;
  // Clear all selection markers
  for I := 0 to 4 do begin
    for J := 0 to 3 do
      SelectShapes[I, J].Visible := False;
  end;
end;

{ Select resp. unselect the dice clicked onto by the user }

procedure GridSelect(var SelectShapes: TSelectShapes;
  Row, Col: Integer; var Grid, GridTaken: TGrid; var Taken: TTaken; var Count: Integer);

var
  I, TX: Integer;
  Mess: string;

begin
  Mess := '';
  if GridTaken[Row, Col] = 0 then begin
    // Dice is not yet selected: Select it (unless there are already 5 dices selected)
    if Count < 5 then begin
      // Save dice value into array element
      GridTaken[Row, Col] := Grid[Row, Col]; Inc(Count);
      // Find a not yet used element in the selection-shapes array
      TX := 0;
      while Taken[TX].Value <> 0 do
        Inc(TX);
      // Fill in (record) values of the used selection-shapes array element
      Taken[TX].Row := Row; Taken[TX].Col := Col; Taken[TX].Value := GridTaken[Row, Col];
      // Move the selection-shapes around the dice clicked onto and make them visible
      for I := 0 to 3 do begin
        if I = 3 then
          SelectShapes[TX, I].Left := 20 + (Col + 1) * 52
        else
          SelectShapes[TX, I].Left := 24 + Col * 52;
        if I = 1 then
          SelectShapes[TX, I].Top := 60 + (Row + 1) * 52
        else
          SelectShapes[TX, I].Top := 64 + Row * 52;
        SelectShapes[TX, I].Visible := True;
      end;
    end
    else
      Mess := 'You cannot take more than 5 dices!';
  end
  // Dice has already been selected: Unselect it
  else begin
    // Reset array element
    GridTaken[Row, Col] := 0; Dec(Count);
    // Find selection-shapes array element that was used to mark this dice
    for I := 0 to 4 do begin
      if (Taken[I].Row = Row) and (Taken[I].Col = Col) then
        TX := I;
    end;
    // Reset selection-shapes array element used and make marker shapes invisible
    Taken[TX].Row := -1; Taken[TX].Col := -1; Taken[TX].Value := 0;
    for I := 0 to 3 do
      SelectShapes[TX, I].Visible := False;
  end;
  if Mess <> '' then
    MessageDlg('Invalid selection', Mess, mtError, [mbOK], 0);
end;

{ Sort array elements }

procedure ArraySort(var Arr: array of Integer);

var
  I, J, Temp: Integer;

begin
  for I := 0 to Length(Arr) - 2 do begin
    for J := I + 1 to Length(Arr) - 1 do begin
      if Arr[I] > Arr[J] then begin
        Temp := Arr[I]; Arr[I] := Arr[J]; Arr[J] := Temp;
      end;
    end;
  end;
end;

{ Check if the dice combination is N-of-a-kind }

function IsNofaKind(N: Integer; Dices: array of Integer): Boolean;

var
  Dice, I: Integer;
  ItIs: Boolean;

begin
  ItIs := True;
  Dice := Dices[0];
  for I := 1 to N - 1 do begin
    if Dices[I] <> Dice then
      ItIs := False;
  end;
  Result := ItIs;
end;

{ Check if the dice combination is two pairs }

function IsTwoPairs(Dices: array of Integer): Boolean;

var
  Count, I: Integer;
  Counts: array[1..6] of Integer;
  IsTwo1, IsTwo2: Boolean;

begin
  for I := 1 to 6 do
    Counts[I] := 0;
  for I := 0 to 4 do
    Inc(Counts[Dices[I]]);
  IsTwo1 := False; IsTwo2 := False; Count := 0;
  for I := 1 to 6 do begin
    if Count = 0 then begin
      if Counts[I] = 2 then begin
        IsTwo1 := True;
        Inc(Count);
      end;
    end
    else if Count = 1 then begin
      if Counts[I] = 2 then
        IsTwo2 := True;
    end;
  end;
  Result := IsTwo1 and IsTwo2;
end;

{ Check if the dice combination is a full house }

function IsFullHouse(Dices: array of Integer): Boolean;

var
  I: Integer;
  Counts: array[1..6] of Integer;
  IsTwo, IsThree: Boolean;

begin
  for I := 1 to 6 do
    Counts[I] := 0;
  for I := 0 to 4 do
    Inc(Counts[Dices[I]]);
  IsTwo := False; IsThree := False;
  for I := 1 to 6 do begin
    if Counts[I] = 3 then
      IsThree := True
    else if Counts[I] = 2 then
      IsTwo := True;
  end;
  Result := IsThree and IsTwo;
end;

{ Check if the dice combination is a (small or large) straight }

function IsStraight(StraightKind: string; Dices: array of Integer): Boolean;

var
  N, I: Integer;
  SDices: string;
  ItIs: Boolean;

begin
  ItIs := False;
  ArraySort(Dices);
  if StraightKind = 'small' then
    N := 4
  else
    N := 5;
  SDices := '';
  for I := 5 - N + 1 to 5 do begin
    SDices += IntToStr(Dices[I - 1]);
  end;
  // Check for small straight
  if StraightKind = 'small' then begin
    if (SDices = '1234') or (SDices = '2345') or (SDices = '3456') then
      ItIs := True;
  end
  // Check for large straight
  else begin
    if (SDices = '12345') or (SDices = '23456') then
      ItIs := True
  end;
  Result := ItIs;
end;

{************}
{ TfFiveDice }
{************}

{ Application start: Initialization }

procedure TfFiveDice.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  // Create two-dimensional array with dice images
  imGrid[0, 0] := imDice1;  imGrid[0, 1] := imDice2;  imGrid[0, 2] := imDice3;  imGrid[0, 3] := imDice4;  imGrid[0, 4] := imDice5;
  imGrid[0, 5] := imDice6;  imGrid[0, 6] := imDice7;  imGrid[0, 7] := imDice8;  imGrid[0, 8] := imDice9;  imGrid[0, 9] := imDice10;
  imGrid[1, 0] := imDice11; imGrid[1, 1] := imDice12; imGrid[1, 2] := imDice13; imGrid[1, 3] := imDice14; imGrid[1, 4] := imDice15;
  imGrid[1, 5] := imDice16; imGrid[1, 6] := imDice17; imGrid[1, 7] := imDice18; imGrid[1, 8] := imDice19; imGrid[1, 9] := imDice20;
  imGrid[2, 0] := imDice21; imGrid[2, 1] := imDice22; imGrid[2, 2] := imDice23; imGrid[2, 3] := imDice24; imGrid[2, 4] := imDice25;
  imGrid[2, 5] := imDice26; imGrid[2, 6] := imDice27; imGrid[2, 7] := imDice28; imGrid[2, 8] := imDice29; imGrid[2, 9] := imDice30;
  imGrid[3, 0] := imDice31; imGrid[3, 1] := imDice32; imGrid[3, 2] := imDice33; imGrid[3, 3] := imDice34; imGrid[3, 4] := imDice35;
  imGrid[3, 5] := imDice36; imGrid[3, 6] := imDice37; imGrid[3, 7] := imDice38; imGrid[3, 8] := imDice39; imGrid[3, 9] := imDice40;
  imGrid[4, 0] := imDice41; imGrid[4, 1] := imDice42; imGrid[4, 2] := imDice43; imGrid[4, 3] := imDice44; imGrid[4, 4] := imDice45;
  imGrid[4, 5] := imDice46; imGrid[4, 6] := imDice47; imGrid[4, 7] := imDice48; imGrid[4, 8] := imDice49; imGrid[4, 9] := imDice50;
  imGrid[5, 0] := imDice51; imGrid[5, 1] := imDice52; imGrid[5, 2] := imDice53; imGrid[5, 3] := imDice54; imGrid[5, 4] := imDice55;
  imGrid[5, 5] := imDice56; imGrid[5, 6] := imDice57; imGrid[5, 7] := imDice58; imGrid[5, 8] := imDice59; imGrid[5, 9] := imDice60;
  imGrid[6, 0] := imDice61; imGrid[6, 1] := imDice62; imGrid[6, 2] := imDice63; imGrid[6, 3] := imDice64; imGrid[6, 4] := imDice65;
  imGrid[6, 5] := imDice66; imGrid[6, 6] := imDice67; imGrid[6, 7] := imDice68; imGrid[6, 8] := imDice69; imGrid[6, 9] := imDice70;
  imGrid[7, 0] := imDice71; imGrid[7, 1] := imDice72; imGrid[7, 2] := imDice73; imGrid[7, 3] := imDice74; imGrid[7, 4] := imDice75;
  imGrid[7, 5] := imDice76; imGrid[7, 6] := imDice77; imGrid[7, 7] := imDice78; imGrid[7, 8] := imDice79; imGrid[7, 9] := imDice80;
  // Create the shape objects that will be used to mark the dices selected by the user
  for I := 0 to 4 do begin
    for J := 0 to 3 do begin
      shSelect[I, J] := TShape.Create(shSelect[I, J]);
      shSelect[I, J].Parent := Self;
      shSelect[I, J].Shape := stRectangle;
      if (J = 0) or (J = 1) then begin
        shSelect[I, J].Width := 50;
        shSelect[I, J].Height := 2;
      end
      else begin
        shSelect[I, J].Width := 2;
        shSelect[I, J].Height := 50;
      end;
      shSelect[I, J].Brush.Color := clBlack;
      shSelect[I, J].Visible := False;
    end;
  end;
  bStart := True;                                                              // flag used in the FormActivate routine to identify application startup
end;

procedure TfFiveDice.FormActivate(Sender: TObject);

var
  Mess: string;

begin
  if bStart = True then begin
    bStart := False;
    ReadScores(aCategories, aScores, Mess);                                    // read scoring values from text file
    if Mess = '' then begin
      Randomize;
      mGameNew.Click;                                                          // start a new game
    end
    else begin
      MessageDlg('File error', Mess, mtError, [mbOK], 0);
      Halt;
    end;
  end;
end;

{ Menu item "Game > New": Start a new game with newly generated dice values }

procedure TfFiveDice.mGameNewClick(Sender: TObject);

begin
  GridGenerate(imGrid, aGrid);                                                 // generate the dice values
  aGridSave := aGrid;                                                          // save the grid (for "Reset")
  // Clear form fields and reset variables
  TakenReset(iTaken, aTaken, aGridTaken, shSelect);
  iScore := 0; iScoreHigh := 0;
  edScore.Text := '0';
  edScoreHigh.Visible := False;                                                // do not display the highscore field
  edScoring.Text := '';
end;

{ Menu item "Game > Reset": Restart the game with same dice values as before }

procedure TfFiveDice.mGameResetClick(Sender: TObject);

begin
  aGrid := aGridSave;                                                          // restore dice values from before
  GridLoad(imGrid, aGrid);                                                     // display the dice pictures

  // Update and display the highscore for this game (= the actual dice values)
  if iScore > iScoreHigh then
    iScoreHigh := iScore;
  edScoreHigh.Text := IntToStr(iScoreHigh);
  edScoreHigh.Visible := True;
  // Clear form fields and reset variables
  TakenReset(iTaken, aTaken, aGridTaken, shSelect);
  iScore := 0;
  edScore.Text := '0';
  edScoring.Text := '';
end;

{ Menu item "Game > Exit": Exit application }

procedure TfFiveDice.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Remove invalid combinations": Toggle 'remove invalid combinations' and error message}

procedure TfFiveDice.mOptionsRemoveClick(Sender: TObject);

begin
  if mOptionsRemove.Checked then
    mOptionsRemove.Checked := False
  else
    mOptionsRemove.Checked := True;
end;

{ Menu item "Help > Help": Display application help }

procedure TfFiveDice.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > Scores": Display scoring table }

procedure TfFiveDice.mHelpScoresClick(Sender: TObject);

begin
  if fScores.Visible then
    fScores.Close
  else
    fScores.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfFiveDice.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Yahtzee-based dice game for 1 player.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, May 2023.';
  MessageDlg('About "FiveDiceGrid"', S, mtInformation, [mbOK], 0);
end;

{ Button "Done": Determine combination, apply score, remove dices }

procedure TfFiveDice.btActionClick(Sender: TObject);

var
  Count, Counts1, SX, R, C, I: Integer;
  Mess: string;
  Invalid, Counts4: Boolean;
  Dices, Counts: array[0..4] of Integer;

begin
  Mess := '';
  // Check if the dices selected by the user are adjacent (touch each other horizontally or vertically)
  // As I did not find a generic algorithm for this, I used the following rules: The 5 fields are adjacent
  //   1. if none of them is without a neighbor
  //   2. if the number of those, who have 1 single neighbor is less than or equal to 3 OR
  //   3. if the number of those, who have 1 single neighbor is equal to 4, and the resting field has 4 neighbors
  if iTaken >= 2 then begin
    // Count each field's neighbors
    for I := 0 to iTaken - 1 do begin
      Count := 0;
      for R := aTaken[I].Row - 1 to aTaken[I].Row + 1 do begin
        for C := aTaken[I].Col - 1 to aTaken[I].Col + 1 do begin
          if (R >= 0) and (R <= 7) and (C >= 0) and (C <= 9) and ((R = aTaken[I].Row) or (C = aTaken[I].Col)) then begin
            if aGridTaken[R, C] <> 0 then
              Inc(Count);
          end;
        end;
      end;
      Counts[I] := Count;
    end;
    Counts1 := 0; Counts4 := False; Invalid := False;
    for I := 0 to iTaken - 1 do begin
      if Counts[I] = 1 then begin
        // Field without neighbor: Fields are not adjacent
        Invalid := True;
      end
      else if Counts[I] = 2 then begin
        // Count fields with 1 neighbor
        Inc(Counts1);
      end
      else if Counts[I] = 5 then begin
        // Set flag if there is a field with 4 neighbors
        Counts4 := True;
      end;
    end;
    if not Invalid then begin
      // Fields are not adjacent if there are more than 3 of them with a single neighbor...
      if Counts1 > 3 then begin
        // ...except if the remaining field has 4 neighbors
        if not Counts4 then
          Invalid := True;
      end;
    end;
    if Invalid then begin
      // User selected dices that are not touching each other
      Mess := 'The dices have to be adjacent to each other!';
    end;
  end
  else
    Mess := 'You must take at least 2 dices!';
  // User dice selection is valid (dices are adjecent)
  if Mess = '' then begin
    // Determine the combination given by the dices selected by checking,
    // for a given number of dices taken, if this is a valid combination
    SX := -1;
    for I := 0 to 4 do
      Dices[I] := aTaken[I].Value;
    case iTaken of
      5: begin
        if IsFullHouse(Dices) then
          SX := 3
        else if IsStraight('large', Dices) then
          SX := 5
        else if IsNofaKind(5, Dices) then
          SX := 7;
      end;
      4: begin
        if IsTwoPairs(Dices) then
          SX := 1
        else if IsStraight('small', Dices) then
          SX := 4
        else if IsNofaKind(4, Dices) then
          SX := 6;
      end;
      3: begin
        if IsNofaKind(3, Dices) then
          SX := 2;
      end;
      2: begin
        if IsNofaKind(2, Dices) then
          SX := 0;
      end;
    end;
    // Dice selection does not correspond to any valid combination
    if SX = -1 then begin
      if mOptionsRemove.Checked then
        edScoring.Text := 'Your selection isn''t a valid combination'
      else
        Mess := 'Your selection isn''t a valid combination!';
    end
    // Dice selection is a valid combination: Update total score for this game
    else begin
      edScoring.Text := aCategories[SX] + ': ' + IntToStr(aScores[SX]);
      iScore += aScores[SX]; edScore.Text := IntToStr(iScore);
    end;
    // Remove the dices taken from the grid
    if (SX <> -1) or mOptionsRemove.Checked then begin
      for I := 0 to 4 do begin
        if aTaken[I].Value <> 0 then begin
          imGrid[aTaken[I].Row, aTaken[I].Col].Visible := False;
          aGrid[aTaken[I].Row, aTaken[I].Col] := -1;
        end;
      end;
      TakenReset(iTaken, aTaken, aGridTaken, shSelect);
    end;
  end;
  if Mess <> '' then
    MessageDlg('User error', Mess, mtError, [mbOK], 0);
end;

{ Mouse-click onto a dice: Select resp. unselect it }

procedure TfFiveDice.imDice1Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice2Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice3Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice4Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice5Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice6Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice7Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice8Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice9Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice10Click(Sender: TObject);

begin
  GridSelect(shSelect, 0, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice11Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice12Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice13Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice14Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice15Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice16Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice17Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice18Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice19Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice20Click(Sender: TObject);

begin
  GridSelect(shSelect, 1, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice21Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice22Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice23Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice24Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice25Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice26Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice27Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice28Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice29Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice30Click(Sender: TObject);

begin
  GridSelect(shSelect, 2, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice31Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice32Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice33Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice34Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice35Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice36Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice37Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice38Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice39Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice40Click(Sender: TObject);

begin
  GridSelect(shSelect, 3, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice41Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice42Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice43Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice44Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice45Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice46Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice47Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice48Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice49Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice50Click(Sender: TObject);

begin
  GridSelect(shSelect, 4, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice51Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice52Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice53Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice54Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice55Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice56Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice57Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice58Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice59Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice60Click(Sender: TObject);

begin
  GridSelect(shSelect, 5, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice61Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice62Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice63Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice64Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice65Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice66Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice67Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice68Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice69Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice70Click(Sender: TObject);

begin
  GridSelect(shSelect, 6, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice71Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 0, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice72Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 1, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice73Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 2, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice74Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 3, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice75Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 4, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice76Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 5, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice77Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 6, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice78Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 7, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice79Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 8, aGrid, aGridTaken, aTaken, iTaken);
end;

procedure TfFiveDice.imDice80Click(Sender: TObject);

begin
  GridSelect(shSelect, 7, 9, aGrid, aGridTaken, aTaken, iTaken);
end;

end.

