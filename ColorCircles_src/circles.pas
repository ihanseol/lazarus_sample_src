{******************************************}
{* Main unit for ColorCircles application *}
{******************************************}

unit circles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, help;

type
  TGrid = array[1..8, 1..8] of TShape;
  {****************}
  { TfColorCircles }
  {****************}
  TfColorCircles = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsCircles, mSettingsCircles4, mSettingsCircles5, mSettingsCircles6, mSettingsCircles8: TMenuItem;
    mSettingsColors, mSettingsColors3, mSettingsColors4, mSettingsColors5, mSettingsColors6: TMenuItem;
    mSettingsColorsEasy: TMenuItem;
    MenuItem1: TMenuItem;
    mSettingsHighScores, mSettingsTScoring: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    Shape1: TShape;
    shCircle101, shCircle102, shCircle103, shCircle104, shCircle105: TShape;
    shCircle106, shCircle107, shCircle108, shCircle109, shCircle110: TShape;
    shCircle111, shCircle112, shCircle113, shCircle114, shCircle115: TShape;
    shCircle116, shCircle117, shCircle118, shCircle119, shCircle120: TShape;
    shCircle121, shCircle122, shCircle123, shCircle124, shCircle125: TShape;
    Shape2: TShape;
    shCircle201, shCircle202, shCircle203, shCircle204, shCircle205: TShape;
    shCircle206, shCircle207, shCircle208, shCircle209, shCircle210: TShape;
    shCircle211, shCircle212, shCircle213, shCircle214, shCircle215: TShape;
    shCircle216, shCircle217, shCircle218, shCircle219, shCircle220: TShape;
    shCircle221, shCircle222, shCircle223, shCircle224, shCircle225: TShape;
    Label1, Label3: TLabel;
    laTime2, laScore2: TLabel;
    edName1, edName2: TEdit;
    edTime1, edTime2: TEdit;
    edScore1, edScore2: TEdit;
    Memo1: TMemo;
    btStart: TButton;
    tiColorCircles: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsCircles4Click(Sender: TObject);
    procedure mSettingsCircles5Click(Sender: TObject);
    procedure mSettingsCircles6Click(Sender: TObject);
    procedure mSettingsCircles8Click(Sender: TObject);
    procedure mSettingsColors3Click(Sender: TObject);
    procedure mSettingsColors4Click(Sender: TObject);
    procedure mSettingsColors5Click(Sender: TObject);
    procedure mSettingsColors6Click(Sender: TObject);
    procedure mSettingsColorsEasyClick(Sender: TObject);
    procedure mSettingsHighScoresClick(Sender: TObject);
    procedure mSettingsTScoringClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure shCircle201MouseDown(Sender: TObject);
    procedure shCircle202MouseDown(Sender: TObject);
    procedure shCircle203MouseDown(Sender: TObject);
    procedure shCircle204MouseDown(Sender: TObject);
    procedure shCircle205MouseDown(Sender: TObject);
    procedure shCircle206MouseDown(Sender: TObject);
    procedure shCircle207MouseDown(Sender: TObject);
    procedure shCircle208MouseDown(Sender: TObject);
    procedure shCircle209MouseDown(Sender: TObject);
    procedure shCircle210MouseDown(Sender: TObject);
    procedure shCircle211MouseDown(Sender: TObject);
    procedure shCircle212MouseDown(Sender: TObject);
    procedure shCircle213MouseDown(Sender: TObject);
    procedure shCircle214MouseDown(Sender: TObject);
    procedure shCircle215MouseDown(Sender: TObject);
    procedure shCircle216MouseDown(Sender: TObject);
    procedure shCircle217MouseDown(Sender: TObject);
    procedure shCircle218MouseDown(Sender: TObject);
    procedure shCircle219MouseDown(Sender: TObject);
    procedure shCircle220MouseDown(Sender: TObject);
    procedure shCircle221MouseDown(Sender: TObject);
    procedure shCircle222MouseDown(Sender: TObject);
    procedure shCircle223MouseDown(Sender: TObject);
    procedure shCircle224MouseDown(Sender: TObject);
    procedure shCircle225MouseDown(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tiColorCirclesTimer(Sender: TObject);
  private
    iPlayers, iCircles, iColors, iRow, iColumn, iTurn, iScore1, iScore2: Integer;
    rTime1, rTime2, rPenality: Real;
    aCircles1, aCircles2: TGrid;
  end;

const
  Colors: array[0..3, 1..6] of TColor = (
    ($0000FF, $FF00000, $00FF00, $00FFFF, $007DFA, $C800C8),
    ($0000FF, $00000EE, $0000CD, $00008B, $0045FF, $0041EE),
    ($FF0000, $EE00000, $CD0000, $8B0000, $FFBF00, $FF901E),
    ($00FF00, $000EE00, $00CD00, $008B00, $7FFF00, $76EE00)
  );

var
  fColorCircles: TfColorCircles;

implementation

{$R *.lfm}

{ Prepare for a new game }

procedure NewGame(Players, Circles: Integer; out Score1, Score2: Integer; Grid1, Grid2: TGrid);

var
  I, J, CSize: Integer;

begin
  // Be sure the timer is disabled...
  fColorCircles.tiColorCircles.Enabled := False;
  // Set last row/coulmn circles visible or not (depending on size of grid)
  if Circles = 4 then begin
    for I := 1 to 5 do begin
      Grid1[I, 5].Visible := False;
      Grid2[I, 5].Visible := False;
    end;
    for I := 1 to 5 do begin
      Grid1[5, I].Visible := False;
      Grid2[5, I].Visible := False;
    end;
    CSize := 120;                                                              // circle size for 4x4 grids
  end
  else begin
    for J := 1 to 5 do begin
      Grid1[J, 5].Visible := True;
      Grid2[J, 5].Visible := True;
    end;
    for I := 1 to 5 do begin
      Grid1[5, I].Visible := True;
      Grid2[5, I].Visible := True;
    end;
    CSize := 96;                                                               // circle size for 5x5 grids
  end;
  // Depending on grid, set circle size and correct grid position ('Left' and 'Top' properties)
  for J := 1 to Circles do begin
    for I := 1 to Circles do begin
      Grid1[J, I].Width := CSize; Grid1[J, I].Height := CSize;
      Grid1[J, I].Left := 10 + (I - 1) * CSize;
      Grid1[J, I].Top := 10 + (J - 1) * CSize;
      Grid1[J, I].Brush.Color := clRed;
      Grid2[J, I].Width := CSize; Grid2[J, I].Height := CSize;
      Grid2[J, I].Left := 560 + (I - 1) * CSize;
      Grid2[J, I].Top := 10 + (J - 1) * CSize;
      Grid2[J, I].Brush.Color := clRed;
    end;
  end;
  // Reset time and scoring counters
  fColorCircles.edTime1.Text := '0:00,00'; fColorCircles.edScore1.Text := '0';
  if Players = 1 then begin
    // 1-player mode: Clear all player 2 related controls
    fColorCircles.edName2.Text := ''; fColorCircles.edName2.Enabled := False;
    fColorCircles.edTime2.Text := ''; fColorCircles.laTime2.Enabled := False; fColorCircles.edTime2.Enabled := False;
    fColorCircles.edScore2.Text := ''; fColorCircles.laScore2.Enabled := False; fColorCircles.edScore2.Enabled := False;
  end
  else begin
    // 2-player mode: Reset player 2 time and scoring counters
    fColorCircles.edName2.Enabled := True; fColorCircles.edName2.Text := 'Player 2';
    fColorCircles.edTime2.Text := '0:00,00'; fColorCircles.laTime2.Enabled := True; fColorCircles.edTime2.Enabled := True;
    fColorCircles.edScore2.Text := '0'; fColorCircles.laScore2.Enabled := True; fColorCircles.edScore2.Enabled := True;
  end;
  // Highlight 1st player
  fColorCircles.edName1.Color := clLime; fColorCircles.edName2.Color := clDefault;
  // Reset the scores
  Score1 := 0; Score2 := 0;
  // Reset button caption
  fColorCircles.btStart.Caption := 'Start'; fColorCircles.btStart.Enabled := True;
end;

{ Circle clicked by player: Do scoring for correct circle; add penality for bad circle }

procedure CircleClicked(Players, Turn, Row, Column: Integer; Penality: Real; Circles1, Circles2: TGrid; var Time1, Time2: Real; var Score1, Score2: Integer);

var
  Score: Integer;

begin
  fColorCircles.tiColorCircles.Enabled := False;
  // Different colored circles in the 2 grids: Do scoring
  if Circles2[Row, Column].Brush.Color <> Circles1[Row, Column].Brush.Color then begin
    // 1-player mode scoring
    if Players = 1 then begin
      Score := 1200 - Round(10 * Time1);
      if Score < 0 then
        Score := 0;
      Score1 += Score;
    end
    // 2-players mode scoring (only done if both players have played)
    else if Turn = 2 then begin
      // Scoring by time difference
      if fColorCircles.mSettingsTScoring.Checked then begin
        Score := Round(10 * Abs(Time1 - Time2));
        if Time1 <= Time2 then
          Score1 += Score;
        if Time2 <= Time1 then
          Score2 += Score;
      end
      // Scoring by 1 point per game won
      else begin
        if Time1 <= Time2 then
          Score1 += 1;
        if Time2 <= Time1 then
          Score2 += 1;
      end;
    end;
    // Display actual scores
    fColorCircles.edScore1.Text := IntToStr(Score1);
    if Players = 2 then
      fColorCircles.edScore2.Text := IntToStr(Score2);
    // Enable the "Start" button (for next game turn)
    fColorCircles.btStart.Enabled := True;
  end
  // Same colored circles in the 2 grids: Add penality to actual player's time
  else begin
    if Turn = 1 then
      Time1 += Penality
    else
      Time2 += Penality;
    // Game turn has to continue, thus resume timer
    fColorCircles.tiColorCircles.Enabled := True;
  end;
end;

{ Display "Not implemented" message }

procedure NotImplemented(N: Integer);

var
  S: string;

begin
  case N of
    0: S := 'The highscore list feature';
    1: S := 'The 6x6 circles grid';
    2: S := 'The 8x8 circles grid';
  end;
  S += ' is not implemented in ColorCircles version 1.0';
  MessageDlg('Feature unavailable', S, mtWarning, [mbOK], 0);
end;

{****************}
{ TfColorCircles }
{****************}

{ Application start: Initialisation }

procedure TfColorCircles.FormCreate(Sender: TObject);

begin
  // Create array with circle shapes for each grid
  aCircles1[1, 1] := shCircle101; aCircles1[1, 2] := shCircle102; aCircles1[1, 3] := shCircle103; aCircles1[1, 4] := shCircle104; aCircles1[1, 5] := shCircle105;
  aCircles1[2, 1] := shCircle106; aCircles1[2, 2] := shCircle107; aCircles1[2, 3] := shCircle108; aCircles1[2, 4] := shCircle109; aCircles1[2, 5] := shCircle110;
  aCircles1[3, 1] := shCircle111; aCircles1[3, 2] := shCircle112; aCircles1[3, 3] := shCircle113; aCircles1[3, 4] := shCircle114; aCircles1[3, 5] := shCircle115;
  aCircles1[4, 1] := shCircle116; aCircles1[4, 2] := shCircle117; aCircles1[4, 3] := shCircle118; aCircles1[4, 4] := shCircle119; aCircles1[4, 5] := shCircle120;
  aCircles1[5, 1] := shCircle121; aCircles1[5, 2] := shCircle122; aCircles1[5, 3] := shCircle123; aCircles1[5, 4] := shCircle124; aCircles1[5, 5] := shCircle125;
  aCircles2[1, 1] := shCircle201; aCircles2[1, 2] := shCircle202; aCircles2[1, 3] := shCircle203; aCircles2[1, 4] := shCircle204; aCircles2[1, 5] := shCircle205;
  aCircles2[2, 1] := shCircle206; aCircles2[2, 2] := shCircle207; aCircles2[2, 3] := shCircle208; aCircles2[2, 4] := shCircle209; aCircles2[2, 5] := shCircle210;
  aCircles2[3, 1] := shCircle211; aCircles2[3, 2] := shCircle212; aCircles2[3, 3] := shCircle213; aCircles2[3, 4] := shCircle214; aCircles2[3, 5] := shCircle215;
  aCircles2[4, 1] := shCircle216; aCircles2[4, 2] := shCircle217; aCircles2[4, 3] := shCircle218; aCircles2[4, 4] := shCircle219; aCircles2[4, 5] := shCircle220;
  aCircles2[5, 1] := shCircle221; aCircles2[5, 2] := shCircle222; aCircles2[5, 3] := shCircle223; aCircles2[5, 4] := shCircle224; aCircles2[5, 5] := shCircle225;
  // Set default (start-up) values
  iPlayers := 1; iTurn := 1; iCircles := 5; iColors := 4; rPenality := 15;
  // Start random number generator
  Randomize;
  // Prepare for a new game (with default settings)
  NewGame(iPlayers, iCircles, iScore1, iScore2, aCircles1, aCircles2);
end;

{ Menu item "Game > New": Prepare for a new game (with actually selected settings) }

procedure TfColorCircles.mGameNewClick(Sender: TObject);

begin
  NewGame(iPlayers, iCircles, iScore1, iScore2, aCircles1, aCircles2);
end;

{ Menu item "Game > Exit": Exit application }

procedure TfColorCircles.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of players > 1": 1-player mode selection }

procedure TfColorCircles.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;
  mSettingsPlayers2.Checked := False;
  iPlayers := 1;
  mSettingsTScoring.Checked := False; mSettingsTScoring.Enabled := False;
  mSettingsHighscores.Enabled := True;
end;

{ Menu item "Settings > Number of players > 2": 2-players mode selection }

procedure TfColorCircles.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := False;
  mSettingsPlayers2.Checked := True;
  iPlayers := 2;
  mSettingsHighscores.Checked := False; mSettingsHighscores.Enabled := False;
  mSettingsTScoring.Enabled := True;
end;

{ Menu items "Settings > Number of circles > ...": number of circles (grid size) selection }

procedure TfColorCircles.mSettingsCircles4Click(Sender: TObject);

begin
  mSettingsCircles4.Checked := True;
  mSettingsCircles5.Checked := False;
  mSettingsCircles6.Checked := False;
  mSettingsCircles8.Checked := False;
  iCircles := 4;
end;

procedure TfColorCircles.mSettingsCircles5Click(Sender: TObject);

begin
  mSettingsCircles4.Checked := False;
  mSettingsCircles5.Checked := True;
  mSettingsCircles6.Checked := False;
  mSettingsCircles8.Checked := False;
  iCircles := 5;
end;

procedure TfColorCircles.mSettingsCircles6Click(Sender: TObject);

begin
  NotImplemented(1);
end;

procedure TfColorCircles.mSettingsCircles8Click(Sender: TObject);

begin
  NotImplemented(2);
end;

{ Menu items "Settings > Number of colors > ...": number of different circle-colors selection }

procedure TfColorCircles.mSettingsColors3Click(Sender: TObject);

begin
  mSettingsColors3.Checked := True;
  mSettingsColors4.Checked := False;
  mSettingsColors5.Checked := False;
  mSettingsColors6.Checked := False;
  iColors := 3;
end;

procedure TfColorCircles.mSettingsColors4Click(Sender: TObject);

begin
  mSettingsColors3.Checked := False;
  mSettingsColors4.Checked := True;
  mSettingsColors5.Checked := False;
  mSettingsColors6.Checked := False;
  iColors := 4;
end;

procedure TfColorCircles.mSettingsColors5Click(Sender: TObject);

begin
  mSettingsColors3.Checked := False;
  mSettingsColors4.Checked := False;
  mSettingsColors5.Checked := True;
  mSettingsColors6.Checked := False;
  iColors := 5;
end;

procedure TfColorCircles.mSettingsColors6Click(Sender: TObject);

begin
  mSettingsColors3.Checked := False;
  mSettingsColors4.Checked := False;
  mSettingsColors5.Checked := False;
  mSettingsColors6.Checked := True;
  iColors := 6;
end;

{ Menu item "Settings > Usage of 'easy' colors": toggle default / easy color usage }

procedure TfColorCircles.mSettingsColorsEasyClick(Sender: TObject);

begin
  if mSettingsColorsEasy.Checked then
    mSettingsColorsEasy.Checked := False
  else
    mSettingsColorsEasy.Checked := True;
end;

{ Menu item "Settings > Usage of highscore list": toggle disable / enable highscore list usage }

procedure TfColorCircles.mSettingsHighScoresClick(Sender: TObject);

begin
  NotImplemented(0);
end;

{ Menu item "Settings > Score by time difference": toggle scoring by games won / time difference }

procedure TfColorCircles.mSettingsTScoringClick(Sender: TObject);

begin
  if mSettingsTScoring.Checked then
    mSettingsTScoring.Checked := False
  else
    mSettingsTScoring.Checked := True;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfColorCircles.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > Help": Display application about text }

procedure TfColorCircles.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Logic game: Find the different colored circle.' + LineEnding;
  S += 'Game based on an idea found in Mathematik alpha,' + LineEnding;
  S += '© 1985-2018 Steffen Polster' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2019.';
  MessageDlg('About "ColorCircles"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Start a new game turn }

procedure TfColorCircles.btStartClick(Sender: TObject);

var
  ColourGroup, Colour, I, J: Integer;
  OK: Boolean;
  ColourCounts: array[1..6] of Integer;

begin
  // Button "Start"
  if btStart.Caption = 'Start' then begin
    btStart.Caption := 'Next';
    iTurn := 1
  end
  // Button "Next"
  else begin
    // 1-player mode
    if iPlayers = 1 then
      iTurn := 1
    // 2-players mode: alternate game turn
    else begin
      if iTurn = 1 then
        iTurn := 2
      else
        iTurn := 1;
    end;
  end;
  // First-player turn: Set player 1 as actual player and reset time for both players
  if iTurn = 1 then begin
    edName1.Color := clLime;
    edName2.Color := clDefault;
    rTime1 := 0;
    edTime1.Text := '0:00,00';
    if iPlayers = 2 then begin
      rTime2 := 0;
      edTime2.Text := '0:00,00';
    end;
  end
  // Second-player turn: Set player 2 as actual player
  else begin
    edName1.Color := clDefault;
    edName2.Color := clLime;
  end;
  // Choose color group for this turn
  if mSettingsColorsEasy.Checked then
    ColourGroup := 0
  else
    ColourGroup := Random(3) + 1;
  // Choose random colors for circles (same colors in both grids)
  for I := 1 to 6 do
    ColourCounts[I] := 0;
  repeat
    OK := True;
    for J := 1 to iCircles do begin
      for I := 1 to iCircles do begin
        Colour := Random(iColors) + 1;
        aCircles1[J, I].Brush.Color := Colors[ColourGroup, Colour];
        aCircles2[J, I].Brush.Color := aCircles1[J, I].Brush.Color;
        Inc(ColourCounts[Colour]);
      end;
    end;
    // Usage of each color selected a minimum of times
    for I := 1 to iColors do begin
      if ColourCounts[I] < iCircles * iCircles / 6 then
      OK := False;
    end;
  until OK;
  // Randomly select 1 different color at random place in second grid
  iRow := Random(iCircles) + 1; iColumn := Random(iCircles) + 1;
  repeat
    aCircles2[iRow, iColumn].Brush.Color := Colors[ColourGroup, Random(iColors) + 1];
  until aCircles2[iRow, iColumn].Brush.Color <> aCircles1[iRow, iColumn].Brush.Color;
  // Disable "Start" button (until correct color has been clicked)
  btStart.Enabled := False;
  // Start timer (to count elapsed time until correct color has been clicked)
  tiColorCircles.Enabled := True;
end;

{ Circle (in right grid) clicked : Do scoring resp. add time penality }

procedure TfColorCircles.shCircle201MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 1, 1, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle202MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 1, 2, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle203MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 1, 3, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle204MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 1, 4, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle205MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 1, 5, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle206MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 2, 1, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle207MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 2, 2, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle208MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 2, 3, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle209MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 2, 4, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle210MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 2, 5, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle211MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 3, 1, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle212MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 3, 2, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle213MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 3, 3, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle214MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 3, 4, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle215MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 3, 5, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle216MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 4, 1, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle217MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 4, 2, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle218MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 4, 3, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle219MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 4, 4, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle220MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 4, 5, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle221MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 5, 1, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle222MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 5, 2, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle223MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 5, 3, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle224MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 5, 4, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

procedure TfColorCircles.shCircle225MouseDown(Sender: TObject);

begin
  CircleClicked(iPlayers, iTurn, 5, 5, rPenality, aCircles1, aCircles2, rTime1, rTime2, iScore1, iScore2);
end;

{ Timer routine: Increment actual player's time by 0.1 sec }

procedure TfColorCircles.tiColorCirclesTimer(Sender: TObject);

var
  Mins, Secs, Tenths: Integer;
  Time0: Real;
  STime: string;

begin
  if iTurn = 1 then
    Time0 := rTime1
  else
    Time0 := rTime2;
  Time0 += 0.1;
  // Calculate secs, mins and tenths of secs for display
  Secs := Round(Int((Time0))); Tenths := Round(Int(10 * (Time0 - Secs)));
  Mins := Round(Int(Secs)) div 60; Secs := Round(Int(Secs)) mod 60;
  STime := IntToStr(Mins) + ':';
  if Secs < 10 then
    STime += '0';
  STime += IntToStr(Secs) + ',' + IntToStr(Tenths);
  // Display the formated time
  if iTurn = 1 then begin
    rTime1 := Time0;
    fColorCircles.edTime1.Text := STime;
  end
  else begin
    rTime2 := Time0;
    fColorCircles.edTime2.Text := STime;
  end;
end;

end.

