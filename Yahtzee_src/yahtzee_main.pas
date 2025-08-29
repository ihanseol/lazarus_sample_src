{*************************************}
{* Main unit for Yahtzee application *}
{*************************************}

unit yahtzee_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, LCLIntf,
  yahtzee_dice, yahtzee_dice2, yahtzee_help1, yahtzee_help2;

type
  {***********}
  { TfYahtzee }
  {***********}
  TfYahtzee = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings, mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2, mSettingsPlayers3: TMenuItem;
    mSettingsJoker, mSettingsJokerForced, mSettingsJokerFree, mSettingsJokerOriginal: TMenuItem;
    MenuItem1, mSettingsRealDices, mSettingsReroll, mSettingsRerollReroll, mSettingsRerollKeep, mSettingsAutoClose: TMenuItem;
    mHelp, mHelpRules, mHelpScoring, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, laPlayer2, laPlayer3: TLabel;
    edPlayer1, edPlayer2, edPlayer3: TEdit;
    edScore1, edScore2, edScore3: TEdit;
    imDice11, imDice12, imDice13, imDice14, imDice15: TImage;
    imDice21, imDice22, imDice23, imDice24, imDice25: TImage;
    imDice31, imDice32, imDice33, imDice34, imDice35: TImage;
    sgUpper, sgLower: TStringGrid;
    btChoose11, btChoose12, btChoose13, btChoose14, btChoose15, btChoose16: TButton;
    btChoose21, btChoose22, btChoose23, btChoose24, btChoose25, btChoose26, btChoose27: TButton;
    btRoll: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsPlayers3Click(Sender: TObject);
    procedure mSettingsRealDicesClick(Sender: TObject);
    procedure mSettingsRerollRerollClick(Sender: TObject);
    procedure mSettingsRerollKeepClick(Sender: TObject);
    procedure mSettingsJokerForcedClick(Sender: TObject);
    procedure mSettingsJokerFreeClick(Sender: TObject);
    procedure mSettingsJokerOriginalClick(Sender: TObject);
    procedure mSettingsAutoCloseClick(Sender: TObject);
    procedure mHelpRulesClick(Sender: TObject);
    procedure mHelpScoringClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btChoose11Click(Sender: TObject);
    procedure btChoose12Click(Sender: TObject);
    procedure btChoose13Click(Sender: TObject);
    procedure btChoose14Click(Sender: TObject);
    procedure btChoose15Click(Sender: TObject);
    procedure btChoose16Click(Sender: TObject);
    procedure btChoose21Click(Sender: TObject);
    procedure btChoose22Click(Sender: TObject);
    procedure btChoose23Click(Sender: TObject);
    procedure btChoose24Click(Sender: TObject);
    procedure btChoose25Click(Sender: TObject);
    procedure btChoose26Click(Sender: TObject);
    procedure btChoose27Click(Sender: TObject);
    procedure btRollClick(Sender: TObject);
  private
    iPlayers, iRound, iPlayer: Integer;
    sJokerRule, sSelect: string;
    bError, bEndOfGame, bAutoClose: Boolean;
    aDices: array[0..4] of Integer;
    imDices: array[0..2, 0..4] of TImage;
  end;

var
  fYahtzee: TfYahtzee;

implementation

{$R *.lfm}

{ Format values for grid (right-align) }

function GFormat(N: Integer): string;

var
  SN: string;

begin
  SN := '   ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  Result := SN;
end;

{ Calculate sum of array elements }

function ArraySum(Arr: array of Integer): Integer;

var
  Sum, I: Integer;

begin
  Sum := 0;
  for I := 0 to Length(Arr) - 1 do
    Sum += Arr[I];
  Result := Sum;
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

{ Check if dice are N of a kind }

function IsNofaKind(N: Integer; Dices: array of Integer): Boolean;

var
  Count, I, J: Integer;
  ItIs: Boolean;

begin
  ItIs := False;
    for I := 1 to 6 do begin
      Count := 0;
      for J := 0 to 4 do begin
        if Dices[J] = I then
          Inc(Count);
      end;
      if Count >= N then
        ItIs := True;
    end;
  Result := ItIs;
end;

{ Check if dice are a Full House }

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

{ Check if dice are a (small or large) Straight }

function IsStraight(StraightKind: string; Dices: array of Integer): Boolean;

var
  I: Integer;
  SDices, SDicesS: string;
  ItIs: Boolean;

begin
  ItIs := False;
  ArraySort(Dices);
  SDices := IntToStr(Dices[0]);
  for I := 1 to 4 do begin
    if Dices[I] <> Dices[I - 1] then
      SDices += IntToStr(Dices[I]);
  end;
  // Check for Small Straight
  if StraightKind = 'small' then begin
    for I := 1 to 2 do begin
      if I = 1 then
        SDicesS := LeftStr(SDices, 4)
      else
        SDicesS := RightStr(SDices, 4);
      if (SDicesS = '1234') or (SDicesS = '2345') or (SDicesS = '3456') then
        ItIs := True;
    end;
  end
  // Check for Large Straight
  else begin
    if (SDices = '12345') or (SDices = '23456') then
      ItIs := True
  end;
  Result := ItIs;
end;

{ Determine players score for actual dice }

procedure AddScore(Player: Integer; Dices: array of Integer; UpperGrid, LowerGrid: TStringGrid; Grid: string; Category: Integer; out IsError: Boolean);

var
  Score, Sum, Bonus63, BonusYahtzee, Total, I: Integer;
  isJoker: Boolean;

begin
  IsError := True;
  if not fYahtzee.btRoll.Enabled then begin
    // Score only if dices have been rolled and scoring not yet done
    if ((Grid = 'upper') and (UpperGrid.Cells[Player, Category] = '')) or ((Grid = 'lower') and (LowerGrid.Cells[Player, Category] = '')) then begin
      // Category selected must be still free
      IsError := False; IsJoker := False; Score := 0; BonusYahtzee := 0;
      if IsNofaKind(5, Dices) and (LowerGrid.Cells[Player, 6] <> '') then begin
        // Supplementary Yahtzee
        if StrToInt(Trim(LowerGrid.Cells[Player, 6])) = 50 then begin
          // Bonus only if first Yahtzee was scored (Yahtzee category not filled with 0)
          MessageDlg('Yahtzee', 'Bonus 100 for supplementary Yahtzee!', mtInformation, [mbOK], 0);
          if LowerGrid.Cells[Player, 9] = '' then
            BonusYahtzee := 100
          else
            BonusYahtzee := StrToInt(Trim(LowerGrid.Cells[Player, 9])) + 100;
          // Determine if the Yahtzee Joker may be used (corr. Upper section category used)
          if UpperGrid.Cells[Player, Dices[0]] <> '' then
            IsJoker := True;
        end;
      end;
      // Upper section scoring
      if Grid = 'upper' then begin
        // Score: Add dice with value equal to category
        for I := 0 to 4 do begin
          if Dices[I] = Category then
            Score += Dices[I];
        end;
        // Fill-in score and Upper section sum
        UpperGrid.Cells[Player, Category] := GFormat(Score);
        if UpperGrid.Cells[Player, 7] = '' then
          Sum := Score
        else
          Sum := StrToInt(Trim(UpperGrid.Cells[Player, 7])) + Score;
        UpperGrid.Cells[Player, 7] := GFormat(Sum);
        // Check if user gets an Upper section bonus (sum >= 63)
        if UpperGrid.Cells[Player, 8] = '' then begin
          Bonus63 := 0;
          if Sum >= 63 then begin
            MessageDlg('Yahtzee', 'Bonus 35 for sum of upper section >= 63!', mtInformation, [mbOK], 0);
            Bonus63 := 35;
          end;
        end
        else
          Bonus63 := StrToInt(Trim(UpperGrid.Cells[Player, 8]));
        // Fill-in Upper section bonus and total
        if Bonus63 > 0 then
          UpperGrid.Cells[Player, 8] := GFormat(Bonus63);
        UpperGrid.Cells[Player, 9] := GFormat(Sum + Bonus63);
      end
      // Lower section scoring
      else begin
        // Score, depending on category (considering a possible Yahtzee bonus)
        case Category of
          1: if IsNofaKind(3, Dices) then Score := ArraySum(Dices);
          2: if IsNofaKind(4, Dices) then Score := ArraySum(Dices);
          3: begin
               if IsFullHouse(Dices) or IsJoker then begin
                 Score := 25;
                 if IsJoker then
                   MessageDlg('Yahtzee', 'Joker used to score for Full House.', mtInformation, [mbOK], 0);
               end;
             end;
          4: begin
               if IsStraight('small', Dices) or IsJoker then begin
                 Score := 30;
                 if IsJoker then
                   MessageDlg('Yahtzee', 'Joker used to score for Small Straight.', mtInformation, [mbOK], 0);
               end;
             end;
          5: begin
               if IsStraight('large', Dices) or IsJoker then begin
                 Score := 40;
                 if IsJoker then
                   MessageDlg('Yahtzee', 'Joker used to score for Large Straight.', mtInformation, [mbOK], 0);
               end;
             end;
          6: if IsNofaKind(5, Dices) then Score := 50;
          7: Score := ArraySum(Dices);
        end;
        // Fill-in score and Lower section sum
        LowerGrid.Cells[Player, Category] := GFormat(Score);
        if LowerGrid.Cells[Player, 8] = '' then
          Sum := Score
        else
          Sum := StrToInt(Trim(LowerGrid.Cells[Player, 8])) + Score;
        LowerGrid.Cells[Player, 8] := GFormat(Sum);
      end;
      // Fill-in Yahtzee bonus and Lower section total
      if BonusYahtzee > 0 then
        LowerGrid.Cells[Player, 9] := GFormat(BonusYahtzee);
      if LowerGrid.Cells[Player, 8] <> '' then
        LowerGrid.Cells[Player, 10] := GFormat(StrToInt(Trim(LowerGrid.Cells[Player, 8])) + BonusYahtzee);
      // Fill-in the grand total (in the actual player's "Score" edit field )
      Total := 0;
      if UpperGrid.Cells[Player, 9] <> '' then
        Total += StrToInt(Trim(UpperGrid.Cells[Player, 9]));
      if LowerGrid.Cells[Player, 10] <> '' then
        Total += StrToInt(Trim(LowerGrid.Cells[Player, 10]));
      case Player of
        1: fYahtzee.edScore1.Text := IntToStr(Total);
        2: fYahtzee.edScore2.Text := IntToStr(Total);
        3: fYahtzee.edScore3.Text := IntToStr(Total);
      end;
    end
    else
      // Error message if user tries to use a category already used
      MessageDlg('Yahtzee', 'The selected category has already been used!', mtError, [mbOK], 0);
  end
  else
    // Error message if user tries to score twice
    MessageDlg('Yahtzee', 'Your score has already been entered!', mtError, [mbOK], 0);
end;

{ Determine next player }

procedure NextPlayer(Players: Integer; var Player: Integer; GameRound: Integer; var EndOfGame: Boolean);

begin
  // Check if game is over (all categories filled)
  if (GameRound = 13) and (Player = Players) then begin
    MessageDlg('Yahtzee', 'Game over...', mtInformation, [mbOK], 0);
    EndOfGame := True;
  end;
  // If game not over, determine next player
  if not EndOfGame then begin
    // Actual player = player 1
    if Player = 1 then begin
      if Players >= 2 then begin
        fYahtzee.edPlayer1.Color := clDefault;
        // Next player will be player 2, if multi-player game
        fYahtzee.edPlayer2.Color := clLime;
        Player := 2;
      end;
    end
    // Actual player = player 2
    else if Player = 2 then begin
      fYahtzee.edPlayer2.Color := clDefault;
      // Next player will be player 3 or player 1 (depending on number of players)
      if Players = 2 then begin
        fYahtzee.edPlayer1.Color := clLime;
        Player := 1;
      end
      else begin
        fYahtzee.edPlayer3.Color := clLime;
        Player := 3;
      end;
    end
    // Actual player = player 3
    else begin
      fYahtzee.edPlayer3.Color := clDefault;
      // Next player will be player 1
      fYahtzee.edPlayer1.Color := clLime;
      Player := 1;
    end;
  end;
  // Enable the "Roll" button (this also "blocks" a further scoring )
  fYahtzee.btRoll.Enabled := True;
  fYahtzee.btRoll.SetFocus;
end;

{***********}
{ TfYahtzee }
{***********}

{ Application start: Initialisation }

procedure TfYahtzee.FormCreate(Sender: TObject);

begin
  // Create array with players' dice images
  imDices[0, 0] := imDice11; imDices[0, 1] := imDice12; imDices[0, 2] := imDice13; imDices[0, 3] := imDice14; imDices[0, 4] := imDice15;
  imDices[1, 0] := imDice21; imDices[1, 1] := imDice22; imDices[1, 2] := imDice23; imDices[1, 3] := imDice24; imDices[1, 4] := imDice25;
  imDices[2, 0] := imDice31; imDices[2, 1] := imDice32; imDices[2, 2] := imDice33; imDices[2, 3] := imDice34; imDices[2, 4] := imDice35;
  Randomize;
  iPlayers := 1; sJokerRule := 'free'; sSelect := 'reroll'; bAutoClose := True;
  // Start a new game
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new game }

procedure TfYahtzee.mGameNewClick(Sender: TObject);

var
  I, J: Integer;

begin
  // Mark player 1 and clear score
  iPlayer := 1; edPlayer1.Color := clLime;
  if iPlayers >= 2 then
    edPlayer2.Color := clDefault;
  if iPlayers = 3 then
    edPlayer3.Color := clDefault;
  edScore1.Text := ''; edScore2.Text := ''; edScore3.Text := '';
  iRound := 0;
  // "Remove" all dices
  for I := 0 to 2 do begin
    for J := 0 to 4 do
      imDices[I, J].Picture.Clear;
  end;
  // Clear both section grids
  for I := 1 to 3 do begin
    for J := 1 to 9 do
      sgUpper.Cells[I, J] := '';
  end;
  for I := 1 to 3 do begin
    for J := 1 to 10 do
      sgLower.Cells[I, J] := '';
  end;
  bEndOfGame := False;
  // Enable the "Roll" button
  btRoll.Enabled := True;
end;

{ Menu item "Game > Exit": Exit application }

procedure TfYahtzee.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Number of players >  ...": Select number of players }

procedure TfYahtzee.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;
  mSettingsPlayers2.Checked := False;
  mSettingsPlayers3.Checked := False;
  laPlayer2.Enabled := False; edPlayer2.Enabled := False; edPlayer2.Text := ''; edPlayer3.Text := ''; edScore2.Enabled := False; edScore2.Text := '';
  laPlayer3.Enabled := False; edPlayer3.Enabled := False; edPlayer3.Text := ''; edPlayer3.Text := ''; edScore3.Enabled := False; edScore3.Text := '';
  iPlayers := 1;
  mGameNew.Click;                                                              // start a new game
end;

procedure TfYahtzee.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := False;
  mSettingsPlayers2.Checked := True;
  mSettingsPlayers3.Checked := False;
  laPlayer2.Enabled := True; edPlayer2.Enabled := True; edScore2.Enabled := True;
  if edPlayer2.Text = '' then
    edPlayer2.Text := 'Player 2';
  laPlayer3.Enabled := False; edPlayer3.Enabled := False; edPlayer3.Text := ''; edScore3.Enabled := False; edScore3.Text := '';
  iPlayers := 2;
  mGameNew.Click;
end;

procedure TfYahtzee.mSettingsPlayers3Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := False;
  mSettingsPlayers2.Checked := False;
  mSettingsPlayers3.Checked := True;
  laPlayer2.Enabled := True; edPlayer2.Enabled := True; edScore2.Enabled := True;
  if edPlayer2.Text = '' then
    edPlayer2.Text := 'Player 2';
  laPlayer3.Enabled := True; edPlayer3.Enabled := True; edScore3.Enabled := True;
  if edPlayer3.Text = '' then
    edPlayer3.Text := 'Player 3';
  iPlayers := 3;
  mGameNew.Click;
end;

{ Menu items "Settings > Yahtzee Joker rule > ...": Select Yahtzee Joker rule }

procedure TfYahtzee.mSettingsJokerForcedClick(Sender: TObject);

// Actually not implemented (menu item disabled)

begin
  mSettingsJokerForced.Checked := True;
  mSettingsJokerFree.Checked := False;
  mSettingsJokerOriginal.Checked := False;
  sJokerRule := 'forced';
end;

procedure TfYahtzee.mSettingsJokerFreeClick(Sender: TObject);

begin
  mSettingsJokerForced.Checked := False;
  mSettingsJokerFree.Checked := True;
  mSettingsJokerOriginal.Checked := False;
  sJokerRule := 'free';
end;

procedure TfYahtzee.mSettingsJokerOriginalClick(Sender: TObject);

// Actually not implemented (menu item disabled)

begin
  mSettingsJokerForced.Checked := False;
  mSettingsJokerFree.Checked := False;
  mSettingsJokerOriginal.Checked := True;
  sJokerRule := 'original';
end;

{ Menu item "Settings > Use real dices": Toggle usage of real dices/program dice roll }

procedure TfYahtzee.mSettingsRealDicesClick(Sender: TObject);

begin
  if mSettingsRealDices.Checked then begin
    mSettingsRealDices.Checked := False;
    mSettingsReroll.Enabled := True;
    mSettingsAutoClose.Enabled := True;
  end
  else begin
    mSettingsRealDices.Checked := True;
    mSettingsReroll.Enabled := False;
    mSettingsAutoClose.Enabled := False;
  end;
end;

{ Menu items "Settings > Dice reroll selection > ...": Select if dices selected by user are to be rerolled or to be kept }

procedure TfYahtzee.mSettingsRerollKeepClick(Sender: TObject);

begin
  mSettingsRerollKeep.Checked := True;
  mSettingsRerollReroll.Checked := False;
  sSelect := 'keep';
end;

procedure TfYahtzee.mSettingsRerollRerollClick(Sender: TObject);

begin
  mSettingsRerollKeep.Checked := False;
  mSettingsRerollReroll.Checked := True;
  sSelect := 'reroll';
end;

{ Menu item "Settings > Autoclose dice-roll window": Toggle to autoclose or not the dice rolling window}

procedure TfYahtzee.mSettingsAutoCloseClick(Sender: TObject);

begin
  if mSettingsAutoClose.Checked then
    mSettingsAutoClose.Checked := False
  else
    mSettingsAutoClose.Checked := True;
  bAutoClose := mSettingsAutoClose.Checked;
end;

{ Menu item "Help > Yahtzee rules": Open browser at corr. Wikipedia website }

procedure TfYahtzee.mHelpRulesClick(Sender: TObject);

begin
  OpenDocument('https://en.wikipedia.org/wiki/Yahtzee');
end;

{ Menu item "Help > Scoring rules": Display table with Yahtzee scoring rules }

procedure TfYahtzee.mHelpScoringClick(Sender: TObject);

begin
  if fScoring.Visible then
    fScoring.Close
  else
    fScoring.Show;
end;

{ Menu item "Help > Application help": Display application help text }

procedure TfYahtzee.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfYahtzee.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Freeware Yahtzee game for 1 - 3 payers.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, March-December 2020.';
  MessageDlg('About "Yahtzee"', S, mtInformation, [mbOK], 0);
end;

{ Button "Roll the dice": Open dice rolling window (with first throw dices displayed) }

procedure TfYahtzee.btRollClick(Sender: TObject);

var
  I, J: Integer;

begin
  edPlayer1.TabStop := False; edPlayer2.TabStop := False; edPlayer3.TabStop := False;
  // Do only so, if the game isn't over
  if not bEndOfGame then begin
    // Hide all players' dices
    for I := 0 to 2 do begin
      for J := 0 to 4 do begin
        imDices[I, J].Visible := False;
      end;
    end;
    // Disable the "Roll" button (when dice rolling window closes, user will have to select a scoring category)
    btRoll.Enabled := False;
    if mSettingsRealDices.Checked then begin
      // Show real dice entry form (and wait until it has been closed)
      fDice2.ShowModal;
      // Show the dice rolling result for actual player
      for I := 0 to 4 do begin
        imDices[iPlayer - 1, I].Visible := True; imDices[iPlayer - 1, I].Picture := fDice2.imDices[I].Picture;
        aDices[I] := fDice2.aDices[I];
      end;
    end
    else begin
      // Pass game parameters to program dice rolling form
      fDice.sSelect := sSelect;
      fDice.bAutoClose := bAutoClose;
      // Show program dice rolling form (and wait until it has been closed)
      fDice.ShowModal;
      // Show the dice rolling result for actual player
      for I := 0 to 4 do begin
        imDices[iPlayer - 1, I].Visible := True; imDices[iPlayer - 1, I].Picture := fDice.imDices[I].Picture;
        aDices[I] := fDice.aDices[I];
      end;
    end;
    // Next round (if all players have thrown the dices)
    if iPlayer = 1 then
      Inc(iRound);
    btChoose11.SetFocus;
  end;
end;

{ Category selection buttons: Determine and fill-in the corr. score (for actual player and actual dice values) }

procedure TfYahtzee.btChoose11Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'upper', 1, bError);
  if not bError then
    // Continue game only if correct scoring done
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose12Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'upper', 2, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose13Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'upper', 3, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose14Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'upper', 4, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose15Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'upper', 5, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose16Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'upper', 6, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose21Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 1, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose22Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 2, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose23Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 3, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose24Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 4, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose25Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 5, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose26Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 6, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

procedure TfYahtzee.btChoose27Click(Sender: TObject);

begin
  AddScore(iPlayer, aDices, sgUpper, sgLower, 'lower', 7, bError);
  if not bError then
    NextPlayer(iPlayers, iPlayer, iRound, bEndOfGame);
end;

end.

