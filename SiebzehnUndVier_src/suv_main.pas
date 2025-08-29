{*********************************************}
{* Main unit for SiebzehnUndVier application *}
{*********************************************}

unit suv_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, LCLIntf,
  suv_dice, suv_dice2, suv_help;

type
  {*******}
  { TfSuv }
  {*******}
  TfSuv = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit, Separator1, Separator2: TMenuItem;
    mSettings, mSettingsPlayers, mSettingsPlayers2, mSettingsPlayers3, mSettingsPlayersComputer: TMenuItem;
    mSettingsDices, mSettingsDicesApp, mSettingsDicesInt, mSettingsDicesReal, mSettingsDicesAuto: TMenuItem;
    mSettingsEval, mSettingsEval1, mSettingsEval2, mSettingsEval3, mSettingsEval4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Memo1: TMemo;
    Label1, laPlayer2, laPlayer3: TLabel;
    edPlayer1, edPlayer2, edPlayer3: TEdit;
    edScore1, edScore2, edScore3, edTotal1, edTotal2, edTotal3: TEdit;
    imDice11, imDice12, imDice13, imDice21, imDice31, imDice32, imDice33, imDice14, imDice15, imDice16: TImage;
    imDice17, imDice18, imDice19, imDice110, imDice111, imDice112, imDice24, imDice25, imDice26: TImage;
    imDice27, imDice28, imDice29, imDice210, imDice211, imDice212, imDice34, imDice35, imDice36: TImage;
    imDice37, imDice38, imDice39, imDice310, imDice311, imDice312, imDice22, imDice23: TImage;
    btRoll1: TButton;
    btRoll2: TButton;
    btRoll3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsPlayers3Click(Sender: TObject);
    procedure mSettingsPlayersComputerClick(Sender: TObject);
    procedure mSettingsDicesAppClick(Sender: TObject);
    procedure mSettingsDicesIntClick(Sender: TObject);
    procedure mSettingsDicesRealClick(Sender: TObject);
    procedure mSettingsDicesAutoClick(Sender: TObject);
    procedure mSettingsEval1Click(Sender: TObject);
    procedure mSettingsEval2Click(Sender: TObject);
    procedure mSettingsEval3Click(Sender: TObject);
    procedure mSettingsEval4Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btRoll1Click(Sender: TObject);
    procedure btRoll2Click(Sender: TObject);
    procedure btRoll3Click(Sender: TObject);
    procedure edPlayer1EditingDone(Sender: TObject);
    procedure edPlayer2EditingDone(Sender: TObject);
    procedure edPlayer3EditingDone(Sender: TObject);
  private
    iPlayers, iEval, iPlayer: Integer;
    sDiceRoll, sPlayer1, sPlayer2, sPlayer3: string;
    bComputer: Boolean;
    edScores, edTotals: array[0..2] of TEdit;
    imDices: array[0..2, 0..3, 0..2] of TImage;
  end;

var
  fSuv: TfSuv;

implementation

{$R *.lfm}

{ Display dices for actual player }

procedure DisplayDices(Player: Integer; var AllDices: TAllDices; Total: Integer);

var
  I, J: Integer;
  FilePath: string;

begin
  Player -= 1;                                                                 // array index starts at 0
  for I := 0 to 3 do begin
    for J := 0 to 2 do begin
      if AllDices[I, J] = 0 then begin
        // Not used dice: hide the image
        fSuv.imDices[Player, I, J].Visible := False;
      end
      else begin
        // Used dice: display corresponding picture
        fSuv.imDices[Player, I, J].Visible := True;
        FilePath := './dices/' + 'dice' + IntToStr(AllDices[I, J]) + '.jpg'; DoDirSeparators(FilePath);
        fSuv.imDices[Player, I, J].Picture.LoadFromFile(FilePath);
      end;
    end;
  end;
  // Display player total
  fSuv.edScores[Player].Text := IntToStr(Total);
  fSuv.edScores[Player].Color := clDefault;
  if Total < 21 then
    fSuv.edScores[Player].Color := clDefault
  else if Total = 21 then
    fSuv.edScores[Player].Color := clLime
  else
    fSuv.edScores[Player].Color := clRed;
end;

{ Determine next player }

procedure NextPlayer(Players: Integer; var Player: Integer);

begin
  // Actual player = player 1
  if Player = 1 then begin
    fSuv.btRoll1.Enabled := False;
    // Next player will be player 2
    fSuv.btRoll2.Enabled := True;
    fSuv.btRoll2.SetFocus;
    Player := 2;
  end
  // Actual player = player 2
  else if Player = 2 then begin
    fSuv.btRoll2.Enabled := False;
    // Next player will be player 3 or player 1
    if Players = 3 then begin
      fSuv.btRoll3.Enabled := True;
      fSuv.btRoll3.SetFocus;
      Player := 3;
    end
    else begin
      fSuv.btRoll1.Enabled := True;
      fSuv.btRoll1.SetFocus;
      Player := 1;
    end;
  end
  // Actual player = player 3
  else begin
    fSuv.btRoll3.Enabled := False;
    // Next player will be player 1
    fSuv.btRoll1.Enabled := True;
    fSuv.btRoll1.SetFocus;
    Player := 1;
  end;
end;

{ Store player's rank in corr. array element }

procedure GetRanks(R1, R2, R3: Integer; var Scores: array of Integer; out Ranks: array of Integer);

var
  I: Integer;

begin
  Ranks[0] := R1; Ranks[1] := R2; Ranks[2] := R3;
  for I := 0 to 2 do begin
    if Scores[I] = 21 then
      Ranks[I] := 0;                                                           // to make difference between "winner" and "winner with exactly 21"
  end;
end;

{ Round evaluation: Determine round points to be added to each player's total }

procedure RoundEval(Players, Eval: Integer);

var
  I, RoundScore: Integer;
  Scores, Totals, Ranks: array[0..2] of Integer;

begin
  // Determine player's rank for this round
  if Players = 2 then
    Scores[2] := -1;                                                           // this sets rank of player 3 always to "3" if there are only 2 players
  for I := 0 to Players - 1 do begin
    if fSuv.edTotals[I].Text = '' then
      Totals[I] := 0
    else
      Totals[I] := StrToInt(fSuv.edTotals[I].Text);
    Scores[I] := StrToInt(fSuv.edScores[I].Text);
    // This puts players with more than 21 at the ranking end
    if Scores[I] > 21 then
      Scores[I] := 0;
  end;
  // Compare player scores and determine their rank; store ranks in array (array element is for corr. player)
  if (Scores[0] > Scores[1]) and (Scores[1] > Scores[2]) then
    GetRanks(1, 2, 3, Scores, Ranks)
  else if (Scores[0] > Scores[2]) and (Scores[2] > Scores[1]) then
    GetRanks(1, 3, 2, Scores, Ranks)
  else if (Scores[1] > Scores[0]) and (Scores[0] > Scores[2]) then
    GetRanks(2, 1, 3, Scores, Ranks)
  else if (Scores[1] > Scores[2]) and (Scores[2] > Scores[0]) then
    GetRanks(3, 1, 2, Scores, Ranks)
  else if (Scores[2] > Scores[0]) and (Scores[0] > Scores[1]) then
    GetRanks(2, 3, 1, Scores, Ranks)
  else if (Scores[2] > Scores[1]) and (Scores[1] > Scores[0]) then
    GetRanks(3, 2, 1, Scores, Ranks)
  else if (Scores[0] > Scores[1]) and (Scores[1] = Scores[2]) then
    GetRanks(1, 2, 2, Scores, Ranks)
  else if (Scores[1] > Scores[0]) and (Scores[0] = Scores[2]) then
    GetRanks(2, 1, 2, Scores, Ranks)
  else if (Scores[2] > Scores[0]) and (Scores[0] = Scores[1]) then
    GetRanks(2, 2, 1, Scores, Ranks)
  else if (Scores[0] = Scores[1]) and (Scores[0] > Scores[2]) then
    GetRanks(1, 1, 3, Scores, Ranks)
  else if (Scores[0] = Scores[2]) and (Scores[0] > Scores[1]) then
    GetRanks(1, 3, 1, Scores, Ranks)
  else if (Scores[1] = Scores[2]) and (Scores[1] > Scores[0]) then
    GetRanks(3, 1, 1, Scores, Ranks)
  else
    GetRanks(1, 1, 1, Scores, Ranks);
  // Determine players' round points, depending on their rank and the actual evaluation settings
  for I := 0 to Players - 1 do begin
    RoundScore := 0;
    case Eval of
      1: begin
        if (Ranks[I] = 0) or (Ranks[I] = 1) then
          RoundScore := 1;                                                     // 1 point for winner
      end;
      2: begin
        if Ranks[I] = 0 then
          RoundScore := 2                                                      // 2 points for winner with 21
        else if Ranks[I] = 1 then
          RoundScore := 1;                                                     // 1 point for winner with less than 21
      end;
      3: begin
        if (Ranks[I] = 0) or (Ranks[I] = 1) then
          RoundScore := 2                                                      // 2 points for winner
        else if Ranks[I] = 2 then
          RoundScore := 1;                                                     // 1 point for second rank
      end;
      4: begin
        if Ranks[I] = 0 then
          RoundScore := 3                                                      // 3 points for winner with 21
        else if Ranks[I] = 1 then
          RoundScore := 2                                                      // 2 points for other winners
        else if Ranks[I] = 2 then
          RoundScore := 1;                                                     // 1 point for second rank
      end;
    end;
    if Scores[I] = 0 then
      RoundScore := 0;                                                         // players with more than 21 never get a point
    // Update player totals
    Totals[I] += RoundScore;
    fSuv.edTotals[I].Text := IntToStr(Totals[I]);
  end;
end;

{*******}
{ TfSuv }
{*******}

{ Application start: Initialisation }

procedure TfSuv.FormCreate(Sender: TObject);

begin
  // Create array with players' score and total fields
  edScores[0] := edScore1; edScores[1] := edScore2; edScores[2] := edScore3;
  edTotals[0] := edTotal1; edTotals[1] := edTotal2; edTotals[2] := edTotal3;
  // Create array with players' dice images
  imDices[0, 0, 0] := imDice11;  imDices[0, 0, 1] := imDice12;  imDices[0, 0, 2] := imDice13;
  imDices[0, 1, 0] := imDice14;  imDices[0, 1, 1] := imDice15;  imDices[0, 1, 2] := imDice16;
  imDices[0, 2, 0] := imDice17;  imDices[0, 2, 1] := imDice18;  imDices[0, 2, 2] := imDice19;
  imDices[0, 3, 0] := imDice110; imDices[0, 3, 1] := imDice111; imDices[0, 3, 2] := imDice112;
  imDices[1, 0, 0] := imDice21;  imDices[1, 0, 1] := imDice22;  imDices[1, 0, 2] := imDice23;
  imDices[1, 1, 0] := imDice24;  imDices[1, 1, 1] := imDice25;  imDices[1, 1, 2] := imDice26;
  imDices[1, 2, 0] := imDice27;  imDices[1, 2, 1] := imDice28;  imDices[1, 2, 2] := imDice29;
  imDices[1, 3, 0] := imDice210; imDices[1, 3, 1] := imDice211; imDices[1, 3, 2] := imDice212;
  imDices[2, 0, 0] := imDice31;  imDices[2, 0, 1] := imDice32;  imDices[2, 0, 2] := imDice33;
  imDices[2, 1, 0] := imDice34;  imDices[2, 1, 1] := imDice35;  imDices[2, 1, 2] := imDice36;
  imDices[2, 2, 0] := imDice37;  imDices[2, 2, 1] := imDice38;  imDices[2, 2, 2] := imDice39;
  imDices[2, 3, 0] := imDice310; imDices[2, 3, 1] := imDice311; imDices[2, 3, 2] := imDice312;
  // Start random generator
  Randomize;
  // Application start-up values
  iPlayers := 2; bComputer := True; iEval := 1;
  sPlayer1 := 'Computer'; sPlayer2 := 'Spieler 2'; sPlayer3 := 'Spieler 3';
  sDiceRoll := 'application';
  // Start a new game
  mGameNew.Click;
end;

{ Menu item "Spiel > Neu": Start a new game }

procedure TfSuv.mGameNewClick(Sender: TObject);

var
  I, J, K: Integer;

begin
  // Clear the form
  for I := 0 to 2 do begin
    edScores[I].Text := ''; edScores[I].Color := clDefault;
    edTotals[I].Text := '';
  end;
  for I := 0 to 2 do begin
    for J := 0 to 3 do begin
      for K := 0 to 2 do
        imDices[I, J, K].Picture.Clear;
    end;
  end;
  // Adapt form layout to number of players
  if iPlayers = 3 then begin
    laPlayer3.Enabled := True; edPlayer3.Enabled := True; edScore3.Enabled := True;
    for J := 0 to 3 do begin
      for K := 0 to 2 do
        imDices[2, J, K].Visible := True;
    end;
    btRoll3.Visible := True; edTotal3.Visible := True;
  end
  else begin
    laPlayer3.Enabled := False; edPlayer3.Enabled := False; edScore3.Enabled := False;
    for J := 0 to 3 do begin
      for K := 0 to 2 do
        imDices[2, J, K].Visible := False;
    end;
    btRoll3.Visible := False; edTotal3.Visible := False;
  end;
  // Player 2 always begins the round
  iPlayer := 2;
  btRoll1.Enabled := False; btRoll2.Enabled := True; btRoll3.Enabled := False;
end;

{ Menu item "Spiel > Verlassen": Exit application }

procedure TfSuv.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Anzahl der Spieler >  2 Spieler": Select number of players = 2 }

procedure TfSuv.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := True;  mSettingsPlayers3.Checked := False;
  edPlayer3.Text := '';
  iPlayers := 2;
  // Disable evaluation types that only apply to 3 players
  if mSettingsEval3.Checked then begin
    mSettingsEval1.Checked := True;
    mSettingsEval3.Checked := False;
  end
  else if mSettingsEval4.Checked then begin
    mSettingsEval2.Checked := True;
    mSettingsEval4.Checked := False;
  end;
  mSettingsEval3.Enabled := False; mSettingsEval4.Enabled := False;
  // Start a new game
  mGameNew.Click;
end;

{ Menu item "Einstellungen > Anzahl der Spieler >  3 Spieler": Select number of players = 3 }

procedure TfSuv.mSettingsPlayers3Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := False;  mSettingsPlayers3.Checked := True;
  edPlayer3.Text := 'Spieler 3';
  iPlayers := 3;
  // Enable 3 players evaluation types
  mSettingsEval3.Enabled := True; mSettingsEval4.Enabled := True;
  // Start a new game
  mGameNew.Click;
end;

{ Menu item "Einstellungen > Anzahl der Spieler >  Computer spielt": Toggle computer plays or not }

procedure TfSuv.mSettingsPlayersComputerClick(Sender: TObject);

begin
  if mSettingsPlayersComputer.Checked then begin
    mSettingsPlayersComputer.Checked := False;
    edPlayer1.Text := 'Spieler 1';
    edPlayer1.ReadOnly := False;
    btRoll1.Caption := 'Würfeln!';
  end
  else begin
    mSettingsPlayersComputer.Checked := True;
    edPlayer1.Text := 'Computer';
    edPlayer1.ReadOnly := True;
    btRoll1.Caption := 'Würfelt...'
  end;
  // Start a new game
  mGameNew.Click;
end;

{ Menu items "Einstellungen > Würfelmethode >  ...": Select dice roll method }

procedure TfSuv.mSettingsDicesAppClick(Sender: TObject);

begin
  mSettingsDicesApp.Checked := True; mSettingsDicesInt.Checked := False; mSettingsDicesReal.Checked := False;
  sDiceRoll := 'application';
end;

procedure TfSuv.mSettingsDicesIntClick(Sender: TObject);

begin
  mSettingsDicesApp.Checked := False; mSettingsDicesInt.Checked := True; mSettingsDicesReal.Checked := False;
  sDiceRoll := 'interactive';
end;

procedure TfSuv.mSettingsDicesRealClick(Sender: TObject);

begin
  mSettingsDicesApp.Checked := False; mSettingsDicesInt.Checked := False; mSettingsDicesReal.Checked := True;
  sDiceRoll := 'user';
end;

{ Menu item "Einstellungen > Würfelmethode >  1. Wurf automatisch": Toggle first dice roll being automaticly done or not }

procedure TfSuv.mSettingsDicesAutoClick(Sender: TObject);

begin
  if mSettingsDicesAuto.Checked then
    mSettingsDicesAuto.Checked := False
  else
    mSettingsDicesAuto.Checked := True;
end;

{ Menu items "Einstellungen > Rundenwertung >  ...": Select round evaluation method }

procedure TfSuv.mSettingsEval1Click(Sender: TObject);

begin
  mSettingsEval1.Checked := True;  mSettingsEval2.Checked := False;
  mSettingsEval3.Checked := False; mSettingsEval4.Checked := False;
  iEval := 1;
end;

procedure TfSuv.mSettingsEval2Click(Sender: TObject);

begin
  mSettingsEval1.Checked := False; mSettingsEval2.Checked := True;
  mSettingsEval3.Checked := False; mSettingsEval4.Checked := False;
  iEval := 2;
end;

procedure TfSuv.mSettingsEval3Click(Sender: TObject);

begin
  mSettingsEval1.Checked := False; mSettingsEval2.Checked := False;
  mSettingsEval3.Checked := True;  mSettingsEval4.Checked := False;
  iEval := 3;
end;

procedure TfSuv.mSettingsEval4Click(Sender: TObject);

begin
  mSettingsEval1.Checked := False; mSettingsEval2.Checked := False;
  mSettingsEval3.Checked := False; mSettingsEval4.Checked := True;
  iEval := 4;
end;

{ Menu item "Hilfe > Hilfe": Display application help text }

procedure TfSuv.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfSuv.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Siebzehn und Vier Würfelspiel für 1 - 3 Spieler.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Mai-Juni 2022.';
  MessageDlg('Über "SiebzehnUndVier"', S, mtInformation, [mbOK], 0);
end;

{ First button "Würfelt/Würfeln" pushed: Computer dice roll resp. roll the dices for first player }

procedure TfSuv.btRoll1Click(Sender: TObject);

var
  iDices, Max, Dice, Score, I, J: Integer;
  FilePath: string;
  AllDices: TAllDices;

begin
  // Computer takes the part of one of the players
  if edPlayer1.Text = 'Computer' then begin
    // Determine winning score so far
    Max := 0;
    if iPlayers = 2 then begin
      if StrToInt(edScore2.Text) <= 21 then
        Max := StrToInt(edScore2.Text)
    end
    else begin
      if (StrToInt(edScore2.Text) <= 21) and (StrToInt(edScore3.Text) <= 21) then begin
        if StrToInt(edScore2.Text) >= StrToInt(edScore3.Text) then
          Max := StrToInt(edScore2.Text)
        else
          Max := StrToInt(edScore3.Text);
      end
      else if StrToInt(edScore2.Text) <= 21 then
        Max := StrToInt(edScore2.Text)
      else if StrToInt(edScore3.Text) <= 21 then
        Max := StrToInt(edScore3.Text);
    end;
    // Reset the dice values
    for I := 0 to 3 do begin
      for J := 0 to 2 do
        AllDices[I, J] := 0;
    end;
    Score := 0;
    // First throw (must be with all 3 dices)
    for I := 0 to 2 do begin
      imDices[0, 0, I].Visible := True;
      Dice := Random(6) + 1;
      FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
      imDices[0, 0, I].Picture.LoadFromFile(FilePath);
      AllDices[0, I] := Dice;
      Score += Dice;
    end;
    // Choice of number of dices for second throw
    if Score <= 5 then begin
      if Score > Max then
        iDices := 2
      else
        iDices := 3;
    end
    else if Score <=10 then begin
      if Score > Max then
        iDices := Random(2) + 1
      else
        iDices := 2;
    end
    else if Score <= 12 then begin
      if Score > Max then
        iDices := 1
      else
        iDices := Random(2) + 1;
    end
    else
      iDices := 1;
    // Second throw (with number of dices chosen)
    for I := 0 to iDices - 1 do begin
      imDices[0, 1, I].Visible := True;
      Dice := Random(6) + 1;
      FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
      imDices[0, 1, I].Picture.LoadFromFile(FilePath);
      AllDices[1, I] := Dice;
      Score += Dice;
    end;
    // Choice of number of dices for third throw
    if Score <= 21 then begin
      if Score > Max then
        iDices := 1
      else begin
        if Score <= 10 then
          iDices := 3
        else if Score <=12 then
          iDices := 2
        else if Score <= 15 then
          iDices := Random(2) + 1
        else
          iDices := 1;
      end;
      // Third throw (with number of dices chosen)
      for I := 0 to iDices - 1 do begin
        imDices[0, 2, I].Visible := True;
        Dice := Random(6) + 1;
        FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
        imDices[0, 2, I].Picture.LoadFromFile(FilePath);
        AllDices[2, I] := Dice;
        Score += Dice;
      end;
    end;
    if Score <= 21 then begin
      // Choice of number of dices for fourth throw (incl. the possibility not to throw the dices)
      if Score > Max then
        iDices := 0
      else begin
        if Score = Max then begin
          if Score >= 18 then
            iDices := 0
          else if Score >= 17 then
            iDices := Random(1) + 1
          else if Score >= 15 then
            iDices := 1
          else if Score >= 12 then
            iDices := Random(2) + 1
          else
            iDices := 2;
        end
        else begin
          if Score >= 15 then
            iDices := 1
          else if Score >= 12 then
            iDices := Random(2) + 1
          else
            iDices := 2;
        end;
      end;
      // Fourth throw (with number of dices chosen)
      for I := 0 to iDices - 1 do begin
        imDices[0, 3, I].Visible := True;
        Dice := Random(6) + 1;
        FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
        imDices[0, 3, I].Picture.LoadFromFile(FilePath);
        AllDices[3, I] := Dice;
        Score += Dice;
      end;
    end;
    edScore1.Text := IntToStr(Score);
  end
  // Computer doesn't take part in the game: Open dice rolling/entering window for player 1
  else begin
    if sDiceRoll = 'user' then begin
      // Usage of real dices: Open dice entering window
      fDice2.ShowModal;
      AllDices := fDice2.aAllDices;
      Score := fDice2.iTotal;
    end
    else begin
      // Dice roll done by application: Open dice rolling window
      fDice.sDiceRoll := sDiceRoll;
      fDice.bFirstAuto := mSettingsDicesAuto.Checked;
      fDice.ShowModal;
      AllDices := fDice.aAllDices;
      Score := fDice.iTotal;
    end;
  end;
  // Display dices and score (on main form)
  DisplayDices(1, AllDices, Score);
  // Move to next player
  NextPlayer(iPlayers, iPlayer);
  // As player 1 always plays last, round evaluation has to be done here
  RoundEval(iPlayers, iEval);
end;

{ Second button "Würfeln" pushed: Roll the dices for second player }

procedure TfSuv.btRoll2Click(Sender: TObject);

var
  Score, I, J, K: Integer;
  AllDices: TAllDices;

begin
  // As second player always plays first, a new round starts now; thus variable and form control reset has to be done here
  for I := 0 to 2 do begin
    edScores[I].Text := ''; edScores[I].Color := clDefault;
  end;
  for I := 0 to 2 do begin
    for J := 0 to 3 do begin
      for K := 0 to 2 do
        imDices[I, J, K].Picture.Clear;
    end;
  end;
  // Open dice rolling/entering window for player 2
  if sDiceRoll = 'user' then begin
    fDice2.ShowModal;
    AllDices := fDice2.aAllDices;
    Score := fDice2.iTotal;
  end
  else begin
    fDice.sDiceRoll := sDiceRoll;
    fDice.bFirstAuto := mSettingsDicesAuto.Checked;
    fDice.ShowModal;
    AllDices := fDice.aAllDices;
    Score := fDice.iTotal;
  end;
  // Display dices and score (on main form)
  DisplayDices(2, AllDices, Score);
  // Move to next player
  NextPlayer(iPlayers, iPlayer);
end;

{ Third button "Würfeln" pushed: Roll the dices for third player }

procedure TfSuv.btRoll3Click(Sender: TObject);

var
  Score: Integer;
  AllDices: TAllDices;

begin
  // Open dice rolling/entering window for player 3
  if sDiceRoll = 'user' then begin
    fDice2.ShowModal;
    AllDices := fDice2.aAllDices;
    Score := fDice2.iTotal;
  end
  else begin
    fDice.sDiceRoll := sDiceRoll;
    fDice.bFirstAuto := mSettingsDicesAuto.Checked;
    fDice.ShowModal;
    AllDices := fDice.aAllDices;
    Score := fDice.iTotal;
  end;
  // Display dices and score (on main form)
  DisplayDices(3, AllDices, Score);
  // Move to next player
  NextPlayer(iPlayers, iPlayer);
end;

{ Check player names if user alters the content of the corr. edit fields }

procedure TfSuv.edPlayer1EditingDone(Sender: TObject);

begin
  if (edPlayer1.Text = '') or (edPlayer1.Text = edPlayer2.Text) or (edPlayer1.Text = edPlayer3.Text) or
     (edPlayer1.Text = 'Computer') or (edPlayer1.Text = 'Spieler 2') or (edPlayer1.Text = 'Spieler 3') then begin
    edPlayer1.Text := sPlayer1;
    MessageDlg('Eingabefehler', 'Ungültiger Name für Spieler 1', mtWarning, [mbOK], 0);
  end
  else
    sPlayer1 := edPlayer1.Text;
end;

procedure TfSuv.edPlayer2EditingDone(Sender: TObject);

begin
  if (edPlayer2.Text = '') or (edPlayer2.Text = edPlayer1.Text) or (edPlayer2.Text = edPlayer3.Text) or
     (edPlayer2.Text = 'Computer') or (edPlayer2.Text = 'Spieler 1') or (edPlayer2.Text = 'Spieler 3') then begin
    edPlayer2.Text := sPlayer2;
    MessageDlg('Eingabefehler', 'Ungültiger Name für Spieler 2', mtWarning, [mbOK], 0);
  end
  else
    sPlayer2 := edPlayer2.Text;
end;

procedure TfSuv.edPlayer3EditingDone(Sender: TObject);

begin
  if (edPlayer3.Text = '') or (edPlayer3.Text = edPlayer1.Text) or (edPlayer3.Text = edPlayer2.Text) or
     (edPlayer3.Text = 'Computer') or (edPlayer3.Text = 'Spieler 1') or (edPlayer3.Text = 'Spieler 2') then begin
    edPlayer3.Text := sPlayer3;
    MessageDlg('Eingabefehler', 'Ungültiger Name für Spieler 3', mtWarning, [mbOK], 0);
  end
  else
    sPlayer3 := edPlayer3.Text;
end;

end.

