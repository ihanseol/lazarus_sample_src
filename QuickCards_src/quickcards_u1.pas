{****************************************}
{* Main unit for QuickCards application *}
{****************************************}

unit quickcards_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LCLType;

type
  TSuit = (diamond, club, heart, spade);
  TCard = record
    Suit: TSuit;
    Rank: Integer;
  end;
  TCardDeck = array[1..52] of TCard;
  {**************}
  { TfQuickCards }
  {**************}
  TfQuickCards = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsWrongCard, mSettingsWrongCardSimple, mSettingsWrongCardDouble: TMenuItem;
    mSettingsBadSuit, mSettingsBadSuitNormal, mSettingsBadSuitHalfDebit, mSettingsBadSuitHalfCredit: TMenuItem;
    MenuItem1: TMenuItem;
    mSettingsTime, mSettingsTime25, mSettingsTime50, mSettingsTime100, mSettingsTime200: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imCard: TImage;
    stAction: TStaticText;
    Label1, Label3, Label4, Label5: TLabel;
    laPlayer2, laName2, laKey2, laScore2: TLabel;
    edName1, edKey1, edScore1: TEdit;
    edName2, edKey2, edScore2: TEdit;
    edEval: TEdit;
    btStart: TButton;
    tiQuickCards: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsWrongCardSimpleClick(Sender: TObject);
    procedure mSettingsWrongCardDoubleClick(Sender: TObject);
    procedure mSettingsBadSuitNormalClick(Sender: TObject);
    procedure mSettingsBadSuitHalfDebitClick(Sender: TObject);
    procedure mSettingsBadSuitHalfCreditClick(Sender: TObject);
    procedure mSettingsTime25Click(Sender: TObject);
    procedure mSettingsTime50Click(Sender: TObject);
    procedure mSettingsTime100Click(Sender: TObject);
    procedure mSettingsTime200Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tiQuickCardsTimer(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure edKey1Change(Sender: TObject);
    procedure edKey2Change(Sender: TObject);
  private
    iPlayers, ixCardDeck, iTime, iScore1, iScore2: Integer;
    rBadCard, rBadSuit: Real;
    sAction: string;
    bKeyCapture: Boolean;
    aCardDeck: TCardDeck;
    CardToFind, CardActual: TCard;
  end;

var
  fQuickCards: TfQuickCards;

implementation

{$R *.lfm}

{ Fill the card deck }

procedure FillCardDeck(out CardDeck: TCardDeck);

var
  N, I: Integer;
  Suit: TSuit;

begin
  N := 1;
  for Suit := diamond to spade do begin
    for I := 1 to 13 do begin
      CardDeck[N].Suit := Suit;
      CardDeck[N].Rank := I;
      Inc(N);
    end;
  end;
end;

{ Shuffle the cards }

procedure Shuffle(var CardDeck: TCardDeck);

var
  R1, R2, I: Integer;
  Card: TCard;

begin
  for I := 1 TO 50 do begin
    R1 := Random(52) + 1; R2 := Random(52) + 1;
    Card := CardDeck[R1]; CardDeck[R1] := CardDeck[R2]; CardDeck[R2] := Card;
  end;
end;

{ Display a given card }

procedure DisplayCard(Card: TCard);

var
  N: Integer;
  Filename: string;

begin
  N := 100 + Ord(Card.Suit) * 13 + Card.Rank;
  Filename := './cmagedeck/' + IntToStr(N) + '.png';
  fQuickCards.imCard.Picture.LoadFromFile(Filename);
end;

{**************}
{ TfQuickCards }
{**************}

{ Application start: Initialisation }

procedure TfQuickCards.FormCreate(Sender: TObject);

begin
  rBadCard := 2; rBadSuit := -1;
  iTime := 500;
  Randomize;
  mGameNew.Click;                                                              // start a new game
end;

{ Menu item "Game > New": Start a new game }

procedure TfQuickCards.mGameNewClick(Sender: TObject);

var
  Filename: string;

begin
  tiQuickCards.Enabled := False;
  bKeyCapture := False;                                                        // to "disable" OnUTF8KeyPressed routine
  // Hide player 2 form controls in 1-player mode
  if mSettingsPlayers1.Checked then
    iPlayers := 1
  else
    iPlayers := 2;
  if iPlayers = 1 then begin
    laPlayer2.Visible := False;
    laName2.Visible := False; edName2.Text := ''; edName2.Visible := False;
    laKey2.Visible := False; edKey2.Text := ''; edKey2.Visible := False;
    laScore2.Visible := False; edScore2.Text := ''; edScore2.Visible := False;
  end
  else begin
    laPlayer2.Visible := True;
    laName2.Visible := True; edName2.Visible := True;
    laKey2.Visible := True; edKey2.Visible := True;
    laScore2.Visible := True; edScore2.Visible := True;
  end;
  // Fill the card deck
  FillCardDeck(aCardDeck);
  // Display card with face down
  Filename := './cmagedeck/155.png'; DoDirSeparators(Filename);
  imCard.Picture.LoadFromFile(Filename);
  // Reset score counters
  iScore1 := 0; iScore2 := 0;
  edScore1.Text := IntToStr(iScore1); edScore2.Text := IntToStr(iScore2);
  edEval.Text := ''; edEval.Color := clDefault;
  // Reset button captions
  btStart.Caption := 'Start';
end;

{ Menu item "Game > Exit": Exit application }

procedure TfQuickCards.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Number of players > ...": Select 1 or 2 players mode }

procedure TfQuickCards.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;
  mSettingsPlayers2.Checked := False;
end;

procedure TfQuickCards.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := False;
  mSettingsPlayers2.Checked := True;
end;

{ Menu items "Settings > Wrong card scoring > ...": Select wrong card scoring }

procedure TfQuickCards.mSettingsWrongCardSimpleClick(Sender: TObject);

begin
  mSettingsWrongCardSimple.Checked := True;
  mSettingsWrongCardDouble.Checked := False;
  rBadCard := 1;
end;

procedure TfQuickCards.mSettingsWrongCardDoubleClick(Sender: TObject);

begin
  mSettingsWrongCardSimple.Checked := False;
  mSettingsWrongCardDouble.Checked := True;
  rBadCard := 2;
end;

{ Menu items "Settings > Bad suit scoring > ...": Select bad suit scoring }

procedure TfQuickCards.mSettingsBadSuitNormalClick(Sender: TObject);

begin
  mSettingsBadSuitNormal.Checked := True;
  mSettingsBadSuitHalfDebit.Checked := False;
  mSettingsBadSuitHalfCredit.Checked := False;
  rBadSuit := -1;
end;

procedure TfQuickCards.mSettingsBadSuitHalfDebitClick(Sender: TObject);

begin
  mSettingsBadSuitNormal.Checked := False;
  mSettingsBadSuitHalfDebit.Checked := True;
  mSettingsBadSuitHalfCredit.Checked := False;
  rBadSuit := -0.5;
end;

procedure TfQuickCards.mSettingsBadSuitHalfCreditClick(Sender: TObject);

begin
  mSettingsBadSuitNormal.Checked := False;
  mSettingsBadSuitHalfDebit.Checked := False;
  mSettingsBadSuitHalfCredit.Checked := True;
  rBadSuit := +0.5;
end;

{ Menu items "Settings > Time interval > ...": Select cards display time interval }

procedure TfQuickCards.mSettingsTime25Click(Sender: TObject);

begin
  mSettingsTime25.Checked := True;
  mSettingsTime50.Checked := False;
  mSettingsTime100.Checked := False;
  mSettingsTime200.Checked := False;
  iTime := 250;
end;

procedure TfQuickCards.mSettingsTime50Click(Sender: TObject);

begin
  mSettingsTime25.Checked := False;
  mSettingsTime50.Checked := True;
  mSettingsTime100.Checked := False;
  mSettingsTime200.Checked := False;
  iTime := 500;
end;

procedure TfQuickCards.mSettingsTime100Click(Sender: TObject);

begin
  mSettingsTime25.Checked := False;
  mSettingsTime50.Checked := False;
  mSettingsTime100.Checked := True;
  mSettingsTime200.Checked := False;
  iTime := 1000;
end;

procedure TfQuickCards.mSettingsTime200Click(Sender: TObject);

begin
  mSettingsTime25.Checked := False;
  mSettingsTime50.Checked := False;
  mSettingsTime100.Checked := False;
  mSettingsTime200.Checked := True;
  iTime := 2000;
end;

{ Menu item "Help > Help": Dislay (short) program help }

procedure TfQuickCards.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'The aim of the game is simple: Take a card deck, shuffle it and show the first card to the players. ';
  S += 'Then hide the card in the deck and show all the cards one by one. If the initial card appears, ';
  S += 'the player, who pushed "his" keyboard key, scores. If he pushes for a wrong card, his score is debited.';
  MessageDlg('"QuickCards" help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Dislay program about }

procedure TfQuickCards.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Cards reaction game for 1 or 2 players.' + LineEnding;
  S += 'Version 1.0, © allu, August 2019.';
  MessageDlg('About "QuickCards"', S, mtInformation, [mbOK], 0);
end;

{ Buttons "Start/Pause/Resume": Start with a new card; pause resp. resume the game }

procedure TfQuickCards.btStartClick(Sender: TObject);

begin
  // Button "Start/Resume": Start with a new card resp. resume the game
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Resume') then begin
    // Button "Start": Prepare for a new card
    if btStart.Caption = 'Start' then begin
      sAction := 'start';                                                      // this tells the timer routine to select a new "to find" card
      tiQuickCards.Interval := 1;
    end;
    edEval.Text := ''; edEval.Color := clDefault;
    bKeyCapture := True;                                                       // "enable" the OnUTF8KeyPressed routine
    tiQuickCards.Enabled := True;
    btStart.Caption := 'Pause';
  end
  // Button "Pause": Pause the game
  else begin
    tiQuickCards.Enabled := False;                                             // Disable the timer
    bKeyCapture := False;                                                      // "disable" the OnUTF8KeyPressed routine
    btStart.Caption := 'Resume';
  end;
end;

{ Game timer routine (card display code here...) }

procedure TfQuickCards.tiQuickCardsTimer(Sender: TObject);

var
  IX: Integer;
  Card: TCard;

begin
  // Start of a new round (display of a new "to find" card )
  if sAction = 'start' then begin
    stAction.Caption := 'Stop the display' + LineEnding + 'for the following card';
    Shuffle(aCardDeck);                                                        // shuffle the card deck
    CardToFind := aCardDeck[1];                                                // take the first card of the deck as "to find" card
    DisplayCard(CardToFind);
    // Hide "to find" card in the deck
    IX := Random(51) + 2;
    Card := aCardDeck[1]; aCardDeck[1] := aCardDeck[IX]; aCardDeck[IX] := Card;
    ixCardDeck := 1;
    tiQuickCards.Interval := 2 * iTime;                                        // time to display the "to find" card
    sAction := 'seq';
  end
  // Display the cards of the deck one by one (restarting from the beginning if all cards have been done)
  else begin
    stAction.Caption := 'Use your player key to stop' + LineEnding + 'the display if the card appears';
    if ixCardDEck > 52 then
      // Restart with first card
      ixCardDeck := 1;
    CardActual := aCardDeck[ixCardDEck];
    DisplayCard(CardActual);
    Inc(ixCardDeck);                                                           // point to next card in the deck
    tiQuickCards.Interval := iTime;                                            // time to display the deck card
  end;
end;

{ Keyboard key capture routine (user key evaluation code here...) }

procedure TfQuickCards.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

var
  Key1, Key2: TUTF8Char;
  Score: Real;
  Eval: string;
  CorrectAll, CorrectRank: Boolean;
  Colour: TColor;

begin
  // Execute routine only if key capture is "enabled"
  if bKeyCapture then begin
    Key1 := edKey1.Text; Key2 := edKey2.Text;
    // Consider the 2 user keys only
    if (UTF8Key = Key1) or (UTF8Key = Key2) then begin
      CorrectAll := False; CorrectRank := False;
      if (CardActual.Suit = CardToFind.Suit) and (CardActual.Rank = CardToFind.Rank) then
        // User stopped dispay for correct card
        CorrectAll := True
      else if CardActual.Rank = CardToFind.Rank then
        // User stopped dispay for correct rank (but bad suit)
        CorrectRank := True;;
      // Correct card
      if CorrectAll then begin
        Score := +10;
        Eval := 'Correct card';
        Colour := clLime;
      end
      // Correct rank (bad suit): Evaluation depends on settings
      else if CorrectRank then begin
        if rBadSuit = 0.5 then begin
          Score := +5;
          Colour := clGreen;
        end
        else if rBadSuit = -0.5 then begin
          Score := -5 * rBadCard;
          Colour := clYellow;
        end
        else begin
          Score := -10 * rBadCard;
          Colour := clRed;
        end;
        Eval := 'Correct rank, but wrong suit';
      end
      // Wrong card
      else begin
        Score := -10 * rBadCard;
        Colour := clRed;
        Eval := 'Wrong card';
      end;
      Eval += ' - Score = ';
      if Score > 0 then
        Eval += '+';                                                           // display all scoring with a sign
      Eval += IntToStr(Round(Score));
      // Key pressed by player 1
      if UTF8Key = Key1 then begin
        edScore1.Text := IntToStr(StrToInt(edScore1.Text) + Round(Score));
        edEval.Text := edName1.Text + ': ' + Eval;
      end
      // Key pressed by player 2
      else begin
        edScore2.Text := IntToStr(StrToInt(edScore2.Text) + Round(Score));
        edEval.Text := edName2.Text + ': ' + Eval;
      end;
      edEval.Color := Colour;
      // Card found: Prepare for a new round (new "to find" card)
      if Score = 10 then begin
        tiQuickCards.Enabled := False;                                         // stop the timer
        bKeyCapture := False;                                                  // "disable" OnUTF8KeyPressed routine
        stAction.Caption := '';
        btStart.Caption := 'Start';                                            // game continues, when user pushes "Start" button
      end;
    end;
  end;
end;

{ Player key changes }

procedure TfQuickCards.edKey1Change(Sender: TObject);

begin
  if (edKey1.Text <> '') and (edKey1.Text = edKey2.Text) then begin
    MessageDlg('Player keys', 'You must use a different key for each player!', mtError, [mbOK], 0);
    edKey1.Text := '';
  end;
end;

procedure TfQuickCards.edKey2Change(Sender: TObject);

begin
  if iPlayers = 1 then
    edKey2.Text := ''
  else if (edKey2.Text <> '') and (edKey2.Text = edKey1.Text) then begin
    MessageDlg('Player keys', 'You must use a different key for each player!', mtError, [mbOK], 0);
    edKey2.Text := '';
  end;
end;

end.

