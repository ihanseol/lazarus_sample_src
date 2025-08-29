{****************************************}
{* Main unit for BlackJack2 application *}
{****************************************}

// If you like the game, enjoy playing it. But, don't try to understand the coding; it works, but IT'S NOTHING BUT A BIG MESS!

unit blackjack2_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, blackjack2_u2;

type
  TSuit  = (diamond, club, heart, spade);
  TCard  = record
    Suit   : TSuit;
    Rank   : Integer;
    BJValue: Integer;
  end;
  TCardDeck = array[1..52] of TCard;
  TCards = array[1..8] of record
    FaceUp: Boolean;
    Card  : TCard;
  end;
  TImages = array[0..2, 1..8] of TImage;
  {**************}
  { TfBlackjack2 }
  {**************}
  TfBlackjack2 = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsBJ, mOptionsBust, mOptionsStandOff, mOptionsHold: TMenuItem;
    MenuItem1, mOptionsOnePlayer: TMenuItem;
    mHelp, mHelpBJ, mHelpProgram, mHelpAbout: TMenuItem;
    StaticText1, StaticText2, StaticText3, stNameCards1, stNameCards2: TStaticText;
    Label3, Label4, laName1, laName2, laNameGame1, laNameGame2, laNameBalance1, laNameBalance2: TLabel;
    edName1, edName2, edGameValue0, edGameValue1, edGameValue2: TEdit;
    edGameResult1, edGameResult2, edBalance1, edBalance2, edStake: TEdit;
    Image1, Image2, Image3, Image4, Image5, Image6, Image7, Image8: TImage;
    Image9, Image10, Image11, Image12, Image13, Image14, Image15, Image16: TImage;
    Image17, Image18, Image19, Image20, Image21, Image22, Image23, Image24: TImage;
    btDeal: TButton;
    btHit: TButton;
    btStay: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsBJClick(Sender: TObject);
    procedure mOptionsBustClick(Sender: TObject);
    procedure mOptionsHoldClick(Sender: TObject);
    procedure mOptionsStandOffClick(Sender: TObject);
    procedure mOptionsOnePlayerClick(Sender: TObject);
    procedure mHelpBJClick(Sender: TObject);
    procedure mHelpProgramClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDealClick(Sender: TObject);
    procedure btHitClick(Sender: TObject);
    procedure btStayClick(Sender: TObject);
    procedure edName1EditingDone(Sender: TObject);
    procedure edName2EditingDone(Sender: TObject);
  private
    iDealerBJValue1, iDealerBJValue2, iPlayer1BJValue1, iPlayer1BJValue2, iPlayer2BJValue1, iPlayer2BJValue2: Integer;
    iCard, iDealerHold, iStake1, iStake2, iPlayer1ValueDone, iPlayer2ValueDone: Integer;
    rMultWin, rMultBust: Real;
    sNameP1, sNameP2, sActualPlayer: string;
    bStandOff, bPlayer1Busted, bPlayer2Busted, bPlayer1OnTheRocks, bPlayer2OnTheRocks: Boolean;
    aCardDeck: TCardDeck;
    aDealerCards, aPlayer1Cards, aPlayer2Cards: TCards;
  end;

var
  fBlackjack2 : TfBlackjack2;
  Images: TImages;

implementation

{$R *.lfm}

{ Function: Get card image filename }

function CardFilename(Card: TCard): string;

var
  N: Integer;

begin
  N := 100 + Ord(Card.Suit) * 13 + Card.Rank;
  CardFilename := IntToStr(N) + '.png';
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

{ Reset the game (for new round) }

procedure ResetGame(var DealerCards, Player1Cards, Player2Cards: TCards);

var
  I: Integer;

begin
  fBlackjack2.edGameValue0.Caption := ''; fBlackjack2.edGameValue1.Caption := ''; fBlackjack2.edGameValue2.Caption := '';
  // Remove all cards from the table
  for I := 1 to 8 do begin
    Player1Cards[I].FaceUp := False;
    Player2Cards[I].FaceUp := False;
    DealerCards[I].FaceUp := False;
    Images[0, I].Picture.Clear;
    Images[1, I].Picture.Clear;
    Images[2, I].Picture.Clear;
  end;
  fBlackjack2.edGameResult1.Caption := ''; fBlackjack2.edGameResult2.Caption := '';
  fBlackjack2.btDeal.Enabled := True;
  fBlackjack2.btHit.Enabled := False;
  fBlackjack2.btStay.Enabled := False;
end;

{ New game }

procedure NewGame(var CardDeck: TCardDeck; var DealerCards, Player1Cards, Player2Cards: TCards; var NameP1, NameP2, ActualPlayer: string;
  var Card: Integer; var P1OnTheRocks, P2OnTheRocks: Boolean);

begin
  NameP1 := fBlackjack2.edName1.Text; NameP2 := fBlackjack2.edName2.Text; ActualPlayer := NameP1;
  fBlackjack2.laNameGame1.Caption := NameP1; fBlackjack2.laNameGame2.Caption := NameP2;
  fBlackjack2.laNameBalance1.Caption := NameP1; fBlackjack2.laNameBalance2.Caption := NameP2;
  fBlackjack2.stNameCards1.Caption := NameP1; fBlackjack2.stNameCards2.Caption := NameP2;
  fBlackjack2.edName1.Color := clAqua; fBlackjack2.edName2.Color := cldefault;
  fBlackjack2.edBalance1.Color := clDefault; fBlackjack2.edBalance2.Color := clDefault;
  fBlackjack2.edBalance1.Caption := '1000'; fBlackjack2.edBalance2.Caption := '1000';
  P1OnTheRocks := False; P2OnTheRocks := False;
  fBlackjack2.edStake.Text := '0'; fBlackjack2.edStake.Enabled := False;
  ResetGame(DealerCards, Player1Cards, Player2Cards);                                    // reset for new round
  Shuffle(CardDeck);                                                                     // shuffle the cards
  Card := 0;
end;

{ Function: Get next card (index) from deck }

function NextDeckCard(var CardDeck: TCardDeck; N: Integer): Integer;

begin
  Inc(N);
  if N > 52 then begin                                                                   // if all cards have been used, reshuffle
    Shuffle(CardDeck);
    N := 1;
  end;
  NextDeckCard := N;
end;

{ Calculate cards' Blackjack value }

procedure BJValue(Cards: TCards; out V1, V2: Integer);

var
  I: Integer;

begin
  V1 := 0; V2 := 0;
  for I := 1 to 8 do begin
    if Cards[I].FaceUp then begin
      V1 := V1 + Cards[I].Card.BJValue;
      // An "as" may be counted as 1 or as 11
      if Cards[I].Card.Rank = 1 then
        V2 := V2 + 11
      else
        V2 := V2 + Cards[I].Card.BJValue;
    end;
  end;
  // Blackjack (2 aces)
  if (V1 = 2) and (V2 = 22) then begin
    V1 := -21; V2 := -21;                                                                // using -21 to represent Blackjack
  end;
end;

{ Get dealer's hold Blackjack value }

function GetDealerDoneValue(DealerHold: Integer; var DealerCards: TCards; DealerBJValue1, DealerBJValue2: Integer): Integer;

var
  DoneValue: Integer;

begin
  DoneValue := 0;
  BJValue(DealerCards, DealerBJValue1, DealerBJValue2);
  if (DealerBJValue1 = -21) and (DealerBJValue2 = -21) then begin                        // Blackjack
    fBlackjack2.edGameValue0.Text := 'BJ';
    DoneValue := -21;
  end
  else begin
    if DealerBJValue2 <= 21 then
      fBlackjack2.edGameValue0.Text := IntToStr(DealerBJValue2)
    else
      fBlackjack2.edGameValue0.Text := IntToStr(DealerBJValue1);
    if (DealerBJValue2 >= DealerHold) and (DealerBJValue2 <= 21) then
      DoneValue := DealerBJValue2
    else if (DealerBJValue1 >= DealerHold) and (DealerBJValue1 <= 21) then
      DoneValue := DealerBJValue1
    else if (DealerBJValue1 > 21) and (DealerBJValue2 > 21) then begin
      if DealerBJValue1 < DealerBJValue2 then
        DoneValue := DealerBJValue1
      else
        DoneValue := DealerBJValue2;
    end;
  end;
  GetDealerDoneValue := DoneValue;
end;

{ Display the dealer/player cards }

procedure DisplayCard(Player, IX: Integer; Cards: TCards);

const
  CardsPath    = 'cmagedeck/';
  DeckCardFile = '155.png';

var
  CardFile: string;

begin
  // Display card face resp. card bottom
  if Cards[IX].FaceUp then
    CardFile := CardFilename(Cards[IX].Card)
  else
    CardFile := DeckCardFile;
  CardFile := CardsPath + CardFile;
  DoDirSeparators(CardFile);                                                             // platform independent file path
  Images[Player, IX].Picture.LoadFromFile(CardFile);
end;

{ Find the winner (dealer, player or possibly stand-off) }

procedure FindWinner(DealerDoneValue, PlayerDoneValue: Integer; out Winner: string);

begin
  if DealerDoneValue = -21 then begin                                                    // dealer has Blackjack (dealer wins or "stand-off")
    if fBlackjack2.mOptionsStandOff.Checked then
      Winner := 'dealer'
    else
      Winner := 'nobody';
  end
  else if PlayerDoneValue = -21 then                                                     // player has Blackjack (player wins if dealer hasn't)
    Winner := 'player'
  else begin
    if (DealerDoneValue > 21)                                                            // dealer busted (dealer loses)
    or (PlayerDoneValue > DealerDoneValue) then                                          // player BJ value > dealer BJ value (player wins)
      Winner := 'player'
    else if DealerDoneValue > PlayerDoneValue then                                       // dealer BJ value > player BJ value (dealer wins)
      Winner := 'dealer'
    else if DealerDoneValue = PlayerDoneValue then begin                                 // dealer BJ value = player BJ value (dealer wins or "stand-off")
      if fBlackjack2.mOptionsStandOff.Checked then
        Winner := 'dealer'
      else
        Winner := 'nobody';
    end;
  end;
end;

{ Display "Invalid stake" message }

procedure InvalidStake(Stake: Integer);

var
  Mess: string;

begin
  if Stake < 10 then
    Mess := 'Your stake must be greater than or equal equal to 10!'
  else
    Mess := 'Your stake must be less than or equal equal to your balance!';
  MessageDlg('Invalid stake', Mess, mtError, [mbOK], 0);
  fBlackjack2.edStake.Enabled := True; fBlackjack2.edStake.SetFocus;                     // enable the edit field for stake re-entry
end;

{ Calculate player's new balance }

function CalculateBalance(Balance, Stake, BJValue: Integer; GameResult: string; MultWin: Real; DealerStandOff: Boolean): Integer;

var
  NewBalance: Integer;

begin
  NewBalance := Balance;
  if GameResult = 'You win!' then begin
    if BJValue = -21 then
      NewBalance += Round(MultWin * Stake)
    else
      NewBalance += Stake;
  end
  else if GameResult = 'Dealer wins' then begin
    NewBalance -= Stake;
  end
  else begin
    if DealerStandOff then
      NewBalance -= Stake;
  end;
  Result := NewBalance;
end;

{**************}
{ TfBlackjack2 }
{**************}

{ Application start: Initialisation  }

procedure TfBlackjack2.FormCreate(Sender: TObject);

var
  Suit: TSuit;
  N, Rank: Integer;

begin
  // Create an array containing the 24 card images
  Images[0, 1] := Image1;  Images[0, 2] := Image2;  Images[0, 3] := Image3;  Images[0, 4] := Image4;
  Images[0, 5] := Image5;  Images[0, 6] := Image6;  Images[0, 7] := Image7;  Images[0, 8] := Image8;
  Images[1, 1] := Image9;  Images[1, 2] := Image10; Images[1, 3] := Image11; Images[1, 4] := Image12;
  Images[1, 5] := Image13; Images[1, 6] := Image14; Images[1, 7] := Image15; Images[1, 8] := Image16;
  Images[2, 1] := Image17; Images[2, 2] := Image18; Images[2, 3] := Image19; Images[2, 4] := Image20;
  Images[2, 5] := Image21; Images[2, 6] := Image22; Images[2, 7] := Image23; Images[2, 8] := Image24;
  // Fill the carddeck
  N := 0;
  for Suit := diamond to spade do begin
    for Rank := 1 to 13 do begin
      Inc(N);
      aCardDeck[N].Suit := Suit;
      aCardDeck[N].Rank := Rank;
      if Rank <= 10 then
        aCardDeck[N].BJValue := Rank
      else
        aCardDeck[N].BJValue := 10;
    end;
  end;
  // Start game with default options
  iDealerHold := 17; rMultWin := 1.5; rMultBust := 1; bStandOff := False;
  Randomize;
  NewGame(aCardDeck, aDealerCards, aPlayer1Cards, aPlayer2Cards, sNameP1, sNameP2, sActualPlayer, iCard, bPlayer1OnTheRocks, bPlayer2OnTheRocks);
end;

{ Menu item: "Game > New": Start a new game }

procedure TfBlackjack2.mGameNewClick(Sender: TObject);

begin
  NewGame(aCardDeck, aDealerCards, aPlayer1Cards, aPlayer2Cards, sNameP1, sNameP2, sActualPlayer, iCard, bPlayer1OnTheRocks, bPlayer2OnTheRocks);
end;

{ Menu item "Game > Exit": Exit application }

procedure TfBlackjack2.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Black Jack wins 1.5x the stake": Select actual "win with Blackjack" option }

procedure TfBlackjack2.mOptionsBJClick(Sender: TObject);

begin
  if mOptionsBJ.Checked then begin
    mOptionsBJ.Checked := False;
    rMultWin := 1;
  end
  else begin
    mOptionsBJ.Checked := True;
    rMultWin := 1.5;
  end;
end;

{ Menu item "Options > Busting costs 2x the stake": Select actual "lose by busting" option }

procedure TfBlackjack2.mOptionsBustClick(Sender: TObject);

begin
  if mOptionsBust.Checked then begin
    mOptionsBust.Checked := False;
    rMultBust := 1;
  end
  else begin
    mOptionsBust.Checked := True;
    rMultBust := 2;
  end;
end;

{ Menu item "Options > Dealer wins at stand-off": Select actual "stand-off" option }

procedure TfBlackjack2.mOptionsHoldClick(Sender: TObject);

begin
  if mOptionsStandOff.Checked then
    mOptionsStandOff.Checked := False
  else
    mOptionsStandOff.Checked := True;
  bStandOff := mOptionsStandOff.Checked;
end;

{ Menu item "Options > Dealer holds on 18": Select actual "dealer hold" option }

procedure TfBlackjack2.mOptionsStandOffClick(Sender: TObject);

begin
  if mOptionsHold.Checked then begin
    mOptionsHold.Checked := False;
    iDealerHold := 17;
  end
  else begin
    mOptionsHold.Checked := True;
    iDealerHold := 18;
  end;
end;

{ Menu item "Options > One player game": Select one or two player game }

procedure TfBlackjack2.mOptionsOnePlayerClick(Sender: TObject);

// Feature not yet implemented (menu item disabled)

begin
  if mOptionsOnePlayer.Checked then begin
    mOptionsOnePlayer.Checked := False;
  end
  else begin
    mOptionsOnePlayer.Checked := True;
  end;
end;

{ Menu item "Help > Blackjack help": Display Blackjack game help }

procedure TfBlackjack2.mHelpBJClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide;
  fHelp.HelpText.Clear;
  fHelp.HelpText.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display program help }

procedure TfBlackjack2.mHelpProgramClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide;
  fHelp.HelpText.Clear;
  fHelp.HelpText.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfBlackjack2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Blackjack game for 2 players.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2018.' + LineEnding;
  S += 'Version 2.0, © allu, March-June 2019.';
  MessageDlg('About "BlackJack2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Deal": Deal the first 6 cards }

procedure TfBlackjack2.btDealClick(Sender: TObject);

var
  PlayerBJValue1, PlayerBJValue2: Integer;
  PV: string;

begin
  // If the balance of one of the players < 10, end the game
  if bPlayer1OnTheRocks or bPlayer2OnTheRocks then begin
    if bPlayer1OnTheRocks then begin
      if StrToInt(edBalance1.Text) > 0 then
        edBalance1.Color := clYellow
      else
        edBalance1.Color := clRed;
    end;
    if bPlayer2OnTheRocks then begin
      if StrToInt(edBalance2.Text) > 0 then
        edBalance2.Color := clYellow
      else
        edBalance2.Color := clRed;
    end;
    if bPlayer1OnTheRocks and bPlayer2OnTheRocks then
      MessageDlg('Game over', 'End of game: Both players are on the rocks!', mtInformation, [mbOK], 0)
    else if bPlayer1OnTheRocks then
      MessageDlg('Game over', sNameP1 + ' is on the rocks! Ending the game...', mtInformation, [mbOK], 0)
    else if bPlayer2OnTheRocks then
      MessageDlg('Game over', sNameP2 + ' is on the rocks! Ending the game...', mtInformation, [mbOK], 0);
    btDeal.Enabled := False;
  end
  // If the balances of both players >= 10, start new round (deal the cards)
  else begin
    if sActualPlayer = sNameP1 then begin
      ResetGame(aDealerCards, aPlayer1Cards, aPlayer2Cards);
      // Deal first 6 cards
      iCard := NextDeckCard(aCardDeck, iCard); aDealerCards[1].Card := aCardDeck[iCard];  aDealerCards[1].FaceUp := True;
      iCard := NextDeckCard(aCardDeck, iCard); aDealerCards[2].Card := aCardDeck[iCard];
      DisplayCard(0, 1, aDealerCards); DisplayCard(0, 2, aDealerCards);
      iCard := NextDeckCard(aCardDeck, iCard); aPlayer1Cards[1].Card := aCardDeck[iCard]; aPlayer1Cards[1].FaceUp := True;
      iCard := NextDeckCard(aCardDeck, iCard); aPlayer1Cards[2].Card := aCardDeck[iCard]; aPlayer1Cards[2].FaceUp := True;
      DisplayCard(1, 1, aPlayer1Cards); DisplayCard(1, 2, aPlayer1Cards);
      iCard := NextDeckCard(aCardDeck, iCard); aPlayer2Cards[1].Card := aCardDeck[iCard]; aPlayer2Cards[1].FaceUp := True;
      iCard := NextDeckCard(aCardDeck, iCard); aPlayer2Cards[2].Card := aCardDeck[iCard]; aPlayer2Cards[2].FaceUp := True;
      DisplayCard(2, 1, aPlayer2Cards); DisplayCard(2, 2, aPlayer2Cards);
      bPlayer1Busted := False; bPlayer2Busted := False;
    end;
    // Calculate Blackjack cards values
    BJValue(aDealerCards, iDealerBJValue1, iDealerBJValue2);
    if sActualPlayer = sNameP1 then begin
      BJValue(aPlayer1Cards, iPlayer1BJValue1, iPlayer1BJValue2);
      PlayerBJValue1 := iPlayer1BJValue1; PlayerBJValue2 := iPlayer1BJValue2;
    end
    else begin
      BJValue(aPlayer2Cards, iPlayer2BJValue1, iPlayer2BJValue2);
      PlayerBJValue1 := iPlayer2BJValue1; PlayerBJValue2 := iPlayer2BJValue2;
    end;
    if (iDealerBJValue1 = -21) and (iDealerBJValue2 = -21) then                          // Blackjack
      edGameValue0.Text := 'BJ'
    else begin
      edGameValue0.Text := IntToStr(iDealerBJValue1);
      if iDealerBJValue2 <> iDealerBJValue1 then
        edGameValue0.Text := edGameValue0.Text + ' / ' + IntToStr(iDealerBJValue2);
    end;
    if (PlayerBJValue1 = -21) and (PlayerBJValue2 = -21) then                            // Blackjack
      PV := 'BJ'
    else begin
      PV := IntToStr(PlayerBJValue1);
      if PlayerBJValue2 <> PlayerBJValue1 then
        PV += ' / ' + IntToStr(PlayerBJValue2);
    end;
    if sActualPlayer = sNameP1 then
      edGameValue1.Text := PV
    else
      edGameValue2.Text := PV;
    // Enable "Hit" and "Stay" buttons
    btDeal.Enabled := False;
    btHit.Enabled := True; btStay.Enabled := True;
    // Enable stake entry
    edStake.Text := '0'; edStake.Enabled := True; edStake.SetFocus;
  end;
end;

{ Button "Hit" pressed: Deal a further card }

procedure TfBlackjack2.btHitClick(Sender: TObject);

var
  NextCard, NStake, NBalance, PlayerBJValue1, PlayerBJValue2, PX, I: Integer;
  PV: string;
  PlayerCards: TCards;

begin
  if edStake.Text = '' then
    NStake := 0
  else
    NStake := StrToInt(edStake.Text);
  if sActualPlayer = sNameP1 then
    NBalance := StrToInt(edBalance1.Text)
  else
    NBalance := StrToInt(edBalance2.Text);
  // If a valid stake has been entered, deal next player-card
  if (NStake >= 10) and (NStake <= NBalance) then begin
    if sActualPlayer = sNameP1 then begin
      PlayerCards := aPlayer1Cards;
      iStake1 := NStake;
      PX := 1;
    end
    else begin
      PlayerCards := aPlayer2Cards;
      iStake2 := NStake;
      PX := 2;
    end;
    // Get the card from deck and display it
    iCard := NextDeckCard(aCardDeck, iCard);
    for I := 1 to 7 do begin
      if PlayerCards[I].FaceUp then
        NextCard := I;
    end;
    Inc(NextCard);
    PlayerCards[NextCard].FaceUp := True;
    PlayerCards[NextCard].Card := aCardDeck[iCard];
    // Calculate new Blackjack cards value
    if sActualPlayer = sNameP1 then begin
      aPlayer1Cards[NextCard] := PlayerCards[NextCard];
      BJValue(PlayerCards, iPlayer1BJValue1, iPlayer1BJValue2);
      PlayerBJValue1 := iPlayer1BJValue1; PlayerBJValue2 := iPlayer1BJValue2;
    end
    else begin
      aPlayer2Cards[NextCard] := PlayerCards[NextCard];
      BJValue(PlayerCards, iPlayer2BJValue1, iPlayer2BJValue2);
      PlayerBJValue1 := iPlayer2BJValue1; PlayerBJValue2 := iPlayer2BJValue2;
    end;
    DisplayCard(PX, NextCard, PlayerCards);
    if (PlayerBJValue1 = -21) and (PlayerBJValue2 = -21) then                            // Blackjack
      PV := 'BJ'
    else begin
      PV := IntToStr(PlayerBJValue1);
      if (PlayerBJValue2 <> PlayerBJValue1) and (PlayerBJValue2 <= 21) then
        PV += ' / ' + IntToStr(PlayerBJValue2);
    end;
    if sActualPlayer = sNameP1 then
      edGameValue1.Text := PV
    else
      edGameValue2.Text := PV;
    // If Blackjack cards value > 21: The player busted
    if PlayerBJValue1 > 21 then begin
      if sActualPlayer = sNameP1 then begin
        edGameResult1.Font.Color := clRed;
        edGameResult1.Text := 'You busted!';
        edBalance1.Text := IntToStr(StrToInt(edBalance1.Text) - Round(rMultBust * iStake1));  // Player 1 new balance
        bPlayer1Busted := True;
        if StrToInt(edBalance1.Text) < 10 then begin
          bPlayer1OnTheRocks := True;
        end;
      end
      else begin
        edGameResult2.Font.Color := clRed;
        edGameResult2.Text := 'You busted!';
        edBalance2.Text:= IntToStr(StrToInt(edBalance2.Text) - Round(rMultBust * iStake2));  // Player 2 new balance
        bPlayer2Busted := True;
        if StrToInt(edBalance2.Text) < 10 then begin
          bPlayer2OnTheRocks := True;
        end;
      end;
      if sActualPlayer = sNameP1 then begin
        // Actual player = player 1: Card deal for player 2
        sActualPlayer := sNameP2;
        edName1.Color := clDefault; edName2.Color := clAqua;
        btDeal.Enabled := True; btDeal.Click;                                            // deal player 2 cards
        btDeal.Enabled := False;
      end
      else begin
        // Actual player = player 2: Card deal for dealer or new round
        if bPlayer1Busted and bPlayer2Busted then begin
          // If both players busted, the dealer has won (new balances already calculated): Enable button to deal new cards
          sActualPlayer := sNameP1;
          edName2.Color := clDefault; edName1.Color := clAqua;
          btHit.Enabled := False; btStay.Enabled := False;
          btDeal.Enabled := True;
        end
        else begin
          // If one of the players didn't bust, dealer has to take cards
          btStay.Click;
        end;
        // Stake can't be changed after a hit has been made
        edStake.Enabled := False;
      end;
    end;
  end
  // Invalid stake (minimum of 10 is an arbitrary choice)
  else
    InvalidStake(NStake);
end;

{ Button "Stay" pressed: Deal dealer's cards and determine round winner }

procedure TfBlackjack2.btStayClick(Sender: TObject);

var
  DealerDoneValue, PlayerDoneValue, NextCard, NStake, NBalance: Integer;
  Winner: string;

begin
  if edStake.Text = '' then
    NStake := 0
  else
    NStake := StrToInt(edStake.Text);
  if sActualPlayer = sNameP1 then
    NBalance := StrToInt(edBalance1.Text)
  else
    NBalance := StrToInt(edBalance2.Text);
  // If a valid stake has been entered, continue with next player 2 resp. next dealer card
  if (NStake >= 10) and (NStake <= NBalance) then begin
    // Actual player = player 1: Card deal for player 2
    if sActualPlayer = sNameP1 then begin
      iStake1 := NStake;
      sActualPlayer := sNameP2;
      edName1.Color := clDefault; edName2.Color := clAqua;
      btDeal.Enabled := True; btDeal.Click;                                              // deal player 2 cards
      btDeal.Enabled := False;
    end
    // Actual player = player 2: Card deal for player 2
    else begin
      iStake2 := NStake;
      // Turn 2nd dealer card face-up
      aDealerCards[2].FaceUp := True;
      DisplayCard(0, 2, aDealerCards);
      NextCard := 3;
      DealerDoneValue := GetDealerDoneValue(iDealerHold, aDealerCards, iDealerBJValue1, iDealerBJValue2);
      // If dealer hasn't reached the hold value, continue dealing cards until he'll have
      if DealerDoneValue = 0 then begin
        repeat
          iCard := NextDeckCard(aCardDeck, iCard);
          aDealerCards[NextCard].FaceUp := True;
          aDealerCards[NextCard].Card := aCardDeck[iCard];
          DisplayCard(0, NextCard, aDealerCards);
          DealerDoneValue := GetDealerDoneValue(iDealerHold, aDealerCards, iDealerBJValue1, iDealerBJValue2);  // Calculate dealer Blackjack cards value
          if DealerDoneValue = 0 then
            // Dealer Blackjack cards value < hold value: Continue dealing
            Inc(NextCard);
        until DealerDoneValue <> 0;
      end;
      // Find the winners of this round
      if not bPlayer1Busted then begin
        // Do winner determination for player 1 (if he busted, it has already be done)
        if iPlayer1BJValue2 > 21 then
          PlayerDoneValue := iPlayer1BJValue1
        else
          PlayerDoneValue := iPlayer1BJValue2;
        iPlayer1ValueDone := PlayerDoneValue;
        edGameValue1.Text := IntToStr(iPlayer1ValueDone);
        FindWinner(DealerDoneValue, PlayerDoneValue, Winner);                            // Determine if player 1 wins or looses
        edGameResult1.Font.Color := clDefault;
        if Winner = 'player' then
          edGameResult1.Text := 'You win!'
        else if Winner = 'dealer' then begin
          if edGameResult1.Text <> 'You busted!' then
            edGameResult1.Text := 'Dealer wins';
        end
        else
          edGameResult1.Text := 'Stand-off';
        NBalance := CalculateBalance(StrToInt(edBalance1.Text), iStake1, iPlayer1ValueDone, edGameResult1.Text, rMultWin, bStandOff);
        edBalance1.Text := IntToStr(NBalance);
        if StrToInt(edBalance1.Text) < 10 then begin
          bPlayer1OnTheRocks := True;
        end;
      end;
      if not bPlayer2Busted then begin
        // Do winner determination for player 2 (if he busted, it has already be done)
        if iPlayer2BJValue2 > 21 then
          PlayerDoneValue := iPlayer2BJValue1
        else
          PlayerDoneValue := iPlayer2BJValue2;
        iPlayer2ValueDone := PlayerDoneValue;
        edGameValue2.Text := IntToStr(iPlayer2ValueDone);
        FindWinner(DealerDoneValue, PlayerDoneValue, Winner);
        edGameResult2.Font.Color := clDefault;
        if Winner = 'player' then
          edGameResult2.Text := 'You win!'
        else if Winner = 'dealer' then begin
          if edGameResult2.Text <> 'You busted!' then
            edGameResult2.Text := 'Dealer wins';
        end
        else
          edGameResult2.Text := 'Stand-off';
        NBalance := CalculateBalance(StrToInt(edBalance2.Text), iStake2, iPlayer2ValueDone, edGameResult2.Text, rMultWin, bStandOff);
        edBalance2.Text := IntToStr(NBalance);
        if StrToInt(edBalance2.Text) < 10 then begin
          bPlayer2OnTheRocks := True;
        end;
      end;
      // Start a ew round (enable button to deal new cards)
      sActualPlayer := sNameP1;
      edName2.Color := clDefault; edName1.Color := clAqua;
      edStake.Text := '0'; edStake.Enabled := False;
      btHit.Enabled := False; btStay.Enabled := False;
      btDeal.Enabled := True;
    end;
  end
  // Invalid stake entry
  else begin
    InvalidStake(NStake);
  end;
end;

{ Adapt players' names if one of the player name edit fields has been modified }

procedure TfBlackjack2.edName1EditingDone(Sender: TObject);

var
  OldName: string;

begin
  OldName := sNameP1;
  sNameP1 := edName1.Text;
  if sActualPlayer = OldName then begin
    sActualPlayer := sNameP1;
    laNameGame1.Caption := sNameP1;
    stNameCards1.Caption := sNameP1;
  end;
  laNameBalance1.Caption := sNameP1;
end;

procedure TfBlackjack2.edName2EditingDone(Sender: TObject);

var
  OldName: string;

begin
  OldName := sNameP2;
  sNameP2 := edName2.Text;
  if sActualPlayer = OldName then begin
    sActualPlayer := sNameP2;
    laNameGame1.Caption := sNameP2;
    stNameCards1.Caption := sNameP2;
  end;
  laNameBalance2.Caption := sNameP2;
end;

end.

