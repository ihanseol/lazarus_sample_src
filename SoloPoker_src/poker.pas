{*****************************************}
{* Main unit for "SoloPoker" application *}
{*****************************************}

unit poker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, help;

type
  TSuit  = (diamond, club, heart, spade);
  TCard  = record
    Suit   : TSuit;
    Rank   : Integer;
  end;
  TCardDeck = array of TCard;
  TCards = array[0..4, 0..4] of record
    Card  : TCard;
    FaceUp: Boolean;
  end;
  TCardImages = array[0..4, 0..4] of TImage;
  TEditFields = array[0..4] of TEdit;
  TLabels = array[0..4] of TLabel;
  TBorders = array[0..4, 0..4, 0..3] of TShape;
  {*********}
  { TfPoker }
  {*********}
  TfPoker = class(TForm)
    mOptionsTwoDecks: TMenuItem;
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsHands, mOptionsHands1, mOptionsHands2, mOptionsHands3, mOptionsHands4, mOptionsHands5: TMenuItem;
    mOptionsBetDefault, mOptionsBetDefault1, mOptionsBetDefault2, mOptionsBetDefault3: TMenuItem;
    mOptionsBetDefault4, mOptionsBetDefault5, mOptionsBetDefaultNone: TMenuItem;
    mOptionsDiscard, mOptionsFaceUp, mOptionsCredit: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label10, Label11: TLabel;
    laBet0, laBet1, laBet2, laBet3, laBet4: TLabel;
    edBet0, edBet1, edBet2, edBet3, edBet4: TEdit;
    edHand0, edHand1, edHand2, edHand3, edHand4: TEdit;
    edPayOut0, edPayOut1, edPayOut2, edPayOut3, edPayOut4: TEdit;
    edBalance, edBetTotal, edPayoutTotal: TEdit;
    imCard30, imCard31, imCard32, imCard33, imCard34, imCard40: TImage;
    imCard41, imCard42, imCard43, imCard44, imCard00, imCard01: TImage;
    imCard02, imCard03, imCard04, imCard10, imCard11, imCard12: TImage;
    imCard20, imCard21, imCard22, imCard23, imCard24, imCard13, imCard14: TImage;
    btAction: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsHands1Click(Sender: TObject);
    procedure mOptionsHands2Click(Sender: TObject);
    procedure mOptionsHands3Click(Sender: TObject);
    procedure mOptionsHands4Click(Sender: TObject);
    procedure mOptionsHands5Click(Sender: TObject);
    procedure mOptionsBetDefault1Click(Sender: TObject);
    procedure mOptionsBetDefault2Click(Sender: TObject);
    procedure mOptionsBetDefault3Click(Sender: TObject);
    procedure mOptionsBetDefault4Click(Sender: TObject);
    procedure mOptionsBetDefault5Click(Sender: TObject);
    procedure mOptionsBetDefaultNoneClick(Sender: TObject);
    procedure mOptionsTwoDecksClick(Sender: TObject);
    procedure mOptionsDiscardClick(Sender: TObject);
    procedure mOptionsFaceUpClick(Sender: TObject);
    procedure mOptionsCreditClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure imCard00Click(Sender: TObject);
    procedure imCard01Click(Sender: TObject);
    procedure imCard02Click(Sender: TObject);
    procedure imCard03Click(Sender: TObject);
    procedure imCard04Click(Sender: TObject);
    procedure imCard10Click(Sender: TObject);
    procedure imCard11Click(Sender: TObject);
    procedure imCard12Click(Sender: TObject);
    procedure imCard13Click(Sender: TObject);
    procedure imCard14Click(Sender: TObject);
    procedure imCard20Click(Sender: TObject);
    procedure imCard21Click(Sender: TObject);
    procedure imCard22Click(Sender: TObject);
    procedure imCard23Click(Sender: TObject);
    procedure imCard24Click(Sender: TObject);
    procedure imCard30Click(Sender: TObject);
    procedure imCard31Click(Sender: TObject);
    procedure imCard32Click(Sender: TObject);
    procedure imCard33Click(Sender: TObject);
    procedure imCard34Click(Sender: TObject);
    procedure imCard40Click(Sender: TObject);
    procedure imCard41Click(Sender: TObject);
    procedure imCard42Click(Sender: TObject);
    procedure imCard43Click(Sender: TObject);
    procedure imCard44Click(Sender: TObject);
  private
    iTotalCards, iBalance, iHands, iDefaultBet, iCard: Integer;
    aCardDeck: TCardDeck;
    aCards: TCards;
    aCards5: array[0..4] of TCard;
  end;

const
  InitialBalance = 100;
  WinHands: array[0..8] of string = (
    'Pair', 'Two Pair', 'Three of a Kind', 'Straight', 'Flush', 'Full House', 'Four of a Kind', 'Straight Flush', 'Royal Flush'
  );
  WinPayOuts: array[0..8] of Integer = (
    1, 2, 3, 4, 6, 9, 25, 50, 800
  );

var
  fPoker : TfPoker;
  imCards: TCardImages;
  edBets, edHands, edPayOuts: TEditFields;
  laBets: TLabels;
  shBorders: TBorders;

implementation

{$R *.lfm}

{ Reset the form }

procedure ResetForm(var Cards: TCards; var CardImages: TCardImages; var BetFields, HandFields, PayOutFields: TEditFields;
  var BetLabels: TLabels; var BorderShapes: TBorders; var TotalBetField, TotalPayOutField: TEdit; Hands, DefaultBet: Integer; Clear: Boolean);

var
  I, J, K: Integer;

begin
  TotalBetField.Text := '';
  TotalPayOutField.Text := '';
  for I := 0 to 4 do begin
    for J := 0 to 4 do begin
      if Clear then begin
        Cards[I, J].FaceUp := False;
        CardImages[I, J].Picture.Clear;
      end;
      for K := 0 to 3 do
        BorderShapes[I, J, K].Visible := False;
    end;
    BetLabels[I].Visible := True;  BetFields[I].Visible := True;
    HandFields[I].Visible := True; PayOutFields[I].Visible := True;
    BetFields[I].Text := ''; HandFields[I].Text := ''; PayOutFields[I].Text := '';
    if I < Hands then begin
      // Hands actually played
      if DefaultBet <> -1 then begin
        // Fill in default bets value
        BetFields[I].Text := IntToStr(DefaultBet);
        if TotalBetField.Text = '' then
          TotalBetField.Text := IntToStr(DefaultBet)
        else
          TotalBetField.Text := IntToStr(StrToInt(TotalBetField.Text) + DefaultBet);
      end;
    end
    else begin
      // Hands actually not played
      BetLabels[I].Visible := False;  BetFields[I].Visible := False;
      HandFields[I].Visible := False; PayOutFields[I].Visible := False;
    end;
  end;
end;

{ Shuffle the cards }

procedure Shuffle(var CardDeck: TCardDeck);

var
  N, R1, R2, I: Integer;
  Card: TCard;

begin
  N := Length(CardDeck);
  for I := 1 TO 250 do begin
    R1 := Random(N); R2 := Random(N);
    Card := CardDeck[R1]; CardDeck[R1] := CardDeck[R2]; CardDeck[R2] := Card;
  end;
end;

{ Get next card (index) from card deck }

function NextDeckCard(var CardDeck: TCardDeck; N: Integer): Integer;

begin
  Inc(N);
  if N >= Length(CardDeck) then begin
    // All cards done: Reshuffle and continue with first card
    Shuffle(CardDeck);
    N := 0;
  end;
  Result := N;
end;

{ Get card image filename }

function CardFilename(Card: TCard): string;

var
  N, R: Integer;

begin
  if Card.Rank = 14 then
    R := 1
  else
    R := Card.Rank;
  N := 100 + Ord(Card.Suit) * 13 + R;
  CardFilename := IntToStr(N) + '.png';
end;

{ Display given card }

procedure DisplayCard(Hand, N: Integer; var Cards: TCards; var CardImages: TCardImages);

const
  CardsPath    = 'cmagedeck/';
  DeckCardFile = '155.png';

var
  CardFile: string;

begin
  if Cards[Hand, N].FaceUp then
    CardFile := CardFilename(Cards[Hand, N].Card)
  else
    CardFile := DeckCardFile;
  CardFile := CardsPath + CardFile; DoDirSeparators(CardFile);
  CardImages[Hand, N].Picture.LoadFromFile(CardFile);
end;

{ Select given card (by making borders visible)}

procedure SelectCard(var BorderShapes: TBorders; Hand, N: Integer);

var
  K: Integer;

begin
  if fPoker.btAction.Caption = 'Hit' then begin
    // Cards selection only active if player has to hit
    for K := 0 to 3 do begin
      // Select, or unselect the card
      if BorderShapes[Hand, N, K].Visible then
        BorderShapes[Hand, N, K].Visible := False
      else
        BorderShapes[Hand, N, K].Visible := True;
    end;
  end;
end;

{ Poker winning hands: Straight Flush and Royal Flush }

function CheckStraightFlush(var Cards: array of TCard): string;

var
  I, J: Integer;
  Hand: string;
  IsStraightFlush: Boolean;
  Card: TCard;

begin
  Hand := 'Nothing';
  // Sort the cards
  for I := 0 to 3 do begin
    for J := I + 1 to 4 do begin
      if Cards[I].Rank > Cards[J].Rank then begin
        Card := Cards[I]; Cards[I] := Cards[J]; Cards[J] := Card;
      end;
    end;
  end;
  // Check for Straight Flush
  IsStraightFlush := True;
  for I := 1 to 4 do begin
    if (Cards[I].Suit <> Cards[I - 1].Suit) or (Cards[I].Rank - Cards[I - 1].Rank <> 1) then
      isStraightFlush := False;
  end;
  if IsStraightFlush then begin
    if Cards[4].Rank = 14 then
      Hand := 'Royal Flush'
    else
      Hand := 'Straight Flush';
  end;
  Result := Hand;
end;

{ Poker winning hands: Flush }

function CheckFlush(var Cards: array of TCard): string;

var
  Count, I: Integer;
  Hand: string;
  Suit: TSuit;

begin
  Hand := 'Nothing';
  for Suit := diamond to spade do begin
    Count := 0;
    for I := 0 to 4 do begin
      if Cards[I].Suit = Suit then
        Inc(Count);
    end;
    if Count = 5 then
      Hand := 'Flush';
  end;
  Result := Hand;
end;

{ Poker winning hands: Straight }

function CheckStraight(var Cards: array of TCard): string;

var
  I, J: Integer;
  Hand: string;
  IsStraight: Boolean;
  Card: TCard;

begin
  Hand := 'Nothing';
  // Sort the cards
  for I := 0 to 3 do begin
    for J := I + 1 to 4 do begin
      if Cards[I].Rank > Cards[J].Rank then begin
        Card := Cards[I]; Cards[I] := Cards[J]; Cards[J] := Card;
      end;
    end;
  end;
  // Check for Straight
  IsStraight := True;
  for I := 1 to 4 do begin
    if Cards[I].Rank - Cards[I - 1].Rank <> 1 then
      isStraight := False;
  end;
  // Check for "special case" Straight Ace, 2, 3, 4, 5
  if not isStraight then begin
    if (Cards[0].Rank = 2) and (Cards[1].Rank = 3) and (Cards[2].Rank = 4) and (Cards[3].Rank = 5) and (Cards[4].Rank = 14) then
      isStraight := True;
  end;
  if IsStraight then
    Hand := 'Straight';
  Result := Hand;
end;

{ Check for 2, 3, or 4 cards of same rank }

function CheckRankTwoThreeFour(var Cards: array of TCard; N: Integer): Integer;

// The function returns the card rank

var
  Rank, Count, R, I: Integer;

begin
  Rank := 0;
  for R := 1 to 14 do begin
    Count := 0;
    for I := 0 to 4 do begin
      if Cards[I].Rank = R then
        Inc(Count);
    end;
    if Count = N then
      Rank := R;
  end;
  Result := Rank;
end;

{ Poker winning hands: Four of a Kind }

function CheckFour(var Cards: array of TCard): string;

var
  Hand: string;

begin
  Hand := 'Nothing';
  if CheckRankTwoThreeFour(Cards, 4) <> 0 then
    Hand := 'Four of a Kind';
  Result := Hand;
end;

{ Poker winning hands: Three of a Kind }

function CheckThree(var Cards: array of TCard): string;

var
  Hand: string;

begin
  Hand := 'Nothing';
  if CheckRankTwoThreeFour(Cards, 3) <> 0 then
    Hand := 'Three of a Kind';
  Result := Hand;
end;

{ Poker winning hands: Pair }

function CheckPair(var Cards: array of TCard): string;

var
  Hand: string;

begin
  Hand := 'Nothing';
  if CheckRankTwoThreeFour(Cards, 2) >=11 then                                 // pairs of 2, 3, ... 10 will not pay out
    Hand := 'Pair';
  Result := Hand;
end;

{ Poker winning hands: Two Pair }

function CheckTwoPair(var Cards: array of TCard): string;

var
  Rank2, I: Integer;
  Hand: string;
  Cards2: array[0..4] of TCard;

begin
  Hand := 'Nothing';
  // Check for 2 cards of same rank
  Rank2 := CheckRankTwoThreeFour(Cards, 2);
  if Rank2 <> 0 then begin
    // If there is a Pair, remove the corresponding cards and check
    // if there are 2 cards of same rank among the remaining ones
    Cards2 := Cards;
    for I := 0 to 4 do begin
      if Cards2[I].Rank = Rank2 then
        Cards2[I].Rank := -I;
    end;
    if CheckRankTwoThreeFour(Cards2, 2) <> 0 then
      Hand := 'Two Pair';
  end;
  Result := Hand;
end;

{ Poker winning hands: Full House }

function CheckFullHouse(var Cards: array of TCard): string;

var
  Rank3, Rank2: Integer;
  Hand: string;

begin
  Hand := 'Nothing';
  // Check for three cards of same rank
  Rank3 := CheckRankTwoThreeFour(Cards, 3);
  if Rank3 <> 0 then begin
    // If there are Three of a Kind, check for 2 cards of the same rank. If the rank of the Pair
    // is different from the rank of the Three of a Kind, the hand is a Full House
    Rank2 := CheckRankTwoThreeFour(Cards, 2);
    if Rank2 <> 0 then begin
      if Rank2 <> Rank3 then
        Hand := 'Full House';
    end;
  end;
  Result := Hand;
end;

{*********}
{ TfPoker }
{*********}

{ Initialize the game at application start }

procedure TfPoker.FormCreate(Sender: TObject);

var
  I, J, K: Integer;

begin
  // Create an array containing the 25 card-images
  imCards[0, 0] := imCard00; imCards[0, 1] := imCard01; imCards[0, 2] := imCard02; imCards[0, 3] := imCard03; imCards[0, 4] := imCard04;
  imCards[1, 0] := imCard10; imCards[1, 1] := imCard11; imCards[1, 2] := imCard12; imCards[1, 3] := imCard13; imCards[1, 4] := imCard14;
  imCards[2, 0] := imCard20; imCards[2, 1] := imCard21; imCards[2, 2] := imCard22; imCards[2, 3] := imCard23; imCards[2, 4] := imCard24;
  imCards[3, 0] := imCard30; imCards[3, 1] := imCard31; imCards[3, 2] := imCard32; imCards[3, 3] := imCard33; imCards[3, 4] := imCard34;
  imCards[4, 0] := imCard40; imCards[4, 1] := imCard41; imCards[4, 2] := imCard42; imCards[4, 3] := imCard43; imCards[4, 4] := imCard44;
  // Create an array containing the 5 bet labels
  laBets[0] := laBet0; laBets[1] := laBet1; laBets[2] := laBet2; laBets[3] := laBet3; laBets[4] := laBet4;
  // Create an array containing the 5 bet, winning hands, and payout edit fields
  edBets[0] := edBet0; edBets[1] := edBet1; edBets[2] := edBet2; edBets[3] := edBet3; edBets[4] := edBet4;
  edHands[0] := edHand0; edHands[1] := edHand1; edHands[2] := edHand2; edHands[3] := edHand3; edHands[4] := edHand4;
  edPayOuts[0] := edPayOut0; edPayOuts[1] := edPayOut1; edPayOuts[2] := edPayOut2; edPayOuts[3] := edPayOut3; edPayOuts[4] := edPayOut4;
  // Dynamically create the border shape objects
  for I := 0 to 4 do begin
    for J := 0 to 4 do begin
      for K := 0 to 3 do begin
        shBorders[I, J, K] := TShape.Create(shBorders[I, J, K]);
        shBorders[I, J, K].Parent := Self;
        shBorders[I, J, K].Shape := stRectangle;
        if (K = 0) or (K = 1) then begin
          // Upper and lower border
          shBorders[I, J, K].Width := 71;
          shBorders[I, J, K].Height := 4;
          shBorders[I, J, K].Left := imCards[I, J].Left;
          if K = 0 then
            shBorders[I, J, K].Top := imCards[I, J].Top
          else
            shBorders[I, J, K].Top := imCards[I, J].Top + 92;
        end
        else begin
          // Left and right border
          shBorders[I, J, K].Width := 4;
          shBorders[I, J, K].Height := 96;
          shBorders[I, J, K].Top := imCards[I, J].Top;
          if K = 2 then
            shBorders[I, J, K].Left := imCards[I, J].Left
          else
            shBorders[I, J, K].Left := imCards[I, J].Left + 67;
        end;
        shBorders[I, J, K].Brush.Color := clLime;
        shBorders[I, J, K].Pen.Color := clLime;
        shBorders[I, J, K].Visible := False;
      end;
    end;
  end;
  // Startup values and random number generator initialization
  iTotalCards := 52; iHands := 5; iDefaultBet := -1;
  Randomize;
  // Start a new game
  mGameNew.Click;
end;

{ Menu item: "Game > New": Start a new game }

procedure TfPoker.mGameNewClick(Sender: TObject);

var
 Suit: TSuit;
  Rank, N, I, J: Integer;

begin
  mOptionsCredit.Enabled := True;                                              // negative balance option only available at atart of game
  btAction.Enabled := True;                                                    // action button will be disabled if player has no money left
  iBalance := InitialBalance; edBalance.Text := IntToStr(iBalance);
  ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
  // Fill the carddeck (1 x 52 cards or 2 x 52 cards)
  SetLength(aCardDeck, iTotalCards);
  N := 0; J := iTotalCards div 52 - 1;
  for Suit := diamond to spade do begin
    for Rank := 1 to 13 do begin
      for I := 0 to J do begin
        Inc(N);
        aCardDeck[N - 1].Suit := Suit;
        if Rank = 1 then
          aCardDeck[N - 1].Rank := 14
        else
          aCardDeck[N - 1].Rank := Rank;
      end;
    end;
  end;
  Shuffle(aCardDeck);
  iCard := -1;                                                                 // next card will be the first one on the dech
end;

{ Menu item: "Game > Exit": Exit application }

procedure TfPoker.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Number of hands > ...": Select number of hands to play (1 to 5) }

procedure TfPoker.mOptionsHands1Click(Sender: TObject);

begin
  if btAction.Caption = 'Deal' then begin
    mOptionsHands1.Checked := True; mOptionsHands2.Checked := False; mOptionsHands3.Checked := False;
    mOptionsHands4.Checked := False; mOptionsHands5.Checked := False;
    iHands := 1;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
  end;
end;

procedure TfPoker.mOptionsHands2Click(Sender: TObject);

begin
  if btAction.Caption = 'Deal' then begin
    mOptionsHands1.Checked := False; mOptionsHands2.Checked := True; mOptionsHands3.Checked := False;
    mOptionsHands4.Checked := False; mOptionsHands5.Checked := False;
    iHands := 2;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
  end;
end;

procedure TfPoker.mOptionsHands3Click(Sender: TObject);

begin
  if btAction.Caption = 'Deal' then begin
    mOptionsHands1.Checked := False; mOptionsHands2.Checked := False; mOptionsHands3.Checked := True;
    mOptionsHands4.Checked := False; mOptionsHands5.Checked := False;
    iHands := 3;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
  end;
end;

procedure TfPoker.mOptionsHands4Click(Sender: TObject);

begin
  if btAction.Caption = 'Deal' then begin
    mOptionsHands1.Checked := False; mOptionsHands2.Checked := False; mOptionsHands3.Checked := False;
    mOptionsHands4.Checked := True; mOptionsHands5.Checked := False;
    iHands := 4;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
  end;
end;

procedure TfPoker.mOptionsHands5Click(Sender: TObject);

begin
  if btAction.Caption = 'Deal' then begin
    mOptionsHands1.Checked := False; mOptionsHands2.Checked := False; mOptionsHands3.Checked := False;
    mOptionsHands4.Checked := False; mOptionsHands5.Checked := True;
    iHands := 5;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
  end;
end;

{ Menu items "Options > Default bet > ...": Select default bet (1 to 5, or none) }

procedure TfPoker.mOptionsBetDefault1Click(Sender: TObject);

begin
  if (btAction.Caption = 'Deal') or (btAction.Caption = 'Bet') then begin
    mOptionsBetDefault1.Checked := True; mOptionsBetDefault2.Checked := False; mOptionsBetDefault3.Checked := False;
    mOptionsBetDefault4.Checked := False; mOptionsBetDefault5.Checked := False; mOptionsBetDefaultNone.Checked := False;
    iDefaultBet := 1;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, False);
  end;
end;

procedure TfPoker.mOptionsBetDefault2Click(Sender: TObject);

begin
  if (btAction.Caption = 'Deal') or (btAction.Caption = 'Bet') then begin
    mOptionsBetDefault1.Checked := False; mOptionsBetDefault2.Checked := True; mOptionsBetDefault3.Checked := False;
    mOptionsBetDefault4.Checked := False; mOptionsBetDefault5.Checked := False; mOptionsBetDefaultNone.Checked := False;
    iDefaultBet := 2;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, False);
  end;
end;

procedure TfPoker.mOptionsBetDefault3Click(Sender: TObject);

begin
  if (btAction.Caption = 'Deal') or (btAction.Caption = 'Bet') then begin
    mOptionsBetDefault1.Checked := False; mOptionsBetDefault2.Checked := False; mOptionsBetDefault3.Checked := True;
    mOptionsBetDefault4.Checked := False; mOptionsBetDefault5.Checked := False; mOptionsBetDefaultNone.Checked := False;
    iDefaultBet := 3;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, False);
  end;
end;

procedure TfPoker.mOptionsBetDefault4Click(Sender: TObject);

begin
  if (btAction.Caption = 'Deal') or (btAction.Caption = 'Bet') then begin
    mOptionsBetDefault1.Checked := False; mOptionsBetDefault2.Checked := False; mOptionsBetDefault3.Checked := False;
    mOptionsBetDefault4.Checked := True; mOptionsBetDefault5.Checked := False; mOptionsBetDefaultNone.Checked := False;
    iDefaultBet := 4;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, False);
  end;
end;

procedure TfPoker.mOptionsBetDefault5Click(Sender: TObject);

begin
  if (btAction.Caption = 'Deal') or (btAction.Caption = 'Bet') then begin
    mOptionsBetDefault1.Checked := False; mOptionsBetDefault2.Checked := False; mOptionsBetDefault3.Checked := False;
    mOptionsBetDefault4.Checked := False; mOptionsBetDefault5.Checked := True; mOptionsBetDefaultNone.Checked := False;
    iDefaultBet := 5;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, False);
  end;
end;

procedure TfPoker.mOptionsBetDefaultNoneClick(Sender: TObject);

begin
  if (btAction.Caption = 'Deal') or (btAction.Caption = 'Bet') then begin
    mOptionsBetDefault1.Checked := False; mOptionsBetDefault2.Checked := False; mOptionsBetDefault3.Checked := False;
    mOptionsBetDefault4.Checked := False; mOptionsBetDefault5.Checked := False; mOptionsBetDefaultNone.Checked := True;
    iDefaultBet := -1;
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, False);
  end;
end;

{ Menu item: "Options > Use 2 card decks": Toggle use 1 card deck/use 2 card decks }

procedure TfPoker.mOptionsTwoDecksClick(Sender: TObject);

begin
  if mOptionsTwoDecks.Checked then begin
    mOptionsTwoDecks.Checked := False;
    iTotalCards := 52;
  end
  else begin
    mOptionsTwoDecks.Checked := True;
    iTotalCards := 104;
  end;
  mGameNew.Click;
end;

{ Menu item: "Options > Click to discard (replace)": Toggle click to keep/discard cards }

procedure TfPoker.mOptionsDiscardClick(Sender: TObject);

var
  I, J, K: Integer;
  Colour: TColor;

begin
  if mOptionsDiscard.Checked then begin
    mOptionsDiscard.Checked := False;
    Colour := clLime;
  end
  else begin
    mOptionsDiscard.Checked := True;
    Colour := clRed;
  end;
  for I := 0 to 4 do begin
    for J := 0 to 4 do begin
      for K := 0 to 3 do begin
        shBorders[I, J, K].Brush.Color := Colour;
        shBorders[I, J, K].Pen.Color := Colour;
      end;
    end;
  end;
end;

{ Menu item: "Options > Enable face-up card": Toggle enable/disable face-up card }

procedure TfPoker.mOptionsFaceUpClick(Sender: TObject);

begin
  if btAction.Caption = 'Deal' then begin
    // Option is only available when a new round is started
    if mOptionsFaceUp.Checked then
      mOptionsFaceUp.Checked := False
    else
      mOptionsFaceUp.Checked := True;
  end;
end;

{ Menu item: "Options > Enable negative balance": Toggle enable/disable negetive balance (credit) }

procedure TfPoker.mOptionsCreditClick(Sender: TObject);

begin
  if mOptionsCredit.Checked then
    mOptionsCredit.Checked := False
  else
    mOptionsCredit.Checked := True;
end;

{ Menu item: "Help > Help": Display application help }

procedure TfPoker.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item: "Help > About": Display application about }

procedure TfPoker.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Solo Poker:' + LineEnding;
  S += 'Free PC Poker game for 1 player.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, July 2024 - February 2025.';
  MessageDlg('About "SoloPoker"', S, mtInformation, [mbOK], 0);
end;

{ Button "Deal/Bet/Hit" pushed: Deal new cards, make bet, exchange cards selected by player and determine winning hands }

procedure TfPoker.btActionClick(Sender: TObject);

var
  Balance, Bet, TotalBet, PayOut, TotalPayOut, N, I, J, K: Integer;
  Hand: string;
  Mess: string;

begin
  mOptionsCredit.Enabled := False;                                             // this option is only available when a new game is started
  // Button "Deal" pushed: Deal new cards
  if btAction.Caption = 'Deal' then begin
    ResetForm(aCards, imCards, edBets, edHands, edPayOuts, laBets, shBorders, edBetTotal, edPayoutTotal, iHands, iDefaultBet, True);
    for I := 0 to iHands - 1 do begin
      // For each hand actually played
      for J := 0 to 4 do begin
        // For each of the 5 cards in this hand
        iCard := NextDeckCard(aCardDeck, iCard);                               // get next card from the deck
        aCards[I, J].Card := aCardDeck[iCard];
        if (J = 0) then begin
          // First card of hand may be face-up (if this option is checked)
          if mOptionsFaceUp.Checked then
            aCards[I, J].FaceUp := True
          else
            aCards[I, J].FaceUp := False;
        end;
        DisplayCard(I, J, aCards, imCards);
      end;
    end;
    edBet0.SetFocus;
    btAction.Caption := 'Bet';                                                 // next action = make a bet
  end
  // Button "Bet" pushed: Player makes bets
  else if btAction.Caption = 'Bet' then begin
    TotalBet := 0; Mess := '';
    Balance := StrToInt(edBalance.Text);
    for I := iHands - 1 downto 0 do begin
      // Player must fill in all bat fields (0 is possible)
      if edBets[I].Text = '' then begin
        Mess := 'Missing bet(s)!';
        N := I;
      end
      else begin
        Bet := StrToInt(edBets[I].Text);
        if (Bet < 0) or (Bet > 5) then begin
          // Bet must be between 0 and 5
          Mess := 'Invalid bet(s)!';
          N := I;                                                              // N is used to focus bad input field
        end
        else
          TotalBet += Bet;
      end;
    end;
    if Mess = '' then begin
      // All bets filled in; check total bet
      edBetTotal.Text := IntToStr(TotalBet);
      Balance -= TotalBet;                                                     // updated balance (total bets subtracted)
      // New balance must not be negative, resp. not ne less than players credit
      if mOptionsCredit.Checked and (Balance < -InitialBalance div 2) then
        Mess := 'Sorry, you are going beyond your credit!'
      else if (not mOptionsCredit.Checked) and (Balance < 0) then
        Mess := 'Sorry, you are not allowed to get credit!';
      N := 0;                                                                  // focus first input field
    end;
    if Mess = '' then begin
      // Bets ok, turn all cards face-up
      edBalance.Text := IntToStr(Balance);
      for I := 0 to iHands - 1 do begin
        for J := 0 to 4 do begin
          aCards[I, J].FaceUp := True;
          DisplayCard(I, J, aCards, imCards);
        end;
      end;
      btAction.Caption := 'Hit';                                               // next action = select cards to keep/discard
    end
    else begin
      // Some user input error...
      MessageDlg('Solo Poker', Mess, mtError, [mbOK], 0);
      edBets[N].SetFocus;
    end;
  end
  // Button "Hit" pushed: Exchange cards selected by player and determine winning hands
  else begin
    // Exchange cards selected by player
    for I := 0 to iHands - 1 do begin
      for J := 0 to 4 do begin
        if (mOptionsDiscard.Checked and shBorders[I, J, 0].Visible) or ((not mOptionsDiscard.Checked) and (not shBorders[I, J, 0].Visible)) then begin
          iCard := NextDeckCard(aCardDeck, iCard);
          aCards[I, J].Card := aCardDeck[iCard];
          DisplayCard(I, J, aCards, imCards);
        end;
        for K := 0 to 3 do
          shBorders[I, J, K].Visible := False;
      end;
    end;
    // Determine winning hands
    TotalPayOut := 0; Balance := StrToInt(edBalance.Text);
    for I := 0 to iHands - 1 do begin
      // Determine winning hand for this hand
      for J := 0 to 4 do begin
        aCards5[J] := aCards[I, J].Card;
      end;
      Hand := CheckStraightFlush(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckFour(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckFullHouse(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckFlush(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckStraight(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckThree(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckTwoPair(aCards5);
      if Hand = 'Nothing' then
        Hand := CheckPair(aCards5);
      edHands[I].Text := Hand;
      if Hand <> 'Nothing' then begin
        // This hand is a winning hand
        for J := 0 to 8 do begin
          // Determine payout for this winning hand
          if Hand = WinHands[J] then begin
            Bet := StrToInt(edBets[I].Text);
            PayOut := Bet * WinPayOuts[J];
            edPayOuts[I].Text := 'Wins ' + IntToStr(Bet) + ' x ' + IntToStr(WinPayOuts[J]);
            TotalPayOut += PayOut;
          end;
        end;
      end;
    end;
    // Display total payout and updated balance (adding total payout)
    Balance += TotalPayOut;
    edBalance.Text := IntToStr(Balance);
    edPayoutTotal.Text := IntToStr(TotalPayout);
    btAction.Caption := 'Deal';                                                // next action = deal cards (new round)
    if (mOptionsCredit.Checked and (Balance = -InitialBalance div 2)) or (not mOptionsCredit.Checked and (Balance = 0)) then begin
      // If player has no more money left, terminate the game (disable action button)
      MessageDlg('Game over', 'Sorry, but you have no more money left. Maybe, you will have more luck next week. Bye.', mtInformation, [mbOK], 0);
      btAction.Enabled := False;
    end;
  end;
end;

{ Card image clicked by player: Select/unselect this card }

procedure TfPoker.imCard00Click(Sender: TObject);

begin
  SelectCard(shBorders, 0, 0);
end;

procedure TfPoker.imCard01Click(Sender: TObject);

begin
  SelectCard(shBorders, 0, 1);
end;

procedure TfPoker.imCard02Click(Sender: TObject);

begin
  SelectCard(shBorders, 0, 2);
end;

procedure TfPoker.imCard03Click(Sender: TObject);

begin
  SelectCard(shBorders, 0, 3);
end;

procedure TfPoker.imCard04Click(Sender: TObject);

begin
  SelectCard(shBorders, 0, 4);
end;

procedure TfPoker.imCard10Click(Sender: TObject);

begin
  SelectCard(shBorders, 1, 0);
end;

procedure TfPoker.imCard11Click(Sender: TObject);

begin
  SelectCard(shBorders, 1, 1);
end;

procedure TfPoker.imCard12Click(Sender: TObject);

begin
  SelectCard(shBorders, 1, 2);
end;

procedure TfPoker.imCard13Click(Sender: TObject);

begin
  SelectCard(shBorders, 1, 3);
end;

procedure TfPoker.imCard14Click(Sender: TObject);

begin
  SelectCard(shBorders, 1, 4);
end;

procedure TfPoker.imCard20Click(Sender: TObject);

begin
  SelectCard(shBorders, 2, 0);
end;

procedure TfPoker.imCard21Click(Sender: TObject);

begin
  SelectCard(shBorders, 2, 1);
end;

procedure TfPoker.imCard22Click(Sender: TObject);

begin
  SelectCard(shBorders, 2, 2);
end;

procedure TfPoker.imCard23Click(Sender: TObject);

begin
  SelectCard(shBorders, 2, 3);
end;

procedure TfPoker.imCard24Click(Sender: TObject);

begin
  SelectCard(shBorders, 2, 4);
end;

procedure TfPoker.imCard30Click(Sender: TObject);

begin
  SelectCard(shBorders, 3, 0);
end;

procedure TfPoker.imCard31Click(Sender: TObject);

begin
  SelectCard(shBorders, 3, 1);
end;

procedure TfPoker.imCard32Click(Sender: TObject);

begin
  SelectCard(shBorders, 3, 2);
end;

procedure TfPoker.imCard33Click(Sender: TObject);

begin
  SelectCard(shBorders, 3, 3);
end;

procedure TfPoker.imCard34Click(Sender: TObject);

begin
  SelectCard(shBorders, 3, 4);
end;

procedure TfPoker.imCard40Click(Sender: TObject);

begin
  SelectCard(shBorders, 4, 0);
end;

procedure TfPoker.imCard41Click(Sender: TObject);

begin
  SelectCard(shBorders, 4, 1);
end;

procedure TfPoker.imCard42Click(Sender: TObject);

begin
  SelectCard(shBorders, 4, 2);
end;

procedure TfPoker.imCard43Click(Sender: TObject);

begin
  SelectCard(shBorders, 4, 3);
end;

procedure TfPoker.imCard44Click(Sender: TObject);

begin
  SelectCard(shBorders, 4, 4);
end;

end.

