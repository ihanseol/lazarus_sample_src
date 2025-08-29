{ ***************************************** }
{ Main unit for FPC application "BlackJack" }
{ ***************************************** }

unit blackjack_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls;

{ Types }

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
  TImages = array[1..2, 1..8] of TImage;
  // TForm1
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Game: TMenuItem;
    Game1: TMenuItem;
    Game2: TMenuItem;
    Options: TMenuItem;
    Options1: TMenuItem;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    DealerValue: TEdit;
    PlayerValue: TEdit;
    GameResult: TEdit;
    DealerScore: TEdit;
    PlayerScore: TEdit;
    ButtonDeal: TButton;
    ButtonHit: TButton;
    ButtonStay: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Game1Click(Sender: TObject);
    procedure Game2Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure ButtonDealClick(Sender: TObject);
    procedure ButtonHitClick(Sender: TObject);
    procedure ButtonStayClick(Sender: TObject);
  end;

{ Global variables }

var
  Form1 : TForm1;
  Images: TImages;
  CardDeck: TCardDeck;
  DealerCards, PlayerCards: TCards;
  DealerBJValue1, PlayerBJValue1, DealerBJValue2, PlayerBJValue2, DealerTotal, PlayerTotal: Integer;
  DealerHold, NCard: Integer;

procedure NewGame;
procedure ResetGame;
procedure Shuffle(var CardDeck: TCardDeck);
function NextDeckCard(N: Integer): Integer;
procedure BJValue(Cards: TCards; var V1, V2: Integer);
function GetDealerDoneValue: Integer;
function CardFilename(Card: TCard): string;
procedure DisplayCard(Player, IX: Integer; Cards: TCards);

implementation

{$R *.lfm}

{ New game }

procedure NewGame;

begin
  DealerTotal := 0; PlayerTotal := 0;
  Form1.DealerScore.Caption := '0';
  Form1.PlayerScore.Caption := '0';
  ResetGame;
  Shuffle(CardDeck);
  NCard := 0;
end;

{ Reset the game (for new round) }

procedure ResetGame;

var
  I: Integer;

begin
  DealerBJValue1 := 0; PlayerBJValue1 := 0;
  DealerBJValue2 := 0; PlayerBJValue2 := 0;
  Form1.DealerValue.Caption := '0'; Form1.PlayerValue.Caption := '0';
  for I := 1 to 8 do begin
    PlayerCards[I].FaceUp := False;
    DealerCards[I].FaceUp := False;
    Images[1, I].Picture.Clear;
    Images[2, I].Picture.Clear;
  end;
  Form1.GameResult.Caption := '';
  Form1.ButtonDeal.Enabled := True;
  Form1.ButtonHit.Enabled := False;
  Form1.ButtonStay.Enabled := False;
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

{ Function: Get next card (index) from deck }

function NextDeckCard(N: Integer): Integer;

begin
  Inc(N);
  if N > 52 then begin
    Shuffle(CardDeck);
    N := 1;
  end;
  NextDeckCard := N;
end;

{ Calculate cards' Blackjack value }

procedure BJValue(Cards: TCards; var V1, V2: Integer);

var
  I: Integer;

begin
  V1 := 0; V2 := 0;
  for I := 1 to 8 do begin
    if Cards[I].FaceUp then begin
      V1 := V1 + Cards[I].Card.BJValue;
      // As may be counted as 1 or as 11
      if Cards[I].Card.Rank = 1 then
        V2 := V2 + 11
      else
        V2 := V2 + Cards[I].Card.BJValue;
    end;
  end;
  // Blackjack (2 aces)
  if (V1 = 2) and (V2 = 22) then begin
    V1 := -21; V2 := -21;                                                      // use -21 to represent Blackjack
  end;
end;

{ Get dealer's hold Blackjack value }

function GetDealerDoneValue: Integer;

var
  DoneValue: Integer;

begin
  DoneValue := 0;
  BJValue(DealerCards, DealerBJValue1, DealerBJValue2);
  if (DealerBJValue1 = -21) and (DealerBJValue2 = -21) then begin              // Blackjack
    Form1.DealerValue.Text := 'BJ';
    DoneValue := -21;
  end
  else begin
    if DealerBJValue2 <= 21 then
      Form1.DealerValue.Text := IntToStr(DealerBJValue2)
    else
      Form1.DealerValue.Text := IntToStr(DealerBJValue1);
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

{ Function: Get card image filename }

function CardFilename(Card: TCard): string;

var
  N: Integer;

begin
  N := 100 + Ord(Card.Suit) * 13 + Card.Rank;
  CardFilename := IntToStr(N) + '.png';
end;

{ Display the dealer/player's cards }

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
  DoDirSeparators(CardFile);                                                   // platform independent file path
  Images[Player, IX].Picture.LoadFromFile(CardFile);
end;

{ ----------------- }
{ TForm1 procedures }
{ ----------------- }

{ Initialize the game at application start }

procedure TForm1.FormCreate(Sender: TObject);

var
  Suit: TSuit;
  N, Rank: Integer;

begin
  Randomize;
  // Create an array containing the 16 form-images
  Images[1, 1] := Image1;  Images[1, 2] := Image2;  Images[1, 3] := Image3;  Images[1, 4] := Image4;
  Images[1, 5] := Image5;  Images[1, 6] := Image6;  Images[1, 7] := Image7;  Images[1, 8] := Image8;
  Images[2, 1] := Image9;  Images[2, 2] := Image10; Images[2, 3] := Image11; Images[2, 4] := Image12;
  Images[2, 5] := Image13; Images[2, 6] := Image14; Images[2, 7] := Image15; Images[2, 8] := Image16;
  // Fill the carddeck
  N := 0;
  for Suit := diamond to spade do begin
    for Rank := 1 to 13 do begin
      Inc(N);
      CardDeck[N].Suit := Suit;
      CardDeck[N].Rank := Rank;
      if Rank <= 10 then
        CardDeck[N].BJValue := Rank
      else
        CardDeck[N].BJValue := 10;
    end;
  end;
  DealerHold := 17;
  NewGame;
end;

{ Menu selection: New game }

procedure TForm1.Game1Click(Sender: TObject);

begin
  NewGame;
end;

{ Menu selection: Exit application }

procedure TForm1.Game2Click(Sender: TObject);

begin
  Close();
end;

{ Menu selection: Options > Dealer holds on 18 }

procedure TForm1.Options1Click(Sender: TObject);

begin
  if Options1.Checked then begin
    Options1.Checked := False;
    DealerHold := 17;
  end
  else begin
    Options1.Checked := True;
    DealerHold := 18;
  end;
end;

{ Button pressed: Deal }

procedure TForm1.ButtonDealClick(Sender: TObject);

begin
  ResetGame;
  // Distribute first 3 cards
  NCard := NextDeckCard(NCard); PlayerCards[1].Card := CardDeck[NCard];
  NCard := NextDeckCard(NCard); DealerCards[1].Card := CardDeck[NCard];
  NCard := NextDeckCard(NCard); PlayerCards[2].Card := CardDeck[NCard];
  NCard := NextDeckCard(NCard); DealerCards[2].Card := CardDeck[NCard];
  PlayerCards[1].FaceUp := True; DealerCards[1].FaceUp := True;
  PlayerCards[2].FaceUp := True;
  DisplayCard(2, 1, PlayerCards); DisplayCard(1, 1, DealerCards);
  DisplayCard(2, 2, PlayerCards); DisplayCard(1, 2, DealerCards);
  BJValue(DealerCards, DealerBJValue1, DealerBJValue2);
  BJValue(PlayerCards, PlayerBJValue1, PlayerBJValue2);
  if (DealerBJValue1 = -21) and (DealerBJValue2 = -21) then                    // Blackjack
    Form1.DealerValue.Text := 'BJ'
  else begin
    Form1.DealerValue.Text := IntToStr(DealerBJValue1);
    if DealerBJValue2 <> DealerBJValue1 then
      Form1.DealerValue.Text := Form1.DealerValue.Text + ' / ' + IntToStr(DealerBJValue2);
  end;
  if (PlayerBJValue1 = -21) and (PlayerBJValue2 = -21) then                    // Blackjack
    Form1.PlayerValue.Text := 'BJ'
  else begin
    Form1.PlayerValue.Text := IntToStr(PlayerBJValue1);
    if PlayerBJValue2 <> PlayerBJValue1 then
      Form1.PlayerValue.Text := Form1.PlayerValue.Text + ' / ' + IntToStr(PlayerBJValue2);
  end;
  Form1.ButtonDeal.Enabled := False;
  Form1.ButtonHit.Enabled := True;
  Form1.ButtonStay.Enabled := True;
  Form1.ButtonHit.SetFocus;
end;

{ Button pressed: Hit }

procedure TForm1.ButtonHitClick(Sender: TObject);

var
  NextCard, I: Integer;

begin
  NCard := NextDeckCard(NCard);
  for I := 1 to 7 do begin
    if PlayerCards[I].FaceUp then
      NextCard := I;
  end;
  Inc(NextCard);
  PlayerCards[NextCard].FaceUp := True;
  PlayerCards[NextCard].Card := CardDeck[NCard];
  DisplayCard(2, NextCard, PlayerCards);
  BJValue(PlayerCards, PlayerBJValue1, PlayerBJValue2);
  if (PlayerBJValue1 = -21) and (PlayerBJValue2 = -21) then                    // Blackjack
    PlayerValue.Text := 'BJ'
  else begin
    PlayerValue.Text := IntToStr(PlayerBJValue1);
    if (PlayerBJValue2 <> PlayerBJValue1) and (PlayerBJValue2 <= 21) then
      PlayerValue.Text := Form1.PlayerValue.Text + ' / ' + IntToStr(PlayerBJValue2);
  end;
  if PlayerBJValue1 > 21 then begin
    GameResult.Font.Color := clRed;
    GameResult.Text := 'You busted!';
    Inc(DealerTotal);
    DealerScore.Caption := IntToStr(DealerTotal);
    ButtonDeal.Enabled := True;
    ButtonHit.Enabled := False;
    ButtonStay.Enabled := False;
  end;
end;

{ Button pressed: Stay }

procedure TForm1.ButtonStayClick(Sender: TObject);

var
  DealerDoneValue, PlayerDoneValue, NextCard: Integer;
  Winner: string;

begin
  DealerCards[2].FaceUp := True;
  DisplayCard(1, 2, DealerCards);
  NextCard := 3;
  //DealerDoneValue := 0;
  DealerDoneValue := GetDealerDoneValue;
  if DealerDoneValue = 0 then begin
    repeat
      NCard := NextDeckCard(NCard);
      DealerCards[NextCard].FaceUp := True;
      DealerCards[NextCard].Card := CardDeck[NCard];
      DisplayCard(1, NextCard, DealerCards);
      DealerDoneValue := GetDealerDoneValue;
      if DealerDoneValue = 0 then
        Inc(NextCard);
    until DealerDoneValue <> 0;
  end;
  if PlayerBJValue2 > 21 then
    PlayerDoneValue := PlayerBJValue1
  else
    PlayerDoneValue := PlayerBJValue2;
  if DealerDoneValue = -21 then                                                // dealer has Blackjack (dealer wins)
    Winner := 'dealer'
  else if PlayerDoneValue = -21 then                                           // player has Blackjack (player wins if dealer hasn't)
    Winner := 'player'
  else begin
    if (DealerDoneValue > 21)                                                  // dealer busted (dealer looses)
    or (PlayerDoneValue > DealerDoneValue) then                                // player BJ value > dealer BJ value (player wins)
      Winner := 'player'
    else                                                                       // dealer BJ value >= player BJ value (dealer wins)
      Winner := 'dealer';
  end;
  if Winner = 'player' then begin
    // Round won by player
    GameResult.Font.Color := clDefault;
    GameResult.Text := 'You win!';
    Inc(PlayerTotal);
    PlayerScore.Caption := IntToStr(PlayerTotal);
  end
  else begin
    // Round won by dealer
    GameResult.Font.Color := clDefault;
    GameResult.Text := 'Dealer wins.';
    Inc(DealerTotal);
    DealerScore.Caption := IntToStr(DealerTotal);
  end;
  ButtonDeal.Enabled := True;
  ButtonHit.Enabled := False;
  ButtonStay.Enabled := False;
end;


end.

