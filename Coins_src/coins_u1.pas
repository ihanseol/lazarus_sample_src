{***********************************}
{* Main unit for Coins application *}
{***********************************}

unit coins_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids;

type
  {*********}
  { TfCoins }
  {*********}
  TfCoins = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings, mSettingsAmount, mSettingsCoins, MenuItem1, mSettingsShow: TMenuItem;
    mSettingsCountry, mSettingsCountryEU, mSettingsCountryUS: TMenuItem;
    mSettingsAmount50, mSettingsAmount100, mSettingsAmount200, mSettingsAmount500: TMenuItem;
    mSettingsCoins5, mSettingsCoins10, mSettingsCoins15, mSettingsCoins20: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Memo1: TMemo;
    Shape1, Shape2: TShape;
    imSelCoins1, imSelCoins2, imSelCoins3, imSelCoins4, imSelCoins5, imSelCoins6, imSelCoins7, imSelCoins8: TImage;
    imCoins1, imCoins2, imCoins3, imCoins4, imCoins5, imCoins6, imCoins7: TImage;
    imCoins8, imCoins9, imCoins10, imCoins11, imCoins12, imCoins13, imCoins14: TImage;
    imCoins15, imCoins16, imCoins17, imCoins18, imCoins19, imCoins20: TImage;
    Label1, Label2, Label3, laUnit: TLabel;
    edCoins, edAmount, edUAmount: TEdit;
    sgEval: TStringGrid;
    imEval: TImage;
    btStart: TButton;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsCountryEUClick(Sender: TObject);
    procedure mSettingsCountryUSClick(Sender: TObject);
    procedure mSettingsAmount50Click(Sender: TObject);
    procedure mSettingsAmount100Click(Sender: TObject);
    procedure mSettingsAmount200Click(Sender: TObject);
    procedure mSettingsAmount500Click(Sender: TObject);
    procedure mSettingsCoins5Click(Sender: TObject);
    procedure mSettingsCoins10Click(Sender: TObject);
    procedure mSettingsCoins15Click(Sender: TObject);
    procedure mSettingsCoins20Click(Sender: TObject);
    procedure mSettingsShowClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure imSelCoins1Click(Sender: TObject);
    procedure imSelCoins2Click(Sender: TObject);
    procedure imSelCoins3Click(Sender: TObject);
    procedure imSelCoins4Click(Sender: TObject);
    procedure imSelCoins5Click(Sender: TObject);
    procedure imSelCoins6Click(Sender: TObject);
    procedure imSelCoins7Click(Sender: TObject);
    procedure imSelCoins8Click(Sender: TObject);
    procedure imCoins1Click(Sender: TObject);
    procedure imCoins2Click(Sender: TObject);
    procedure imCoins3Click(Sender: TObject);
    procedure imCoins4Click(Sender: TObject);
    procedure imCoins5Click(Sender: TObject);
    procedure imCoins6Click(Sender: TObject);
    procedure imCoins7Click(Sender: TObject);
    procedure imCoins8Click(Sender: TObject);
    procedure imCoins9Click(Sender: TObject);
    procedure imCoins10Click(Sender: TObject);
    procedure imCoins11Click(Sender: TObject);
    procedure imCoins12Click(Sender: TObject);
    procedure imCoins13Click(Sender: TObject);
    procedure imCoins14Click(Sender: TObject);
    procedure imCoins15Click(Sender: TObject);
    procedure imCoins16Click(Sender: TObject);
    procedure imCoins17Click(Sender: TObject);
    procedure imCoins18Click(Sender: TObject);
    procedure imCoins19Click(Sender: TObject);
    procedure imCoins20Click(Sender: TObject);
  private
    iSMaxAmount, iMaxAmount, iSMaxCoins, iMaxCoins, iSMinCoins, iMinCoins: Integer;
    iGameCoins, iCoins, iUserCoins, iAmount, iUserAmount, iQuestion, iCorrect: Integer;
    sSCoins, sCoins: string;
    aCoins: array[0..19] of Integer;
    imSelCoins: array[0..7] of TImage;
    aSelCoinsNames: array[0..7] of string;
    aSelCoinsValues: array[0..7] of Integer;
    imCoins: array[0..19] of TImage;
    aCoinsValues: array[0..19] of Integer;
  end;

var
  fCoins: TfCoins;

implementation

{$R *.lfm}

{ Format number for grid display (right-align) }

function GFormat(N: Integer): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  GFormat := SN;
end;

{ Get name and value of cuurent set (EU or US) coins }

procedure GetCoins(CoinsSet: string; out CoinsNames: array of string; out CoinsValues: array of Integer);

const
  CoinsEUNames: array[0..7] of string = (
    'cents1', 'cents2', 'cents5', 'cents10', 'cents20', 'cents50', 'euros1', 'euros2'
  );
  CoinsEUValues: array[0..7] of Integer = (
    1, 2, 5, 10, 20, 50, 100, 200
  );
  CoinsUSNames: array[0..7] of string = (
    'penny', 'nickel', 'dime', 'quarter', 'half-dollar', 'dollar', '', ''
  );
  CoinsUSValues: array[0..7] of Integer = (
    1, 5, 10, 25, 50, 100, 0, 0
  );

var
  I: Integer;

begin
  for I := 0 to 7 do begin
    if CoinsSet = 'eu' then begin
      CoinsNames[I] := CoinsEUNames[I]; CoinsValues[I] := CoinsEUValues[I];
    end
    else begin
      CoinsNames[I] := CoinsUSNames[I]; CoinsValues[I] := CoinsUSValues[I];
    end;
  end;
end;

{ Display "amount so far" value }

procedure DisplayAmount(Amount, UserAmount: Integer);

begin
  // Display as cents or euros, depending on amount to reach
  if Amount < 100 then
    fCoins.edUAmount.Text := FloatToStr(UserAmount)
  else
    fCoins.edUAmount.Text := FloatToStrF(UserAmount / 100, ffFixed, 0, 2);
  // Use colors to indicate if amount so far is equal to or less/greater than value to reach
  if UserAmount = Amount then
    fCoins.edUAmount.Font.Color := clLime
  else if UserAmount < Amount then
    fCoins.edUAmount.Font.Color := clYellow
  else
    fCoins.edUAmount.Font.Color := clRed;
end;

{ Coin selection: Display the coin, that the user clicked, in the left box (user answer coins) }

procedure CoinClick(Amount, NCoins, CoinValue: Integer; ImgSelCoin: TImage;
  var UserNCoins, UserAmount: Integer; var Values: array of Integer; var ImgCoins: array of TImage);

var
  I: Integer;
  Found: Boolean;

// The procedure also saves the selected coin's value, the number of coins used and the total amount so far

begin
  // Only do if a (new) question has been generated and isn't yet answered
  if fCoins.btDone.Enabled then begin;
    // If user tries to use more coins than indicated, display error message
    if UserNCoins + 1 > NCoins then
      MessageDlg('Invalid input', 'Number of coins to be used = ' + IntToStr(NCoins), mtError, [mbOK], 0)
    // Otherwise, proceed...
    else begin
      // Display coin at first free place in left box
      I := 0; Found := False;
      repeat
        if not ImgCoins[I].Visible then begin                                  // an invisible coin indicates a free place
          ImgCoins[I].Picture := ImgSelCoin.Picture;                           // actual coin's picture
          ImgCoins[I].Visible := True;                                         // display the coin by making it visible
          Values[I] := CoinValue;                                              // save coin value (this is needed for the case the user removes the coin later)
          Found := True;
        end;
        Inc(I);
      until Found or (I > 19);
      // New number of coins used and new money amount
      Inc(UserNCoins); UserAmount += CoinValue;
      // Display amount so far (if this options has beem selected)
      if fCoins.mSettingsShow.Checked then
        DisplayAmount(Amount, UserAmount);
    end;
  end;
end;

{ Coin removal: Remove the coin clicked by the user by making it invisible }

procedure UserCoinClick(Amount, XCoin: Integer; var UserCoins, UserAmount: Integer; var Values: array of Integer; var imCoins: array of TImage);

// The procedure also saves the number of coins used and the total amount so far

begin
  if imCoins[XCoin].Visible then begin                                         // if there actually is a coin...
    // Remove coin (by making it invisible)
    imCoins[XCoin].Visible := False;
    // New number of coins and new money amount
    Dec(UserCoins);
    UserAmount -= Values[XCoin]; Values[XCoin] := 0;
    // Display amount so far (if this options has beem selected)
    if fCoins.mSettingsShow.Checked then
      DisplayAmount(Amount, UserAmount);
  end;
end;

{ Update evaluation values }

procedure UpdateEval(Question, Correct: Integer);

begin
  fCoins.sgEval.Cells[1, 0] := GFormat(Question);
  fCoins.sgEval.Cells[1, 1] := GFormat(Correct);
  fCoins.sgEval.Cells[1, 2] := GFormat(Question - Correct);
  fCoins.sgEval.Cells[1, 3] := GFormat(Round(100 * Correct / Question)) + '%';
end;

{*********}
{ TfCoins }
{*********}

{ Application start: Initialisation }

procedure TfCoins.FormCreate(Sender: TObject);

begin
  // Create array with coin images (right box)
  imSelCoins[0]  := imSelCoins1; imSelCoins[1]  := imSelCoins2; imSelCoins[2]  := imSelCoins3; imSelCoins[3]  := imSelCoins4;
  imSelCoins[4]  := imSelCoins5; imSelCoins[5]  := imSelCoins6; imSelCoins[6]  := imSelCoins7; imSelCoins[7]  := imSelCoins8;
  // Create array with coin images (left box)
  imCoins[0]  := imCoins1;  imCoins[1]  := imCoins2;  imCoins[2]  := imCoins3;  imCoins[3]  := imCoins4;  imCoins[4] := imCoins5;
  imCoins[5]  := imCoins6;  imCoins[6]  := imCoins7;  imCoins[7]  := imCoins8;  imCoins[8]  := imCoins9;  imCoins[9] := imCoins10;
  imCoins[10] := imCoins11; imCoins[11] := imCoins12; imCoins[12] := imCoins13; imCoins[13] := imCoins14; imCoins[14] := imCoins15;
  imCoins[15] := imCoins16; imCoins[16] := imCoins17; imCoins[17] := imCoins18; imCoins[18] := imCoins19; imCoins[19] := imCoins20;
  // Start a new game
  Randomize;
  sSCoins := 'eu'; iSMaxAmount := 100;
  iSMinCoins := 5; iSMaxCoins := 10;
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new game }

procedure TfCoins.mGameNewClick(Sender: TObject);

var
  I: Integer;
  Filename: string;

begin
  // Make settings active now
  sCoins := sSCoins; iMaxAmount := iSMaxAmount;
  iMinCoins := iSMinCoins; iMaxCoins := iSMaxCoins;
  // Number of coins in the actual set
  if sCoins = 'eu' then
    iGameCoins := 8
  else
    iGameCoins := 5;
  // Get and display actual coins set (EU or US)
  GetCoins(sCoins, aSelCoinsNames, aSelCoinsValues);
  for I := 0 to 7 do begin
    if aSelCoinsNames[I] = '' then
      imSelCoins[I].Visible := False
    else begin
      imSelCoins[I].Visible := True;
      Filename := './coins/' + sCoins + '_' + aSelCoinsNames[I] + '.jpg'; DoDirSeparators(Filename);
      imSelCoins[I].Picture.LoadFromFile(Filename);
    end;
  end;
  fCoins.edCoins.Text := ''; fCoins.edAmount.Text := '';
  // Clear coin images (left box)
  for I := 0 to 19 do
    imCoins[I].Visible := False;
  fCoins.edUAmount.Text := '';
  // Clear evaluation grid and values
  for I := 0 to 3 do
    fCoins.sgEval.Cells[1, I] := '';
  fCoins.imEval.Visible := False;
  iQuestion := 0; iCorrect := 0;
  // Adapt button properties
  fCoins.btStart.Caption := 'Start';
  fCoins.btDone.Enabled := False;                                              // enable "Done" button, when "Start" will be pushed
end;

{ Menu item "Game > Exit": Exit application }

procedure TfCoins.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Coins to be used > ...": Choose coins set to be used (EU or US) }

procedure TfCoins.mSettingsCountryEUClick(Sender: TObject);

begin
  mSettingsCountryEU.Checked := True; mSettingsCountryUS.Checked := False;
  sSCoins := 'eu';
end;

procedure TfCoins.mSettingsCountryUSClick(Sender: TObject);

begin
  mSettingsCountryEU.Checked := False; mSettingsCountryUS.Checked := True;
  sSCoins := 'us';
end;

{ Menu items "Settings > Amount maximum > ...": Choose minimum and maximum number of coins to be used }

procedure TfCoins.mSettingsAmount50Click(Sender: TObject);

begin
  mSettingsAmount50.Checked  := True;  mSettingsAmount100.Checked := False;
  mSettingsAmount200.Checked := False; mSettingsAmount500.Checked := False;
  iSMaxAmount := 50;
end;

procedure TfCoins.mSettingsAmount100Click(Sender: TObject);

begin
  mSettingsAmount50.Checked  := False; mSettingsAmount100.Checked := True;
  mSettingsAmount200.Checked := False; mSettingsAmount500.Checked := False;
  iSMaxAmount := 100;
end;

procedure TfCoins.mSettingsAmount200Click(Sender: TObject);

begin
  mSettingsAmount50.Checked  := False; mSettingsAmount100.Checked := False;
  mSettingsAmount200.Checked := True;  mSettingsAmount500.Checked := False;
  iSMaxAmount := 200;
end;

procedure TfCoins.mSettingsAmount500Click(Sender: TObject);

begin
  mSettingsAmount50.Checked  := False; mSettingsAmount100.Checked := False;
  mSettingsAmount200.Checked := False; mSettingsAmount500.Checked := True;
  iSMaxAmount := 500;
end;

{ Menu items "Settings > Coins number > ...": Choose minimum and maximum number of coins to be used }

procedure TfCoins.mSettingsCoins5Click(Sender: TObject);

begin
  mSettingsCoins5.Checked  := True;  mSettingsCoins10.Checked := False;
  mSettingsCoins15.Checked := False; mSettingsCoins20.Checked := False;
  iSMinCoins := 3; iSMaxCoins := 5;
end;

procedure TfCoins.mSettingsCoins10Click(Sender: TObject);

begin
  mSettingsCoins5.Checked  := False; mSettingsCoins10.Checked := True;
  mSettingsCoins15.Checked := False; mSettingsCoins20.Checked := False;
  iSMinCoins := 5; iSMaxCoins := 10;
end;

procedure TfCoins.mSettingsCoins15Click(Sender: TObject);

begin
  mSettingsCoins5.Checked  := False;  mSettingsCoins10.Checked := False;
  mSettingsCoins15.Checked := True; mSettingsCoins20.Checked := False;
  iSMinCoins := 8; iSMaxCoins := 15;
end;

procedure TfCoins.mSettingsCoins20Click(Sender: TObject);

begin
  mSettingsCoins5.Checked  := False; mSettingsCoins10.Checked := False;
  mSettingsCoins15.Checked := False; mSettingsCoins20.Checked := True;
  iSMinCoins := 10; iSMaxCoins := 20;
end;

{ Menu item "Settings > Display amount": Toggle to display or not the "so far" money amount }

procedure TfCoins.mSettingsShowClick(Sender: TObject);

begin
  if mSettingsShow.Checked then
    mSettingsShow.Checked := False
  else
    mSettingsShow.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfCoins.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Arithmetic baubles.' + LineEnding;
  S += 'Try to reach a given amount of money with a given number of coins.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, April 2019 - February 2021.';
  MessageDlg('About "Coins"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Generate a new question }

procedure TfCoins.btStartClick(Sender: TObject);

var
  I: Integer;

begin
  if btStart.Caption = 'Next' then
    // Be sure evaluation values are up to date (user may have not answered at all the previous question!)
    UpdateEval(iQuestion, iCorrect);
  // Remove all coins from the left box (and set coin value in corr. array to 0)
  for I := 0 to 19 do begin
    imCoins[I].Visible := False;
    aCoinsValues[I] := 0;
  end;
  // Clear form fields / reset values
  edUAmount.Text := '';
  imEval.Visible := False;
  iUserCoins := 0; iUserAmount := 0;
  // Generate question
  Inc(iQuestion);
  iCoins := Random(iMaxCoins - iMinCoins + 1) + iMinCoins;                     // (random) number of coins to be used
  // Continue choosing random coins until the sum of their values is less or equal the max. amount allowed
  repeat
    I := 0; iAmount := 0;
    repeat
      Inc(I);
      aCoins[I] := aSelCoinsValues[Random(iGameCoins)];
      iAmount += aCoins[I];
    until (I = iCoins - 1) or (iAmount > iMaxAmount);                          // break if the wanted number of coins is reached or the coins sum exceed max. selected
  until iAmount <= iMaxAmount;                                                 // if the coins sum is ok (only possible with correct number of coins), it's done!
  // Display number of coins to be used and amount to be reached
  edCoins.Text := IntToStr(iCoins);
  if iAmount < 100 then begin                                                  // display as cents
    edAmount.Text := IntToStr(iAmount);
    laUnit.Caption := 'Cent';
  end
  else begin                                                                   // display as euros/dollars
    edAmount.Text := FloatToStrF(iAmount / 100, ffFixed, 0, 2);
    if sCoins = 'eu' then
      laUnit.Caption := 'Euro'
    else
      laUnit.Caption := 'Dollar';
  end;
  // Button settings for game continuation
  btStart.Caption := 'Next'; btDone.Enabled := True;
end;

{ Button "Done": Check user's answer }

procedure TfCoins.btDoneClick(Sender: TObject);

begin
  // If both the number of coins and the sum of the coins' values are equal to the given values, the user's answer is correct
  if (iUserCoins = iCoins) and (iUserAmount = iAmount) then begin
    Inc(iCorrect);
    imEval.Picture.LoadFromFile('correct.png');
    imEval.Visible := True;
  end
  // Otherwise, it's false
  else begin
    imEval.Picture.LoadFromFile('false.png');
  end;
  imEval.Visible := True;
  // Update evaluation values
  UpdateEval(iQuestion, iCorrect);
  // Disable "Done" button
  btDone.Enabled := False;
end;

{ Coin selections (user clicking coin in right box): Call procedure to display coin in left box }

procedure TfCoins.imSelCoins1Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[0], imSelCoins[0], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins2Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[1], imSelCoins[1], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins3Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[2], imSelCoins[2], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins4Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[3], imSelCoins[3], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins5Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[4], imSelCoins[4], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins6Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[5], imSelCoins[5], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins7Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[6], imSelCoins[6], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imSelCoins8Click(Sender: TObject);

begin
  CoinClick(iAmount, iCoins, aSelCoinsValues[7], imSelCoins[7], iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

{ Coin removals (user clicking coin in left box): Call procedure to remove (hide) coin in left box }

procedure TfCoins.imCoins1Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 0, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins2Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 1, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins3Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 2, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins4Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 3, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins5Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 4, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins6Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 5, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins7Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 6, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins8Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 7, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins9Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 8, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins10Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 9, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins11Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 10, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins12Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 11, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins13Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 12, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins14Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 13, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins15Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 14, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins16Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 15, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins17Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 16, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins18Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 17, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins19Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 18, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

procedure TfCoins.imCoins20Click(Sender: TObject);

begin
  UserCoinClick(iAmount, 19, iUserCoins, iUserAmount, aCoinsValues, imCoins);
end;

end.

