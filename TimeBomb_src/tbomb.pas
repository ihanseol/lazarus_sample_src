{************************************}
{ Main unit for TimeBomb application }
{************************************}

unit tbomb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls;

type
  TComb = array[0..3] of Integer;
  TStComb = array[0..9, 0..3] of TStaticText;
  TStCorrect = array[0..9] of TStaticText;
  {************}
  { TfTimeBomb }
  {************}
  TfTimeBomb = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsTime, mOptionsTime1, mOptionsTime2, mOptionsTime5, mOptionsDuplicates: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1: TLabel;
    StaticText1, StaticText2, StaticText3, StaticText4, StaticText5: TStaticText;
    StaticText6, StaticText7, StaticText8, StaticText9, StaticText10: TStaticText;
    StaticText11, StaticText12, StaticText13, StaticText14, StaticText15: TStaticText;
    stComb00, stComb01, stComb02, stComb03, stComb10, stComb11, stComb12, stComb13: TStaticText;
    stComb20, stComb21, stComb22, stComb23, stComb30, stComb31, stComb32, stComb33: TStaticText;
    stComb40, stComb41, stComb42, stComb43, stComb50, stComb51, stComb52, stComb53: TStaticText;
    stComb60, stComb61, stComb62, stComb63, stComb70, stComb71, stComb72, stComb73: TStaticText;
    stComb80, stComb81, stComb82, stComb83, stComb90, stComb91, stComb92, stComb93: TStaticText;
    stCT0, stCT1, stCT2, stCT3, stCT4, stCT5, stCT6, stCT7, stCT8, stCT9: TStaticText;
    stCP0, stCP1, stCP2, stCP3, stCP4, stCP5, stCP6, stCP7, stCP8, stCP9: TStaticText;
    stTime, stExplode: TStaticText;
    Shape1: TShape;
    bt0, bt1, bt2, bt3, bt4, bt5, bt6, bt7, bt8, bt9: TButton;
    btCE, btCA, btDone: TButton;
    tiBomb: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsTime1Click(Sender: TObject);
    procedure mOptionsTime2Click(Sender: TObject);
    procedure mOptionsTime5Click(Sender: TObject);
    procedure mOptionsDuplicatesClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure tiBombTimer(Sender: TObject);
    procedure bt0Click(Sender: TObject);
    procedure bt1Click(Sender: TObject);
    procedure bt2Click(Sender: TObject);
    procedure bt3Click(Sender: TObject);
    procedure bt4Click(Sender: TObject);
    procedure bt5Click(Sender: TObject);
    procedure bt6Click(Sender: TObject);
    procedure bt7Click(Sender: TObject);
    procedure bt8Click(Sender: TObject);
    procedure bt9Click(Sender: TObject);
    procedure btCEClick(Sender: TObject);
    procedure btCAClick(Sender: TObject);
  private
    iBombTime0, iBombTime, iTime, iRow, iCol: Integer;
    bDuplicate0, bDuplicate, bInput: Boolean;
    aComb: TComb;
    stComb: TStComb;
    stCT, stCP: TStCorrect;
  end;

var
  fTimeBomb: TfTimeBomb;

implementation

{$R *.lfm}

{ Format time: sec -> m:ss }

function FormatTime(T: Integer): string;

var
  M, S: Integer;
  FT: string;

begin
  M := T div 60; S := T mod 60;
  FT := IntToStr(M) + ':';
  if S < 10 then
    FT += '0';
  FT += IntToStr(S);
  Result := FT;
end;

{ Generatie 4 digits random combination }

function RandomCombination(Duplicate: Boolean): TComb;

var
  I, J: Integer;
  OK: Boolean;
  Comb: TComb;

begin
  repeat
    OK := True;
    for I := 0 to 3 do
      Comb[I] := Random(9);
    if not Duplicate then begin
      // Check if there are duplicates
      for I := 0 to 2 do begin
        for J := I + 1 to 3 do begin
          if Comb[J] = Comb[I] then
            OK := False;
        end;
      end;
    end;
  until OK;
  Result := Comb;
end;

{ Insert a number (or the empty string) at given position in actual guess row }

procedure InsertNumber(StComb: TStComb; Input: Boolean; Number: string; Row: Integer; var Col: Integer);

begin
  if Input then begin
    if (Number = '') then begin
      // Empty string: Go one column back (to erase number in preceding column)
      if (Col < 3) or ((Col = 3) and (StComb[Row, Col].Caption = '')) then begin
        Dec(Col);
        if Col < 0 then
          Col := 0;
      end;
    end;
    StComb[Row, Col].Caption := Number;
    if Number <> '' then begin
      // Number: Point to next column
      Inc(Col);
      if Col > 3 then
        Col := 3;
    end;
  end;
end;

{ Clear all numbers in current row }

procedure ClearNumbers(var StComb: TStComb; Input: Boolean; Row: Integer; var Col: Integer);

var
  I: Integer;

begin
  if Input then begin
    for I := 0 to 3 do
      StComb[Row, I].Caption := '';
    Col := 0;                                                                  // point to first column in the row
  end;
end;

// Bomb wasn't deactivated and explodes

procedure Explosion;

// Use your fantasy and programming skills to replace the message by some "exploding bomb" graphics...

begin
  fTimeBomb.stExplode.Visible := True;
end;

// Bomb was deactivated: Player wins

procedure Success(G, BT, T: Integer);

var
  S: string;

begin
  S := 'Congratulations! You found the combination before it was to late.';
  if T < 30 then                                                               // bomb was deactivated in less than 30 secs
    S += ' And this in only ' + IntToStr(T) + ' seconds. You must really be an expert!'
  else if BT - T < 10 then                                                     // bomb was deactivated less than 10 secs before explosion
    S += ' If you imagine, if it had take just ' + IntToStr(BT - T) + ' seconds longer...'
  else if G < 5 then                                                           // bomb was deactivated with less than 5 guesses
    S += ' And this in only ' + IntToStr(G) + ' guesses. Amazing!';
  MessageDlg('You win!', S, mtInformation, [mbOK], 0);
end;

{************}
{ TfTimeBomb }
{************}

{ Application start: Initialization }

procedure TfTimeBomb.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  // Create two-dimensional array with combination guesses
  stComb[0, 0] := stComb00; stComb[0, 1] := stComb01; stComb[0, 2] := stComb02; stComb[0, 3] := stComb03;
  stComb[1, 0] := stComb10; stComb[1, 1] := stComb11; stComb[1, 2] := stComb12; stComb[1, 3] := stComb13;
  stComb[2, 0] := stComb20; stComb[2, 1] := stComb21; stComb[2, 2] := stComb22; stComb[2, 3] := stComb23;
  stComb[3, 0] := stComb30; stComb[3, 1] := stComb31; stComb[3, 2] := stComb32; stComb[3, 3] := stComb33;
  stComb[4, 0] := stComb40; stComb[4, 1] := stComb41; stComb[4, 2] := stComb42; stComb[4, 3] := stComb43;
  stComb[5, 0] := stComb50; stComb[5, 1] := stComb51; stComb[5, 2] := stComb52; stComb[5, 3] := stComb53;
  stComb[6, 0] := stComb60; stComb[6, 1] := stComb61; stComb[6, 2] := stComb62; stComb[6, 3] := stComb63;
  stComb[7, 0] := stComb70; stComb[7, 1] := stComb71; stComb[7, 2] := stComb72; stComb[7, 3] := stComb73;
  stComb[8, 0] := stComb80; stComb[8, 1] := stComb81; stComb[8, 2] := stComb82; stComb[8, 3] := stComb83;
  stComb[9, 0] := stComb90; stComb[9, 1] := stComb91; stComb[9, 2] := stComb92; stComb[9, 3] := stComb93;
  // Create arrays for "correct total" and "correct place" values
  stCT[0] := stCT0; stCT[1] := stCT1; stCT[2] := stCT2; stCT[3] := stCT3; stCT[4] := stCT4;
  stCT[5] := stCT5; stCT[6] := stCT6; stCT[7] := stCT7; stCT[8] := stCT8; stCT[9] := stCT9;
  stCP[0] := stCP0; stCP[1] := stCP1; stCP[2] := stCP2; stCP[3] := stCP3; stCP[4] := stCP4;
  stCP[5] := stCP5; stCP[6] := stCP6; stCP[7] := stCP7; stCP[8] := stCP8; stCP[9] := stCP9;
  // Application startup options
  iBombTime0 := 300; bDuplicate0 := True;
  // Start random number generator
  Randomize;
  // Clear all combination and correct values fields
  for I := 0 to 9 do begin
    for J := 0 to 3 do
      stComb[I, J].Caption := '';
    stCT[I].Caption := ''; stCP[I].Caption := '';
  end;
  // Disable number buttons (until game is started)
  bInput := False;
end;

{ Menu item "Game > New": Start a new game }

procedure TfTimeBomb.mGameNewClick(Sender: TObject);

var
  I, J: Integer;

begin
  mGameNew.Enabled := False;                                                   // player must terminate actual game first
  stExplode.Visible := False;
  btDone.Caption := 'Done';
  iBombTime := iBombTime0; bDuplicate := bDuplicate0;                          // game options become active now
  iRow := 0; iCol := 0;
  // Clear all combination and correct values fields
  for I := 0 to 9 do begin
    for J := 0 to 3 do
      stComb[I, J].Caption := '';
    stCT[I].Caption := ''; stCP[I].Caption := '';
  end;
  // Generate random combination
  aComb := RandomCombination(bDuplicate);
  // Game initialization
  iTime := 0; stTime.Caption := '0:00';
  bInput := True;                                                              // enable number buttons
  tiBomb.Enabled := True;                                                      // start "bomb timer"
end;

{ Menu item "Game > Exit": Exit application }

procedure TfTimeBomb.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Game > New": Start a new game }

procedure TfTimeBomb.mOptionsTime1Click(Sender: TObject);

begin
  mOptionsTime1.Checked := True; mOptionsTime2.Checked := False; mOptionsTime5.Checked := False;
  iBombTime0 := 60;
end;

{ Menu items "Options > Bomb timer > ...": Select time before bomb explodes }

procedure TfTimeBomb.mOptionsTime2Click(Sender: TObject);

begin
  mOptionsTime1.Checked := False; mOptionsTime2.Checked := True; mOptionsTime5.Checked := False;
  iBombTime0 := 120;
end;

procedure TfTimeBomb.mOptionsTime5Click(Sender: TObject);

begin
  mOptionsTime1.Checked := False; mOptionsTime2.Checked := False; mOptionsTime5.Checked := True;
  iBombTime0 := 300;
end;

{ Menu item "Options > Allow duplicates": Toggle allowing or not duplicate numbers }

procedure TfTimeBomb.mOptionsDuplicatesClick(Sender: TObject);

begin
  if mOptionsDuplicates.Checked then
    mOptionsDuplicates.Checked := False
  else
    mOptionsDuplicates.Checked := True;
  bDuplicate0 := mOptionsDuplicates.Checked;
end;

{ Menu item "Help > Help": Display (short) application help text }

procedure TfTimeBomb.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Deactivate the time bomb before it explodes by guessing the correct combination (4-digits number sequence). ';
  S += 'You have 10 guesses for each game. After each guess, you are shown the number of correct digits (Correct total) ';
  S += 'and the number of correct digits that are at the correct place (Correct place). If you haven''t found the ';
  S += 'combination after 10 guesses, or if the time runs out, the bomb explodes and you have lost!';
  MessageDlg('"TimeBomb" help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfTimeBomb.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Deactivate the time bomb before it explodes!' + LineEnding;
  S += 'The application is based on the Windows 95 game "Time Bomb" by Jeremy Dickson.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April 2023.';
  MessageDlg('About "TimeBomb"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Done" pushed: Start the game resp. check user combination }

procedure TfTimeBomb.btDoneClick(Sender: TObject);

var
  CT, CP, I: Integer;
  OK: Boolean;
  CCounts, UCounts: array[0..9] of Integer;

begin
  // Button "Start": Start a new game (at aoolication start, instead of starting the game immediately)
  if btDone.Caption = 'Start' then begin
    mGameNew.Click;
  end
  // Button "Done": Check player combination
  else begin
    // Player must enter a number for all 4 combination digits
    OK := True;
    for I := 0 to 3 do begin
      if stComb[iRow, I].Caption = '' then
        OK := False;
    end;
    // All numbers entered: Check if it is the correct combination
    if OK then begin
      CT := 0; CP := 0;
      // Determine and display total number of correct digits
      for I := 0 to 9 do begin
        CCounts[I] := 0; UCounts[I] := 0;
      end;
      for I := 0 to 3 do
        Inc(CCounts[aComb[I]]);
      for I := 0 to 3 do
        Inc(UCounts[StrToInt(stComb[iRow, I].Caption)]);
      for I := 0 to 9 do begin
        if UCounts[I] > CCounts[I] then
          CT += CCounts[I]
        else
          CT += UCounts[I];
      end;
      stCT[iRow].Caption := IntToStr(CT);
      // Determine and display number of correct digits at correct place
      for I := 0 to 3 do begin
        if StrToInt(stComb[iRow, I].Caption) = aComb[I] then
          Inc(CP);
      end;
      stCP[iRow].Caption := IntToStr(CP);
      // If all 4 numbers are at the correct place, the game is over (player has succeeded)
      if CP = 4 then begin
        tiBomb.Enabled := False;                                               // stop the timer
        Success(iRow, iBombTime, iTime);
        mGameNew.Enabled := True;
        bInput := False;
      end
      // If all 10 guesses have been made, the game is over (bomb explodes)
      else begin
        Inc(iRow); iCol := 0;
        if iRow > 9 then begin
          tiBomb.Enabled := False;                                             // stop the timer
          Explosion;
          mGameNew.Enabled := True;                                            // now the player may start a new game again
          bInput := False;                                                     // disable number buttons
        end;
      end;
    end
    // Player did not enter all 4 digits
    else
      MessageDlg('Invalid combination', 'You must enter a number for all 4 positions', mtError, [mbOK], 0);
  end;
end;

{ Timer rotine: Update time passed; let the bomb explode if selected time is over }

procedure TfTimeBomb.tiBombTimer(Sender: TObject);

begin
  Inc(iTime);
  stTime.Caption := FormatTime(iTime);
  if iTime = iBombTime then begin
    // Time is over!
    tiBomb.Enabled := False;
    Explosion;
    mGameNew.Enabled := True;
    bInput := False;
  end;
end;

{ Number button pushed: Insert this number in actual column of actual guess row }

procedure TfTimeBomb.bt0Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '0', iRow, iCol);
end;

procedure TfTimeBomb.bt1Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '1', iRow, iCol);
end;

procedure TfTimeBomb.bt2Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '2', iRow, iCol);
end;

procedure TfTimeBomb.bt3Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '3', iRow, iCol);
end;

procedure TfTimeBomb.bt4Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '4', iRow, iCol);
end;

procedure TfTimeBomb.bt5Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '5', iRow, iCol);
end;

procedure TfTimeBomb.bt6Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '6', iRow, iCol);
end;

procedure TfTimeBomb.bt7Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '7', iRow, iCol);
end;

procedure TfTimeBomb.bt8Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '8', iRow, iCol);
end;

procedure TfTimeBomb.bt9Click(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '9', iRow, iCol);
end;

{ Button "CE" pushed: Erase preceding column number }

procedure TfTimeBomb.btCEClick(Sender: TObject);

begin
  InsertNumber(stComb, bInput, '', iRow, iCol);
end;

{ Button "CA" pushed: Erase all numbers in actual guess row }

procedure TfTimeBomb.btCAClick(Sender: TObject);

begin
  ClearNumbers(stComb, bInput, iRow, iCol);
end;

end.

