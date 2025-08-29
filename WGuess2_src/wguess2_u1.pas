{*************************************}
{* Main unit for WGuess2 application *}
{*************************************}

unit wguess2_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LCLType, ExtCtrls, Grids, wguess2_u2;

type
  TWords = array of UTF8string;
  TGallows = array[1..2, 1..10] of TShape;
  TGallowsX = array[1..2] of Integer;
  {**************}
  { TWGuess2Form }
  {**************}
  TWGuess2Form = class(TForm)
    mMenu: TMainMenu;
    MenuGame: TMenuItem;
    GameNew: TMenuItem;
    GameExit: TMenuItem;
    MenuSettings: TMenuItem;
    SettingsWordFile: TMenuItem;
    SettingsScoring: TMenuItem;
    SettingsScoringWords: TMenuItem;
    SettingsScoringTrials: TMenuItem;
    SettingsLetters: TMenuItem;
    SettingsFirst: TMenuItem;
    SettingsAccents: TMenuItem;
    AccentsEnable: TMenuItem;
    AccentsDisable: TMenuItem;
    AccentsTransform: TMenuItem;
    MenuHelp: TMenuItem;
    HelpHelp: TMenuItem;
    HelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edName1: TEdit;
    edName2: TEdit;
    shGallows101: TShape;
    shGallows102: TShape;
    shGallows103: TShape;
    shGallows104: TShape;
    shGallows105: TShape;
    shGallows106: TShape;
    shGallows107: TShape;
    shGallows108: TShape;
    shGallows109: TShape;
    shGallows110: TShape;
    shGallows201: TShape;
    shGallows202: TShape;
    shGallows203: TShape;
    shGallows204: TShape;
    shGallows205: TShape;
    shGallows206: TShape;
    shGallows207: TShape;
    shGallows208: TShape;
    shGallows209: TShape;
    shGallows210: TShape;
    edTrials1: TEdit;
    edTrials2: TEdit;
    sgScore: TStringGrid;
    LabelWord: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    WordToGuess: TEdit;
    Attempts: TEdit;
    Letters: TEdit;
    ButtonStart: TButton;
    ButtonSolve: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure GameNewClick(Sender: TObject);
    procedure GameExitClick(Sender: TObject);
    procedure SettingsWordFileClick(Sender: TObject);
    procedure SettingsScoringWordsClick(Sender: TObject);
    procedure SettingsScoringTrialsClick(Sender: TObject);
    procedure SettingsLettersClick(Sender: TObject);
    procedure SettingsFirstClick(Sender: TObject);
    procedure AccentsEnableClick(Sender: TObject);
    procedure AccentsDisableClick(Sender: TObject);
    procedure AccentsTransformClick(Sender: TObject);
    procedure HelpHelpClick(Sender: TObject);
    procedure HelpAboutClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonSolveClick(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure edName1Change(Sender: TObject);
    procedure edName2Change(Sender: TObject);
  private
    iWordCount, iPlayer, iGames1, iGames2, iTrials1, iTrials2, iUserAttempts: Integer;
    sWordGuess, sWordDisplay, sWordDisplaySave: UTF8string;
    bKeybEvent: Boolean;
    aWords: TWords;
    aGallows: TGallows;
    aGX: TGallowsX;
  end;

var
  WGuess2Form: TWGuess2Form;

implementation

{$R *.lfm}

{ Function UTF8Length: Get length of UTF8String }

function UTF8Length(S: UTF8String): Word;

var
  I: Integer;
  L: Word;

// Note: the routine considers letters only!!!

begin
  L := 0; I := 1;
  while I <= Length(S) do begin
    if not (S[I] in ['a'..'z', 'A'..'Z']) then                                 // non-English letter (2 bytes)
      Inc(I);
    Inc(L); Inc(I);
  end;
  UTF8Length := L;
end;

{ Transform accents and umlauts to English letters }

function CharTransform(Ch: TUTF8Char): UTF8String;

var
  S: UTF8String;

begin
  case Ch of
    'à' : S += 'a';
    'â' : S += 'a';
    'é' : S += 'e';
    'è' : S += 'e';
    'ê' : S += 'e';
    'ë' : S += 'e';
    'î' : S += 'i';
    'ï' : S += 'i';
    'ó' : S += 'o';
    'ù' : S += 'u';
    'û' : S += 'u';
    'ä' : S += 'ae';
    'ö' : S += 'oe';
    'ü' : S += 'ue';
    'É' : S += 'E';
    'Î' : S += 'I';
    'Ä' : S += 'Ae';
    'Ö' : S += 'Oe';
    'Ü' : S += 'Ue';
  end;
  CharTransform := S;
end;

{ Read words from file }

procedure ReadWords(Filename: string; out Words: TWords);

var
  S: UTF8string;
  InFile: Text;

begin
  SetLength(Words, 0);
  Assign(InFile, Filename); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, S);
    if S <> '' then begin
      SetLength(Words, Length(Words) + 1);
      Words[Length(Words) - 1] := S;
    end;
  end;
  Close(InFile);
end;

{ (Re)calculate the number of remaining guess words in the list }

function WordCount(var Words: TWords): Integer;

var
  Count, I, J: Integer;
  S: UTF8String;
  DoCount: Boolean;

// Number of remaining words = total words - words already done - words with accents (if this option is selected)

begin
  Count := 0;
  for I := 0 to Length(Words) - 1 do begin
    if LeftStr(Words[I], 1) <> '*' then begin                                  // count words not yet done, only
      DoCount := True;
      if WGuess2Form.AccentsDisable.Checked then begin
        // Accents/umlauts disabled
        S := Words[I]; J := 1;
        while (J <= Length(S)) and DoCount do begin
          if not (S[J] in ['a'..'z', 'A'..'Z']) then                           // non-English letter
            DoCount := False;
          Inc(J);
        end;
      end;
      if DoCount then
        Inc(Count);
    end;
  end;
  WordCount := Count;
end;

{ Start a new game }

procedure NewGame(var Words: TWords; out Count, P, G1, G2, T1, T2: Integer; Gallows: TGallows; out KeybEvent: Boolean);

var
  I, J: Integer;

begin
  // Use full word list (remove '*' of works marked having been done)
  for I := 0 to Length(Words) - 1 do begin
    if LeftStr(Words[I], 1) = '*' then
      Delete(Words[I], 1, 1);
  end;
  Count := WordCount(Words);
  // Start game with player 1
  P := 0;
  WGuess2Form.edName1.Color := clDefault;
  WGuess2Form.edName2.Color := clDefault;
  // Reset score
  G1 := 0; G2 := 0; T1 := 0; T2 := 0;
  for I := 1 to 2 do
    WGuess2Form.sgScore.Cells[1, I] := '' ;
  // Reset the gallows (make invisible)
  for J := 1 to 2 do begin
    for I := 1 to 10 do
      Gallows[J, I].Visible := False;
  end;
  // Show/hide trials fields (depending on scoring method selected)
  if WGuess2Form.SettingsScoringTrials.Checked then begin
    WGuess2Form.edTrials1.Visible := True;
    WGuess2Form.edTrials2.Visible := True;
    WGuess2Form.edTrials1.Text := ''; WGuess2Form.edTrials2.Text := '';
  end
  else begin
    WGuess2Form.edTrials1.Visible := False;
    WGuess2Form.edTrials2.Visible := False;
  end;
  // Clear the game fields
  WGuess2Form.WordToGuess.Color := clForm;
  WGuess2Form.WordToGuess.Font.Color := clDefault;
  WGuess2Form.WordToGuess.Text := '';
  WGuess2Form.Attempts.Text := '';
  WGuess2Form.Letters.Text := '';
  WGuess2Form.WordToGuess.ReadOnly := True;                                    // user will be able to edit the word only after the "Solve" button has been pushed
  WGuess2Form.SettingsWordFile.Enabled := True;
  WGuess2Form.SettingsScoring.Enabled := True;
  // Starting button conditions
  WGuess2Form.ButtonStart.Caption := 'Start';
  WGuess2Form.ButtonStart.Enabled := True;
  WGuess2Form.ButtonSolve.Caption := 'Solve';
  WGuess2Form.ButtonSolve.Enabled := False;
  // Disable capture of keyboard (key pressed) events (!)
  KeybEvent := False;
end;

{ Update scores (with "words found" scoring method) }

procedure UpdateScoreWords(Player: Integer);

var
  Score: Integer;
  S: string;

begin
  if WGuess2Form.sgScore.Cells[1, Player] = '' then
    Score := 0
  else
    Score := StrToInt(WGuess2Form.sgScore.Cells[1, Player]);
  Inc(Score);
  // Add spaces for right-alignment
  S := ' ' + IntToStr(Score);
  if Score < 10 then
    S := '  ' + S
  else if Score < 100 then
    S := ' ' + S;
  WGuess2Form.sgScore.Cells[1, Player] := S;
end;

{ Update scores (with "trials compare" scoring method) }

procedure UpdateScoreTrials(Trial1, Trial2: Integer);

var
  Score1, Score2: Integer;
  S: string;

begin
  if WGuess2Form.sgScore.Cells[1, 1] = '' then
    Score1 := 0
  else
    Score1 := StrToInt(WGuess2Form.sgScore.Cells[1, 1]);
  if WGuess2Form.sgScore.Cells[1, 2] = '' then
    Score2 := 0
  else
    Score2 := StrToInt(WGuess2Form.sgScore.Cells[1, 2]);
  // The player with the fewest attempts get the differents of trials as points
  if Trial1 > Trial2 then
    Score2 += (Trial1 - Trial2)
  else if Trial2 > Trial1 then
    Score1 += (Trial2 - Trial1);
  // Add spaces for right-alignment
  S := ' ' + IntToStr(Score1);
  if Score1 < 10 then
    S := '  ' + S
  else if Score1 < 100 then
    S := ' ' + S;
  WGuess2Form.sgScore.Cells[1, 1] := S;
  S := ' ' + IntToStr(Score2);
  if Score2 < 10 then
    S := '  ' + S
  else if Score2 < 100 then
    S := ' ' + S;
  WGuess2Form.sgScore.Cells[1, 2] := S;
end;

{**************}
{ TWGuess2Form }
{**************}

{ Application start: Initialize the game }

procedure TWGuess2Form.FormCreate(Sender: TObject);

begin
  // Create gallows arrays
  aGallows[1, 1] := shGallows101; aGallows[1, 2] := shGallows102; aGallows[1, 3] := shGallows103; aGallows[1, 4] := shGallows104; aGallows[1, 5]  := shGallows105;
  aGallows[1, 6] := shGallows106; aGallows[1, 7] := shGallows107; aGallows[1, 8] := shGallows108; aGallows[1, 9] := shGallows109; aGallows[1, 10] := shGallows110;
  aGallows[2, 1] := shGallows201; aGallows[2, 2] := shGallows202; aGallows[2, 3] := shGallows203; aGallows[2, 4] := shGallows204; aGallows[2, 5]  := shGallows205;
  aGallows[2, 6] := shGallows206; aGallows[2, 7] := shGallows207; aGallows[2, 8] := shGallows208; aGallows[2, 9] := shGallows209; aGallows[2, 10] := shGallows210;
  // Initialisation
  ReadWords('default.txt', aWords);                                            // get default word list
  NewGame(aWords, iWordCount, iPlayer, iGames1, iGames2, iTrials1, iTrials2, aGallows, bKeybEvent);
  Randomize;
end;

{ Menu item "Game > New": Start a new game }

procedure TWGuess2Form.GameNewClick(Sender: TObject);

begin
  NewGame(aWords, iWordCount, iPlayer, iGames1, iGames2, iTrials1, iTrials2, aGallows, bKeybEvent);
end;

{ Menu item "Game > Exit": Exit the application }

procedure TWGuess2Form.GameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Load words file ...": Read words list from file }

procedure TWGuess2Form.SettingsWordFileClick(Sender: TObject);

var
  Filename: string;

begin
  dlgOpen.InitialDir := GetCurrentDir;                                         // look for words files in current directory
  if dlgOpen.Execute then begin                                                // user has selected a file
    Filename := dlgOpen.Filename;                                              // get filename
    ReadWords(Filename, aWords);                                               // read words from file
  end;
end;

{ Menu item "Settings > Scoring method > Words found": Select Scoring per word found }

procedure TWGuess2Form.SettingsScoringWordsClick(Sender: TObject);

begin
  SettingsScoringWords.Checked := True;
  SettingsScoringTrials.Checked := False;
end;

{ Menu item "Settings > Scoring method > Trials compare": Select Scoring by number of trials comparison }

procedure TWGuess2Form.SettingsScoringTrialsClick(Sender: TObject);

begin
  SettingsScoringWords.Checked := False;
  SettingsScoringTrials.Checked := True;
end;

{ Menu item "Settings > Show letters used": Toggle show/hide letters used }

procedure TWGuess2Form.SettingsLettersClick(Sender: TObject);

begin
  if SettingsLetters.Checked then begin
    SettingsLetters.Checked := False;
    Letters.Visible := False;
  end
  else begin
    SettingsLetters.Checked := True;
    Letters.Visible := True;
  end;
end;

{ Menu item "Settings > Show first letter": Toggle show/don't show first letter }

procedure TWGuess2Form.SettingsFirstClick(Sender: TObject);

begin
  if SettingsFirst.Checked then
    SettingsFirst.Checked := False
  else
    SettingsFirst.Checked := True;
end;

{ Menu item "Settings > Accents/umlauts > Enable": Enable (allow) words with accents/umlauts }

procedure TWGuess2Form.AccentsEnableClick(Sender: TObject);

begin
  AccentsDisable.Checked := False;
  AccentsTransform.Checked := False;
  AccentsEnable.Checked := True;
  iWordCount := WordCount(aWords);                                             // recalculate number of words in list
end;

{ Menu item "Settings > Accents/umlauts > Disable": Disable (ignore) words with accents/umlauts }

procedure TWGuess2Form.AccentsDisableClick(Sender: TObject);

begin
  AccentsEnable.Checked := False;
  AccentsTransform.Checked := False;
  AccentsDisable.Checked := True;
  iWordCount := WordCount(aWords);                                             // recalculate number of words in list
end;

{ Menu item "Settings > Accents/umlauts > Transform": Enable transformed words with accents/umlauts }

procedure TWGuess2Form.AccentsTransformClick(Sender: TObject);

begin
  AccentsEnable.Checked := False;
  AccentsDisable.Checked := False;
  AccentsTransform.Checked := True;
  iWordCount := WordCount(aWords);                                             // recalculate number of words in list
end;

{ Menu item "Help > Help": Display help content }

procedure TWGuess2Form.HelpHelpClick(Sender: TObject);

begin
  HelpForm.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TWGuess2Form.HelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := '"Guess the word game for 2 players".' + LineEnding;
  S += 'Version 1.0, © allu, December 2018.';
  MessageDlg('About "WGuess2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Start/continue the game with a new word }

procedure TWGuess2Form.ButtonStartClick(Sender: TObject);

var
  Ch0: TUTF8Char;
  Word0, Word1: UTF8string;
  WordOK: Boolean;
  W: Word;
  I, J: Integer;

begin
  if ButtonStart.Caption = 'Start' then begin
    // Disable acces to some options during game
    SettingsScoring.Enabled := False;
    WGuess2Form.SettingsWordFile.Enabled := False;
  end;
  // Play the game with next random word
  if iWordCount > 0 then begin                                                 // proceed if there are (not yet guessed) words left
    // Change player (alternate turns)
    if iPlayer = 1 then begin
      iPlayer := 2;
      WGuess2Form.edName1.Color := clDefault;
      WGuess2Form.edName2.Color := clLime;
    end
    else begin
      iPlayer := 1;
      WGuess2Form.edName1.Color := clLime;
      WGuess2Form.edName2.Color := clDefault;
    end;
    // Reset gallows (make invisible) and trials counters
    if iPlayer = 1 then begin
      for J := 1 to 2 do begin
        for I := 1 to 10 do
          aGallows[J, I].Visible := False;
      end;
      edTrials1.Text := '0'; edTrials2.Text := '0';
    end;
    // Get a random word
    repeat
      W := Random(Length(aWords)); Word1 := aWords[W]; WordOK := True;
      if LeftStr(Word1, 1) = '*' then                                          // word has already been displayed for guess
        WordOK := False
      else begin
        if AccentsDisable.Checked then begin
          // Ignore words having accents/umlauts (if this option is checked)
          I := 1; Word0 := Word1;
          repeat
            if not (Word0[I] in ['a'..'z', 'A'..'Z']) then                     // non English letter
              Word0 := '';
            Inc(I);
          until (Word0 = '') or (I >= Length(Word1));
          if Word0 = '' then
            WordOK := False;
        end
        else if AccentsTransform.Checked then begin
          // Transform accents/umlauts to English letters (if this option is checked)
          I := 1; Word0 := aWords[W]; Word1 := '';
          repeat
            if Word0[I] in ['a'..'z', 'A'..'Z'] then
              Word1 += Word0[I]                                                // the transformed word
            else begin
              // Accent/umlaut transformation
              Ch0 := Copy(Word0, I, 2);
              Word1 += CharTransform(Ch0);
              Inc(I);
            end;
            Inc(I);
          until (Word0 = '') or (I > Length(aWords[W]));
        end;
      end;
    until WordOK;
    sWordGuess := Word1;                                                       // the word to be guessed
    // Display the word as hyphens
    sWordDisplay := ''; I := 1;
    repeat
      if not (sWordGuess[I] in ['a'..'z', 'A'..'Z']) then
        // Non English letter (2 bytes)
        Inc(I);
      sWordDisplay += '-';
      Inc(I);
    until I > Length(sWordGuess);
    if SettingsFirst.Checked then begin                                        // show first character (if this option is selected)
      Delete(sWordDisplay, 1, 1);
      if sWordGuess[1] in ['a'..'z', 'A'..'Z'] then                            // English letter (1 byte)
        sWordDisplay := sWordGuess[1] + sWordDisplay
      else                                                                     // non-English letter (2 bytes)
        sWordDisplay := Copy(sWordGuess, 1, 2) + sWordDisplay;
    end;
    aWords[W] := '*' + aWords[W];                                              // remove word from list (by marking it with '*')
    Dec(iWordCount);                                                           // new count of words left
    // Adapt form fields
    WordToGuess.Color := clForm;
    WordToGuess.Font.Color := clDefault;
    WordToGuess.Font.Style := [];
    WordToGuess.Text := sWordDisplay;                                          // display the hyphens
    WordToGuess.ReadOnly := True;                                              // user will only be able to edit the word after "Solve" has been pushed
    iUserAttempts := 0; Attempts.Text:= IntToStr(iUserAttempts);               // reset number of guess attempts
    Letters.Text := '';
    // Adapt buttons
    ButtonStart.Caption := 'Next';
    ButtonStart.Enabled := False;
    ButtonSolve.Enabled := True;
    ButtonSolve.SetFocus;
    // Activate catching keyboards events (= listen for letter-keys pressed)
    bKeybEvent := True;
    // Set gallows array index to 1
    aGX[1] := 1; aGX[2] := 1;
  end
  // All words done
  else begin
    WordToGuess.Text := '';
    Attempts.Text := '';
    Letters.Text := '';
    MessageDlg('Game over', 'All words done. Choose "New game" to do another one...', mtInformation, [mbOK], 0);
  end;
end;

{ Button "Solve/Check": Give possibility to solve / check the solution }

procedure TWGuess2Form.ButtonSolveClick(Sender: TObject);

begin
  if WGuess2Form.ButtonSolve.Caption = 'Solve' then begin
    // Give the user the possibility to solve
    bKeybEvent := False;                                                         // disable catching of keyboard events (!)
    WGuess2Form.WordToGuess.ReadOnly := False;                                   // give user the possibility to edit the word
    WGuess2Form.WordToGuess.Color := clDefault;
    WGuess2Form.WordToGuess.SetFocus;
    WGuess2Form.ButtonSolve.Caption := 'Check';
    sWordDisplaySave := sWordDisplay;                                            // save displayed (hyphened) guess word
  end
  else begin
    // Check the solution proposed by the user
    Inc(iUserAttempts); Attempts.Text := IntToStr(iUserAttempts);
    if iPlayer = 1 then
      edTrials1.Text := IntToStr(iUserAttempts)
    else
      edTrials2.Text := IntToStr(iUserAttempts);
    sWordDisplay := WGuess2Form.WordToGuess.Text;
    if LowerCase(sWordDisplay) = LowerCase(sWordGuess) then begin
      // Solution is correct
      WGuess2Form.WordToGuess.Text := sWordGuess;
      WGuess2Form.WordToGuess.Font.Style := [fsBold];
      if SettingsScoringWords.Checked then
        UpdateScoreWords(iPlayer)                                              // update score (by word found) for actual player
      else if iPlayer = 2 then
        UpdateScoreTrials(StrToInt(edTrials1.Text), StrToInt(edTrials2.Text)); // update score (by trials compare) for both players
      WGuess2Form.ButtonStart.Enabled := True;                                 // the game may continue with a new word
      WGuess2Form.ButtonSolve.Caption := 'Solve';
      WGuess2Form.ButtonSolve.Enabled := False;                                // disable Solve button (until game continued)
      WGuess2Form.ButtonStart.SetFocus;
    end
    else begin
      // Solution is false
      aGallows[iPlayer, aGX[iPlayer]].Visible := True;                         // display 1 part of actual player's  gallows
      Inc(aGX[iPlayer]);                                                       // point index for actual player to next gallows parts
      if aGX[iPlayer] <= 10 then begin
        // The gallows is not yet complete
        sWordDisplay := sWordDisplaySave;                                      // restore displayed (hyphened) guess word
        WGuess2Form.WordToGuess.Text := sWordDisplay;
        bKeybEvent := True;                                                    // re-enable catching of keyboard events (!)
        WGuess2Form.WordToGuess.ReadOnly := True;                              // disable possibility to edit the word ...
        WGuess2Form.WordToGuess.Color := clForm;
        WGuess2Form.ButtonSolve.Caption := 'Solve';                            // ... until the Solve button will be pressed again
        WGuess2Form.ButtonSolve.SetFocus;
      end
      else begin
        // The gallows is complete
        if SettingsScoringWords.Checked then begin
          if iPlayer = 1 then
            UpdateScoreWords(2)                                                // update score (by word found) for non-actual player
          else
            UpdateScoreWords(1);
        end
        else begin
          if iPlayer = 1 then
            edTrials1.Text := '25'                                             // complete gallows => number of trials set to 25
          else begin
            edTrials2.Text := '25';
            UpdateScoreTrials(StrToInt(edTrials1.Text), StrToInt(edTrials2.Text)); // update score (by trials compare) for both players
          end;
        end;
        WGuess2Form.WordToGuess.Text := sWordGuess;                            // display the word
      end;
      WGuess2Form.ButtonStart.Enabled := True;                                 // the game may continue with a new word
      WGuess2Form.ButtonSolve.Caption := 'Solve';
      WGuess2Form.ButtonSolve.Enabled := False;                                // disable Solve button (until game continued)
      WGuess2Form.ButtonStart.SetFocus;
    end;
  end;
end;

{ Keyboard capture: Get key pressed on the keyboard and check if it is part of the actual word }

procedure TWGuess2Form.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

var
  S: UTF8string;
  I, J, L: Byte;
  NewLetter, LetterInWord: Boolean;

begin
  if bKeybEvent then begin                                                     // proceed only if keyboard capture is enabled (!)
    UTF8Key := LowerCase(UTF8Key); L := Length(UTF8Key);
    Inc(iUserAttempts);
    S := Letters.Text;
    // Check if the letter pressed hasn't already been entered
    NewLetter := True;
    for I := 1 to Length(S) do begin
      J := (I - 1) * 2 + 1;
      if Copy(S, J, L) = UTF8Key then
        NewLetter := False;
    end;
    if NewLetter then begin
      // Add the (new) letter to those already entered
      LetterInWord := False;
      S += UTF8Key;
      // Insert the letter in the displayed (hyphenated) word if it actually is part of it
      I := 1; J := 1;
      repeat
        if LowerCase(Copy(sWordGuess, I, L)) = UTF8Key then begin
          Delete(sWordDisplay, J, 1);
          Insert(Copy(sWordGuess, I, L), sWordDisplay, J);
          LetterInWord := True;
        end;
        // Non English letters (2 bytes)
        if not (sWordGuess[I] in ['a'..'z', 'A'..'Z']) then
          Inc(I);
        if not (sWordDisplay[J] in ['a'..'z', 'A'..'Z', '-']) then
          Inc(J);
        Inc(I); Inc(J);
      until I > Length(sWordGuess);
    end;
    // Update the form fields
    WGuess2Form.Letters.Text := S;                                             // list of letters entered by the user
    WGuess2Form.Attempts.Text := IntToStr(iUserAttempts);
    if iPlayer = 1 then
      edTrials1.Text := IntToStr(iUserAttempts)                                // number of guess attempts
    else
      edTrials2.Text := IntToStr(iUserAttempts);
    WGuess2Form.WordToGuess.Text := sWordDisplay;                              // the (hyphenated) word
    // Letter entered is not part of the word
    if not LetterInWord then begin
      aGallows[iPlayer, aGX[iPlayer]].Visible := True;                         // display part of actual player's gallows
      Inc(aGX[iPlayer]);
      // The gallow is complete now
      if aGX[iPlayer] > 10 then begin
        if SettingsScoringWords.Checked then begin
          if iPlayer = 1 then
            UpdateScoreWords(2)                                                // update score (by word found) of non-actual player
          else
            UpdateScoreWords(1);
        end
        else begin
          if iPlayer = 1 then
            edTrials1.Text := '25'                                             // complete gallows => number of trials set to 25
          else begin
            edTrials2.Text := '25';
            UpdateScoreTrials(StrToInt(edTrials1.Text), StrToInt(edTrials2.Text)); // update score (by trials compare) for both players
          end;
        end;
        WGuess2Form.WordToGuess.Text := sWordGuess;
        ButtonStart.Enabled := True;                                           // enable Next button (to continue with a new word)
        WGuess2Form.ButtonStart.SetFocus;
        ButtonSolve.Enabled := False;
        bKeybEvent := False;                                                   // stop catching keyboard entry (until game continued) (!)
      end;
    end
    else if LowerCase(sWordDisplay) = LowerCase(sWordGuess) then begin
      // The user has guessed the word
      WGuess2Form.WordToGuess.Font.Style := [fsBold];
      if SettingsScoringWords.Checked then
        UpdateScoreWords(iPlayer)
      else if iPlayer = 2 then
        UpdateScoreTrials(StrToInt(edTrials1.Text), StrToInt(edTrials2.Text));
      ButtonStart.Enabled := True;                                             // enable Next button (to continue with a new word)
      WGuess2Form.ButtonStart.SetFocus;
      ButtonSolve.Enabled := False;
      bKeybEvent := False;                                                     // stop catching keyboard entry (until game continued) (!)
    end;
  end;
end;

{ Update player 1 name in score grid (if it has been changed in edit field) }

procedure TWGuess2Form.edName1Change(Sender: TObject);

begin
  sgScore.Cells[0, 1] := edName1.Text;
end;

{ Update player 2 name in score grid (if it has been changed in edit field) }

procedure TWGuess2Form.edName2Change(Sender: TObject);

begin
    sgScore.Cells[0, 2] := edName2.Text;
end;

end.

