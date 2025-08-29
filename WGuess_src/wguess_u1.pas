//
// Main unit for WGuess application
//

unit wguess_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, PopupNotifier, LCLType, wguess_u2;

type
  { TWGuessForm }
  TWGuessForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuGame: TMenuItem;
    GameNew: TMenuItem;
    GameReset: TMenuItem;
    GameExit: TMenuItem;
    MenuHelp: TMenuItem;
    HelpHelp: TMenuItem;
    HelpAbout: TMenuItem;
    MenuSettings: TMenuItem;
    SettingsLetters: TMenuItem;
    SettingsFirst: TMenuItem;
    SettingsAccents: TMenuItem;
    AccentsEnable: TMenuItem;
    AccentsDisable: TMenuItem;
    AccentsTransform: TMenuItem;
    StaticText1: TStaticText;
    Language1: TRadioButton;
    Language2: TRadioButton;
    Language3: TRadioButton;
    Language4: TRadioButton;
    WordClass1: TCheckBox;
    WordClass2: TCheckBox;
    WordClass3: TCheckBox;
    WordLengthMin: TEdit;
    WordLengthMax: TEdit;
    LabelWord: TLabel;
    WordToGuess: TEdit;
    Attempts: TEdit;
    Letters: TEdit;
    ButtonStart: TButton;
    ButtonSolve: TButton;
    AboutNotifier: TPopupNotifier;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GameNewClick(Sender: TObject);
    procedure GameResetClick(Sender: TObject);
    procedure GameExitClick(Sender: TObject);
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
    procedure Language1Change(Sender: TObject);
    procedure Language2Change(Sender: TObject);
    procedure Language3Change(Sender: TObject);
    procedure Language4Change(Sender: TObject);
    procedure WordClass1Change(Sender: TObject);
    procedure WordClass2Change(Sender: TObject);
    procedure WordClass3Change(Sender: TObject);
    procedure WordLengthMaxEditingDone(Sender: TObject);
    procedure WordLengthMinEditingDone(Sender: TObject);
  end;

var
  Words: array[1..1000] of UTF8string;
  WordGuess, WordDisplay, WordDisplaySave: UTF8string;
  Language: Char;
  WordsNouns, WordsVerbs, WordsAdjectives, KeybEvent: Boolean;
  LengthMin, LengthMax, UserAttempts: Byte;
  WordCount, WordCount0: Word;
  WGuessForm: TWGuessForm;

procedure ResetGame;
procedure NewGame;
procedure GetWords;
procedure ReadFile(FileName: string; var IX: Word);
procedure FormUpdate;
procedure WordCountUpdate;
function UTF8Length(S: UTF8String): Word;

implementation

{$R *.lfm}

//
// Reset the game options to their default values
//

procedure ResetGame;

begin
  WGuessForm.SettingsLetters.Checked := True;                                  // show letters entered = enabled
  WGuessForm.Letters.Visible := True;
  WGuessForm.SettingsFirst.Checked := False;                                   // show first letter = disabled
  WGuessForm.AccentsEnable.Checked := True;                                    // enable words with accents/umlauts
  WGuessForm.AccentsDisable.Checked := False;
  WGuessForm.AccentsTransform.Checked := False;
  Language := 'L';                                                             // words language (Luxembourgish)
  WordsNouns := True; WordsVerbs := False; WordsAdjectives := False;           // words classes (nouns only)
  LengthMin := 0; LengthMax := 0;                                              // words length (any length)
  FormUpdate;                                                                  // update the form fields
  NewGame;                                                                     // start a new game
end;

//
// Start a new game
//

procedure NewGame;

begin
  // Give the user access to the game options
  WGuessForm.Language1.Enabled := True;
  WGuessForm.Language2.Enabled := True;
  WGuessForm.Language3.Enabled := True;
  WGuessForm.Language4.Enabled := True;
  WGuessForm.WordClass1.Enabled := True;
  WGuessForm.WordClass2.Enabled := True;
  WGuessForm.WordClass3.Enabled := True;
  WGuessForm.WordLengthMin.Enabled := True;
  WGuessForm.WordLengthMax.Enabled := True;
  // Clear the game fields
  WGuessForm.LabelWord.Caption := 'Word to guess:';
  WGuessForm.WordToGuess.Color := clForm;
  WGuessForm.WordToGuess.Font.Color := clDefault;
  WGuessForm.WordToGuess.Text := '';
  WGuessForm.Attempts.Text := '';
  WGuessForm.Letters.Text := '';
  // Starting button conditions
  WGuessForm.ButtonStart.Caption := 'Start';
  WGuessForm.ButtonStart.Enabled := True;
  WGuessForm.ButtonSolve.Caption := 'Solve';
  WGuessForm.ButtonSolve.Enabled := False;
  // Disable capture of keyboard (key pressed) events (!)
  KeybEvent := False;
end;

//
// Get the words for the current game
//

procedure GetWords;

var
  Classes, FileName: string;
  IX, I: Word;

begin
  Classes := ''; IX := 0;
  if WordsNouns then
    Classes += 'n';
  if WordsVerbs then
    Classes += 'v';
  if WordsAdjectives then
    Classes += 'a';
  // Read the words for given language and given word classes
  for I := 1 to Length(Classes) do begin
    FileName := Language + '_' + copy(Classes, I, 1) + '.txt';                 // word list filename
    ReadFile(FileName, IX);                                                    // IX = current index in the words array
  end;
  WordCount0 := IX; WordCount := WordCount0;
end;

//
// Read the words from a given data file
//

procedure ReadFile(FileName: string; var IX: Word);

var
  WordFile: Text;
  S: UTF8string;

begin
  Assign(WordFile, FileName); Reset(WordFile);
  while (not EOF(WordFile)) and (IX < 1000) do begin
    Readln(WordFile, S);
    if S <> '' then begin
      if ((LengthMax = 0) and (UTF8Length(S) >= LengthMin)) or                 // keep words with specified lengths only
         ((LengthMax <> 0) and (UTF8Length(S) >= LengthMin) and (UTF8Length(S) <= LengthMax)) then begin
        Inc(IX);
        Words[IX] := S;
      end;
    end;
  end;
  Close(WordFile);
end;

//
// Update the form fields (game options that might have been modified by the user)
//

procedure FormUpdate;

begin
  case Language of
    'E': WGuessForm.Language1.Checked := True;
    'F': WGuessForm.Language2.Checked := True;
    'G': WGuessForm.Language3.Checked := True;
    'L': WGuessForm.Language4.Checked := True;
  end;
  WGuessForm.Language1.TabStop := False;
  WGuessForm.Language2.TabStop := False;
  WGuessForm.Language3.TabStop := False;
  WGuessForm.Language4.TabStop := False;
  if WordsNouns then
    WGuessForm.WordClass1.Checked := True
  else
    WGuessForm.WordClass1.Checked := False;
  if WordsVerbs then
    WGuessForm.WordClass2.Checked := True
  else
    WGuessForm.WordClass2.Checked := False;
  if WordsAdjectives then
    WGuessForm.WordClass3.Checked := True
  else
    WGuessForm.WordClass3.Checked := False;
  WGuessForm.WordLengthMin.Text := IntToStr(LengthMin);
  WGuessForm.WordLengthMax.Text := IntToStr(LengthMax);
end;

//
// (Re)calculate the number of remaining guess words in the list
//

procedure WordCountUpdate;

var
  I, J: Integer;
  S: UTF8String;
  DoCount: Boolean;

begin
  WordCount := 0;
  for I := 1 to WordCount0 do begin
    if Words[I] <> '' then begin                                               // count words not yet done, only
      DoCount := True;
      if WGuessForm.AccentsDisable.Checked then begin
        // Accents/umlauts disabled
        S := Words[I]; J := 1;
        while (J <= Length(S)) and DoCount do begin
          if not (S[J] in ['a'..'z', 'A'..'Z']) then                           // non-English letter
            DoCount := False;
          Inc(J);
        end;
      end;
      if DoCount then
        Inc(WordCount);
    end;
  end;
end;

//
// Function UTF8Length: Get length of UTF8String
//

function UTF8Length(S: UTF8String): Word;

var
  I: Byte;
  L: Word;

begin
  // Note: the routine considers letters only!!!
  L := 0; I := 1;
  while I <= Length(S) do begin
    if not (S[I] in ['a'..'z', 'A'..'Z']) then                                 // non-English letter (2 bytes)
      Inc(I);
    Inc(L); Inc(I);
  end;
  UTF8Length := L;
end;

{ =========== }
{ TWGuessForm }
{ =========== }

//
// Application start: Initialize the game
//

procedure TWGuessForm.FormCreate(Sender: TObject);

begin
  Randomize;
  ResetGame;
end;

//
// Menu item "Game > New": Start a new game
//

procedure TWGuessForm.GameNewClick(Sender: TObject);

begin
  NewGame;
end;

//
// Menu item "Game > Reset": Start a new game with default options
//

procedure TWGuessForm.GameResetClick(Sender: TObject);

begin
  ResetGame;
end;

//
// Menu item "Game > Exit": Exit the application
//

procedure TWGuessForm.GameExitClick(Sender: TObject);

begin
  Close();
end;

//
// Menu item "Settings > Show letters used": Toggle show/hide letters used
//

procedure TWGuessForm.SettingsLettersClick(Sender: TObject);

begin
  if SettingsLetters.Checked then begin
    SettingsLetters.Checked := False;
    Letters.Visible := False;                                                  // hide used letters
  end
  else begin
    SettingsLetters.Checked := True;
    Letters.Visible := True;                                                   // show used letters
  end;
end;

//
// Menu item "Settings > Show first letter": Toggle show/don't show first letter
//

procedure TWGuessForm.SettingsFirstClick(Sender: TObject);

begin
  if SettingsFirst.Checked then
    SettingsFirst.Checked := False
  else
    SettingsFirst.Checked := True;
end;

//
// Menu item "Settings > Accents/umlauts > Enable": Enable (allow) words with accents/umlauts
//

procedure TWGuessForm.AccentsEnableClick(Sender: TObject);

begin
  if not AccentsEnable.Checked then begin
    AccentsDisable.Checked := False;
    AccentsTransform.Checked := False;
    AccentsEnable.Checked := True;
    WordCountUpdate;                                                           // be sure to count all words
  end;
end;

//
// Menu item "Settings > Accents/umlauts > Disable": Disable (ignore) words with accents/umlauts
//

procedure TWGuessForm.AccentsDisableClick(Sender: TObject);

begin
  if not AccentsDisable.Checked then begin
    AccentsEnable.Checked := False;
    AccentsTransform.Checked := False;
    AccentsDisable.Checked := True;
    WordCountUpdate;                                                           // be sure not to count words with accents/umlauts
  end;
end;

//
// Menu item "Settings > Accents/umlauts > Transform": Enable transformed words with accents/umlauts
//

procedure TWGuessForm.AccentsTransformClick(Sender: TObject);

begin
  if not AccentsTransform.Checked then begin
    AccentsEnable.Checked := False;
    AccentsDisable.Checked := False;
    AccentsTransform.Checked := True;
    WordCountUpdate;                                                           // be sure to count all words
  end;
end;

//
// Menu item "Help > Help": Display help content (as form)
//

procedure TWGuessForm.HelpHelpClick(Sender: TObject);

begin
  HelpForm.Show;
end;

//
// Menu item "Help > About": Display application info (as pop-up)
//

procedure TWGuessForm.HelpAboutClick(Sender: TObject);

begin
  if not AboutNotifier.Visible then
    AboutNotifier.Visible := True;
end;

//
// Button "Start/Next": Start/continue the game with a new word
//

procedure TWGuessForm.ButtonStartClick(Sender: TObject);

var
  Ch0: TUTF8Char;
  Word0, Word1: UTF8string;
  WordOK: Boolean;
  W: Word;
  I: Byte;
  S: string;

begin
  // Game start (first word for new game)
  if ButtonStart.Caption = 'Start' then begin
    WGuessForm.WordToGuess.ReadOnly := True;                                   // user can't edit the word
    FormUpdate;                                                                // just to be sure to catch all changes
    GetWords;                                                                  // read words (as selected) from data file(s)
    WordCountUpdate;                                                           // be sure to get correct number of guess words
  end;
  // Play the game with next random word
  if WordCount > 0 then begin                                                  // proceed if there are (not yet guessed) words left
    // Remove user access to game options
    WGuessForm.Language1.Enabled := False;
    WGuessForm.Language2.Enabled := False;
    WGuessForm.Language3.Enabled := False;
    WGuessForm.Language4.Enabled := False;
    WGuessForm.WordClass1.Enabled := False;
    WGuessForm.WordClass2.Enabled := False;
    WGuessForm.WordClass3.Enabled := False;
    WGuessForm.WordLengthMin.Enabled := False;
    WGuessForm.WordLengthMax.Enabled := False;
    // Get a random word (from those read for this game)
    repeat
      W := Random(WordCount0) + 1; WordOK := True;
      if Words[W] = '' then                                                    // word already displayed for guess
        WordOK := False
      else begin
        if WGuessForm.AccentsDisable.Checked then begin
          // Ignore words having accents/umlauts (if this option is checked)
          I := 1; Word0 := Words[W];
          repeat
            if not (Word0[I] in ['a'..'z', 'A'..'Z']) then                     // non English letter
              Word0 := '';
            Inc(I);
          until (Word0 = '') or (I >= Length(Words[W]));
          if Word0 = '' then
            WordOK := False;
        end
        else if WGuessForm.AccentsTransform.Checked then begin
          // Transform accents/umlauts to English letters (if this option is checked)
          I := 1; Word0 := Words[W]; Word1 := '';
          repeat
            if Word0[I] in ['a'..'z', 'A'..'Z'] then
              Word1 += Word0[I]                                                // the transformed word
            else begin
              // Accent/umlaut transformation
              Ch0 := Copy(Word0, I, 2);
              case Ch0 of
                'à' : Word1 += 'a';
                'â' : Word1 += 'a';
                'é' : Word1 += 'e';
                'è' : Word1 += 'e';
                'ê' : Word1 += 'e';
                'ë' : Word1 += 'e';
                'î' : Word1 += 'i';
                'ï' : Word1 += 'i';
                'ù' : Word1 += 'u';
                'û' : Word1 += 'u';
                'ä' : Word1 += 'ae';
                'ö' : Word1 += 'oe';
                'ü' : Word1 += 'ue';
                'É' : Word1 += 'E';
                'Î' : Word1 += 'I';
                'Ä' : Word1 += 'Ae';
                'Ö' : Word1 += 'Oe';
                'Ü' : Word1 += 'Ue';
              end;
              Inc(I);
            end;
            Inc(I);
          until (Word0 = '') or (I > Length(Words[W]));
          Words[W] := Word1;
        end;
      end;
    until WordOK;
    WordGuess := Words[W];                                                     // the word to be guessed
    // Display the word as hyphens
    WordDisplay := ''; I := 1;
    repeat
      if not (WordGuess[I] in ['a'..'z', 'A'..'Z']) then
        // Non English letter (2 bytes)
        Inc(I);
      WordDisplay += '-';
      Inc(I);
    until I > Length(WordGuess);
    if WGuessForm.SettingsFirst.Checked then begin                             // show first character (if this option is selected)
      Delete(WordDisplay, 1, 1);
      if WordGuess[1] in ['a'..'z', 'A'..'Z'] then                             // English letter (1 byte)
        WordDisplay := WordGuess[1] + WordDisplay
      else                                                                     // non-English letter (2 bytes)
        WordDisplay := Copy(WordGuess, 1, 2) + WordDisplay;
    end;
    S := 'Word to guess [' + IntToStr(WordCount) + ']';
    WGuessForm.LabelWord.Caption := S;
    Words[W] := ''; Dec(WordCount);                                            // remove word from list
    WGuessForm.WordToGuess.Color := clForm;
    WGuessForm.WordToGuess.Font.Color := clDefault;
    WGuessForm.WordToGuess.Font.Style := [];
    WGuessForm.WordToGuess.Text := WordDisplay;                                // display the hyphens
    UserAttempts := 0; WGuessForm.Attempts.Text:= IntToStr(UserAttempts);      // reset the number of guess attempts
    WGuessForm.Letters.Text := '';
    WGuessForm.ButtonStart.Caption := 'Next';                                  // change caption of Start button
    WGuessForm.ButtonStart.Enabled := False;                                   // disable Start button (until word will be guessed)
    WGuessForm.ButtonSolve.Enabled := True;                                    // enable Solve button (give possibility to enter whole word)
    WGuessForm.ButtonSolve.SetFocus;
    KeybEvent := True;                                                         // catch keyboards events (= listen for letter-keys pressed)
  end
  // No words found for actual settings or all words done
  else begin
    if WordCount0 = 0 then begin
      // No words found
      WGuessForm.WordToGuess.Font.Color := clRed;
      WGuessForm.WordToGuess.Text := 'No words found!';
    end
    else begin
      // All words done
      WGuessForm.WordToGuess.Font.Color := clFuchsia;
      WGuessForm.WordToGuess.Text := 'All words done!';
      WGuessForm.Attempts.Text := '';
      WGuessForm.Letters.Text := '';
    end;
  end;
end;

//
// Button "Solve/Check": Give possibility to solve / check the solution
//

procedure TWGuessForm.ButtonSolveClick(Sender: TObject);

begin
  if WGuessForm.ButtonSolve.Caption = 'Solve' then begin
    // Give the user the possibility to solve
    KeybEvent := False;                                                        // disable catching of keyboard events (!)
    WGuessForm.WordToGuess.ReadOnly := False;                                  // give user the possibility to edit the word
    WGuessForm.WordToGuess.Color := clDefault;
    WGuessForm.WordToGuess.SetFocus;
    WGuessForm.ButtonSolve.Caption := 'Check';
    WordDisplaySave := WordDisplay;                                            // save displayed (hyphened) guess word
  end
  else begin
    // Check the solution proposed by the user
    Inc(UserAttempts); Attempts.Text := IntToStr(UserAttempts);
    WordDisplay := WGuessForm.WordToGuess.Text;
    if LowerCase(WordDisplay) = LowerCase(WordGuess) then begin
      // Solution is correct
      WGuessForm.WordToGuess.Text := WordGuess;
      WGuessForm.WordToGuess.Font.Style := [fsBold];
      WGuessForm.ButtonStart.Enabled := True;                                  // the game may continue with a new word
      WGuessForm.ButtonSolve.Caption := 'Solve';
      WGuessForm.ButtonSolve.Enabled := False;                                 // disable Solve button (until game continued)
      WGuessForm.ButtonStart.SetFocus;
    end
    else begin
      // Solution is false
      WordDisplay := WordDisplaySave;                                          // restore displayed (hyphened) guess word
      WGuessForm.WordToGuess.Text := WordDisplay;
      KeybEvent := True;                                                       // enable catching of keyboard events (!)
      WGuessForm.WordToGuess.ReadOnly := True;                                 // disable possibility to edit the word ...
      WGuessForm.WordToGuess.Color := clForm;
      WGuessForm.ButtonSolve.Caption := 'Solve';                               // ... until the Solve button is pressed again
      WGuessForm.ButtonSolve.SetFocus;
    end;
  end;
end;

//
// Keyboard capture: Get key pressed on the keyboard and check if it is part of the actual word
//

procedure TWGuessForm.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

var
  S: UTF8string;
  I, J, L: Byte;
  NewLetter: Boolean;

begin
  if KeybEvent then begin                                                      // proceed only if keyboard capture is enabled (!)
    UTF8Key := LowerCase(UTF8Key); L := Length(UTF8Key);
    Inc(UserAttempts);
    S := Letters.Text;
    // Check if the letter pressed hasn't already been entered
    NewLetter := True;
    for I := 1 to Length(S) do begin
      J := (I - 1) * 2 + 1;
      if Copy(S, J, L) = UTF8Key then
        NewLetter := False;
    end;
    if NewLetter then begin
      // Add the letter to those already entered
      S := S + UTF8Key + ' ';
      // Insert the letter in the displayed (hyphenated) word if it actually is part of it
      I := 1; J := 1;
      repeat
        if LowerCase(Copy(WordGuess, I, L)) = UTF8Key then begin
          Delete(WordDisplay, J, 1);
          Insert(Copy(WordGuess, I, L), WordDisplay, J);
        end;
        // Non English letters (2 bytes)
        if not (WordGuess[I] in ['a'..'z', 'A'..'Z']) then
          Inc(I);
        if not (WordDisplay[J] in ['a'..'z', 'A'..'Z', '-']) then
          Inc(J);
        Inc(I); Inc(J);
      until I > Length(WordGuess);
    end;
    // Update the form fields
    WGuessForm.Letters.Text := S;                                              // all letters entered by the user
    WGuessForm.Attempts.Text := IntToStr(UserAttempts);
    WGuessForm.WordToGuess.Text := WordDisplay;
    if LowerCase(WordDisplay) = LowerCase(WordGuess) then begin
      // The user has guessed the word
      WGuessForm.WordToGuess.Font.Style := [fsBold];
      ButtonStart.Enabled := True;                                             // enable Next button (to continue with a new word)
      WGuessForm.ButtonStart.SetFocus;
      ButtonSolve.Enabled := False;
      KeybEvent := False;                                                      // stop catching keyboard entry (until game continued) (!)
    end;
  end;
end;

//
// Program options: Language selection
//

procedure TWGuessForm.Language1Change(Sender: TObject);

begin
  Language := 'E';
  WGuessForm.Language1.TabStop := False;
end;

procedure TWGuessForm.Language2Change(Sender: TObject);

begin
  Language := 'F';
  WGuessForm.Language2.TabStop := False;
end;

procedure TWGuessForm.Language3Change(Sender: TObject);

begin
  Language := 'G';
  WGuessForm.Language3.TabStop := False;
end;

procedure TWGuessForm.Language4Change(Sender: TObject);

begin
  Language := 'L';
  WGuessForm.Language4.TabStop := False;
end;

//
// Program options: Word classes selection
//

procedure TWGuessForm.WordClass1Change(Sender: TObject);

begin
  if WordsNouns then
    WordsNouns := False
  else
    WordsNouns := True;
end;

procedure TWGuessForm.WordClass2Change(Sender: TObject);

begin
  if WordsVerbs then
    WordsVerbs := False
  else
    WordsVerbs := True;
end;

procedure TWGuessForm.WordClass3Change(Sender: TObject);

begin
  if WordsAdjectives then
    WordsAdjectives := False
  else
    WordsAdjectives := True;
end;

//
// Program options: Word length

procedure TWGuessForm.WordLengthMinEditingDone(Sender: TObject);

begin
  LengthMin := StrToInt(WordLengthMin.Text);
end;

procedure TWGuessForm.WordLengthMaxEditingDone(Sender: TObject);

begin
  LengthMax := StrToInt(WordLengthMax.Text);
end;

end.

