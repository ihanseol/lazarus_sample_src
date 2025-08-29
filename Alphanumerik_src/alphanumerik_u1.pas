{**************************************** *}
{* Main unit for Alphanumerik application *}
{******************************************}

unit alphanumerik_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, LazUTF8;

type
  TAllWords = array[0..3] of array of string;
  TWords = array of string;
  TWordsDone = array of Boolean;
  TKeys = array[0..9] of string;
  {****************}
  { TfAlphanumerik }
  {****************}
  TfAlphanumerik = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsLanguage, mOptionsLanguageLUX, mOptionsLanguageGER, mOptionsLanguageFRA, mOptionsLanguageEng: TMenuItem;
    mOptionsWordLength, mOptionsWordLength9, mOptionsWordLength8, mOptionsWordLength10, mOptionsWordLength12: TMenuItem;
    mOptionsGivenChars, mOptionsGivenChars5, mOptionsGivenChars4, mOptionsGivenChars3: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1, StaticText15, StaticText16, StaticText17: TStaticText;
    Label1, Label2: TLabel;
    Shape1, Shape2, Shape3: TShape;
    Memo1: TMemo;
    laLetter1, laLetter2, laLetter3, laLetter4, laLetter5, laLetter6: TLabel;
    laLetter7, laLetter8, laLetter9, laLetter10, laLetter11, laLetter12: TLabel;
    shN0, shN1, shN2, shN3, shN4, shN5, shN6, shN7, shN8, shN9, shStar, shNumberSign: TShape;
    shPhoneUp, shPhoneDown, shLeft, shRight, shMiddle, shCursor: TShape;
    stN1: TStaticText;
    stN2: TStaticText;
    stN3: TStaticText;
    stN4: TStaticText;
    stN5: TStaticText;
    stN6: TStaticText;
    stN7: TStaticText;
    stN8: TStaticText;
    stN9: TStaticText;
    stN0: TStaticText;
    stStar: TStaticText;
    stNumberSign: TStaticText;
    edNumbers: TEdit;
    edWord: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsLanguageEngClick(Sender: TObject);
    procedure mOptionsLanguageFRAClick(Sender: TObject);
    procedure mOptionsLanguageGERClick(Sender: TObject);
    procedure mOptionsLanguageLUXClick(Sender: TObject);
    procedure mOptionsWordLength12Click(Sender: TObject);
    procedure mOptionsWordLength10Click(Sender: TObject);
    procedure mOptionsWordLength8Click(Sender: TObject);
    procedure mOptionsWordLength9Click(Sender: TObject);
    procedure mOptionsGivenChars3Click(Sender: TObject);
    procedure mOptionsGivenChars4Click(Sender: TObject);
    procedure mOptionsGivenChars5Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure shLeftMouseDown(Sender: TObject);
    procedure shMiddleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shRightMouseDown(Sender: TObject);
    procedure shPhoneUpMouseDown(Sender: TObject);
    procedure shPhoneDownMouseDown(Sender: TObject);
    procedure stN0Click(Sender: TObject);
    procedure stN1Click(Sender: TObject);
    procedure stN2Click(Sender: TObject);
    procedure stN3Click(Sender: TObject);
    procedure stN4Click(Sender: TObject);
    procedure stN5Click(Sender: TObject);
    procedure stN6Click(Sender: TObject);
    procedure stN7Click(Sender: TObject);
    procedure stN8Click(Sender: TObject);
    procedure stN9Click(Sender: TObject);
    procedure stNumberSignClick(Sender: TObject);
    procedure stStarClick(Sender: TObject);
  private
    iLanguage, iChars, iGiven, iOldKey, iKey, iKeyLetter, iPos, iQuestions, iCorrect: Integer;
    sWord: string;
    bKeysEnabled: Boolean;
    rPushTime, rOldPushTime: TDateTime;
    aAllWords: TAllWords;
    aWords: TWords;
    aWordsDone: TWordsDone;
    aKeys: TKeys;
    laLetters: array[1..12] of TLabel;
  end;

var
  fAlphanumerik: TfAlphanumerik;

implementation

{$R *.lfm}

{ Transform letter/symbol to correspondig number }

function WordToNumbers(var Keys: TKeys; Wrd: string): string;

var
  I, J, K: Integer;
  Letters, Number: string;
  Found: Boolean;

begin
  Number := '';
  for I := 1 to UTF8Length(Wrd) do begin
    Found := False;
    for J := 2 to 9 do begin
      if not Found then begin
        Letters := Keys[J];
        for K := 1 to UTF8Length(Letters) do begin
          if not Found then begin
            if UTF8Copy(Letters, K, 1) = UTF8Copy(Wrd, I, 1) then begin
              Found := True;
              Number += IntToStr(J);
            end;
          end;
        end;
      end;
    end;
  end;
  Result := Number;
end;

{ Read the 4 word lists from text file }

procedure ReadAllWords(out AllWords: TAllWords);

const
  Languages: array[0..3] of string = (
    'eng', 'fra', 'ger', 'lux'
  );

var
  I, J: Integer;
  Filename, Line: string;
  InFile: Text;

begin
  for I := 0 to 3 do begin
    Filename := 'words_' + Languages[I] + '.txt';
    Assign(InFile, Filename); Reset(InFile);
    J := 0;
    while not EoF(InFile) do begin
      Readln(InFile, Line); Line := UTF8Trim(Line);
      if UTF8Length(Line) in [8..10, 12] then begin
        Inc(J); SetLength(AllWords[I], J);
        AllWords[I][J - 1] := Line;
      end;
    end;
    Close(InFile);
  end;
end;

{ Read the cell phone key definitions from text file }

procedure ReadKeys(out Keys: TKeys);

var
  I: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'keys.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := UTF8Trim(Line);
    if Line <> '' then begin
      I := StrToInt(LeftStr(Line, 1)); UTF8Delete(Line, 1, 2);
      Keys[I] := Line;
    end;
  end;
  Close(InFile);
end;

{ Start a new game }

procedure NewGame(var AllWords: TAllWords; Language, Chars: Integer; out Words: TWords; out WordsDone: TWordsDone; out Questions, Correct: Integer);

var
  N, I: Integer;

begin
  // Create actual word list (given language, given number of letters)
  N := 0;
  for I := 0 to Length(AllWords[Language]) - 1 do begin
    if Length(AllWords[Language][I]) = Chars then begin
      Inc(N);
      SetLength(Words, N); Words[N - 1] := AllWords[Language][I];
      SetLength(WordsDone, N); WordsDone[N - 1] := False;
    end;
  end;
  // Clear the cell phone display
  for I := 1 to 12 do
    fAlphanumerik.laLetters[I].Visible := False;
  fAlphanumerik.shCursor.Left := 33;
  // Init evaluation counters
  Questions := 0; Correct := 0;
end;

{ Cell phone number key pushed: Display the corresponding character }

procedure NumberKeypushed(var Keys: TKeys; var OldKey, Key, KeyLetter, WPos, WLength: Integer; var PushTime, OldPushTime: TDateTime);

var
  SaveKey: Integer;

begin
  SaveKey := OldKey;
  if PushTime <> 0 then
    OldPushTime := PushTime;
  PushTime := TimeStampToMSecs(DateTimeToTimeStamp(Now));
  if Key = OldKey then begin
    // Key pushed is the same as before
    if PushTime - OldPushTime < 1000 then begin
      // Less than 1s since key was pushed: Display next character defined for this key at actual cursor position
      Inc(KeyLetter);
      if KeyLetter > UTF8Length(Keys[Key]) then
        KeyLetter := 1;
    end
    else begin
      // More than 1s since key was pushed: Display first character defined for this key at next cursor position
      KeyLetter := 1;
      if WPos < WLength then
        Inc(WPos);
    end;
  end
  else begin
    // Key pushed is another than before: Display first character defined for this key at next cursor position
    KeyLetter := 1;
    if WPos < WLength then
      Inc(WPos);
    OldKey := Key;
  end;
  // Display character only if cursor position is within word limits
  if (Key = SaveKey) or (fAlphanumerik.shCursor.Left < 33 + WLength * 20) then
    fAlphanumerik.laLetters[WPos].Caption := UTF8Copy(Keys[Key], KeyLetter, 1);
  // Update cursor position
  fAlphanumerik.shCursor.Left := 33 + WPos * 20;
end;

{ Undefined cell phone key pushed: Display message }

procedure Undefined;

begin
  MessageDlg('"Alphanumerik"', 'This button isn''t actually defined!', mtWarning, [mbOK], 0);
end;

{****************}
{ TfAlphanumerik }
{****************}

{ Application start: Initialization }

procedure TfAlphanumerik.FormCreate(Sender: TObject);

begin
  laLetters[1] := laLetter1; laLetters[2] := laLetter2; laLetters[3] := laLetter3;
  laLetters[4] := laLetter4; laLetters[5] := laLetter5; laLetters[6] := laLetter6;
  laLetters[7] := laLetter7; laLetters[8] := laLetter8; laLetters[9] := laLetter9;
  laLetters[10] := laLetter10; laLetters[11] := laLetter11; laLetters[12] := laLetter12;
  iLanguage := 0;
  iChars := 8; iGiven := 3;
  ReadAllWords(aAllWords);
  ReadKeys(aKeys);
  Randomize;
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new game }

procedure TfAlphanumerik.mGameNewClick(Sender: TObject);

begin
  NewGame(aAllWords, iLanguage, iChars, aWords, aWordsDone, iQuestions, iCorrect);
  shLeftMouseDown(self);                                                       // start a new word
end;

{ Menu item "Game > Exit": Exit application }

procedure TfAlphanumerik.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Language > ...": Select words language }

procedure TfAlphanumerik.mOptionsLanguageEngClick(Sender: TObject);

begin
  mOptionsLanguageEng.Checked := True;  mOptionsLanguageFra.Checked := False;
  mOptionsLanguageGer.Checked := False; mOptionsLanguageLux.Checked := False;
  iLanguage := 0;
  mGameNew.Click;                                                              // start a new game
end;

procedure TfAlphanumerik.mOptionsLanguageFRAClick(Sender: TObject);

begin
  mOptionsLanguageEng.Checked := False; mOptionsLanguageFra.Checked := True;
  mOptionsLanguageGer.Checked := False; mOptionsLanguageLux.Checked := False;
  iLanguage := 1;
  mGameNew.Click;
end;

procedure TfAlphanumerik.mOptionsLanguageGERClick(Sender: TObject);

begin
  mOptionsLanguageEng.Checked := False; mOptionsLanguageFra.Checked := False;
  mOptionsLanguageGer.Checked := True;  mOptionsLanguageLux.Checked := False;
  iLanguage := 2;
  mGameNew.Click;
end;

procedure TfAlphanumerik.mOptionsLanguageLUXClick(Sender: TObject);

begin
  mOptionsLanguageEng.Checked := False; mOptionsLanguageFra.Checked := False;
  mOptionsLanguageGer.Checked := False; mOptionsLanguageLux.Checked := True;
  iLanguage := 3;
  mGameNew.Click;
end;

{ Menu items "Options > Word length > ...": Select maximum word length }

procedure TfAlphanumerik.mOptionsWordLength8Click(Sender: TObject);

begin
  mOptionsWordLength8.Checked := True;   mOptionsWordLength9.Checked := False;
  mOptionsWordLength10.Checked := False; mOptionsWordLength12.Checked := False;
  iChars := 8;
  mGameNew.Click;                                                              // start a new game
end;

procedure TfAlphanumerik.mOptionsWordLength9Click(Sender: TObject);

begin
  mOptionsWordLength8.Checked := False;  mOptionsWordLength9.Checked := True;
  mOptionsWordLength10.Checked := False; mOptionsWordLength12.Checked := False;
  iChars := 9;
  mGameNew.Click;
end;

procedure TfAlphanumerik.mOptionsWordLength10Click(Sender: TObject);

begin
  mOptionsWordLength8.Checked := False; mOptionsWordLength9.Checked := False;
  mOptionsWordLength10.Checked := True; mOptionsWordLength12.Checked := False;
  iChars := 10;
  mGameNew.Click;
end;

procedure TfAlphanumerik.mOptionsWordLength12Click(Sender: TObject);

begin
  mOptionsWordLength8.Checked := False;  mOptionsWordLength9.Checked := False;
  mOptionsWordLength10.Checked := False; mOptionsWordLength12.Checked := True;
  iChars := 12;
  mGameNew.Click;
end;

{ Menu items "Options > Given characters > ...": Select number of word characters given by the application }

procedure TfAlphanumerik.mOptionsGivenChars3Click(Sender: TObject);

begin
  mOptionsGivenChars3.Checked := True; mOptionsGivenChars4.Checked := False; mOptionsGivenChars5.Checked := False;
  iGiven := 3;
  mGameNew.Click;
end;

procedure TfAlphanumerik.mOptionsGivenChars4Click(Sender: TObject);

begin
  mOptionsGivenChars3.Checked := False; mOptionsGivenChars4.Checked := True; mOptionsGivenChars5.Checked := False;
  iGiven := 4;
  mGameNew.Click;
end;

procedure TfAlphanumerik.mOptionsGivenChars5Click(Sender: TObject);

begin
  mOptionsGivenChars3.Checked := False; mOptionsGivenChars4.Checked := False; mOptionsGivenChars5.Checked := True;
  iGiven := 5;
  mGameNew.Click;
end;

{ Menu item "Help > About": Display application about }

procedure TfAlphanumerik.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := '"Guess the word" game with cellphone GUI.' + LineEnding;
  S += 'The game is based on a kind of puzzle, that I found in the free Luxembourgish newspaper "L''essentiel".' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2021 - December 2022.';
  MessageDlg('About "Alphanumerik"', S, mtInformation, [mbOK], 0);
end;

{ Left cell phone button pushed: Generate a new word }

procedure TfAlphanumerik.shLeftMouseDown(Sender: TObject);

var
  R, I: Integer;

begin
  Inc(iQuestions);
  if iQuestions <= Length(aWords) then begin
    // If there are still questions left, generate new word
    iOldKey := -1; iKeyLetter := 1; iPos := 0;
    rOldPushTime := 0; rPushTime := 0;
    // Get random word (that has not yet been asked)
    repeat
      R := Random(Length(aWords));
    until not aWordsDone[R];
    sWord := UTF8UpperCase(aWords[R]);
    // Mark the word as done
    aWordsDone[R] := True;
    // Display the word as hyphens, except for the characters given
    edWord.Text := sWord;
    edNumbers.Text := WordToNumbers(aKeys, sWord);
    for I := 1 to UTF8Length(sWord) - iGiven do begin
      // Random given characters positions
      repeat
        R := Random(UTF8Length(sWord)) + 1;
      until UTF8Copy(edWord.Text, R, 1) <> '-';
      if R = 1 then
        edWord.Text := '-' + UTF8Copy(edWord.Text, 2, UTF8Length(edWord.Text))
      else
        edWord.Text := UTF8Copy(edWord.Text, 1, R - 1) + '-' + UTF8Copy(edWord.Text, R + 1, UTF8Length(edWord.Text));
    end;
    // Display the hyphenated word at the cell phone screen
    for I := 1 to UTF8Length(sWord) do
      laLetters[I].Visible := True;
    for I := 1 to UTF8Length(sWord) do
      laLetters[I].Caption := UTF8Copy(edWord.Text, I, 1);
    // Set the cell phone cursor before the first word character
    shCursor.Left := 33;
    // Enable cell phone buttons
    bKeysEnabled := True;
  end
  else begin
    // If there aren't questions left, display message
    MessageDlg('"Alphanumerik"', 'All words done. Please, start a new game...', mtInformation, [mbOK], 0);
  end;
end;

{ Middle cell phone button pushed: Take action, depending on which part of the button has been pushed }

procedure TfAlphanumerik.shMiddleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  I: Integer;
  S: string;
  Correct: Boolean;

begin
  if bKeysEnabled then begin
    if (Y > 20) and (Y < shMiddle.Height - 20) then begin
      if (X < 25) then begin
        // Left part of middle cell phone button pushed: Move cell phone cursor left
        if iPos >= 1 then begin
          Dec(iPos);
          shCursor.Left := 33 + iPos * 20;
        end;
      end
      else if (X > shMiddle.Width - 25) then begin
        // Right part of middle cell phone button pushed: Move cell phone cursor right
        if iPos < iChars then begin
          Inc(iPos);
          shCursor.Left := 33 + iPos * 20;
        end;
      end
      else begin
        // Middle part of middle cell phone button pushed: Check user answer
        bKeysEnabled := False;
        // If one character of the user input word does not match the corr. character of the word generated, the answer is false
        Correct := True;
        for I := 1 to UTF8Length(sWord) do begin
          if UTF8Copy(sWord, I, 1) <> laLetters[I].Caption then
            Correct := False;
        end;
        // User answer evaluation (with update of the evaluation counters)
        if Correct then begin
          Inc(iCorrect);
          S := 'Nice! You have found the word.' + LineEnding
        end
        else
          S := 'Sorry, this is not correct. The correct word is: ' + sWord + '.' + LineEnding;
        S += 'Words asked: ' + IntToStr(iQuestions) + ', words found: ' + IntToStr(iCorrect);
        S += ' - success: ' + IntToStr(Round(100 * iCorrect / iQuestions)) + '%';
        MessageDlg('"Alphanumerik"', S, mtInformation, [mbOK], 0);
      end;
    end;
  end;
end;

{ Right cell phone button pushed: Clear character before cursor position }

procedure TfAlphanumerik.shRightMouseDown(Sender: TObject);

begin
  if bKeysEnabled then begin
    if iPos >= 1 then begin
      laLetters[iPos].Caption := '-';
      Dec(iPos);
      shCursor.Left := 33 + iPos * 20;                                         // Move cell phone cursor one position backwards
    end;
  end;
end;

{ Cell phone number buttons pushed: Display the corresponding character }

procedure TfAlphanumerik.stN0Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 0;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN1Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 1;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN2Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 2;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN3Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 3;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN4Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 4;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN5Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 5;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN6Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 6;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN7Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 7;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN8Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 8;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

procedure TfAlphanumerik.stN9Click(Sender: TObject);

begin
  if bKeysEnabled then begin
    iKey := 9;
    NumberKeypushed(aKeys, iOldKey, iKey, iKeyLetter, iPos, iChars, rPushTime, rOldPushTime);
  end;
end;

{ Cell phone "*" or "#" buttons pushed: Display "not defined" message }

procedure TfAlphanumerik.stStarClick(Sender: TObject);

begin
  Undefined;
end;

procedure TfAlphanumerik.stNumberSignClick(Sender: TObject);

begin
  Undefined;
end;

{ Cell phone "phone up" or "phone down" buttons pushed: Display "not defined" message }

procedure TfAlphanumerik.shPhoneUpMouseDown(Sender: TObject);

begin
  Undefined;
end;

procedure TfAlphanumerik.shPhoneDownMouseDown(Sender: TObject);

begin
  Undefined;
end;

end.

