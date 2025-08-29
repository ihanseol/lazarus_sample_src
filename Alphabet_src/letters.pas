{**************************************}
{* Main unit for Alphabet application *}
{**************************************}

// Version 1.0 (June 2019)
// Version 1.1 (December 2019):
//   - User choice between English and German letters.
//   - Addition of German word list and letters audio files.
// Version 1.2 (January 2020):
//   - Correcting bug with upper-/lowercase choice
//   - Making language choice available during exercise
//   - Removal of the "clicking noise" at the end of the German letters audio files.

unit letters;

// The unit uses MMSystem (Windows-API unit) for audio playback (thus runs on MS Windows only)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, LCLType, LazUTF8, MMSystem;

type
  TLetters = array of TUTF8Char;
  TWordList = array of string;
  TShapes = array[1..5, 1..5] of TShape;
  TLabels = array[1..5, 1..5] of TStaticText;
  TGrid   = array[1..5, 1..5] of string;
  {************}
  { TfAlphabet }
  {************}
  TfAlphabet = class(TForm)
    mMenu: TMainMenu;
    mAlphabet, mAlphabetGLetters, mAlphabetWLetters, mAlphabetExit: TMenuItem;
    mOptionsLanguage, mOptionsLanguageEng, mOptionsLanguageGer: TMenuItem;
    mOptions, mOptionsUpper, mOptionsLower: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    sh1, sh2, sh3, sh4, sh5: TShape;
    sh6, sh7, sh8, sh9, sh10: TShape;
    sh11, sh12, sh13, sh14, sh15: TShape;
    sh16, sh17, sh18, sh19, sh20: TShape;
    sh21, sh22, sh23, sh24, sh25: TShape;
    st1, st2, st3, st4, st5: TStaticText;
    st6, st7, st8, st9, st10: TStaticText;
    st11, st12, st13, st14, st15: TStaticText;
    st16, st17, st18, st19, st20: TStaticText;
    st21, st22, st23, st24, st25: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edQuestion, edCorrect, edFalse, edSuccess: TEdit;
    btStart: TButton;
    btCheck: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mAlphabetGLettersClick(Sender: TObject);
    procedure mAlphabetWLettersClick(Sender: TObject);
    procedure mAlphabetExitClick(Sender: TObject);
    procedure mOptionsLanguageEngClick(Sender: TObject);
    procedure mOptionsLanguageGerClick(Sender: TObject);
    procedure mOptionsUpperClick(Sender: TObject);
    procedure mOptionsLowerClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btCheckClick(Sender: TObject);
    procedure sh10MouseDown(Sender: TObject);
    procedure sh11MouseDown(Sender: TObject);
    procedure sh12MouseDown(Sender: TObject);
    procedure sh13MouseDown(Sender: TObject);
    procedure sh14MouseDown(Sender: TObject);
    procedure sh15MouseDown(Sender: TObject);
    procedure sh16MouseDown(Sender: TObject);
    procedure sh17MouseDown(Sender: TObject);
    procedure sh18MouseDown(Sender: TObject);
    procedure sh19MouseDown(Sender: TObject);
    procedure sh1MouseDown(Sender: TObject);
    procedure sh20MouseDown(Sender: TObject);
    procedure sh21MouseDown(Sender: TObject);
    procedure sh22MouseDown(Sender: TObject);
    procedure sh23MouseDown(Sender: TObject);
    procedure sh24MouseDown(Sender: TObject);
    procedure sh25MouseDown(Sender: TObject);
    procedure sh2MouseDown(Sender: TObject);
    procedure sh3MouseDown(Sender: TObject);
    procedure sh4MouseDown(Sender: TObject);
    procedure sh5MouseDown(Sender: TObject);
    procedure sh6MouseDown(Sender: TObject);
    procedure sh7MouseDown(Sender: TObject);
    procedure sh8MouseDown(Sender: TObject);
    procedure sh9MouseDown(Sender: TObject);
    procedure st10Click(Sender: TObject);
    procedure st11Click(Sender: TObject);
    procedure st12Click(Sender: TObject);
    procedure st13Click(Sender: TObject);
    procedure st14Click(Sender: TObject);
    procedure st15Click(Sender: TObject);
    procedure st16Click(Sender: TObject);
    procedure st17Click(Sender: TObject);
    procedure st18Click(Sender: TObject);
    procedure st19Click(Sender: TObject);
    procedure st1Click(Sender: TObject);
    procedure st20Click(Sender: TObject);
    procedure st21Click(Sender: TObject);
    procedure st22Click(Sender: TObject);
    procedure st23Click(Sender: TObject);
    procedure st24Click(Sender: TObject);
    procedure st25Click(Sender: TObject);
    procedure st2Click(Sender: TObject);
    procedure st3Click(Sender: TObject);
    procedure st4Click(Sender: TObject);
    procedure st5Click(Sender: TObject);
    procedure st6Click(Sender: TObject);
    procedure st7Click(Sender: TObject);
    procedure st8Click(Sender: TObject);
    procedure st9Click(Sender: TObject);
  private
    iTest, iQuestion, iCorrect: Integer;
    sLanguage: string;
    cLetter: TUTF8Char;
    aLetters: TLetters;
    aWords: TWordList;
    aGrid: TGrid;
    aShapes: TShapes;
    aLabels: TLabels;
  end;

const
  GermanLetters: array[0..29] of TUTF8Char = (
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
  'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'ä', 'ö', 'ü', 'ß'
  );
  clGold = $00D7FF;

var
  fAlphabet: TfAlphabet;

implementation

{$R *.lfm}

{ Read wordlist from text file }

procedure ReadWords(Language: string; out Words: TWordList);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Words, 0);
  Assign(InFile, '.\' + Language + '\words.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      SetLength(Words, N);
      Words[N - 1] := UTF8Trim(Line);
    end;
  end;
  Close(InFile);
end;

{ Get a random letter }

function RandomLetter(Letters: TLetters; Ch0: TUTF8Char): TUTF8Char;

var
  C: Integer;
  Ch: TUTF8Char;
  OK: Boolean;

begin
  // Generate a letter different from the one gives as argument (this will keep the "to find" letters between 1/6 and 4/6 occurences; cf. below)
  repeat
    OK := True;
    Ch := Letters[Random(Length(Letters))];
    if not fAlphabet.mOptionsLower.Checked and (Ch = 'ß') then
      OK := False                                                              // there is no uppercase ß
    else begin
      // Upper- or lowercase letter depends on options selected (and is random if both are to be used)
      if fAlphabet.mOptionsUpper.Checked and fAlphabet.mOptionsLower.Checked then
        C := Random(2)
      else if fAlphabet.mOptionsUpper.Checked then
        C := 0
      else
        C := 1;
      if C = 0 then begin
        if Ch <> 'ß' then
          Ch := UTF8UpperCase(Ch);                                             // can't transform ß to uppercase
      end;
      if not ((Ch0 = '-') or ((Ch <> Ch0) and (Ch <> UTF8UpperCase(Ch0)) and (Ch <> UTF8LowerCase(Ch0)))) then
        OK := False;
    end;
  until OK;
  Result := Ch;
end;

{ Fill the grid with letters or words and reset shapes' color to yellow }

procedure FillGrid(var Grid: TGrid; var Shapes: TShapes; var Labels: TLabels);

var
  I, J: Integer;

begin
  for I := 1 to 5 do begin
    for J := 1 to 5 do begin
      Shapes[I, J].Brush.Color := clYellow;
      Labels[I, J].Caption := Grid[I, J];
    end;
  end;
end;

{ Create a new grid filled with letters }

procedure NewLetterGrid(var Letters: TLetters; out Letter: TUTF8Char; out Grid: TGrid);

var
  N, C, I, J, K: Integer;

begin
  for I := 1 to 5 do begin
    for J := 1 to 5 do
      Grid[I, J] := '';
  end;
  // Choose a random letter as the one to be found by user
  Letter := RandomLetter(Letters, '-');
  // Random number (betwwen 1/6 and 4/6) of this letter occurences in the grid
  if fAlphabet.mOptionsUpper.Checked and fAlphabet.mOptionsLower.Checked then
    N := Random(6) + 1
  else
    N := Random(4) + 1;
  C := 0;
  // Fill in the "to find" letter at random grid positions
  for K := 1 to N do begin
    repeat
      I := Random(5) + 1; J := Random(5) + 1;
    until Grid[I, J] = '';
    Grid[I, J] := Letter;
    if fAlphabet.mOptionsUpper.Checked and fAlphabet.mOptionsLower.Checked then begin
      // If both cases are to be used, be sure both cases are present in the grid
      Inc(C);
      if C > N / 2 then begin
        if Letter <> 'ß' then begin                                            // ß always lowercase
          if Letter = UTF8LowerCase(Letter) then
            Grid[I, J] := UTF8UpperCase(Letter)
          else
            Grid[I, J] := UTF8LowerCase(Letter);
        end;
      end;
    end;
  end;
  // Fill the rest of the grid with random letters
  for I := 1 to 5 do begin
    for J := 1 to 5 do begin
      if Grid[I, J] = '' then
        Grid[I, J] := RandomLetter(Letters, Letter);
    end;
  end;
end;

{ Create a new grid filled with words }

procedure NewWordGrid(Lang: string; var Letters: TLetters; var Words: TWordList; out Letter: TUTF8Char; out Grid: TGrid);

var
  N, N0, LCase, P1, P2, I, J, K, KN, L: Integer;
  SWord: string;
  OK: Boolean;
  LetterWords: array of string;
  GridWords: array[1..25] of string;

begin
  for I := 1 to 5 do begin
    for J := 1 to 5 do
      Grid[I, J] := '';
  end;
  for I := 1 to 25 do
    GridWords[I] := '';
  // Choose a random letter (to be found by user)
  Letter := RandomLetter(Letters, '-');
  // Extract all words containing this letter from the wordlist
  SetLength(LetterWords, 0);
  N0 := Length(Words); N := 0;
  for I := 0 to N0 - 1 do begin
    P1 := Pos(UTF8UpperCase(Letter), UTF8UpperCase(Words[I])); P2 := Pos(UTF8LowerCase(Letter), UTF8LowerCase(Words[I]));
    if (P1 > 0) or (P2 > 0) then begin
      if not (not fAlphabet.mOptionsUpper.Checked and (UTF8Copy(Words[I], 1, 1) = UTF8UpperCase(UTF8Copy(Words[I], 1, 1)))) then begin
        // Do not use German nouns (beginning with an uppercase letter), if all letetrs have to be lowercase
        Inc(N);
        SetLength(LetterWords, N);
        LetterWords[N - 1] := Words[I];
      end;
    end;
  end;
  // Use 1 - 5 of the words, containing the "to find" letter and fill them in at random grid positions
  if (Lang = 'German') and (Letter = 'y') and not fAlphabet.mOptionsUpper.Checked then
    // There are only 2 non-noun words with 'y' in the list
    KN := Random(2) + 1
  else
    // General case...
    KN := Random(5) + 1;
  for K := 1 to KN do begin
    repeat
      OK := True;
      repeat
        I := Random(5) + 1; J := Random(5) + 1;
      until Grid[I, J] = '';
      SWord := LetterWords[Random(N)];
      // Be sure to use each word only once
      for L := 1 to K - 1 do begin
        if SWord = GridWords[L] then
          OK := False;
      end;
    until OK;
    GridWords[K] := SWord;
    // Upper- and/or lowercase words
    if fAlphabet.mOptionsUpper.Checked then begin
      if fAlphabet.mOptionsLower.Checked then
        LCase := Random(2)
      else
        LCase := 1;
      if LCase = 1 then begin
        P1 := Pos('ß', SWord);
        if P1 = 0 then
          SWord := UTF8UpperCase(SWord);
      end;
    end;
    Grid[I, J] := SWord;
  end;
  // Fill in the rest of the grid with random words (containing or not the "to find" letter )
  for I := 1 to 5 do begin
    for J := 1 to 5 do begin
      if Grid[I, J] = '' then begin
        Inc(K);
        repeat
          OK := True;
          SWord := Words[Random(N0)];
          if not fAlphabet.mOptionsUpper.Checked and (UTF8Copy(SWord, 1, 1) = UTF8UpperCase(UTF8Copy(SWord, 1, 1))) then
            OK := False
          else begin
            // Be sure to use each word only once
            for L := 1 to K - 1 do begin
              if SWord = GridWords[L] then
                OK := False;
            end;
          end;
        until OK;
        GridWords[K] := SWord;
        // Upper- and/or lowercase words
        if fAlphabet.mOptionsUpper.Checked then begin
          if fAlphabet.mOptionsLower.Checked then
            LCase := Random(2)
          else
            LCase := 1;
          if LCase = 1 then begin
            P1 := Pos('ß', SWord);
            if P1 = 0 then
              SWord := UTF8UpperCase(SWord);
          end;
        end;
        Grid[I, J] := SWord;
      end;
    end;
  end;
end;

{ Get letters and words for actual language }

procedure ResetLanguage(Lang: string; out Letters: TLetters; out Words: TWordList);

var
  I, L: Integer;

begin
  // Re-init the letters array
  if Lang = 'English' then
    L := 26                                                                    // use 26 first letters of "German letetrs" array
  else
    L := 30;                                                                   // use all letters of "German letetrs" array
  SetLength(Letters, L);
  for I := 0 to L - 1 do
    Letters[I] := GermanLetters[I];
  // Re-init word list
  ReadWords(Lang, Words);
end;

{ Reset all for new alphabet test }

procedure NewTest(Lang: string; out Letters: TLetters; out Words: TWordList; var Question, Correct: Integer;
  out Grid: TGrid; var Shapes: TShapes; var Labels: TLabels);

var
  I, J: Integer;

begin
  ResetLanguage(Lang, Letters, Words);
  // Clear the grid
  for I := 1 to 5 do begin
    for J := 1 to 5 do begin
      Grid[I, J] := ' ';
    end;
  end;
  FillGrid(Grid, Shapes, Labels);
  // Reset the evaluation counters
  Question := 0; Correct := 0;
  fAlphabet.edQuestion.Text := '';
  fAlphabet.edCorrect.Text := '';
  fAlphabet.edFalse.Text := '';
  fAlphabet.edSuccess.Text := '';
  fAlphabet.edSuccess.Color := clDefault;
  // Reset the buttons
  fAlphabet.btStart.Caption := 'Start';
  fAlphabet.btCheck.Enabled := False;
end;

{ Action to be taken when user clicks a field in the grid: Select or unselect this field }

procedure FieldSelect(N: Integer; var Fields: TShapes);

var
  Row, Col: Integer;

begin
  // Action taken only, if test has been started
  if fAlphabet.btStart.Caption = 'Replay' then begin
    // Calculate row/column values from sequential 1-25 index
    Row := (N - 1) div 5 + 1; Col := N - 5 * (Row - 1);
    // Toggle field select/unselect
    if Fields[Row, Col].Brush.Color = clYellow then
      Fields[Row, Col].Brush.Color := clGold                                   // a selected field will have a golden color
    else
      Fields[Row, Col].Brush.Color := clYellow;                                // an unselected field will have a standard (yellow) color
  end;
end;

{************}
{ TfAlphabet }
{************}

{ Application start: Initialisation }

procedure TfAlphabet.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  Randomize;
  // Create array with the grid fields (shapes)
  aShapes[1, 1] := sh1;  aShapes[1, 2] := sh2;  aShapes[1, 3] := sh3;  aShapes[1, 4] := sh4;  aShapes[1, 5] := sh5;
  aShapes[2, 1] := sh6;  aShapes[2, 2] := sh7;  aShapes[2, 3] := sh8;  aShapes[2, 4] := sh9;  aShapes[2, 5] := sh10;
  aShapes[3, 1] := sh11; aShapes[3, 2] := sh12; aShapes[3, 3] := sh13; aShapes[3, 4] := sh14; aShapes[3, 5] := sh15;
  aShapes[4, 1] := sh16; aShapes[4, 2] := sh17; aShapes[4, 3] := sh18; aShapes[4, 4] := sh19; aShapes[4, 5] := sh20;
  aShapes[5, 1] := sh21; aShapes[5, 2] := sh22; aShapes[5, 3] := sh23; aShapes[5, 4] := sh24; aShapes[5, 5] := sh25;
  aLabels[1, 1] := st1;  aLabels[1, 2] := st2;  aLabels[1, 3] := st3;  aLabels[1, 4] := st4;  aLabels[1, 5] := st5;
  aLabels[2, 1] := st6;  aLabels[2, 2] := st7;  aLabels[2, 3] := st8;  aLabels[2, 4] := st9;  aLabels[2, 5] := st10;
  aLabels[3, 1] := st11; aLabels[3, 2] := st12; aLabels[3, 3] := st13; aLabels[3, 4] := st14; aLabels[3, 5] := st15;
  aLabels[4, 1] := st16; aLabels[4, 2] := st17; aLabels[4, 3] := st18; aLabels[4, 4] := st19; aLabels[4, 5] := st20;
  aLabels[5, 1] := st21; aLabels[5, 2] := st22; aLabels[5, 3] := st23; aLabels[5, 4] := st24; aLabels[5, 5] := st25;
  // Start up with English letter test
  sLanguage := 'English';
  SetLength(aLetters, 26);
  for I := 0 to 25 do
    aLetters[I] := GermanLetters[I];
  iTest := 1;
  NewTest(sLanguage, aLetters, aWords, iQuestion, iCorrect, aGrid, aShapes, aLabels);
end;

{ Menu item "Alphabet > Letter grid": Prepare new letter grid test }

procedure TfAlphabet.mAlphabetGLettersClick(Sender: TObject);

begin
  iTest := 1;
  NewTest(sLanguage, aLetters, aWords, iQuestion, iCorrect, aGrid, aShapes, aLabels);
end;

{ Menu item "Alphabet > Word grid": Prepare new word grid test }

procedure TfAlphabet.mAlphabetWLettersClick(Sender: TObject);

begin
  iTest := 2;
  NewTest(sLanguage, aLetters, aWords, iQuestion, iCorrect, aGrid, aShapes, aLabels);
end;

{ Menu item "Alphabet > Exit": Exit application }

procedure TfAlphabet.mAlphabetExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options" > Alphabet language > ...": Toggle alphabet language }

procedure TfAlphabet.mOptionsLanguageEngClick(Sender: TObject);

begin
  if not mOptionsLanguageEng.Checked then begin
    mOptionsLanguageEng.Checked := True;
    mOptionsLanguageGer.Checked := False;
    sLanguage := 'English';
    ResetLanguage(sLanguage, aLetters, aWords);
  end;
end;

procedure TfAlphabet.mOptionsLanguageGerClick(Sender: TObject);

begin
  if not mOptionsLanguageGer.Checked then begin
    mOptionsLanguageEng.Checked := False;
    mOptionsLanguageGer.Checked := True;
    sLanguage := 'German';
    ResetLanguage(sLanguage, aLetters, aWords);
  end;
end;

{ Menu item "Options > Uppercase letters": Toggle usage of uppercase letters }

procedure TfAlphabet.mOptionsUpperClick(Sender: TObject);

begin
  if mOptionsUpper.Checked then
    mOptionsUpper.Checked := False
  else
    mOptionsUpper.Checked := True;
end;

{ Menu item "Options > Lowercase letters": Toggle usage of lowercase letters }

procedure TfAlphabet.mOptionsLowerClick(Sender: TObject);

begin
  if mOptionsLower.Checked then
    mOptionsLower.Checked := False
  else
    mOptionsLower.Checked := True;
end;

{ Menu item "Help > About": Display program about }

procedure TfAlphabet.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Educational program for children:' + LineEnding;
  S += 'Learn the ALPHABET (English and German letetrs).' + LineEnding + LineEnding;
  S += 'Version 1.2, © allu, June 2019 - January 2020.';
  MessageDlg('About "Alphabet"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Play/Replay": Generate new grid / Play the letter to find }

procedure TfAlphabet.btStartClick(Sender: TObject);

var
  Filename: string;
  LetterFile: string;

begin
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Play') then begin
    // Button "Play" ("Start", if new test is started): Generate new letter or word grid
    if not mOptionsUpper.Checked and not mOptionsLower.Checked then begin
      // If no letter case checked, use upper- + lowercase by default
      mOptionsUpper.Checked := True; mOptionsLower.Checked := True;
    end;
    // Generate new grid
    if iTest = 1 then
      NewLetterGrid(aLetters, cLetter, aGrid)                                  // generate new letter grid
    else
      NewWordGrid(sLanguage, aLetters, aWords, cLetter, aGrid);                // generate new word grid
    // Fill in the letters resp. words
    FillGrid(aGrid, aShapes, aLabels);
    // Update the buttons
    btStart.Caption := 'Replay'; btCheck.Enabled := True;
  end;
  // Button "Play" ("Start") or "Replay": Play letter audio file (using Windows-API)
  Filename := UTF8UpperCase(cLetter);
  Filename := StringReplace(Filename, 'Ä', 'AE', []);
  Filename := StringReplace(Filename, 'Ö', 'OE', []);
  Filename := StringReplace(Filename, 'Ü', 'UE', []);
  Filename := StringReplace(Filename, 'ß', 'SZ', []);
  LetterFile := '.\' + sLanguage + '\' + Filename + '.wav';
  sndPlaySound(pchar(LetterFile), snd_Async or snd_NoDefault);
end;

{ Button "Check": Check user answer; update evaluation counters }

procedure TfAlphabet.btCheckClick(Sender: TObject);

var
  Percent, I, J, P: Integer;
  CorrectField, UserCorrectField, UserMissedField, Correct: Boolean;

begin
  Correct := True;
  for I := 1 to 5 do begin
    for J := 1 to 5 do begin
      CorrectField := False; UserCorrectField := False; UserMissedField := False;
      if iTest = 1 then begin
        // Check user answer for letter grid test
        if (aGrid[I, J] = UTF8UpperCase(cLetter)) or (aGrid[I, J] = UTF8LowerCase(cLetter))then
          CorrectField := True;
      end
      else begin
        // Check user answer for word grid test
        P := Pos(UTF8UpperCase(cLetter), aGrid[I, J]);
        if P > 0 then
          CorrectField := True
        else begin
          P := Pos(UTF8LowerCase(cLetter), aGrid[I, J]);
          if P > 0 then
            CorrectField := True;
        end;
      end;
      if CorrectField and (aShapes[I, J].Brush.Color = clGold) then
        // User selected a field that does contain the letter
        UserCorrectField := True
      else if not CorrectField and (aShapes[I, J].Brush.Color = clYellow) then
        // User didn't select a field that doesn't contain the letter
        UserCorrectField := True
      else if CorrectField and (aShapes[I, J].Brush.Color = clYellow) then
        // User didn't select a field that does contain the letter
        UserMissedField := True;
      if UserCorrectField then begin
        // Correct field selected : shown by lime color
        if aShapes[I, J].Brush.Color = clGold then
          aShapes[I, J].Brush.Color := clLime;
      end
      else begin
        if UserMissedField then
          // Field missed: shown by fuchsia color
          aShapes[I, J].Brush.Color := clFuchsia
        else
          // False field selected : shown by red color
          aShapes[I, J].Brush.Color := clRed;
        Correct := False;
      end;
    end;
  end;
  // Update evaluation counters
  Inc(iQuestion);
  if Correct then
    Inc(iCorrect);
  edQuestion.Text := IntToStr(iQuestion);
  edCorrect.Text := IntToStr(iCorrect);
  edFalse.Text := IntToStr(iQuestion - iCorrect);
  Percent := Round(100 * (iCorrect / iQuestion));
  edSuccess.Text := IntToStr(Percent) + '%';
  if Percent < 50 then
    edSuccess.Color := clRed
  else if Percent < 60 then
    edSuccess.Color := clYellow
  else
    edSuccess.Color := clLime;
  // Update the buttons
  btStart.Caption := 'Play'; btCheck.Enabled := False;
end;

{ User field selection (mousedown event on field shapes) }

procedure TfAlphabet.sh1MouseDown(Sender: TObject);

begin
  FieldSelect(1, aShapes);
end;

procedure TfAlphabet.sh2MouseDown(Sender: TObject);

begin
  FieldSelect(2, aShapes);
end;

procedure TfAlphabet.sh3MouseDown(Sender: TObject);

begin
  FieldSelect(3, aShapes);
end;

procedure TfAlphabet.sh4MouseDown(Sender: TObject);

begin
  FieldSelect(4, aShapes);
end;

procedure TfAlphabet.sh5MouseDown(Sender: TObject);

begin
  FieldSelect(5, aShapes);
end;

procedure TfAlphabet.sh6MouseDown(Sender: TObject);

begin
  FieldSelect(6, aShapes);
end;

procedure TfAlphabet.sh7MouseDown(Sender: TObject);

begin
  FieldSelect(7, aShapes);
end;

procedure TfAlphabet.sh8MouseDown(Sender: TObject);

begin
  FieldSelect(8, aShapes);
end;

procedure TfAlphabet.sh9MouseDown(Sender: TObject);

begin
  FieldSelect(9, aShapes);
end;

procedure TfAlphabet.sh10MouseDown(Sender: TObject);

begin
  FieldSelect(10, aShapes);
end;

procedure TfAlphabet.sh11MouseDown(Sender: TObject);

begin
  FieldSelect(11, aShapes);
end;

procedure TfAlphabet.sh12MouseDown(Sender: TObject);

begin
  FieldSelect(12, aShapes);
end;

procedure TfAlphabet.sh13MouseDown(Sender: TObject);

begin
  FieldSelect(13, aShapes);
end;

procedure TfAlphabet.sh14MouseDown(Sender: TObject);

begin
  FieldSelect(14, aShapes);
end;

procedure TfAlphabet.sh15MouseDown(Sender: TObject);

begin
  FieldSelect(15, aShapes);
end;

procedure TfAlphabet.sh16MouseDown(Sender: TObject);

begin
  FieldSelect(16, aShapes);
end;

procedure TfAlphabet.sh17MouseDown(Sender: TObject);

begin
  FieldSelect(17, aShapes);
end;

procedure TfAlphabet.sh18MouseDown(Sender: TObject);

begin
  FieldSelect(18, aShapes);
end;

procedure TfAlphabet.sh19MouseDown(Sender: TObject);

begin
  FieldSelect(19, aShapes);
end;

procedure TfAlphabet.sh20MouseDown(Sender: TObject);

begin
  FieldSelect(20, aShapes);
end;

procedure TfAlphabet.sh21MouseDown(Sender: TObject);

begin
  FieldSelect(21, aShapes);
end;

procedure TfAlphabet.sh22MouseDown(Sender: TObject);

begin
  FieldSelect(22, aShapes);
end;

procedure TfAlphabet.sh23MouseDown(Sender: TObject);

begin
  FieldSelect(23, aShapes);
end;

procedure TfAlphabet.sh24MouseDown(Sender: TObject);

begin
  FieldSelect(24, aShapes);
end;

procedure TfAlphabet.sh25MouseDown(Sender: TObject);

begin
  FieldSelect(25, aShapes);
end;

{ User field selection (click event on letter/word statictext) }

procedure TfAlphabet.st1Click(Sender: TObject);

begin
  FieldSelect(1, aShapes);
end;

procedure TfAlphabet.st2Click(Sender: TObject);

begin
  FieldSelect(2, aShapes);
end;

procedure TfAlphabet.st3Click(Sender: TObject);

begin
  FieldSelect(3, aShapes);
end;

procedure TfAlphabet.st4Click(Sender: TObject);

begin
  FieldSelect(4, aShapes);
end;

procedure TfAlphabet.st5Click(Sender: TObject);

begin
  FieldSelect(5, aShapes);
end;

procedure TfAlphabet.st6Click(Sender: TObject);

begin
  FieldSelect(6, aShapes);
end;

procedure TfAlphabet.st7Click(Sender: TObject);

begin
  FieldSelect(7, aShapes);
end;

procedure TfAlphabet.st8Click(Sender: TObject);

begin
  FieldSelect(8, aShapes);
end;

procedure TfAlphabet.st9Click(Sender: TObject);

begin
  FieldSelect(9, aShapes);
end;

procedure TfAlphabet.st10Click(Sender: TObject);

begin
  FieldSelect(10, aShapes);
end;

procedure TfAlphabet.st11Click(Sender: TObject);

begin
  FieldSelect(11, aShapes);
end;

procedure TfAlphabet.st12Click(Sender: TObject);

begin
  FieldSelect(12, aShapes);
end;

procedure TfAlphabet.st13Click(Sender: TObject);

begin
  FieldSelect(13, aShapes);
end;

procedure TfAlphabet.st14Click(Sender: TObject);

begin
  FieldSelect(14, aShapes);
end;

procedure TfAlphabet.st15Click(Sender: TObject);

begin
  FieldSelect(15, aShapes);
end;

procedure TfAlphabet.st16Click(Sender: TObject);

begin
  FieldSelect(16, aShapes);
end;

procedure TfAlphabet.st17Click(Sender: TObject);

begin
  FieldSelect(17, aShapes);
end;

procedure TfAlphabet.st18Click(Sender: TObject);

begin
  FieldSelect(18, aShapes);
end;

procedure TfAlphabet.st19Click(Sender: TObject);

begin
  FieldSelect(19, aShapes);
end;

procedure TfAlphabet.st20Click(Sender: TObject);

begin
  FieldSelect(20, aShapes);
end;

procedure TfAlphabet.st21Click(Sender: TObject);

begin
  FieldSelect(21, aShapes);
end;

procedure TfAlphabet.st22Click(Sender: TObject);

begin
  FieldSelect(22, aShapes);
end;

procedure TfAlphabet.st23Click(Sender: TObject);

begin
  FieldSelect(23, aShapes);
end;

procedure TfAlphabet.st24Click(Sender: TObject);

begin
  FieldSelect(24, aShapes);
end;

procedure TfAlphabet.st25Click(Sender: TObject);

begin
  FieldSelect(25, aShapes);
end;

end.

