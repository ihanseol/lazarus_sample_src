{*************************************}
{* Main unit for WGuess3 application *}
{*************************************}

unit wguess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, LazUTF8, LCLType, help;

type
  TSubject = record
    Name: string;
    WordFirst, WordNumber: Integer;
  end;
  TSubjects = array[0..15] of TSubject;
  TWords = array of string;
  {***********}
  { TfWGuess3 }
  {***********}
  TfWGuess3 = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions, mOptionsPlayers, mOptionsPlayers1, mOptionsPlayers2: TMenuItem;
    mOptionsWords, mOptionsGuesses, mOptionsLettersEntered: TMenuItem;
    mOptionsSubject, mOptionsSubjectFixed, mOptionsSubjectUser, mOptionsSubjectRandom: TMenuItem;
    mOptionsNotFound, mOptionsNotFound0, mOptionsNotFound2, mOptionsNotFound5, mOptionsNotFound10: TMenuItem;
    mHelp, mHelpAbout, mHelpHelp: TMenuItem;
    StaticText1: TStaticText;
    shSubject0, shSubject1, shSubject2, shSubject3, shSubject4, shSubject5, shSubject6, shSubject7: TShape;
    shSubject8, shSubject9, shSubject10, shSubject11, shSubject12, shSubject13, shSubject14, shSubject15: TShape;
    stSubject0, stSubject1, stSubject2, stSubject3, stSubject4, stSubject5, stSubject6, stSubject7: TStaticText;
    stSubject8, stSubject9, stSubject10, stSubject11, stSubject12, stSubject13, stSubject14, stSubject15: TStaticText;
    edPlayer1, edPlayer2, edScore1, edScore2: TEdit;
    shPlayer10, shPlayer1, shPlayer20, shPlayer2: TShape;
    laWord, laLettersEntered: TLabel;
    edWord, edWord2, edLettersEntered: TEdit;
    btStart: TButton;
    btSolve: TButton;
    tiSelect: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsPlayers1Click(Sender: TObject);
    procedure mOptionsPlayers2Click(Sender: TObject);
    procedure mOptionsWordsClick(Sender: TObject);
    procedure mOptionsGuessesClick(Sender: TObject);
    procedure mOptionsNotFound0Click(Sender: TObject);
    procedure mOptionsNotFound2Click(Sender: TObject);
    procedure mOptionsNotFound5Click(Sender: TObject);
    procedure mOptionsNotFound10Click(Sender: TObject);
    procedure mOptionsSubjectFixedClick(Sender: TObject);
    procedure mOptionsSubjectUserClick(Sender: TObject);
    procedure mOptionsSubjectRandomClick(Sender: TObject);
    procedure mOptionsLettersEnteredClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btSolveClick(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure shSubject0MouseDown(Sender: TObject);
    procedure shSubject1MouseDown(Sender: TObject);
    procedure shSubject2MouseDown(Sender: TObject);
    procedure shSubject3MouseDown(Sender: TObject);
    procedure shSubject4MouseDown(Sender: TObject);
    procedure shSubject5MouseDown(Sender: TObject);
    procedure shSubject6MouseDown(Sender: TObject);
    procedure shSubject7MouseDown(Sender: TObject);
    procedure shSubject8MouseDown(Sender: TObject);
    procedure shSubject9MouseDown(Sender: TObject);
    procedure shSubject10MouseDown(Sender: TObject);
    procedure shSubject11MouseDown(Sender: TObject);
    procedure shSubject12MouseDown(Sender: TObject);
    procedure shSubject13MouseDown(Sender: TObject);
    procedure shSubject14MouseDown(Sender: TObject);
    procedure shSubject15MouseDown(Sender: TObject);
    procedure stSubject0Click(Sender: TObject);
    procedure stSubject1Click(Sender: TObject);
    procedure stSubject2Click(Sender: TObject);
    procedure stSubject3Click(Sender: TObject);
    procedure stSubject4Click(Sender: TObject);
    procedure stSubject5Click(Sender: TObject);
    procedure stSubject6Click(Sender: TObject);
    procedure stSubject7Click(Sender: TObject);
    procedure stSubject8Click(Sender: TObject);
    procedure stSubject9Click(Sender: TObject);
    procedure stSubject10Click(Sender: TObject);
    procedure stSubject11Click(Sender: TObject);
    procedure stSubject12Click(Sender: TObject);
    procedure stSubject13Click(Sender: TObject);
    procedure stSubject14Click(Sender: TObject);
    procedure stSubject15Click(Sender: TObject);
    procedure edPlayer1EditingDone(Sender: TObject);
    procedure edPlayer2EditingDone(Sender: TObject);
    procedure tiSelectTimer(Sender: TObject);
  private
    iPlayers, iPlayers0, iWords, iWords0, iGuessMax, iGuessMax0, iNotFound0, iNotFound, iPlayer, iRandomCount, iRandomSel1, iRandomSel2: Integer;
    iSubject, iOldSubject, iWord, iCorrect, iFalseAttempts, iScore1, iScore2: Integer;
    sSelect, sSelect0, sWord, sWordDisplay: string;
    bSelDone, bKeybEvent: Boolean;
    aSubjects: TSubjects;
    aWords: TWords;
    aWordsDone: array of Boolean;
    shSubjects: array[0..15] of TShape;
    stSubjects: array[0..15] of TStaticText;
  end;

var
  fWGuess3: TfWGuess3;

implementation

{$R *.lfm}

{ Custom uppercase function }

function WG3UpperCase(S: string): string;

// UTF-8 uppercase function, conserving the hyphen as hyphen

begin
  S := UTF8UpperCase(S);
  S := StringReplace(S, '_', '-', [rfReplaceAll]);
  Result := S;
end;

{ Read wordlists from textfiles }

procedure ReadWords(var Labels: array of TStaticText; out Subjects: TSubjects; out Words: TWords);

// 15 different files, one for each subject

var
  F, W, I: Integer;
  Filename, Line: string;
  InFile: Text;

begin
  F := 0;
  for I := 0 to 15 do begin
    Subjects[I].Name := Labels[I].Caption;
    Subjects[I].WordFirst := F;                                                // index of first word of this subject
    Filename := './wordlists/' + UTF8LowerCase(Subjects[I].Name) + '.txt'; DoDirSeparators(Filename);
    Assign(InFile, Filename); Reset(InFile);
    W := 0;                                                                    // subject word counter
    while not EoF(InFile) do begin
      Readln(InFile, Line); Line := UTF8Trim(Line);
      if Line <> '' then begin
        Inc(W); SetLength(Words, F + W);
        Words[F + W - 1] := WG3UpperCase(Line);
      end;
    end;
    Subjects[I].WordNumber := W;                                               // total number of words for this subject
    F += W;                                                                    // index of first word of next subject
    Close(InFile);
  end;
end;

{ Subject selection (user click on subject shape or static text) }

procedure SubjectSelect(Select: string; var SelDone: Boolean; Subject: Integer; var OldSubject: Integer; var Subjects: array of TShape);

begin
  if not SelDone then begin
    // Only action if subject has not yet been selected
    if Select <> 'random' then begin
      // No action if random subject selection is active
      if OldSubject <> -1 then
        Subjects[OldSubject].Brush.Color := clYellow;                          // reset color of subject selected before
      Subjects[Subject].Brush.Color := clLime;                                 // highlight selected subject
      OldSubject := Subject;
      SelDone := True;                                                         // subject selection is now done
      fWGuess3.btStart.Click;                                                  // automatically continue the game
    end;
  end;
end;

{ Game continuation }

procedure NextStep(SubjSelect: string; Wrd, Wrds, Players, Score1, Score2, OldSubject: Integer;
  var RandomCount: Integer; var SelDone: Boolean; var Subjects: array of TShape);

begin
  if (Wrd = Wrds) or (Score1 = 0) or ((Players = 2) and (Score2 = 0)) then begin
    // Game over conditions
    if Wrd = Wrds then begin
      // All words have been done
      MessageDlg('WGuess3', 'All words have been done. Game over.', mtInformation, [mbOK], 0);
    end
    else begin
      // One of the players' score has reached 0
      if Players = 1 then
        MessageDlg('WGuess3', 'Your score has reached 0. Game over.', mtInformation, [mbOK], 0)
      else if Score1 = 0 then
        MessageDlg('WGuess3', fWGuess3.edPlayer1.Text + '''s score reached 0. Game over.', mtInformation, [mbOK], 0)
      else
        MessageDlg('WGuess3', fWGuess3.edPlayer2.Text + '''s score reached 0. Game over.', mtInformation, [mbOK], 0)
    end;
    fWGuess3.btStart.Enabled := False; fWGuess3.btSolve.Enabled := False;
  end
  else begin
    // Game continuation
    if SubjSelect <> 'fix' then begin
      if SubjSelect = 'user' then
        Subjects[OldSubject].Brush.Color := clYellow                           // reset subject selected before
      else
        RandomCount := 0;                                                      // reset counter for random selection "shape flickering"
      SelDone := False;                                                        // a new subject has to be selected (except for fix subject)
    end;
    fWGuess3.btStart.Enabled := True; fWGuess3.btSolve.Enabled := False;
    fWGuess3.btStart.SetFocus;
  end;
end;

{***********}
{ TfWGuess3 }
{***********}

{ Application start: Initialisation }

procedure TfWGuess3.FormCreate(Sender: TObject);

begin
  // Create arrays with subject shapes and static texts
  shSubjects[0]  := shSubject0;  shSubjects[1]  := shSubject1;  shSubjects[2]  := shSubject2;  shSubjects[3]  := shSubject3;
  shSubjects[4]  := shSubject4;  shSubjects[5]  := shSubject5;  shSubjects[6]  := shSubject6;  shSubjects[7]  := shSubject7;
  shSubjects[8]  := shSubject8;  shSubjects[9]  := shSubject9;  shSubjects[10] := shSubject10; shSubjects[11] := shSubject11;
  shSubjects[12] := shSubject12; shSubjects[13] := shSubject13; shSubjects[14] := shSubject14; shSubjects[15] := shSubject15;
  stSubjects[0]  := stSubject0;  stSubjects[1]  := stSubject1;  stSubjects[2]  := stSubject2;  stSubjects[3]  := stSubject3;
  stSubjects[4]  := stSubject4;  stSubjects[5]  := stSubject5;  stSubjects[6]  := stSubject6;  stSubjects[7]  := stSubject7;
  stSubjects[8]  := stSubject8;  stSubjects[9]  := stSubject9;  stSubjects[10] := stSubject10; stSubjects[11] := stSubject11;
  stSubjects[12] := stSubject12; stSubjects[13] := stSubject13; stSubjects[14] := stSubject14; stSubjects[15] := stSubject15;
  // Read word lists from text files
  ReadWords(stSubjects, aSubjects, aWords);
  SetLength(aWordsDone, Length(aWords));
  // Application start-up values
  iPlayers0 := 1; iWords0 := 10; iGuessMax0 := 10; iNotFound0 := 2; sSelect0 := 'random';
  // Start random number generator
  Randomize;
  // Start a new game (with default options)
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new game }

procedure TfWGuess3.mGameNewClick(Sender: TObject);

var
  I: Integer;

begin
  tiSelect.Enabled := False;  bKeybEvent := False;                             // disable timer and keystroke caption
  iPlayers := iPlayers0; iWords := iWords0; iGuessMax := iGuessMax0; sSelect := sSelect0; iNotFound := iNotFound0;
  iWord := 0; iCorrect := 0; iSubject := -1; iPlayer := 0; iRandomCount := -1; iOldSubject := -1;
  iScore1 := Round(iGuessMax * iWords); iScore2 := iScore1;                    // initial score = number of words * max. false attempts
  edScore1.Text := IntToStr(iScore1);
  edPlayer2.Visible := False; edScore2.Visible := False;
  if iPlayers = 2 then begin
    // Show player 2 controls in 2-player mode
    edPlayer2.Visible := True; edScore2.Visible := True;
    edScore2.Text := IntToStr(iScore2);
    iWords *= 2;                                                               // word count value entered by user is "per player"
  end;
  shPlayer10.Visible := False; shPlayer1.Visible := False;
  shPlayer20.Visible := False; shPlayer2.Visible := False;
  laWord.Caption := 'Word';
  edWord.Text := ''; edWord2.Text := ''; edLettersEntered.Text := '';
  laLettersEntered.Visible := False; edLettersEntered.Visible := False;
  if mOptionsLettersEntered.Checked then begin
    // Show letters entered controls if this option is active
    laLettersEntered.Visible := True; edLettersEntered.Visible := True;
  end;
  // Reset all words to "not done", all shapes to default color
  for I := 0 to Length(aWordsDone) - 1 do
    aWordsDone[I] := False;
  for I := 0 to 15 do
    shSubjects[I].Brush.Color := clYellow;
  // Set application in "wait for subject selection" state
  bSelDone := False;
  if sSelect = 'fix' then
    MessageDlg('WGuess3', 'Please, select a subject by clicking on the corr. shape.', mtInformation, [mbOK], 0);
  btStart.Caption := 'Start'; btStart.Enabled := True; btSolve.Enabled := False;
end;

{ Menu item "Game > Exit": Exit application }

procedure TfWGuess3.mGameExitClick(Sender: TObject);

begin
  tiSelect.Enabled := False;
  bKeybEvent := False;
  Close;
end;

{ Menu items "Options > Number of players > ...": Select 1- or 2-player mode }

procedure TfWGuess3.mOptionsPlayers1Click(Sender: TObject);

begin
  mOptionsPlayers1.Checked := True; mOptionsPlayers2.Checked := False;
  iPlayers0 := 1;
  mGameNew.Click;
end;

procedure TfWGuess3.mOptionsPlayers2Click(Sender: TObject);

begin
  mOptionsPlayers1.Checked := False; mOptionsPlayers2.Checked := True;
  iPlayers0 := 2;
  mGameNew.Click;
end;

{ Menu item "Options > Number of words ...": User entry of number of words to guess }

procedure TfWGuess3.mOptionsWordsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('WGuess3', 'Number of words to find', IntToStr(iWords));
  if S <> '' then
    iWords0 := StrToInt(S);
  // Number of words arbitrarly fixed between 5 and 40 (value "per player")
  if iWords0 < 5 then
    iWords0 := 5
  else if iWords0 > 40 then
    iWords0 := 40;
end;

{ Menu item "Options > Max. wrong guesses ...": User entry of maximum of wrong guesses before word is considered "not found" }

procedure TfWGuess3.mOptionsGuessesClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('WGuess3', 'Maximum of wrong guesses', IntToStr(iGuessMax));
  if S <> '' then
    iGuessMax0 := StrToInt(S);
  // Number of wrong guesses arbitrarly fixed between 1 and 12
  if iGuessMax0 < 1 then
    iGuessMax0 := 1
  else if iGuessMax0 > 12 then
    iGuessMax0 := 12;
end;

{ Menu items "Options > Not found penality > ...": Select not found penality value }

procedure TfWGuess3.mOptionsNotFound0Click(Sender: TObject);

begin
  mOptionsNotFound0.Checked := True;  mOptionsNotFound2.Checked  := False;
  mOptionsNotFound5.Checked := False; mOptionsNotFound10.Checked := False;
  iNotFound0 := 0;
end;

procedure TfWGuess3.mOptionsNotFound2Click(Sender: TObject);

begin
  mOptionsNotFound0.Checked := False; mOptionsNotFound2.Checked  := True;
  mOptionsNotFound5.Checked := False; mOptionsNotFound10.Checked := False;
  iNotFound0 := 2;
end;

procedure TfWGuess3.mOptionsNotFound5Click(Sender: TObject);

begin
  mOptionsNotFound0.Checked := False; mOptionsNotFound2.Checked  := False;
  mOptionsNotFound5.Checked := True;  mOptionsNotFound10.Checked := False;
  iNotFound0 := 5;
end;

procedure TfWGuess3.mOptionsNotFound10Click(Sender: TObject);

begin
  mOptionsNotFound0.Checked := False; mOptionsNotFound2.Checked  := False;
  mOptionsNotFound5.Checked := False; mOptionsNotFound10.Checked := True;
  iNotFound0 := 10;
end;

{ Menu items "Options > Subject selection > ...": Select subject selection method }

procedure TfWGuess3.mOptionsSubjectFixedClick(Sender: TObject);

begin
  mOptionsSubjectFixed.Checked := True; mOptionsSubjectUser.Checked := False; mOptionsSubjectRandom.Checked := False;
  sSelect0 := 'fix';
end;

procedure TfWGuess3.mOptionsSubjectUserClick(Sender: TObject);

begin
  mOptionsSubjectFixed.Checked := False; mOptionsSubjectUser.Checked := True; mOptionsSubjectRandom.Checked := False;
  sSelect0 := 'user';
end;

procedure TfWGuess3.mOptionsSubjectRandomClick(Sender: TObject);

begin
  mOptionsSubjectFixed.Checked := False; mOptionsSubjectUser.Checked := False; mOptionsSubjectRandom.Checked := True;
  sSelect0 := 'random';
end;

{ Menu item "Options > Show letters entered": Toggle "show letters entered" on/off }

procedure TfWGuess3.mOptionsLettersEnteredClick(Sender: TObject);

begin
  if mOptionsLettersEntered.Checked then
    mOptionsLettersEntered.Checked := False
  else
    mOptionsLettersEntered.Checked := True;
  laLettersEntered.Visible := mOptionsLettersEntered.Checked;
  edLettersEntered.Visible := mOptionsLettersEntered.Checked;
end;

{ Menu item "Help > Help": Display application help }

procedure TfWGuess3.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfWGuess3.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Specialized subjects word guess game.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October-November 2021.';
  MessageDlg('About "WGuess3"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Generate a new word }

procedure TfWGuess3.btStartClick(Sender: TObject);

var
  W, I: Integer;

begin
  bKeybEvent := False;                                                         // no keystroke capture yet
  // If there is a subject selected, generate the new word
  if bSelDone then begin
    // Determine the actual player
    if iPlayer = 0 then
      iPlayer := 1
    else begin
      if iPlayers = 2 then begin
        if iPlayer = 1 then
          iPlayer := 2
        else
          iPlayer := 1;
      end;
    end;
    // Set controls depending on actual player
    edPlayer1.Color := clDefault; edPlayer2.Color := clDefault;
    shPlayer10.Visible := False; shPlayer1.Visible := False;
    shPlayer20.Visible := False; shPlayer2.Visible := False;
    if iPlayer = 1 then begin
      edPlayer1.Color := clAqua;
      shPlayer10.Visible := True; shPlayer1.Visible := True;
      shPlayer10.Width := 20 * iGuessMax; shPlayer1.Width := shPlayer10.Width;
    end
    else begin
      edPlayer2.Color := clAqua;
      shPlayer20.Visible := True; shPlayer2.Visible := True;
      shPlayer20.Width := 20 * iGuessMax; shPlayer2.Width := shPlayer20.Width;
    end;
    Inc(iWord);                                                                // word counter for this game
    iFalseAttempts := 0;                                                       // false attempts counter for this word
    laWord.Caption := 'Word ' + IntToStr(iWord) + '/' + IntToStr(iWords);
    // Get index of subject actually selected
    for I := 0 to 15 do begin
      if shSubjects[I].Brush.Color = clLime then                               // selected subject is highlighted
        iSubject := I;
    end;
    // Get random word belonging to actual subject (and that has not yet been done)
    repeat
      W := Random(aSubjects[iSubject].WordNumber) + aSubjects[iSubject].WordFirst;
    until not aWordsDone[W];
    aWordsDone[W] := True;                                                     // mark this word as "done"
    sWord := aWords[W];
    edWord.Text := ''; edWord2.Text := '';
    edLettersEntered.Text := ''; edWord.ReadOnly := True;
    // Display actual word as asterisks (except for space and hyphen, displayed as such)
    for I := 1 to UTF8Length(sWord) do begin
      if (UTF8Copy(sWord, I, 1) = ' ') or (UTF8Copy(sWord, I, 1) = '-') then
        edWord.Text := edWord.Text + UTF8Copy(sWord, I, 1)
      else
        edWord.Text := edWord.Text + '*'
    end;
    btStart.Enabled := False; btStart.Caption := 'Next'; btSolve.Enabled := True;
    // Now, activate keystroke caption
    bKeybEvent := True;
  end
  // No subject selected: Do so (random selection) or tell user to do it (other selection modes)
  else begin
    if sSelect = 'random' then begin
      // Random subject selection: Start the selection timer
      if iRandomCount < 1 then
        tiSelect.Enabled := True;
    end
    else if sSelect = 'user' then begin
      // User selection: Remember user to select a subject
      MessageDlg('WGuess3', 'Please, select a subject by clicking on the corr. shape.', mtInformation, [mbOK], 0);
    end
    else begin
      // Tell user that they didn't select a subject when they were told to do so
      MessageDlg('WGuess3', 'No subject selected! Please, click on one of the subject shapes.', mtError, [mbOK], 0);
    end;
  end;
end;

{ Button "Solve/Done": Give user possibility to enter the word directly resp. check user's answer }

procedure TfWGuess3.btSolveClick(Sender: TObject);

begin
  bKeybEvent := False;                                                         // disable keyboard capture
  if btSolve.Caption = 'Solve' then begin
    // Button "Solve": Make word edit field writable for direct user input
    edWord.ReadOnly := False;
    btSolve.Caption := 'Done';
    edWord.SetFocus;
  end
  else begin
    // Button "Done": Check if word entered is correct
    edWord.Text := WG3UpperCase(edWord.Text);
    sWordDisplay := edWord.Text;
    if sWordDisplay <> sWord then begin
      // Word entered is false: Decrement actual player's score
      if iPlayer = 1 then begin
        iScore1 := iScore1 + iFalseAttempts - iGuessMax;                       // decrement = max. number of false attempts allowed
        iScore1 -= iNotFound;                                                  // apply "not found" penality
        if iScore1 < 0 then
          iScore1 := 0;
        edScore1.Text := IntToStr(iScore1);
      end
      else begin
        iScore2 := iScore2 + iFalseAttempts - iGuessMax;
        iScore2 -= iNotFound;
        if iScore2 < 0 then
          iScore2 := 0;
        edScore2.Text := IntToStr(iScore2);
      end;
    end;
    // Game continuation
    NextStep(sSelect, iWord, iWords, iPlayers, iScore1, iScore2, iOldSubject, iRandomCount, bSelDone, shSubjects);
    edWord2.Text := sWord;                                                     // display correct word in the 2nd edit field
    btSolve.Caption := 'Solve'; btSolve.Enabled := False;
  end;
end;

{ Keystroke capture: Check if letter is part of the word }

procedure TfWGuess3.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);

var
  S: string;
  I: Integer;
  NewLetter, LetterInWord: Boolean;

begin
  // React on keystroke only if keyboard capture is enabled (!)
  if bKeybEvent then begin
    UTF8Key := WG3UpperCase(UTF8Key);
    S := edLettersEntered.Text;
    // Check if letter pressed hasn't already been entered
    NewLetter := True;
    for I := 1 to UTF8Length(S) do begin
    if UTF8Copy(S, I, 1) = UTF8Key then
      NewLetter := False;
    end;
    // This is a new letter
    if NewLetter then begin
      // Add the letter to those already entered
      S += WG3UpperCase(UTF8Key);
      edLettersEntered.Text := S;
      // Insert the letter in the displayed asterisk word, if it actually is part of it
      I := 1; sWordDisplay := edWord.Text;
      LetterInWord := False;
      repeat
        if UTF8Copy(sWord, I, 1) = UTF8Key then begin
          // Insert the letter
          UTF8Delete(sWordDisplay, I, 1);
          UTF8Insert(UTF8Copy(sWord, I, 1), sWordDisplay, I);
          LetterInWord := True;
        end;
        Inc(I);
      until I > UTF8Length(sWord);
      edWord.Text := sWordDisplay;                                             // asterisk word with new letter inserted
    end
    // This is a letter that has already been entered
    else begin
      // Consider duplicate letter input as false attempt
      LetterInWord := False;
    end;
    // Letter not in word (or duplicate letter): Decrement the actual player's score
    if not LetterInWord then begin
      Inc(iFalseAttempts);
      if iPlayer = 1 then begin
        Dec(iScore1);                                                          // decrement = 1
        if iFalseAttempts = iGuessMax then
          iScore1 -= iNotFound;                                                // apply "not found" penality
        if iScore1 < 0 then
          iScore1 := 0;
        edScore1.Text := IntToStr(iScore1);
        // Reduce the size of the "wrong attempts left" bar
        if iFalseAttempts = iGuessMax then
          shPlayer1.Visible := False
        else
          shPlayer1.Width := (iGuessMax - iFalseAttempts) * 20;
      end
      else begin
        Dec(iScore2);
        if iFalseAttempts = iGuessMax then
          iScore2 -= iNotFound;
        if iScore2 < 0 then
          iScore2 := 0;
        edScore2.Text := IntToStr(iScore2);
        if iFalseAttempts = iGuessMax then
          shPlayer2.Visible := False
        else
          shPlayer2.Width := (iGuessMax - iFalseAttempts) * 20;
      end;
    end;
    if (sWordDisplay = sWord) or (iFalseAttempts = iGuessMax) or (iScore1 = 0) or ((iPlayers = 2) and (iScore2 = 0)) then begin
      // End of guessing for this word or end of game conditions
      bKeybEvent := False;                                                     // disable keystroke capture
      NextStep(sSelect, iWord, iWords, iPlayers, iScore1, iScore2, iOldSubject, iRandomCount, bSelDone, shSubjects);
      edWord2.Text := sWord;
    end;
  end;
end;

{ Timer for random subject selection "shape flickering" }

procedure TfWGuess3.tiSelectTimer(Sender: TObject);

begin
  if iRandomCount = -1 then
    iRandomSel2 := -1;
  Inc(iRandomCount);
  if iRandomCount <= 10 then begin
    // Let shapes "flicker" 10 times
    if iRandomSel2 <> -1 then
      shSubjects[iRandomSel2].Brush.Color := clYellow;
    repeat
      iRandomSel1 := Random(16);
    until iRandomSel1 <> iRandomSel2;
    shSubjects[iRandomSel1].Brush.Color := clLime;
    iRandomSel2 := iRandomSel1;
  end
  else begin
    // End of "shape flickering"
    iRandomCount := -1;
    tiSelect.Enabled := False;                                                 // disable the timer
    bSelDone := True;                                                          // there is now a subject selected
    btStart.Click;                                                             // automatically continue the game
  end;
end;

{ User click on subject shapes: Select corresponding subject }

procedure TfWGuess3.shSubject0MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 0, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject1MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 1, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject2MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 2, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject3MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 3, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject4MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 4, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject5MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 5, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject6MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 6, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject7MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 7, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject8MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 8, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject9MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 9, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject10MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 10, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject11MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 11, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject12MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 12, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject13MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 13, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject14MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 14, iOldSubject, shSubjects);
end;

procedure TfWGuess3.shSubject15MouseDown(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 15, iOldSubject, shSubjects);
end;

{ User click on subject static texts: Select corresponding subject }

procedure TfWGuess3.stSubject0Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 0, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject1Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 1, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject2Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 2, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject3Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 3, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject4Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 4, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject5Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 5, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject6Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 6, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject7Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 7, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject8Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 8, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject9Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 9, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject10Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 10, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject11Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 11, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject12Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 12, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject13Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 13, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject14Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 14, iOldSubject, shSubjects);
end;

procedure TfWGuess3.stSubject15Click(Sender: TObject);

begin
  SubjectSelect(sSelect, bSelDone, 15, iOldSubject, shSubjects);
end;

{ Player names changed: Name correction }

procedure TfWGuess3.edPlayer1EditingDone(Sender: TObject);

begin
  if edPlayer1.Text = '' then
    edPlayer1.Text := 'Player 1';
end;

procedure TfWGuess3.edPlayer2EditingDone(Sender: TObject);

begin
    if (edPlayer2.Text = '') or (edPlayer2.Text = edPlayer1.Text) then
    edPlayer2.Text := 'Player 2';
end;

end.

