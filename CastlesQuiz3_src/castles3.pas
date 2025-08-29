{******************************************}
{* Main unit for CastlesQuiz3 application *}
{******************************************}

unit castles3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LazUTF8;

type
  TCastles  = array of record
    Region, Name: string;
  end;
  TBooleans = array of Boolean;
  {**********}
  { TfCQuiz3 }
  {**********}
  TfCQuiz3 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSlideshow, mFileExit, mSettings, MenuItem1: TMenuItem;
    mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsRegion, mSettingsRegionENG, mSettingsRegionSCO, mSettingsRegionNWEUR1, mSettingsRegionNWEUR2: TMenuItem;
    mSettingsAnswers, mSettingsAnswers5, mSettingsAnswers10, mSettingsAnswers20, mOptionsNoAnswer: TMenuItem;
    mSettingsSlideshow, mSettingsSlideshow1, mSettingsSlideshow2, mSettingsSlideshow3, mSettingsSlideshow4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout, mHelpPhotos: TMenuItem;
    stQuiz: TStaticText;
    edName1, edName2, edScore1, edScore2: TEdit;
    imCastle: TImage;
    rbAnswer1, rbAnswer4, rbAnswer2, rbAnswer5, rbAnswer3, rbAnswer6: TRadioButton;
    cobAnswers: TComboBox;
    edSlide, edAnswer, edAnswer2: TEdit;
    stSlide: TStaticText;
    btStart: TButton;
    btPause: TButton;
    tiRQ: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSlideshowClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsRegionSCOClick(Sender: TObject);
    procedure mSettingsRegionENGClick(Sender: TObject);
    procedure mSettingsRegionNWEUR1Click(Sender: TObject);
    procedure mSettingsRegionNWEUR2Click(Sender: TObject);
    procedure mSettingsAnswers5Click(Sender: TObject);
    procedure mSettingsAnswers10Click(Sender: TObject);
    procedure mSettingsAnswers20Click(Sender: TObject);
    procedure mOptionsNoAnswerClick(Sender: TObject);
    procedure mSettingsSlideshow1Click(Sender: TObject);
    procedure mSettingsSlideshow2Click(Sender: TObject);
    procedure mSettingsSlideshow3Click(Sender: TObject);
    procedure mSettingsSlideshow4Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpPhotosClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tiVQTimer(Sender: TObject);
  private
    iPlayers, iAnswers, iAnswersTemp, iQuestions, iQuestion1, iQuestion2, iCorrect1, iCorrect2, iQuizQuestion: Integer;
    sRegion, sRegionTemp, sCorrectAnswer: string;
    bNoAnswer, bNoAnswerTemp: Boolean;
    aCastlesSCO, aCastlesENG, aCastlesEU50, aCastlesEUAll, aCastles: TCastles;
    aCastlesDone: TBooleans;
    rbAnswers: array[0..5] of TRadioButton;
  end;

var
  fCQuiz3: TfCQuiz3;

implementation

{$R *.lfm}

{ Read castles form castles.txt file }

procedure ReadCastles(out CastlesSCO, CastlesENG, CastlesEU50, CastlesEUAll: TCastles);

var
  SCO, ENG, EU50, N: Integer;
  Line: string;
  CastleFile: Text;

begin
  SetLength(CastlesSCO, 35); SetLength(CastlesENG, 49);
  SetLength(CastlesEU50, 50); SetLength(CastlesEUAll, 104);
  Assign(CastleFile, 'castles.txt'); Reset(CastleFile);
  SCO := 0; ENG := 0; EU50 := 0; N := 0;
  while not EoF(CastleFile) do begin
    Readln(CastleFile, Line);
    if Line <> '' then begin
      Inc(N);
      CastlesEUAll[N - 1].Region := LeftStr(Line, 3);
      CastlesEUAll[N - 1].Name := UTF8Trim(UTF8Copy(Line, 7, UTF8Length(Line)));
      if Copy(Line, 5, 1) = '2' then begin
        // North-West Europe castles 50 items list
        Inc(EU50);
        CastlesEU50[EU50 - 1] := CastlesEUAll[N - 1];
      end;
      if CastlesEUAll[N - 1].Region = 'ENG' then begin
        // English castles
        Inc(ENG);
        CastlesENG[ENG - 1] := CastlesEUAll[N - 1];
      end
      else if LeftStr(Line, 3) = 'SCO' then begin
        // Scottish castles
        Inc(SCO);
        CastlesSCO[SCO - 1] := CastlesEUAll[N - 1];
      end;
    end;
  end;
  Close(CastleFile);
end;

{ Display radiobuttons labels or fill the castles names combobox with the answer proposals for the actual question }

procedure DisplayAnswers(Answers: Integer; PicCastle: string; NoAnswer: Boolean; var Castles: TCastles; Out CorrectAnswer: string);

var
  I, J: Integer;
  Castle: string;
  OK: Boolean;
  CastleList: array of string;

begin
  CorrectAnswer := PicCastle;                                                  // correct answer may be 'no correct answer' (-> belwow)
  SetLength(CastleList, Answers);
  // Fill array with random castle names
  for I := 0 to Length(CastleList) - 1 do begin
    repeat
      OK := True;
      Castle := Castles[Random(Length(Castles))].Name;
      if Castle = PicCastle then begin
        // Only false answers (for now)
        OK := False;
      end
      else if I > 0 then begin
        // Avoid doubles
        for J := 0 to I - 1 do begin
          if Castle = CastleList[J] then
            OK := False;
        end;
      end;
    until OK;
    CastleList[I] := Castle;
  end;
  if not NoAnswer or (Random(6) <> 0) then begin
    // Replace one random answer item by the correct answer
    CastleList[Random(Length(CastleList))] := PicCastle;
  end
  else begin
    // List will not contain any correct answer
    CorrectAnswer := 'No correct answer';
  end;
  // Alphabetic sort
  for I := 0 to Length(CastleList) - 2 do begin
    for J := I + 1 to Length(CastleList) - 1 do begin
      if CastleList[J] < CastleList[I] then begin
        Castle := CastleList[I]; CastleList[I] := CastleList[J]; CastleList[J] := Castle;
      end;
    end;
  end;
  // Fill castles from array to form controls
  if Answers = 5 then begin
    // If 5 answers, use radio buttons
    for I := 0 to 4 do begin
      fCQuiz3.rbAnswers[I].Caption := ' ' + CastleList[I];
      fCQuiz3.rbAnswers[I].Checked := False;
    end;
    if NoAnswer then begin
      // If "no correct answer" is selected, add a 6th radiobutton with this option
      fCQuiz3.rbAnswers[5].Caption := ' No correct answer';
      fCQuiz3.rbAnswers[5].Checked := False;
    end;
  end
  else begin
    // If 10 or 20 answers, use combobox
    fCQuiz3.cobAnswers.Clear;
    if NoAnswer then begin
      // If "no correct answer" is selected, add an item with this option to combobox
      fCQuiz3.cobAnswers.Items.AddText('--No correct answer--');
    end;
    for I := 0 to Length(CastleList) - 1 do
      // Add the 10 or 20 answer choices to the combobox
      fCQuiz3.cobAnswers.Items.AddText(CastleList[I]);
    fCQuiz3.cobAnswers.ItemIndex := 0;
  end;
end;

{ Get player answer (castle selection) from the form }

function GetUserAnswer(Answers: Integer): string;

var
  Rb, I: Integer;
  Answer: string;

begin
  Answer := '';
  if Answers = 5 then begin
    // If 5 answers, get player answer from radiobuttons
    if fCQuiz3.rbAnswer6.Visible then
      Rb := 6                                                                  // "no correct answer" selected
    else
      Rb := 5;                                                                 // "no correct answer" unselected
    for I := 0 to Rb - 1 do begin
      // Get player answer from the radiobutton, that is actually checked
      if fCQuiz3.rbAnswers[I].Checked then begin
        Answer := fCQuiz3.rbAnswers[I].Caption;
        Delete(Answer, 1, 1);                                                  // removes the space at the beginning of the radiobutton label
      end;
    end;
  end
  else begin
    // If 10 or 20 answers, get player answer from combobox
    Answer := fCQuiz3.cobAnswers.Text;
    Answer := StringReplace(Answer, '--', '', [rfReplaceAll]);                 // remove the '--' at the beginning and end of combobox item
  end;
  Result := Answer;
end;

{************}
{* TfCQuiz3 *}
{************}

{ Application start: Initialisation }

procedure TfCQuiz3.FormCreate(Sender: TObject);

begin
  // Create array with radiobuttons
  rbAnswers[0] := rbAnswer1; rbAnswers[1] := rbAnswer2; rbAnswers[2] := rbAnswer3;
  rbAnswers[3] := rbAnswer4; rbAnswers[4] := rbAnswer5; rbAnswers[5] := rbAnswer6;
  // Read castle lists from file
  ReadCastles(aCastlesSCO, aCastlesENG, aCastlesEU50, aCastlesEUAll);
  // Initialize variables (startup parameters)
  iPlayers := 1; sRegionTemp := 'North-West Europe (I)'; iAnswersTemp := 5; bNoAnswerTemp := True;
  // Start random number generator
  Randomize;
  // Prepare for a new quiz (with startup parameters)
  mFileNew.Click;
end;

{ Menu item "File > New quiz": Prepare for new quiz }

procedure TfCQuiz3.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  tiRQ.Enabled := False;
  sRegion := sRegionTemp; iAnswers := iAnswersTemp; bNoAnswer := bNoAnswerTemp;  // Settings selected become active now
  // Set quiz title and choose Region/region dependent castle list
  stQuiz.Caption := 'QUIZ: Castles in ' + sRegion + '.';
  // Choose castles list
  if sRegion = 'England' then
    aCastles := aCastlesENG
  else if sRegion = 'Scotland' then
    aCastles := aCastlesSCO
  else if sRegion = 'North-West Europe (I)' then
    aCastles := aCastlesEU50
  else
    aCastles := aCastlesEUAll;
  // Show and clear or hide radiobuttons resp. combobox
  if iAnswers = 5 then begin
    // If user selected 5 answer proposals, use radiobuttons
    for I := 0 to 5 do begin
      if (I < 5) or ((I = 5) and bNoAnswer) then begin                         // 6th radiobutton only if "no correct answer" is selected
        rbAnswers[I].Visible := True; rbAnswers[I].Checked := False;
        rbAnswers[I].Caption := ' Answer ' + IntToStr(I + 1);
      end;
    end;
    cobAnswers.Visible := False;                                               // combobox not used, with 5 answer proposals
  end
  else begin
    // If user selected 10 or 20 answer proposals, use combobox
    for I := 0 to 5 do begin
      rbAnswers[I].Visible := False;                                           // radiobuttons not used, with 10 or 20 answer proposals
    end;
    cobAnswers.Visible := True; cobAnswers.Clear;
  end;
  // Clear form controls
  edScore1.Text := ''; edScore2.Text := '';
  edAnswer.Text := ''; edAnswer2.Text := '';
  stSlide.Caption := 'Quiz'; edSlide.Text := '';
  imCastle.Picture.Clear;
  // Reset button captions
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Visible := False;
end;

{ Menu item "File > Slideshow": Start slideshow }

procedure TfCQuiz3.mFileSlideshowClick(Sender: TObject);

// The slideshow is actually performed within the timer routine

var
  I: Integer;

begin
  mFileNew.Click;                                                              // just done to clear the quiz data on the form
  iQuestions := Length(aCastles);                                              // images shown will be those corr. to current country/region
  iQuestion1 := 0;
  stSlide.Caption := 'Slideshow'; edSlide.Text := '';
  // Hide non-slideshow-related form controls
  for I := 0 to 5 do
    rbAnswers[I].Visible := False;
  cobAnswers.Visible := False;
  // Show slideshow-related form controls
  edSlide.Visible := True;
  // Adapt buttons
  btStart.Caption := 'Stop'; btStart.Enabled := True; btStart.SetFocus;        // use the Quiz button as Stop button for the slideshow
  btPause.Caption := 'Pause'; btPause.Visible := True;                         // use a second button (slideshow only) to pause/resume the slideshow
  tiRQ.Enabled := True;                                                        // enable the slideshow timer
end;

{ Menu item "File > Exit": Exit the application }

procedure TfCQuiz3.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Number of players > ..": Select 1- or 2-player quiz }

procedure TfCQuiz3.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;  mSettingsPlayers2.Checked := False;
  edName2.Enabled := False;  edScore2.Enabled := False;                        // in 1-player mode, disable not used Player 2 controls
  iPlayers := 1;
  mFileNew.Click;                                                              // automatically start a new quiz, if number of players changes
end;

procedure TfCQuiz3.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := False; mSettingsPlayers2.Checked := True;
  edName2.Enabled := True;  edScore2.Enabled := True;
  iPlayers := 2;
  mFileNew.Click;
end;

{ Menu items "Options > Country/Region > ..": Select castles country/region }

procedure TfCQuiz3.mSettingsRegionSCOClick(Sender: TObject);

begin
  mSettingsRegionSCO.Checked := True;     mSettingsRegionENG.Checked := False;
  mSettingsRegionNWEUR1.Checked := False; mSettingsRegionNWEUR2.Checked := False;
  sRegionTemp := 'Scotland';
end;

procedure TfCQuiz3.mSettingsRegionENGClick(Sender: TObject);

begin
  mSettingsRegionSCO.Checked := False;    mSettingsRegionENG.Checked := True;
  mSettingsRegionNWEUR1.Checked := False; mSettingsRegionNWEUR2.Checked := False;
  sRegionTemp := 'England';
end;

procedure TfCQuiz3.mSettingsRegionNWEUR1Click(Sender: TObject);

begin
  mSettingsRegionSCO.Checked := False;   mSettingsRegionENG.Checked := False;
  mSettingsRegionNWEUR1.Checked := True; mSettingsRegionNWEUR2.Checked := False;
  sRegionTemp := 'North-West Europe (I)';
end;

procedure TfCQuiz3.mSettingsRegionNWEUR2Click(Sender: TObject);

begin
  mSettingsRegionSCO.Checked := False;    mSettingsRegionENG.Checked := False;
  mSettingsRegionNWEUR1.Checked := False; mSettingsRegionNWEUR2.Checked := True;
  sRegionTemp := 'North-West Europe (II)';
end;

{ Menu items "Options > Answer proposals > ..": Select number of answer choices to be displayed }

procedure TfCQuiz3.mSettingsAnswers5Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := True; mSettingsAnswers10.Checked := False; mSettingsAnswers20.Checked := False;
  iAnswersTemp := 5;
end;

procedure TfCQuiz3.mSettingsAnswers10Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := True; mSettingsAnswers20.Checked := False;
  iAnswersTemp := 10;
end;

procedure TfCQuiz3.mSettingsAnswers20Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := False; mSettingsAnswers20.Checked := True;
  iAnswersTemp := 20;
end;

{ Menu item "Options > Enable 'No correct answer'": Toggle 'no correct answer' enable/disable }

procedure TfCQuiz3.mOptionsNoAnswerClick(Sender: TObject);

begin
  if mOptionsNoAnswer.Checked then
    mOptionsNoAnswer.Checked := False
  else
    mOptionsNoAnswer.Checked := True;
  bNoAnswerTemp := mOptionsNoAnswer.Checked;
  rbAnswer6.Visible := bNoAnswerTemp;                                          // show 6th radiobutton only if 'no correct answer' is selected
end;

{ Menu items "Options > Slideshow interval > ...": Select slideshow timer interval }

procedure TfCQuiz3.mSettingsSlideshow1Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := True;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False; mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1000;
end;

procedure TfCQuiz3.mSettingsSlideshow2Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := True;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1500;
end;

procedure TfCQuiz3.mSettingsSlideshow3Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := True;   mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 2000;
end;

procedure TfCQuiz3.mSettingsSlideshow4Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := True;
  tiRQ.Interval := 3000;
end;

{ Menu item "Help > Help": Display (short) application help text }

procedure TfCQuiz3.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Select the number of players, a country/region, the number of answers that should be proposed ';
  S += 'and if the proposal "No correct answer" should be used or not. Note that your selections will ';
  S += 'only become active after choosing "New quiz" in the "File" menu. Push "Start"/"Question" to ';
  S += 'display a new photo, push "Answer" to validate your selection, done among the different proposals displayed.';
  MessageDlg('"CastlesQuiz3 Help"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about text }

procedure TfCQuiz3.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Photoquiz (plus slideshow), concerning the castles of Nort-West Europe (England, Scotland, Wales, Ireland).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2021.';
  MessageDlg('About "CastlesQuiz3"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > Data/photos": Display information concerning the origin of castle data and photos }

procedure TfCQuiz3.mHelpPhotosClick(Sender: TObject);

var
  S: string;

begin
  S := 'Origin of data and photos:' + LineEnding;
  S += '"CastlesQuiz3" takes into account the castles listed on the website https://www.burgen.de/. ';
  S += 'A great part of the photos are also taken from that site.' + LineEnding + LineEnding;
  MessageDlg('"CastlesQuiz3" Info', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer/Stop": Quiz question/answer resp. stop of slideshow }

procedure TfCQuiz3.btStartClick(Sender: TObject);

var
  Score1, Score2, I: Integer;
  Filename, UserAnswer: string;

begin
  // Button "Start/Question": Generation of a new quiz question
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Question') then begin
    // Some initialisations to do if it is the beginning of a new quiz
    // Note: The slideshow is automatically started ("Start" is always for the quiz)
    if btStart.Caption = 'Start' then begin
      SetLength(aCastlesDone, Length(aCastles));
      for I := 0 to Length(aCastlesDone) - 1 do
        aCastlesDone[I] := False;                                              // reset all castles to "not done"!
      iQuestions := Length(aCastles);                                          // the number of questions (castles) for current country/region
      iQuestion1 := 0; iQuestion2 := 0;                                        // questions asked, players 1 and 2
      iCorrect1 := 0; iCorrect2 := 0;                                          // correct answers, players 1 and 2
      Score1 := 0; edScore1.Text := '';                                        // success percentages, players 1 and 2
      if iPlayers = 2 then begin
        // Player 2 initialisation (if 2-player mode is selected)
        Score2 := 0;
        edScore2.Text := '';
        if iQuestions mod 2 <> 0 then                                          // equal number of questions for the 2 players
          Dec(iQuestions);
      end;
      edName1.Color := clAqua; edName2.Color := clDefault;                     // "aqua" highlighting denotes the player currently playing
    end;
    if iPlayers = 1 then begin
      // 1-player mode
      Inc(iQuestion1);
    end
    else begin
      // 2-player mode
      if edName1.Color = clAqua then
        Inc(iQuestion1)
      else
        Inc(iQuestion2);
    end;
    // Find a random castle (that has not yet been shown during this quiz)
    repeat
      iQuizQuestion := Random(iQuestions);
    until not aCastlesDone[iQuizQuestion];
    aCastlesDone[iQuizQuestion] := True;                                       // mark this castle as done
    // Display the castle photo
    Filename := aCastles[iQuizQuestion].Name;
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imCastle.Picture.LoadFromFile(Filename);
    stSlide.Caption := 'Castle ' + IntToStr(iQuestion1 + iQuestion2) + '/' + IntToStr(iQuestions);
    // Display answer proposals (5, 10 or 20 proposals, from which player can select answer from)
    DisplayAnswers(iAnswers, aCastles[iQuizQuestion].Name, bNoAnswer, aCastles, sCorrectAnswer);
    // Prepare for player's answer
    edAnswer.Text := ''; edAnswer2.Text := '';
    btStart.Caption := 'Answer';
  end
  // Button "Answer": Check the castle name entered by the player; update the score
  else if btStart.Caption = 'Answer' then begin
    // Get player's answer from form (radiobuttons or combobox)
    UserAnswer := GetUserAnswer(iAnswers);
    // Castle name entered is correct
    if UserAnswer = sCorrectAnswer then begin
      // Update score for player 1 OR player 2
      if edName1.Color = clAqua then
        Inc(iCorrect1)
      else
        Inc(iCorrect2);
      // Green font text for correct answer
      edAnswer.Text := 'This answer is correct!';
      edAnswer.Font.Color := clLime;
    end
    // Castle name entered is not correct
    else begin
      // Red font text for false answer
      edAnswer.Text := 'This answer is false!';
      edAnswer.Font.Color := clRed;
      edAnswer2.Text := 'Correct: ' + sCorrectAnswer;                          // dispaly correct answer
    end;
    // Update the scores
    Score1 := Round(100 * (iCorrect1 / iQuestion1));
    edScore1.Text := IntToStr(Score1) + ' %';
    if iPlayers = 2 then begin
      // 2-player mode
      if iQuestion2 <> 0 then begin                                            // avoid division by zero with the very first question!
        Score2 := Round(100 * (iCorrect2 / iQuestion2));
        edScore2.Text := IntToStr(Score2) + ' %';
      end;
      // Change "actual player" highlighting (alternate play)
      if edName1.Color = clAqua then begin
        edName1.Color := clDefault; edName2.Color := clAqua;
      end
      else begin
        edName1.Color := clAqua; edName2.Color := clDefault;
      end;
    end;
    if iQuestion1 + iQuestion2 < iQuestions then begin
      // Still questions (castles) left: Next button push will be for a new question
      btStart.Caption := 'Question';
    end
    else begin
      // All questions (castles) done: End of quiz message and quiz button disable
      MessageDlg('Quiz end', 'All castles have been done. Quiz terminated.', mtInformation, [mbOK], 0);
      btStart.Caption := 'Start'; btStart.Enabled := False;
    end
  end
  // Button "Stop": During slideshow, use "Stop" button to terminate it
  else if btStart.Caption = 'Stop' then begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Slideshow end', 'The slideshow has been terminated by user.', mtInformation, [mbOK], 0);
    btStart.Enabled := False; btPause.Visible := False;
  end;
end;

{ Button "Pause/Resume": Pause resp. resume the slideshow }

procedure TfCQuiz3.btPauseClick(Sender: TObject);

begin
  // Button "Pause": Pause the slideshow
  if btPause.Caption = 'Pause' then begin
    tiRQ.Enabled := False;                                                     // disable the timer
    btPause.Caption := 'Resume';
  end
  // Button "Resume": Resume the slideshow
  else begin
    btPause.Caption := 'Pause';
    tiRQ.Enabled := True;                                                      // re-enable the timer
  end;
end;

{ Slideshow timer routine }

procedure TfCQuiz3.tiVQTimer(Sender: TObject);

var
  Region, Filename: string;

begin
  Inc(iQuestion1);
  // Proceed if there are still castles to be displayed
  if iQuestion1 <= iQuestions then begin
    // Display castle's name
    stSlide.Caption := 'Castle ' + IntToStr(iQuestion1) + '/' + IntToStr(iQuestions);
    edSlide.Text := aCastles[iQuestion1 - 1].Name;
    if LeftStr(sRegion, 17) = 'North-West Europe' then begin
      case LeftStr(aCastles[iQuestion1 - 1].Region, 1)[1] of
        'E': Region := 'England';
        'S': Region := 'Scotland';
        'W': Region := 'Wales';
        'I': Region := 'Ireland';
      end;
      // Add castle country/region
      edSlide.Text := edSlide.Text + ' (' + Region + ')';
    end;
    // Display castle image
    Filename := aCastles[iQuestion1 - 1].Name;
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    if not FileExists(Filename) then
      // This should never happen (is useful during testing...)
      MessageDlg('File error', 'Photo not found for ' + aCastles[iQuestion1 - 1].Name, mtInformation, [mbOK], 0);
    imCastle.Picture.LoadFromFile(Filename);
  end
  // Stop slideshow if all castles (for country/region selected) have been displayed
  else begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Slideshow end', 'All castles have been shown. Slideshow terminated.', mtInformation, [mbOK], 0);
    // Disable all buttons (until user selects an item in the "File" menu)
    btStart.Enabled := False; btPause.Visible := False;
  end;
end;

end.

