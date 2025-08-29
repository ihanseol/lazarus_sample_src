{***************************************}
{* Main unit for VogelQuiz application *}
{***************************************}

// Change log:
// Version 1.0 (June 2018): original version
// Version 2.0 (December 2019): addition of English bird names
//   - consideration of 2 data files, one with the German, the other with the English bird names
//   - GUI adapted for bird name language selection
// Version 2.1 (June 2020): bug fix
//   - program hangs, when starting a new quiz

unit vquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls;

type
  TVQRecord = record
    Bird: string[30];
    BirdShort: string[20];
    QuizLevels: string[5];
    BirdnameModif: string[1];
  end;
  TVQRecords  = array of TVQRecord;
  TVQBooleans = array of Boolean;
  { TfVQ }
  TfVQ = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSlideshow, mFileExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsLevel, mSettingsLevel1, mSettingsLevel2, mSettingsLevel3, mSettingsLevel4, mSettingsLevelX: TMenuItem;
    mSettingsLanguage, mSettingsLanguageGer, mSettingsLanguageEng: TMenuItem;
    mSettingsShortNames, mSettingsShortNamesEng: TMenuItem;
    MenuItem1: TMenuItem;
    mSettingsSlideshow, mSettingsSlideshow1, mSettingsSlideshow2, mSettingsSlideshow3, mSettingsSlideshow4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imBird: TImage;
    stQuiz: TStaticText;
    edName1, edName2: TEdit;
    edScore1, edScore2: TEdit;
    cobBirds: TComboBox;
    edAnswer, edAnswer2: TEdit;
    stSlide: TStaticText;
    edSlide: TEdit;
    btStart: TButton;
    btPause: TButton;
    tiVQ: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSlideshowClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsLevel1Click(Sender: TObject);
    procedure mSettingsLevel2Click(Sender: TObject);
    procedure mSettingsLevel3Click(Sender: TObject);
    procedure mSettingsLevel4Click(Sender: TObject);
    procedure mSettingsLevelXClick(Sender: TObject);
    procedure mSettingsLanguageGerClick(Sender: TObject);
    procedure mSettingsLanguageEngClick(Sender: TObject);
    procedure mSettingsShortNamesClick(Sender: TObject);
    procedure mSettingsShortNamesEngClick(Sender: TObject);
    procedure mSettingsSlideshow1Click(Sender: TObject);
    procedure mSettingsSlideshow2Click(Sender: TObject);
    procedure mSettingsSlideshow3Click(Sender: TObject);
    procedure mSettingsSlideshow4Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tiVQTimer(Sender: TObject);
  private
    iPlayers, iQuestions, iQuestion1, iQuestion2, iCorrect1, iCorrect2, iQuizQuestion: Integer;
    cLevel: Char;
    sLanguage: string;
    aAllBirdsGer, aAllBirdsEng, aQuizBirds: TVQRecords;
    bQuizBirds: TVQBooleans;
  end;

var
  fVQ: TfVQ;

implementation

{$R *.lfm}

{ Read bird records form data files }

procedure ReadBirds(out BirdsGer, BirdsEng: TVQRecords);

var
  I, IX: Integer;
  FileName: string;
  Birds: TVQRecords;
  BirdsFile: file of TVQRecord;

begin
  for I := 1 to 2 do begin
    // Do twice: the first time for German, the second time for English names
    if I = 1 then
      FileName := 'vogelquiz.dat'
    else
      FileName := 'birdquiz.dat';
    Assign(BirdsFile, FileName); Reset(BirdsFile);
    IX := 0;
    while not EoF(BirdsFile) do begin
      Inc(IX);
      SetLength(Birds, IX + 1);
      Read(BirdsFile, Birds[IX]);                                              // fill the data of all 120 birds in an array of records
    end;
    if I = 1 then begin
      SetLength(BirdsGer, Length(Birds));
      BirdsGer := Birds;                                                       // array with German names
    end
    else begin
      SetLength(BirdsEng, Length(Birds));
      BirdsEng := Birds;                                                       // array with English names
    end;
    Close(BirdsFile);
  end;
end;

{ Get (German) filename for a given (English named) bird }

function GetFileName(Bird: string; var GerBirds, EngBirds: TVQRecords): string;

var
  I, IX: Integer;

begin
  I := 0; IX := -1;
  while (IX = -1) and (I < Length(EngBirds)) do begin
    if Bird = EngBirds[I].Bird then
      IX := I;
    Inc(I);
  end;
  Result := Trim(GerBirds[IX].Bird);
end;

{ Get a bird's (long or short) name with umlauts and ß as applicable }

function BirdName(Birds: TVQRecords; IX: Integer): string;

// Using ae, oe, ue and ss instead of ä, ö, ü and ß in the files avoids all problems with filenames and Pascal Copy instruction

var
  Bird: string;

begin
  if fVQ.mSettingsShortNames.Checked and (fVQ.mSettingsLevel1.Checked or fVQ.mSettingsLevel2.Checked) then
    // Short names at levels 1 and 2 (if this is selected)
    Bird := Birds[IX].BirdShort
  else
    // Full name in all other cases
    Bird := Birds[IX].Bird;
  if fVQ.mSettingsLanguageGer.Checked then begin
    // Transformation to umlauts and ß (as indicated in the bird's data record)
    if Birds[IX].BirdnameModif = 'u' then begin
      Bird := StringReplace(Bird, 'ae', 'ä', [rfReplaceAll]);
      Bird := StringReplace(Bird, 'oe', 'ö', [rfReplaceAll]);
      Bird := StringReplace(Bird, 'ue', 'ü', [rfReplaceAll]);
    end
    else if Birds[IX].BirdnameModif = 's' then
      Bird := StringReplace(Bird, 'ss', 'ß', [rfReplaceAll]);
  end
  else begin
    // Use simplified English names (if this is selected)
    if fVQ.mSettingsShortNamesEng.Checked then begin
      Bird := StringReplace(Bird, 'common ', '', []);
      Bird := StringReplace(Bird, 'European ', '', []);
      Bird := StringReplace(Bird, 'Eurasian ', '', []);
      Bird := StringReplace(Bird, 'Western ', '', []);
      Bird := StringReplace(Bird, 'Northern ', '', []);
    end;
  end;
  BirdName := Bird;
end;

{ Fill the bird names combobox with the values for the actual quiz }

procedure FillBirdnames(Birds: TVQRecords);

var
  I, J: Integer;
  Bird: string;

begin
  fVQ.cobBirds.Clear;
  // Add a "no answer" item
  if fVQ.mSettingsLanguageGer.Checked then
    fVQ.cobBirds.Items.AddText('-Keine Antwort-')
  else
    fVQ.cobBirds.Items.AddText('-No answer-');
  fVQ.cobBirds.ItemIndex := 0;
  // Fill in actual values
  for I := 1 to Length(Birds) - 1 do
    fVQ.cobBirds.Items.AddText(BirdName(Birds, I));
  if fVQ.mSettingsLanguageEng.Checked or (fVQ.mSettingsShortNames.Checked and (fVQ.mSettingsLevel1.Checked or fVQ.mSettingsLevel2.Checked)) then begin
    // If English or short names are used, resort the combobox items
    for I := 1 to Length(Birds) - 2 do begin
      for J := I + 1 to Length(Birds) - 1 do begin
        if LowerCase(fVQ.cobBirds.Items[J]) < LowerCase(fVQ.cobBirds.Items[I]) then begin
          Bird := fVQ.cobBirds.Items[I]; fVQ.cobBirds.Items[I] := fVQ.cobBirds.Items[J]; fVQ.cobBirds.Items[J] := Bird;
        end;
      end;
    end;
  end;
end;

{ Create array of records for birds to be considered with the actual quiz }

procedure CreateQuiz(AllBirds: TVQRecords; Level: Char; out QuizBirds: TVQRecords; out BQuizBirds: TVQBooleans);

var
  I, J, P: Integer;

begin
  J := 0;
  for I := 1 to Length(AllBirds) - 1 do begin
    // For all birds, keep those that are marked to be used with actual quiz level
    P := Pos(Level, AllBirds[I].QuizLevels);
    if P > 0 then begin
      Inc(J);
      SetLength(QuizBirds, J + 1);
      SetLength(BQuizBirds, J + 1);
      QuizBirds[J] := AllBirds[I];                                             // fill in the birds record
      BQuizBirds[J] := False;                                                  // array of Booleans to be used to mark birds that have already been done
    end;
  end;
  FillBirdnames(QuizBirds);                                                    // fill the bird names combobox
end;

{******}
{ TfVQ }
{******}

{ Application start: Initialisation }

procedure TfVQ.FormCreate(Sender: TObject);

begin
  SetLength(aAllBirdsGer, 0); SetLength(aAllBirdsEng, 0);
  SetLength(aQuizBirds, 0); SetLength(bQuizBirds, 0);
  ReadBirds(aAllBirdsGer, aAllBirdsEng);                                       // read bird data from files
  iPlayers := 1; cLevel := '2'; sLanguage := 'ger';
  CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds);                    // create default quiz
  Randomize;
  mFileNew.Click;
end;

{ Menu item "Datei > Neuer Quiz": Reset all for new quiz }

procedure TfVQ.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to Length(bQuizBirds) do
    bQuizBirds[I] := False;                                                    // reset all birds to "not done"!
  stQuiz.Caption := '';
  edScore1.Text := ''; edScore2.Text := '';
  edAnswer.Text := ''; edAnswer2.Text := '';
  cobBirds.ItemIndex := 0;
  imBird.Picture.Clear;
  mSettings.Enabled := True;                                                   // (re)enable access to "Einstellungen" menu
  mSettingsPlayers.Enabled := True;                                            // slideshow image interval settings remains enabled
  mSettingsLevel.Enabled := True;
  mSettingsLanguage.Enabled := True;
  btStart.Caption := 'Start'; btStart.Enabled := True;
end;

{ Menu item "Datei > Diashow": Start slideshow }

procedure TfVQ.mFileSlideshowClick(Sender: TObject);

// The slideshow is actually performed within the timer routine

begin
  mFileNew.Click;                                                              // just done to clear the quiz data on the form
  mSettingsPlayers.Enabled := False;                                           // slideshow image interval settings remains enabled
  mSettingsLevel.Enabled := False;
  mSettingsLanguage.Enabled := False;
  iQuestions := Length(aQuizBirds) - 1;                                        // images shown will be those corresponding to current level
  iQuestion1 := 0;
  stSlide.Caption := ''; edSlide.Text := '';
  stSlide.Visible := True;
  edSlide.Visible := True;                                                     // bird's name will be displayed here
  btStart.Caption := 'Stop';                                                   // use the quiz Start/Next button as stop button for the slideshow
  btStart.Enabled := True;
  btPause.Caption := 'Pause';                                                  // a second button (slideshow only) to pause/resume the slideshow
  btPause.Visible := True;
  tiVQ.Enabled := True;                                                        // enable the slideshow timer (slideshow will be started)
end;

{ Menu item "Datei > Verlassen": Exit the application }

procedure TfVQ.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Einstellungen > Anzahl der Spieler > ..": Select 1- or 2-player-quiz }

procedure TfVQ.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;
  mSettingsPlayers2.Checked := False;
  edName2.Enabled := False;
  edScore2.Enabled := False;
  iPlayers := 1;
end;

procedure TfVQ.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := True;
  mSettingsPlayers1.Checked := False;
  edName2.Enabled := True;
  edScore2.Enabled := True;
  iPlayers := 2;
end;

{ Menu items "Einstellungen > Schwierigkeitsgrad > ..": Select quiz level }

procedure TfVQ.mSettingsLevel1Click(Sender: TObject);

begin
  if not mSettingsLevel1.Checked then begin
    mSettingsLevel1.Checked := True;  mSettingsLevel2.Checked := False;  mSettingsLevel3.Checked := False;
    mSettingsLevel4.Checked := False; mSettingsLevelX.Checked := False;
    mSettingsShortNames.Enabled := True;
    cLevel := '1';
    if sLanguage = 'ger' then
      CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds)
    else
      CreateQuiz(aAllBirdsEng, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

procedure TfVQ.mSettingsLevel2Click(Sender: TObject);

begin
  if not mSettingsLevel2.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := True;   mSettingsLevel3.Checked := False;
    mSettingsLevel4.Checked := False;  mSettingsLevelX.Checked := False;
    mSettingsShortNames.Enabled := True;
    cLevel := '2';
    if sLanguage = 'ger' then
      CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds)
    else
      CreateQuiz(aAllBirdsEng, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

procedure TfVQ.mSettingsLevel3Click(Sender: TObject);

begin
  if not mSettingsLevel3.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := False;  mSettingsLevel3.Checked := True;
    mSettingsLevel4.Checked := False;  mSettingsLevelX.Checked := False;
    mSettingsShortNames.Checked := False;
    mSettingsShortNames.Enabled := False;
    cLevel := '3';
    if sLanguage = 'ger' then
      CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds)
    else
      CreateQuiz(aAllBirdsEng, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

procedure TfVQ.mSettingsLevel4Click(Sender: TObject);

begin
  if not mSettingsLevel4.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := False;  mSettingsLevel3.Checked := False;
    mSettingsLevel4.Checked := True;   mSettingsLevelX.Checked := False;
    mSettingsShortNames.Checked := False;
    mSettingsShortNames.Enabled := False;
    cLevel := '4';
    if sLanguage = 'ger' then
      CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds)
    else
      CreateQuiz(aAllBirdsEng, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

procedure TfVQ.mSettingsLevelXClick(Sender: TObject);

begin
  if not mSettingsLevelX.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := False;  mSettingsLevel3.Checked := False;
    mSettingsLevel4.Checked := False;  mSettingsLevelX.Checked := True;
    mSettingsShortNames.Checked := False;
    mSettingsShortNames.Enabled := False;
    cLevel := 'X';
    if sLanguage = 'ger' then
      CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds)
    else
      CreateQuiz(aAllBirdsEng, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

{ Menu items "Einstellungen > Sprache für Vögelnamen > ..": Select language for bird names }

procedure TfVQ.mSettingsLanguageGerClick(Sender: TObject);

begin
  if not mSettingsLanguageGer.Checked then begin
    mSettingsLanguageGer.Checked := True;  mSettingsLanguageEng.Checked := False;
    sLanguage := 'ger';
    CreateQuiz(aAllBirdsGer, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

procedure TfVQ.mSettingsLanguageEngClick(Sender: TObject);

begin
  if not mSettingsLanguageEng.Checked then begin
    mSettingsLanguageGer.Checked := False;  mSettingsLanguageEng.Checked := True;
    sLanguage := 'eng';
    CreateQuiz(aAllBirdsEng, cLevel, aQuizBirds, bQuizBirds);
  end;
end;

{ Menu item "Einstellungen > Vögel-Kurznamen": Select short or long bird names }

procedure TfVQ.mSettingsShortNamesClick(Sender: TObject);

begin
  if mSettingsShortNames.Checked then
    mSettingsShortNames.Checked := False
  else
    mSettingsShortNames.Checked := True;
  FillBirdnames(aQuizBirds);
end;

{ Menu item "Einstellungen > Vereinfachte engl. Namen": Select simple or full English bird names }

procedure TfVQ.mSettingsShortNamesEngClick(Sender: TObject);

begin
  if mSettingsShortNamesEng.Checked then
    mSettingsShortNamesEng.Checked := False
  else
    mSettingsShortNamesEng.Checked := True;
  FillBirdnames(aQuizBirds);
end;

{ Menu items "Einstellungen > Diashow Zeitintervall > ...": Select diashow timer interval }

procedure TfVQ.mSettingsSlideshow1Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := True;   mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiVQ.Interval := 1000;
end;

procedure TfVQ.mSettingsSlideshow2Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := True;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiVQ.Interval := 1500;
end;

procedure TfVQ.mSettingsSlideshow3Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := True;   mSettingsSlideshow4.Checked := False;
  tiVQ.Interval := 2000;
end;

procedure TfVQ.mSettingsSlideshow4Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := True;
  tiVQ.Interval := 3000;
end;

{ Menu item "Hilfe > Hilfe": Display (very brief) help text }

procedure TfVQ.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Anzahl der Spieler, Spache (für die Vögelnamen) und Level wählen. Mit "Start" resp. "Frage" wird der Vogel angezeigt. ';
  S += 'Seinen Namen auswählen und "Antwort" drücken. Vögel-Kurznamen gelten nur für Level 1 und 2. "Neuer Quiz" ';
  S += 'macht die Einstellungen wieder verfügbar.';
  MessageDlg('"VogelQuiz" Hilfe', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Über": Display about text }

procedure TfVQ.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Vogelquiz für 2 Spieler.' + LineEnding;
  S += 'Quiz mit 120 Vögelbildern; Vögelnamen auf Deutsch oder Englisch. Inklusive Diashow.' + LineEnding + LineEnding;
  S += 'Version 2.1, © allu, Juni 2018 - Juni 2020.';
  MessageDlg('Über "VogelQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Frage/Antwort/Stop": Quiz question/answer resp. stop of slideshow }

procedure TfVQ.btStartClick(Sender: TObject);

var
  Score1, Score2: Real;
  Filename: string;

begin
  // Generation of a new quiz question
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Frage') then begin
    // Some initialisations to do if it is the beginning of a new quiz
    if btStart.Caption = 'Start' then begin
      mSettings.Enabled := False;                                              // during quiz, no access to the settings menu
      iQuestions := Length(aQuizBirds) - 1;                                    // the number of questions (birds) for current level
      iQuestion1 := 0; iQuestion2 := 0;
      iCorrect1 := 0; iCorrect2 := 0;
      Score1 := 0; edScore1.Text := '';
      if iPlayers = 2 then begin
        Score2 := 0;
        edScore2.Text := '';
        if iQuestions mod 2 <> 0 then                                          // equal number of questions for the 2 players
          Dec(iQuestions);
      end;
      edName1.Color := clAqua; edName2.Color := clDefault;                     // "aqua" highlighting denotes the player currently playing
    end;
    // iQuestion1 and iQuestion2: actual number of questions for player 1 resp. 2
    if iPlayers = 1 then
      Inc(iQuestion1)
    else begin
      if edName1.Color = clAqua then                                           // actual player = player 1
        Inc(iQuestion1)
      else                                                                     // actual player = player 2
        Inc(iQuestion2);
    end;
    stQuiz.Caption := 'QUIZ: Vogel ' + IntToStr(iQuestion1 + iQuestion2) + ' / ' + IntToStr(iQuestions);
    // Find a random bird that has not yet been shown during this quiz
    repeat
      iQuizQuestion := Random(iQuestions) + 1;
    until bQuizBirds[iQuizQuestion] = False;
    bQuizBirds[iQuizQuestion] := True;                                         // mark this bird as done
    // Display the bird's image
    if mSettingsLanguageGer.Checked then
      Filename := Trim(aQuizBirds[iQuizQuestion].Bird)
    else
      Filename := GetFileName(aQuizBirds[iQuizQuestion].Bird, aAllBirdsGer, aAllBirdsEng);
    Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imBird.Picture.LoadFromFile(Filename);
    // Prepare for player's answer
    edAnswer.Text := '';
    edAnswer2.Text := '';
    btStart.Caption := 'Antwort';
  end
  // Check the bird name entered by the player; update the score
  else if btStart.Caption = 'Antwort' then begin
    // Bird entered is correct
    if cobBirds.Text = BirdName(aQuizBirds, iQuizQuestion) then begin
      // Update score for player 1 or player 2 as appropriate
      if edName1.Color = clAqua then                                           // actual player = player 1
        Inc(iCorrect1)
      else                                                                     // actual player = player 2
        Inc(iCorrect2);
      // "Lime" text for correct answer
      edAnswer.Text := 'Diese Antwort ist richtig!';
      edAnswer.Font.Color := clLime;
    end
    // Bird entered is not correct
    else begin
      // "Red" text for false answer
      edAnswer.Text := 'Diese Antwort ist falsch!';
      edAnswer.Font.Color := clRed;
      // Dispaly of the correct answer
      edAnswer2.Text := 'Richtig: ' + BirdName(aQuizBirds, iQuizQuestion);
    end;
    // Update the scores (percentage with 2 decimal digits)
    Score1 := 100 * (iCorrect1 / iQuestion1);
    edScore1.Text := FloatToStrF(Score1, ffGeneral, 4, 2) + ' %';
    if iPlayers = 2 then begin
      if iQuestion2 <> 0 then begin                                            // this is the case with the very first question
        Score2 := 100 * (iCorrect2 / iQuestion2);
        edScore2.Text := FloatToStrF(Score2, ffGeneral, 4, 2) + ' %';
      end;
      // Change "actual player" highlighting
      if edName1.Color = clAqua then begin
        edName1.Color := clDefault;
        edName2.Color := clAqua;
      end
      else begin
        edName1.Color := clAqua;
        edName2.Color := clDefault;
      end;
    end;
    if iQuestion1 + iQuestion2 < iQuestions then begin
      // Birds left: Next button push will be for a new question
      btStart.Caption := 'Frage';
    end
    else begin
      // All birds done: Message, button disable and re-enable settings
      MessageDlg('Ende dieses Quizes', 'Im Menü "Quiz" kann ein neuer Quiz gestartet werden', mtInformation, [mbOK], 0);
      mSettings.Enabled := True;                                               // re-enable "Einstellungen" menu (for new quiz)
      btStart.Enabled := False;
    end
  end
  // During slideshow, use "Stop" button to terminate it
  else if btStart.Caption = 'Stop' then begin
    tiVQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Ende der Diashow', 'Die Diashow wurde vom Benutzer abgebrochen', mtInformation, [mbOK], 0);
    mSettingsPlayers.Enabled := True; mSettingsLevel.Enabled := True; mSettingsLanguage.Enabled := True;
    stSlide.Visible := False; edSlide.Visible := False;                        // hide all slideshow related controls
    btStart.Enabled := False;                                                  // disable main button (until a new quiz/slideshow selected)
    btPause.Visible := False;                                                  // hide the "Pause" button
  end;
end;

{ Button "Pause/Weiter": Pause resp. resume the slideshow }

procedure TfVQ.btPauseClick(Sender: TObject);

begin
  // The slideshow has to be paused
  if btPause.Caption = 'Pause' then begin
    tiVQ.Enabled := False;                                                     // just disable the timer
    btPause.Caption := 'Weiter';
  end
  // The slideshow has to be resumed
  else begin
    btPause.Caption := 'Pause';
    tiVQ.Enabled := True;                                                      // re-enable the timer
  end;
end;

{ Slideshow timer routine }

procedure TfVQ.tiVQTimer(Sender: TObject);

var
  Filename: string;

begin
  Inc(iQuestion1);
  // There are still birds to be displayed
  if iQuestion1 <= iQuestions then begin
    // Display title and bird's name
    stSlide.Caption := 'DIASHOW: Vogel ' + IntToStr(iquestion1) + ' / ' + IntToStr(iQuestions);
    edSlide.Text := BirdName(aQuizBirds, iQuestion1);
    // Display bird's image
    if mSettingsLanguageGer.Checked then
      Filename := Trim(aQuizBirds[iQuestion1].Bird)
    else
      Filename := GetFileName(aQuizBirds[iQuestion1].Bird, aAllBirdsGer, aAllBirdsEng);
    Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imBird.Picture.LoadFromFile(Filename);
  end
  // All birds (for level selected) have been displayed
  else begin
    tiVQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Ende der Diashow', 'Alle Vogelbilder (dieser Gruppe) wurden angezeigt', mtInformation, [mbOK], 0);
    stSlide.Visible := False; edSlide.Visible := False;                        // hide all slideshow related controls
    btStart.Enabled := False;                                                  // disable main button (until a new quiz/slideshow selected)
    btPause.Visible := False;                                                  // hide the "Pause" button
  end;
end;

end.

