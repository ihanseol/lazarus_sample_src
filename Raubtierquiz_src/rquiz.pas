{******************************************}
{* Main unit for RaubtierQuiz application *}
{******************************************}

// Change log:
//   * Version 1.0 (July 2019):
//     Original application
//   * Version 1.1 (June 2020)
//     Bug fixes: program hangs when starting a new quiz
//                first (or first 2) animal(s) in list are "omitted"

unit rquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls;

type
  TQuizAnimal = record
    Name: string[30];
    ShortName: string[30];
    QuizLevels: string[4];
    AnimalNameModif: string[1];
  end;
  TQuizAnimals  = array of TQuizAnimal;
  TQuizAnimalsDone = array of Boolean;
  {******}
  { TfRQ }
  {******}
  TfRQ = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSlideshow, mFileExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsLevel, mSettingsLevel1, mSettingsLevel2, mSettingsLevel3, mSettingsLevel4: TMenuItem;
    mSettingsShortNames: TMenuItem;
    MenuItem1: TMenuItem;
    mSettingsSlideshow, mSettingsSlideshow1, mSettingsSlideshow2, mSettingsSlideshow3, mSettingsSlideshow4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imAnimal: TImage;
    stQuiz: TStaticText;
    edName1, edName2: TEdit;
    edScore1, edScore2: TEdit;
    cobAnimals: TComboBox;
    edAnswer, edAnswer2: TEdit;
    stSlide: TStaticText;
    edSlide: TEdit;
    btStart: TButton;
    btPause: TButton;
    tiRQ: TTimer;
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
    procedure mSettingsShortNamesClick(Sender: TObject);
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
    aAllAnimals, aQuizAnimals: TQuizAnimals;
    aAnimalsDone: TQuizAnimalsDone;
  end;

var
  fRQ: TfRQ;

implementation

{$R *.lfm}

{ Read animals records form raubtierquiz.dat file }

procedure ReadAnimals(out Animals: TQuizAnimals);

const
  AllSpaces = '                              ';

var
  IX: Integer;
  AnimalFile: file of TQuizAnimal;

begin
  Assign(AnimalFile, 'raubtierquiz.dat'); Reset(AnimalFile);
  IX := 0;
  while not EoF(AnimalFile) do begin
    Inc(IX);
    SetLength(Animals, IX);
    Read(AnimalFile, Animals[IX - 1]);                                         // fill the data of all (100) animals in an array of records
    if Animals[IX - 1].ShortName = AllSpaces then
      Animals[IX - 1].ShortName := Animals[IX - 1].Name;
  end;
  Close(AnimalFile);
end;

{ Get an animal's (long or short) name with umlauts and ß as applicable }

function AnimalName(Animals: TQuizAnimals; IX: Integer): string;

// Using ae, oe, ue and ss instead of ä, ö, ü and ß avoids all problems with filenames and Pascal Copy instruction

var
  Animal: string;

begin
  if fRQ.mSettingsShortNames.Checked and (fRQ.mSettingsLevel1.Checked or fRQ.mSettingsLevel2.Checked) then begin
    // Short names at levels 1 and 2 (if so selected)
    Animal := Animals[IX].ShortName;
  end
  else begin
    // Full name in all other cases
    Animal := Animals[IX].Name;
  end;
  // Transformation to umlauts and ß (as indicated in the animal's data record)
  if Animals[IX].AnimalNameModif = 'u' then begin
    Animal := StringReplace(Animal, 'ae', 'ä', [rfReplaceAll]);
    Animal := StringReplace(Animal, 'oe', 'ö', [rfReplaceAll]);
    Animal := StringReplace(Animal, 'ue', 'ü', [rfReplaceAll]);
  end
  else if Animals[IX].AnimalNameModif = 's' then
    Animal := StringReplace(Animal, 'ss', 'ß', [rfReplaceAll]);
  Result := Animal;
end;

{ Fill the animal names combobox with the values for the actual quiz }

procedure FillAnimalNames(Animals: TQuizAnimals);

var
  I, J: Integer;
  Animal: string;
  AnimalList: array of string;

begin
  SetLength(AnimalList, 0);
  // Fill actual names into list
  for I := 0 to Length(Animals) - 1 do begin
    SetLength(AnimalList, I + 1);
    AnimalList[I] := AnimalName(Animals, I);
  end;
  if fRQ.mSettingsShortNames.Checked and (fRQ.mSettingsLevel1.Checked or fRQ.mSettingsLevel2.Checked) then begin
    // If short names are used, resort the combobox items
    for I := 0 to Length(AnimalList) - 2 do begin
      for J := I + 1 to Length(AnimalList) - 1 do begin
        if AnimalList[J] < AnimalList[I] then begin
          Animal := AnimalList[I]; AnimalList[I] := AnimalList[J]; AnimalList[J] := Animal;
        end;
      end;
    end;
  end;
  // Fill combobox (eliminating doubles, as exist with short names)
  fRQ.cobAnimals.Clear;
  fRQ.cobAnimals.Items.AddText('-Keine Antwort-');                             // add a "no answer" item
  fRQ.cobAnimals.ItemIndex := 0;
  for I := 0 to Length(AnimalList) - 1 do begin
    if (I = 0) or (AnimalList[I] <> AnimalList[I - 1]) then
      fRQ.cobAnimals.Items.AddText(AnimalList[I]);
  end;
end;

{ Create array of records for animals to be considered with the actual quiz }

procedure CreateQuiz(AllAnimals: TQuizAnimals; Level: Char; out QuizAnimals: TQuizAnimals; out BQuizAnimals: TQuizAnimalsDone);

var
  I, J, P: Integer;

begin
  J := 0;
  for I := 0 to Length(AllAnimals) - 1 do begin
    // For all animals, keep those that are marked to be used with actual quiz level
    P := Pos(Level, AllAnimals[I].QuizLevels);
    if P > 0 then begin
      Inc(J);
      SetLength(QuizAnimals, J);
      SetLength(BQuizAnimals, J);
      QuizAnimals[J - 1] := AllAnimals[I];                                     // fill in the animal record
      BQuizAnimals[J - 1] := False;                                            // array of Booleans to be used to mark animals that have already been done
    end;
  end;
  FillAnimalNames(QuizAnimals);                                                // fill the animal names combobox
end;

{********}
{* TfRQ *}
{********}

{ Application start: Initialisation }

procedure TfRQ.FormCreate(Sender: TObject);

begin
  SetLength(aAllAnimals, 0);
  SetLength(aQuizAnimals, 0); SetLength(aAnimalsDone, 0);
  ReadAnimals(aAllAnimals);                                                    // read animal data from file
  iPlayers := 1; cLevel := '2';
  CreateQuiz(aAllAnimals, cLevel, aQuizAnimals, aAnimalsDone);                 // create default quiz
  mFileNew.Click;
  Randomize;
end;

{ Menu item "Datei > Neuer Quiz": Reset all for new quiz }

procedure TfRQ.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to Length(aAnimalsDone) - 1 do
    aAnimalsDone[I] := False;                                                  // reset all animals to "not done"!
  stQuiz.Caption := '';
  edScore1.Text := ''; edScore2.Text := '';
  edAnswer.Text := ''; edAnswer2.Text := '';
  cobAnimals.ItemIndex := 0;
  imAnimal.Picture.Clear;
  mSettings.Enabled := True;                                                   // (re)enable access to "Einstellungen" menu
  mSettingsPlayers.Enabled := True; mSettingsLevel.Enabled := True;
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Visible := False;
end;

{ Menu item "Datei > Diashow": Start slideshow }

procedure TfRQ.mFileSlideshowClick(Sender: TObject);

// The slideshow is actually performed within the timer routine

begin
  mFileNew.Click;                                                              // just done to clear the quiz data on the form
  mSettingsPlayers.Enabled := False;                                           // slideshow image interval settings remains enabled
  mSettingsLevel.Enabled := False;
  iQuestions := Length(aQuizAnimals);                                          // images shown will be those corresponding to current level
  iQuestion1 := 0;
  stSlide.Caption := ''; edSlide.Text := '';
  stSlide.Visible := True;
  edSlide.Visible := True;                                                     // animal's name will be displayed here
  btStart.Caption := 'Stop';                                                   // use the quiz button as stop button for the slideshow
  btStart.Enabled := True;
  btPause.Caption := 'Pause';                                                  // use a second button (slideshow only) to pause/resume the slideshow
  btPause.Visible := True;
  tiRQ.Enabled := True;                                                        // enable the slideshow timer
end;

{ Menu item "Datei > Verlassen": Exit the application }

procedure TfRQ.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Einstellungen > Spieler > ..": Select 1- or 2-player-quiz }

procedure TfRQ.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;  mSettingsPlayers2.Checked := False;
  edName2.Enabled := False;  edScore2.Enabled := False;
  iPlayers := 1;
end;

procedure TfRQ.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := True;  mSettingsPlayers1.Checked := False;
  edName2.Enabled := True;  edScore2.Enabled := True;
  iPlayers := 2;
end;

{ Menu items "Einstellungen > Schwierigkeit > ..": Select quiz level }

procedure TfRQ.mSettingsLevel1Click(Sender: TObject);

begin
  if not mSettingsLevel1.Checked then begin
    mSettingsLevel1.Checked := True;  mSettingsLevel2.Checked := False;
    mSettingsLevel3.Checked := False; mSettingsLevel4.Checked := False;
    mSettingsShortNames.Enabled := True;
    cLevel := '1';
    CreateQuiz(aAllAnimals, cLevel, aQuizAnimals, aAnimalsDone);
  end;
end;

procedure TfRQ.mSettingsLevel2Click(Sender: TObject);

begin
  if not mSettingsLevel2.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := True;
    mSettingsLevel3.Checked := False;  mSettingsLevel4.Checked := False;
    mSettingsShortNames.Enabled := True;
    cLevel := '2';
    CreateQuiz(aAllAnimals, cLevel, aQuizAnimals, aAnimalsDone);
  end;
end;

procedure TfRQ.mSettingsLevel3Click(Sender: TObject);

begin
  if not mSettingsLevel3.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := False;
    mSettingsLevel3.Checked := True;   mSettingsLevel4.Checked := False;
    mSettingsShortNames.Enabled := False;
    cLevel := '3';
    CreateQuiz(aAllAnimals, cLevel, aQuizAnimals, aAnimalsDone);
  end;
end;

procedure TfRQ.mSettingsLevel4Click(Sender: TObject);

begin
  if not mSettingsLevel4.Checked then begin
    mSettingsLevel1.Checked := False;  mSettingsLevel2.Checked := False;
    mSettingsLevel3.Checked := False;  mSettingsLevel4.Checked := True;
    mSettingsShortNames.Enabled := False;
    cLevel := '4';
    CreateQuiz(aAllAnimals, cLevel, aQuizAnimals, aAnimalsDone);
  end;
end;

{ Menu item "Einstellungen > Raubtier-Kurznamen": Select short or long animal names }

procedure TfRQ.mSettingsShortNamesClick(Sender: TObject);

begin
  if mSettingsShortNames.Checked then
    mSettingsShortNames.Checked := False
  else
    mSettingsShortNames.Checked := True;
  FillAnimalNames(aQuizAnimals);
end;

{ Menu items "Einstellungen > Diashow > ...": Select diashow timer interval }

procedure TfRQ.mSettingsSlideshow1Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := True;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False; mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1000;
end;

procedure TfRQ.mSettingsSlideshow2Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := True;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1500;
end;

procedure TfRQ.mSettingsSlideshow3Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := True;   mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 2000;
end;

procedure TfRQ.mSettingsSlideshow4Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := True;
  tiRQ.Interval := 3000;
end;

{ Menu item "Hilfe > Hilfe": Display (very brief) help text }

procedure TfRQ.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Anzahl der Spieler und Level wählen. Mit "Start" resp. "Frage" wird das Raubtier angezeigt. Seinen ';
  S += 'Namen auswählen und "Antwort" drücken. Raubtier-Kurznamen gelten nur für Level 1 und 2. "Neuer Quiz" ';
  S += 'macht die Einstellungen wieder verfügbar.';
  MessageDlg('"Raubtierquiz" Hilfe', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Über": Display about text }

procedure TfRQ.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Raubtierquiz für 1 oder 2 Spieler.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, Juli 2019 - Juni 2020.';
  MessageDlg('Über "Raubtierquiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Frage/Antwort/Stop": Quiz question/answer resp. stop of slideshow }

procedure TfRQ.btStartClick(Sender: TObject);

var
  Score1, Score2: Integer;
  Filename: string;

begin
  // Button "Start/Frage": Generation of a new quiz question
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Frage') then begin
    // Some initialisations to do if it is the beginning of a new quiz
    if btStart.Caption = 'Start' then begin
      mSettings.Enabled := False;                                              // during quiz, no access to "Einstellungen" menu
      iQuestions := Length(aQuizAnimals);                                      // the number of questions (animals) for current level
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
      if edName1.Color = clAqua then
        Inc(iQuestion1)
      else
        Inc(iQuestion2);
    end;
    stQuiz.Caption := 'QUIZ: Raubtier ' + IntToStr(iQuestion1 + iQuestion2) + ' / ' + IntToStr(iQuestions);
    // Find a random animal that has not yet been shown during this quiz
    repeat
      iQuizQuestion := Random(iQuestions);
    until aAnimalsDone[iQuizQuestion] = False;
    // Mark this animal as done
    aAnimalsDone[iQuizQuestion] := True;
    // Display the animal's image
    Filename := Trim(aQuizAnimals[iQuizQuestion].Name);
    Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imAnimal.Picture.LoadFromFile(Filename);
    // Prepare for player's answer
    edAnswer.Text := ''; edAnswer2.Text := '';
    btStart.Caption := 'Antwort';
  end
  // // Button "Antwort": Check the animal name entered by the player; update the score
  else if btStart.Caption = 'Antwort' then begin
    // Animal name entered is correct
    if cobAnimals.Text = AnimalName(aQuizAnimals, iQuizQuestion) then begin
      // Update score for player 1 OR player 2
      if edName1.Color = clAqua then
        Inc(iCorrect1)
      else
        Inc(iCorrect2);
      // "Lime" text for correct answer
      edAnswer.Text := 'Diese Antwort ist richtig!';
      edAnswer.Font.Color := clLime;
    end
    // Animal name entered is not correct
    else begin
      // "Red" text for false answer
      edAnswer.Text := 'Diese Antwort ist falsch!';
      edAnswer.Font.Color := clRed;
      // Dispaly of the correct answer
      edAnswer2.Text := 'Richtig: ' + AnimalName(aQuizAnimals, iQuizQuestion);
    end;
    // Update the scores
    Score1 := Round(100 * (iCorrect1 / iQuestion1));
    edScore1.Text := IntToStr(Score1) + ' %';
    if iPlayers = 2 then begin
      if iQuestion2 <> 0 then begin                                            // this is the case with the very first question
        Score2 := Round(100 * (iCorrect2 / iQuestion2));
        edScore2.Text := IntToStr(Score2) + ' %';
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
      // Animals left: Next button push will be for a new question
      btStart.Caption := 'Frage';
    end
    else begin
      // All animals done: Message, quiz button disable and re-enable settings
      MessageDlg('Ende dieses Quizes', 'Im Menü "Quiz" kann ein neuer Quiz gestartet werden', mtInformation, [mbOK], 0);
      mSettings.Enabled := True;                                               // re-enable "Einstellungen" menu (for new quiz)
      btStart.Caption := 'Start'; btStart.Enabled := False;
    end
  end
  // Button "Stop": During slideshow: Use "Stop" button to terminate it
  else if btStart.Caption = 'Stop' then begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Ende der Diashow', 'Die Diashow wurde vom Benutzer abgebrochen', mtInformation, [mbOK], 0);
    mSettingsPlayers.Enabled := True; mSettingsLevel.Enabled := True;
    stSlide.Visible := False; edSlide.Visible := False;                        // hide all slideshow related controls
    btStart.Enabled := False; btPause.Visible := False;
  end;
end;

{ Button "Pause/Weiter": pause resp. resume the slideshow }

procedure TfRQ.btPauseClick(Sender: TObject);

begin
  // Button "Pause": Pause the slideshow
  if btPause.Caption = 'Pause' then begin
    tiRQ.Enabled := False;                                                     // just disable the timer
    btPause.Caption := 'Weiter';
  end
  // Button "Weiter"; Resume the slideshow
  else begin
    btPause.Caption := 'Pause';
    tiRQ.Enabled := True;                                                      // re-enable the timer
  end;
end;

{ Slideshow timer routine }

procedure TfRQ.tiVQTimer(Sender: TObject);

var
  Filename: string;

begin
  Inc(iQuestion1);
  // Proceed if there are still animals to be displayed
  if iQuestion1 <= iQuestions then begin
    // Display title and animal's name
    stSlide.Caption := 'DIASHOW: Raubtier ' + IntToStr(iQuestion1) + ' / ' + IntToStr(iQuestions);
    edSlide.Text := AnimalName(aQuizAnimals, iQuestion1 - 1);
    // Display animal's image
    Filename := Trim(aQuizAnimals[iQuestion1 - 1].Name);
    Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imAnimal.Picture.LoadFromFile(Filename);
  end
  // Stop slideshow if all animals (for level selected) have been displayed
  else begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Ende der Diashow', 'Alle Raubtierbilder (dieser Gruppe) wurden angezeigt', mtInformation, [mbOK], 0);
    mSettingsPlayers.Enabled := True; mSettingsLevel.Enabled := True;
    stSlide.Visible := False; edSlide.Visible := False;                        // hide all slideshow related controls
    btStart.Enabled := False; btPause.Visible := False;
  end;
end;

end.

