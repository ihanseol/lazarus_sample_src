{******************************************}
{* Main unit for CastlesQuiz1 application *}
{******************************************}

unit castles1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LazUTF8;

type
  TCastles  = array of record
    Country, Name: string;
  end;
  TBooleans = array of Boolean;
  {**********}
  { TfCQuiz1 }
  {**********}
  TfCQuiz1 = class(TForm)
    MenuItem1: TMenuItem;
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSlideshow, mFileExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsRegion, mSettingsRegionLU, mSettingsRegionBNL, mSettingsRegionF1, mSettingsRegionF2, mSettingsRegionAll: TMenuItem;
    mSettingsAnswers, mSettingsAnswers5, mSettingsAnswers10, mSettingsAnswers20, mOptionsNoAnswer: TMenuItem;
    mSettingsSlideshow, mSettingsSlideshow1, mSettingsSlideshow2, mSettingsSlideshow3, mSettingsSlideshow4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout, mHelpPhotos: TMenuItem;
    stQuiz: TStaticText;
    edName1, edName2: TEdit;
    edScore1, edScore2: TEdit;
    imCastle: TImage;
    rbAnswer1, rbAnswer4, rbAnswer2, rbAnswer5, rbAnswer3, rbAnswer6: TRadioButton;
    cobAnswers: TComboBox;
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
    procedure mSettingsRegionLUClick(Sender: TObject);
    procedure mSettingsRegionBNLClick(Sender: TObject);
    procedure mSettingsRegionF1Click(Sender: TObject);
    procedure mSettingsRegionF2Click(Sender: TObject);
    procedure mSettingsRegionAllClick(Sender: TObject);
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
    aCastlesLU, aCastlesBNL, aCastlesF1, aCastlesF2, aCastlesAll, aCastles: TCastles;
    aCastlesDone: TBooleans;
    rbAnswers: array[0..5] of TRadioButton;
  end;

var
  fCQuiz1: TfCQuiz1;

implementation

{$R *.lfm}

{ Read castles form castles.txt file }

procedure ReadCastles(out CastlesLU, CastlesBNL, CastlesF1, CastlesF2, CastlesAll: TCastles);

var
  LU, BNL, F1, F2, N: Integer;
  Line: string;
  CastleFile: Text;

begin
  SetLength(CastlesLU, 40); SetLength(CastlesBNL, 50);
  SetLength(CastlesF1, 50); SetLength(CastlesF2, 106); SetLength(CastlesAll, 163);
  Assign(CastleFile, 'castles.txt'); Reset(CastleFile);
  LU := 0; BNL := 0; F1 := 0; F2 := 0; N := 0;
  while not EoF(CastleFile) do begin
    Readln(CastleFile, Line);
    if Line <> '' then begin
      Inc(N);
      CastlesAll[N - 1].Name := UTF8Trim(UTF8Copy(Line, 7, UTF8Length(Line)));
      if LeftStr(Line, 3) = 'LUX' then begin
        // Luxembourgish castles
        CastlesAll[N - 1].Country := 'L';
        Inc(LU);
        CastlesLU[LU - 1] := CastlesAll[N - 1];
        if Copy(Line, 5, 1) = 'B' then begin
          // Luxembourgish castles, used in Benelux list
          Inc(BNL);
          CastlesBNL[BNL - 1] := CastlesAll[N - 1];
        end;
      end
      else if (LeftStr(Line, 3) = 'BEL') or (LeftStr(Line, 3) = 'NLD') then begin
        // Belgian and Dutch castles: Benelux list
        if LeftStr(Line, 3) = 'BEL' then
          CastlesAll[N - 1].Country := 'B'
        else
          CastlesAll[N - 1].Country := 'NL';
        Inc(BNL);
        CastlesBNL[BNL - 1] := CastlesAll[N - 1];
      end
      else if LeftStr(Line, 3) = 'FRA' then begin
        // French castles
        CastlesAll[N - 1].Country := 'F';
        Inc(F2);
        CastlesF2[F2 - 1] := CastlesAll[N - 1];
        if Copy(Line, 5, 1) = '1' then begin
          // French castles, used in France I list
          Inc(F1);
          CastlesF1[F1 - 1] := CastlesAll[N - 1];
        end;
      end;
    end;
  end;
  Close(CastleFile);
end;

{ Display radiobuttons labels or fill the castles names combobox with the answer selections for the actual question }

procedure DisplayAnswers(Answers: Integer; PicCastle: string; NoAnswer: Boolean; var Castles: TCastles; Out CorrectAnswer: string);

var
  I, J: Integer;
  Castle: string;
  OK: Boolean;
  CastleList: array of string;

begin
  CorrectAnswer := PicCastle;                                                  // correct answer may be 'no correct answer'
  SetLength(CastleList, Answers);
  // Fill castle names into array
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
    CorrectAnswer := 'Aucune réponse correcte';;
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
      fCQuiz1.rbAnswers[I].Caption := ' ' + CastleList[I];
      fCQuiz1.rbAnswers[I].Checked := False;
    end;
    if NoAnswer then begin
      // If "no correct answer" is selected, add a 6th radiobutton with this option
      fCQuiz1.rbAnswers[5].Caption := ' Aucune réponse correcte';
      fCQuiz1.rbAnswers[5].Checked := False;
    end;
  end
  else begin
    // If 10 or 20 answers, use combobox
    fCQuiz1.cobAnswers.Clear;
    if NoAnswer then begin
      // If "no correct answer" is selected, add an item with this option to combobox
      fCQuiz1.cobAnswers.Items.AddText('--Aucune réponse correcte--');
    end;
    for I := 0 to Length(CastleList) - 1 do
      // Add the 10 or 20 answer choices to the combobox
      fCQuiz1.cobAnswers.Items.AddText(CastleList[I]);
    fCQuiz1.cobAnswers.ItemIndex := 0;
  end;
end;

{ Get user answer (castle selection) from the form }

function GetUserAnswer(Answers: Integer): string;

var
  Rb, I: Integer;
  Answer: string;

begin
  Answer := '';
  if Answers = 5 then begin
    // If 5 answers, get user answer from radiobuttons
    if fCQuiz1.rbAnswer6.Visible then
      Rb := 5                                                                  // "no correct answer" selected
    else
      Rb := 4;                                                                 // "no correct answer" unselected
    for I := 0 to Rb do begin
      // Get user answer from the radiobutton, that is actually checked
      if fCQuiz1.rbAnswers[I].Checked then begin
        Answer := fCQuiz1.rbAnswers[I].Caption;
        Delete(Answer, 1, 1);                                                  // removes the space at the beginning of the radiobutton label
      end;
    end;
  end
  else begin
    // If 10 or 20 answers, get user answer from combobox
    Answer := fCQuiz1.cobAnswers.Text;
    if LeftStr(Answer, 2) = '--' then
      Answer := UTF8Copy(Answer, 3, UTF8Length(Answer) - 4);                   // remove the '--' at the beginning and end of combobox item
  end;
  Result := Answer;
end;

{************}
{* TfCQuiz1 *}
{************}

{ Application start: Initialisation }

procedure TfCQuiz1.FormCreate(Sender: TObject);

begin
  // Create array with radiobuttons
  rbAnswers[0] := rbAnswer1; rbAnswers[1] := rbAnswer2; rbAnswers[2] := rbAnswer3;
  rbAnswers[3] := rbAnswer4; rbAnswers[4] := rbAnswer5; rbAnswers[5] := rbAnswer6;
  // Read castle lists from file
  ReadCastles(aCastlesLU, aCastlesBNL, aCastlesF1, aCastlesF2, aCastlesAll);
  // Initialize variables (startup parameters)
  iPlayers := 1; sRegionTemp := 'Luxembourg'; iAnswersTemp := 5; bNoAnswerTemp := True;
  // Start random number generator
  Randomize;
  // Prepare for a new quiz (with startup parameters)
  mFileNew.Click;
end;

{ Menu item "Fichier > Nouveau quiz": Prepare for new quiz }

procedure TfCQuiz1.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  tiRQ.Enabled := False;
  sRegion := sRegionTemp; iAnswers := iAnswersTemp; bNoAnswer := bNoAnswerTemp;  // Settings selected become active now
  // Set quiz title and choose country/region dependent castle list
  stQuiz.Caption := 'QUIZ: Châteaux';
  if sRegion = 'Luxembourg' then begin
    stQuiz.Caption := stQuiz.Caption + ' à Luxembourg.';
    aCastles := aCastlesLU;
  end
  else if sRegion = 'Benelux' then begin
    stQuiz.Caption := stQuiz.Caption + ' au Benelux.';
    aCastles := aCastlesBNL;
  end
  else if sRegion = 'France I' then begin
    stQuiz.Caption := stQuiz.Caption + ' en France (I).';
    aCastles := aCastlesF1;
  end
  else if sRegion = 'France II' then begin
    stQuiz.Caption := stQuiz.Caption + ' en France (II).';
    aCastles := aCastlesF2;
  end
  else if sRegion = 'Liste complète' then begin
    stQuiz.Caption := stQuiz.Caption + ' en Europe de l''ouest.';
    aCastles := aCastlesAll;
  end;
  // Show and clear or hide radiobuttons resp. combobox
  if iAnswers = 5 then begin
    // If answers = 5, use radiobuttons
    for I := 0 to 5 do begin
      if (I < 5) or ((I = 5) and bNoAnswer) then begin                         // 6th radiobutton only if "no correct answer" is selected
        rbAnswers[I].Visible := True; rbAnswers[I].Checked := False;
        rbAnswers[I].Caption := ' Réponse ' + IntToStr(I + 1);
      end;
    end;
    cobAnswers.Visible := False;                                               // combobox not used, if 5 answers
  end
  else begin
    // If 10 or 20 answers, use combobox
    for I := 0 to 5 do begin
      rbAnswers[I].Visible := False;                                           // radiobuttons not used, if 10 or 20 answers
    end;
    cobAnswers.Visible := True; cobAnswers.Clear;
  end;
  // Clear form controls
  edScore1.Text := ''; edScore2.Text := '';
  edAnswer.Text := ''; edAnswer2.Text := '';
  stSlide.Caption := 'Quiz'; edSlide.Text := '';
  imCastle.Picture.Clear;
  btStart.Caption := 'Départ'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Visible := False;
end;

{ Menu item "Fichier > Diaporama": Start slideshow }

procedure TfCQuiz1.mFileSlideshowClick(Sender: TObject);

// The slideshow is actually performed within the timer routine

var
  I: Integer;

begin
  mFileNew.Click;                                                              // just done to clear the quiz data on the form
  iQuestions := Length(aCastles);                                              // images shown will be those corresponding to current country/region
  iQuestion1 := 0;
  stSlide.Caption := 'Diaporama'; edSlide.Text := '';
  // Hide non-slideshow-related form controls
  for I := 0 to 5 do
    rbAnswers[I].Visible := False;
  cobAnswers.Visible := False;
  // Show slideshow-related form controls
  edSlide.Visible := True;
  // Adapt buttons
  btStart.Caption := 'Stop'; btStart.Enabled := True; btStart.SetFocus;        // use the quiz button as stop button for the slideshow
  btPause.Caption := 'Pause'; btPause.Visible := True;                         // use a second button (slideshow only) to pause/resume the slideshow
  tiRQ.Enabled := True;                                                        // enable the slideshow timer}
end;

{ Menu item "Fichier > Quitter": Exit the application }

procedure TfCQuiz1.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Nombre de joueurs > ..": Select 1- or 2-player-quiz }

procedure TfCQuiz1.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;  mSettingsPlayers2.Checked := False;
  edName2.Enabled := False;  edScore2.Enabled := False;                        // disable not used Player 2 controls
  iPlayers := 1;
  mFileNew.Click;                                                              // automatically start a new quiz, if number of players changes
end;

procedure TfCQuiz1.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := True;  mSettingsPlayers1.Checked := False;
  edName2.Enabled := True;  edScore2.Enabled := True;
  iPlayers := 2;
  mFileNew.Click;
end;

{ Menu items "Options > Pays/Région > ..": Select castles country/region }

procedure TfCQuiz1.mSettingsRegionLUClick(Sender: TObject);

begin
  mSettingsRegionLU.Checked := True; mSettingsRegionBNL.Checked := False;
  mSettingsRegionF1.Checked := False; mSettingsRegionF2.Checked := False; mSettingsRegionAll.Checked := False;
  sRegionTemp := 'Luxembourg';
end;

procedure TfCQuiz1.mSettingsRegionBNLClick(Sender: TObject);

begin
  mSettingsRegionLU.Checked := False; mSettingsRegionBNL.Checked := True;
  mSettingsRegionF1.Checked := False; mSettingsRegionF2.Checked := False; mSettingsRegionAll.Checked := False;
  sRegionTemp := 'Benelux';
end;

procedure TfCQuiz1.mSettingsRegionF1Click(Sender: TObject);

begin
  mSettingsRegionLU.Checked := False; mSettingsRegionBNL.Checked := False;
  mSettingsRegionF1.Checked := True; mSettingsRegionF2.Checked := False; mSettingsRegionAll.Checked := False;
  sRegionTemp := 'France I';
end;

procedure TfCQuiz1.mSettingsRegionF2Click(Sender: TObject);

begin
  mSettingsRegionLU.Checked := False; mSettingsRegionBNL.Checked := False;
  mSettingsRegionF1.Checked := False; mSettingsRegionF2.Checked := True; mSettingsRegionAll.Checked := False;
  sRegionTemp := 'France II';
end;

procedure TfCQuiz1.mSettingsRegionAllClick(Sender: TObject);

begin
  mSettingsRegionLU.Checked := False; mSettingsRegionBNL.Checked := False;
  mSettingsRegionF1.Checked := False; mSettingsRegionF2.Checked := False; mSettingsRegionAll.Checked := True;
  sRegionTemp := 'Liste complète';
end;

{ Menu items "Options > Réponses proposées > ..": Select number of answer choices to be displayed }

procedure TfCQuiz1.mSettingsAnswers5Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := True; mSettingsAnswers10.Checked := False; mSettingsAnswers20.Checked := False;
  iAnswersTemp := 5;
end;

procedure TfCQuiz1.mSettingsAnswers10Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := True; mSettingsAnswers20.Checked := False;
  iAnswersTemp := 10;
end;

procedure TfCQuiz1.mSettingsAnswers20Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := False; mSettingsAnswers20.Checked := True;
  iAnswersTemp := 20;
end;

{ Menu item "Options > Activer '0 réponse correcte'": Toggle 'no correct answer' enable/disable }

procedure TfCQuiz1.mOptionsNoAnswerClick(Sender: TObject);

begin
  if mOptionsNoAnswer.Checked then
    mOptionsNoAnswer.Checked := False
  else
    mOptionsNoAnswer.Checked := True;
  bNoAnswerTemp := mOptionsNoAnswer.Checked;
  rbAnswer6.Visible := bNoAnswerTemp;                                          // Hide 6th radiobutton, if 'no correct answer' is unselected
end;

{ Menu items "Options > Interval diaporama > ...": Select slideshow timer interval }

procedure TfCQuiz1.mSettingsSlideshow1Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := True;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False; mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1000;
end;

procedure TfCQuiz1.mSettingsSlideshow2Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := True;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1500;
end;

procedure TfCQuiz1.mSettingsSlideshow3Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := True;   mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 2000;
end;

procedure TfCQuiz1.mSettingsSlideshow4Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := True;
  tiRQ.Interval := 3000;
end;

{ Menu item "Aide > Aide programme": Display (very short) application help text }

procedure TfCQuiz1.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Sélectionnez le nombre de joueurs, un pays/une région, le nombre de réponses qui seront proposées ';
  S += 'et si la proposition "Aucune réponse correcte" sera ou non utilisée. Notez que vos sélections ne ';
  S += 'seront actives que lorsque vous choisissez "Nouveau quiz" dans le menu "Fichier". Appuyez sur ';
  S += '"Démarrer"/"Question" pour afficher une nouvelle photo, appuyez sur "Réponse" pour sélectionner ';
  S += 'votre réponse parmi les propositions affichées.';
  MessageDlg('Aide "CastlesQuiz1"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Aide > Info programme": Display application about text }

procedure TfCQuiz1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Quiz photos (plus diaporama), concernant les châteaux et forteresses de France, ';
  S += 'du Grand-Duché de Luxembourg, de Belgique et des Pays-Bas.' + LineEnding + LineEnding;
  S += 'Version 1.0.1, © allu, Mai-Juin 2021.';
  MessageDlg('Info "CastlesQuiz1"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Aide > Info données/photos": Display information concerning the origin of castle data and photos }

procedure TfCQuiz1.mHelpPhotosClick(Sender: TObject);

var
  S: string;

begin
  S := 'Origine des données et photos:' + LineEnding;
  S += 'À part pour le GD de Luxembourg, "CastlesQuiz1" prend en compte les châteaux/forteresses listées ';
  S += 'sur le site Web https://www.burgen.de/. La plupart des photos proviennent également de ce site.' + LineEnding + LineEnding;
  MessageDlg('Info "CastlesQuiz1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Départ/Question/Réponse/Arrêt": Quiz question/answer resp. stop of slideshow }

procedure TfCQuiz1.btStartClick(Sender: TObject);

var
  Score1, Score2, I: Integer;
  Filename, UserAnswer: string;

begin
  // Button "Départ/Question": Generation of a new quiz question
  if (btStart.Caption = 'Départ') or (btStart.Caption = 'Question') then begin
    // Some initialisations to do if it is the beginning of a new quiz
    // Note: The slideshow is automatically started ("Départ" is always for the quiz)
    if btStart.Caption = 'Départ' then begin
      SetLength(aCastlesDone, Length(aCastles));
      for I := 0 to Length(aCastlesDone) - 1 do
        aCastlesDone[I] := False;                                              // reset all castles to "not done"!
      iQuestions := Length(aCastles);                                          // the number of questions (castles) for current country/region
      iQuestion1 := 0; iQuestion2 := 0;                                        // questions asked Players 1 and 2
      iCorrect1 := 0; iCorrect2 := 0;                                          // correct answers Players 1 and 2
      Score1 := 0; edScore1.Text := '';                                        // score percentages Players 1 and 2
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
    // Find a random castle, that has not yet been shown during this quiz
    repeat
      iQuizQuestion := Random(iQuestions);
    until not aCastlesDone[iQuizQuestion];
    aCastlesDone[iQuizQuestion] := True;                                       // mark this castle as done
    // Display the castle photo
    Filename := aCastles[iQuizQuestion].Name;
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imCastle.Picture.LoadFromFile(Filename);
    stSlide.Caption := 'Château/Forteresse ' + IntToStr(iQuestion1 + iQuestion2) + '/' + IntToStr(iQuestions);
    // Display answers (5, 10 or 20 proposals, to select answer from)
    DisplayAnswers(iAnswers, aCastles[iQuizQuestion].Name, bNoAnswer, aCastles, sCorrectAnswer);
    // Prepare for player's answer
    edAnswer.Text := ''; edAnswer2.Text := '';
    btStart.Caption := 'Réponse';
  end
  // Button "Réponse": Check the castle name entered by the player; update the score
  else if btStart.Caption = 'Réponse' then begin
    // Get user answer from form (radiobuttons or combobox)
    UserAnswer := GetUserAnswer(iAnswers);
    // Castle name entered is correct
    if UserAnswer = sCorrectAnswer then begin
      // Update score for player 1 OR player 2
      if edName1.Color = clAqua then
        Inc(iCorrect1)
      else
        Inc(iCorrect2);
      // Green font text for correct answer
      edAnswer.Text := 'Cette réponse est correcte!';
      edAnswer.Font.Color := clLime;
    end
    // Castle name entered is not correct
    else begin
      // Red font text for false answer
      edAnswer.Text := 'Cette réponse est fausse!';
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
      MessageDlg('Fin du quiz', 'Tous les châteaux et forteresses ont été demandés. Quiz terminé.', mtInformation, [mbOK], 0);
      btStart.Caption := 'Départ'; btStart.Enabled := False;
    end
  end
  // Button "Stop": During slideshow: Use "Stop" button to terminate it
  else if btStart.Caption = 'Stop' then begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Fin du diaporama', 'Le diaporama a été terminé par l''utilisatueur', mtInformation, [mbOK], 0);
    btStart.Enabled := False; btPause.Visible := False;
  end;
end;

{ Button "Pause/Reprise": Pause resp. resume the slideshow }

procedure TfCQuiz1.btPauseClick(Sender: TObject);

begin
  // Button "Pause": Pause the slideshow
  if btPause.Caption = 'Pause' then begin
    tiRQ.Enabled := False;                                                     // disable the timer
    btPause.Caption := 'Reprise';
  end
  // Button "Reprise": Resume the slideshow
  else begin
    btPause.Caption := 'Pause';
    tiRQ.Enabled := True;                                                      // re-enable the timer
  end;
end;

{ Slideshow timer routine }

procedure TfCQuiz1.tiVQTimer(Sender: TObject);

var
  Filename: string;

begin
  Inc(iQuestion1);
  // Proceed if there are still castles to be displayed
  if iQuestion1 <= iQuestions then begin
    // Display castle's name
    stSlide.Caption := 'Château/Forteresse ' + IntToStr(iQuestion1) + '/' + IntToStr(iQuestions);
    edSlide.Text := aCastles[iQuestion1 - 1].Name;
    if (sRegion = 'Benelux') or (sRegion = 'Liste complète') then begin
      // Add castle's country code
      edSlide.Text := edSlide.Text + ' (' + aCastles[iQuestion1 - 1].Country + ')';
    end;
    // Display castle's image
    Filename := aCastles[iQuestion1 - 1].Name;
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    if not FileExists(Filename) then
      // This should never happen (is useful during testing...)
      MessageDlg('Datei error', 'Foto nicht gefunden für ' + aCastles[iQuestion1 - 1].Name, mtInformation, [mbOK], 0);
    imCastle.Picture.LoadFromFile(Filename);
  end
  // Stop slideshow if all castles (for country/region selected) have been displayed
  else begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Fin du diaporama', 'Tous les châteaux et forteresses ont été montrés. Diaporama terminé.', mtInformation, [mbOK], 0);
    btStart.Enabled := False;
    btPause.Visible := False;
  end;
end;

end.

