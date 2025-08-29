{******************************************}
{* Main unit for CastlesQuiz4 application *}
{******************************************}

unit castles4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LazUTF8;

type
  TCastles  = array of record
    Country, Name, Name2: string;
  end;
  TBooleans = array of Boolean;
  {**********}
  { TfCQuiz4 }
  {**********}
  TfCQuiz4 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSlideshow, mFileExit: TMenuItem;
    mSettings, mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsCastles, mSettingsCastlesN, mSettingsCastlesE, mSettingsCastlesNE: TMenuItem;
    mSettingsCastlesS, mSettingsCastlesSNE: TMenuItem;
    MenuItem2, mSettingsCastlesSP, mSettingsCastlesI: TMenuItem;
    mSettingsAnswers, mSettingsAnswers5, mSettingsAnswers10, mSettingsAnswers15, mSettingsNoAnswer, mSettingsGerNames: TMenuItem;
    MenuItem1, mSettingsSlideshow, mSettingsSlideshow1, mSettingsSlideshow2, mSettingsSlideshow3, mSettingsSlideshow4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout, mHelpPhotos: TMenuItem;
    stQuiz: TStaticText;
    edName1, edName2, edScore1, edScore2: TEdit;
    imCastle: TImage;
    rbAnswer1, rbAnswer4, rbAnswer2, rbAnswer5, rbAnswer3, rbAnswer6: TRadioButton;
    cobAnswers: TComboBox;
    edAnswer, edAnswer2: TEdit;
    stSlide: TStaticText;
    edSlideCastle, edSlideCountry: TEdit;
    btStart: TButton;
    btPause: TButton;
    tiRQ: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSlideshowClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsCastlesNClick(Sender: TObject);
    procedure mSettingsCastlesEClick(Sender: TObject);
    procedure mSettingsCastlesNEClick(Sender: TObject);
    procedure mSettingsCastlesSClick(Sender: TObject);
    procedure mSettingsCastlesSNEClick(Sender: TObject);
    procedure mSettingsCastlesSPClick(Sender: TObject);
    procedure mSettingsCastlesIClick(Sender: TObject);
    procedure mSettingsAnswers5Click(Sender: TObject);
    procedure mSettingsAnswers10Click(Sender: TObject);
    procedure mSettingsAnswers15Click(Sender: TObject);
    procedure mSettingsNoAnswerClick(Sender: TObject);
    procedure mSettingsGerNamesClick(Sender: TObject);
    procedure mSettingsSlideshow1Click(Sender: TObject);
    procedure mSettingsSlideshow2Click(Sender: TObject);
    procedure mSettingsSlideshow3Click(Sender: TObject);
    procedure mSettingsSlideshow4Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpPhotosClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tiVQTimer(Sender: TObject);
  private
    iPlayers, iAnswers, iAnswersTemp, iQuestions, iQuestion1, iQuestion2, iCorrect1, iCorrect2, iQuizQuestion: Integer;
    sRegion, sRegionTemp, sCorrectAnswer: string;
    bNoAnswer, bNoAnswerTemp: Boolean;
    aCastlesNorth, aCastlesEast, aCastlesNorthEast, aCastlesSouth, aCastlesAll, aCastlesEsPt, aCastlesIt, aCastles: TCastles;
    aCastlesDone: TBooleans;
    rbAnswers: array[0..5] of TRadioButton;
  end;

var
  fCQuiz4: TfCQuiz4;

implementation

{$R *.lfm}

{ Read castles form castles.txt file }

procedure ReadCastles(out CastlesNorth, CastlesEast, CastlesNorthEast, CastlesSouth, CastlesAll, CastlesEsPt, CastlesIt: TCastles);

var
  North, East, NorthEast, South, All, EsPt, It: Integer;
  Line: string;
  Region: Char;
  CastleFile: Text;

begin
  SetLength(CastlesNorth, 15); SetLength(CastlesEast, 20);
  SetLength(CastlesNorthEast, 35); SetLength(CastlesSouth, 57);
  SetLength(CastlesAll, 92);
  SetLength(CastlesEsPt, 16); SetLength(CastlesIt, 39);
  Assign(CastleFile, 'castles.txt'); Reset(CastleFile);
  North := 0; East := 0; NorthEast := 0;
  South := 0; All  := 0;
  EsPt  := 0; It   := 0;
  while not EoF(CastleFile) do begin
    Readln(CastleFile, Line);
    if Line <> '' then begin
      // Fill "all castles" array
      Inc(All);
      CastlesAll[All - 1].Country := UTF8Trim(UTF8Copy(Line, 1, 15));
      CastlesAll[All - 1].Name := UTF8Trim(UTF8Copy(Line, 18, 40));
      CastlesAll[All - 1].Name2 := UTF8Trim(UTF8Copy(Line, 58, UTF8Length(Line)));
      if CastlesAll[All - 1].Name = '-' then
        CastlesAll[All - 1].Name := CastlesAll[All - 1].Name2;
      if CastlesAll[All - 1].Name2 = '-' then
        CastlesAll[All - 1].Name2 := CastlesAll[All - 1].Name;
      // Copy record of "all castles" array to regional array(s)
      Region := UTF8Copy(Line, 16, 1)[1];
      case Region of
        'N': begin
          Inc(North); Inc(NorthEast);
          CastlesNorth[North - 1] := CastlesAll[All - 1];
          CastlesNorthEast[NorthEast - 1] := CastlesAll[All - 1];
        end;
        'O': begin
          Inc(East); Inc(NorthEast);
          CastlesEast[East - 1] := CastlesAll[All - 1];
          CastlesNorthEast[NorthEast - 1] := CastlesAll[All - 1];
        end;
        'S': begin
          Inc(South);
          CastlesSouth[South - 1] := CastlesAll[All - 1];
        end;
      end;
      // Copy record of "all castles" array to country array
      if (CastlesAll[All - 1].Country = 'Spanien') or (CastlesAll[All - 1].Country = 'Portugal') then begin
        Inc(EsPt);
        CastlesEsPt[EsPt - 1] := CastlesAll[All - 1];
      end
      else if CastlesAll[All - 1].Country = 'Italien' then begin
        Inc(It);
        CastlesIt[It - 1] := CastlesAll[All - 1];
      end;
    end;
  end;
  Close(CastleFile);
end;

{ Display radiobutton labels or fill the castle names combobox with the answer selections for the actual question }

procedure DisplayAnswers(Answers: Integer; PicCastle1, PicCastle2: string; NoAnswer: Boolean; var Castles: TCastles; Out CorrectAnswer: string);

var
  I, J: Integer;
  Castle: string;
  OK: Boolean;
  CastleList: array of string;

begin
  if fCQuiz4.mSettingsGerNames.Checked then
    CorrectAnswer := PicCastle1
  else
    CorrectAnswer := PicCastle2;
  if fCQuiz4.mSettingsCastlesN.Checked and (Answers = 15) then begin
    // For North-Europe, there are only 15 - 1 = 14 wrong answers
    Answers := 14;
  end;
  SetLength(CastleList, Answers);
  // Fill castle names into array
  for I := 0 to Length(CastleList) - 1 do begin
    repeat
      OK := True;
      if fCQuiz4.mSettingsGerNames.Checked then
        Castle := Castles[Random(Length(Castles))].Name
      else
        Castle := Castles[Random(Length(Castles))].Name2;
      if (Castle = PicCastle1) or (Castle = PicCastle2) then begin
        // Only wrong answers (for now)
        OK := False;
      end
      else if I > 0 then begin
        // Eliminate doubles
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
    if fCQuiz4.mSettingsGerNames.Checked then
      CastleList[Random(Length(CastleList))] := PicCastle1
    else
      CastleList[Random(Length(CastleList))] := PicCastle2;
  end
  else begin
    // List will not contain any correct answer
    CorrectAnswer := 'Keine richtige Antwort';;
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
      fCQuiz4.rbAnswers[I].Caption := ' ' + CastleList[I];
      fCQuiz4.rbAnswers[I].Checked := False;
    end;
    if NoAnswer then begin
      // If "no correct answer" is selected, add a 6th radiobutton with this option
      fCQuiz4.rbAnswers[5].Caption := ' Keine richtige Antwort';
      fCQuiz4.rbAnswers[5].Checked := False;
    end;
  end
  else begin
    // If 10 or 15 answers, use combobox
    fCQuiz4.cobAnswers.Clear;
    if NoAnswer then begin
      // If "no correct answer" is selected, add an item with this option to combobox
      fCQuiz4.cobAnswers.Items.AddText('--Keine richtige Antwort--');
    end;
    for I := 0 to Length(CastleList) - 1 do
      // Add the 10 or 15 answer choices to the combobox
      fCQuiz4.cobAnswers.Items.AddText(CastleList[I]);
    fCQuiz4.cobAnswers.ItemIndex := 0;
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
    if fCQuiz4.rbAnswer6.Visible then
      Rb := 5                                                                  // "no correct answer" selected
    else
      Rb := 4;                                                                 // "no correct answer" unselected
    for I := 0 to Rb do begin
      // Get user answer from the radiobutton, that is actually checked
      if fCQuiz4.rbAnswers[I].Checked then begin
        Answer := fCQuiz4.rbAnswers[I].Caption;
        Delete(Answer, 1, 1);                                                  // remove space at beginning of the radiobutton label
      end;
    end;
  end
  else begin
    // If 10 or 15 answers, get user answer from combobox
    Answer := fCQuiz4.cobAnswers.Text;
    if LeftStr(Answer, 2) = '--' then
      Answer := UTF8Copy(Answer, 3, UTF8Length(Answer) - 4);                   // remove '--' at beginning and end of combobox item
  end;
  Result := Answer;
end;

{************}
{* TfCQuiz4 *}
{************}

{ Application start: Initialisation }

procedure TfCQuiz4.FormCreate(Sender: TObject);

begin
  // Create array with radiobuttons
  rbAnswers[0] := rbAnswer1; rbAnswers[1] := rbAnswer2; rbAnswers[2] := rbAnswer3;
  rbAnswers[3] := rbAnswer4; rbAnswers[4] := rbAnswer5; rbAnswers[5] := rbAnswer6;
  // Read castle lists from file
  ReadCastles(aCastlesNorth, aCastlesEast, aCastlesNorthEast, aCastlesSouth, aCastlesAll, aCastlesEsPt, aCastlesIt);
  // Initialize variables (startup parameters)
  iPlayers := 1; sRegionTemp := 'Süd-, Nord- und Osteuropa'; iAnswersTemp := 5; bNoAnswerTemp := True;
  // Start random number generator
  Randomize;
  // Prepare for a new quiz (with startup parameters)
  mFileNew.Click;
end;

{ Menu item "Datei > Neuer Quiz": Prepare for new quiz }

procedure TfCQuiz4.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  tiRQ.Enabled := False;
  // Settings selected become active now
  sRegion := sRegionTemp; iAnswers := iAnswersTemp; bNoAnswer := bNoAnswerTemp;
  // Choose castle list for selected region
  if sRegion = 'Nordeuropa' then
    aCastles := aCastlesNorth
  else if sRegion = 'Osteuropa' then
    aCastles := aCastlesEast
  else if sRegion = 'Nord- und Osteuropa' then
    aCastles := aCastlesNorthEast
  else if sRegion = 'Südeuropa' then
    aCastles := aCastlesSouth
  else if sRegion = 'Spanien und Portugal' then
    aCastles := aCastlesEsPt
  else if sRegion = 'Italien' then
    aCastles := aCastlesIt
  else
    aCastles := aCastlesAll;
  // Set quiz title
  stQuiz.Caption := 'Burgen und Schlösser-Quiz: ' + sRegion + '.';
  // Show and clear or hide radiobuttons resp. combobox
  if iAnswers = 5 then begin
    // If answers = 5, use radiobuttons
    for I := 0 to 5 do begin
      if (I < 5) or ((I = 5) and bNoAnswer) then begin                         // 6th radiobutton only if "no correct answer" is selected
        rbAnswers[I].Visible := True; rbAnswers[I].Checked := False;
        rbAnswers[I].Caption := ' Antwort ' + IntToStr(I + 1);
      end;
    end;
    cobAnswers.Visible := False;                                               // combobox not used, if 5 answers
  end
  else begin
    // If 10 or 15 answers, use combobox
    for I := 0 to 5 do begin
      rbAnswers[I].Visible := False;                                           // radiobuttons not used, if 10 or 15 answers
    end;
    cobAnswers.Visible := True; cobAnswers.Clear;
  end;
  // Reset form controls
  edScore1.Text := ''; edScore2.Text := '';
  edAnswer.Text := ''; edAnswer2.Text := '';
  stSlide.Caption := 'Quiz'; edSlideCastle.Text := ''; edSlideCountry.Text := '';
  imCastle.Picture.Clear;
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Visible := False;
end;

{ Menu item "Datei > Diashow": Start slideshow }

procedure TfCQuiz4.mFileSlideshowClick(Sender: TObject);

// The slideshow is actually performed within the timer routine

var
  I: Integer;

begin
  mFileNew.Click;
  iQuestions := Length(aCastles);                                              // images shown will be those corresponding to current region
  iQuestion1 := 0;
  stSlide.Caption := 'Diashow'; edSlideCastle.Text := ''; edSlideCountry.Text := '';
  // Hide non-slideshow-related form controls
  for I := 0 to 5 do
    rbAnswers[I].Visible := False;
  cobAnswers.Visible := False;
  // Show slideshow-related form controls
  edSlideCastle.Visible := True;
  // Adapt buttons
  btStart.Caption := 'Stop'; btStart.Enabled := True; btStart.SetFocus;        // use the quiz button as stop button for the slideshow
  btPause.Caption := 'Pause'; btPause.Visible := True;                         // use a second button (slideshow only) to pause/resume the slideshow
  tiRQ.Enabled := True;                                                        // enable the slideshow timer
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfCQuiz4.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Einstellungen > Anzahl der Spieler > ..": Select 1- or 2-player-quiz }

procedure TfCQuiz4.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;  mSettingsPlayers2.Checked := False;
  edName2.Enabled := False;  edScore2.Enabled := False;                        // disable not used Player 2 controls
  iPlayers := 1;
  mFileNew.Click;                                                              // automatically start a new quiz, if number of players changes
end;

procedure TfCQuiz4.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := True;  mSettingsPlayers1.Checked := False;
  edName2.Enabled := True;  edScore2.Enabled := True;
  iPlayers := 2;
  mFileNew.Click;
end;

{ Menu items "Einstellungen > Burgen/Schlösser Auswahl > ..": Select castles to be included into the quiz }

procedure TfCQuiz4.mSettingsCastlesNClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := True;  mSettingsCastlesE.Checked   := False; mSettingsCastlesNE.Checked := False;
  mSettingsCastlesS.Checked  := False; mSettingsCastlesSNE.Checked := False;
  mSettingsCastlesSP.Checked := False; mSettingsCastlesI.Checked   := False;
  sRegionTemp := 'Nordeuropa';
end;

procedure TfCQuiz4.mSettingsCastlesEClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := False; mSettingsCastlesE.Checked   := True;  mSettingsCastlesNE.Checked := False;
  mSettingsCastlesS.Checked  := False; mSettingsCastlesSNE.Checked := False;
  mSettingsCastlesSP.Checked := False; mSettingsCastlesI.Checked   := False;
  sRegionTemp := 'Osteuropa';
end;

procedure TfCQuiz4.mSettingsCastlesNEClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := False; mSettingsCastlesE.Checked   := False; mSettingsCastlesNE.Checked := True;
  mSettingsCastlesS.Checked  := False; mSettingsCastlesSNE.Checked := False;
  mSettingsCastlesSP.Checked := False; mSettingsCastlesI.Checked   := False;
  sRegionTemp := 'Nord- und Osteuropa';
end;

procedure TfCQuiz4.mSettingsCastlesSClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := False; mSettingsCastlesE.Checked   := False; mSettingsCastlesNE.Checked := False;
  mSettingsCastlesS.Checked  := True;  mSettingsCastlesSNE.Checked := False;
  mSettingsCastlesSP.Checked := False; mSettingsCastlesI.Checked   := False;
  sRegionTemp := 'Südeuropa';
end;

procedure TfCQuiz4.mSettingsCastlesSNEClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := False; mSettingsCastlesE.Checked   := False; mSettingsCastlesNE.Checked := False;
  mSettingsCastlesS.Checked  := False; mSettingsCastlesSNE.Checked := True;
  mSettingsCastlesSP.Checked := False; mSettingsCastlesI.Checked   := False;
  sRegionTemp := 'Süd-, Nord- und Osteuropa';
end;

procedure TfCQuiz4.mSettingsCastlesSPClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := False; mSettingsCastlesE.Checked   := False; mSettingsCastlesNE.Checked := False;
  mSettingsCastlesS.Checked  := False; mSettingsCastlesSNE.Checked := False;
  mSettingsCastlesSP.Checked := True;  mSettingsCastlesI.Checked   := False;
  sRegionTemp := 'Spanien und Portugal';
end;

procedure TfCQuiz4.mSettingsCastlesIClick(Sender: TObject);

begin
  mSettingsCastlesN.Checked  := False; mSettingsCastlesE.Checked   := False; mSettingsCastlesNE.Checked := False;
  mSettingsCastlesS.Checked  := False; mSettingsCastlesSNE.Checked := False;
  mSettingsCastlesSP.Checked := False; mSettingsCastlesI.Checked   := True;
  sRegionTemp := 'Italien';
end;

{ Menu items "Einstellungen > Antwortvorschläge > ..": Select number of answer choices to be displayed }

procedure TfCQuiz4.mSettingsAnswers5Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := True; mSettingsAnswers10.Checked := False; mSettingsAnswers15.Checked := False;
  iAnswersTemp := 5;
end;

procedure TfCQuiz4.mSettingsAnswers10Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := True; mSettingsAnswers15.Checked := False;
  iAnswersTemp := 10;
end;

procedure TfCQuiz4.mSettingsAnswers15Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := False; mSettingsAnswers15.Checked := True;
  iAnswersTemp := 15;
end;

{ Menu item "Einstellungen > 'Keine richtige Antwort' aktivieren": Toggle 'no correct answer' enable/disable }

procedure TfCQuiz4.mSettingsNoAnswerClick(Sender: TObject);

begin
  if mSettingsNoAnswer.Checked then
    mSettingsNoAnswer.Checked := False
  else
    mSettingsNoAnswer.Checked := True;
  bNoAnswerTemp := mSettingsNoAnswer.Checked;
  rbAnswer6.Visible := bNoAnswerTemp;                                          // Hide 6th radiobutton, if 'no correct answer' is unselected
end;

{ Menu item "Einstellungen > Deutsche Namen bevorzugen": Toggle between German and international castle names }

procedure TfCQuiz4.mSettingsGerNamesClick(Sender: TObject);

begin
  if mSettingsGerNames.Checked then
    mSettingsGerNames.Checked := False
  else
    mSettingsGerNames.Checked := True;
end;

{ Menu items "Einstellungen > Diashow Zeitintervall > ...": Select slideshow timer interval }

procedure TfCQuiz4.mSettingsSlideshow1Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := True;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False; mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1000;
end;

procedure TfCQuiz4.mSettingsSlideshow2Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := True;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1500;
end;

procedure TfCQuiz4.mSettingsSlideshow3Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := True;   mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 2000;
end;

procedure TfCQuiz4.mSettingsSlideshow4Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := True;
  tiRQ.Interval := 3000;
end;

{ Menu item "Hilfe > Hilfe": Display (very short) application help text }

procedure TfCQuiz4.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Die Anzahl der Spieler, eine der Burgen/Schlösser-Listen, die Anzahl der vorgeschlagenen Antworten ';
  S += 'und ob der Vorschlag "Keine richtige Antwort" benutzt werden soll oder nicht, auswählen. Wobei zu ';
  S += 'beachten ist, dass die Auswahl erst wirksam wird, wenn ein neuer Quiz im Menu "Datei" gestarted wird. ';
  S += 'Mit "Start/Frage" wird ein neues Foto angezeigt, mit "Antwort" wird die vom Benutzer ausgewählte Antwort ';
  S += 'validiert.';
  MessageDlg('Hilfe "CastlesQuiz4"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Daten/Fotos": Display information concerning the origin of castle data and photos }

procedure TfCQuiz4.mHelpPhotosClick(Sender: TObject);

var
  S: string;

begin
  S := 'Herkunft der Daten und Fotos:' + LineEnding;
  S += 'Die in "CastlesQuiz4" enthaltenen Burgen und Schlösser sind diejenigen, die auf der Webseite https://www.burgen.de/ ';
  S += 'aufgelistet sind. Die große Mehrzahl der Fotos ist ebenfalls dieser Seite entnommen.';
  MessageDlg('Info "CastlesQuiz4"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Über": Display application about text }

procedure TfCQuiz4.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Fotoquiz (plus Diashow) über die Burgen und Schlösser in Süd-, Nord- und Osteuropa.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Oktober 2021.';
  MessageDlg('Über "CastlesQuiz4"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Frage/Antwort/Stop": Quiz question/answer resp. stop of slideshow }

procedure TfCQuiz4.btStartClick(Sender: TObject);

var
  Score1, Score2, I: Integer;
  Filename, UserAnswer: string;

begin
  // Button "Start/Frage": Generation of a new quiz question
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Frage') then begin
    // Some initialisations to do if it is the beginning of a new quiz
    // Note: The slideshow is automatically started (The "Start" button is always for the quiz)
    if btStart.Caption = 'Start' then begin
      SetLength(aCastlesDone, Length(aCastles));
      for I := 0 to Length(aCastlesDone) - 1 do
        aCastlesDone[I] := False;                                              // reset all castles to "not done"!
      iQuestions := Length(aCastles);                                          // the number of questions (castles) for current region
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
    if not FileExists(Filename) then begin
      Filename := aCastles[iQuizQuestion].Name2;
      Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    end;
    imCastle.Picture.LoadFromFile(Filename);
    stSlide.Caption := 'Burg/Schloss ' + IntToStr(iQuestion1 + iQuestion2) + '/' + IntToStr(iQuestions);
    // Display answers (5, 10 or 15 proposals, to select answer from)
    DisplayAnswers(iAnswers, aCastles[iQuizQuestion].Name, aCastles[iQuizQuestion].Name2, bNoAnswer, aCastles, sCorrectAnswer);
    // Prepare for player's answer
    edAnswer.Text := ''; edAnswer2.Text := '';
    btStart.Caption := 'Antwort';
  end
  // Button "Antwort": Check the castle name entered by the player; update the score
  else if btStart.Caption = 'Antwort' then begin
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
      edAnswer.Text := 'Diese Antwort ist richtig!';
      edAnswer.Font.Color := clLime;
    end
    // Castle name entered is not correct
    else begin
      // Red font text for false answer
      edAnswer.Text := 'Diese Antwort ist falsch!';
      edAnswer.Font.Color := clRed;
      edAnswer2.Text := 'Richtig: ' + sCorrectAnswer;                          // dispaly correct answer
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
      btStart.Caption := 'Frage';
    end
    else begin
      // All questions (castles) done: End of quiz message and quiz button disable
      MessageDlg('Ende des Quizes', 'Alle Burgen und Schlösser sind abgefragt. Quiz beendet.', mtInformation, [mbOK], 0);
      btStart.Caption := 'Start'; btStart.Enabled := False;
    end
  end
  // Button "Stop": During slideshow: Use "Stop" button to terminate it
  else if btStart.Caption = 'Stop' then begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Ende der Diashow', 'Die Diashow wurde vom Benutzer abgebrochen.', mtInformation, [mbOK], 0);
    btStart.Enabled := False; btPause.Visible := False;
  end;
end;

{ Button "Pause/Weiter": Pause resp. resume the slideshow }

procedure TfCQuiz4.btPauseClick(Sender: TObject);

begin
  // Button "Pause": Pause the slideshow
  if btPause.Caption = 'Pause' then begin
    tiRQ.Enabled := False;                                                     // disable the timer
    btPause.Caption := 'Weiter';
  end
  // Button "Weiter": Resume the slideshow
  else begin
    btPause.Caption := 'Pause';
    tiRQ.Enabled := True;                                                      // re-enable the timer
  end;
end;

{ Slideshow timer routine }

procedure TfCQuiz4.tiVQTimer(Sender: TObject);

var
  Filename: string;

begin
  Inc(iQuestion1);
  // Proceed if there are still castles to be displayed
  if iQuestion1 <= iQuestions then begin
    // Display castle's name
    stSlide.Caption := 'Burg/Schloss ' + IntToStr(iQuestion1) + '/' + IntToStr(iQuestions);
    if mSettingsGerNames.Checked then
      edSlideCastle.Text := aCastles[iQuestion1 - 1].Name
    else
      edSlideCastle.Text := aCastles[iQuestion1 - 1].Name2;
    // Display country
    edSlideCountry.Text := aCastles[iQuestion1 - 1].Country;
    // Display castle's image
    Filename := aCastles[iQuestion1 - 1].Name;
    Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    if not FileExists(Filename) then begin
      Filename := aCastles[iQuestion1 - 1].Name2;
      Filename := './pics/' + Filename + '.jpg'; DoDirSeparators(Filename);
    end;
    if not FileExists(Filename) then
      // This should never happen (is useful during testing...)
      MessageDlg('Datei error', 'Foto nicht gefunden für ' + aCastles[iQuestion1 - 1].Name, mtInformation, [mbOK], 0);
    imCastle.Picture.LoadFromFile(Filename);
  end
  // Stop slideshow if all castles (for region selected) have been displayed
  else begin
    tiRQ.Enabled := False;                                                     // stop the timer
    MessageDlg('Ende der Diashow', 'Alle Burgen und Schlösser wurden gezeigt. Diashow beendet.', mtInformation, [mbOK], 0);
    btStart.Enabled := False;
    btPause.Visible := False;
  end;
end;

end.

