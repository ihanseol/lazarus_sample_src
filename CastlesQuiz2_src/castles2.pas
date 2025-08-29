{******************************************}
{* Main unit for CastlesQuiz2 application *}
{******************************************}

unit castles2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LazUTF8;

type
  TCastles  = array of record
    Country, Land, Name: string;
  end;
  TBooleans = array of Boolean;
  {**********}
  { TfCQuiz2 }
  {**********}
  TfCQuiz2 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSlideshow, mFileExit: TMenuItem;
    mSettings, mSettingsPlayers, mSettingsPlayers1, mSettingsPlayers2: TMenuItem;
    mSettingsCastles, mSettingsCastlesGermany: TMenuItem;
    mSettingsCastlesGermany1, mSettingsCastlesGermany2, mSettingsCastlesGermany3, mSettingsCastlesGermany4: TMenuItem;
    mSettingsCastlesGermanyN, mSettingsCastlesGermanyS, mSettingsCastlesAutChe: TMenuItem;
    mSettingsCastlesGermanyAll, mSettingsCastlesAll: TMenuItem;
    mSettingsAnswers, mSettingsAnswers5, mSettingsAnswers10, mSettingsAnswers20, mOptionsNoAnswer, MenuItem1: TMenuItem;
    mSettingsSlideshow, mSettingsSlideshow1, mSettingsSlideshow2, mSettingsSlideshow3, mSettingsSlideshow4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout, mHelpPhotos: TMenuItem;
    stQuiz: TStaticText;
    edName1, edName2, edScore1, edScore2: TEdit;
    imCastle: TImage;
    rbAnswer1, rbAnswer4, rbAnswer2, rbAnswer5, rbAnswer3, rbAnswer6: TRadioButton;
    cobAnswers: TComboBox;
    edAnswer, edAnswer2: TEdit;
    stSlide: TStaticText;
    edSlideCastle, edSlideRegion: TEdit;
    btStart: TButton;
    btPause: TButton;
    tiRQ: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSlideshowClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsPlayers1Click(Sender: TObject);
    procedure mSettingsPlayers2Click(Sender: TObject);
    procedure mSettingsCastlesGermany1Click(Sender: TObject);
    procedure mSettingsCastlesGermany2Click(Sender: TObject);
    procedure mSettingsCastlesGermany3Click(Sender: TObject);
    procedure mSettingsCastlesGermany4Click(Sender: TObject);
    procedure mSettingsCastlesGermanyNClick(Sender: TObject);
    procedure mSettingsCastlesGermanySClick(Sender: TObject);
    procedure mSettingsCastlesAutCheClick(Sender: TObject);
    procedure mSettingsCastlesGermanyAllClick(Sender: TObject);
    procedure mSettingsCastlesAllClick(Sender: TObject);
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
    aCastlesGer1, aCastlesGer2, aCastlesGer3, aCastlesGer4, aCastlesGerN, aCastlesGerS: TCastles;
    aCastlesAutChe, aCastlesGerAll, aCastlesAll, aCastles: TCastles;
    aCastlesDone: TBooleans;
    rbAnswers: array[0..5] of TRadioButton;
  end;

var
  fCQuiz2: TfCQuiz2;

implementation

{$R *.lfm}

{ Read castles form castles.txt file }

procedure ReadCastles(out CastlesGer1, CastlesGer2, CastlesGer3, CastlesGer4,
  CastlesGerN, CastlesGerS, CastlesAutChe, CastlesGerAll, CastlesAll: TCastles);

const
  Lands: array[1..12] of string = (
    'Bayern', 'Baden-Württemberg', 'Hessen', 'Mecklenburg-Vorpommern', 'Niedersachsen', 'Thüringen',
    'Rheinland-Pfalz', 'Sachsen-Anhalt', 'Brandenburg', 'Sachsen', 'Schleswig-Holstein', 'Nordrhein-Westfalen'
  );

var
  Ger1, Ger2, Ger3, Ger4, GerN, GerS, AutChe, Ger, N: Integer;
  Line: string;
  CastleFile: Text;

begin
  SetLength(CastlesGer1, 60); SetLength(CastlesGer2, 89);
  SetLength(CastlesGer3, 80); SetLength(CastlesGer4, 62);
  SetLength(CastlesGerN, 111); SetLength(CastlesGerS, 180);
  SetLength(CastlesAutChe, 53); SetLength(CastlesGerAll, 291);
  SetLength(CastlesAll, 344);
  Assign(CastleFile, 'castles.txt'); Reset(CastleFile);
  Ger1 := 0; Ger2 := 0; Ger3 := 0; Ger4 := 0;
  GerN := 0; GerS := 0; AutChe := 0; Ger := 0; N := 0;
  while not EoF(CastleFile) do begin
    Readln(CastleFile, Line);
    if Line <> '' then begin
      Inc(N);
      CastlesAll[N - 1].Name := UTF8Trim(UTF8Copy(Line, 12, UTF8Length(Line)));
      if LeftStr(Line, 3) = 'DEU' then begin
        // German castles
        CastlesAll[N - 1].Country := 'Deutschland';
        CastlesAll[N - 1].Land := Lands[StrToInt(Copy(Line, 5, 2))];
        Inc(Ger);
        CastlesGerAll[Ger - 1] := CastlesAll[N - 1];
        case Copy(Line, 8, 1)[1] of
          // North-south subdivision
          'N': begin Inc(GerN); CastlesGerN[GerN - 1] := CastlesAll[N - 1]; end;
          'S': begin Inc(GerS); CastlesGerS[GerS - 1] := CastlesAll[N - 1]; end;
        end;
        case Copy(Line, 10, 1)[1] of
          // North-south-west-east subdivision
          'N': begin Inc(Ger1); CastlesGer1[Ger1 - 1] := CastlesAll[N - 1]; end;
          'S': begin Inc(Ger2); CastlesGer2[Ger2 - 1] := CastlesAll[N - 1]; end;
          'W': begin Inc(Ger3); CastlesGer3[Ger3 - 1] := CastlesAll[N - 1]; end;
          'O': begin Inc(Ger4); CastlesGer4[Ger4 - 1] := CastlesAll[N - 1]; end;
        end;
      end
      else begin
        // Austria and Switzerland
        if LeftStr(Line, 3) = 'AUT' then
          CastlesAll[N - 1].Country := 'Österreich'
        else
          CastlesAll[N - 1].Country := 'Schweiz';
        CastlesAll[N - 1].Land := '';
        Inc(AutChe);
        CastlesAutChe[AutChe - 1] := CastlesAll[N - 1];
      end;
    end;
  end;
  Close(CastleFile);
end;

{ Display radiobutton labels or fill the castle names combobox with the answer selections for the actual question }

procedure DisplayAnswers(Answers: Integer; PicCastle: string; NoAnswer: Boolean; var Castles: TCastles; Out CorrectAnswer: string);

var
  I, J: Integer;
  Castle: string;
  OK: Boolean;
  CastleList: array of string;

begin
  CorrectAnswer := PicCastle;                                                  // correct answer may also be 'no correct answer'
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
      fCQuiz2.rbAnswers[I].Caption := ' ' + CastleList[I];
      fCQuiz2.rbAnswers[I].Checked := False;
    end;
    if NoAnswer then begin
      // If "no correct answer" is selected, add a 6th radiobutton with this option
      fCQuiz2.rbAnswers[5].Caption := ' Keine richtige Antwort';
      fCQuiz2.rbAnswers[5].Checked := False;
    end;
  end
  else begin
    // If 10 or 20 answers, use combobox
    fCQuiz2.cobAnswers.Clear;
    if NoAnswer then begin
      // If "no correct answer" is selected, add an item with this option to combobox
      fCQuiz2.cobAnswers.Items.AddText('--Keine richtige Antwort--');
    end;
    for I := 0 to Length(CastleList) - 1 do
      // Add the 10 or 20 answer choices to the combobox
      fCQuiz2.cobAnswers.Items.AddText(CastleList[I]);
    fCQuiz2.cobAnswers.ItemIndex := 0;
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
    if fCQuiz2.rbAnswer6.Visible then
      Rb := 5                                                                  // "no correct answer" selected
    else
      Rb := 4;                                                                 // "no correct answer" unselected
    for I := 0 to Rb do begin
      // Get user answer from the radiobutton, that is actually checked
      if fCQuiz2.rbAnswers[I].Checked then begin
        Answer := fCQuiz2.rbAnswers[I].Caption;
        Delete(Answer, 1, 1);                                                  // removes the space at the beginning of the radiobutton label
      end;
    end;
  end
  else begin
    // If 10 or 20 answers, get user answer from combobox
    Answer := fCQuiz2.cobAnswers.Text;
    if LeftStr(Answer, 2) = '--' then
      Answer := UTF8Copy(Answer, 3, UTF8Length(Answer) - 4);                   // remove the '--' at the beginning and end of combobox item
  end;
  Result := Answer;
end;

{************}
{* TfCQuiz2 *}
{************}

{ Application start: Initialisation }

procedure TfCQuiz2.FormCreate(Sender: TObject);

begin
  // Create array with radiobuttons
  rbAnswers[0] := rbAnswer1; rbAnswers[1] := rbAnswer2; rbAnswers[2] := rbAnswer3;
  rbAnswers[3] := rbAnswer4; rbAnswers[4] := rbAnswer5; rbAnswers[5] := rbAnswer6;
  // Read castle lists from file
  ReadCastles(aCastlesGer1, aCastlesGer2, aCastlesGer3, aCastlesGer4, aCastlesGerN, aCastlesGerS, aCastlesAutChe, aCastlesGerAll, aCastlesAll);
  // Initialize variables (startup parameters)
  iPlayers := 1; sRegionTemp := 'GerAll'; iAnswersTemp := 5; bNoAnswerTemp := True;
  // Start random number generator
  Randomize;
  // Prepare for a new quiz (with startup parameters)
  mFileNew.Click;
end;

{ Menu item "Datei > Neuer Quiz": Prepare for new quiz }

procedure TfCQuiz2.mFileNewClick(Sender: TObject);

var
  I: Integer;
  S: string;

begin
  tiRQ.Enabled := False;
  sRegion := sRegionTemp; iAnswers := iAnswersTemp; bNoAnswer := bNoAnswerTemp;  // Settings selected become active now
  // Set quiz title and choose country/region dependent castle list
  S := '';
  if sRegion = 'Ger1' then begin
    aCastles := aCastlesGer1;
    S := ' (Norden)';
  end
  else if sRegion = 'Ger2' then begin
    aCastles := aCastlesGer2;
    S := ' (Süden)';
  end
  else if sRegion = 'Ger3' then begin
    aCastles := aCastlesGer3;
    S := ' (Westen)';
  end
  else if sRegion = 'Ger4' then begin
    aCastles := aCastlesGer4;
    S := ' (Osten)';
  end
  else if sRegion = 'GerN' then begin
    aCastles := aCastlesGerN;
    S := 'Norddeutschland';
  end
  else if sRegion = 'GerS' then begin
    aCastles := aCastlesGerS;
    S := 'Süddeutschland';
  end
  else if sRegion = 'AutChe' then begin
    aCastles := aCastlesAutChe;
    S := 'Österreich u. Schweiz';
  end
  else if sRegion = 'GerAll' then begin
    aCastles := aCastlesGerAll;
    S := 'Deutschland';
  end
  else begin
    aCastles := aCastlesAll;
    S := 'Mitteleuropa';
  end;
  stQuiz.Caption := 'Burgen und Schlösser-Quiz: ';
  stQuiz.Caption := stQuiz.Caption + aCastles[0].Country;
  if LeftStr(S, 2) = ' (' then
    stQuiz.Caption := stQuiz.Caption + S
  else
    stQuiz.Caption := StringReplace(stQuiz.Caption, aCastles[0].Country, S, []);
  stQuiz.Caption := stQuiz.Caption + '.';
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
    // If 10 or 20 answers, use combobox
    for I := 0 to 5 do begin
      rbAnswers[I].Visible := False;                                           // radiobuttons not used, if 10 or 20 answers
    end;
    cobAnswers.Visible := True; cobAnswers.Clear;
  end;
  // Reset form controls
  edScore1.Text := ''; edScore2.Text := '';
  edAnswer.Text := ''; edAnswer2.Text := '';
  stSlide.Caption := 'Quiz'; edSlideCastle.Text := ''; edSlideRegion.Text := '';
  imCastle.Picture.Clear;
  btStart.Caption := 'Start'; btStart.Enabled := True;
  btPause.Caption := 'Pause'; btPause.Visible := False;
end;

{ Menu item "Datei > Diashow": Start slideshow }

procedure TfCQuiz2.mFileSlideshowClick(Sender: TObject);

// The slideshow is actually performed within the timer routine

var
  I: Integer;

begin
  mFileNew.Click;
  iQuestions := Length(aCastles);                                              // images shown will be those corresponding to current country/region
  iQuestion1 := 0;
  stSlide.Caption := 'Diashow'; edSlideCastle.Text := ''; edSlideRegion.Text := '';
  // Hide non-slideshow-related form controls
  for I := 0 to 5 do
    rbAnswers[I].Visible := False;
  cobAnswers.Visible := False;
  // Show slideshow-related form controls
  edSlideCastle.Visible := True;
  // Adapt buttons
  btStart.Caption := 'Stop'; btStart.Enabled := True; btStart.SetFocus;        // use the quiz button as stop button for the slideshow
  btPause.Caption := 'Pause'; btPause.Visible := True;                         // use a second button (slideshow only) to pause/resume the slideshow
  tiRQ.Enabled := True;                                                        // enable the slideshow timer}
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfCQuiz2.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Einstellungen > Anzahl der Spieler > ..": Select 1- or 2-player-quiz }

procedure TfCQuiz2.mSettingsPlayers1Click(Sender: TObject);

begin
  mSettingsPlayers1.Checked := True;  mSettingsPlayers2.Checked := False;
  edName2.Enabled := False;  edScore2.Enabled := False;                        // disable not used Player 2 controls
  iPlayers := 1;
  mFileNew.Click;                                                              // automatically start a new quiz, if number of players changes
end;

procedure TfCQuiz2.mSettingsPlayers2Click(Sender: TObject);

begin
  mSettingsPlayers2.Checked := True;  mSettingsPlayers1.Checked := False;
  edName2.Enabled := True;  edScore2.Enabled := True;
  iPlayers := 2;
  mFileNew.Click;
end;

{ Menu items "Einstellungen > Burgen/Schlösser Auswahl > ..": Select castles to be included into the quiz }

procedure TfCQuiz2.mSettingsCastlesGermany1Click(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := True; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'Ger1';
end;

procedure TfCQuiz2.mSettingsCastlesGermany2Click(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := True;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'Ger2';
end;

procedure TfCQuiz2.mSettingsCastlesGermany3Click(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := True; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'Ger3';
end;

procedure TfCQuiz2.mSettingsCastlesGermany4Click(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := True;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'Ger4';
end;

procedure TfCQuiz2.mSettingsCastlesGermanyNClick(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := True; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'GerN';
end;

procedure TfCQuiz2.mSettingsCastlesGermanySClick(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := True;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'GerS';
end;

procedure TfCQuiz2.mSettingsCastlesAutCheClick(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := True; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'AutChe';
end;

procedure TfCQuiz2.mSettingsCastlesGermanyAllClick(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := True; mSettingsCastlesAll.Checked := False;
  sRegionTemp := 'GerAll';
end;

procedure TfCQuiz2.mSettingsCastlesAllClick(Sender: TObject);

begin
  mSettingsCastlesGermany1.Checked := False; mSettingsCastlesGermany2.Checked := False;
  mSettingsCastlesGermany3.Checked := False; mSettingsCastlesGermany4.Checked := False;
  mSettingsCastlesGermanyN.Checked := False; mSettingsCastlesGermanyS.Checked := False;
  mSettingsCastlesAutChe.Checked := False; mSettingsCastlesGermanyAll.Checked := False; mSettingsCastlesAll.Checked := True;
  sRegionTemp := 'All';
end;

{ Menu items "Einstellungen > Antwortvorschläge > ..": Select number of answer choices to be displayed }

procedure TfCQuiz2.mSettingsAnswers5Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := True; mSettingsAnswers10.Checked := False; mSettingsAnswers20.Checked := False;
  iAnswersTemp := 5;
end;

procedure TfCQuiz2.mSettingsAnswers10Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := True; mSettingsAnswers20.Checked := False;
  iAnswersTemp := 10;
end;

procedure TfCQuiz2.mSettingsAnswers20Click(Sender: TObject);

begin
  mSettingsAnswers5.Checked := False; mSettingsAnswers10.Checked := False; mSettingsAnswers20.Checked := True;
  iAnswersTemp := 20;
end;

{ Menu item "Einstellungen > 'Keine richtige Antwort' aktivieren": Toggle 'no correct answer' enable/disable }

procedure TfCQuiz2.mOptionsNoAnswerClick(Sender: TObject);

begin
  if mOptionsNoAnswer.Checked then
    mOptionsNoAnswer.Checked := False
  else
    mOptionsNoAnswer.Checked := True;
  bNoAnswerTemp := mOptionsNoAnswer.Checked;
  rbAnswer6.Visible := bNoAnswerTemp;                                          // Hide 6th radiobutton, if 'no correct answer' is unselected
end;

{ Menu items "Einstellungen > Diashow Zeitintervall > ...": Select slideshow timer interval }

procedure TfCQuiz2.mSettingsSlideshow1Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := True;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False; mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1000;
end;

procedure TfCQuiz2.mSettingsSlideshow2Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := True;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 1500;
end;

procedure TfCQuiz2.mSettingsSlideshow3Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := True;   mSettingsSlideshow4.Checked := False;
  tiRQ.Interval := 2000;
end;

procedure TfCQuiz2.mSettingsSlideshow4Click(Sender: TObject);

begin
  mSettingsSlideshow1.Checked := False;  mSettingsSlideshow2.Checked := False;
  mSettingsSlideshow3.Checked := False;  mSettingsSlideshow4.Checked := True;
  tiRQ.Interval := 3000;
end;

{ Menu item "Hilfe > Hilfe": Display (very short) application help text }

procedure TfCQuiz2.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Die Anzahl der Spieler, eine der Burgen/Schlösser-Listen, die Anzahl der vorgeschlagenen Antworten ';
  S += 'und ob der Vorschlag "Keine richtige Antwort" benutzt werden soll oder nicht, auswählen. Wobei zu ';
  S += 'beachten ist, dass die Auswahl erst wirksam wird, wenn ein neuer Quiz im Menu "Datei" gestarted wird. ';
  S += 'Mit "Start/Frage" wird ein neues Foto angezeigt, mit "Antwort" wird die vom Benutzer ausgewählte Antwort ';
  S += 'validiert.';
  MessageDlg('Hilfe "CastlesQuiz2"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Über": Display application about text }

procedure TfCQuiz2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Fotoquiz (plus Diashow) über die Burgen und Schlösser in Deutschland, Österreich und der Schweiz.';
  S += LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Juni 2021.';
  MessageDlg('Über "CastlesQuiz2"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Daten/Fotos": Display information concerning the origin of castle data and photos }

procedure TfCQuiz2.mHelpPhotosClick(Sender: TObject);

var
  S: string;

begin
  S := 'Herkunft der Daten und Fotos:' + LineEnding;
  S += 'Die in "CastlesQuiz2" enthaltenen Burgen und Schlösser sind diejenigen, die auf der Webseite https://www.burgen.de/ ';
  S += 'aufgelistet sind. Die große Mehrzahl der Fotos ist ebenfalls dieser Seite entnommen.';
  MessageDlg('Info "CastlesQuiz2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Frage/Antwort/Stop": Quiz question/answer resp. stop of slideshow }

procedure TfCQuiz2.btStartClick(Sender: TObject);

var
  Score1, Score2, I: Integer;
  Filename, UserAnswer: string;

begin
  // Button "Start/Frage": Generation of a new quiz question
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Frage') then begin
    // Some initialisations to do if it is the beginning of a new quiz
    // Note: The slideshow is automatically started ("Départ" is always for the quiz)
    if btStart.Caption = 'Start' then begin
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
    stSlide.Caption := 'Burg/Schloss ' + IntToStr(iQuestion1 + iQuestion2) + '/' + IntToStr(iQuestions);
    // Display answers (5, 10 or 20 proposals, to select answer from)
    DisplayAnswers(iAnswers, aCastles[iQuizQuestion].Name, bNoAnswer, aCastles, sCorrectAnswer);
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

procedure TfCQuiz2.btPauseClick(Sender: TObject);

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

procedure TfCQuiz2.tiVQTimer(Sender: TObject);

var
  Filename: string;

begin
  Inc(iQuestion1);
  // Proceed if there are still castles to be displayed
  if iQuestion1 <= iQuestions then begin
    // Display castle's name
    stSlide.Caption := 'Burg/Schloss ' + IntToStr(iQuestion1) + '/' + IntToStr(iQuestions);
    edSlideCastle.Text := aCastles[iQuestion1 - 1].Name;
    // Display country resp. German Bundesland
    if aCastles[iQuestion1 - 1].Country = 'Deutschland' then
      edSlideRegion.Text := aCastles[iQuestion1 - 1].Land
    else
      edSlideRegion.Text := aCastles[iQuestion1 - 1].Country;
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
    MessageDlg('Ende der Diashow', 'Alle Burgen und Schlösser wurden gezeigt. Diashow beendet.', mtInformation, [mbOK], 0);
    btStart.Enabled := False;
    btPause.Visible := False;
  end;
end;

end.

