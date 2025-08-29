{*****************************************}
{* Main unit for Substantifs application *}
{*****************************************}

unit subst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8, help;

type
  TNoun = record
    Singular: string;
    Plural: Char;
  end;
  TNouns = array of TNoun;
  TNounsDone = array of Boolean;
  TException = record
    Singular, Plural: string;
  end;
  {*********}
  { TfSubst }
  {*********}
  TfSubst = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsRegExclude: TMenuItem;
    mHelp, mHelpGrammar, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label5, Label6, Label7, Label8, Label9: TLabel;
    laQuestion: TLabel;
    edSg, edPl, edEval: TEdit;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAcirc, btEaigu, btEgrave, btEcirc, btEtrema: TButton;
    btItrema, btIcirc, btUcirc, btCcedil: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsRegExcludeClick(Sender: TObject);
    procedure mHelpGrammarClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btAcircClick(Sender: TObject);
    procedure btEaiguClick(Sender: TObject);
    procedure btEgraveClick(Sender: TObject);
    procedure btEcircClick(Sender: TObject);
    procedure btEtremaClick(Sender: TObject);
    procedure btIcircClick(Sender: TObject);
    procedure btItremaClick(Sender: TObject);
    procedure btUcircClick(Sender: TObject);
    procedure btCcedilClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iNounsAll, iNounsIrreg, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sPlural1, sPlural2: string;
    bExcludeReg, bExcludeRegTemp: Boolean;
    aNouns: TNouns;
    aNounsDone: TNounsDone;
  end;

const
  Irregulars: array[0..7] of TException = (
    (Singular: 'oeil'; Plural: 'yeux'),
    (Singular: 'aïeul'; Plural: 'aïeux'),
    (Singular: 'ail'; Plural: 'aulx'),
    (Singular: 'ciel (paradis)'; Plural: 'cieux'),
    (Singular: 'maximum'; Plural: 'maxima'),
    (Singular: 'minimum'; Plural: 'minima'),
    (Singular: 'erratum'; Plural: 'errata'),
    (Singular: 'scenario'; Plural: 'scenarii')
  );
  ExceptionsAU: array[0..1] of string = (
    'landau', 'sarrau'
  );
  ExceptionsEU: array[0..4] of string = (
    'bleu', 'pneu', 'émeu', 'enfeu', 'lieu (poisson)'
  );
  ExceptionsOU: array[0..7] of string = (
    'bijou', 'caillou', 'chou', 'genou', 'hibou', 'joujou', 'pou', 'ripou'
  );
  ExceptionsAL: array[0..16] of string = (
    'aval', 'bal', 'cal', 'carnaval', 'chacal', 'cérémonial', 'festival', 'pal', 'régal',
    'serval', 'étal', 'val', 'récital', 'final (musique)', 'choral (musique)', 'idéal', 'chenal'
  );
  ExceptionsAIL: array[0..9] of string = (
    'bail', 'corail', 'émail', 'fermail', 'gemmail', 'soupirail', 'travail', 'vantail',
    'ventail', 'vitrail'
  );
  ExceptionsNP: array[0..5] of string = (
    'Amérique (nord et sud)', 'Bourbon (famille)', 'César (famille)', 'Corée (nord et sud)', 'Einstein (métaphoriquement)',
    'Gracque (famille)'
  );

var
  fSubst: TfSubst;

implementation

{$R *.lfm}

{ Read nouns from text file }

procedure ReadNouns(out Nouns: TNouns; out NounsAll, NounsIrreg: Integer);

var
  Line: string;
  InFile: Text;

begin
  NounsAll := 0; NounsIrreg := 0;
  SetLength(Nouns, 0);
  Assign(InFile, 'substantifs.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(NounsAll);
      SetLength(Nouns, NounsAll);
      Nouns[NounsAll - 1].Singular := UTF8Trim(UTF8Copy(Line, 1, 30));         // noun (singular)
      Nouns[NounsAll - 1].Plural := UTF8Copy(Line, 31, 1)[1];                  // "rule" for plural
      if Nouns[NounsAll - 1].Plural <> '-' then
        Inc(NounsIrreg);                                                       // count "not regular" nouns
    end;
  end;
  Close(InFile);
end;

{ Insert letter corr. to button pushed into user answer field }

procedure InsertLetter(Letter: string);

// Did not find a way to move cursor to end of text (???)

begin
  fSubst.edPl.Text := fSubst.edPl.Text + Letter;
  fSubst.edPl.SetFocus;
end;

{ Determine plural suffix for given noun }

procedure PluralOfNoun(Noun: TNoun; out Plural1, Plural2: string);

var
  I, P: Integer;
  Singular: string;

begin
  Singular := Noun.Singular; Plural1 := ''; Plural2 := '';
  // Remove noun info (text between brackets)
  P := Pos(' (', Singular);
  if P <> 0 then
    Singular := Copy(Singular, 1, P - 1);                                      // avec Pos(), utiliser Copy(), et non pas UTF8Copy()!
  // "Noms propres"
  if Noun.Plural = 'N' then begin
    // Normally invariable, but some exceptions
    Plural1 := Singular;
    for I := 0 to Length(ExceptionsNP) - 1 do begin
      if Noun.Singular = ExceptionsNP[I] then
        Plural1 += 's';
    end;
  end
  // Irregular plurals
  else if UpperCase(Noun.Plural) = 'I' then begin
    // Plural from list (for 'i', there is also a regular form)
    for I := 0 to Length(Irregulars) - 1 do begin
      if Noun.Singular = Irregulars[I].Singular then begin
        Plural1 := Irregulars[I].Plural;
        if Noun.Plural = 'i' then
          Plural2 := Singular + 's';
      end;
    end;
  end
  // Special plural rule, depending on singular suffix
  else if (UpperCase(Noun.Plural) = 'R') or (UpperCase(Noun.Plural) = 'L') then begin
    // Plural rule for all nouns with this suffix
    if Noun.Plural = 'R' then begin
      // Nouns in -s, -x or -z
      if (UTF8Copy(Singular, UTF8Length(Singular), 1) = 's') or
         (UTF8Copy(Singular, UTF8Length(Singular), 1) = 'x') or
         (UTF8Copy(Singular, UTF8Length(Singular), 1) = 'z') then
        Plural1 := Singular
      // Nouns in -eau
      else if UTF8Copy(Singular, UTF8Length(Singular) - 2, 3) = 'eau' then
        Plural1 := Singular + 'x';
    end
    // Plural rule for most nouns with this suffix (exceptions from lists)
    else if UpperCase(Noun.Plural) = 'L' then begin
      // Nouns in -au
      if UTF8Copy(Singular, UTF8Length(Singular) - 1, 2) = 'au' then begin
        for I := 0 to Length(ExceptionsAU) - 1 do begin
          // Exceptions
          if Noun.Singular = ExceptionsAU[I] then
            Plural1 := Singular + 's';
        end;
        if Plural1 = '' then
          // Following rule
          Plural1 := Singular + 'x';
      end
      // Nouns in -eu
      else if UTF8Copy(Singular, UTF8Length(Singular) - 1, 2) = 'eu' then begin
        for I := 0 to Length(ExceptionsEU) - 1 do begin
          // Exceptions
          if Noun.Singular = ExceptionsEU[I] then
            Plural1 := Singular + 's';
        end;
        if Plural1 = '' then
          // Following rule
          Plural1 := Singular + 'x';
      end
      // Nouns in -ou
      else if UTF8Copy(Singular, UTF8Length(Singular) - 1, 2) = 'ou' then begin
        for I := 0 to Length(ExceptionsOU) - 1 do begin
          // Exceptions
          if Noun.Singular = ExceptionsOU[I] then
            Plural1 := Singular + 'x';
        end;
        if Plural1 = '' then
          // Following rule
          Plural1 := Singular + 's';
      end
      // Nouns in -al
      else if UTF8Copy(Singular, UTF8Length(Singular) - 1, 2) = 'al' then begin
        for I := 0 to Length(ExceptionsAL) - 1 do begin
          // Exceptions
          if Noun.Singular = ExceptionsAL[I] then
            Plural1 := Singular + 's';
        end;
        if Plural1 = '' then
          // Following rule
          Plural1 := UTF8Copy(Singular, 1, UTF8Length(Singular) - 2) + 'aux';
      end
      // Nouns in -ail
      else if UTF8Copy(Singular, UTF8Length(Singular) - 2, 3) = 'ail' then begin
        for I := 0 to Length(ExceptionsAIL) - 1 do begin
          // Exceptions
          if Noun.Singular = ExceptionsAIL[I] then
            Plural1 := UTF8Copy(Singular, 1, UTF8Length(Singular) - 3) + 'aux';
        end;
        if Plural1 = '' then
          // Following rule
          Plural1 := Singular + 's';
      end;
      // 2nd form of plural (= regular)
      if Noun.Plural = 'l' then
        Plural2 := Singular + 's';
    end;
  end
  // Regular plural
  else if Noun.Plural = '-' then
    Plural1 := Singular + 's';
  // If there isn't a second form for the plural, set Plural2 = Plural1
  if Plural2 = '' then
    Plural2 := Plural1;
end;

{*********}
{ TfSubst }
{*********}

{ Application start: Initialisation }

procedure TfSubst.FormCreate(Sender: TObject);

begin
  Randomize;
  ReadNouns(aNouns, iNounsAll, iNounsIrreg);
  SetLength(aNounsDone, iNounsAll);
  iQuestionsTemp := 20; bExcludeRegTemp := False;
  mExerciseNew.Click;                                                          // start a new exercise
end;

{ Menu item "Exercice > Nouveau": Prepare for new exercise }

procedure TfSubst.mExerciseNewClick(Sender: TObject);

var
  I: Integer;

begin
  laQuestion.Caption := 'Question:';
  edSg.Text := ''; edPl.Text := ''; edEval.Text := ''; edEval.Color := clDefault;
  edQuestions.Text := ''; edCorrect.Text := ''; edFalse.Text := '';
  edSuccess.Text := ''; edSuccess.Color := clDefault;
  iQuestions := iQuestionsTemp;                                                // number of questions selected now becomes active
  bExcludeReg := bExcludeRegTemp;                                              // idem for option "exclude regular plurals"
  if bExcludeReg and (iQuestions > iNounsIrreg) then                           // number of questions can't be greater than number of (not regular) nouns
    iQuestions := iNounsIrreg;
  iQuestion := 0; iCorrect := 0;
  for I := 0 to iNounsAll - 1 do
    aNounsDone[I] := False;                                                    // mark all nouns as "not yet asked"
  btQuestion.Caption := 'Commencer'; btQuestion.Enabled := True;
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfSubst.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Nombre de questions...": User entry of number of exercise questions }

procedure TfSubst.mOptionsQuestionsClick(Sender: TObject);

var
  QT: Integer;
  S: string;

begin
  S := InputBox('Substantifs français.', 'Nombre de questions', IntToStr(iQuestions));
  if S <> '' then begin
    // This is the case if OK (and not Cancel) button has been pushed
    QT := StrToInt(S);
    if QT < 10 then                                                            // minimum of questions arbitrarily fixed to 10
      QT := 10
    else if QT > iNounsAll then                                                // number of questions can't be greater than number of nouns
      QT := iNounsAll;
    iQuestionsTemp := QT;                                                      // number of questions becomes active, if user chooses to do a new exercise
  end;
end;

{ Menu item "Options > Exclure substantifs réguliers": Toggle questions/no questions with regular nouns }

procedure TfSubst.mOptionsRegExcludeClick(Sender: TObject);

begin
  if mOptionsRegExclude.Checked then
    mOptionsRegExclude.Checked := False
  else
    mOptionsRegExclude.Checked := True;
  bExcludeRegTemp := mOptionsRegExclude.Checked;
end;

{ Menu item "Aide > Aide grammaire": Display grammar help text }

procedure TfSubst.mHelpGrammarClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Aide programme": Display program help text }

procedure TfSubst.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Info programme": Display program about }

procedure TfSubst.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Grammaire française.' + LineEnding;
  S += 'Le pluriel des substantifs.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, mars - avril 2020.';
  MessageDlg('Info "Substantifs"', S, mtInformation, [mbOK], 0);
end;

{ Button "Commencer/Question/Réponse": Generate new question resp. check user answer }

procedure TfSubst.btQuestionClick(Sender: TObject);

var
  IX, Success: Integer;
  OK: Boolean;

begin
  // Button "Commencer/Question": Generate new question
  if (btQuestion.Caption = 'Commencer') or (btQuestion.Caption = 'Question') then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    edPl.Text := ''; edEval.Text := ''; edEval.Color := clDefault;
    // Select random noun (do so, until a valid one has been found)
    repeat
      OK := True;
      IX := Random(iNounsAll);
      if aNounsDone[IX] then                                                   // only nouns not asked so far
        OK := False
      else if bExcludeReg and (aNouns[IX].Plural = '-') then                   // only irregular nouns (if this option is selected)
        OK := False;
    until OK;
    aNounsDone[IX] := True;                                                    // mark this noun as done
    edSg.Text := aNouns[IX].Singular;                                          // display noun (singular)
    PluralOfNoun(aNouns[IX], sPlural1, sPlural2);                              // determine the noun's plural
    if sPlural1 = '' then begin
      // This should normally not happen!
      MessageDlg('Program error', 'Could not determine plural of ' + aNouns[IX].Singular + '!', mtError, [mbOK], 0);
      Halt;                                                                    // this aborts the application
    end;
    edPl.SetFocus;
    // Next button push will be to check the user's answer
    btQuestion.Caption := 'Réponse';
  end
  // Button "Réponse": Check user's answer
  else begin
    if (edPl.Text = sPlural1) or (edPl.Text = sPlural2) then begin
      // User answer is correct
      Inc(iCorrect);
      edEval.Text := 'Réponse correcte'; edEval.Color := clLime;
    end
    else begin
      // User answer is false
      edEval.Text := 'Faux! Réponse correcte: ' + sPlural1; edEval.Color := clRed;
      if sPlural2 <> sPlural1 then
        edEval.Text := edEval.Text + ' / ' + sPlural2;
    end;
    // Update the evaluation counters
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iQuestion - iCorrect);
    Success := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Success) + '%';
    // Success field colored, depending on its actual value
    if Success >= 60 then
      edSuccess.Color := clLime
    else if Success >= 50 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clRed;
    // Next button push will be to generate a new question
    btQuestion.Caption := 'Question';
    if iQuestion = iQuestions then begin
      // All questions done: End of exercise
      MessageDlg('Substantifs français', 'Fin de l''exercice.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Letter-buttons: Insert corresponding letter into user answer field }

procedure TfSubst.btAcircClick(Sender: TObject);

begin
  InsertLetter('â');
end;

procedure TfSubst.btEaiguClick(Sender: TObject);

begin
  InsertLetter('é');
end;

procedure TfSubst.btEgraveClick(Sender: TObject);

begin
  InsertLetter('è');
end;

procedure TfSubst.btEcircClick(Sender: TObject);

begin
  InsertLetter('ê');
end;

procedure TfSubst.btEtremaClick(Sender: TObject);

begin
  InsertLetter('ë');
end;

procedure TfSubst.btIcircClick(Sender: TObject);

begin
  InsertLetter('î');
end;

procedure TfSubst.btItremaClick(Sender: TObject);

begin
  InsertLetter('ï');
end;

procedure TfSubst.btUcircClick(Sender: TObject);

begin
  InsertLetter('û');
end;

procedure TfSubst.btCcedilClick(Sender: TObject);

begin
  InsertLetter('ç');
end;

end.

