{***************************************}
{* Main unit for Adjectifs application *}
{***************************************}

unit adj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8, help;

type
  TAdjective = record
    Name: string;
    Feminine, Plural: Char;
  end;
  TAdjectives = array of TAdjective;
  TAdjectivesDone = array of Boolean;
  TException = record
    Masculine, Feminine: string;
  end;
  {*******}
  { TfAdj }
  {*******}
  TfAdj = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsRegExclude: TMenuItem;
    mOptionsQType, mOptionsQTypeFS, mOptionsQTypeMP, mOptionsQTypeFP: TMenuItem;
    mHelp, mHelpGrammar, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9: TLabel;
    laQuestion: TLabel;
    edAdjMS, edAdjFS, edAdjMP, edAdjFP: TEdit;
    edEval: TMemo;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAcirc, btEaigu, btEgrave, btEcirc, btEtrema: TButton;
    btItrema, btIcirc, btUcirc, btCcedil: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsQTypeFSClick(Sender: TObject);
    procedure mOptionsQTypeMPClick(Sender: TObject);
    procedure mOptionsQTypeFPClick(Sender: TObject);
    procedure mOptionsRegExcludeClick(Sender: TObject);
    procedure mHelpGrammarClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAcircClick(Sender: TObject);
    procedure btEaiguClick(Sender: TObject);
    procedure btEcircClick(Sender: TObject);
    procedure btEgraveClick(Sender: TObject);
    procedure btEtremaClick(Sender: TObject);
    procedure btIcircClick(Sender: TObject);
    procedure btItremaClick(Sender: TObject);
    procedure btUcircClick(Sender: TObject);
    procedure btCcedilClick(Sender: TObject);
    procedure edAdjFPEnter(Sender: TObject);
    procedure edAdjFSEnter(Sender: TObject);
    procedure edAdjMPEnter(Sender: TObject);
  private
    iAdjectivesAll, iAdjectivesIrreg, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sFemSg, sMascPl, sFemPl, sEditField: string;
    bRegExclude, bRegExcludeTemp: Boolean;
    aAdjectives: TAdjectives;
    aAdjectivesDone: TAdjectivesDone;
  end;

const
  ExceptionsFem: array[0..11] of TException = (
    (Masculine: 'andalou'; Feminine: 'andalouse'),
    (Masculine: 'bénin'; Feminine: 'bénigne'),
    (Masculine: 'coi'; Feminine: 'coite'),
    (Masculine: 'doux'; Feminine: 'douce'),
    (Masculine: 'faux'; Feminine: 'fausse'),
    (Masculine: 'favori'; Feminine: 'favorite'),
    (Masculine: 'frais'; Feminine: 'fraîche'),
    (Masculine: 'grec'; Feminine: 'grecque'),
    (Masculine: 'roux'; Feminine: 'rousse'),
    (Masculine: 'sec'; Feminine: 'sèche'),
    (Masculine: 'tiers'; Feminine: 'tierce'),
    (Masculine: 'vieux'; Feminine: 'vieille')
  );
  ExceptionsFemDoubleConsonant: array[0..6] of string = (
    'bas', 'gras', 'gros', 'las', 'métis', 'sot', 'boulot'
  );
  ExceptionsFemTEUR: array[0..5] of string = (
    'accélérateur', 'accusateur', 'calculateur', 'créateur', 'directeur', 'protecteur'
  );
  ExceptionsFemEUR1: array[0..0] of string = (
    'majeur'
  );
  ExceptionsFemEUR2: array[0..0] of string = (
    'vengeur'
  );
  ExceptionsFemET: array[0..9] of string = (
    'complet', 'concret', 'désuet', 'discret', 'incomplet', 'indiscret', 'inquiet', 'replet', 'secret', 'quiet'
  );
  ExceptionsFemC: array[0..1] of string = (
    'blanc', 'franc'
  );
  ExceptionsFemU: array[0..3] of string = (
    'exigu', 'contigu', 'aigu', 'ambigu'
  );
  ExceptionsPlAL: array[0..6] of string = (
    'austral', 'bancal', 'fatal', 'final', 'natal', 'naval', 'banal'
  );
  VariableAdjsFromNouns: array[0..2] of string = (
    'rose', 'fauve', 'mauve'
  );

var
  fAdj: TfAdj;

implementation

{$R *.lfm}

{ Read adjectives from text file }

procedure ReadAdjectives(out Adjectives: TAdjectives; out AdjAll, AdjIrreg: Integer);

var
  Line: string;
  InFile: Text;

begin
  AdjAll := 0; AdjIrreg := 0;
  SetLength(Adjectives, 0);
  Assign(InFile, 'adjectifs.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(AdjAll);
      SetLength(Adjectives, AdjAll);
      Adjectives[AdjAll - 1].Name := UTF8Trim(UTF8Copy(Line, 1, 20));          // adjective name (masculine singular)
      Adjectives[AdjAll - 1].Feminine := UTF8Copy(Line, 21, 1)[1];             // "rule" for feminine
      Adjectives[AdjAll - 1].Plural := UTF8Copy(Line, 23, 1)[1];               // "rule" for masculine plural
      if (Adjectives[AdjAll - 1].Feminine <> '-') or (Adjectives[AdjAll - 1].Plural <> '-') then
        Inc(AdjIrreg);                                                         // count "non regular" adjectives
    end;
  end;
  Close(InFile);
end;

{ Insert letter corr. to button pushed into actual edit field }

procedure InsertLetter(EditField, Letter: string);

begin
  if EditField = 'FS' then begin
    fAdj.edAdjFS.Text := fAdj.edAdjFS.Text + Letter;
    fAdj.edAdjFS.SetFocus;
  end
  else if EditField = 'MP' then begin
    fAdj.edAdjMP.Text := fAdj.edAdjMP.Text + Letter;
    fAdj.edAdjMP.SetFocus;
  end
  else if EditField = 'FP' then begin
    fAdj.edAdjFP.Text := fAdj.edAdjFP.Text + Letter;
    fAdj.edAdjFP.SetFocus;
  end;
end;

{ Determine suffixes for given adjective }

procedure DoAdjective(Adjective: TAdjective; out FemSg, MascPl, FemPl: string);

var
  I: Integer;
  MascSg: string;

begin
  MascSg := Adjective.Name; FemSg := ''; MascPl := ''; FemPl := '';
  // Invariable adjectives
  // ---------------------
  if (Adjective.Feminine = 'N') or (Adjective.Feminine = 'I') then begin
    if Adjective.Feminine = 'I' then begin
      // Always invariable adjectives
      FemSg  := MascSg; MascPl := MascSg; FemPl := MascSg;
    end
    else begin
      // Adjectives formed from nouns: invariable (with exceptions list!)
      for I := 0 to Length(VariableAdjsFromNouns) - 1 do begin
        // Treat variable adjectives as regular
        if MascSg = VariableAdjsFromNouns[I] then begin
          Adjective.Feminine := '-'; Adjective.Plural := '-';
        end;
      end;
      if Adjective.Feminine = 'N' then begin
        // Invariable adjectives
        FemSg := MascSg; MascPl := MascSg; FemPl := MascSg;
      end;
    end;
  end;
  // Feminine singular
  //------------------
  if FemSg = '' then begin
    // General routine, except for invariable adjectives (already done)
    if Adjective.Feminine = '-' then begin
      // Regular feminine: add suffix -e (except if masculine ends in -e)
      FemSg := MascSg;
      if UTF8Copy(MascSg, UTF8Length(MascSg), 1) <> 'e' then
        FemSg += 'e';
    end
    else if Adjective.Feminine = 'E' then begin
      // Exceptions: special feminine for given adjective
      for I := 0 to Length(ExceptionsFem) - 1 do begin
        if MascSg = ExceptionsFem[I].Masculine then
          FemSg := ExceptionsFem[I].Feminine;
      end;
    end
    else if (Adjective.Feminine = 'R') or (Adjective.Feminine = 'L') then begin
      // Special cases: feminine suffix depends on masculine suffix
      if UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 'x' then begin
        // Masculine in -x: feminine in -se
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 1) + 'se';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 'f' then begin
        // Masculine in -f: feminine in -ve
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 1) + 've';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 2, 3) = 'eau' then begin
        // Masculine in -eau: feminine in -elle
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 3) + 'elle';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'ou' then begin
        // Masculine in -ou: feminine in -olle
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 2) + 'olle';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'er' then begin
        // Masculine in -er: feminine in -ère
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 2) + 'ère';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 'g' then begin
        // Masculine in -g: feminine in -gue
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 1) + 'gue';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg), 3) = 'eux' then begin
        // Masculine in -eux: feminine in -euse
        FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 3) + 'euse';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 3, 4) = 'teur' then begin
        // Masculine in -teur: feminine in -teuse (with exceptions list!)
        for I := 0 to Length(ExceptionsFemTEUR) - 1 do begin
          if MascSg = ExceptionsFemTEUR[I] then
            // Adjectives in the list have feminine in -trice
            FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 4) + 'trice';
        end;
        if FemSg = '' then begin
          // Normal case for adjectives in -teur: feminine in -teuse
          FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 4) + 'teuse';
        end;
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 2, 3) = 'eur' then begin
        // Masculine in -eur: feminine in -euse (with exceptions list!)
        for I := 0 to Length(ExceptionsFemEUR1) - 1 do begin
          if MascSg = ExceptionsFemEUR1[I] then
            // Adjectives in this list have regular feminine
            FemSg := MascSg + 'e';
        end;
        if FemSg = '' then begin
          for I := 0 to Length(ExceptionsFemEUR2) - 1 do begin
            if MascSg = ExceptionsFemEUR2[I] then
              // Adjectives in this list have feminine in -eresse
              FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 3) + 'eresse';
          end;
        end;
        if FemSg = '' then begin
          // Normal case for adjectives in -eur: feminine in -euse
          FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 3) + 'euse';
        end;
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 'c' then begin
        // Masculine in -c: feminine in -que (with exceptions list!)
        for I := 0 to Length(ExceptionsFemC) - 1 do begin
          if MascSg = ExceptionsFemC[I] then
            // Adjectives in the list have feminine in -che
            FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 1) + 'che';
        end;
        if FemSg = '' then begin
          // Normal case for adjectives in -c: feminine in -que
          FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 1) + 'que';
        end;
      end
      else if (UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'el') or
              (UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'il') or
              (UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'ul') or
              (UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'en') or
              (UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'on') then begin
        // Masculine in -el/-il/-ul/-en/-on: feminine with doubling the final consonant (plus -e)
        FemSg := MascSg + UTF8Copy(MascSg, UTF8Length(MascSg), 1) + 'e';
      end
      else if (UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 's') or (UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'ot') then begin
        // Masculine in -s/-ot: regular feminine (with exceptions list!)
        FemSg := MascSg;
        for I := 0 to Length(ExceptionsFemDoubleConsonant) - 1 do begin
          if MascSg = ExceptionsFemDoubleConsonant[I] then
            // Adjectives in the list double the consonant
            FemSg += UTF8Copy(MascSg, UTF8Length(MascSg), 1);
        end;
        FemSg += 'e';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'et' then begin
        // Masculine in -et: feminine with doubling the final t (plus -e) (with exceptions list!)
        for I := 0 to Length(ExceptionsFemET) - 1 do begin
          if MascSg = ExceptionsFemET[I] then
            // Adjectives in the list have feminine in -ète
            FemSg := UTF8Copy(MascSg, 1, UTF8Length(MascSg) - 2) + 'ète';
        end;
        if FemSg = '' then begin
          // Normal case for adjectives in -et: feminine with doubling of final t
          FemSg := MascSg + 't' + 'e';
        end;
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 'u' then begin
        // Masculine in -u: regular feminine (with exceptions list!)
        for I := 0 to Length(ExceptionsFemU) - 1 do begin
          if MascSg = ExceptionsFemU[I] then
            // Adjectives in the list have trema on e
            FemSg := MascSg + 'ë';
        end;
        if FemSg = '' then begin
          // Normal case for adjectives in -u: regular feminine
          FemSg := MascSg + 'e';
        end;
      end;
    end;
  end;
  // Masculine plural
  // ----------------
  if MascPl = '' then begin
    if Adjective.Plural = '-' then begin
      // Regular plural: add suffix -s
      MascPl := MascSg + 's';
    end
    else if (Adjective.Plural = 'R') or (Adjective.Plural = 'L') then begin
      // Special cases: plural suffix depends on singular suffix
      if (UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 's') or (UTF8Copy(MascSg, UTF8Length(MascSg), 1) = 'x') then begin
        // Masc. sg. ending in -s or -x: no plural suffix added
        MascPl := MascSg;
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 2, 3) = 'eau' then begin
        // Masc. sg. ending in -eau: add suffix -x (instead of -s)
        MascPl := MascSg + 'x';
      end
      else if UTF8Copy(MascSg, UTF8Length(MascSg) - 1, 2) = 'al' then begin
        // Masc. sg. ending in -al: replace -al by -aux (with exceptions list!)
        MascPl := MascSg;
        for I := 0 to Length(ExceptionsPlAL) - 1 do begin
          if MascSg = ExceptionsPlAL[I] then
            // Adjectives in the list have regular plural
            MascPl += 's';
        end;
        if MascPl = MascSg then
          // Normal case for adjectives in -al: plural in -aux
          MascPl := UTF8Copy(MascPl, 1, UTF8Length(MascPl) - 2) + 'aux';
      end
    end;
  end;
  // Feminine plural
  // ---------------
  if FemPl = '' then
    FemPl := FemSg + 's';
end;

{*******}
{ TfAdj }
{*******}

{ Application start: Initialisation }

procedure TfAdj.FormCreate(Sender: TObject);

begin
  Randomize;
  ReadAdjectives(aAdjectives, iAdjectivesAll, iAdjectivesIrreg);
  SetLength(aAdjectivesDone, iAdjectivesAll);
  iQuestionsTemp := 20; bRegExcludeTemp := False;
  mExerciseNew.Click;                                                          // start new exercise
end;

{ Menu item "Exercice > Nouveau": Prepare for new exercise }

procedure TfAdj.mExerciseNewClick(Sender: TObject);

var
  I: Integer;

begin
  laQuestion.Caption := 'Question:';
  edAdjMS.Text := ''; edAdjFS.Text := ''; edAdjMP.Text := ''; edAdjFP.Text := ''; edEval.Clear;
  edQuestions.Text := ''; edCorrect.Text := ''; edFalse.Text := '';
  edSuccess.Text := ''; edSuccess.Color := clDefault;
  iQuestions := iQuestionsTemp;                                                // number of questions selected now becomes active
  bRegExclude := bRegExcludeTemp;                                              // id. for "exclude reg. adj." selection
  if bRegExclude and (iQuestions > iAdjectivesIrreg) then                      // question number can't be > number of all (actual) adjectives
    iQuestions := iAdjectivesIrreg;
  iQuestion := 0; iCorrect := 0;
  for I := 0 to iAdjectivesAll - 1 do
    aAdjectivesDone[I] := False;                                               // set all adjectives as "not yet asked"
  sEditField := '';                                                            // using this variable to know which field is currently edited
  btQuestion.Caption := 'Commencer'; btQuestion.Enabled := True;
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfAdj.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Nombre de questions...": User entry of number of exercise questions }

procedure TfAdj.mOptionsQuestionsClick(Sender: TObject);

var
  QT: Integer;
  S: string;

begin
  S := InputBox('Adectifs français.', 'Nombre de questions', IntToStr(iQuestions));
  if S <> '' then begin
    // This is the case if OK (and not Cancel) button has been pushed
    QT := StrToInt(S);
    if QT < 10 then                                                            // minimum of questions arbitrarily fixed to 10
      QT := 10
    else if QT > iAdjectivesAll then                                           // number of questions can't be greater than number of adjectives
      QT := iAdjectivesAll;
    iQuestionsTemp := QT;                                                      // number of questions becomes active, if user chooses to do a new exercise
  end;
end;

{ Menu items "Options > Terminaisons à trouver > ...": Select what adjective forms, the exercise should be about }

procedure TfAdj.mOptionsQTypeFSClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Réponse' then begin
    if mOptionsQTypeFS.Checked then
      mOptionsQTypeFS.Checked := False
    else
      mOptionsQTypeFS.Checked := True;
  end;
end;

procedure TfAdj.mOptionsQTypeMPClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Réponse' then begin
    if mOptionsQTypeMP.Checked then
      mOptionsQTypeMP.Checked := False
    else
      mOptionsQTypeMP.Checked := True;
  end;
end;

procedure TfAdj.mOptionsQTypeFPClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Réponse' then begin
    if mOptionsQTypeFP.Checked then
      mOptionsQTypeFP.Checked := False
    else
      mOptionsQTypeFP.Checked := True;
  end;
end;

{ Menu item "Options > Exclure adjectifs réguliers": Toggle questions/no questions with regular adjectives }

procedure TfAdj.mOptionsRegExcludeClick(Sender: TObject);

begin
  if mOptionsRegExclude.Checked then
    mOptionsRegExclude.Checked := False
  else
    mOptionsRegExclude.Checked := True;
  bRegExcludeTemp := mOptionsRegExclude.Checked;
end;

{ Menu item "Aide > Aide grammaire": Display grammar help text }

procedure TfAdj.mHelpGrammarClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Aide programme": Display program help text }

procedure TfAdj.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Info grammaire": Display program about }

procedure TfAdj.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Grammaire française.' + LineEnding;
  S += 'Les terminaisons de l’adjectif.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, janvier - avril 2020.';
  MessageDlg('Info "Adjectifs"', S, mtInformation, [mbOK], 0);
end;

{ Button "Commencer/Question/Réponse": Generate new question resp. check user answer }

procedure TfAdj.btQuestionClick(Sender: TObject);

var
  IX, Success: Integer;
  OK: Boolean;

begin
  // Button "Commencer/Question": Generate new question
  if (btQuestion.Caption = 'Commencer') or (btQuestion.Caption = 'Question') then begin
    if mOptionsQTypeFS.Checked or mOptionsQTypeMP.Checked or mOptionsQTypeFP.Checked then begin
      // Proceed if at least one of the adjective forms is selected
      Inc(iQuestion);
      laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
      // Select random adjective (do so, until a valid one has been found)
      repeat
        OK := True;
        IX := Random(iAdjectivesAll);
        if aAdjectivesDone[IX] then                                            // only adjectives not asked so far
          OK := False
        else if bRegExclude then begin
          if (aAdjectives[IX].Feminine = '-') and (aAdjectives[IX].Plural = '-') then  // only irregular adjectives (if this option is selected)
            OK := False;
        end;
      until OK;
      aAdjectivesDone[IX] := True;                                             // mark adjective as "already asked"
      edAdjMS.Text := aAdjectives[IX].Name;                                    // display adjective (masculine singular)
      edAdjFS.Text := ''; edAdjMP.Text := ''; edAdjFP.Text := '';
      edAdjFS.Color := cldefault; edAdjMP.Color := cldefault; edAdjFP.Color := cldefault;
      edEval.Clear;
      DoAdjective(aAdjectives[IX], sFemSg, sMascPl, sFemPl);                   // determine f. sg., m. pl. and f. pl.
      // Enable/disable answer fields depending on adjective forms actually selected
      if mOptionsQTypeFS.Checked then
        edAdjFS.Enabled := True
      else
        edAdjFS.Enabled := False;
      if mOptionsQTypeMP.Checked then
        edAdjMP.Enabled := True
      else
        edAdjMP.Enabled := False;
      if mOptionsQTypeFP.Checked then
        edAdjFP.Enabled := True
      else
        edAdjFP.Enabled := False;
      // Focus the first enabled answer field and remember what field this is
      if mOptionsQTypeFS.Checked then begin
        sEditField := 'FS'; edAdjFS.SetFocus;
      end
      else if mOptionsQTypeMP.Checked then begin
        sEditField := 'MP'; edAdjMP.SetFocus;
      end
      else begin
        sEditField := 'FP'; edAdjFP.SetFocus;
      end;
      // Next button push will be to check the user's answer
      btQuestion.Caption := 'Réponse';
    end
    else
      // Error message, if no adjective form selected
      MessageDlg('Sélection invalide', 'Il faut choisir au moins une terminaison à trouver!', mtError, [mbOK], 0);
  end
  // Button "Réponse": Check user's answer
  else begin
    edEval.Clear;
    if (not mOptionsQTypeFS.Checked or (edAdjFS.Text = sFemSg)) and
       (not mOptionsQTypeMP.Checked or (edAdjMP.Text = sMascPl)) and
       (not mOptionsQTypeFP.Checked or (edAdjFP.Text = sFemPl)) then begin
      // All (selected) adjective forms, entered by the user, are correct
      Inc(iCorrect);
      edEval.Lines.AddText('Réponse correcte');
    end
    else begin
      // At least one of the (selected) adjective forms, entered by the user, is false
      edEval.Lines.AddText('Faux! Réponse correcte:');
      // Highlight the false answer(s) and display the correct one
      if mOptionsQTypeFS.Checked and (edAdjFS.Text <> sFemSg) then begin
        edEval.Lines.AddText('  fém. sg.  = ' + sFemSg);
        edAdjFS.Color := clRed;
      end;
      if mOptionsQTypeMP.Checked and (edAdjMP.Text <> sMascPL) then begin
        edEval.Lines.AddText('  masc. pl. = ' + sMascPl);
        edAdjMP.Color := clRed;
      end;
      if mOptionsQTypeFP.Checked and (edAdjFP.Text <> sFemPl) then begin
        edEval.Lines.AddText('  fém. pl.  = ' + sFemPl);
        edAdjFP.Color := clRed;
      end;
    end;
    // Update the evaluation counters
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iQuestion - iCorrect);
    Success := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Success) + '%';
    // Successfield colored, depending on its actual value
    if Success >= 60 then
      edSuccess.Color := clLime
    else if Success >= 50 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clRed;
    // Reset "current edit field" variable
    sEditField := '';
    // Next button push will be to generate a new question
    btQuestion.Caption := 'Question';
    if iQuestion = iQuestions then begin
      // All questions done: End of exercise
      MessageDlg('Adjectifs français', 'Fin de l''exercice.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Letter-buttons: Insert corresponding letter into actually edited field }

procedure TfAdj.btAcircClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'â');
end;

procedure TfAdj.btEaiguClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'é');
end;

procedure TfAdj.btEgraveClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'è');
end;

procedure TfAdj.btEcircClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ê');
end;

procedure TfAdj.btEtremaClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ë');
end;

procedure TfAdj.btIcircClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'î');
end;

procedure TfAdj.btItremaClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ï');
end;

procedure TfAdj.btUcircClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'û');
end;

procedure TfAdj.btCcedilClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ç');
end;

{ Begin of editing in "Fém. sg." field: Set "actual edit field" variable to corr. value }

procedure TfAdj.edAdjFSEnter(Sender: TObject);

begin
  sEditField := 'FS';
end;

{ Begin of editing in "Masc. pl." field: Set "actual edit field" variable to corr. value }

procedure TfAdj.edAdjMPEnter(Sender: TObject);

begin
  sEditField := 'MP';
end;

{ Begin of editing in "Fém. pl." field: Set "actual edit field" variable to corr. value }

procedure TfAdj.edAdjFPEnter(Sender: TObject);

begin
  sEditField := 'FP';
end;

end.

