{******************************************}
{* Main unit for Substantifs2 application *}
{******************************************}

unit subst2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8, help;

type
  TNoun = record
    Masculine: string;
    Feminine: Char;
  end;
  TNouns = array of TNoun;
  TNounsDone = array of Boolean;
  TIrregular = record
    Masculine, Feminine1, Feminine2: string;
  end;
  TIrregulars = array of TIrregular;
  {**********}
  { TfSubst2 }
  {**********}
  TfSubst2 = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsRegExclude: TMenuItem;
    mHelp, mHelpGrammar, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label5, Label6, Label7, Label8, Label9: TLabel;
    laQuestion: TLabel;
    edMasc, edFem, edEval: TEdit;
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
    sFeminine1, sFeminine2: string;
    bExcludeReg, bExcludeRegTemp: Boolean;
    aNouns: TNouns;
    aNounsDone: TNounsDone;
    aIrregulars: TIrregulars;
  end;

const
  // Lists of nouns with special femininie (irregular nouns are taken from file irreguliers.txt)
  aExceptionsN: array[0..6] of string = (
    'Persan', 'faisan', 'courtisan', 'sultan', 'Lapon', 'artisan', 'vétéran'
  );
  aExceptionsT: array[0..9] of string = (
    'lauréat', 'candidat', 'avocat', 'scélérat', 'bigot', 'manchot', 'dévot', 'idiot', 'cheminot', 'soldat'
  );
  aExceptionsTEUR: array[0..21] of string = (
    'menteur', 'chanteur', 'acheteur', 'batteur', 'abatteur', 'ajusteur', 'basketteur', 'compteur',
    'conteur', 'emprunteur', 'orienteur', 'planteur', 'recruteur', 'sauveteur', 'solliciteur', 'teinteur', 'testeur',
    'traiteur', 'transporteur', 'visiteur', 'lutteur', 'skateur'
  );
  aExceptionsEUR: array[0..0] of string = (
    'ambassadeur'
  );
  aExceptionsEUR2: array[0..8] of string = (
    'pécheur', 'demandeur (juridique)', 'défendeur', 'vendeur (juridique)', 'bailleur (juridique)', 'chasseur (poétique)',
    'charmeur (poétique)', 'enchanteur', 'vengeur'
  );
  aExceptionsEUR3: array[0..3] of string = (
    'supérieur', 'mineur (d''âge)', 'prieur', 'mayeur'
  );
  aExceptionsE: array[0..7] of string = (
    'maître', 'comte', 'pauvre', 'prêtre', 'vicomte', 'contremaître', 'abbé', 'prince'
  );

var
  fSubst2: TfSubst2;

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
      Nouns[NounsAll - 1].Masculine := UTF8Trim(UTF8Copy(Line, 1, 30));        // noun (masculine)
      Nouns[NounsAll - 1].Feminine := UTF8Copy(Line, 31, 1)[1];                // "rule" for feminine
      if Nouns[NounsAll - 1].Feminine <> '-' then
        Inc(NounsIrreg);                                                       // count "not regular" nouns
    end;
  end;
  Close(InFile);
end;

{ Read nouns with irregular feminine from text file }

procedure ReadIrregulars(out Irregulars: TIrregulars);

var
  N, P: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Irregulars, 0); N := 0;
  Assign(InFile, 'irreguliers.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N); SetLength(Irregulars, N);
      P := UTF8Pos(' - ', Line);                                               // separator between masculine and feminine forms
      Irregulars[N - 1].Masculine := UTF8Trim(UTF8Copy(Line, 1, P - 1));       // masculine form
      UTF8Delete(Line, 1, P + 2);
      P := UTF8Pos(',', Line);                                                 // separator, used if there are 2 forms of the feminine
      if P = 0 then begin
        Irregulars[N - 1].Feminine1 := UTF8Trim(Line);                         // (unique) feminine form
        Irregulars[N - 1].Feminine2 := Irregulars[N - 1].Feminine1;
      end
      else begin
        Irregulars[N - 1].Feminine1 := UTF8Trim(UTF8Copy(Line, 1, P - 1));     // feminine, 1st form
        Irregulars[N - 1].Feminine2 := UTF8Trim(UTF8Copy(Line, P + 1, UTF8Length(Line)));  // feminine, 2nd form
      end;
    end;
  end;
end;

{ Insert letter corr. to button pushed into user answer field }

procedure InsertLetter(Letter: string);

// Did not find a way to move cursor to end of text (???)

begin
  fSubst2.edFem.Text := fSubst2.edFem.Text + Letter;
  fSubst2.edFem.SetFocus;
end;

{ Check if a given noun is part of a given exception list }

function IsInExceptionList(var EList: array of string; Noun: string): Boolean;

var
  I: Integer;
  ItIs: Boolean;

begin
  ItIs := False;
  for I := 0 to Length(EList) - 1 do begin
    if Noun = EList[I] then
      ItIs := True;
  end;

  Result := ItIs;
end;

{ Determine feminine suffix for given noun }
{ ---------------------------------------- }

procedure FeminineOfNoun(Noun: TNoun; var Irregulars: TIrregulars; out Fem1, Fem2: string);

var
  I, P: Integer;
  Masc: string;

begin
  Masc := Noun.Masculine; Fem1 := ''; Fem2 := '';
  // Remove noun info (text between brackets)
  P := UTF8Pos(' (', Masc);
  if P <> 0 then
    Masc := UTF8Copy(Masc, 1, P - 1);
  if UpperCase(Noun.Feminine) = 'I' then begin
    // Irregular feminine: Get feminine form(s) from text file data
    for I := 0 to Length(Irregulars) - 1 do begin
      if Noun.Masculine = Irregulars[I].Masculine then begin
        Fem1 := 'une ' + Irregulars[I].Feminine1;
        Fem2 := 'une ' + Irregulars[I].Feminine2;
      end;
    end;
  end
  else if (UpperCase(Noun.Feminine) = 'N') or (UpperCase(Noun.Feminine) = 'C') then begin
    // Nouns with no feminine or feminine equal to masculie
    Fem1 := Masc;
    if UpperCase(Noun.Feminine) = 'N' then
      // Nouns with no feminine
      Fem1 := 'un ' + Fem1
    else
      // Nouns with feminine = masculine
      Fem1 := 'une ' + Fem1;
    Fem2 := Fem1;
  end
  else if UpperCase(Noun.Feminine) = 'P' then begin
    // Professions with no feminine (feminisation of nouns is only partial, esp. in Europe)
    Fem1 := 'un ' + Masc;
    Fem2 := 'une femme ' + Masc;
  end
  else if UpperCase(Noun.Feminine) = 'M' then begin
    // Professions with noun feminisation
    Fem1 := 'une ' + Masc; Fem2 := Fem1;
    if (Noun.Feminine <> 'm') and (UTF8Copy(Fem1, UTF8Length(Fem1), 1) <> 'e') then
      // "m" indicates not to add the final -e (cf. "médecin")
      Fem2 += 'e';
  end
  else begin
    // All other cases: special ruled or regular feminine
    if (UpperCase(Noun.Feminine) = 'R') or (UpperCase(Noun.Feminine) = 'L') then begin
      // Special feminine rule, depending on masculine suffix
      // ----------------------------------------------------
      if (UTF8Copy(Masc, UTF8Length(Masc), 1) = 'f')  or (UTF8Copy(Masc, UTF8Length(Masc), 1) = 'p') then begin
        // Nouns in -f or -p: -> -ve
        Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 1) + 've';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc) - 1, 2) = 'el' then begin
        // Nouns in -el: -> -elle
        Fem1 := Masc + 'l' + 'e';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc), 1) = 'x' then begin
        // Nouns in -x: -> se
        Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 1) + 'se';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc) - 1, 2) = 'er' then begin
        // Nouns in -er: -> ère
        Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 2) + 'ère';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc) - 2, 3) = 'eau' then begin
        // Nouns in -eau: -> -elle
        Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 3) + 'elle';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc) - 3, 4) = 'teur' then begin
        // Nouns in -teur: -> trice (or, if in exception list, -> teuse)
        if IsInExceptionList(aExceptionsTEUR, Noun.Masculine) then
          Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 4) + 'teuse'
        else
          Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 4) + 'trice'
      end
      else if UTF8Copy(Masc, UTF8Length(Masc) - 2, 3) = 'eur' then begin
        // Other nouns in -eur: -> euse (several other suffixes, if in one of the exception lists)
        if IsInExceptionList(aExceptionsEUR, Noun.Masculine) then begin
          // Feminie in -trice
          if UTF8Copy(Masc, UTF8Length(Masc) - 3, 1) = 'd' then
            // If the masculine ends in -deuse, the "d" is dropped
            Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 4) + 'trice'
          else
            Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 3) + 'trice';
        end
        else if IsInExceptionList(aExceptionsEUR2, Noun.Masculine) then begin
          // Feminine in -eresse
          if Masc = 'défendeur' then
            // Exception noun
            Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 3) + 'resse'
          else
            Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 3) + 'eresse';
        end
        else if IsInExceptionList(aExceptionsEUR3, Noun.Masculine) then
          // Feminine in -e
          Fem1 := Masc + 'e'
        else
          // "Normal" case: Feminine in -euse
          Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 3) + 'euse';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc), 1) = 't' then begin
        // Nouns in -t: -> -tte (of, if in exception list, -> -te)
        if IsInExceptionList(aExceptionsT, Noun.Masculine) then
          Fem1 := Masc + 'e'
        else
          Fem1 := Masc + 't' + 'e';
      end
      else if UTF8Copy(Masc, UTF8Length(Masc), 1) = 'n' then begin
        // Nouns in -n: -> -nne (except for nouns in -in and those in exception list: -> -ne)
        if (UTF8Copy(Masc, UTF8Length(Masc) - 1, 2) = 'in') or IsInExceptionList(aExceptionsN, Noun.Masculine) then
          Fem1 := Masc + 'e'
        else
          Fem1 := Masc + 'n' + 'e';
      end
      else if (UTF8Copy(Masc, UTF8Length(Masc), 1) = 'e') or (UTF8Copy(Masc, UTF8Length(Masc), 1) = 'é') then begin
        // Irregular nouns in -e and -é (regular ones are tagged '-'): -> -esse
        if IsInExceptionList(aExceptionsE, Noun.Masculine) then
          Fem1 := UTF8Copy(Masc, 1, UTF8Length(Masc) - 1) + 'esse';
      end;
      if (Noun.Feminine = 'l') or (Noun.Feminine = 'r') then begin
        // Lowecase tags mean that there are two forms of the feminine for this noun
        // These cases are hard-coded here
        if Masc = 'inventeur' then
          Fem2 := 'inventeuse'
        else if Masc = 'reporteur' then
          Fem2 := 'reporteuse';
      end;
    end
    else begin
      // Regular feminine: -> -e
      Fem1 := Masc;
      if UTF8Copy(Fem1, UTF8Length(Fem1), 1) <> 'e' then
        Fem1 += 'e';
    end;
    Fem1 := 'une ' + Fem1;
    // If there has no 2nd feminine form been given before, i.e. there is only one -> set 2nd form = 1st form
    if Fem2 = '' then
      Fem2 := Fem1;
  end;
end;

{**********}
{ TfSubst2 }
{**********}

{ Application start: Initialisation }

procedure TfSubst2.FormCreate(Sender: TObject);

begin
  Randomize;
  ReadNouns(aNouns, iNounsAll, iNounsIrreg);
  SetLength(aNounsDone, iNounsAll);
  ReadIrregulars(aIrregulars);
  iQuestionsTemp := 20; bExcludeRegTemp := False;
  mExerciseNew.Click;                                                          // start a new exercise
end;

{ Menu item "Exercice > Nouveau": Prepare for new exercise }

procedure TfSubst2.mExerciseNewClick(Sender: TObject);

var
  I: Integer;

begin
  laQuestion.Caption := 'Question:';
  edMasc.Text := ''; edFem.Text := ''; edEval.Text := ''; edEval.Color := clDefault;
  edQuestions.Text := ''; edCorrect.Text := ''; edFalse.Text := '';
  edSuccess.Text := ''; edSuccess.Color := clDefault;
  iQuestions := iQuestionsTemp;                                                // number of questions selected now becomes active
  bExcludeReg := bExcludeRegTemp;                                              // idem for option "exclude regular feminines"
  if bExcludeReg and (iQuestions > iNounsIrreg) then                           // number of questions can't be greater than number of (not regular) nouns
    iQuestions := iNounsIrreg;
  iQuestion := 0; iCorrect := 0;
  for I := 0 to iNounsAll - 1 do
    aNounsDone[I] := False;                                                    // mark all nouns as "not yet asked"
  btQuestion.Caption := 'Commencer'; btQuestion.Enabled := True;
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfSubst2.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Nombre de questions...": User entry of number of exercise questions }

procedure TfSubst2.mOptionsQuestionsClick(Sender: TObject);

var
  QT: Integer;
  S: string;

begin
  S := InputBox('Substantifs français.', 'Nombre de questions', IntToStr(iQuestions));
  if S <> '' then begin
    QT := StrToInt(S);
    if QT < 10 then                                                            // minimum of questions arbitrarily fixed to 10
      QT := 10
    else if QT > iNounsAll then                                                // number of questions can't be greater than number of nouns
      QT := iNounsAll;
    iQuestionsTemp := QT;                                                      // number of questions will become active, when user chooses to do a new exercise
  end;
end;

{ Menu item "Options > Exclure substantifs réguliers": Toggle questions/no questions with regular nouns }

procedure TfSubst2.mOptionsRegExcludeClick(Sender: TObject);

begin
  if mOptionsRegExclude.Checked then
    mOptionsRegExclude.Checked := False
  else
    mOptionsRegExclude.Checked := True;
  bExcludeRegTemp := mOptionsRegExclude.Checked;
end;

{ Menu item "Aide > Aide grammaire": Display grammar help text }

procedure TfSubst2.mHelpGrammarClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Font.Size := fHelp.Font.Size - 1;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Aide programme": Display program help text }

procedure TfSubst2.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Font.Size := fHelp.Font.Size;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Aide > Info programme": Display program about }

procedure TfSubst2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Grammaire française.' + LineEnding;
  S += 'Le féminin des substantifs.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, juillet 2020.';
  MessageDlg('Info "Substantifs2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Commencer/Question/Réponse": Generate new question resp. check user answer }

procedure TfSubst2.btQuestionClick(Sender: TObject);

var
  IX, Success: Integer;
  OK: Boolean;

begin
  // Button "Commencer/Question": Generate new question
  if (btQuestion.Caption = 'Commencer') or (btQuestion.Caption = 'Question') then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    edFem.Text := ''; edEval.Text := ''; edEval.Color := clDefault;
    // Select random noun (do so, until a valid one has been found)
    repeat
      OK := True;
      IX := Random(iNounsAll);
      if aNounsDone[IX] then                                                   // only nouns not asked so far
        OK := False
      else if bExcludeReg and (aNouns[IX].Feminine = '-') then                 // only irregular nouns (if this option is selected)
        OK := False;
    until OK;
    aNounsDone[IX] := True;                                                    // mark this noun as done
    edMasc.Text := 'un ' + aNouns[IX].Masculine;                               // display noun (masculine)
    FeminineOfNoun(aNouns[IX], aIrregulars, sFeminine1, sFeminine2);           // determine the noun's feminine
    if (sFeminine1 = 'un ') or (sFeminine1 = 'une ') then begin
      // No feminine found: This should normally not happen!
      MessageDlg('Program error', 'Could not determine feminine of ' + aNouns[IX].Masculine + '!', mtError, [mbOK], 0);
      Halt;                                                                    // this aborts the application
    end;
    edFem.SetFocus;
    // Next button push will be to check the user's answer
    btQuestion.Caption := 'Réponse';
  end
  // Button "Réponse": Check user's answer
  else begin
    if (edFem.Text = sFeminine1) or (edFem.Text = sFeminine2) then begin
      // User answer is correct
      Inc(iCorrect);
      edEval.Text := 'Réponse correcte'; edEval.Color := clLime;
    end
    else begin
      // User answer is false
      edEval.Text := 'Faux! Réponse correcte: ' + sFeminine1; edEval.Color := clRed;
      if sFeminine2 <> sFeminine1 then
        edEval.Text := edEval.Text + ' / ' + sFeminine2;
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

procedure TfSubst2.btAcircClick(Sender: TObject);

begin
  InsertLetter('â');
end;

procedure TfSubst2.btEaiguClick(Sender: TObject);

begin
  InsertLetter('é');
end;

procedure TfSubst2.btEgraveClick(Sender: TObject);

begin
  InsertLetter('è');
end;

procedure TfSubst2.btEcircClick(Sender: TObject);

begin
  InsertLetter('ê');
end;

procedure TfSubst2.btEtremaClick(Sender: TObject);

begin
  InsertLetter('ë');
end;

procedure TfSubst2.btIcircClick(Sender: TObject);

begin
  InsertLetter('î');
end;

procedure TfSubst2.btItremaClick(Sender: TObject);

begin
  InsertLetter('ï');
end;

procedure TfSubst2.btUcircClick(Sender: TObject);

begin
  InsertLetter('û');
end;

procedure TfSubst2.btCcedilClick(Sender: TObject);

begin
  InsertLetter('ç');
end;

end.

