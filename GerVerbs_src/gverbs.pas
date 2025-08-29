{**************************************}
{* Main unit for GerVerbs application *}
{**************************************}

unit gverbs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8, gconj, help;

type
  TVerb = record
    VLevel, VInfinitive, VPresent, VPast1, VPast2, VPerfect1, VPerfect2, VPrefix1, VPrefix2, VNote: string;
    VConjugationPT, VAuxiliary1, VAuxiliary2, VSeparation, VPersons: Char;
    VPronominal: Boolean;
  end;
  TVerbs = array of TVerb;
  {**********}
  { TfGVerbs }
  {**********}
  TfGVerbs = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExercise, mFileConj, mFileExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsP100: TMenuItem;
    mSettingsLevel, mSettingsTenses, mSettingsCompound: TMenuItem;
    mSettingsLevelA1, mSettingsLevelA2, mSettingsLevelB1: TMenuItem;
    mSettingsLevelB2, mSettingsLevelC1, mSettingsLevelC2: TMenuItem;
    mSettingsLevelAll, mSettingsLevelNone: TMenuItem;
    mSettingsTensesPT, mSettingsTensesSP, mSettingsTensesPF: TMenuItem;
    MenuItem2, MenuItem3, MenuItem4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, Label4, Label5, Label6, Label7: TLabel;
    laConj, laQuestion: TLabel;
    edVerb, edConj, edAnswer, edEval: TEdit;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAe, btOe, btUe, btSs: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExerciseClick(Sender: TObject);
    procedure mFileConjClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsLevelA1Click(Sender: TObject);
    procedure mSettingsLevelA2Click(Sender: TObject);
    procedure mSettingsLevelB1Click(Sender: TObject);
    procedure mSettingsLevelB2Click(Sender: TObject);
    procedure mSettingsLevelC1Click(Sender: TObject);
    procedure mSettingsLevelC2Click(Sender: TObject);
    procedure mSettingsLevelAllClick(Sender: TObject);
    procedure mSettingsLevelNoneClick(Sender: TObject);
    procedure mSettingsTensesPTClick(Sender: TObject);
    procedure mSettingsTensesSPClick(Sender: TObject);
    procedure mSettingsTensesPFClick(Sender: TObject);
    procedure mSettingsCompoundClick(Sender: TObject);
    procedure mSettingsP100Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAeClick(Sender: TObject);
    procedure btOeClick(Sender: TObject);
    procedure btUeClick(Sender: TObject);
    procedure btSsClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    iSQuestions, iQuestions, iMaxMark, iQuestion, iCorrect, iFalse: Integer;
    sLevels, sTenses, sAnswer1, sAnswer2: string;
    bCompound: Boolean;
    rHaben, rSein: TVerb;
    aVerbs: TVerbs;
  end;

const
  NLevels = 6;
  Levels: array[1 .. NLevels] of string = ('A1', 'A2', 'B1', 'B2', 'C1', 'C2');
  Tenses: array[0..2] of string = ('Präsens', 'Präteritum', 'Perfekt');
  Numbers: array[0 .. 1] of string = ('Sg', 'Pl');
  Genuus: array[0..2] of string = ('m', 'w', 'n');

var
  fGVerbs: TfGVerbs;

implementation

{$R *.lfm}

{ Read verbs from text file and fill them into an array of TVerb records }

procedure ReadVerbs(out Verbs: TVerbs);

var
  N, P, I: Integer;
  Filename, Line, S: string;
  InFile: Text;

begin
  N := 0;
  for I := 1 to NLevels do begin
    Filename := './verbs/' + Levels[I] + '.txt'; DoDirSeparators(Filename);
    Assign(InFile, Filename); Reset(InFile);
    while not EoF(InFile) do begin
      Readln(InFile, Line);
      if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin                           // lines starting with '#' are comments
        S := UTF8Trim(UTF8Copy(Line, 1, 20));
        if (RightStr(S, 2) <> '_?') and (RightStr(S, 2) <> '_D') then begin              // verbs with tags _D and _? are to be ignored
          Inc(N); SetLength(Verbs, N);
          // Fill in the TVerb record
          with Verbs[N - 1] do begin
            VLevel := Levels[I];
            // Check if verb is pronominal
            VPronominal := False;
            P := Pos('sich ', S);
            if P <> 0 then begin
              VPronominal := True;
              S := Copy(S, P + 5, Length(S));
            end;
            // Check if verb is a compound verb
            VPrefix1 := ''; VPrefix2 := '';
            P := Pos('-', S);
            if P <> 0 then begin
              // Compound verb
              VPrefix1 := LeftStr(S, P - 1);
              S := Copy(S, P + 1, Length(S));
              P := Pos('-', S);
              if P <> 0 then begin
                // Verb with 2 prefixes
                VPrefix2 := LeftStr(S, P - 1);
                S := Copy(S, P + 1, Length(S));
              end;
            end;
            VInfinitive := S;
            VPresent := ''; VPast1 := ''; VPast2 := ''; VPerfect1 := ''; VPerfect2 := ''; VNote := '';
            // Simple verbs
            if VPrefix1 = '' then begin
              VPresent := UTF8Trim(UTF8Copy(Line, 21, 20));
              S := UTF8Trim(UTF8Copy(Line, 41, 20));
              // Check if there are 2 forms for simple past
              P := Pos('/', S);
              if P = 0 then begin
                VPast1 := S; VPast2 := '';
              end
              else begin
                VPast1 := LeftStr(S, P - 1); VPast2 := Copy(S, P + 1, Length(S));
              end;
              S := UTF8Trim(UTF8Copy(Line, 61, 20));
              // Check if there are 2 forms for past participle
              P := Pos('/', S);
              if P = 0 then begin
                VPerfect1 := S; VPerfect2 := '';
              end
              else begin
                VPerfect1 := LeftStr(S, P - 1); VPerfect2 := Copy(S, P + 1, Length(S));
              end;
              VSeparation := '-';                                                        // simple verbs will be indicated by VSeparation = '-'
              VConjugationPT := UTF8Copy(Line, 81, 1)[1];
            end
            // Compound verbs
            else begin
              VConjugationPT := '?';                                                     // conjugation depends on corresponding simple verb
              VSeparation := UTF8Copy(Line, 81, 1)[1];
            end;
            // Auxiliaries
            if UTF8Copy(Line, 83, 1) = 'X' then begin
              VAuxiliary1 := 'H'; VAuxiliary2 := 'S';
            end
            else begin
              VAuxiliary1 := UTF8Copy(Line, 83, 1)[1]; VAuxiliary2 := VAuxiliary1;
            end;
            // Persons to be used in exercises
            VPersons := UTF8Copy(Line, 85, 1)[1];
            // Note for verbs where different conjugation forms correspond to different meanings
            if Length(Line) > 85 then
              VNote := UTF8Trim(UTF8Copy(Line, 86, UTF8Length(Line)));
          end;
        end;
      end;
    end;
    Close(InFile);
  end;
end;

{ Get auxiliary verbs "haben" and "sein" (as TVerb records) }

procedure GetAux(var Verbs: TVerbs; out Haben, Sein: TVerb);

var
  I: Integer;

begin
  for I := 0 to Length(Verbs) - 1 do begin
    if (Verbs[I].VInfinitive = 'haben') and (Verbs[I].VSeparation = '-') then
      Haben := Verbs[I]
    else if (Verbs[I].VInfinitive = 'sein') and (Verbs[I].VSeparation = '-') then
      Sein := Verbs[I];
  end;
end;

{ Prepare a new exercise (clear form and reset counters) }

procedure NewExercise(SQuestions: Integer; var Questions, Question, QCorrect, QFalse: Integer);

begin
  fGVerbs.laQuestion.Caption := 'Frage';
  fGVerbs.edVerb.Text := '';
  fGVerbs.edConj.Text := ''; fGVerbs.edAnswer.Text := ''; fGVerbs.edEval.Text := '';
  fGVerbs.edQuestions.Text := ''; fGVerbs.edCorrect.Text := ''; fGVerbs.edFalse.Text := '';
  fGVerbs.edSuccess.Text := ''; fGVerbs.edSuccess.Color := clCream;
  fGVerbs.btQuestion.Caption := 'Frage';
  Questions := SQuestions;                                                               // selected number of questions now becomes active
  Question := 0; QCorrect := 0; QFalse := 0;
end;

{ Get simple verb from which compound verb is formed }

procedure GetSimpleVerb(var Verbs: TVerbs; Verb:TVerb; out SimpleVerb: TVerb; out Mess: string);

var
  I: Integer;
  Found: Boolean;

begin
  Mess := '';
  I := 0; Found := False;
  while (I < Length(Verbs)) and not Found do begin
    if Verb.VInfinitive = Verbs[I].VInfinitive then
      Found := True;
    Inc(I);
  end;
  if Found then
    SimpleVerb := Verbs[I - 1]
  else
    Mess := 'Konjugation des zusammengesetzten Verbs konnte nicht ermittelt werden!';
end;

{ German verbs irregular present tense conjugation routine }

procedure ConjugationPTIrreg(Verb: TVerb; XPerson: Integer; out Conj, Mess: string);

var
  Filename, Line: string;
  InFile: Text;

// Conjugation is directly read from file: 1 specific file for each (simple) irregular verb

begin
  Conj := ''; Mess := '';
  Filename := Verb.VInfinitive;
  Filename := StringReplace(Filename, 'ä', 'ae', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ö', 'oe', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ü', 'ue', [rfReplaceAll]);
  Filename := './verbs/' + Filename + '.txt'; DoDirSeparators(Filename);
  if FileExists(Filename) then begin
    Assign(InFile, Filename); Reset(InFile);
    while not EoF(InFile) do begin
      Readln(INFile, Line);
      if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
        if XPerson = StrToInt(LeftStr(Line, 1)) then
          Conj := UTF8Trim(UTF8Copy(Line, 3, UTF8Length(Line) - 2));
      end;
    end;
    if RightStr(Conj, 1) = '*' then                                                      // remove '*' tag (actually not used)
      Conj := UTF8Copy(Conj, 1, UTF8Length(Conj) - 1);
    Close(InFile);
  end
  else
    Mess := 'Verbdatei ' + Filename + ' nicht gefunden!';
end;

{ German verbs present tense conjugation routine }

procedure ConjugationPT(Verb: TVerb; XPerson: Integer; out Conj, Mess: string);

const
  Suffixes: array[1 .. 6] of string = ('e', 'st', 't', 'en', 't', 'en');

var
  Stem: string;

// The routine is for simple verbs (prefixes of compound verbs will be added later)

begin
  Conj := ''; Mess := '';
  if Verb.VConjugationPT = 'U' then
    // Verbs with irregular present tense
    ConjugationPTIrreg(Verb, XPerson, Conj, Mess)
  else begin
    // Verbs with regular present tense
    if RightStr(Verb.VInfinitive, 2) = 'en' then
      Stem := UTF8Copy(Verb.VInfinitive, 1, UTF8Length(Verb.VInfinitive) - 2)
    else
      Stem := UTF8Copy(Verb.VInfinitive, 1, UTF8Length(Verb.VInfinitive) - 1);
    if (RightStr(Stem, 1) = 't') or (RightStr(Stem, 1) = 'd') then begin
      // For stems ending in -t/-d, an 'e' is inserted before a -t/-st suffix
      // Examples: du bietest, er bietet, ihr bietet; du findest, er findet, ihr findet
      if (Suffixes[XPerson] = 't') or (Suffixes[XPerson] = 'st') then
        Stem += 'e';
    end;
    if not ((XPerson = 2) or (XPerson = 3)) then begin
      // 1st pers sg and all pers pl always formed with unchanged stem
      Conj := Stem + Suffixes[XPerson];
    end
    else begin
      // 2nd and 3rd pers sg are formed with modified stem for strong verbs
      if Verb.VConjugationPT = 'R' then begin                                            // 'R' indicates weak verb (for present tense)
        // Unchanged stem for weak verbs
        Conj := Stem + Suffixes[XPerson];
      end
      else begin                                                                         // 'S' indicates strong verb (for present tense)
        // Changed stem for strong verbs
        if XPerson = 3 then
          // 3rd pers sg: Conjugation from verbs file
          Conj := Verb.VPresent
        else
          // 2nd person: use 3rd pers conjugation with adequate suffix
          Conj := UTF8Copy(Verb.VPresent, 1, UTF8Length(Verb.VPresent) - 1) + 'st';
      end;
    end;
  end;
end;

{ German verbs simple past conjugation routine }

procedure ConjugationSP(Verb: TVerb; XPerson: Integer; out Conj1, Conj2, Mess: string);

const
  Suffixes: array[1 .. 6] of string = ('', 'st', '', 'en', 't', 'en');

var
  Stem1, Stem2: string;

// The routine is for simple verbs (prefixes of compound verbs will be added later)
// The term "stem" is used here to designate the simple past stem for 3rd pers. sg. (that is indicated in the verb list file)

begin
  Conj1 := ''; Conj2 := ''; Mess := '';
  Stem1 := Verb.VPast1; Stem2 := Stem1;
  if Verb.VPast2 <> '' then                                                              // verbs with 2 forms of past tense
    Stem2 := Verb.VPast2;
  if (RightStr(Stem1, 2) = 'te') or (RightStr(Stem1, 1) = 'de') then begin
    // For stems ending in -te/-de, the 'e' is removed before an -en suffix
    // Examples: wir brachten; sie durften
    if Suffixes[XPerson] = 'en' then
      Stem1 := UTF8Copy(Stem1, 1, UTF8Length(Stem1) - 1);
  end
  else if (RightStr(Stem1, 1) = 't') or (RightStr(Stem1, 1) = 'd') then begin
    // For stems ending in -t/-d, an 'e' is inserted before a -t suffix
    // Examples: ihr tatet, ihr fandet
    if Suffixes[XPerson] = 't' then
      Stem1 += 'e';
  end;
  if (RightStr(Stem2, 2) = 'te') or (RightStr(Stem2, 1) = 'de') then begin
    if Suffixes[XPerson] = 'en' then
      Stem2 := UTF8Copy(Stem2, 1, UTF8Length(Stem2) - 1);
  end
  else if (RightStr(Stem2, 1) = 't') or (RightStr(Stem2, 1) = 'd') then begin
    if Suffixes[XPerson] = 't' then
      Stem2 += 'e';
  end;
  Conj1 := Stem1 + Suffixes[XPerson];
  Conj2 := Stem2 + Suffixes[XPerson];
end;

{ German verbs past participle conjugation routine }

procedure ConjugationPP(Verb: TVerb; out Conj1, Conj2, Mess: string);

// The routine is for simple verbs (prefixes of compound verbs will be added later)
// The past participle is indicated in the verb list file

begin
  Conj1 := Verb.VPerfect1; Conj2 := Conj1; Mess := '';
  if Verb.VPerfect2 <> '' then
    Conj2 := Verb.VPerfect2;                                                             // verbs with 2 forms of past participle
end;

{ German verbs main conjugation routine }

procedure Conjugation(var Verbs: TVerbs; Verb, VHaben, VSein: TVerb; Tense, Number: String; Person: Integer; Genus: string; out Conj1, Conj2, Mess: string);

const
  PronounsPers:   array[1 .. 6] of string = ('ich', 'du', 'er', 'wir', 'ihr', 'sie');
  PronounsReflex: array[1 .. 6] of string = ('mich', 'dich', 'sich', 'uns', 'euch', 'sich');

var
  XP: Integer;
  Pronoun, Auxiliary1, Auxiliary2: string;
  SimpleVerb: TVerb;

// The routine conjugates the simple verb, from wich the verb is formed,
// then adds the compound verb prefixes, the personal and reflexive pronouns

begin
  Conj1 := ''; Conj2 := ''; Mess := '';
  if Number = 'Sg' then
    XP := Person
  else
    XP := 3 + Person;
  // Conjugate the simple (not compound) verb
  if Verb.VPrefix1 = '-' then
    SimpleVerb := Verb
  else begin
    Verb.VInfinitive :=  StringReplace(Verb.VInfinitive, '_a', '', []);                  // remove tags
    Verb.VInfinitive :=  StringReplace(Verb.VInfinitive, '_b', '', []);
    GetSimpleVerb(Verbs, Verb, SimpleVerb, Mess);                                        // determine simple verb, from which the verb is formed
  end;
  if Mess = '' then begin
    // Conjugation for present tense, simple past or perfect (conjugate past participle in this case)
    SimpleVerb.VInfinitive := StringReplace(SimpleVerb.VInfinitive, '_1', '', []);       // remove tags
    SimpleVerb.VInfinitive := StringReplace(SimpleVerb.VInfinitive, '_2', '', []);
    if Tense = Tenses[0] then begin
      ConjugationPT(SimpleVerb, XP, Conj1, Mess); Conj2 := Conj1;                        // present tense
    end
    else if Tense = Tenses[1] then begin
      ConjugationSP(SimpleVerb, XP, Conj1, Conj2, Mess);                                 // simple past
    end
    else begin
      ConjugationPP(SimpleVerb, Conj1, Conj2, Mess);                                     // past participle
    end;
  end;
  if Mess = '' then begin
    // Present tense and simple past: Add reflexive pronoun for pronominal verbs
    if (Tense = Tenses[0]) or (Tense = Tenses[1]) then begin
      if Verb.VPronominal then begin
        Conj1 += ' ' + PronounsReflex[XP]; Conj2 += ' ' + PronounsReflex[XP];
      end;
    end;
    // Add the prefix(es) for compound verbs
    if Verb.VSeparation <> '-' then begin
      if Verb.VPrefix2 = '' then begin
        // Verb with 1 prefix
        if Verb.VSeparation = 'N' then begin
          // Un-separable prefix
          if Tense = Tenses[2] then begin
            // Perfect: Remove 'ge' (will be replaced by un-separable prefix)
            if LeftStr(Conj1, 2) = 'ge' then
              Conj1 := StringReplace(Conj1, 'ge', '', []);
            if LeftStr(Conj2, 2) = 'ge' then
              Conj2 := StringReplace(Conj2, 'ge', '', []);
          end;
          Conj1 := Verb.VPrefix1 + Conj1; Conj2 := Verb.VPrefix1 + Conj2;
        end
        else begin
          // Separable prefix
          if (Tense = Tenses[0]) or (Tense = Tenses[1]) then begin
            // Present tense and simple past: Separable prefix after simple verb form
            Conj1 += ' ' + Verb.VPrefix1; Conj2 += ' ' + Verb.VPrefix1;
          end
          else begin
            // Perfect: Separable prefix before simple verb form
            Conj1 := Verb.VPrefix1 + Conj1; Conj2 := Verb.VPrefix1 + Conj2;
          end;
        end;
      end
      else begin
        // Verb with 2 prefixes (2nd one being supposed to be un-separable!)
        if Tense = Tenses[2] then begin
          // Perfect: Remove 'ge' (will be replaced by un-separable prefix)
          if LeftStr(Conj1, 2) = 'ge' then
            Conj1 := StringReplace(Conj1, 'ge', '', []);
          if LeftStr(Conj2, 2) = 'ge' then
            Conj2 := StringReplace(Conj2, 'ge', '', []);
        end;
        Conj1 := Verb.VPrefix2 + Conj1;  Conj2 := Verb.VPrefix2 + Conj2;
        if Verb.VSeparation = 'N' then begin
          // Un-separable (1st) prefix
          Conj1 := Verb.VPrefix1 + Conj1; Conj2 := Verb.VPrefix1 + Conj2;
        end
        else begin
          // Separable (1st) prefix
          if (Tense = Tenses[0]) or (Tense = Tenses[1]) then begin
            // Present tense and simple past: Separable prefix after simple verb form
            Conj1 += ' ' + Verb.VPrefix1; Conj2 += ' ' + Verb.VPrefix1;
          end
          else begin
            // Perfect: Separable prefix before simple verb form
            Conj1 := Verb.VPrefix1 + Conj1; Conj2 := Verb.VPrefix1 + Conj2;
          end;
        end;
      end;
    end;
    if Tense = Tenses[2] then begin
      // Perfect: Conjugate auxiliary (or auxiliaries)
      if (Verb.VAuxiliary2 = '') or (Verb.VAuxiliary1 = Verb.VAuxiliary2) then begin
        // Verbs with "haben" or "sein"
        if Verb.VAuxiliary1 = 'H' then
          ConjugationPT(VHaben, XP, Auxiliary1, Mess)
        else
          ConjugationPT(VSein, XP, Auxiliary1, Mess);
        Auxiliary2 := Auxiliary1;
      end
      else begin
        // Verbs with "haben" and "sein"
        ConjugationPT(VHaben, XP, Auxiliary1, Mess);
        ConjugationPT(VSein, XP, Auxiliary2, Mess);
      end;
    end;
  end;
  if Mess = '' then begin
    if Tense = Tenses[2] then begin
      // Perfect: Add reflexive pronoun for pronominal verbs
      if Verb.VPronominal then begin
        Auxiliary1 += ' ' + PronounsReflex[XP]; Auxiliary2 += ' ' + PronounsReflex[XP];
      end;
      // Perfect: Add auxiliary to past participle
      Conj1 := Auxiliary1 + ' ' + Conj1; Conj2 := Auxiliary2 + ' ' + Conj2;
    end;
    // Add personal pronoun
    Pronoun := PronounsPers[XP];
    if (Number = 'Sg') and (Person = 3) then begin
      // 3rd pers. sg. pronoun depends on genus
      if Genus = 'w' then
        Pronoun := 'sie'
      else if Genus = 'n' then
        Pronoun := 'es';
    end;
    Conj1 := Pronoun + ' ' + Conj1; Conj2 := Pronoun + ' ' + Conj2;
  end;
end;

{**********}
{ TfGVerbs }
{**********}

{ Application start: Initialisation }

procedure TfGVerbs.FormCreate(Sender: TObject);

begin
  SetLength(aVerbs, 0);
  ReadVerbs(aVerbs);                                                                     // read complete verb list into an array of TVerb records
  GetAux(aVerbs, rHaben, rSein);                                                         // get the 2 auxiliaries (as TVerb record)
  // Exercise startup parameters
  iSQuestions := 20;
  sLevels := 'A1A2B1B2C1C2';                                                             // string, containing the levels to be used
  sTenses := Tenses[0] + Tenses[1] + Tenses[2];                                          // string, containing the tenses to be used
  bCompound := True;                                                                     // usage or not of compound verbs
  iMaxMark := 60;
  NewExercise(iSQuestions, iQuestions, iQuestion, iCorrect, iFalse);                     // prepare for new exercise
  Randomize;
end;

{ Menu item "Datei > Neue Aufgabe": Prepare for a new exercise }

procedure TfGVerbs.mFileExerciseClick(Sender: TObject);

begin
  NewExercise(iSQuestions, iQuestions, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Datei > Konjugation": Conjugate the verb entered by the user (only if it is part of the list!) }

procedure TfGVerbs.mFileConjClick(Sender: TObject);

var
  Person, IX, I, J, K, L, L0: Integer;
  VerbName, VerbName0, VerbName2, Number, Genus, Conj1, Conj2, Mess, Sep: string;
  ConjError, Pronominal: Boolean;
  Verb: TVerb;
  Verbs: array[0..1] of TVerb;

{ The routine conjugates the verb and fills in the string-grids on the fConj form }

begin
  // Get the verb to conjugate from user
  VerbName0 := InputBox('Deutsche Verben', 'Zu konjugierendes Verb', ''); VerbName := VerbName0;
  // Conjugate the verb (only if it is part of the list)
  if (VerbName <> '') and (Length(VerbName) >= 3) then begin
    Pronominal := False;
    if LeftStr(VerbName, 5) = 'sich ' then begin
      // Verb is a pronominal verb
      VerbName := StringReplace(VerbName, 'sich ', '', []);
      Pronominal := True;
    end;
    // Check if the program knows this verb (i.e. if it is one of those in the list read from the verb files)
    I := 0; IX := -1;
    while (IX = -1) and (I <= Length(aVerbs) - 1) do begin
      VerbName2 := aVerbs[I].VPrefix1 + aVerbs[I].VPrefix2 + aVerbs[I].VInfinitive;
      if UTF8Copy(VerbName2, UTF8Length(VerbName2) - 1, 1) = '_' then
        VerbName2 := UTF8Copy(VerbName2, 1, UTF8Length(VerbName2) - 2);                  // remove tags
      if VerbName = VerbName2 then begin
        // Actually just conjugating verbs as given in list
        // (in particular not conjugating pronominal voice of not-mandatory-pronominal verbs)
        if Pronominal = aVerbs[I].VPronominal then
          IX := I;
      end;
      Inc(I);
    end;
    if IX = -1 then begin
      // Verb has not been found in the list: Display message (no conjugation)
      MessageDlg('Deutsche Verben"', 'Sorry, unbekanntes Verb: ' + VerbName0, mtError, [mbOK], 0);
    end
    else begin
      // Verb has been found in the list: all data for correct conjugation is available in the array
      // created from the verbs file (or a specific verb file for irregular verbs)
      // For verbs with 2 forms (different simple pasts / past participles; different prefix types),
      // consider both forms (consideration of conjugation with both auxiliaries will be done later)
      Verbs[0] := aVerbs[IX];
      if (UTF8Copy(Verbs[0].VInfinitive, UTF8Length(Verbs[0].VInfinitive) - 1, 1) = '_') and (UTF8Copy(aVerbs[IX + 1].VInfinitive, UTF8Length(aVerbs[IX + 1].VInfinitive) - 1, 1) = '_') then begin
        Verbs[1] := aVerbs[IX + 1]; L0 := 1;
      end
      else
        L0 := 0;
      // Display infinitive of verb as form title
      fConj.stTitle.Caption := 'Konjugation des Verbs: ' + VerbName0 + '.';
      // Clear the string grids
      for I := 1 to 2 do begin
        for J := 0 to 3 do begin
          fConj.sgConjPT.Cells[I, J] := '';
          fConj.sgConjSP.Cells[I, J] := '';
          fConj.sgConjPF.Cells[I, J] := '';
        end;
      end;
      // Conjugate the verb: Present tense, simple past and perfect
      ConjError := False;
      for L := 0 to L0 do begin                                                          // conjugate both verb forms
        Verb := Verbs[L];
        if L = 0 then
          Sep := ''
        else
          Sep := ' / ';                                                                  // separator for conjugated verb form display
        for K := 0 to 2 do begin
          for I := 0 to 1 do begin
            Number := Numbers[I];
            for J := 1 to 3 do begin
              if not ConjError then begin
                Person := J;
                if (Verb.VPersons <> 'U') or ((Verb.VPersons = 'U') and (Number = 'Sg') and (Person = 3)) then begin
                  // Personal verb or impersonal verb with 3rd pers. sg: Do conjugation
                  if (Verb.VPersons = 'U') and (Number = 'Sg') and (Person = 3) then
                    Genus := 'n'
                  else
                    Genus := 'm';
                  Conjugation(aVerbs, Verb, rHaben, rSein, Tenses[K], Number, Person, Genus, Conj1, Conj2, Mess);
                  if Mess = '' then begin
                    if K = 0 then begin
                      // Present tense
                      fConj.sgConjPT.Cells[I + 1, J] := fConj.sgConjPT.Cells[I + 1, J] + Sep + Conj1;
                    end
                    else if K = 1 then begin
                      // Simple past
                      fConj.sgConjSP.Cells[I + 1, J] := fConj.sgConjSP.Cells[I + 1, J] + Sep + Conj1;
                      if Conj2 <> Conj1 then
                        fConj.sgConjSP.Cells[I + 1, J] := fConj.sgConjSP.Cells[I + 1, J] + ' / ' + Conj2;
                    end
                    else begin
                      // Perfect
                      fConj.sgConjPF.Cells[I + 1, J] := fConj.sgConjPF.Cells[I + 1, J] + Sep + Conj1;
                      if Conj2 <> Conj1 then
                        fConj.sgConjPF.Cells[I + 1, J] := fConj.sgConjPF.Cells[I + 1, J] + ' / ' + Conj2;
                    end;
                  end
                  else
                    ConjError := True;
                end
                else begin
                  // Impersonal verb with person <> 3rd pers. sg: No conjugated form
                  fConj.sgConjPT.Cells[I + 1, J] := '------';
                  fConj.sgConjSP.Cells[I + 1, J] := '------';
                  fConj.sgConjPF.Cells[I + 1, J] := '------';
                end;
              end;
            end;
          end;
        end;
      end;
      // Show the form with the conjugation string-grid. Using "Show" (and not "ShowModal") allows to keep this form open,
      // when selecting to conjugate another verb and entering it on the main form
      if ConjError then
        MessageDlg('Deutsche Verben"', 'Konjugationsfehler für das Verb: ' + VerbName, mtError, [mbOK], 0)
      else
        fConj.Show;
    end;
  end;
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfGVerbs.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Anzahl der Fragen": Get number of exercise questions from user }

procedure TfGVerbs.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

{ The value entered is stored in a temporary variable and copied to the actual number of questions when "Neue Aufgabe" is selected }

begin
  S := InputBox('Deutsche Verben', 'Anzahl der Fragen', IntToStr(iSQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                              // minimum of questions arbitrarily fixed to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);
  end;
end;

{ Menu items "Einstellungen > Sprachniveau > A1 .. C2": Language levels selection }

procedure TfGVerbs.mSettingsLevelA1Click(Sender: TObject);

begin
  if mSettingsLevelA1.Checked then begin
    mSettingsLevelA1.Checked := False;
    sLevels := StringReplace(sLevels, 'A1', '', []);
  end
  else begin
    mSettingsLevelA1.Checked := True;
    sLevels += 'A1';
  end;
end;

procedure TfGVerbs.mSettingsLevelA2Click(Sender: TObject);

begin
  if mSettingsLevelA2.Checked then begin
    mSettingsLevelA2.Checked := False;
    sLevels := StringReplace(sLevels, 'A2', '', []);
  end
  else begin
    mSettingsLevelA2.Checked := True;
    sLevels += 'A2';
  end;
end;

procedure TfGVerbs.mSettingsLevelB1Click(Sender: TObject);

begin
  if mSettingsLevelB1.Checked then begin
    mSettingsLevelB1.Checked := False;
    sLevels := StringReplace(sLevels, 'B1', '', []);
  end
  else begin
    mSettingsLevelB1.Checked := True;
    sLevels += 'B1';
  end;
end;

procedure TfGVerbs.mSettingsLevelB2Click(Sender: TObject);

begin
  if mSettingsLevelB2.Checked then begin
    mSettingsLevelB2.Checked := False;
    sLevels := StringReplace(sLevels, 'B2', '', []);
  end
  else begin
    mSettingsLevelB2.Checked := True;
    sLevels += 'B2';
  end;
end;

procedure TfGVerbs.mSettingsLevelC1Click(Sender: TObject);

begin
  if mSettingsLevelC1.Checked then begin
    mSettingsLevelC1.Checked := False;
    sLevels := StringReplace(sLevels, 'C1', '', []);
  end
  else begin
    mSettingsLevelC1.Checked := True;
    sLevels += 'C1';
  end;
end;

procedure TfGVerbs.mSettingsLevelC2Click(Sender: TObject);

begin
  if mSettingsLevelC2.Checked then begin
    mSettingsLevelC2.Checked := False;
    sLevels := StringReplace(sLevels, 'C2', '', []);
  end
  else begin
    mSettingsLevelC2.Checked := True;
    sLevels += 'C2';
  end;
end;

{ Menu items "Einstellungen > Sprachniveau > Alle auswählen": All language levels selection }

procedure TfGVerbs.mSettingsLevelAllClick(Sender: TObject);

begin
  mSettingsLevelA1.Checked := True; mSettingsLevelA2.Checked := True;
  mSettingsLevelB1.Checked := True; mSettingsLevelB2.Checked := True;
  mSettingsLevelC1.Checked := True; mSettingsLevelC2.Checked := True;
  sLevels := 'A1A2B1B2C1C2';
end;

{ Menu items "Einstellungen > Sprachniveau > Alle löschen": All language levels de-selection }

procedure TfGVerbs.mSettingsLevelNoneClick(Sender: TObject);

begin
  mSettingsLevelA1.Checked := False; mSettingsLevelA2.Checked := False;
  mSettingsLevelB1.Checked := False; mSettingsLevelB2.Checked := False;
  mSettingsLevelC1.Checked := False; mSettingsLevelC2.Checked := False;
  sLevels := '';
end;

{ Menu items "Einstellungen > Grammatikalische Zeiten > ...": Tenses selection }

procedure TfGVerbs.mSettingsTensesPTClick(Sender: TObject);

begin
  if mSettingsTensesPT.Checked then begin
    mSettingsTensesPT.Checked := False;
    sTenses := StringReplace(sTenses, Tenses[0], '', []);
  end
  else begin
    mSettingsTensesPT.Checked := True;
    sTenses += Tenses[0];
  end;
end;

procedure TfGVerbs.mSettingsTensesSPClick(Sender: TObject);

begin
  if mSettingsTensesSP.Checked then begin
    mSettingsTensesSP.Checked := False;
    sTenses := StringReplace(sTenses, Tenses[1], '', []);
  end
  else begin
    mSettingsTensesSP.Checked := True;
    sTenses += Tenses[1];
  end;
end;

procedure TfGVerbs.mSettingsTensesPFClick(Sender: TObject);

begin
  if mSettingsTensesPF.Checked then begin
    mSettingsTensesPF.Checked := False;
    sTenses := StringReplace(sTenses, Tenses[2], '', []);
  end
  else begin
    mSettingsTensesPF.Checked := True;
    sTenses += Tenses[2];
  end;
end;

{ Menu item "Einstellungen > Zusammengesetzte Verben": Usage of compound verbs selection }

procedure TfGVerbs.mSettingsCompoundClick(Sender: TObject);

begin
  if mSettingsCompound.Checked then
    mSettingsCompound.Checked := False
  else
    mSettingsCompound.Checked := True;
  bCompound := mSettingsCompound.Checked;
end;

{ Menu item "Einstellungen > Maximale Note: 100": Maximum mark selection (60 or 100) }

procedure TfGVerbs.mSettingsP100Click(Sender: TObject);

begin
  if mSettingsP100.Checked then begin
    mSettingsP100.Checked := False;
    iMaxMark := 60;
  end
  else begin
    mSettingsP100.Checked := True;
    iMaxMark := 100;
  end;
end;

{ Menu item "Hilfe > Hilfe": Display program help }

procedure TfGVerbs.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Hilfe > Über": Display program about }

procedure TfGVerbs.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := '"GerVerbs" ist eine Desktopanwendung, die Aufgaben über die Konjugation der unregelmäßigen und starken deutschen ';
  S += 'Verben im Präsens, Präteritum und Perfekt erstellt.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Juli-November 2019.';
  MessageDlg('Über "GerVerbs"', S, mtInformation, [mbOK], 0);
end;

{ Button "Fage/Antwort": Generate new question resp. check user answer }

procedure TfGVerbs.btQuestionClick(Sender: TObject);

var
  Person, IX, P: Integer;
  Infinitive, Number, Genus, Tense, Mess: string;
  OK: Boolean;

begin
  // Button "Frage": Generate new conjugation question
  if btQuestion.Caption = 'Frage' then begin
    if iQuestion < iQuestions then begin
      OK := True;
      if not mSettingsTensesPT.Checked and not mSettingsTensesSP.Checked and not mSettingsTensesPF.Checked then
        // There must be at least 1 tense selected
        OK := False
      else if not mSettingsLevelA1.Checked and not mSettingsLevelA2.Checked and not mSettingsLevelB1.Checked
          and not mSettingsLevelB2.Checked and not mSettingsLevelC1.Checked and not mSettingsLevelC2.Checked then
        // There must be at least 1 level selected
        OK := False;
      if OK then begin
        Inc(iQuestion);
        laQuestion.Caption := 'Frage ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
        // Choose a random verb (among those selected)
        repeat
          OK := True;
          IX := Random(Length(aVerbs));
          P := Pos(aVerbs[IX].VLevel, sLevels);
          if P = 0 then
            OK := False
          else if not bCompound and (aVerbs[IX].VSeparation <> '-') then
            OK := False;
        until OK;
        Infinitive := aVerbs[IX].VInfinitive;
        if (UTF8Copy(Infinitive, UTF8Length(Infinitive) - 1, 1) = '_') then              // remove tags
          Delete(Infinitive, Length(Infinitive) - 1, 2);
        edVerb.Text := aVerbs[IX].VPrefix1 + aVerbs[IX].VPrefix2 + Infinitive;
        if aVerbs[IX].VPronominal then
          edVerb.Text := 'sich ' + edVerb.Text;                                          // pronominal verb
        if aVerbs[IX].VNote <> '' then
          edVerb.Text := edVerb.Text + ' (' + aVerbs[IX].VNote + ')';                    // verb with different conjugation depending on actual meaning
        // Choose a random tense (among those selected)
        repeat
          OK := True;
          Tense := Tenses[Random(3)];
          P := Pos(Tense, sTenses);
          if P = 0 then
            OK := False;
        until OK;
        // Choose number and person (random as adequate)
        if aVerbs[IX].VPersons = 'U' then begin;
          // Impersonal verbs
          Number := Numbers[0]; Person := 3; Genus := Genuus[2];
        end
        else begin
          if aVerbs[IX].VPersons = 'u' then begin;
            // Verbs that should be conjugated with 3rd person
            Number := Numbers[Random(2)]; Person := 3;
          end
          else if aVerbs[IX].VPersons = 'P' then begin;
            // Verbs that should be conjugated with plural only
            Number := Numbers[1]; Person := Random(3) + 1;
          end
          else begin
            // Normal case
            Number := Numbers[Random(2)]; Person := Random(3) + 1;
          end;
          Genus := '-';
          // For 3rd person sg, choose random genus
          if (Number = 'Sg') and (Person = 3) then
            Genus := Genuus[Random(3)];
        end;
        // Display conjugation question
        edConj.Text := Tense;
        edConj.Text := edConj.Text + ' (' + IntToStr(Person) + '. Pers. ' + Number;
        if Genus <> '-' then
          edConj.Text := edConj.Text + ', ' + Genus;
        edConj.Text := edConj.Text + ')';
        // Conjugate the verb with actual parameters
        Conjugation(aVerbs, aVerbs[IX], rHaben, rSein, Tense, Number, Person, Genus, sAnswer1, sAnswer2, Mess);
        if Mess <> '' then begin
          // Some error while conjugating the verb (should normally not happen)
          MessageDlg('Programmfehler', Mess + '!', mtError, [mbOK], 0);
          Dec(iQuestion);                                                                // just ignore this question
        end
        else begin
          // Clear answer fields and change button caption (next push will be a user answer)
          edAnswer.Text := ''; edAnswer.SetFocus;
          edEval.Text := '';
          btQuestion.Caption := 'Antwort';
        end;
      end
      else begin
        if not mSettingsTensesPT.Checked and not mSettingsTensesSP.Checked and not mSettingsTensesPF.Checked then
          MessageDlg('Fehler', 'Es ist keine grammatikalische Zeit ausgewählt!', mtError, [mbOK], 0)
        else
          MessageDlg('Fehler', 'Es ist kein Sprachniveau ausgewählt!', mtError, [mbOK], 0)
      end;
    end;
  end
  // Button "Antwort": Check user answer and update evaluation counters
  else begin
    if iQuestion <= iQuestions then begin
      // Correct answer
      if (edAnswer.Text = sAnswer1) or (edAnswer.Text = sAnswer2) then begin             // if there are 2 different verb forms, user may enter either of them
        Inc(iCorrect);
        edEval.Text := 'Richtig!';
        edEval.Font.Color := clDefault;
      end
      // False answer
      else begin
        Inc(iFalse);
        edEval.Text := 'Falsch! Richtig = ' + sAnswer1;
        if sAnswer2 <> sAnswer1 then
          edEval.Text := edEval.Text + ' / ' + sAnswer2;
        edEval.Font.Color := clRed;
      end;
      // Update evaluation counters
      edQuestions.Text := IntToStr(iQuestion);
      edCorrect.Text := IntToStr(iCorrect);
      edFalse.Text := IntToStr(iFalse);
      // Calculate success (marks with maximum of 60 or 100 as selected)
      P := Round(iMaxMark * (iCorrect / iQuestion));
      edSuccess.Text := IntToStr(P);
      // Use different colors for different success ranges
      if 100 * (P / iMaxMark) >= 60 then
        edSuccess.Color := clLime
      else if 100 * (P / iMaxMark) >= 50 then
        edSuccess.Color := clYellow
      else
        edSuccess.Color := clRed;
    end;
    // Change button caption (next push will be another question to generate)
    btQuestion.Caption := 'Frage';
    // If all questions are done, display "end of exercise" message
    if iQuestion = iQuestions then begin
      MessageDlg('Deutsche Verben', 'Ende des Verbentests...', mtInformation, [mbOK], 0);
      Inc(iQuestion);                                                                    // this avoids that the message is displayed more than once
    end;
  end;
end;

{ Not-English letter buttons: Include the corr. letter into user answer }

procedure TfGVerbs.btAeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ä';
end;

procedure TfGVerbs.btOeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ö';
end;

procedure TfGVerbs.btUeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ü';
end;

procedure TfGVerbs.btSsClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ß';
end;

{ Button "Löschen": Clear answer field }

procedure TfGVerbs.btClearClick(Sender: TObject);

begin
  edAnswer.Text := '';
end;

end.

