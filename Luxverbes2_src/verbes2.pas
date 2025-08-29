{****************************************}
{* Main unit for Luxverbes2 application *}
{****************************************}

unit verbes2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, PopupNotifier, LazUTF8, conj2, help;

type
  TVerb = record
    VName: string;
    VType, VCasus, VPrefix, VAuxiliary, VConjugation: string[1];
    VGerman, VFrench: string;
    Participle: string;
  end;
  TVerbs = array of TVerb;
  {************}
  {* TfVerbes *}
  {************}

  { TfVerbes }

  TfVerbes = class(TForm)
    MenuItem1: TMenuItem;
    mSettingsNoEN: TMenuItem;
    mSettingsNoFrench: TMenuItem;
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseConj, mExerciseExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsP100, mSettingsInfHide: TMenuItem;
    mSettingsTenses, mSettingsTensesPP, mSettingsTensesPC, mSettingsTensesPQP: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, laConj, Label4, Label5, Label6, Label7: TLabel;
    laQuestion: TLabel;
    edVerb, edVerbGe, edVerbFr: TEdit;
    edConj, edAnswer, edEval: TEdit;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAccAigu, btAccGrave, btTrema, btAe, btOe, btUe: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseConjClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mSettingsNoENClick(Sender: TObject);
    procedure mSettingsNoFrenchClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsP100Click(Sender: TObject);
    procedure mSettingsInfHideClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAccAiguClick(Sender: TObject);
    procedure btAccGraveClick(Sender: TObject);
    procedure btTremaClick(Sender: TObject);
    procedure btAeClick(Sender: TObject);
    procedure btOeClick(Sender: TObject);
    procedure btUeClick(Sender: TObject);
    procedure mSettingsTensesPCClick(Sender: TObject);
    procedure mSettingsTensesPPClick(Sender: TObject);
    procedure mSettingsTensesPQPClick(Sender: TObject);
  private
    iSQuestions, iQuestions, iQuestion, iCorrect, iFalse: Integer;
    sAnswer: string;
    aVerbs: TVerbs;
  end;

const
  Tenses: array[0..2] of string = ('Partizip', 'Perfekt', 'Plusquamperfekt');
  Numbers: array[0 .. 1] of string = ('singulier', 'pluriel');
  Genuus: array[0..2] of string = ('m', 'f', 'n');
  Consonants: array[1 .. 21] of string = (
    'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'y', 'z'
  );
  EiffelChars: array[1 .. 16] of string = (
    'b', 'c', 'f', 'g', 'j', 'k', 'l', 'm', 'p', 'q', 'r', 's', 'v', 'w', 'x', 'y'
  );

var
  fVerbes: TfVerbes;

implementation

{$R *.lfm}

{ Function to correctly right-trim a string containing UTF8 characters }

function RTrim(S: string): string;

var
  L: Integer;

begin
  L := UTF8Length(S);
  while UTF8Copy(S, L, 1) = ' ' do
    Dec(L);
  S := UTF8Copy(S, 1, L);
  RTrim := S;
end;

{ Function to uppercase the first letter of a word }

function UpperFirst(S: string): string;

var
  First: string;

begin
  First := UTF8Copy(S, 1, 1);
  if First = 'ä' then
    First := 'Ä'
  else if First = 'ö' then
    First := 'Ö'
  else if First = 'ü' then
    First := 'Ü'
  else if First = 'é' then
    First := 'É'
  else if First = 'ë' then
    First := 'Ë'
  else
    First := UpperCase(First);
  UpperFirst := First + UTF8Copy(S, 2, UTF8Length(S));
end;

{ Function to check if a letter is a consonant }

function IsConsonant(Letter: string): Boolean;

var
  I: Integer;
  IsCons: Boolean;

begin
  IsCons := False;
  for I := 1 to 21 do
    if Letter = Consonants[I] then
      IsCons := True;
  IsConsonant := IsCons;
end;

{ Function to check if a verb may be considered to be directly dirived from French }

function IsFrenchVerb(Verb: string): Boolean;

// Note, that for compound verbs (verbs with prefix), the verb passed to the routine
// must be the verbbase in order to be detected as Luxembourgish verb!

const
  NNonFrench = 7;
  // Luxembourgish verbs (not to be considered as directly dirived from French) in -éieren
  NonFrench: array[1 .. NNonFrench] of string = (
    'éieren', 'héieren', 'féieren', 'fréieren', 'léieren', 'kéieren', 'réieren'
  );
var
  I: Integer;
  IsFrench: Boolean;

begin
  IsFrench := False;
  if RightStr(Verb, 7) = 'éieren' then begin
    IsFrench := True;
    // Check if verb isn't part of the list of Luxembourgish verbs in -éieren
    for I := 1 to NNonFrench do begin
      if Verb = NonFrench[I] then
        IsFrench := False;
    end;
  end;
  IsFrenchVerb := IsFrench;
end;

{ Function to check if a letter is one of those where the Eiffel rule says to drop the "n" preceding it }

function IsEiffelChar(Letter: string): Boolean;

var
  I: Integer;
  IsEiffel: Boolean;

begin
  IsEiffel := False;
  for I := 1 to 16 do
    if Letter = EiffelChars[I] then
      IsEiffel := True;
  IsEiffelChar := IsEiffel;
end;

{ Luxembourgish orthography: Eiffel rule }

function AdjustEiffel(Word1, Word2: string): string;

{ If a word ends in -n and the following word starts with a consonant other than d,t,z,h,n than the "n" is dropped }

begin
  if RightStr(Word1, 1) = 'n' then begin
    if IsEiffelChar(UTF8Copy(Word2, 1, 1)) then begin
      Word1 := UTF8Copy(Word1, 1, UTF8Length(Word1) - 1);
      if RightStr(Word1, 1) = 'n' then                                         // if there was a second "n", drop this one, too
        Word1 := UTF8Copy(Word1, 1, UTF8Length(Word1) - 1);
    end;
  end;
  AdjustEiffel := Word1;
end;

{ Luxembourgish orthography: Verb stem modifications according to the luxembourgish spelling rules }

function StemModifications(Stem, Suffix: string): string;

const
  // The verbstems mentionned here are words with a short vowel, even though followed by a single consonant
  NExceptions = 2;
  Exceptions: array[1..NExceptions] of string = (
    'mix', 'box'
  );

var
  I: Integer;
  StemLast1, StemLast2, SuffixFirst: string;
  Exception: Boolean;

begin
  StemLast1 := UTF8Copy(Stem, UTF8Length(Stem), 1);
  StemLast2 := UTF8Copy(Stem, UTF8Length(Stem) - 1, 1);
  SuffixFirst := UTF8Copy(Suffix, 1, 1);
  // Rule for stem ending in -w: "w" becomes "f" if suffix starts with "s" or with "t"
  if StemLast1 = 'w' then begin
    if (SuffixFirst = 's') or (SuffixFirst = 't') then
      Stem := UTF8Copy(Stem, 1, UTF8Length(Stem) - 1) + 'f';
  end
  // Rule for stem ending in -d: "d" becomes "t" if suffix starts with "s" or with "t"
  else if StemLast1 = 'd' then begin
    if (SuffixFirst = 's') or (SuffixFirst = 't') then begin
      if StemLast2 = 'd' then
        // If ending was -dd, transform both 'd' to 't'
        Stem := UTF8Copy(Stem, 1, UTF8Length(Stem) - 2) + 'tt'
      else
        Stem := UTF8Copy(Stem, 1, UTF8Length(Stem) - 1) + 't';
    end;
  end;
  // Rule for stem ending in a nasal, preceded by a "hindering" consonant: insertion of an "e" if suffix starts with "s" or with "t"
  if ((StemLast1 = 'm') or (StemLast1 = 'n')) and IsConsonant(StemLast2) then begin
    if (StemLast2 <> StemLast1) and ((SuffixFirst = 's') or (SuffixFirst = 't')) then begin
      Stem := UTF8Copy(Stem, 1, UTF8Length(Stem) - 1) + 'e' + StemLast1;
      // Special case for "ootmen": vowel reduction necessary
      if RightStr(Stem, 5) = 'ootem' then
        Stem := StringReplace(Stem, 'ootem', 'otem', []);
    end;
  end;
  // Vowel doubling rule (for stem ending in long i,u,o,a,ä,ö, ü followed by a consonant)
  Exception := False;
  for I := 1 to NExceptions do
    if Stem = Exceptions[I] then
      Exception := True;
  if not Exception then begin
    if IsConsonant(StemLast1) and ((StemLast2 = 'i') or (StemLast2 = 'u') or (StemLast2 = 'o') or (StemLast2 = 'a')
                                                     or (StemLast2 = 'ä') or (StemLast2 = 'ö') or (StemLast2 = 'ü')) then begin
      // Rule only applies to i,u,o,a,ä,ö,ü not to potential diphdongues (check letter preceding the vowel)
      if (Length(Stem) = 2) or IsConsonant(UTF8Copy(Stem, UTF8Length(Stem) - 2, 1)) then begin
        if (SuffixFirst = 's') or (SuffixFirst = 't') then begin
          if StemLast1 <> 'r' then
            // Normal case: Doubling the vowel
            Stem := UTF8Copy(Stem, 1, UTF8Length(Stem) - 2) + StemLast2 + StemLast2 + StemLast1
          else begin
            // If the consonant following the long vowel is "r", an "e" is inserted (instead of doubling the vowel)
            Stem := UTF8Copy(Stem, 1, UTF8Length(Stem) - 2) + StemLast2 + 'e' + StemLast1;
          end;
        end;
      end;
    end;
  end;
  StemModifications := Stem;
end;

{ Get "real" verb name }

function AdjustVerb(Verb, VerbType: string): string;

var
  P: Integer;
  Prefix: string;

{ Remove prefix-hyphen (as is in verbs file) and add 'sech' for pronominal verbs }

begin
  P := Pos('-', Verb);
  if P > 0 then begin
    Prefix := Copy(Verb, 1, P - 1);                                            // usage of "Copy" (not "UTF8Copy") because of "Pos" before
    Verb   := Copy(Verb, P + 1, Length(Verb));
    Prefix := AdjustEiffel(Prefix, Verb);
    Verb := Prefix + Verb;
  end;
  if VerbType = 'P' then
    Verb := 'sech ' + Verb;
  AdjustVerb := Verb;
end;

{ Read verbs from text file and fill them into an array of TVerb records }

procedure ReadVerbs(out Verbs: TVerbs);

const
  VerbFile = 'verbes.txt';

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, VerbFile); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin                   // lines starting with '#' are comments
      Inc(N); SetLength(Verbs, N);
      with Verbs[N - 1] do begin
        VName := RTrim(UTF8Copy(Line, 1, 20));
        VType := UTF8Copy(Line, 21, 1);
        VCasus := UTF8Copy(Line, 22, 1);
        VAuxiliary := UTF8Copy(Line, 25, 1);
        VPrefix := UTF8Copy(Line, 26, 1);
        VConjugation := UTF8Copy(Line, 28, 1);
        VGerman := RTrim(UTF8Copy(Line, 31, 20));
        VFrench := RTrim(UTF8Copy(Line, 51, 25));
        Participle := '-';
        if (VConjugation = 'M') or (VConjugation = 'I') then
          Participle := RTrim(UTF8Copy(Line, 76, UTF8Length(Line)));
      end;
    end;
  end;
  Close(InFile);
end;

{ Prepare a new exercise (clear form and reset counters) }

procedure NewExercise(SQuestions: Integer; var Questions, Question, QCorrect, QFalse: Integer);

begin
  Questions := SQuestions;                                                     // selected number of questions now becomes active
  Question := 0; QCorrect := 0; QFalse := 0;
  fVerbes.laQuestion.Caption := 'Fro';
  fVerbes.edVerb.Text := ''; fVerbes.edVerbGe.Text := ''; fVerbes.edVerbFr.Text := '';
  fVerbes.edConj.Text := ''; fVerbes.edAnswer.Text := ''; fVerbes.edEval.Text := '';
  fVerbes.edQuestions.Text := ''; fVerbes.edCorrect.Text := ''; fVerbes.edFalse.Text := '';
  fVerbes.edSuccess.Text := ''; fVerbes.edSuccess.Color := clCream;
  fVerbes.btQuestion.Caption := 'Fro';
end;

{ Luxembourgish verbs conjugation: Participle and compound past tenses }

function Conjugation(Verb: TVerb; Tense: string; Number: string; Person: Integer; Genus: string): string;

const
  // Personal pronouns ("forme tonique")
  PronounsPers: array[1 .. 2, 1 .. 3] of string = (
    ('ech', 'du', 'hien'),
    ('mir', 'dir', 'si')
  );
  // Reflexive pronouns
  PronounsReflex: array[1 .. 2, 1 .. 3] of string = (
    ('mech', 'dech', 'sech'),
    ('äis', 'iech', 'sech')
  );
  // Auxiliary "hunn" (to have)
  AuxHunn: array[1..2, 1 .. 2, 1 .. 3] of string = (
    ( ('hunn', 'hues', 'huet'),
      ('hunn', 'huet', 'hunn')
    ),
    (
      ('hat', 'has', 'hat'),
      ('haten', 'hat', 'haten')
    )
  );
  // Auxiliary "sinn" (to be)
  AuxSinn: array[1..2, 1 .. 2, 1 .. 3] of string = (
    ( ('sinn', 'bass', 'ass'),
      ('sinn', 'sidd', 'sinn')
    ),
    (
      ('war', 'waars', 'war'),
      ('waren', 'waart', 'waren')
    )
  );

var
  TenseX, NumberX, P: Integer;
  VerbName0, VerbName, Stem, VerbPrefix, Participle, ParticiplePrefix, ParticipleSuffix, Pronoun, Reflexive, ConjVerb, ConjAuxiliary: string;

begin
  VerbName0 := Verb.VName; VerbName := VerbName0; VerbPrefix := '';
  // Verb with prefix (indicated with a hyphen as read from verbs file )
  if (Verb.VPrefix = 'I') or (Verb.VPrefix = 'S') then begin
    P := Pos('-', VerbName);
    VerbPrefix := Copy(VerbName, 1, P - 1);                                    // must use "Copy" after usage of "Pos"
    VerbName0 := Copy(VerbName, P + 1, Length(VerbName));
    if Verb.VPrefix = 'I' then begin
      // Unremovable prefix
      VerbPrefix := AdjustEiffel(VerbPrefix, VerbName0);
      VerbName := VerbPrefix + VerbName0;                                      // verb name = prefix + verb base
    end
    else if Verb.VPrefix = 'S' then
      // Removable prefix
      VerbName := VerbName0;                                                   // verb name = verb base (prefix is separated for conjugation)
  end;
  if Number = Numbers[0] then
    NumberX := 1
  else
    NumberX := 2;
  Pronoun := ''; Reflexive := '';
  // Get the pronouns for actual number and person
  if Tense <> Tenses[0] then begin                                             // not for participle
    Pronoun := PronounsPers[NumberX, Person];
    Reflexive := PronounsReflex[NumberX, Person];
    // Adjust reflexive pronoun for verbs with dative
    if Verb.VCasus = 'D' then begin
      Reflexive := StringReplace(Reflexive, 'mech', 'mir', []);
      Reflexive := StringReplace(Reflexive, 'dech', 'dir', []);
    end;
    // Adaption: For 3rd person sg, personal pronoun varies with genus
    if Pronoun = 'hien' then begin
      if UpperCase(Verb.VType) = 'I' then
        // Use "ët" with impersoanl verbs
        Pronoun := 'ët'
      else begin
        // For other verbs, use genus dependent pronoun
        if Genus = 'f' then
          Pronoun := 'si'
        else if Genus = 'n' then
          Pronoun := 'hatt';
      end;
    end;
  end;
  // --------------------------------------------
  // Luxembourgish verbs conjugation ("Partizip")
  // --------------------------------------------
  Stem := UTF8Copy(VerbName, 1, UTF8Length(VerbName) - 2);                // this stem = "racine" (final "radical" may be different)
  if (Verb.VConjugation = 'M') or (Verb.VConjugation = 'I') then
    // Verb with "métaphonèse": Participle is indicated in the verbs file
    Participle := Verb.Participle
  else begin
    // Regular conjugation (i.e. non-irregualr verb and non-"métaphonèse" condition)
    if Verb.VConjugation = 'T' then begin
      ParticiplePrefix := 'ge'; ParticipleSuffix := 't';
    end
    else if Verb.VConjugation = 't' then begin
      ParticiplePrefix := ''; ParticipleSuffix := 't';
    end
    else if Verb.VConjugation = 'N' then begin
      ParticiplePrefix := 'ge'; ParticipleSuffix := 'en';
      if fVerbes.mSettingsNoEN.Checked then
        ParticipleSuffix := '';
    end
    else if Verb.VConjugation = 'n' then begin
      ParticiplePrefix := ''; ParticipleSuffix := 'en';
      if fVerbes.mSettingsNoEN.Checked then
        ParticipleSuffix := '';
    end;
    // Apply lux. spelling rules to the stem for actual suffix
    if ParticipleSuffix = 't' then
      Stem := StemModifications(Stem, ParticipleSuffix);
    // Adaption: Special conjugation rules
    if RightStr(Stem, 1) = 't' then begin
      // Rule: If stem ends in -t, suffix "t" is dropped
      if ParticipleSuffix = 't' then
        ParticipleSuffix := '';
    end;
    if (UTF8Copy(Stem, UTF8Length(Stem) - 1, 2) = 'ee') then begin
      // Rule: If stem ends in -ee, suffix "en" becomes a trema
      if ParticipleSuffix = 'en' then
        ParticipleSuffix := 'ën';
    end;
    // Participle verb form: participle prefix + stem (radical) + participle suffix
    Participle := ParticiplePrefix + Stem + ParticipleSuffix;
  end;
  // For a verb with removable prefix, add this prefix at the beginning of the participle
  if Verb.VPrefix = 'S' then begin
    VerbPrefix := AdjustEiffel(VerbPrefix, Participle);                        // Eiffel rule for removable prefix (followed by participle)
    Participle := VerbPrefix + Participle;
  end;
  ConjVerb := Participle;
  // -----------------------------------------------------------------
  // Luxembourgish verbs conjugation ("Perfekt" and "Plusquamperfekt")
  // -----------------------------------------------------------------
  if Tense <> Tenses[0] then begin
    // Transform string tenses and numbers to integer indexes
    if Tense = Tenses[1] then
      TenseX := 1
    else
      TenseX := 2;
    // Get conjugated form of auxiliary
    if Verb.VAuxiliary = 'H' then
      ConjAuxiliary := AuxHunn[TenseX, NumberX, Person]                        // verb conjugated with "hunn"
    else
      ConjAuxiliary := AuxSinn[TenseX, NumberX, Person];                       // verb conjugated with "sinn"
    // Add the personal pronoun
    Pronoun := AdjustEiffel(Pronoun, ConjAuxiliary);                           // Eiffel rule for personal pronoun (followed by auxiliary)
    ConjAuxiliary := Pronoun + ' ' + ConjAuxiliary;
    // For a pronominal verb, add the reflexive pronoun
    if Verb.VType = 'P' then begin
      ConjAuxiliary := AdjustEiffel(ConjAuxiliary, Reflexive);                 // Eiffel rule for conjugated auxiliary (followed by reflexive pronoun)
      ConjAuxiliary += ' ' + Reflexive;
    end;
    // Conjugated verb = conjugated auxiliary + participle
    ConjAuxiliary := AdjustEiffel(ConjAuxiliary, Participle);                  // Eiffel rule for conjugated auxiliary (followed by participle)
    ConjVerb := ConjAuxiliary + ' ' + Participle;
  end;
  Conjugation := ConjVerb;
end;

{ Guess prefix of a given verb by comparing its starting letters to a list of prefixes }

procedure GuessPrefix(VerbName: string; out VerbBase, VerbPrefix, PrefixType: string);

const
  NInSeps = 16; NSeps = 55;
  // Unremovable prefixes
  InSeps: array[1 .. NInSeps] of string = (
    'be', 'ent', 'ënner', 'er', 'ge', 'iwwer', 'mëss', 'ver', 'zer', 'de', 'des', 'dis', 're', 'konter', 'trans', 'poly'
  );
  // A selection of removable prefixes
  Seps: array[1 .. NSeps] of string = (
    'an', 'aus', 'bäi', 'bevir', 'blo', 'blouss', 'breet', 'brooch', 'derbäi', 'derduerch', 'derfir', 'dergéint', 'dernieft', 'dertëschent',
    'dervun', 'dicht', 'do', 'doheem', 'dohin', 'dout', 'dran', 'drënner', 'drop', 'drun', 'duer', 'duerch', 'ëm', 'ënner', 'entgéint', 'eran',
    'eraus', 'erbäi', 'erduerch', 'erëm', 'eriwwer', 'erof', 'erop', 'erun', 'ervir', 'ewech', 'fest', 'fort', 'hannescht', 'hier', 'hin',
    'iwwer', 'kennen', 'mat', 'no', 'of', 'op', 'un', 'vir', 'zeréck', 'zou'
  );

var
  I, J: Integer;
  Prefix: string;
  Found, NRemoved: Boolean;

// Determining the verb prefix is not only necessary to correctly handle removable prefixes, but also allows to correctly conjugate
// unknown irregular verbs and verbs with "métaphonèse" that are compounds (prefix + verbbase) of verbs known by the program

begin
  VerbBase := VerbName; VerbPrefix := ''; PrefixType := '-';
  // Check for removable prefix
  Found := False; I := 1;
  repeat
    Prefix := Seps[I];
    NRemoved := False;
    if (RightStr(Prefix, 1) = 'n') and (UTF8Copy(VerbName, 1, UTF8Length(Prefix) - 1) = UTF8Copy(Prefix, 1, UTF8Length(Prefix) -1)) then begin
      // Prefixes ending in -n may appear without the final "n" in the compound verb (ex: eraklammen = eran + klammen) due to
      // the Eiffel rule. Thus if the Eiffel rule applies, the prefix to compare the beginning of the word with, must be adapted
      for J := 1 to 20 do begin
        if Copy(VerbName, Length(Prefix), 1) = EiffelChars[J] then begin
          Prefix := UTF8Copy(Prefix, 1, UTF8Length(Prefix) - 1);
          NRemoved := True;
        end;
      end;
    end;
    // Check if the beginning of the verb is equal to the (eventually adapted) prefix from the list
    if UTF8Copy(VerbName, 1, UTF8Length(Prefix)) = Prefix then begin
      // Consider the beginning of the word being this prefix only for verbs not ending in -éieren
      // (Such verbs haven't such prefixes and this avoids to make errors like considering the "a" of "agraféieren" as prefix)
      VerbBase := UTF8Copy(VerbName, UTF8Length(Prefix) + 1, UTF8Length(VerbName) - UTF8Length(Prefix));
      if not IsFrenchVerb(VerbBase) then begin
        // Do not consider prefixes for verbs directly dirived from French
        VerbPrefix := Prefix;
        if NRemoved then
          // Add the prefix final -n if it has been removed for correct comparison before
          VerbPrefix += 'n';
        PrefixType := 'S';
        Found := True;
      end;
    end;
    Inc(I);
  until Found or (I > NSeps);
  // Check for unremovable prefix
  if not Found then begin
    I := 1;
    repeat
      Prefix := InSeps[I];
      if UTF8Copy(VerbName, 1, UTF8Length(Prefix)) = Prefix then begin
        VerbBase := UTF8Copy(VerbName, UTF8Length(Prefix) + 1, UTF8Length(VerbName) - UTF8Length(Prefix));
        if not IsFrenchVerb(VerbBase) then begin
          // Do not consider prefixes for verbs directly dirived from French
          VerbPrefix := Prefix;
          PrefixType := 'I';
          Found := True;
        end;
      end;
      Inc(I);
    until Found or (I > NInSeps);
  end;
end;

{************}
{* TfVerbes *}
{************}

{ Application start: Initialisation }

procedure TfVerbes.FormCreate(Sender: TObject);

begin
  SetLength(aVerbs, 0);
  ReadVerbs(aVerbs);
  iSQuestions := 20;
  NewExercise(iSQuestions, iQuestions, iQuestion, iCorrect, iFalse);
  Randomize;
end;

{ Menu item "Exercice > Neien Exercice": Prepare for a new exercise }

procedure TfVerbes.mExerciseNewClick(Sender: TObject);

begin
  NewExercise(iSQuestions, iQuestions, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Exercice > Konjugatioun": Conjugate the verb entered by the user }

procedure TfVerbes.mExerciseConjClick(Sender: TObject);

var
  Person, IX, I, J: Integer;
  VerbName, VerbName2, VerbName3, VerbBase, VerbPrefix, PrefixType, Number, S: string;
  Verb: TVerb;
  Pronominal: Boolean;

{ The routine conjugates the verb and fills in the string-grids on the fConj form }

begin
  Pronominal := False;   fConj.edVerbDetails.Clear;
  // Get the verb to conjugate from user
  VerbName := InputBox('Lëtzeburgesch Verben', 'Konjugatioun vu wellechem Verb', '');
  // Conjugate (or try to conjugate) the verb
  if (VerbName <> '') and (Length(VerbName) >= 4) then begin
    if LeftStr(VerbName, 5) = 'sech ' then begin
      // Consider verb to be conjugated using pronominal voice
      VerbName := StringReplace(VerbName, 'sech ', '', []);                    // remove reflexive pronoun for verb lookup
      Pronominal := True;
    end;
    // Check if the program knows this verb (i.e. if it is one of those in the list read from the "verbes.txt" file)
    IX := -1;
    for I := 0 to Length(aVerbs) - 1 do begin
      VerbName2 := StringReplace(aVerbs[I].VName, '-', '', [rfReplaceAll]);
      VerbName3 := StringReplace(aVerbs[I].VName, 'n-', '', []);
      VerbName3 := StringReplace(VerbName3, '-', '', [rfReplaceAll]);
      if (VerbName = VerbName2) or (VerbName = VerbName3) then
        IX := I;
    end;
    if IX <> -1 then begin
      // Verb has been found in the list: all data for correct conjugation is available in the array created from the verbs file
      Verb := aVerbs[IX];
      if Pronominal then
        // Change veb type if user entered a verb starting with "sech": this allows to conjugate any verb using the pronominal voice
        // However, do this only if file data indicates that the verb may be conjugated using the pronominal voice
        if Verb.VCasus <> '-' then
          Verb.VType := 'P';
      // Text for conjugation details for this verb
      S := '"' + UpperFirst(VerbName) + '"';
      case Verb.VConjugation[1] of
        'T': S += ' huet e Partizip vun der Form: ge- + Radical + -t.';
        't': S += ' huet e Partizip vun der Form: Radical + -t (ouni ge-).';
        'N': S += ' huet e Partizip vun der Form: ge- Radical [+ -en].';
        'n': S += ' huet e Partizip vun der Form: Radical [+ -en] (ouni ge-).';
        'I': S += ' huet en onregelméissege Partizip.';
        'M': S += ' huet e Partizip mat Metaphonèse.';
      end;
      fConj.edVerbDetails.Lines.AddText(S);
    end
    else begin
      // Verb has not been found in the list: Warning message and try to conjugate "as well as possible"
      S := '"' + UpperFirst(VerbName) + '"';
      S += ' ass e Verb, dat de "Luxverbes2" Programm net kennt! Déi affichéiert Konjugatioun ass ouni Garantie';
      S += ' a kann een oder méi Fehler opweisen!' + Chr(13);
      fConj.edVerbDetails.Lines.AddText(S);
      Verb.VName := VerbName; Verb.VFrench := ''; Verb.VGerman := '';
      Verb.VType := '?'; Verb.VCasus := '?'; Verb.VConjugation := '?'; Verb.VPrefix := '?';
      Verb.Participle := ''; Verb.VAuxiliary := '?';
      // Set verb type = pronominal, if user entered a verb starting with "sech "
      if Pronominal then
        Verb.VType := 'P';
      // Try to find a verb prefix
      GuessPrefix(VerbName, VerbBase, VerbPrefix, PrefixType);
      // Prefix type of unknown verb should now be ok
      Verb.VPrefix := PrefixType;
      // If the verb has a prefix, check if the verbbase (verb minus the prefix) is known by the program
      if (PrefixType = 'S') or (PrefixType = 'I') then begin
        Verb.VName := VerbPrefix + '-' + VerbBase;
        IX := -1;
        for I := 0 to Length(aVerbs) - 1 do begin
          if VerbBase = aVerbs[I].VName then
            IX := I;
        end;
      end;
      // Verbbase has been found in the list: Conjugate the verb using the verbbase as model
      if IX <> -1 then begin
        // Conjugation type should be ok now
        Verb.VConjugation := aVerbs[IX].VConjugation;
        // Auxiliary supposed to be the same as for baseverb
        Verb.VAuxiliary := aVerbs[IX].VAuxiliary;
        if (Verb.VConjugation = 'M') or ((Verb.VConjugation = 'I')) then
          // For verbs with "métaphonèse" and irregular verbs, use verbform as specified for the baseverb
          Verb.Participle := aVerbs[IX].Participle;
      end;
      // Details about how the program conjugated the unknown verb
      if IsFrenchVerb(VerbBase) then begin
        // Verbs in -éieren have participle of form: stem + -t
        S := 'D''Endung "éieren" suggeréiert dass d''Verb direkt aus dem Franséischen iwwerholl ass. De Programm geet duefir dervun';
        S += ' aus dass de Partizip d''Form "Radical + -t" huet.';
        Verb.VConjugation := 't';
        // If auxiliary not known, use "hunn"
        if Verb.VAuxiliary = '?' then
          Verb.VAuxiliary := 'H';
      end
      else if Verb.VConjugation = '?' then begin
        // For other verbs, where no prefix has been found or if the verbbase is unknown, the verb is conjugated as if it was regular.
        // This will always result in a conjugation with errors if there is "métaphonèse" for this verb
        S := 'De Programm huet keng kloer Indikatiounen iwwert d''Konjugatioun (notamment wat eng eventuell Metaphonèse';
        S += ' ugeet) fond an ët wéi e regelméissegt Verb, ënner Berücksichtigung vun den Orthographiesregele, konjugéiert:';
        if PrefixType = 'I' then begin
          S += ' Partizip = Radical + -t (ouni ge-, well "' + VerbPrefix + '" ee net separable Prefix zë si schéngt).';
          Verb.VConjugation := 't';
        end
        else begin
          S += ' Partizip = ge- + Radical + -t.';
          Verb.VConjugation := 'T';
        end;
        // Use "hunn" as auxiliary
        Verb.VAuxiliary := 'H';
      end
      else begin
        // The verb is supposed to be a compound of a prefix and a known baseverb and will be conjugated similarly to this one
        // However, verbs with a non-removable prefix, always have a participle without ge-
        // (This should be correct results for nearly all irregular verbs and a good part of unknown verbs with "métaphonèse")
        S := 'De Programm geet dervun aus dass "' + VerbPrefix + '" ee';
        if PrefixType = 'I' then begin
          S += ' net separable Prefix ass an d''Verb ähnlech wéi "' + VerbBase + '"';
          if (Verb.VConjugation = 'T') or (Verb.VConjugation = 'N') then begin
            S += ' (awer ouni ge-)';
            Verb.VConjugation := LowerCase(Verb.VConjugation);
          end
          else if (Verb.VConjugation = 'M') or (Verb.VConjugation = 'I') then begin
            if (LeftStr(Verb.Participle, 2) = 'ge') and (LeftStr(Verb.VName, 2) <> 'ge') then begin
              S += ' (awer ouni ge-)';
              Verb.Participle := UTF8Copy(Verb.Participle, 3, UTF8Length(Verb.Participle) - 2);
            end;
          end;
          S += ' konjugéiert gëtt, also:';
        end
        else
          S += ' net separable Prefix ass an d''Verb ''t selwecht wéi "' + VerbBase + '" konjugéiert gëtt, also:';
        case Verb.VConjugation[1] of
          'T': S += ' Partizip = ge- + Radical + -t.';
          't': S += ' Partizip = Radical + -t (ouni ge-).';
          'N': S += ' Partizip = ge- Radical [+ -en].';
          'n': S += ' Partizip = Radical [+ -en] (ouni ge-).';
          'I': S += ' Partizip = onregelméisseg.';
          'M': S += ' Partizip mat Metaphonèse.';
        end;
        S += Chr(13);
      end;
      fConj.edVerbDetails.Lines.AddText(S);
      // Add warning for auxiliary
      if Verb.VType <> 'P' then begin
        S := 'Net auszëschléissen datt d''Verb mat "';
        if Verb.VAuxiliary = 'H' then
          S += 'sinn'
        else
          S += 'hunn';

        S += '" konjugéiert misst ginn!' + Chr(13);
        fConj.edVerbDetails.Lines.AddText(S);
      end;
      // Add warning for pronominal verb (using accusative if unknown)
      if Verb.VType = 'P' then begin
        S := 'Keng Garantie datt dëst Verb eng Pronominalform huet; och net auszeschléissen datt et mam Dativ konjugéiert misst ginn!';
        fConj.edVerbDetails.Lines.AddText(S);
      end;
    end;
    // Adapt participle verbform for verbs with unremovable prefix
    // (For verbs with removable prefix, the conjugation routine awaits these verbforms without prefix)
    if PrefixType = 'I' then begin
      Verb.Participle := VerbPrefix + Verb.Participle;
    end;
    // Be sure auxiliary of pronominal form is "hunn"
    if Verb.VType = 'P' then
      Verb.VAuxiliary := 'H';
    // Display infinitive of verb as form title
    if Verb.VType = 'P' then
      VerbName := 'sech ' + VerbName;
    fConj.stTitle.Caption := 'Konjugatioun vum Verb: ' + VerbName;
    // Conjugate the verb: "Perfekt" and "Plusquamperfekt" active, pronominal or not
    for I := 0 to 1 do begin
      Number := Numbers[I];
      for J := 1 to 3 do begin
        Person := J;
        if UpperCase(Verb.VType) = 'I' then begin
          // Impersonal verbs only exist for 3rd person singular (neuter genus)
          if (Number = 'singulier') and (Person = 3) then begin
            fConj.sgConjPC.Cells[I + 1, J] := Conjugation(Verb, Tenses[1], Number, Person, 'n');
            fConj.sgConjPQP.Cells[I + 1, J] := Conjugation(Verb, Tenses[2], Number, Person, 'n');
          end
          else begin
            fConj.sgConjPC.Cells[I + 1, J] := '------';
            fConj.sgConjPQP.Cells[I + 1, J] := '------';
          end;
        end
        else begin
          // For other verbs, conjugate for all persons (use masculine form for 3rd person sg)
          fConj.sgConjPC.Cells[I + 1, J] := Conjugation(Verb, Tenses[1], Number, Person, 'm');
          fConj.sgConjPQP.Cells[I + 1, J] := Conjugation(Verb, Tenses[2], Number, Person, 'm');
        end;
      end;
    end;
    // Show the form with the conjugation string-grid. Using "Show" (and not "ShowModal") allows to keep this form open,
    // when selecting to conjugate another verb and entering it on the main form
    fConj.Show;
  end;
end;

{ Menu item "Exercice > Verloossen": Exit application }

procedure TfVerbes.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Astellungen > Zuel vun de Froen": Get number of exercise questions from user }

procedure TfVerbes.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

{ The value entered is stored in a temporary variable and copy to the actual number of questions when "Neien Exercice" is selected }

begin
  S := InputBox('Lëtzeburgesch Verben', 'Zuel vun de Foen', IntToStr(iSQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                    // minimum of questions arbitrarily fixed to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);
  end;
end;

{ Menu items "Astellungen > Zäitenauswiel > ...": Tenses for exercices selection }

procedure TfVerbes.mSettingsTensesPPClick(Sender: TObject);

begin
  if mSettingsTensesPP.Checked then
    mSettingsTensesPP.Checked := False
  else
    mSettingsTensesPP.Checked := True;
end;

procedure TfVerbes.mSettingsTensesPCClick(Sender: TObject);

begin
  if mSettingsTensesPC.Checked then
    mSettingsTensesPC.Checked := False
  else
    mSettingsTensesPC.Checked := True;
end;

procedure TfVerbes.mSettingsTensesPQPClick(Sender: TObject);

begin
  if mSettingsTensesPQP.Checked then
    mSettingsTensesPQP.Checked := False
  else
    mSettingsTensesPQP.Checked := True;
end;

{ Menu item "Astellungen > Punkten op 100": Select of maximum mark is 60 or 100 }

procedure TfVerbes.mSettingsP100Click(Sender: TObject);

begin
  if mSettingsP100.Checked then
    mSettingsP100.Checked := False
  else
    mSettingsP100.Checked := True;
end;

{ Menu item "Astellungen > Keng Verben op -éieren": Select if "French" verb questions should be included or not }

procedure TfVerbes.mSettingsNoFrenchClick(Sender: TObject);

begin
  if mSettingsNoFrench.Checked then
    mSettingsNoFrench.Checked := False
  else
    mSettingsNoFrench.Checked := True;
end;

{ Menu item "Astellungen > Infinitiv verstoppen": Select if luxembourgish infinitive is displayed or not }

procedure TfVerbes.mSettingsInfHideClick(Sender: TObject);

begin
  if mSettingsInfHide.Checked then
    mSettingsInfHide.Checked := False
  else
    mSettingsInfHide.Checked := True;
  edVerb.Text := '';
end;

{ Menu item "Astellungen > -en Engunge wegloossen": Select participles in -en should be with or without this suffix }

procedure TfVerbes.mSettingsNoENClick(Sender: TObject);

begin
  if mSettingsNoEN.Checked then
    mSettingsNoEN.Checked := False
  else
    mSettingsNoEN.Checked := True;
end;

{ Menu item "Hëllef > Hëllef": Display program help }

procedure TfVerbes.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Hëllef > Iwwer": Display program about }

procedure TfVerbes.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'Luxverbes2 ass ee Programm, deen Exercicen iwwert d''Konjugatioun vun de ';
    S += 'lëtzebuergesche Verben am Perfekt (passé composé) a Plusquamperfekt generéiert.' + Chr(13) + Chr(13);
    S += 'Versioun 1.0, © allu, November, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Fro/Äntwert": Generate new question resp. check user answer }

procedure TfVerbes.btQuestionClick(Sender: TObject);

var
  Person, IX, P, T, PMax: Integer;
  Number, Genus, Tense: string;
  OK: Boolean;

begin
  // Button "Fro": Generate new conjugation question
  if btQuestion.Caption = 'Fro' then begin
    if iQuestion < iQuestions then begin
      if mSettingsTensesPP.Checked or mSettingsTensesPC.Checked or mSettingsTensesPQP.Checked then begin
        Inc(iQuestion);
        laQuestion.Caption := 'Fro ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
        // Choose a random verb
        repeat
          OK := True;
          IX := Random(Length(aVerbs));
          if mSettingsNoFrench.Checked and (UTF8Copy(aVerbs[IX].VName, UTF8Length(aVerbs[IX].VName) - 5, 6) = 'éieren') then
            OK := False;
        until OK;
        // Display verb infinitive (unless not to do so is selected)
        if not mSettingsInfHide.Checked then
          edVerb.Text := AdjustVerb(aVerbs[IX].VName, aVerbs[IX].VType);
        // Display German and French translation of verb
        edVerbGe.Text := aVerbs[IX].VGerman; edVerbFr.Text := aVerbs[IX].VFrench;
        // Choose random tense (among those selected)
        repeat
          OK := True;
          T := Random(3);
          case T of
            0: if not mSettingsTensesPP.Checked then OK := False;
            1: if not mSettingsTensesPC.Checked then OK := False;
            2: if not mSettingsTensesPQP.Checked then OK := False;
          end;
        until OK;
        Tense := Tenses[T];
        // Choose random number and person (except for participle and impersonal verbs)
        if UpperCase(aVerbs[IX].VType) = 'I' then begin
          // Impersonal verbs
          Number := Numbers[0]; Person := 3; Genus := Genuus[2];
        end
        else begin
          // Number and person not considered for particple
          if Tense = Tenses[0] then begin
            Number := ''; Person := 0;
          end
          else begin
            Number := Numbers[Random(2)]; Person := Random(3) + 1;
          end;
          Genus := '';
          // For 3rd person sg, choose random genus
          if (Number = 'singulier') and (Person = 3) then
            Genus := Genuus[Random(3)];
        end;
        // Display conjugation question
        edConj.Text := Tense;
        if Tense <> Tenses[0] then begin
          edConj.Text := edConj.Text + ' (' + IntToStr(Person) + '. Pers. ' + Number;
          if Genus <> '' then
            edConj.Text := edConj.Text + ', ' + Genus;
          edConj.Text := edConj.Text + ')';
        end;
        // Conjugate the verb with actual parameters
        sAnswer := Conjugation(aVerbs[IX], Tense, Number, Person, Genus);
        // Clear answer fields and change button caption (next push will be a user answer)
        edAnswer.Text := ''; edAnswer.SetFocus;
        edEval.Text := '';
        btQuestion.Caption := 'Äntwert';
      end
      else
        MessageDlg('Fehler', 'Et ass keng Verbzäit ausgewielt!', mtError, [mbOK], 0);
    end;
  end
  // Button "Äntwert": Check user answer and update evaluation
  else begin
    if iQuestion <= iQuestions then begin
      // Correct answer
      if edAnswer.Text = sAnswer then begin
        Inc(iCorrect);
        edEval.Text := 'Richteg!';
        edEval.Font.Color := clDefault;
      end
      // False answer
      else begin
        Inc(iFalse);
        edEval.Text := 'Falsch! Richteg = ' + sAnswer;
        edEval.Font.Color := clRed;
      end;
      // Update evaluation values
      edQuestions.Text := IntToStr(iQuestion);
      edCorrect.Text := IntToStr(iCorrect);
      edFalse.Text := IntToStr(iFalse);
      // Calculate success (marks with maximum of 60 or 100 as selected)
      if mSettingsP100.Checked then
        PMax := 100
      else
        PMax := 60;
      P := Round(PMax * (iCorrect / iQuestion));
      edSuccess.Text := IntToStr(P);
      // Use different colors for different success ranges
      if 100 * (P / PMax) >= 60 then
        edSuccess.Color := clLime
      else if 100 * (P / PMax) >= 50 then
        edSuccess.Color := clYellow
      else
        edSuccess.Color := clRed;
    end;
    // Change button caption (next push will be another question to generate)
    btQuestion.Caption := 'Fro';
    // If all questions are done, display "end of exercise" message
    if iQuestion = iQuestions then begin
      MessageDlg('Lëtzebuergesch Verben', 'Enn vum Exercice...', mtInformation, [mbOK], 0);
      Inc(iQuestion);                                                          // this avoids that message is displayed more than once
    end;
  end;
end;

{ Not-English letter buttons: Include the corr. letter into user answer }

procedure TfVerbes.btAccAiguClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'é';
end;

procedure TfVerbes.btAccGraveClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'è';
end;

procedure TfVerbes.btTremaClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ë';
end;

procedure TfVerbes.btAeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ä';
end;

procedure TfVerbes.btOeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ö';
end;

procedure TfVerbes.btUeClick(Sender: TObject);

begin
  edAnswer.Text := edAnswer.Text + 'ü';
end;

end.

