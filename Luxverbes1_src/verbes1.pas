{****************************************}
{* Main unit for Luxverbes1 application *}
{****************************************}

unit verbes1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, PopupNotifier, IniFiles, LazUTF8, conj1, help;

type
  TVerb = record
    VName: string;
    VType, VCasus, VPrefix, VConjugation: string[1];
    VGerman, VFrench: string;
    Present2, Present3: string;
  end;
  TVerbs = array of TVerb;
  {************}
  {* TfVerbes *}
  {************}
  TfVerbes = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseConj, mExerciseExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsP100, mSettingsInfHide, mSettingsNoFrench, mSettingsNoPlural: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    laQuestion: TLabel;
    edVerb: TEdit;
    edVerbGe: TEdit;
    edVerbFr: TEdit;
    edConj: TEdit;
    edAnswer: TEdit;
    edEval: TEdit;
    edQuestions: TEdit;
    edCorrect: TEdit;
    edFalse: TEdit;
    edSuccess: TEdit;
    btQuestion: TButton;
    btAccAigu: TButton;
    btAccGrave: TButton;
    btTrema: TButton;
    btAe: TButton;
    btOe: TButton;
    btUe: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseConjClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsP100Click(Sender: TObject);
    procedure mSettingsInfHideClick(Sender: TObject);
    procedure mSettingsNoFrenchClick(Sender: TObject);
    procedure mSettingsNoPluralClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAccAiguClick(Sender: TObject);
    procedure btAccGraveClick(Sender: TObject);
    procedure btTremaClick(Sender: TObject);
    procedure btAeClick(Sender: TObject);
    procedure btOeClick(Sender: TObject);
    procedure btUeClick(Sender: TObject);
  private
    iSQuestions, iQuestions, iQuestion, iCorrect, iFalse: Integer;
    sAnswer: string;
    aVerbs: TVerbs;
  end;

const
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
  IrregFile: TINIFile;

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
  // Vowel doubling rule (for stem ending in long i,u,o,a,ä,ö,ü, followed by a consonant)
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
  N, P: Integer;
  Line, S: string;
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
        VPrefix := UTF8Copy(Line, 26, 1);
        VConjugation := UTF8Copy(Line, 27, 1);
        VGerman := RTrim(UTF8Copy(Line, 31, 20));
        VFrench := RTrim(UTF8Copy(Line, 51, 25));
        Present2 := '-'; Present3 := '-';
        if VConjugation = 'M' then begin
          S := RTrim(UTF8Copy(Line, 76, UTF8Length(Line)));
          P := Pos(',', S);                                                    // after usage of "Pos", you must use "Copy" and not "UTF8Copy"!
          Present2 := Copy(S, 1, P - 1);
          Present3 := Copy(S, P + 1, Length(S));
        end;
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

{ Luxembourgish verbs conjugation: Present time (Indikativ Präsens) }

function ConjIndPres(Verb: TVerb; Number: string; Person: Integer; Genus: string): string;

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
  // "Indikativ Präsens" suffixes
  Suffixes: array[1 .. 2, 1 .. 3] of string = (
    ('en', 's', 't'),
    ('en', 't', 'en')
  );

var
  NumberX, P: Integer;
  VerbName0, VerbName, Stem, Prefix, Suffix, Pronoun, Reflexive, ConjVerb, InifileSection, InifileKey: string;

begin
  VerbName0 := Verb.VName; VerbName := VerbName0; Prefix := '';
  // Verb with prefix (indicated with a hyphen as read from verbs file )
  if (Verb.VPrefix = 'I') or (Verb.VPrefix = 'S') then begin
    P := Pos('-', VerbName);
    Prefix := Copy(VerbName, 1, P - 1);                                        // must use "Copy" after usage of "Pos"
    VerbName0 := Copy(VerbName, P + 1, Length(VerbName));
    if Verb.VPrefix = 'I' then begin
      // Unremovable prefix
      Prefix := AdjustEiffel(Prefix, VerbName0);
      VerbName := Prefix + VerbName0;                                          // verb name = prefix + verb base
    end
    else if Verb.VPrefix = 'S' then
      // Removable prefix
      VerbName := VerbName0;                                                   // verb name = verb base (prefix is separated for conjugation)
  end;
  // Transform string numbers to integer indexes
  if Number = 'singulier' then
    NumberX := 1
  else
    NumberX := 2;
  // Get the pronouns for actual number and person
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
  // ---------------------------------------------------
  // Luxembourgish verbs conjugation (Indikativ Präsens)
  // ---------------------------------------------------
  if Verb.VConjugation = 'I' then begin
    // Irregular verb (one of those detailed in the "irreguliers" INI-like text file or a compound form of one of these)
    // Conjugated verb form is directly read from the "irreguliers" file (using the verb base to access data in the file)
    InifileSection := VerbName0;
    if Number = 'singulier' then
      InifileKey := 'sg'
    else
      InifileKey := 'pl';
    InifileKey += IntToStr(Person);
    ConjVerb := IrregFile.ReadString(InifileSection, InifileKey, '');
    // Adaption for verb with unremovable prefix: add the prefix to the conjugated verb
    if Verb.VPrefix = 'I' then begin
      Prefix := AdjustEiffel(Prefix, ConjVerb);
      ConjVerb := Prefix + ConjVerb;
    end;
  end
  else begin
    // Non-irregular verb (any verb with a base not detailed in the "irreguliers" file)
    Stem := UTF8Copy(VerbName, 1, UTF8Length(VerbName) - 2);                // this stem = "racine" (final "radical" may be different)
    Suffix  := Suffixes[NumberX, Person];                                   // number and person dependent suffix
    if (Verb.VConjugation = 'M') and (Number = 'singulier') and ((Person = 2) or (Person = 3)) then begin
      // Verb with "métaphonèse": 2nd and 3rd person sg. have been indicated in the verbs file
      if Person = 2 then
        ConjVerb := Verb.Present2
      else
        ConjVerb := Verb.Present3;
    end
    else begin
      // Regular conjugation (i.e. non-irregualr word and non-"métaphonèse" condition)
      if Verb.VConjugation <> 'R' then
        // Except if explicitly said that the verb is regular, apply lux. spelling rules to the stem for actual suffix
        Stem := StemModifications(Stem, Suffix);
      // Adaption: Special conjugation rules
      if RightStr(Stem, 1) = 't' then begin
        // Rule: If stem ends in -t, suffix "t" is dropped
        if Suffix = 't' then
          Suffix := '';
      end;
      if (RightStr(Stem, 1) = 's') or (RightStr(Stem, 1) = 'z') or (RightStr(Stem, 1) = 'x') then begin
        // Rule: If stem ends in -s, -z, -x, suffix "s" is dropped
        if Suffix = 's' then
          Suffix := '';
      end;
      if (UTF8Copy(Stem, UTF8Length(Stem) - 1, 2) = 'ee') then begin
        // Rule: If stem ends in -ee, suffix "en" becomes a trema
        if Suffix = 'en' then
          Suffix := 'ën';
      end;
      // Conjugated verb form: stem (radical) + suffix (for actual number and person)
      ConjVerb := Stem + Suffix;
    end;
  end;
  // Add the personal pronoun
  Pronoun := AdjustEiffel(Pronoun, ConjVerb);                                  // Eiffel rule for personal pronoun (followed by verb)
  ConjVerb := Pronoun + ' ' + ConjVerb;
  // For a pronominal verb, add the reflexive pronoun
  if Verb.VType = 'P' then begin
    ConjVerb := AdjustEiffel(ConjVerb, Reflexive);                             // Eiffel rule for conjugated verb (followed by reflexive pronoun)
    ConjVerb += ' ' + Reflexive;
  end;
  // For a verb with removable prefix, add this prefix at the end of the conjugated verb
  if Verb.VPrefix = 'S' then begin
    ConjVerb := AdjustEiffel(ConjVerb, Prefix);                                // Eiffel rule for conjugated verb (followed by removable prefix)
    ConjVerb += ' ' + Prefix;
  end;
  ConjIndPres := ConjVerb;
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
  IrregFile := TINIFile.Create('irreguliers.txt');                             // the "irreguliers.txt" file is accessed as a FPC INI file
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

{ The routine conjugates the verb and fills in the string-grid on the fConj form }

begin
  Pronominal := False;   fConj.edVerbDetails.Clear;
  // Get the verb to conjugate from user
  VerbName := InputBox('Lëtzeburgesch Verben', 'Indikativ Präsens vu wellechem Verb', '');
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
        'R': S += ' huet e regelméissegen Indikativ Präsens.';
        'I': S += ' ass en onregelméissegt Verb.';
        'M': S += ' ass e Verb mat Metaphonèse an deer 2. an 3. Persoun singulier vum Indikativ Präsens.';
        'S': begin
               S += ' huet e regelméissegen Indikativ Präsens, huet awer fir eng oder méi Persounen eng speziell Form,';
               S += ' déi op d''Regele vun der lëtzebuergescher Orthographie zeréckzeféiere sinn.'
             end;
      end;
      fConj.edVerbDetails.Lines.AddText(S);
    end
    else begin
      // Verb has not been found in the list: Warning message and try to conjugate "as well as possible"
      S := '"' + UpperFirst(VerbName) + '"';
      S += ' ass e Verb, dat de "Luxverbes1" Programm net kennt! Seng Konjugatioun, wéi hei affichéiert, ass ouni Garantie';
      S += ' a kann, speziell da wann ët sech ëm e Verb mat Metaphonèse hanndelt, een oder méi Fehler opweisen!' + Chr(13);
      fConj.edVerbDetails.Lines.AddText(S);
      Verb.VName := VerbName; Verb.VFrench := ''; Verb.VGerman := '';
      Verb.VType := '?'; Verb.VCasus := '?'; Verb.VConjugation := '?'; Verb.VPrefix := '?';
      Verb.Present2 := ''; Verb.Present3 := '';
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
        // Verbbase has been found in the list: Conjugate the verb using the verbbase as model
        if IX <> -1 then begin
          // Conjugation type should be ok now
          Verb.VConjugation := aVerbs[IX].VConjugation;
          if Verb.VConjugation = 'M' then begin
            // For verbs with "métaphonèse" use 2nd and 3rd person sg verbforms as specified for the baseverb
            Verb.Present2 := aVerbs[IX].Present2;
            Verb.Present3 := aVerbs[IX].Present3;
            // Adapt 2nd and 3rd person sg verbforms for verbs with unremovable prefix
            // (For verbs with removable prefix, the conjugation routine awaits these verbforms without prefix)
            if PrefixType = 'I' then begin
              Verb.Present2 := VerbPrefix + Verb.Present2;
              Verb.Present3 := VerbPrefix + Verb.Present3;
            end;
          end;
        end;
      end;
      // Details about how the program conjugated the unknown verb
      if IsFrenchVerb(VerbBase) then begin
        // Verbs directly dirived from French are considered to be regular
        S := 'D''Endung "éieren" suggeréiert dass d''Verb direkt aus dem Franséischen iwwerholl ass. De Programm geet duefir dervun';
        S += ' aus dass d''Verb eng regelméisseg Konjugatioun huet.'
      end
      else if Verb.VConjugation = '?' then begin
        // If no prefix has been found or if the verbbase is unknown, the verb is conjugated as if it was regular.
        // This will always result in a conjugation with errors if there is "métaphonèse" for this verb
        S := 'De Programm huet keng kloer Indikatiounen iwwert d''Konjugatioun (notamment wat eng eventuell Metaphonèse';
        S += ' ugeet) fond an ët wéi e regelméissegt Verb, ënner Berücksichtigung vun den Orthographiesregele, konjugéiert.'
      end
      else begin
        // The verb is supposed to be a compound of a prefix and a known baseverb and will be conjugated similarly to this one
        // (This should be correct results for nearly all irregular verbs and a good part of unknown verbs with "métaphonèse")
        S := 'De Programm geet dervun aus dass "' + VerbPrefix + '" ee ';
        if PrefixType = 'S' then
          S += 'separable'
        else
          S += 'net separable';
        S += ' Prefix ass an d''Verb ''t selwecht wéi "' + VerbBase + '" konjugéiert gëtt, also:';
        case Verb.VConjugation[1] of
          'R': S += ' regelméissegen Indikativ Präsens.';
          'I': S += ' onregelméissegt Verb.';
          'M': S += ' Verb mat Metaphonèse.';
          'S': S += ' regelméissegen Indikativ Präsens mat Particularitéite wéinst den Orthographie-Regelen.';
        end;
        S += Chr(13);
      end;
      fConj.edVerbDetails.Lines.AddText(S);
      if Verb.VType = 'P' then begin
        S := 'Keng Garantie datt dëst Verb eng Pronominalform huet; och net auszeschléissen datt et mam Dativ konjugéiert misst ginn!';
        fConj.edVerbDetails.Lines.AddText(S);
      end;
    end;
    // Display infinitive of verb as form title
    if Verb.VType = 'P' then
      VerbName := 'sech ' + VerbName;
    fConj.stTitle.Caption := 'Indikativ Präsens vum Verb: ' + VerbName;
    // Conjugate the verb: "Indikativ Präsens" active, pronominal or not
    for I := 0 to 1 do begin
      Number := Numbers[I];
      for J := 1 to 3 do begin
        Person := J;
        if UpperCase(Verb.VType) = 'I' then begin
          // Impersonal verbs only exist for 3rd person singular (neuter genus)
          if (Number = 'singulier') and (Person = 3) then
            fConj.sgConj.Cells[I + 1, J] := ConjIndPres(Verb, Number, Person, 'n')
          else
            fConj.sgConj.Cells[I + 1, J] := '------';
        end
        else
          // For other verbs, conjugate for all persons (use masculine form for 3rd person sg)
          fConj.sgConj.Cells[I + 1, J] := ConjIndPres(Verb, Number, Person, 'm');
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

{ Menu item "Astellungen > Punkten op 100": Select of maximum mark is 60 or 100 }

procedure TfVerbes.mSettingsP100Click(Sender: TObject);

begin
  if mSettingsP100.Checked then
    mSettingsP100.Checked := False
  else
    mSettingsP100.Checked := True;
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

{ Menu item "Astellungen > -éieren Froe wegloossen": Select if questions with verbs in -éieren should be included or not }

procedure TfVerbes.mSettingsNoFrenchClick(Sender: TObject);

begin
  if mSettingsNoFrench.Checked then
    mSettingsNoFrench.Checked := False
  else
    mSettingsNoFrench.Checked := True;
end;

{ Menu item "Astellungen > Pluriel Froe wegloossen": Select if questions with plural conjugation should be included or not }

procedure TfVerbes.mSettingsNoPluralClick(Sender: TObject);

begin
  if mSettingsNoPlural.Checked then
    mSettingsNoPlural.Checked := False
  else
    mSettingsNoPlural.Checked := True;
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
    S := 'Luxverbes1 ass ee Programm, deen Exercicen iwwert d''Konjugatioun vun de ';
    S += 'lëtzebuergesche Verben am Indikativ Präsens generéiert.' + Chr(13) + Chr(13);
    S += 'Versioun 1.1, © allu, November, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Fro/Äntwert": Generate new question resp. check user answer }

procedure TfVerbes.btQuestionClick(Sender: TObject);

var
  Person, IX, P, PMax: Integer;
  Number, Genus: string;
  OK: Boolean;

begin
  // Button "Fro": Generate new conjugation question
  if btQuestion.Caption = 'Fro' then begin
    if iQuestion < iQuestions then begin
      Inc(iQuestion);
      laQuestion.Caption := 'Fro ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
      // Choose a random verb (exclude verbs in -éieren if this option is selected)
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
      // Choose random number and person (except for impersonal verbs)
      if UpperCase(aVerbs[IX].VType) = 'I' then begin
        Number := 'singulier'; Person := 3; Genus := 'n';
      end
      else begin
        if mSettingsNoPlural.Checked then
          // Always use "singulier" if exclude plurals is selected
          Number := 'singulier'
        else
          Number := Numbers[Random(2)];
        Person := Random(3) + 1;
        Genus := '';
        // For 3rd person sg, choose random genus
        if (Number = 'singulier') and (Person = 3) then
          Genus := Genuus[Random(3)];
      end;
      // Display conjugation question
      edConj.Text := IntToStr(Person) + '. Persoun ' + Number;
      if Genus <> '' then
        edConj.Text := edConj.Text + ' (' + Genus + ')';
      // Conjugate the verb with actual parameters
      sAnswer := ConjIndPres(aVerbs[IX], Number, Person, Genus);
      // Clear answer fields and change button caption (next push will be a user answer)
      edAnswer.Text := ''; edAnswer.SetFocus;
      edEval.Text := '';
      btQuestion.Caption := 'Äntwert';
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

