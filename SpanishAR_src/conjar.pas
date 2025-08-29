{************************************************}
{* Conjugation of Spanish verbs in -ar routines *}
{************************************************}

unit conjar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8, Dialogs;

const
  NVerbsMax = 101; NVerbsSpecial = 3;
  NMoods = 5; NTensesMax = 5; NForms = 2;
  NNumbers = 2; NPersons = 3; NGenders = 2;

type
  TVerb = record
    VerbName, VerbNamesEng: string;
    Conjugation: Char;
  end;
  TVerbs = array[0..NVerbsMax - 1] of TVerb;
  TTenses = array[1..NMoods, 1..NTensesMax] of string;

const
  Moods: array[1..NMoods] of string = (
    'infinitive', 'participle', 'imperative', 'indicative', 'subjunctive'
  );
  NTenses: array[1..NMoods] of Integer = (
    1, 2, 1, 5, 2
  );
  Tenses: TTenses = (
    ('infinitive', '', '', '', ''),
    ('gerund', 'past participle', '', '', ''),
    ('imperative', '', '', '', ''),
    ('present', 'imperfect', 'preterite', 'future', 'conditional'),
    ('present', 'imperfect', '', '', '')
  );
  VForms: array[1..NForms] of string = (
    'affirmative', 'negative'
  );
  Numbers: array[1..NNumbers] of string = (
    'singular', 'plural'
  );
  Persons: array[1..NPersons] of string = (
    '1st person', '2nd person', '3rd person'
  );
  Genders: array[1..NGenders] of string = (
    'masculine', 'feminine'
  );
  PersPronounsMasc: array[1..NNumbers, 1..NPersons] of string = (
    ('yo', 'tú', 'él'),
    ('nosotros', 'vosotros', 'ellos')
  );
  PersPronounsFem: array[1..NNumbers, 1..NPersons] of string = (
    ('yo', 'tú', 'ella'),
    ('nosotras', 'vosotras', 'ellas')
  );
  VerbUnknown: TVerb = (
    VerbName: ''; VerbNamesEng: ''; Conjugation: '?';
  );

function Conjugation(Verb, VerbType: TVerb; Mood, Tense, Form, Number, Person, Gender: string; ImpPron: Boolean; VerbFile: TINIFile): string;

implementation

{ Get personal pronoun for given gender, number, person (and mood) }

function PersonalPronoun(Gender, Number, Person, Mood: string): string;

var
  XNumber, XPerson: Integer;
  Pronoun: string;

begin
  if Number = 'singular' then
    XNumber := 1
  else
    XNumber := 2;
  XPerson := StrToInt(LeftStr(Person, 1));
  if Gender = 'masculine' then
    Pronoun := PersPronounsMasc[XNumber, XPerson]
  else
    Pronoun := PersPronounsFem[XNumber, XPerson];
  if Mood = 'imperative' then begin
    Pronoun := StringReplace(Pronoun, 'él', 'usted', []);
    Pronoun := StringReplace(Pronoun, 'ella', 'usted', []);
    Pronoun := StringReplace(Pronoun, 'ellos', 'ustedes', []);
    Pronoun := StringReplace(Pronoun, 'ellas', 'ustedes', []);
  end;
  Result := Pronoun;
end;

{ Main verbs in -ar "voice = active" conjugation routine }

function ConjugationActive(Verb, VerbType: TVerb; Mood, Tense, Form, Number, Person: string; VerbFile: TINIFile): string;

// The routine only conjugates the simple tenses of the verb

var
  XNumber, XPerson, P: Integer;
  RootVerb, RootVerbType, VerbConj, InifileSection, InifileKey, InifileTense: string;

begin
  VerbConj := '';
  // Get indexes for 'number' and 'person' arrays
  if (Mood = 'infinitive') or (Mood = 'participle') then begin
    XNumber := 0; XPerson := 0;
  end
  else begin
    if Number = 'singular' then
      XNumber := 1
    else
      XNumber := 2;
    XPerson := StrToInt(LeftStr(Person, 1));
  end;
  // Read conjugation for selected verb from appropriate conjugation file:
  // Files for regular verbs, one for each of the 4 semi-irregular cases, one for each irregular verb
  IniFileTense := StringReplace(Tense, ' ', '_', []);
  if (Mood = 'infinitive') or (Mood = 'participle') or (Mood = 'imperative') then
    InifileSection := InifileTense
  else
    InifileSection := Mood + '_' + InifileTense;
  IniFileKey := 'P' + IntToStr(XNumber) + IntToStr(XPerson);
  VerbConj := VerbFile.ReadString(InifileSection, InifileKey, '');
  if Mood = 'imperative' then begin
    // 2nd pers. imperative has different conjugation for affirmative and negative form
    P := UTF8Pos(',', VerbConj);                                               // in the file, the 2 forms are separated by comma
    if P <> 0 then begin
      if Form = 'affirmative' then
        VerbConj := UTF8Copy(VerbConj, 1, P - 1)
      else
        VerbConj := UTF8Copy(VerbConj, P + 1, UTF8Length(VerbConj));
    end;
  end;
  if RightStr(Verb.VerbName, 2) = 'ar' then begin
    // Create conjugated form of selected verb:
    // The conjugated verb read from the file is for one of the verbs mentionned above;
    // Verbs typed as "T", "S" or "I" need no changes. For the others ("R" and "s"):
    // To get the conjugated form of the actual verb, the root of the verb read has to
    // be changed to the one of the actual verb
    RootVerbType  := UTF8Copy(VerbType.VerbName, 1, UTF8Length(VerbType.VerbName) - 2);
    RootVerb := UTF8Copy(Verb.VerbName, 1, UTF8Length(Verb.VerbName) - 2);
    if Verb.Conjugation = 'R' then begin
      // ****************************
      // General case (regular verbs)
      // ****************************
      // The root of the conjugated verb as read from the 'type verb' conjugation file has to
      // be replaced with the one of the verb actually selected
      VerbConj := StringReplace(VerbConj, RootVerbType, RootVerb, []);
    end
    else if Verb.Conjugation = 's' then begin
      // ********************
      // Semi-irregular verbs
      // ********************
      // 's' indicates a semi-irregular verb, conjugated similarily to some other semi-irregular verb
      // not having its own conjugation file, the root has to be changed to the one of the actual verb
      // Different processing for verbs in -car, -gar, -zar and -uar
      if RightStr(Verb.VerbName, 3) = 'car' then begin
        // Verbs ending in -car
        // --------------------
        if ((Mood = 'indicative') and (Tense = 'preterite') and (Number = 'singular') and (Person = '1st person')) or
          ((Mood = 'subjunctive') and (Tense = 'present')) or
          ((Mood = 'imperative') and not ((RightStr(VerbConj, 1) = 'a') or (RightStr(VerbConj, 2) = 'ad'))) then begin
          UTF8Delete(RootVerbType, UTF8Length(RootVerbType), 1); RootVerbType += 'qu';
          UTF8Delete(RootVerb, UTF8Length(RootVerb), 1); RootVerb += 'qu';
        end;
        VerbConj := StringReplace(VerbConj, RootVerbType, RootVerb, []);
      end
      else if RightStr(Verb.VerbName, 3) = 'gar' then begin
        // Verbs ending in -gar
        // --------------------
        if ((Mood = 'indicative') and (Tense = 'preterite') and (Number = 'singular') and (Person = '1st person')) or
          ((Mood = 'subjunctive') and (Tense = 'present')) or
          ((Mood = 'imperative') and not ((RightStr(VerbConj, 1) = 'a') or (RightStr(VerbConj, 2) = 'ad'))) then begin
          RootVerbType += 'u';
          RootVerb += 'u';
        end;
        VerbConj := StringReplace(VerbConj, RootVerbType, RootVerb, []);
      end
      else if RightStr(Verb.VerbName, 3) = 'zar' then begin
        // Verbs ending in -zar
        // --------------------
        if ((Mood = 'indicative') and (Tense = 'preterite') and (Number = 'singular') and (Person = '1st person')) or
          ((Mood = 'subjunctive') and (Tense = 'present')) or
          ((Mood = 'imperative') and not ((RightStr(VerbConj, 1) = 'a') or (RightStr(VerbConj, 2) = 'ad'))) then begin
          UTF8Delete(RootVerbType, UTF8Length(RootVerbType), 1); RootVerbType += 'c';
          UTF8Delete(RootVerb, UTF8Length(RootVerb), 1); RootVerb += 'c';
        end;
        VerbConj := StringReplace(VerbConj, RootVerbType, RootVerb, []);
      end
      else if RightStr(Verb.VerbName, 3) = 'uar' then begin
        // Verbs ending in -uar
        // --------------------
        if ((Mood = 'indicative') and (Tense = 'present') and ((Number = 'singular') or ((Number = 'plural') and (Person = '3rd person')))) or
          ((Mood = 'subjunctive') and (Tense = 'present') and ((Number = 'singular') or ((Number = 'plural') and (Person = '3rd person')))) or
          ((Mood = 'imperative') and ((Number = 'singular') or ((Number = 'plural') and (Person = '3rd person')))) then begin
          UTF8Delete(RootVerbType, UTF8Length(RootVerbType), 1); RootVerbType += 'ú';
          UTF8Delete(RootVerb, UTF8Length(RootVerb), 1); RootVerb += 'ú';
        end;
        VerbConj := StringReplace(VerbConj, RootVerbType, RootVerb, []);
      end;
    end;
  end;
  // For negative form: Add 'no'
  if Form = 'negative' then
    VerbConj := 'no ' + VerbConj;
  Result := VerbConj;
end;

{ Main verbs in -ar conjugation routine }

function Conjugation(Verb, VerbType: TVerb; Mood, Tense, Form, Number, Person, Gender: string; ImpPron: Boolean; VerbFile: TINIFile): string;

// The routine only conjugates the active voice of simple tenses
// Thus, are not conjugated:
//   - passive and pronominal voice
//   - compound tenses

var
  Conj: string;

begin
  Conj := '';
  // Conjugation of -ar verbs (voice = active; simple tenses only)
  if RightStr(Verb.VerbName, 2) = 'ar' then begin
    Conj := ConjugationActive(Verb, VerbType, Mood, Tense, Form, Number, Person, VerbFile);
    if Conj <> 'n/a' then begin
      // Add personal pronoun (for personal Moods)
      if (Mood = 'indicative') or (Mood = 'subjunctive') or ((Mood = 'imperative') and ImpPron) then
        // Personal pronoun for imperative only added if selected so
        Conj := PersonalPronoun(Gender, Number, Person, Mood) + ' ' + Conj;
    end;
    Result := Conj;
  end;
end;

end.

