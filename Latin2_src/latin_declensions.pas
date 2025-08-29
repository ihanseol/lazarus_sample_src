{**************************}
{* Latin declensions unit *}
{**************************}

unit latin_declensions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSuffixs = array[1..6] of string;
  TSuffix = record
    Sg, Pl: TSuffixs;
  end;

const
  AllCasus: array[1..6] of string = (
    'no', 'vo', 'ac', 'ge', 'da', 'ab'
  );
  AllNumbers: array[1..2] of string = (
    'sg', 'pl'
  );
  AllGenus: array[1..3] of string = (
    'm', 'f', 'n'
  );

function DeclensionNouns(Declension: Integer; Nominative, Genitive, Genus, Casus, Number: string): string;
function DeclensionNouns1(Nominative, Genitive, Casus, Number: string): string;
function DeclensionNouns2(Nominative, Genitive, Genus, Casus, Number: string): string;
function DeclensionNouns3(Nominative, Genitive, Genus, Casus, Number: string): string;
function DeclensionNouns4(Nominative, Genitive, Casus, Number: string): string;
function DeclensionNouns5(Genitive, Casus, Number: string): string;
function DeclensionAdjs(Masculine, Feminine, Neuter, Genus, Casus, Number: string; Person: Boolean): string;
function DeclensionAdjs1(Masculine, Feminine, Genus, Casus, Number: string): string;
function DeclensionAdjs2(Masculine, Feminine, Neuter, Genus, Casus, Number: string; Person: Boolean): string;
function CasusSuffix(Casus, Number: string; Suffixes: TSuffix): string;

implementation

{ Latin nouns declension: Main }

function DeclensionNouns(Declension: Integer; Nominative, Genitive, Genus, Casus, Number: string): string;

var
  NounDecl: string;

begin
  case Declension of
    1: NounDecl := DeclensionNouns1(Nominative, Genitive, Casus, Number);
    2: NounDecl := DeclensionNouns2(Nominative, Genitive, Genus, Casus, Number);
    3: NounDecl := DeclensionNouns3(Nominative, Genitive, Genus, Casus, Number);
    4: NounDecl := DeclensionNouns4(Nominative, Genitive, Casus, Number);
    5: NounDecl := DeclensionNouns5(Genitive, Casus, Number);
  end;
  Result := NounDecl;
end;

{ Latin nouns declension: 1st declension }

function DeclensionNouns1(Nominative, Genitive, Casus, Number: string): string;

{ Genitive stem + suffix; exceptions: dat/abl pl. for "dea" and "filia" }

const
  Suffixes: TSuffix = (
    Sg: ('a', 'a', 'am', 'ae', 'ae', 'a');
    Pl: ('ae', 'ae', 'as', 'arum', 'is', 'is');
  );

var
  Stem, NounDecl: string;

begin
  NounDecl := '';
  Stem := LeftStr(Genitive, Length(Genitive) - 2);
  if Number = 'sg' then begin
    NounDecl := Stem + CasusSuffix(Casus, 'sg', Suffixes);
  end
  else begin
    if ((Nominative = 'dea') or (Nominative = 'filia')) and ((Casus = 'da') or (Casus = 'ab')) then
      NounDecl := Stem + 'abus'
    else
      NounDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
  end;
  Result := NounDecl;
end;

{ Latin nouns declension: 2nd declension }

function DeclensionNouns2(Nominative, Genitive, Genus, Casus, Number: string): string;

{ Genitive stem + suffix, that is different for nouns in -us (and -er) and the neuter nouns in -um; }
{ special case for nom/voc sg. of nouns in -er; exceptions: nom/voc/acc sg. and having no plural for neuter }
{ nouns in -us, voc sg. for "filius" }

const
  SuffixesUS: TSuffix = (
    Sg: ('us', 'e', 'um', 'i', 'o', 'o');
    Pl: ('i', 'i', 'os', 'orum', 'is', 'is');
  );
  SuffixesUM: TSuffix = (
    Sg: ('um', 'um', 'um', 'i', 'o', 'o');
    Pl: ('a', 'a', 'a', 'orum', 'is', 'is');
  );

var
  Stem, NounDecl: string;
  Suffixes: TSuffix;

begin
  NounDecl := '';
  Stem := LeftStr(Genitive, Length(Genitive) - 1);
  if RightStr(Nominative, 2) = 'us' then
    Suffixes := SuffixesUS
  else if RightStr(Nominative, 2) = 'um' then
    Suffixes := SuffixesUM
  else if (RightStr(Nominative, 2) = 'er') or (Nominative = 'vir') then
    Suffixes := SuffixesUS;
  if ((RightStr(Nominative, 2) = 'er') or (Nominative = 'vir')) and ((Casus = 'no') or (Casus = 'vo')) and (Number = 'sg') then
    NounDecl := Nominative
  else begin
    if Number = 'sg' then begin
      if (RightStr(Nominative, 2) = 'us') and (Genus = 'n') then begin
        if ((Casus = 'no') or (Casus = 'vo') or (Casus = 'ac')) and (Number = 'sg') then
          NounDecl := Stem + 'us'
        else
          NounDecl := Stem + CasusSuffix(Casus, 'sg', Suffixes);
      end
      else if (Nominative = 'filius') and (Casus = 'vo') then
        NounDecl := 'fili'
      else
        NounDecl := Stem + CasusSuffix(Casus, 'sg', Suffixes);
    end
    else
      if (RightStr(Nominative, 2) = 'us') and (Genus = 'n') then
        NounDecl := '---'
      else
        NounDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
  end;
  Result := NounDecl;
end;

{ Latin nouns declension: 3rd declension }

function DeclensionNouns3(Nominative, Genitive, Genus, Casus, Number: string): string;

{ Declension in the same way the corresponding model does; model determaination, as described in the "Latin grammar" help text; }
{ nom/Voc for masc/fem and nom/voc/acc for neuter are always the same, for other cases, the model's suffix is added to the genitive stem }

const
  SuffixesCivis: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'e');
    Pl: ('es', 'es', 'es', 'ium', 'ibus', 'ibus');
  );
  SuffixesMare: TSuffix = (
    Sg: ('-', '-', '-', 'is', 'i', 'i');
    Pl: ('ia', 'ia', 'ia', 'ium', 'ibus', 'ibus');
  );
  SuffixesConsul: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'e');
    Pl: ('es', 'es', 'es', 'um', 'ibus', 'ibus');
  );
  SuffixesCorpus: TSuffix = (
    Sg: ('-', '-', '-', 'is', 'i', 'e');
    Pl: ('a', 'a', 'a', 'um', 'ibus', 'ibus');
  );
  SuffixesPater: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'e');
    Pl: ('es', 'es', 'es', 'um', 'ibus', 'ibus');
  );
  SuffixesUrbs: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'e');
    Pl: ('es', 'es', 'es', 'ium', 'ibus', 'ibus');
  );
  SuffixesAnimal: TSuffix = (
    Sg: ('-', '-', '-', 'is', 'i', 'i');
    Pl: ('ia', 'ia', 'ia', 'ium', 'ibus', 'ibus');
  );
  SuffixesFebris: TSuffix = (
    Sg: ('-', '-', 'im', 'is', 'i', 'i');
    Pl: ('es', 'es', 'es', 'ium', 'ibus', 'ibus');
  );

var
  SylNominative, SylGenitive, I: Integer;
  Stem, NounDecl: string;
  Suffixes: TSuffix;

begin
  NounDecl := '';
  if ((Casus = 'no') or (Casus = 'vo')) and (Number = 'sg') then
    NounDecl := Nominative
  else if (Genus = 'n') and (Casus = 'ac') and (Number = 'sg') then
    NounDecl := Nominative
  else begin
    if Nominative = 'vis' then begin
      if ((Casus = 'ge') or (Casus = 'da')) and (Number = 'sg') then
        NounDecl := '---'
      else begin
        if Number = 'sg' then
          Stem := 'v'
        else
          Stem := 'vir';
        Suffixes := SuffixesFebris;
        NounDecl := Stem + CasusSuffix(Casus, Number, Suffixes);
      end;
    end
    else begin
      Stem := LeftStr(Genitive, Length(Genitive) - 2);
      SylNominative := 0; SylGenitive := 0;
      // Determine number of nominative syllables
      for I := 1 to Length(Nominative) do begin
        if Nominative[I] in ['i', 'u', 'e', 'o', 'a'] then begin
          if (I > 1) and (((Nominative[I - 1] = 'a') and ((Nominative[I] = 'e') or (Nominative[I] = 'u'))) or ((Nominative[I - 1] = 'o') and (Nominative[I] = 'e'))) then
            // just continue...
          else
            Inc(SylNominative);
        end;
      end;
      // Determine number of genitive syllables
      for I := 1 to Length(Genitive) do begin
        if Genitive[I] in ['i', 'u', 'e', 'o', 'a'] then begin
          if (I > 1) and (((Genitive[I - 1] = 'a') and ((Genitive[I] = 'e') or (Genitive[I] = 'u'))) or ((Genitive[I - 1] = 'o') and (Genitive[I] = 'e'))) then
            // just continue...
          else
            Inc(SylGenitive);
        end;
      end;
      // Number of nominative syllables = number of genitive syllables
      if SylNominative = SylGenitive then begin
        if (Genus = 'm') or (Genus = 'f') then begin
          if (Nominative = 'febris') or (Nominative = 'puppis') or (Nominative = 'securis') or (Nominative = 'sitis') or (Nominative = 'turris') or (Nominative = 'tussis') then
            Suffixes := SuffixesFebris
          else if (Nominative = 'pater') or (Nominative = 'mater') or (Nominative = 'frater') or (Nominative = 'juvenis') or (Nominative = 'senex') or (Nominative = 'canis') then
            Suffixes := SuffixesPater
          else
            Suffixes := SuffixesCivis
        end
        else
          Suffixes := SuffixesMare;
      end
      // Number of nominative syllables <> number of genitive syllables
      else begin
        if (Genus = 'm') or (Genus = 'f') then begin
          if (Stem[Length(Stem)] in ['i', 'u', 'e', 'o', 'a']) or (Stem[Length(Stem) - 1] in ['i', 'u', 'e', 'o', 'a']) then
            Suffixes := SuffixesConsul
          else
            Suffixes := SuffixesUrbs;
        end
        else begin
          if (RightStr(Nominative, 2) = 'al') or (RightStr(Nominative, 2) = 'ar') then
            Suffixes := SuffixesAnimal
          else
            Suffixes := SuffixesCorpus;
        end;
      end;
      NounDecl := Stem + CasusSuffix(Casus, Number, Suffixes);
    end;
  end;
  Result := NounDecl;
end;

{ Latin nouns declension: 4th declension }

function DeclensionNouns4(Nominative, Genitive, Casus, Number: string): string;

{ Genitive stem + suffix, that is different for nouns in -us and the neuter nouns in -u; exception: special declension for "domus" }

const
  SuffixesUS: TSuffix = (
    Sg: ('us', 'us', 'um', 'us', 'ui', 'u');
    Pl: ('us', 'us', 'us', 'uum', 'ibus', 'ibus');
  );
  SuffixesU: TSuffix = (
    Sg: ('u', 'u', 'u', 'us', 'ui', 'u');
    Pl: ('ua', 'ua', 'ua', 'uum', 'ibus', 'ibus');
  );

var
  Stem, NounDecl: string;
  Suffixes: TSuffix;

begin
  NounDecl := '';
  Stem := LeftStr(Genitive, Length(Genitive) - 2);
  if RightStr(Nominative, 2) = 'us' then
    Suffixes := SuffixesUS
  else if RightStr(Nominative, 1) = 'u' then
    Suffixes := SuffixesU;
  if Number = 'sg' then begin
    if (Nominative = 'domus') and (Casus = 'ab') then
      NounDecl := 'domo'
    else
      NounDecl := Stem + CasusSuffix(Casus, 'sg', Suffixes);
  end
  else begin
    if Nominative = 'domus' then begin
      if Casus = 'ac' then
        NounDecl := 'domos'
      else if Casus = 'ge' then
        NounDecl := 'domorum'
      else
        NounDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
    end
    else if (Nominative = 'acus') or (Nominative = 'arcus') or (Nominative = 'lacus') or (Nominative = 'quercus') or (Nominative = 'specus') or (Nominative = 'artus') or (Nominative = 'tribus') then begin
      if (Casus = 'da') or (Casus = 'ab') then
        NounDecl := Stem + 'ubus'
      else
        NounDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
    end
    else
      NounDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
  end;
  Result := NounDecl;
end;

{ Latin nouns declension: 5th declension }

function DeclensionNouns5(Genitive, Casus, Number: string): string;

{ Genitive stem + suffix }

const
  Suffixes: TSuffix = (
    Sg: ('es', 'es', 'em', 'ei', 'ei', 'e');
    Pl: ('es', 'es', 'es', 'erum', 'ebus', 'ebus');
  );

var
  Stem: string;

begin
  Stem := LeftStr(Genitive, Length(Genitive) - 2);
  Result := Stem + CasusSuffix(Casus, Number, Suffixes);
end;

{ Latin adjectives declension: Determine which of the 2 classes must be used }

function DeclensionAdjs(Masculine, Feminine, Neuter, Genus, Casus, Number: string; Person: Boolean): string;

{ Adjectives with feminine in -a and neuter in -um are considered 1st class, all other 2nd class }

const
  SuffixesM: TSuffix = (
    Sg: ('us', 'e', 'um', 'i', 'o', 'o');
    Pl: ('i', 'i', 'os', 'orum', 'is', 'is');
  );
  SuffixesF: TSuffix = (
    Sg: ('a', 'a', 'am', 'ae', 'ae', 'a');
    Pl: ('ae', 'ae', 'as', 'arum', 'is', 'is');
  );
  SuffixesN: TSuffix = (
    Sg: ('um', 'um', 'um', 'i', 'o', 'o');
    Pl: ('a', 'a', 'a', 'orum', 'is', 'is');
  );

var
  Stem, AdjDecl: string;
  Suffixes: TSuffix;

begin
  AdjDecl := '';
  if RightStr(Masculine, 1) = 'i' then begin
    // Some adjectives, that exist only as plural (ex: viginti, vigintae, viginta)
    Stem := LeftStr(Feminine, Length(Feminine) - 2);
    if Genus = 'm' then
      Suffixes := SuffixesM
    else if Genus = 'f' then
      Suffixes := SuffixesF
    else
      Suffixes := SuffixesN;
    AdjDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
  end
  else if (RightStr(Feminine, 1) = 'a') and (RightStr(Neuter, 2) = 'um') then begin
    // Adjectives with femininum in -a and neuter in -um are first class
    AdjDecl := DeclensionAdjs1(Masculine, Feminine, Genus, Casus, Number);
  end
  else begin
    // All other adjectives are second class
    AdjDecl := DeclensionAdjs2(Masculine, Feminine, Neuter, Genus, Casus, Number, Person);
  end;
  Result := AdjDecl;
end;

{ Latin adjectives declension: First class }

function DeclensionAdjs1(Masculine, Feminine, Genus, Casus, Number: string): string;

{ Stem + suffix, that is different for masculine, feminine and neuter; special case for nom/voc sg. masc. of adjectives in -er }

const
  SuffixesM: TSuffix = (
    Sg: ('us', 'e', 'um', 'i', 'o', 'o');
    Pl: ('i', 'i', 'os', 'orum', 'is', 'is');
  );
  SuffixesF: TSuffix = (
    Sg: ('a', 'a', 'am', 'ae', 'ae', 'a');
    Pl: ('ae', 'ae', 'as', 'arum', 'is', 'is');
  );
  SuffixesN: TSuffix = (
    Sg: ('um', 'um', 'um', 'i', 'o', 'o');
    Pl: ('a', 'a', 'a', 'orum', 'is', 'is');
  );

var
  Stem, AdjDecl: string;
  Suffixes: TSuffix;

begin
  AdjDecl := '';
  Stem := LeftStr(Feminine, Length(Feminine) - 1);
  if Genus = 'm' then
    Suffixes := SuffixesM
  else if Genus = 'f' then
    Suffixes := SuffixesF
  else
    Suffixes := SuffixesN;
  if Number = 'sg' then begin
    if (RightStr(Masculine, 2) = 'er') and ((Casus = 'no') or (Casus = 'vo')) and (Genus = 'm') then
      AdjDecl := Masculine
    else
      AdjDecl := Stem + CasusSuffix(Casus, 'sg', Suffixes);
  end
  else
    AdjDecl := Stem + CasusSuffix(Casus, 'pl', Suffixes);
  DeclensionAdjs1 := AdjDecl;
end;

{ Latin adjectives declension: Second class }

function DeclensionAdjs2(Masculine, Feminine, Neuter, Genus, Casus, Number: string; Person: Boolean): string;

{ Difference is made between adjectives being identical at all 3 genus and those that aren't; in the last case the adjective is indicated }
{ as for the 1st class (m, f, n), in the first case a special notation is used: m, the tag "3id" and m genitive. }
{ Decination of all 2nd class adjectives is fundamentally the same, except for "vetus", "pauper" and "dives" that have special suffixs; }
{ In the "normal" case, nominative and vocative (and accusative for neuter) are identical; the other cases = genitive stem + suffixs. }
{ Special case: ablative of adjectives in -ens/-entis: suffix -e if they qualify a person, suffix -i for an object. }

const
  Suffixes1M: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'i');
    Pl: ('es', 'es', 'es', 'ium', 'ibus', 'ibus');
  );
  Suffixes1F: TSuffix = (
    Sg: ('is', 'is', 'em', 'is', 'i', 'i');
    Pl: ('es', 'es', 'es', 'ium', 'ibus', 'ibus');
  );
  Suffixes1N: TSuffix = (
    Sg: ('e', 'e', 'e', 'is', 'i', 'i');
    Pl: ('ia', 'ia', 'ia', 'ium', 'ibus', 'ibus');
  );
  Suffixes2M: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'e');
    Pl: ('es', 'es', 'es', 'um', 'ibus', 'ibus');
  );
  Suffixes2F: TSuffix = (
    Sg: ('-', '-', 'em', 'is', 'i', 'e');
    Pl: ('es', 'es', 'es', 'um', 'ibus', 'ibus');
  );
  Suffixes2N: TSuffix = (
    Sg: ('-', '-', '-', 'is', 'i', 'e');
    Pl: ('a', 'a', 'a', 'um', 'ibus', 'ibus');
  );

var
  Stem, AdjDecl: string;
  Suffixes: TSuffix;

begin
  AdjDecl := '';
  if ((Casus = 'no') or (Casus = 'vo')) and (Number = 'sg') then begin
    if Feminine = '3id' then
      AdjDecl := Masculine
    else begin
      if Genus = 'm' then
        AdjDecl := Masculine
      else if Genus = 'f' then
        AdjDecl := Feminine
      else
        AdjDecl := Neuter;
    end;
  end
  else if (Casus = 'ac') and (Number = 'sg') and (Genus = 'n') then begin
    if Feminine = '3id' then
      AdjDecl := Masculine
    else
      AdjDecl := Neuter;
  end
  else begin
    if Feminine = '3id' then
      Stem := LeftStr(Neuter, Length(Neuter) - 2)
    else
      Stem := LeftStr(Feminine, Length(Feminine) - 2);
    if (Masculine = 'vetus') or (Masculine = 'pauper') or (Masculine = 'dives') then begin
      if Genus = 'm' then
        Suffixes := Suffixes2M
      else if Genus = 'f' then
        Suffixes := Suffixes2F
      else
        Suffixes := Suffixes2N;
    end
    else begin
      if Genus = 'm' then
        Suffixes := Suffixes1M
      else if Genus = 'f' then
        Suffixes := Suffixes1F
      else
        Suffixes := Suffixes1N;
    end;
    if Person and (RightStr(Stem, 2) = 'nt') and (Casus = 'ab') and (Number = 'sg') and ((Genus = 'm') or (Genus = 'f')) then
      AdjDecl := Stem + 'e'
    else
      AdjDecl := Stem + CasusSuffix(Casus, Number, Suffixes);
  end;
  DeclensionAdjs2 := AdjDecl;
end;

{ Get suffix for given casus and number (out of record of arrays with suffixes applicable) }

function CasusSuffix(Casus, Number: string; Suffixes: TSuffix): string;

var
  Index, I: Integer;
  Suffix: string;

begin
  Index := 0;
  for I := 1 to 6 do begin
    if Casus = AllCasus[I] then
      Index := I;
  end;
  if Number = 'sg' then
    Suffix := Suffixes.Sg[Index]
  else
    Suffix := Suffixes.Pl[Index];
  CasusSuffix := Suffix;
end;

end.

