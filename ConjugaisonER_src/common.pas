{**********************************************************************************************************}
{* Unité commune (contenant le code pour la conjugaison proprement dite) pour l'application ConjugaisonER *}
{**********************************************************************************************************}

unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, LazUTF8;

const
  NVerbsMax = 300; NModes = 5; NTempsMax = 5;
  NVoix = 3; NFormes = 4;
  NNombres = 2; NPersonnes = 3; NGenres = 2;

type
  TVerbe = record
    VerbeNom: string;
    VoixPassive, VoixPronominale: Boolean;
    VerbeClass, Auxiliaire, Conjugaison: Char;
  end;
  TVerbes = array[1..NVerbsMax] of TVerbe;
  TTemps = array[1..NModes, 1..NTempsMax] of string;

const
  Modes: array[1..NModes] of string = (
    'infinitif', 'participe', 'impératif', 'indicatif', 'subjonctif'
  );
  NTemps: array[1..NModes] of Integer = (
    1, 2, 1, 5, 2
  );
  TempsSimples: TTemps = (
    ('présent', '', '', '', ''),
    ('présent', 'passé', '', '', ''),
    ('présent', '', '', '', ''),
    ('présent', 'imparfait', 'passé simple', 'futur simple', 'conditionnel présent'),
    ('présent', 'imparfait', '', '', '')
  );
  TempsComposes: TTemps =  (
    ('passé', '', '', '', ''),
    ('passé composé', '-', '', '', ''),
    ('passé', '', '', '', ''),
    ('passé composé', 'plus-que-parfait', 'passé antérieur', 'futur antérieur', 'conditionnel passé'),
    ('passé', 'plus-que-parfait', '', '', '')
  );
  Voix: array[1..NVoix] of string = (
    'actif', 'passif', 'pronominal'
  );
  Formes: array[1..NFormes] of string = (
    'affirmative', 'négative', 'intérogative', 'intérogative-négative'
  );
  Nombres: array[1..NNombres] of string = (
    'singulier', 'pluriel'
  );
  Personnes: array[1..NPersonnes] of string = (
    '1ère personne', '2e personne', '3e personne'
  );
  Genres: array[1..NGenres] of string = (
    'masculin', 'féminin'
  );
  PronomsPersonnels: array[1..NNombres, 1..NPersonnes] of string = (
    ('je', 'tu', 'il'),
    ('nous', 'vous', 'ils')
  );
  PronomsReflechis: array[1..NNombres, 1..NPersonnes] of string = (
    ('me', 'te', 'se'),
    ('nous', 'vous', 'se')
  );
  VerbeAvoir: TVerbe = (
    VerbeNom: 'avoir'; VoixPassive: False; VoixPronominale: False;
    VerbeClass: 'A'; Auxiliaire: 'A'; Conjugaison: 'I';
  );
  VerbeEtre: TVerbe = (
    VerbeNom: 'être'; VoixPassive: False; VoixPronominale: False;
    VerbeClass: 'A'; Auxiliaire: 'A'; Conjugaison: 'I';
  );

function Conjugaison(Verbe, VerbeType: TVerbe; Voix, Mode, Temps, Forme, Nombre, Personne, Genre: string;
  Reforme1990, KeepY: Boolean; var AvoirFile, EtreFile, VerbFile: TINIFile): string;
function ConjugaisonActif(Verbe, VerbeType: TVerbe; Mode, Temps, Nombre, Personne, Genre, Auxiliaire: string;
  Reforme1990, KeepY: Boolean; var AvoirFile, EtreFile, VerbFile: TINIFile): string;
function ConjugaisonExceptions(Verbe, VerbeConjugue: string; Reforme1990, KeepY: Boolean): string;
function ConjugaisonExceptionsCER(VerbeConjugue: string): string;
function ConjugaisonExceptionsGER(VerbeConjugue: string): string;
function ConjugaisonExceptionsELER(VerbeConjugue: string; Reforme1990: Boolean): string;
function ConjugaisonExceptionsETER(VerbeConjugue: string; Reforme1990: Boolean): string;
function ConjugaisonExceptionsYER(VerbeConjugue: string; KeepY: Boolean): string;
function ConjugaisonExceptionsE_ER(VerbeConjugue: string): string;
function AccordPartPasse(Auxiliaire, Nombre, Genre: string): string;
function GetAuxiliaire(Verbe: TVerbe): TVerbe;
function PronomPersonnel(Nombre, Personne, Genre: string): string;
function PronomReflechi(Nombre, Personne: string): string;
function PronomImperatif(Verb, Nombre, Personne: string): string;
function DoubleVoyelle(Mot, Lettre: string): string;

implementation

{ Main verbs in -er conjugation routine }

function Conjugaison(Verbe, VerbeType: TVerbe; Voix, Mode, Temps, Forme, Nombre, Personne, Genre: string;
  Reforme1990, KeepY: Boolean; var AvoirFile, EtreFile, VerbFile: TINIFile): string;

var
  XMode, I: Integer;
  Conj, Pronom, Lettre: string;
  TCompose, OK: Boolean;
  VerbeAuxiliaire: TVerbe;

begin
  OK := True;
  if Verbe.VerbeClass = 'I' then begin
    // Impersonnel verbs
    if Mode = 'impératif' then
      OK := False;
    if (Mode <> 'infinitif') and (Mode <> 'participe') then begin
      if (Nombre = 'pluriel') or (Personne <> '3e personne') then
        OK := False;
      if Genre = 'féminin' then
        OK := False;
    end;
  end;
  if OK then begin
    // For given mode, get index of tenses array
    if Mode = 'indicatif' then
      XMode := 4
    else if Mode = 'subjonctif' then
      XMode := 5
    else if Mode = 'infinitif' then
      XMode := 1
    else if Mode = 'participe' then
      XMode := 2
    else
      XMode := 3;
    // Check if tense specified is a simple or a compound tense
    TCompose := False;
    for I:= 1 to NTemps[XMode] do begin
      if Temps = TempsComposes[XMode, I] then begin
        Temps := TempsSimples[XMode, I];
        TCompose := True;
      end;
    end;
    // Get the correct auxiliary verb
    VerbeAuxiliaire := GetAuxiliaire(Verbe);
    // Conjugation for voice = 'actif'
    if Voix = 'actif' then begin
      // 'Actif' conjugation of compouned tenses
      if TCompose then begin
        // Conjugate the selected tense of the auxiliary verb
        Conj := ConjugaisonActif(VerbeAuxiliaire, VerbeType, Mode, Temps, Nombre, Personne, Genre, '-',
          Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
        // Some changements for 'y aller'
        if Verbe.VerbeNom = 'y aller' then
          if Mode = 'impératif' then
            Conj += '-y'
          else
            Conj := 'y ' + Conj;
        // Conjugate the 'participe passé' of the selected verb
        Conj += ' ' + ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, VerbeAuxiliaire.VerbeNom,
          Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
      end
      // 'Actif' conjugation of simple tenses
      else
        // Conjugate the selected tense of the selected verb
        Conj := ConjugaisonActif(Verbe, VerbeType, Mode, Temps, Nombre, Personne, Genre, VerbeAuxiliaire.VerbeNom,
          Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
    end
    // Conjugation for voice = 'passif'
    else if Voix = 'passif' then begin
      // 'Passif' conjugation of 'participe passé'
      if (Mode = 'participe') and (Temps = 'passé') then
        Conj := ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, 'être',
          Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile)
      // 'Passif' conjugation (general case = <> 'participe passé')
      else begin
        // 'Passif' conjugation of compound tenses
        if TCompose then begin
          if Mode = 'impératif' then                                             // l'impératif passé n'existe pas au passif
            Conj := 'n/a'
          else begin
            // Conjugate the selected tense of 'avoir' (acive voice)
            Conj := ConjugaisonActif(VerbeAvoir, VerbeAvoir, Mode, Temps, Nombre, Personne, Genre, '-',
              Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
            // Conjugate the 'participe passé' of 'être'
            Conj += ' ' + ConjugaisonActif(VerbeEtre, VerbeEtre, 'participe', 'passé', Nombre, Personne, Genre, 'avoir',
              Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
          end;
        end
        // 'Passif' conjugation of simple tenses
        else
          // Conjugate the selected tense of 'être' (acive voice)
          Conj := ConjugaisonActif(VerbeEtre, VerbeEtre, Mode, Temps, Nombre, Personne, Genre, '-',
            Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
        // For both simple and compouned tenses, conjugate the 'participe passé' of the selected verb
        if Conj <> 'n/a' then
          Conj += ' ' + ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, 'être',
          Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
      end;
    end
    // Conjugation for voice = 'pronominal'
    else begin
      // 'Pronominal' conjugation of 'participe passé'
      if (Mode = 'participe') and (Temps = 'passé') then
        Conj := 'n/a'
      // 'Pronominal' conjugation (general case = <> 'participe passé')
      else begin
        // 'Pronominal' conjugation of compound tenses
        if TCompose then begin
          if Mode = 'impératif' then                                             // l'impératif passé n'existe pas à la forme pronominale
            Conj := 'n/a'
          else begin
            // Conjugate the selected tense of 'être' (acive voice)
            Conj := ConjugaisonActif(VerbeEtre, VerbeEtre, Mode, Temps, Nombre, Personne, Genre, '-',
              Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
            // Conjugate the 'participe passé' of the selected verb
            Conj+= ' ' + ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, 'être',
              Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
            // Some changements for "s'en aller"
            if (Verbe.VerbeNom = 'en aller') and (Mode <> 'impératif') then
              Conj := 'en ' + Conj;
          end;
        end
        // 'Pronominal' conjugation of simple tenses
        else
          // Conjugate the selected tense of the selected (acive voice)
          Conj := ConjugaisonActif(Verbe, VerbeType, Mode, Temps, Nombre, Personne, Genre, '-',
            Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
        // Make changements necessary for mode = 'impéraif'
        if Conj <> 'n/a' then begin
          if Mode = 'impératif' then begin
            // Special case: s'en aller
            if Verbe.VerbeNom = 'en aller' then begin
              Conj := StringReplace(Conj, 'en ', '', []);
              Conj += '-' + PronomImperatif(Verbe.VerbeNom, Nombre, Personne) + '-en';
            end
            // Normal case (verbs other than s'en aller)
            else
              Conj += '-' + PronomImperatif(Verbe.VerbeNom, Nombre, Personne);
          end
          else begin
            Lettre := UTF8Copy(Conj, 1, 1);
            Conj := DoubleVoyelle(PronomReflechi(Nombre, Personne), Lettre) + Conj;
          end;
        end;
      end;
    end;
    if Conj <> 'n/a' then begin
      // For personal modes, add the 'pronom personnel'
      if (Mode = 'indicatif') or (Mode = 'subjonctif') then begin
        Lettre := UTF8Copy(Conj, 1, 1);
        Pronom := PronomPersonnel(Nombre, Personne, Genre);
        if Pronom = 'elle' then
          Pronom += ' '
        else
          Pronom := DoubleVoyelle(Pronom, Lettre);                             // change se to s' if conjugated verb starts with vowel
        Conj := Pronom + Conj;
        // Add 'que' for mode = 'subjonctif'
        if Mode = 'subjonctif' then begin
          Lettre := UTF8Copy(Conj, 1, 1);
          Conj := DoubleVoyelle('que', Lettre) + Conj;
        end;
      end;
    end;
  end
  else
    Conj := 'n/a';
  Conjugaison := Conj;
end;

{ Main verbs in -er "voice = actif" conjugation routine }

function ConjugaisonActif(Verbe, VerbeType: TVerbe; Mode, Temps, Nombre, Personne, Genre, Auxiliaire: string;
  Reforme1990, KeepY: Boolean; var AvoirFile, EtreFile, VerbFile: TINIFile): string;

const
  NRadicauxAller = 5;
  RadicauxAller: array[1..NRadicauxAller] of string = ('all', 'va', 'vont', 'ir', 'aill');

var
  XNombre, XPersonne, I: Integer;
  RacineVerbe, RacineType, VerbeConjugue, InifileSection, InifileKey: string;

begin
  Temps := StringReplace(Temps, ' ', '-', []);
  // Get indexes for 'personne' and 'nombre' arrays
  if (Mode = 'infinitif') or (Mode = 'participe') then begin
    XNombre := 0; XPersonne := 0;
  end
  else begin
    if Nombre = 'singulier' then
      XNombre := 1
    else
      XNombre := 2;
    XPersonne := StrToInt(LeftStr(Personne, 1));
  end;
  // Read conjugation for selected verb from appropriate conjugation file
  // Special files for avoir, être, aller, envoyer; for all others 'aimer.txt' will be used
  InifileSection := Mode + '_' + Temps;
  IniFileKey := 'P' + IntToStr(XNombre) + IntToStr(XPersonne);
  if Verbe.VerbeNom = 'avoir' then
    VerbeConjugue := AvoirFile.ReadString(InifileSection, InifileKey, '')
  else if Verbe.VerbeNom = 'être' then
    VerbeConjugue := EtreFile.ReadString(InifileSection, InifileKey, '')
  else
    VerbeConjugue := VerbFile.ReadString(InifileSection, InifileKey, '');
  // Create conjugated form of selected verb
  // The conjugated verb read from the file is for one of the verbs mentionned above;
  // To get the conjugated form of the actual verb, the racine or radical of the verb read
  // has to be changed to the one of the actal verb
  if RightStr(Verbe.VerbeNom, 2) = 'er' then begin
    if (Verbe.Conjugaison <> 'I') and (Verbe.VerbeNom <> VerbeType.VerbeNom) then begin
      // 'i' indicates an irregular verb, conjugated similarily to some other irregular verb,
      // not having its own conjugation file, thus the radical having to be adapted
      // (actually the case for y aller, s'en aller and renvoyer)
      if Verbe.Conjugaison = 'i' then begin
        // Correct conjugated form for: s'en aller
        if Verbe.VerbeNom = 'en aller' then begin
          if not ((Mode = 'participe') and ((Temps = 'passé') or (Temps = 'passé composé'))) then begin
            for I := 1 to NRadicauxAller do
              VerbeConjugue := StringReplace(VerbeConjugue, RadicauxAller[I], 'en ' + RadicauxAller[I], []);
          end;
        end
        // Correct conjugated form for: y aller
        else if Verbe.VerbeNom = 'y aller' then begin
          if Mode = 'impératif' then begin
            if Nombre = 'pluriel' then
              VerbeConjugue += '-y'
            else
              VerbeConjugue += 's-y';
          end
          else if not ((Mode = 'participe') and ((Temps = 'passé') or (Temps = 'passé composé'))) then begin
             for I := 1 to NRadicauxAller do
              VerbeConjugue := StringReplace(VerbeConjugue, RadicauxAller[I], 'y ' + RadicauxAller[I], []);
          end;
        end
        // Correct conjugated form for: renvoyer
        else if Verbe.VerbeNom = 'renvoyer' then
          VerbeConjugue := StringReplace(VerbeConjugue, 'env', 'renv', [])
      end
      // General case (non-irregular verbs)
      // The racine or radical of the conjugated verb as read from 'aimer.txt' has to be relaced with
      // the one of the verb actually selected
      else begin
        RacineType  := UTF8Copy(VerbeType.VerbeNom, 1, UTF8Length(VerbeType.VerbeNom) - 2);
        RacineVerbe := UTF8Copy(Verbe.VerbeNom, 1, UTF8Length(Verbe.VerbeNom) - 2);
        VerbeConjugue := StringReplace(VerbeConjugue, RacineType, RacineVerbe, []);
        // For verbs with special conjugation (or where unsure) perform the "exceptions" conjugation routine
        if (Verbe.Conjugaison = 'S') or (Verbe.Conjugaison = '?') then
          VerbeConjugue := ConjugaisonExceptions(Verbe.VerbeNom, VerbeConjugue, Reforme1990, KeepY);
      end;
    end;
  end;
  // Determine the correct 'participe passé', considering actual auxiliary, number and person
  if (Mode = 'participe') and ((Temps = 'passé') or (Temps = 'passé composé')) then
    VerbeConjugue += AccordPartPasse(Auxiliaire, Nombre, Genre);
  ConjugaisonActif := VerbeConjugue;
end;

{ Main verbs in -er "voice = actif" conjugation routine for "exceptions" verbs }

function ConjugaisonExceptions(Verbe, VerbeConjugue: string; Reforme1990, KeepY: Boolean): string;

// The routine checks the ending of the verb's infinitif and calls a special routine for each exception case
// Exception cases are: -cer, -ger, -eler, -eter, -ayer/-oyer/-uyer, -e*er/-é*er
// For verbs presenting 2 exceptions (as verbs in -eger/éger), both routines will be executed

var
  Term1, Term2: string;

begin
  if RightStr(Verbe, 3) = 'cer' then begin
    VerbeConjugue := ConjugaisonExceptionsCER(VerbeConjugue);
    if (RightStr(Verbe, 4) = 'ecer') or (RightStr(Verbe, 5) = 'écer') then
      VerbeConjugue := ConjugaisonExceptionsE_ER(VerbeConjugue);
  end
  else if RightStr(Verbe, 3) = 'ger' then begin
    VerbeConjugue := ConjugaisonExceptionsGER(VerbeConjugue);
    if (RightStr(Verbe, 4) = 'eger') or (RightStr(Verbe, 5) = 'éger') then
      VerbeConjugue := ConjugaisonExceptionsE_ER(VerbeConjugue);
  end
  else if RightStr(Verbe, 4) = 'eler' then begin
    VerbeConjugue := ConjugaisonExceptionsELER(VerbeConjugue, Reforme1990);
  end
  else if RightStr(Verbe, 4) = 'eter' then begin
    VerbeConjugue := ConjugaisonExceptionsETER(VerbeConjugue, Reforme1990);
  end
  else if (RightStr(Verbe, 4) = 'ayer') or (RightStr(Verbe, 4) = 'oyer') or (RightStr(Verbe, 4) = 'uyer') then begin
    VerbeConjugue := ConjugaisonExceptionsYER(VerbeConjugue, KeepY);
  end
  else begin
    Term1 := 'e' + RightStr(Verbe, 3); Term2 := 'é' + RightStr(Verbe, 3);
    if (RightStr(Verbe, 4) = Term1) or (UTF8Copy(Verbe, UTF8Length(Verbe) - 3, 4) = Term2) then begin
      VerbeConjugue := ConjugaisonExceptionsE_ER(VerbeConjugue);
    end;
  end;
  ConjugaisonExceptions := VerbeConjugue;
end;

{ Exceptions conjugation: verbs in -cer }

function ConjugaisonExceptionsCER(VerbeConjugue: string): string;

const
  ACirc: string = 'â';

var
  P1, P2: Integer;

begin
  P1 := Pos('--', VerbeConjugue) - 1; P2 := P1 + 3;
  if Copy(VerbeConjugue, P1, 1) = 'c' then begin
    if (Copy(VerbeConjugue, P2, 1) = 'a') or (Copy(VerbeConjugue, P2, 2) = ACirc) or (Copy(VerbeConjugue, P2, 1) = 'o') then
      VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'ç' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue));
  end;
  ConjugaisonExceptionsCER := VerbeConjugue;
end;

{ Exceptions conjugation: verbs in -ger }

function ConjugaisonExceptionsGER(VerbeConjugue: string): string;

const
  ACirc: string = 'â';

var
  P1, P2: Integer;

begin

  P1 := Pos('--', VerbeConjugue) - 1; P2 := P1 + 3;
  if Copy(VerbeConjugue, P1, 1) = 'g' then begin
    if (Copy(VerbeConjugue, P2, 1) = 'a') or (Copy(VerbeConjugue, P2, 2) = ACirc) or (Copy(VerbeConjugue, P2, 1) = 'o') then
      VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'ge' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue));
  end;
  ConjugaisonExceptionsGER := VerbeConjugue;
end;

{ Exceptions conjugation: verbs in -eler }

function ConjugaisonExceptionsELER(VerbeConjugue: string; Reforme1990: Boolean): string;

var
  P1, P2: Integer;

begin
  P1 := Pos('--', VerbeConjugue) - 2; P2 := P1 + 4;
  if Copy(VerbeConjugue, P1, 2) = 'el' then begin
    if (Copy(VerbeConjugue, P2, 1) = 'e') and (Copy(VerbeConjugue, P2 + 1, 1) <> 'z') and (RightStr(VerbeConjugue, 1) <> 'r') then begin
      // Conjugation according to the rules of "réforme 1990"
      if Reforme1990 then begin
        if (LeftStr(VerbeConjugue, 5) = 'appel') or (LeftStr(VerbeConjugue, 6) = 'rappel') then
          VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'ell' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue))
        else
          VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'èl' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue));
      end
      // Conjugation according to the rules before "réforme 1990"
      else begin
        // Not implemented in this version
      end;
    end;
  end;
  ConjugaisonExceptionsELER := VerbeConjugue;
end;

{ Exceptions conjugation: verbs in -eter }

function ConjugaisonExceptionsETER(VerbeConjugue: string; Reforme1990: Boolean): string;

var
  P1, P2: Integer;

begin
  P1 := Pos('--', VerbeConjugue) - 2; P2 := P1 + 4;
  if Copy(VerbeConjugue, P1, 2) = 'et' then begin
    if (Copy(VerbeConjugue, P2, 1) = 'e') and (Copy(VerbeConjugue, P2 + 1, 1) <> 'z') and (RightStr(VerbeConjugue, 1) <> 'r') then begin
      // Conjugation according to the rules of "réforme 1990"
      if Reforme1990 then begin
        if (LeftStr(VerbeConjugue, 3) = 'jet') or (LeftStr(VerbeConjugue, 5) = 'rejet')  or
          (LeftStr(VerbeConjugue, 6) = 'projet') or (LeftStr(VerbeConjugue, 6) = 'déjet') then
          VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'ett' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue))
        else
          VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'èt' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue));
      end
      // Conjugation according to the rules before "réforme 1990"
      else begin
        // Not implemented in this version
      end;
    end;
  end;
  ConjugaisonExceptionsETER := VerbeConjugue;
end;

{ Exceptions conjugation: verbs in -ayer/-uyer/oyer }

function ConjugaisonExceptionsYER(VerbeConjugue: string; KeepY: Boolean): string;

var
  P1, P2: Integer;

begin
  P1 := Pos('--', VerbeConjugue) - 2; P2 := P1 + 4;
  if (Copy(VerbeConjugue, P1, 2) = 'ay') or (Copy(VerbeConjugue, P1, 2) = 'oy') or (Copy(VerbeConjugue, P1, 2) = 'uy') then begin
    if (Copy(VerbeConjugue, P2, 1) = 'e') and (Copy(VerbeConjugue, P2 + 1, 1) <> 'z') and (RightStr(VerbeConjugue, 1) <> 'r') then begin
      if Copy(VerbeConjugue, P1, 2) = 'ay' then begin
        // For verbs in -ayer, make y -> i changements only if this option is selected
        if not KeepY then
          VerbeConjugue := Copy(VerbeConjugue, 1, P1) + 'i' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue));
      end
      else
        VerbeConjugue := Copy(VerbeConjugue, 1, P1) + 'i' + Copy(VerbeConjugue, P2 - 2, Length(VerbeConjugue));
    end;
  end;
  ConjugaisonExceptionsYER := VerbeConjugue;
end;

{ Exception conjugation: verbs in -e*er/é*er }

function ConjugaisonExceptionsE_ER(VerbeConjugue: string): string;

const
  EAigu: string = 'é';

var
  P1, P2: Integer;

begin
  P1 := Pos('--', VerbeConjugue) - 2; P2 := P1 + 4;
  if (Copy(VerbeConjugue, P1, 1) = 'e') or (Copy(VerbeConjugue, P1 - 1, 2) = EAigu) then begin
    if (Copy(VerbeConjugue, P2, 1) = 'e') and (Copy(VerbeConjugue, P2 + 1, 1) <> 'z') and (RightStr(VerbeConjugue, 1) <> 'r') then begin
      if Copy(VerbeConjugue, P1, 1) = 'e' then
        VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 1) + 'è' + Copy(VerbeConjugue, P2 - 3, Length(VerbeConjugue))
      else
        VerbeConjugue := Copy(VerbeConjugue, 1, P1 - 2) + 'è' + Copy(VerbeConjugue, P2 - 3, Length(VerbeConjugue));
    end;
  end;
  ConjugaisonExceptionsE_ER := VerbeConjugue;
end;

{ Determine 'participe passé' ending, considering actual auxiliary, number and person }

function AccordPartPasse(Auxiliaire, Nombre, Genre: string): string;

var
  Accord: string;

begin
  Accord := '';
  if (Auxiliaire = 'être') and (Genre = 'féminin') then
    Accord := 'e';
  if (Auxiliaire = 'être') and (Nombre = 'pluriel') then
    Accord += 's';
  AccordPartPasse := Accord;
end;

{ Get 'pronom personnel' for given number, person and genre }

function PronomPersonnel(Nombre, Personne, Genre: string): string;

var
  XNombre, XPersonne: Integer;
  Pronom: string;

begin
  if Nombre = 'singulier' then
    XNombre := 1
  else
    XNombre := 2;
  XPersonne := StrToInt(LeftStr(Personne, 1));
  Pronom := PronomsPersonnels[XNombre, XPersonne];
  if Genre = 'féminin' then
    Pronom := StringReplace(Pronom, 'il', 'elle', []);
  PronomPersonnel := Pronom;
end;

{ Get 'pronom personnel' for given number and person }

function PronomReflechi(Nombre, Personne: string): string;

var
  XNombre, XPersonne: Integer;
  Pronom: string;

begin
  if (Nombre = '-') and (Personne = '-') then
    Pronom := 'se'
  else begin
    if Nombre = 'singulier' then
      XNombre := 1
    else
      XNombre := 2;
    XPersonne := StrToInt(LeftStr(Personne, 1));
    Pronom := PronomsReflechis[XNombre, XPersonne];
  end;
  PronomReflechi := Pronom;
end;

{ Get 'pronom personnel' for given number and person }

function PronomImperatif(Verb, Nombre, Personne: string): string;

var
  Pronom: string;

// The function also needs the verb name as argument, in order to be able to deal with special cases as: s'en aller

begin
  if Nombre = 'singulier' then begin
    if Verb = 'en aller' then
      Pronom := 't'''
    else
      Pronom := 'toi';
  end
  else begin
    if LeftStr(Personne, 1) = '1' then
      Pronom := 'nous'
    else
      Pronom := 'vous';
  end;
  PronomImperatif := Pronom;
end;

{ 'e' (as in 'se' or 'que') followed by a vowel: replace by apostrophe }

function DoubleVoyelle(Mot, Lettre: string): string;

const
  NLettres = 9;
  Lettres: array[1..NLettres] of string = ('i', 'u', 'e', 'é', 'ê', 'o', 'a', 'y', 'h');

var
  I: Integer;
  Apostrophe: Boolean;

begin
  Apostrophe := False;
  if UTF8Copy(Mot, UTF8Length(Mot), 1) = 'e' then begin
    for I := 1 to NLettres do begin
      if Lettre = Lettres[I] then
        Apostrophe := True;
    end;
  end;
  if Apostrophe then
    Mot[UTF8Length(Mot)] := ''''
  else
    Mot += ' ';
  DoubleVoyelle := Mot;
end;

{ Get the auxiliary of a given verb }

function GetAuxiliaire(Verbe: TVerbe): TVerbe;

var
  Aux: TVerbe;

begin
  Aux := VerbeAvoir;
  if Verbe.Auxiliaire <> 'A' then                                              // 'A' = avoir, 'E' = 'être', 'X' = 'both'
    Aux := VerbeEtre;
  GetAuxiliaire := Aux;
end;

end.

