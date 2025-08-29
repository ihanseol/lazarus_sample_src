{**********************************************************************************************************}
{* Unité commune (contenant le code pour la conjugaison proprement dite) pour l'application ConjugaisonRE *}
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
  var AvoirFile, EtreFile, VerbFile: TINIFile): string;
function ConjugaisonActif(Verbe, VerbeType: TVerbe; Mode, Temps, Nombre, Personne, Genre, Auxiliaire: string;
  var AvoirFile, EtreFile, VerbFile: TINIFile): string;
function PartPasseAccord(PartPasse, Auxiliaire, Nombre, Genre: string): string;
function GetAuxiliaire(Verbe: TVerbe): TVerbe;
function PronomPersonnel(Nombre, Personne, Genre: string): string;
function PronomReflechi(Nombre, Personne: string): string;
function PronomImperatif(Nombre, Personne: string): string;
function DoubleVoyelle(Mot, Lettre: string): string;

implementation

{ Main verbs in -re conjugation routine }

function Conjugaison(Verbe, VerbeType: TVerbe; Voix, Mode, Temps, Forme, Nombre, Personne, Genre: string;
  var AvoirFile, EtreFile, VerbFile: TINIFile): string;

var
  XMode, I: Integer;
  Conj, Pronom, Lettre, Last: string;
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
        Conj := ConjugaisonActif(VerbeAuxiliaire, VerbeType, Mode, Temps, Nombre, Personne, Genre, '-', AvoirFile, EtreFile, VerbFile);
        // Conjugate the 'participe passé' of the selected verb
        Conj += ' ' + ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, VerbeAuxiliaire.VerbeNom, AvoirFile, EtreFile, VerbFile);
      end
      // 'Actif' conjugation of simple tenses
      else
        // Conjugate the selected tense of the selected verb
        Conj := ConjugaisonActif(Verbe, VerbeType, Mode, Temps, Nombre, Personne, Genre, VerbeAuxiliaire.VerbeNom, AvoirFile, EtreFile, VerbFile);
    end
    // Conjugation for voice = 'passif'
    else if Voix = 'passif' then begin
      // 'Passif' conjugation of 'participe passé'
      if (Mode = 'participe') and (Temps = 'passé') then
        Conj := ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, 'être', AvoirFile, EtreFile, VerbFile)
      // 'Passif' conjugation (general case = <> 'participe passé')
      else begin
        // 'Passif' conjugation of compound tenses
        if TCompose then begin
          if Mode = 'impératif' then                                                     // l'impératif passé n'existe pas au passif
            Conj := 'n/a'
          else begin
            // Conjugate the selected tense of 'avoir' (acive voice)
            Conj := ConjugaisonActif(VerbeAvoir, VerbeAvoir, Mode, Temps, Nombre, Personne, Genre, '-', AvoirFile, EtreFile, VerbFile);
            // Conjugate the 'participe passé' of 'être'
            Conj += ' ' + ConjugaisonActif(VerbeEtre, VerbeEtre, 'participe', 'passé', Nombre, Personne, Genre, 'avoir',
              AvoirFile, EtreFile, VerbFile);
          end;
        end
        // 'Passif' conjugation of simple tenses
        else
          // Conjugate the selected tense of 'être' (acive voice)
          Conj := ConjugaisonActif(VerbeEtre, VerbeEtre, Mode, Temps, Nombre, Personne, Genre, '-', AvoirFile, EtreFile, VerbFile);
        // For both simple and compouned tenses, conjugate the 'participe passé' of the selected verb
        if Conj <> 'n/a' then
          Conj += ' ' + ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, 'être', AvoirFile, EtreFile, VerbFile);
      end;
    end
    // Conjugation for voice = 'pronominal'
    else begin
      // 'Pronominal' conjugation of 'participe passé'
      if (Mode = 'participe') and (Temps = 'passé') then
        Conj := 'n/a'
      // 'Pronominal' conjugation (general case: not 'participe passé')
      else begin
        // 'Pronominal' conjugation of compound tenses
        if TCompose then begin
          if Mode = 'impératif' then                                                     // 'impératif passé' does not exist in pronominal voice
            Conj := 'n/a'
          else begin
            // Conjugate the selected tense of 'être' (acive voice)
            Conj := ConjugaisonActif(VerbeEtre, VerbeEtre, Mode, Temps, Nombre, Personne, Genre, '-', AvoirFile, EtreFile, VerbFile);
            // Conjugate the 'participe passé' of the selected verb
            Conj+= ' ' + ConjugaisonActif(Verbe, VerbeType, 'participe', 'passé', Nombre, Personne, Genre, 'être', AvoirFile, EtreFile, VerbFile);
          end;
        end
        // 'Pronominal' conjugation of simple tenses
        else
          // Conjugate the selected tense of the selected (acive voice)
          Conj := ConjugaisonActif(Verbe, VerbeType, Mode, Temps, Nombre, Personne, Genre, '-', AvoirFile, EtreFile, VerbFile);
        // Add reflexive pronoun
        if Conj <> 'n/a' then begin
          // Special case 'impératif'
          if Mode = 'impératif' then begin
            Last := RightStr(Conj, 1);
            if Last[1] in ['!', '*', '$', '^', '+'] then
              Conj := UTF8Copy(Conj, 1, UTF8Length(Conj) - 1)
            else
              Last := '';
            if Verbe.VerbeNom = 'en foutre' then begin
              // Special case for "s'en foutre"
              if (Nombre = 'singulier') then
                Conj := 'fous-t''en'
              else if Personne = '1ère personne' then
                Conj := 'foutons-nous-en'
              else
                Conj := 'foutez-vous-en';
            end
            else
              // Normal case
              Conj += '-' + PronomImperatif(Nombre, Personne) + Last;
          end
          // Normal case
          else begin

            if Verbe.VerbeNom = 'en foutre' then begin
              // Special case for "s'en foutre"
              Conj := StringReplace(Conj, 'en ', '', []);
              Conj := DoubleVoyelle(PronomReflechi(Nombre, Personne), 'e') + 'en ' + Conj;
            end
            else begin
              // Normal case
              Lettre := UTF8Copy(Conj, 1, 1);
              Conj := DoubleVoyelle(PronomReflechi(Nombre, Personne), Lettre) + Conj;
            end;
          end;
        end;
      end;
    end;
    if Conj <> 'n/a' then begin
      // Add personal pronoun (for personal modes)
      if (Mode = 'indicatif') or (Mode = 'subjonctif') then begin
        Lettre := UTF8Copy(Conj, 1, 1);
        Pronom := PronomPersonnel(Nombre, Personne, Genre);
        if Pronom = 'elle' then
          Pronom += ' '
        else
          Pronom := DoubleVoyelle(Pronom, Lettre);                                       // change je to j' if conjugated verb starts with vowel
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

{ Main verbs in -re "voice = actif" conjugation routine }

function ConjugaisonActif(Verbe, VerbeType: TVerbe; Mode, Temps, Nombre, Personne, Genre, Auxiliaire: string;
  var AvoirFile, EtreFile, VerbFile: TINIFile): string;

var
  XNombre, XPersonne: Integer;
  RacineVerbe, RacineType, RadicalVerbe, RadicalType, VerbeConjugue, Last, InifileSection, InifileKey, InifileTemps: string;

begin
  IniFileTemps := StringReplace(Temps, ' ', '-', []);
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
  // Files for regular verbs, one for each of the 3 semi-irregular cases, one for each irregular verb
  InifileSection := Mode + '_' + InifileTemps;
  IniFileKey := 'P' + IntToStr(XNombre) + IntToStr(XPersonne);
  if Verbe.VerbeNom = 'avoir' then
    VerbeConjugue := AvoirFile.ReadString(InifileSection, InifileKey, '')
  else if Verbe.VerbeNom = 'être' then
    VerbeConjugue := EtreFile.ReadString(InifileSection, InifileKey, '')
  else
    VerbeConjugue := VerbFile.ReadString(InifileSection, InifileKey, '');
  if RightStr(Verbe.VerbeNom, 2) = 're' then begin
    // Create conjugated form of selected verb
    // The conjugated verb read from the file is for one of the verbs mentionned above;
    // To get the conjugated form of the actual verb, the racine or radical of the verb read
    // has to be changed to the one of the actal verb
    RacineType  := UTF8Copy(VerbeType.VerbeNom, 1, UTF8Length(VerbeType.VerbeNom) - 2);
    RacineVerbe := UTF8Copy(Verbe.VerbeNom, 1, UTF8Length(Verbe.VerbeNom) - 2);
    RadicalType := RacineType; RadicalVerbe := RacineVerbe;
    if Verbe.Conjugaison = '-' then begin
      // ****************************
      // General case (regular verbs)
      // ****************************
      // The racine of the conjugated verb as read from the 'type verbe' conjugation file has to
      // be replaced with the one of the verb actually selected
      // Corrections have to be done for verbs in -dre and verbs in -ttre
      VerbeConjugue := StringReplace(VerbeConjugue, RacineType, RacineVerbe, []);
      // Correct 'ind. prés. 3.pers sg for verbs in -dre'
      if RightStr(Verbe.VerbeNom, 3) = 'dre' then begin
        if RightStr(VerbeConjugue, 4) = 'd--t' then
          VerbeConjugue := UTF8Copy(VerbeConjugue, 1, UTF8Length(VerbeConjugue) - 3);
      end
      // Correct 'ind. prés. sg and impératif prés. sg. for verbs in -ttre'
      else if RightStr(Verbe.VerbeNom, 4) = 'ttre' then begin
        if RightStr(VerbeConjugue, 5) = 'tt--s' then
          VerbeConjugue := UTF8Copy(VerbeConjugue, 1, UTF8Length(VerbeConjugue) - 5) + 't--s'
        else if RightStr(VerbeConjugue, 5) = 'tt--t' then
          VerbeConjugue := UTF8Copy(VerbeConjugue, 1, UTF8Length(VerbeConjugue) - 4);
      end;
    end
    else if Verbe.Conjugaison = 's' then begin
      // ********************
      // Semi-irregular verbs
      // ********************
      // 's' indicates a semi-irregular verb, conjugated similarily to some other semi-irregular verb,
      // not having its own conjugation file, thus the radical having to be adapted
      // Different processing for verbs in -aindre/-eindre/-oindre, -aître and -uire
      if (RightStr(Verbe.VerbeNom, 6) = 'aindre') or (RightStr(Verbe.VerbeNom, 6) = 'eindre') or (RightStr(Verbe.VerbeNom, 6) = 'oindre') then begin
        // Verbs ending in -aindre, -eindre or -oindre
        // -------------------------------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and ((Temps = 'imparfait') or (Temps = 'passé simple'))) or
          (Mode = 'subjonctif') then begin
          RadicalType := StringReplace(RadicalType, 'aind', 'aign', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'aind', 'aign', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'eind', 'eign', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'oind', 'oign', []);
        end
        else if ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'aind', 'ain', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'aind', 'ain', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'eind', 'ein', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'oind', 'oin', []);
        end;
        VerbeConjugue := StringReplace(VerbeConjugue, RadicalType, RadicalVerbe, []);
      end
      else if (UTF8Copy(VerbeType.VerbeNom, UTF8Length(VerbeType.VerbeNom) - 4, 5) = 'aître') and
        (UTF8Copy(VerbeType.VerbeNom, UTF8Length(VerbeType.VerbeNom) - 5, 6) <> 'naître')then begin
        // Verbs ending in -aître
        // -----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) then begin
          RadicalType := StringReplace(RadicalType, 'aît', 'aiss', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'aît', 'aiss', []);
        end
        else if ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier') and (Personne <> '3e personne')) then begin
          RadicalType := StringReplace(RadicalType, 'aît', 'ai', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'aît', 'ai', []);
        end
        else if ((Mode = 'participe')  and (Temps = 'passé')) or
          ((Mode = 'indicatif') and (Temps = 'passé simple')) or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'aît', '', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'aît', '', []);
        end;
        VerbeConjugue := StringReplace(VerbeConjugue, RadicalType, RadicalVerbe, []);
      end
      else if RightStr(Verbe.VerbeNom, 4) = 'uire' then begin
        // Verbs ending in -uire
        // ---------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and ((Temps = 'imparfait') or (Temps = 'passé simple'))) or
          (Mode = 'subjonctif') then begin
          RadicalType := LeftStr(RadicalType, Length(RadicalType) - 2) + 'uis';
          RadicalVerbe := LeftStr(RadicalVerbe, Length(RadicalVerbe) - 2) + 'uis';
        end;
        VerbeConjugue := StringReplace(VerbeConjugue, RadicalType, RadicalVerbe, []);
      end;
    end
    else if Verbe.Conjugaison = 'i' then begin
      // ***************
      // Irregular verbs
      // ***************
      // 'i' indicates an irregular verb, conjugated similarily to some other irregular verb,
      // not having its own conjugation file, thus the radical having to be adapted
      if RightStr(Verbe.VerbeNom, 5) = 'clure' then begin
        // Verbs ending in -clure
        // ----------------------
        if (Temps = 'passé simple') or ((Mode = 'subjonctif') and (Temps = 'imparfait')) or ((Mode = 'participe') and (Temps = 'passé')) then begin
          if (Mode = 'participe') and (Temps = 'passé') then begin
            if (RightStr(Verbe.VerbeNom, 7) <> 'exclure') then
              VerbeConjugue := StringReplace(VerbeConjugue, '--u*', '--us!', []);
          end;
          RadicalType := LeftStr(RadicalType, Length(RadicalType) - 1);
          RadicalVerbe := LeftStr(RadicalVerbe, Length(RadicalVerbe) - 1);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'mettre' then begin
        // Verbs ending in -mettre
        // -----------------------
        if (Temps = 'passé simple') or ((Mode = 'subjonctif') and (Temps = 'imparfait')) or ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'mett', 'm', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'mett', 'm', []);
        end
        else if ((Mode = 'indicatif') or (Mode = 'impératif')) and ((Temps = 'présent') and (Nombre = 'singulier')) then begin
          RadicalType := StringReplace(RadicalType, 'mett', 'met', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'mett', 'met', []);
        end
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'clore' then begin
        // Verbs ending in -clore
        // ----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) then begin
          RadicalType := StringReplace(RadicalType, 'clo', 'clos', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'clo', 'clos', []);
        end;
        VerbeConjugue := StringReplace(VerbeConjugue, 'clô', 'clo', []);
        VerbeConjugue := StringReplace(VerbeConjugue, '^', '', []);
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'coudre' then begin
        // Verbs ending in -coudre
        // -----------------------
        if (Mode = 'participe') or (Mode = 'subjonctif') or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and ((Temps = 'imparfait') or (Temps = 'passé simple'))) then begin
          RadicalType := StringReplace(RadicalType, 'coud', 'cous', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'coud', 'cous', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'moudre' then begin
        // Verbs ending in -moudre
        // -----------------------
        if (Mode = 'participe') or (Mode = 'subjonctif') or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and ((Temps = 'imparfait') or (Temps = 'passé simple'))) then begin
          RadicalType := StringReplace(RadicalType, 'moud', 'moul', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'moud', 'moul', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'oudre' then begin
        // Other verbs ending in -oudre
        // ----------------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) then begin
          RadicalType := StringReplace(RadicalType, 'oud', 'olv', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'oud', 'olv', []);
        end
        else if ((Mode = 'indicatif') and (Temps = 'passé simple')) or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          if UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 7, 8) = 'résoudre' then begin
            RadicalType := StringReplace(RadicalType, 'oud', 'ol', []);
            RadicalVerbe := StringReplace(RadicalVerbe, 'oud', 'ol', []);
          end
          else
            // Dissoudre, redissoudre and absoudre are defective...
            VerbeConjugue := '#';
        end
        else if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then begin
          RadicalType := StringReplace(RadicalType, 'oud', 'ou', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'oud', 'ou', []);
        end
        else if (Mode = 'participe') and (Temps = 'passé') then begin
          if UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 7, 8) = 'résoudre' then begin
            RadicalType := StringReplace(RadicalType, 'oud', 'ol', []);
            RadicalVerbe := StringReplace(RadicalVerbe, 'oud', 'ol', []);
          end
          else begin
            RadicalType := StringReplace(RadicalType, 'oud', 'ou', []);
            RadicalVerbe := StringReplace(RadicalVerbe, 'oud', 'ou', []);
            // Another form of "part. passé" is in -s...
            VerbeConjugue := StringReplace(VerbeConjugue, 'ol--u^', 'ou--t!', []);
          end;
        end;
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'crire' then begin
        // Verbs ending in -crire
        // ----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or (Mode = 'subjonctif') or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and ((Temps = 'imparfait') or (Temps = 'passé simple'))) then begin
          RadicalType := StringReplace(RadicalType, 'cri', 'criv', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'cri', 'criv', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'frire' then begin
        // Verbs ending in -frire
        // ----------------------
        // On some websites, these tenses exist; I choose to follow those, who consider these verbforms as defective...
        {if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'fri', 'fris', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fri', 'fris', []);
        end
        else if (Mode = 'indicatif') and (Temps = 'passé simple') then begin
          RadicalType := StringReplace(RadicalType, 'fri', 'fr', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fri', 'fr', []);
        end}
      end
      else if RightStr(Verbe.VerbeNom, 4) = 'rire' then begin
        // Other verbs ending in -rire
        // ---------------------------
        if (Temps = 'passé simple') or ((Mode = 'subjonctif') and (Temps = 'imparfait')) or ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'ri', 'r', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'ri', 'r', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 4) = 'dire' then begin
        // Verbs ending in -dire
        // ---------------------
        VerbeConjugue := StringReplace(VerbeConjugue, 'dites!', 'dis--ez*', []);
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'di', 'dis', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'di', 'dis', []);
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'di', 'd', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'di', 'd', []);
        end
      end
      else if RightStr(Verbe.VerbeNom, 4) = 'lire' then begin
        // Verbs ending in -lire
        // ---------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'li', 'lis', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'li', 'lis', []);
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) or
          ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'li', 'l', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'li', 'l', []);
        end
      end
      else if (RightStr(Verbe.VerbeNom, 7) = 'suffire') or (RightStr(Verbe.VerbeNom, 7) = 'confire') or (RightStr(Verbe.VerbeNom, 10) = 'circoncire') then begin
        // Some other verbs ending in -ire
        // -------------------------------
        if (Mode = 'participe') and (Temps = 'passé') then begin
          RadicalType := LeftStr(RadicalType, Length(RadicalType) - 1);
          RadicalVerbe := LeftStr(RadicalVerbe, Length(RadicalVerbe) - 1);
          if RightStr(Verbe.VerbeNom, 7) = 'confire' then
            VerbeConjugue := StringReplace(VerbeConjugue, '--i$', '--it!', [])
          else if RightStr(Verbe.VerbeNom, 10) = 'circoncire' then
            VerbeConjugue := StringReplace(VerbeConjugue, '--i$', '--is!', []);
        end;
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType += 's';
          RadicalVerbe += 's';
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := LeftStr(RadicalType, Length(RadicalType) - 1);
          RadicalVerbe := LeftStr(RadicalVerbe, Length(RadicalVerbe) - 1);
        end
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'suivre' then begin
        // Verbs ending in -suivre
        // -----------------------
        if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then begin
          RadicalType := StringReplace(RadicalType, 'suiv', 'sui', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'suiv', 'sui', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'vivre' then begin
        // Verbs ending in -vivre
        // ----------------------
        if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then begin
          RadicalType := StringReplace(RadicalType, 'viv', 'vi', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'viv', 'vi', []);
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) or
          ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'viv', 'véc', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'viv', 'véc', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'traire' then begin
        // Verbs ending in -traire
        // -----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          (((Mode = 'subjonctif') or (Mode = 'indicatif')) and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne <> '3e personne')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'trai', 'tray', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'trai', 'tray', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'faire' then begin
        // Verbs ending in -faire
        // ----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne = '1ère personne')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne = '1ère personne')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'fai', 'fais', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fai', 'fais', []);
        end
        else if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne = '2e personne')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne = '2e personne')) then begin
          RadicalType := StringReplace(RadicalType, 'fai', 'faites', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fai', 'faites', []);
        end
        else if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne = '3e personne')) then begin
          RadicalType := StringReplace(RadicalType, 'fai', 'font', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fai', 'font', []);
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'fai', 'f', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fai', 'f', []);
        end
        else if (Temps = 'futur simple') or (Temps = 'conditionnel présent') then begin
          RadicalType := StringReplace(RadicalType, 'fai', 'fe', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fai', 'fe', []);
        end
        else if (Mode = 'subjonctif') and (Temps = 'présent') then begin
          RadicalType := StringReplace(RadicalType, 'fai', 'fass', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fai', 'fass', []);
        end
      end
      else if RightStr(Verbe.VerbeNom, 4) = 'aire' then begin
        // Other verbs ending in -aire
        // ---------------------------
        if (Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier') and (Personne = '3e personne') then begin
          if Verbe.VerbeNom = 'taire' then
            VerbeConjugue := StringReplace(VerbeConjugue, 'aî--t^', 'ai--t', [])
          else begin
            RadicalType := StringReplace(RadicalType, 'ai', 'aî', []);
            RadicalVerbe := StringReplace(RadicalVerbe, 'ai', 'aî', []);
          end;
        end;
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'ai', 'ais', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'ai', 'ais', []);
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) or
          ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'ai', '', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'ai', '', []);
        end
      end
      else if RightStr(Verbe.VerbeNom, 7) = 'prendre' then begin
        // Verbs ending in -prendre
        // ------------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne <> '3e personne')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'prend', 'pren', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'prend', 'pren', []);
        end
        else if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel') and (Personne = '3e personne')) then begin
          RadicalType := StringReplace(RadicalType, 'prend', 'prenn', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'prend', 'prenn', []);
        end
        else if (Temps = 'passé simple') or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) or
          ((Mode = 'participe') and (Temps = 'passé')) then begin
          RadicalType := StringReplace(RadicalType, 'prend', 'pr', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'prend', 'pr', []);
        end
        else if (Mode = 'subjonctif') and (Temps = 'présent') and ((Nombre = 'singulier') or ((Nombre = 'pluriel') and (Personne = '3e personne'))) then begin
          RadicalType := StringReplace(RadicalType, 'prend', 'prenn', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'prend', 'prenn', []);
        end
        else if (Mode = 'subjonctif') and (Temps = 'présent') and ((Nombre = 'pluriel') and (Personne <> '3e personne')) then begin
          RadicalType := StringReplace(RadicalType, 'prend', 'pren', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'prend', 'pren', []);
        end
      end
      else if RightStr(Verbe.VerbeNom, 7) = 'vaincre' then begin
        // Verbs ending in -vaincre
        // ------------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          (Mode = 'subjonctif') or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and ((Temps = 'imparfait') or (Temps = 'passé simple'))) then begin
          RadicalType := StringReplace(RadicalType, 'ainc', 'ainqu', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'ainc', 'ainqu', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 5) = 'boire' then begin
        // Verbs ending in -boire
        // ----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) or
          ((Mode = 'subjonctif') and (Temps = 'présent') and (Nombre = 'pluriel')) then begin
          RadicalType := StringReplace(RadicalType, 'boi', 'buv', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'boi', 'buv', []);
        end
        else if ((Mode = 'participe') and (Temps = 'passé') or
          (Mode = 'indicatif') and (Temps = 'passé simple')) or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'boi', 'b', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'boi', 'b', []);
        end
        else if (Mode = 'subjonctif') and (Temps = 'présent') and (Nombre = 'singulier') then begin
          RadicalType := StringReplace(RadicalType, 'boi', 'boiv', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'boi', 'boiv', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'croire' then begin
        // Verbs ending in -croire
        // -----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) or
          ((Mode = 'subjonctif') and (Temps = 'présent') and (Nombre = 'pluriel')) then begin
          RadicalType := StringReplace(RadicalType, 'croi', 'croy', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'croi', 'croy', []);
        end
        else if ((Mode = 'participe') and (Temps = 'passé') or
          (Mode = 'indicatif') and (Temps = 'passé simple')) or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'croi', 'cr', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'croi', 'cr', []);
        end;
      end
      else if UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 6, 7) = 'croître' then begin
        // Verbs ending in -croître
        // ------------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) then begin
          RadicalType := StringReplace(RadicalType, 'croît', 'croiss', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'croît', 'croiss', []);
        end
        else if ((Mode = 'participe') and (Temps = 'passé')) or
          ((Mode = 'indicatif') and (Temps = 'passé simple')) or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'croît', 'cr', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'croît', 'cr', []);
        end
        else if
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then begin
          RadicalType := StringReplace(RadicalType, 'croît', 'croî', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'croît', 'croî', []);
        end;
      end
      else if UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 5, 6) = 'naître' then begin
        // Verbs ending in -naître
        // -----------------------
        if ((Mode = 'participe') and (Temps = 'présent')) or
          ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'pluriel')) or
          ((Mode = 'indicatif') and (Temps = 'imparfait')) or
          ((Mode = 'subjonctif') and (Temps = 'présent')) then begin
          RadicalType := StringReplace(RadicalType, 'naît', 'naiss', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'naît', 'naiss', []);
        end
        else if ((Mode = 'indicatif') and (Temps = 'passé simple')) or
          ((Mode = 'subjonctif') and (Temps = 'imparfait')) then begin
          RadicalType := StringReplace(RadicalType, 'naît', 'naqu', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'naît', 'naqu', []);
        end
        else if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then begin
          if Personne = '3e personne' then begin
            RadicalType := StringReplace(RadicalType, 'naît', 'naî', []);
            RadicalVerbe := StringReplace(RadicalVerbe, 'naît', 'naî', []);
          end
          else begin
            RadicalType := StringReplace(RadicalType, 'naît', 'nai', []);
            RadicalVerbe := StringReplace(RadicalVerbe, 'naît', 'nai', []);
          end;
        end
        else if (Mode = 'participe') and (Temps = 'passé') then begin
          RadicalType := StringReplace(RadicalType, 'naît', 'n', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'naît', 'n', []);
        end;
      end
      else if RightStr(Verbe.VerbeNom, 6) = 'foutre' then begin
        // Verbs ending in -foutre
        // -----------------------
        if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
          ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then begin
          RadicalType := StringReplace(RadicalType, 'fout', 'fou', []);
          RadicalVerbe := StringReplace(RadicalVerbe, 'fout', 'fou', []);
        end;
      end;
      VerbeConjugue := StringReplace(VerbeConjugue, RadicalType, RadicalVerbe, []);
    end;
  end;
  // Adaptions to be made for "verbes défectifs" and "participe passé"
  if VerbeConjugue = '#' then
    // "Verbe défectif": Set non-existing verb form to 'n/a'
    VerbeConjugue := 'n/a'
  else begin
    // Normal case: Determine the correct 'participe passé', considering actual auxiliary, number and person
    if (Mode = 'participe') and ((Temps = 'passé') or (Temps = 'passé composé')) then begin
      Last := RightStr(VerbeConjugue, 1);
      if Last[1] in ['!', '*', '$', '^', '+'] then
        // Remove "irregulary tag" before calling the 'accord part. passé' routine
        VerbeConjugue := UTF8Copy(VerbeConjugue, 1, UTF8Length(VerbeConjugue) - 1)
      else
        Last := '';
      VerbeConjugue := PartPasseAccord(VerbeConjugue, Auxiliaire, Nombre, Genre) + Last;
    end;
  end;
  ConjugaisonActif := VerbeConjugue;
end;

{ Adapt 'participe passé' ending, considering actual auxiliary, number and person }

function PartPasseAccord(PartPasse, Auxiliaire, Nombre, Genre: string): string;

var
  PPAccord: string;

begin
  PPAccord := PartPasse;
  if (Auxiliaire = 'être') and (Genre = 'féminin') then
    PPAccord += 'e';
  if (Auxiliaire = 'être') and (Nombre = 'pluriel') then begin
    if RightStr(PPAccord, 1) <> 's' then
      PPAccord += 's';
  end;
  PartPasseAccord := PPAccord;
end;

{ Get personal pronoun for given number, person and genre }

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

{ Get reflexive pronoun for given number and person }

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

{ Get 'impératif' reflexive pronoun for given number and person }

function PronomImperatif(Nombre, Personne: string): string;

var
  Pronom: string;

begin
  if Nombre = 'singulier' then
    Pronom := 'toi'
  else begin
    if LeftStr(Personne, 1) = '1' then
      Pronom := 'nous'
    else
      Pronom := 'vous';
  end;
  PronomImperatif := Pronom;
end;

{ "Double vowels" adaption: 'e' (as in 'se', 'je' or 'que') followed by a vowel to be replaced by apostrophe }

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
  if Verbe.Auxiliaire <> 'A' then                                                        // 'A' = avoir, 'E' = 'être', 'X' = 'both'
    Aux := VerbeEtre;
  GetAuxiliaire := Aux;
end;

end.

