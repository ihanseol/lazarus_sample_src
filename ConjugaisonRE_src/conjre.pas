{*****************************************************}
{* Unité principale pour l'application ConjugaisonRE *}
{*****************************************************}

unit conjRE;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  IniFiles, LazUTF8, conj, selectconj, unknownconj, selectex, common, help;

type
  {**********}
  { TfConjRE }
  {**********}
  TfConjRE = class(TForm)
    mMenu: TMainMenu;
    mMenuConj: TMenuItem;
    mConjRE, mConjFile, mConjConj, mConjExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsDisplay: TMenuItem;
    mOptionsDisplayIndicatif, mOptionsDisplaySep, mOptionsDisplaySepRacine, mOptionsDisplayIrreg, mOptionsDisplaySameCol, mOptionsNoWarning: TMenuItem;
    MenuItem2, mOptionsAux2: TMenuItem;
    mHelp: TMenuItem;
    mHelpConjRE, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Label1: TLabel;
    edQuestions: TEdit;
    laExercice: TLabel;
    edExercise, edAnswer, edEvaluation: TEdit;
    Label2, Label3, Label4, Label5: TLabel;
    edQuestion, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAnswer: TButton;
    btLetter1, btLetter2, btLetter3, btLetter4, btLetter5, btLetter6: TButton;
    btLetter7, btLetter8, btLetter9, btLetter10, btLetter11: TButton;
    memoQuestions: TMemo;
    memoAnswers: TMemo;
    dlgSave: TSaveDialog;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mConjREClick(Sender: TObject);
    procedure mConjFileClick(Sender: TObject);
    procedure mConjConjClick(Sender: TObject);
    procedure mConjExitClick(Sender: TObject);
    procedure mOptionsDisplayIndicatifClick(Sender: TObject);
    procedure mOptionsDisplaySameColClick(Sender: TObject);
    procedure mOptionsDisplaySepClick(Sender: TObject);
    procedure mOptionsDisplaySepRacineClick(Sender: TObject);
    procedure mOptionsDisplayIrregClick(Sender: TObject);
    procedure mOptionsNoWarningClick(Sender: TObject);
    procedure mHelpConjREClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
    procedure btLetter1Click(Sender: TObject);
    procedure btLetter2Click(Sender: TObject);
    procedure btLetter3Click(Sender: TObject);
    procedure btLetter4Click(Sender: TObject);
    procedure btLetter5Click(Sender: TObject);
    procedure btLetter6Click(Sender: TObject);
    procedure btLetter7Click(Sender: TObject);
    procedure btLetter8Click(Sender: TObject);
    procedure btLetter9Click(Sender: TObject);
    procedure btLetter10Click(Sender: TObject);
    procedure btLetter11Click(Sender: TObject);
  private
    iVerbs, iQuestions, iCorrect, iFalse, iQuestion: Integer;
    sConjugaison, sQuestion, sAnswer, sUserAnswer: string;
    bActif, bPassif, bPronominal, bIndicatif, bSubjonctif, bMOthers: Boolean;
    bTsimples, bTcomposes, bMasculin, bFeminin, bLetters: Boolean;
    rcVerbe, rcVerbeType: TVerbe;
  end;

var
  fConjRE: TfConjRE;
  Verbes: TVerbes;
  AvoirFile, EtreFile, VerbFile: TINIFile;
  OldVerbFilename: string;

implementation

{$R *.lfm}

{ Read verbs from text file }

procedure ReadVerbs(var Verbs: TVerbes; var NVerbs: Integer);

var
  Line, S: string;
  I: Integer;
  VFile: Text;

begin
  Assign(VFile, 'verbesRE.txt'); Reset(VFile);
  NVerbs := 0;
  while not EOF(VFile) and (NVerbs < NVerbsMax) do begin
    Inc(NVerbs);
    Readln(VFile, Line);
    with Verbs[NVerbs] do begin
      // Read verb name (chars 1-20)
      S := Copy(Line, 1, 20);
      // Remove trailing spaces
      I := Length(S);
      while S[I] = ' ' do
        Dec(I);
      VerbeNom := Copy(S, 1, I);
      // Read verb class, conjugation type and auxiliaire (chars 21, 22 and 23)
      S := UTF8Copy(Line, 21, 1); VerbeClass  := S[1];
      S := UTF8Copy(Line, 22, 1); Conjugaison := S[1];
      S := UTF8Copy(Line, 23, 1); Auxiliaire  := S[1];
      // Read passif and pronominal voice info (chars 24 and 25)
      VoixPassive := False; VoixPronominale := False;
      if Upcase(UTF8Copy(Line, 24, 1)) = 'P' then
        VoixPassive := True;
      if Upcase(UTF8Copy(Line, 25, 1)) = 'P' then
        VoixPronominale := True;
    end;
  end;
  Close(VFile);
end;

{ Get verb (full record) of given name by looking up the list read from file }

function GetVerbe(Verb: string; var Verbes: TVerbes; NVerbes: Integer): TVerbe;

const
  VerbeInconnu: TVerbe = (
    VerbeNom: ''; VoixPassive: False; VoixPronominale: False;
    VerbeClass: '-'; Auxiliaire: 'A'; Conjugaison: '?';
  );

var
  I, IX: Integer;
  Verbe: TVerbe;

begin
  I := 0; IX := -1;
  repeat
    Inc(I);
    if Verb = Verbes[I].VerbeNom then
      IX := I;                                                                           // verb index in list
  until (IX <> -1) or (I = NVerbes);
  // Verb not found: Set verb to 'unknown' (conjugation = '?') )
  if IX = - 1 then begin
    Verbe := VerbeInconnu;
    Verbe.VerbeNom := Verb;                                                              // fill-in the verb name
  end
  // Verb found in list: Fill in record variables
  else
    Verbe := Verbes[IX];
  GetVerbe := Verbe;
end;

{ Get "verbe type" for given verb }

procedure GetVerbeType(var Verbes: TVerbes; NVerbes: Integer; var Verbe: TVerbe; out VerbeType: TVerbe);

var
  Verb: string;
  Conjugaison: Char;
  WarnMess: Boolean;

// The procedure also sets the type of conjugation of the verb to be conjugated

begin
  Verb := ''; Conjugaison := Verbe.Conjugaison;
  // Regular verbs
  if Verbe.Conjugaison = '-' then
    Verb := 'rompre'
  // Semi-regular or irregular verbs
  else if Verbe.Conjugaison in ['S', 'I'] then
    Verb := Verbe.VerbeNom;
  if Verb = '' then begin
    // Compound of semi-irregular verb (or unknown conjugation verb)
    if (Verbe.Conjugaison = 's') or (Verbe.Conjugaison = '?') then begin
      if (UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 4, 5) = 'aître') and
        (UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 5, 6) <> 'naître')then
        Verb := 'paraître'
      else if (RightStr(Verbe.VerbeNom, 6) = 'aindre') or (RightStr(Verbe.VerbeNom, 6) = 'eindre')  or (RightStr(Verbe.VerbeNom, 6) = 'oindre') then
        Verb := 'craindre'
      else if RightStr(Verbe.VerbeNom, 4) = 'uire' then
        Verb := 'traduire';
      if (Verb <> '') and (Conjugaison = '?') then
        Conjugaison := 's';
    end;
    // Compound irregular verb (or unknown conjugation verb)
    if (Verbe.Conjugaison = 'i') or (Verbe.Conjugaison = '?') then begin
      if RightStr(Verbe.VerbeNom, 5) = 'clure' then
        Verb := 'conclure'
      else if RightStr(Verbe.VerbeNom, 6) = 'mettre' then
        Verb := 'mettre'
      else if RightStr(Verbe.VerbeNom, 5) = 'clore' then
        Verb := 'clore'
      else if RightStr(Verbe.VerbeNom, 6) = 'coudre' then
        Verb := 'coudre'
      else if RightStr(Verbe.VerbeNom, 6) = 'moudre' then
        Verb := 'moudre'
      else if RightStr(Verbe.VerbeNom, 5) = 'oudre' then
        Verb := 'résoudre'
      else if RightStr(Verbe.VerbeNom, 5) = 'crire' then
        Verb := 'écrire'
      else if RightStr(Verbe.VerbeNom, 5) = 'frire' then
        Verb := 'frire'
      else if RightStr(Verbe.VerbeNom, 4) = 'rire' then
        Verb := 'rire'
      else if RightStr(Verbe.VerbeNom, 4) = 'dire' then
        Verb := 'dire'
      else if RightStr(Verbe.VerbeNom, 4) = 'lire' then
        Verb := 'lire'
      else if (RightStr(Verbe.VerbeNom, 7) = 'suffire') or (RightStr(Verbe.VerbeNom, 7) = 'confire') or (RightStr(Verbe.VerbeNom, 10) = 'circoncire') then
        Verb := 'suffire'
      else if RightStr(Verbe.VerbeNom, 6) = 'suivre' then
        Verb := 'suivre'
      else if RightStr(Verbe.VerbeNom, 5) = 'vivre' then
        Verb := 'vivre'

      else if RightStr(Verbe.VerbeNom, 6) = 'traire' then
        Verb := 'extraire'
      else if RightStr(Verbe.VerbeNom, 5) = 'faire' then
        Verb := 'faire'
      else if RightStr(Verbe.VerbeNom, 4) = 'aire' then
        Verb := 'plaire'
      else if RightStr(Verbe.VerbeNom, 7) = 'prendre' then
        Verb := 'prendre'
      else if RightStr(Verbe.VerbeNom, 7) = 'vaincre' then
        Verb := 'vaincre'
      else if RightStr(Verbe.VerbeNom, 5) = 'boire' then
        Verb := 'boire'
      else if RightStr(Verbe.VerbeNom, 6) = 'croire' then
        Verb := 'croire'
      else if UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 6, 7) = 'croître' then
        Verb := 'croître'
      else if UTF8Copy(Verbe.VerbeNom, UTF8Length(Verbe.VerbeNom) - 5, 6) = 'naître' then
        Verb := 'naître'
      else if RightStr(Verbe.VerbeNom, 6) = 'foutre' then
        Verb := 'foutre';
      if (Verb <> '') and (Conjugaison = '?') then
        Conjugaison := 'i';
    end;
    // If conjugation couldn't be determined, suppose it's a regular verb
    if Conjugaison = '?' then begin
      if not fConjRE.mOptionsNoWarning.Checked then begin
        WarnMess := True;
        // Do never display warning for these verbs: they are regular
        if (RightStr(Verbe.VerbeNom, 5) = 'ompre') or
          (RightStr(Verbe.VerbeNom, 4) = 'ndre') or
          (RightStr(Verbe.VerbeNom, 5) = 'erdre') or
          (RightStr(Verbe.VerbeNom, 4) = 'ttre') or
          (RightStr(Verbe.VerbeNom, 5) = 'ordre') then
          WarnMess := False;
        if WarnMess then
          MessageDlg('Verbe inconnu', 'Type de conjugaison incertain! Verbe supposé être régulier.', mtWarning, [mbOK], 0);
      end;
      Verb := 'rompre'; Conjugaison := '-';
    end;
  end;
  Verbe.Conjugaison := Conjugaison;
  VerbeType := GetVerbe(Verb, Verbes, NVerbes);
end;

{ Choose file to be used as basis for conjugation of -re verbs}

procedure ChooseVerbFile(Verbe, VerbeType: TVerbe);

var
  VerbFilename: string;

// Verbfiles are .ini like text files (access by section and key). The files for "avoir" and "être" are apart and
// always open. The file chosen (and opened if it isn't) concerns only -re verbs.

begin
  if not ((Verbe.VerbeNom = 'avoir') or (Verbe.VerbeNom = 'être')) then begin
    VerbFilename := VerbeType.VerbeNom;
    VerbFilename := './verbes/' + VerbFilename + '.txt';
    DoDirSeparators(VerbFilename);
    // Open the file (unless it is already open)
    if VerbFilename <> OldVerbFilename then begin
      if OldVerbFilename <> '' then
        VerbFile.Free;
      VerbFile := TINIFile.Create(VerbFilename);
      OldVerbFilename := VerbFilename;
    end;
  end;
end;

{ Determine the exercise-conjugation-type (based on user selections on the "fSelectEx" form) }

function GetConjugaison: string;

var
  conj: string;
  Regular, Irregular, SemiIrregular: Boolean;

// The function returns a string which is decoded in order to determine if a given random verb
// is part of those, the user wants to do exercises with

begin
  conj := '';
  Regular := fSelectEx.cbRegular.Checked;
  Irregular := fSelectEx.cbIrregular.Checked;
  SemiIrregular := fSelectEx.cbSemiIrregular.Checked;
  if Regular and Irregular and SemiIrregular then
    conj := 're_all'
  else begin
    if fSelectEx.cbRegular.Checked then
      conj := 're_regall+';
    if SemiIrregular then
      conj += 're_semirregall+';
    if Irregular then
      conj += 're_irregall';
    if RightStr(conj, 1) = '+' then
      conj := LeftStr(conj, Length(conj) - 1);
  end;
  GetConjugaison := conj;
end;

{ Get short display form of "nombres", "personnes" and "genres" }

function ShortNPG(Nombre, Personne, Genre: string): string;

var
  NPG: string;

begin
  NPG := '';
  if (Personne <> '-') and (Nombre <> '-') then begin
    NPG := Personne + ' ' + Nombre;
    if Genre = 'féminin' then
      NPG += ', ' + Genre;
  end
  else begin
    if Genre = 'féminin' then
      NPG += Genre;
  end;
  NPG := StringReplace(NPG, 'personne', 'pers', []);
  NPG := StringReplace(NPG, 'singulier', 'sg', []);
  NPG := StringReplace(NPG, 'pluriel', 'pl', []);
  NPG := StringReplace(NPG, 'féminin', 'fém', []);
  ShortNPG := NPG;
end;

{ Clear exercise form fields and reset variables }

procedure ExerciseReset(var Question, AnswCorrect, AnswFalse: Integer);

begin
  Question := 0; AnswCorrect := 0; AnswFalse := 0;
  fConjRE.laExercice.Caption := 'Exercice';
  fConjRE.edExercise.Text := ''; fConjRE.edAnswer.Text := '';
  fConjRE.edEvaluation.Text := ''; fConjRE.edEvaluation.Font.Color := clDefault;
  fConjRE.edQuestion.Text := ''; fConjRE.edCorrect.Text := ''; fConjRE.edFalse.Text := '';
  fConjRE.edSuccess.Text := ''; fConjRE.edSuccess.Color := clDefault;
  fConjRE.edQuestions.Enabled := True; fConjRE.bLetters := False;
  fConjRE.btQuestion.Enabled := True; fConjRE.btAnswer.Enabled := False;
end;

{ Display success percentage }

procedure DisplaySuccess(Q, C: Integer);

var
  P: Real;

begin
  P := 100 * (C / Q);
  P := Int(100 * P) / 100;
  fConjRE.edSuccess.Text := FloatToStr(P) + '%';
  if P < 50 then
    fConjRE.edSuccess.Color := clRed
  else if P < 60 then
    fConjRE.edSuccess.Color := clYellow
  else
    fConjRE.edSuccess.Color := clLime;
end;

{ End of exercise session message }

procedure EndOfExercise;

begin
  fConjRE.btQuestion.Enabled := False;
  fConjRE.btAnswer.Enabled := False;
  MessageDlg('Exercice terminé', 'Utilisez le menu "Conjugaison" pour en démarrer un autre.', mtInformation, [mbOK], 0);
end;

{ Generate exercise with given random verb}

procedure ExerciseGenerate(Verbe, VerbeType: TVerbe; Actif, Passif, Pronominal,
  Ind, Subj, MOth, Simp, Comp, Masc, Fem: Boolean; var ExQuestion, ExAnswer: string);

var
  M, T, R: Integer;
  VerbeNom, Mode, Temps, Voix, Forme, Nombre, Personne, Genre, NPG: string;
  OK: Boolean;

begin
  Voix := '';
  if Verbe.VerbeClass = 'P' then
    // For pronominal verbs always use "voix pronominale"
    Voix := 'pronominal'
  else begin
    // For other verbs, get random voice (from those possible and selected)
    repeat
      R := Random(3);
      case R of
        0: if Actif then
             Voix := 'actif';
        1: if Passif then
             Voix := 'passif';
        2: if Pronominal then
             Voix := 'pronominal';
      end;
    until Voix <> '';
  end;
  // Get random mode (from those possible and selected)
  repeat
    OK := True;
    repeat
      Mode := '';
      R := Random(20) + 1;
      case R of
        1, 2, 3: if MOth then M := R;
          4..12: if Ind then M := 4;
         13..20: if Subj then M := 5;
      end;
      if M > 0 then
        Mode := Modes[M];
      if (Verbe.VerbeClass = 'I') and (Mode = 'impératif') then                          // impersonal verbs haven't an "impératif"
        Mode := '';
    until Mode <> '';
    // Get random tense  (from those possible for given mode)
    repeat
      case M of
        1, 3: T := 1;
           2: T := Random(2) + 1;
           4: begin
                R := Random(8) + 1;
                if R in [1, 2] then                                                      // less "présent" and "imparfait" than others
                  T := R
                else
                  T := (R + 3) div 2;;
              end;
           5: T := Random(2) + 1;
      end;
      // Get random tense group (from those possible and selected)
      Temps := '';
      R := Random(4);
      if (R = 0) and Comp then                                                           // 25% "temps composés"
        Temps := TempsComposes[M, T]
      else if (R > 0) and Simp then
        Temps := TempsSimples[M, T];
    until (Temps <> '') and (Temps <> '-');
    if (Voix <> 'actif') and (Mode = 'impératif') and (Temps = 'passé') then             // "impératif" only exists for "voix active"
      OK := False;
    if (Voix = 'pronominal') and (Mode = 'participe') and (Temps = 'passé') then         // "participe passé" doesn't exist for "voix pronominale"
      OK := False;
  until OK;
  Forme := Formes[1];                                                                    // "forme" not implemented in this version
  Nombre := '-'; Personne := '-'; Genre := 'masculin';
  // Get random number (from those possible)
  if (Mode = 'indicatif') or (Mode = 'subjonctif') or (Mode = 'impératif') then begin
    if Verbe.VerbeClass = 'I' then                                                       // "verbes impersonnels": "nombre" = "singulier"
      Nombre := Nombres[1]
    else begin                                                                           // other verbs: "nombre" = random
      R := Random(2) + 1; Nombre := Nombres[R];
    end;
    // Get random person (from those possible)
    if Mode = 'impératif' then begin                                                     // "impératif": some persons do not exist
      if Nombre = 'singulier' then
        Personne := Personnes[2]
      else begin
        R := Random(2) + 1; Personne := Personnes[R];
      end;
    end
    else begin                                                                           // other modes
      if Verbe.VerbeClass = 'I' then                                                     // "verbes impersonnels": "personne" = "3e personne"
        Personne := Personnes[3]
      else begin                                                                         // other verbs: "personne" = random
        R := Random(3) + 1; Personne := Personnes[R];
      end;
    end;
  end;
  // Get random genre (from those possible and selected)
  if Verbe.VerbeClass = 'I' then                                                         // "verbes impersonnels": "genre" = "masculin"
    Genre := Genres[1]
  else begin                                                                             // other verbs: genre = random
    repeat
      Genre := '';
      R := Random(2) + 1;
      if ((R = 1) and Masc) or ((R = 2) and Fem) then
        Genre := Genres[R];
    until Genre <> '';
  end;
  // Create the question text
  VerbeNom := Verbe.VerbeNom;
  if Voix = 'pronominal' then
    // Add pronoun "se" at beginning of pronominal verbs
    VerbeNom := DoubleVoyelle('se', UTF8Copy(VerbeNom, 1, 1)) + VerbeNom;
  ExQuestion := VerbeNom + ',';
  if (Mode <> 'indicatif') or (Temps = 'présent') or not fConjRE.mOptionsDisplayIndicatif.Checked then
    // Omit "indicatif" if this option is selected
    ExQuestion += ' ' + Mode;
  ExQuestion += ' ' + Temps;
  if Voix = 'passif' then
    // Omit voice if it is "actif" (normal case) or "pronominal" (in this case, the verb starts with pronoun "se")
    ExQuestion +=  ', ' + Voix;
  // Number and person only taken if applicable; genre omited, if it is "masculin"
  NPG := ShortNPG(Nombre, Personne, Genre);
  if NPG <> '' then
    ExQuestion += ', ' + NPG;
  if Forme <> 'affirmative' then
    // Omit "forme affirmative" (normal case)
    ExQuestion += ' (f. ' + Forme + ')';
  // Do the conjugation (with the parameters computed here)
  ExAnswer := Conjugaison(Verbe, VerbeType, Voix, Mode, Temps, Forme, Nombre, Personne, Genre, AvoirFile, EtreFile, VerbFile);
  ExAnswer := StringReplace(ExAnswer, '--', '', [rfReplaceAll]);                         // remove the verb part seperators
end;

{ Choose random verb (from those conjugation groups selected by the user) and generate random exercise question }

procedure ExerciseRandomQuestion(Verbs: Integer; var Verbes: TVerbes; Conjugaison: string; Actif, Passif, Pronominal,
  Indicatif, Subjonctif, MOthers, TSimples, TComposes, Masculin, Feminin: Boolean; var  Verbe: TVerbe; var Question, Answer: string);

var
  R, P1, P2: Integer;
  VerbeNom: string;
  Pass, Pron, VerbeOK: Boolean;
  VerbeType: TVerbe;

begin
  // Repeat picking a random verb until you got one which is actually part of the conjugation group selected by the user
  repeat
    Pass := Passif; Pron := Pronominal; VerbeOK := False;
    // Mixed groups of -re verbs
    if LeftStr(Conjugaison, 3) = 're_' then begin
      R := Random(Verbs) + 1;
      if Conjugaison = 're_all' then
        // Any verb...
        VerbeOK := True
      else begin
        // Verb groups...
        P1 := Pos('re_semirregall', Conjugaison); P2 := Pos('re_irregall', Conjugaison);
        if (LeftStr(Conjugaison, 9) = 're_regall') and (Verbes[R].Conjugaison = '-') then
          // Regular -re verbs
          VerbeOK := True
        else if (P1 <> 0) and (UpperCase(Verbes[R].Conjugaison) = 'S') then
          // Semi-irregular -re verb
          VerbeOK := True
        else if (P2 <> 0) and (UpperCase(Verbes[R].Conjugaison) = 'I') then
          // Any irregular -re verb
          VerbeOK := True;
      end;
      if VerbeOK then
        // Verb found!
        VerbeNom := Verbes[R].VerbeNom;
    end;
    // Check if the random verb found may effectively be conjugated with the actual conjugation parameters
    if VerbeOK then begin
      Verbe := GetVerbe(VerbeNom, Verbes, Verbs);                                        // get verb parameters (from -re verb list)
      if not Verbe.VoixPassive then
        Pass := False;
      if not Verbe.VoixPronominale then
        Pron := False;
      if not Pass then begin
        if not (Actif or Pron) then
          VerbeOK := False;
      end;
      if not Pron then begin
        if not (Actif or Pass) then
          VerbeOK := False;
      end;
      if (Verbe.VerbeClass = 'P') and not Pron then
        VerbeOK := False;
      if (Verbe.VerbeClass = 'I') and not Masculin then
        VerbeOK := False;
    end;
  until VerbeOK;
  // Generate the exercise
  GetVerbeType(Verbes, Verbs, Verbe, VerbeType);
  ChooseVerbFile(Verbe, VerbeType);                                                      // define the verb-file to be used for -re verbs
  Pass := Passif; Pron := Pronominal;
  if not Verbe.VoixPassive then                                                          // be sure not trying to use "voix passive" if verb hasn't one
    Pass := False;
  if not Verbe.VoixPronominale then                                                      // be sure not trying to use "voix pronominale" if verb hasn't one
    Pron := False;
  ExerciseGenerate(Verbe, VerbeType,
    Actif, Pass, Pron, Indicatif, Subjonctif, MOthers, TSimples, TComposes, Masculin, Feminin, Question, Answer);
end;

{ Show help text (after loading it from text file ) }

procedure ShowHelpText(FileName: string);

begin
  if FileName = 'conjRE' then
    fHelp.stTitle.Caption := 'Conjugaison des verbes français en -re.'
  else
    fHelp.stTitle.Caption := 'ConjugaisonRE: Aide programme.';
  FileName += '.txt';
  fHelp.memoHelp.Lines.Clear;
  fHelp.memoHelp.Lines.LoadFromFile(FileName);
  fHelp.Show;
end;

{ Insert "French" letter corresponding to letter-button pushed by user }

procedure InsertLetter(IX: Integer);

const
  Letters: array[1..11] of string = (
    'à', 'â', 'é', 'è', 'ê', 'ë', 'î', 'ô', 'ù', 'û', 'ç'
  );

var
  S: string;

begin
  if fConjRE.bLetters then begin
    S := fConjRE.edAnswer.Text;
    S += Letters[IX];
    fConjRE.edAnswer.Text := S;
  end;
end;

{************}
{* TfConjRE *}
{************}

{ Application start: Initialisation }

procedure TfConjRE.FormCreate(Sender: TObject);

var
  Filename: string;

begin
  ReadVerbs(Verbes, iVerbs);                                                             // read -re verblist from file
  Filename := './verbes/avoir.txt'; DoDirSeparators(Filename);
  AvoirFile := TINIFile.Create(Filename);                                                // open "avoir" conjugation files
  Filename := './verbes/être.txt'; DoDirSeparators(Filename);
  EtreFile  := TINIFile.Create(Filename);                                                // open "être" conjugation files
  OldVerbFilename := '';
  iQuestions := 20;                                                                      // initial number of questions
  sConjugaison := 're_all';                                                              // all -re verbs in exercises
  bActif := True; bPassif := False; bPronominal := False;                                // other initial exercise settings
  bIndicatif := True; bSubjonctif := True; bMOthers := True;
  bTSimples := True; bTComposes := True;
  bMasculin := True; bFeminin := False;
  Randomize;                                                                             // start of random generator
end;

{ Form activation: Clear form fields }

procedure TfConjRE.FormActivate(Sender: TObject);

begin
  ExerciseReset(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Conjugaison > Exercices verbes -re": Select "verbes -re" conjugation as exercises }

procedure TfConjRE.mConjREClick(Sender: TObject);

begin
  // Open form for user to select verbs and conjugation options
  fSelectEx.ShowModal;
  // Get the "conjugation string" (verbs to include) for user selection
  sConjugaison := GetConjugaison;
  // Set wanted voice(s), mode(s), tense(s) and sexe(s)
  bActif  := fSelectEx.cbActif.Checked;   bPassif := fSelectEx.cbPassif.Checked;   bPronominal := fSelectEx.cbPron.Checked;
  bIndicatif  := fSelectEx.cbInd.Checked; bSubjonctif := fSelectEx.cbSubj.Checked; bMOthers := fSelectEx.cbMOthers.Checked;
  bTSimples := fSelectEx.cbTSimp.Checked; bTComposes := fSelectEx.cbTComp.Checked;
  bMasculin := fSelectEx.cbMasc.Checked;  bFeminin := fSelectEx.cbFem.Checked;
  // Reset exercise counters
  ExerciseReset(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Fichier exercices": Generate exercises files }

procedure TfConjRE.mConjFileClick(Sender: TObject);

var
  I: Integer;
  Dir, FileName, FileExt, FileName1, FileName2: string;

begin
  iQuestions := StrToInt(edQuestions.Text);
  memoQuestions.Clear; memoAnswers.Clear;
  // Exercise questions are generated, using the verbs and conjugation options actually selected
  for I := 1 to iQuestions do begin
    // Generate an exercise question
    repeat
      ExerciseRandomQuestion(iVerbs, Verbes, sConjugaison, bActif, bPassif, bPronominal, bIndicatif, bSubjonctif,
        bMOthers, bTSimples, bTComposes, bMasculin, bFeminin, rcVerbe, sQuestion, sAnswer);
    until sAnswer <> 'n/a';                                                              // to avoid conugated verb forms that don't exist
    // Append question and answer to corresponding Memo (invisible Memo, just use for storage)
    memoQuestions.Append(sQuestion);
    // Remove "irregularity tags"
    sAnswer := StringReplace(sAnswer, '!', '', []);
    sAnswer := StringReplace(sAnswer, '$', '', []);
    sAnswer := StringReplace(sAnswer, '^', '', []);
    sAnswer := StringReplace(sAnswer, '*', '', []);
    sAnswer := StringReplace(sAnswer, '+', '', []);
    memoAnswers.Append(sQuestion + ': ' + sAnswer);
  end;
  Dir := GetUserDir + 'Documents';
  if DirectoryExists(Dir) then
    Dir += '/'
  else
    Dir := '';
  DoDirSeparators(Dir);
  dlgSave.InitialDir := Dir;
  // Create the files by writing the Memos content to them
  if dlgSave.Execute then begin
    FileName := ExtractFileName(dlgSave.Filename);
    FileExt := ExtractFileExt(FileName);
    FileName := StringReplace(FileName, FileExt, '', []);
    if Fileext = '' then
      Fileext := '.txt';
    FileName1 := Dir + FileName + ' (Questions)' + FileExt;                              // questions file
    FileName2 := Dir + FileName + ' (Réponses)' + FileExt;                               // questions + answers file
    memoQuestions.Lines.SaveToFile(FileName1);
    memoAnswers.Lines.SaveToFile(FileName2);
  end;
end;

{ Menu item "Conjugaison > Affichage conjugaison": Do conjugation for given verb }

procedure TfConjRE.mConjConjClick(Sender: TObject);

var
  VerbeNom, PReflex: string;
  OK: Boolean;

begin
  OK := False;
  // Show up the verb entry form to get the verb to be conjugated
  fSelectConj.sButton := '';
  fSelectConj.ShowModal;
  // Proceed if the user pushed the "OK" button (on fSelectConj form)
  if fSelectConj.sButton = 'ok' then begin
    OK := True;
    VerbeNom := fSelectConj.sVerbe;
    // Check if the the verb entered is a verb ending in -re
    if (RightStr(VerbeNom, 2) <> 're') then begin
      MessageDlg('Verbe invalide', 'Ce programme ne sait conjuguer que les verbes en -re!', mtError, [mbOK], 0);
      OK := False;
    end
    // Check if user entered a verb handled by this program
    else begin
      // Get verb data
      PReflex := '';
      if (LeftStr(VerbeNom, 3) = 'se ') or (LeftStr(VerbeNom, 2) = 's''') then begin
        if LeftStr(VerbeNom, 3) = 'se ' then
          PReflex := LeftStr(VerbeNom, 3)
        else
          PReflex := LeftStr(VerbeNom, 2);
        VerbeNom := StringReplace(VerbeNom, PReflex, '', []);                  // reflexive pronoun removed in order to find verb in list
      end;
      rcVerbe := GetVerbe(VerbeNom, Verbes, iVerbs);
      // User entered a pronominal verb (verb starting with "se" or "s'")
      if PReflex <> '' then begin
        if rcVerbe.Conjugaison = '?' then begin
          rcVerbe.VerbeNom := PReflex + rcVerbe.VerbeNom;                      // reflexive pronoun readded (to recognize them as pronominal in "unknown verb" code below)
        end
        else begin
          if not rcVerbe.VoixPronominale then begin                            // known verbs, listed "no pronominal voice"
            MessageDlg('Verbe invalide', 'La forme pronominale du verbe ' + rcVerbe.VerbeNom + ' n''existe pas!', mtError, [mbOK], 0);
            OK := False;
          end;
        end;
        // Set verb data for pronominal verb
        if OK then begin
          rcVerbe.VerbeClass := 'P';
          rcVerbe.VoixPronominale := True;
          rcVerbe.VoixPassive := False;
          rcVerbe.Auxiliaire := 'E';
        end;
      end;
      if OK then begin
        // If verb isn't part of verbs known by the program (verb described in file "VerbesRE.txt") ),
        // show up another form, where the user has the possibilty to enter the verb data by herself
        if rcVerbe.Conjugaison = '?' then begin
          fUnknown.Verbe := rcVerbe;
          fUnknown.sButton := '';
          fUnknown.ShowModal;
          // Proceed if user pushed the "OK" button (on the fUnknown form)
          if fUnknown.sButton = 'ok' then begin
            rcVerbe := fUnknown.Verbe;
            // Remove reflexive pronoun (re-added above)
            if LeftStr(rcVerbe.VerbeNom, 3) = 'se ' then
              rcVerbe.VerbeNom := StringReplace(rcVerbe.VerbeNom, 'se ', '', [])
            else if LeftStr(rcVerbe.VerbeNom, 2) = 's''' then
              rcVerbe.VerbeNom := StringReplace(rcVerbe.VerbeNom, 's''', '', []);
          end
          else
            OK := False;
        end;
      end;
    end;
    // Pass all necessary data for conjugation of actual verb to the fConj form and display this one
    if OK then begin
      GetVerbeType(Verbes, iVerbs, rcVerbe, rcVerbeType);
      // Actual verb and "verbe type"
      fConj.Verbe := rcVerbe;
      fConj.VerbeType := rcVerbeType;
      // Voice buttons enable/disable
      fConj.btActif.Enabled := True; fConj.btPassif.Enabled := True; fConj.btPronominal.Enabled := True;
      // Default settings for form start-up
      fConj.sVoix := 'actif';
      fConj.btActif.Font.Style := [fsBold]; fConj.btPassif.Font.Style := []; fConj.btPronominal.Font.Style := [];
      fConj.btPronominal.Visible := True;
      if rcVerbe.VerbeClass = 'P' then
        fConj.btPronominal.Visible := False;
      fConj.sForme := 'affirmative';
      fConj.btAffirmatif.Font.Style := [fsBold]; fConj.btNegatif.Font.Style := [];
      fConj.btInterrogatif.Font.Style := []; fConj.btIntNegatif.Font.Style := [];
      fConj.sTemps := 'simples';
      fConj.btTSimples.Font.Style := [fsBold]; fConj.btTComposes.Font.Style := [];
      fConj.sGenre := 'masculin';
      fConj.btMasculin.Font.Style := [fsBold]; fConj.btFeminin.Font.Style := [];
      fConj.btFeminin.Enabled := True;
      // Buttons disable if the corresponding selection is invalid for actual verb
      if rcVerbe.VerbeClass = 'I' then
        fConj.btFeminin.Enabled := False;
      if not rcVerbe.VoixPassive then
        fConj.btPassif.Enabled := False;
      if not rcVerbe.VoixPronominale then
        fConj.btPronominal.Enabled := False;
      // Conjugation parameters
      fConj.bSepTerm := mOptionsDisplaySep.Checked;
      fConj.bSepRacine := mOptionsDisplaySepRacine.Checked;;
      fConj.bColorIrreg := mOptionsDisplayIrreg.Checked;
      fConj.bColorSame := mOptionsDisplaySameCol.Checked;
      // Verb files
      ChooseVerbFile(rcVerbe, rcVerbeType);
      fConj.AvoirFile := AvoirFile;
      fConj.EtreFile  := EtreFile;
      fConj.VerbFile  := VerbFile;
      // Show the fConj form (with default settings conjugation)
      if not fConj.Visible then
        fConj.Show;
    end;
  end;
end;

{ Menu item "Conjugaison > Quitter": Exit application }

procedure TfConjRE.mConjExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Affichage > Ne pas afficher 'Indicatif'": Choose display option for mode = 'Indicatif' }

procedure TfConjRE.mOptionsDisplayIndicatifClick(Sender: TObject);

begin
  if mOptionsDisplayIndicatif.Checked then
    mOptionsDisplayIndicatif.Checked := False
  else
    mOptionsDisplayIndicatif.Checked := True;
end;

{ Menu item "Options > Affichage > Séparation entre le radical et la terminaison": Choose display option for 'terminaison' separator }

procedure TfConjRE.mOptionsDisplaySepClick(Sender: TObject);

begin
  if mOptionsDisplaySep.Checked then begin
    mOptionsDisplaySep.Checked := False;
    mOptionsDisplaySepRacine.Checked := False;
    mOptionsDisplaySepRacine.Enabled := False;
  end
  else begin
    mOptionsDisplaySep.Checked := True;
    mOptionsDisplaySepRacine.Enabled := True;
  end;
end;

{ Menu item "Options > Affichage > Séparation entre la racine et le reste du radical": Choose display option for 'radical' separator }

procedure TfConjRE.mOptionsDisplaySepRacineClick(Sender: TObject);

begin
  if mOptionsDisplaySepRacine.Checked then
    mOptionsDisplaySepRacine.Checked := False
  else
    mOptionsDisplaySepRacine.Checked := True;
end;

{ Menu item "Options > Affichage > Afficher les irrégularités en couleur": Choose display option for irregular verbs (colored or not) }

procedure TfConjRE.mOptionsDisplayIrregClick(Sender: TObject);

begin
  if mOptionsDisplayIrreg.Checked then begin
    mOptionsDisplayIrreg.Checked := False;
    mOptionsDisplaySameCol.Checked := False;
    mOptionsDisplaySameCol.Enabled := False;
  end
  else begin
    mOptionsDisplayIrreg.Checked := True;
    mOptionsDisplaySameCol.Enabled := True;
  end;
end;

{ Menu item "Options > Affichage des verbes > Utiliser la même couleur pour toutes les irrégularités": Choose display option for irregular verbs (one or several colors) }

procedure TfConjRE.mOptionsDisplaySameColClick(Sender: TObject);

begin
  if mOptionsDisplaySameCol.Checked then
    mOptionsDisplaySameCol.Checked := False
  else
    mOptionsDisplaySameCol.Checked := True;
end;

{ Menu item "Options > Ne pas afficher les avertissements": Choose to display or not a warning when no conjugation type could be determined }

procedure TfConjRE.mOptionsNoWarningClick(Sender: TObject);

begin
  if mOptionsNoWarning.Checked then
    mOptionsNoWarning.Checked := False
  else
    mOptionsNoWarning.Checked := True;
end;

{ Menu item "Aide > Verbes en -re": Display "verbs in -re" help }

procedure TfConjRE.mHelpConjREClick(Sender: TObject);

begin
  ShowHelpText('conjRE');
end;

{ Menu item "Aide > Aide programme": Display application help }

procedure TfConjRE.mHelpHelpClick(Sender: TObject);

begin
  ShowHelpText('help');
end;

{ Menu item "Aide > Info programme": Display application about }

procedure TfConjRE.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Conjugaison des verbes français:' + LineEnding;
  S += 'Conjugaison des verbes en -re.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, octobre 2018 - avril 2019.';
  MessageDlg('Grammaire française', S, mtInformation, [mbOK], 0);
end;

{ Button "Question": Generate an exercise with parameters as selected }

procedure TfConjRE.btQuestionClick(Sender: TObject);

begin
  // Get number of questions for actual exercise session
  if iQuestion = 0 then begin
    iQuestions := StrToInt(edQuestions.Text);
    edQuestions.Enabled := False;
  end;
  // Do this for answers skipped (user pressing the "Question" button for next question without answering)
  if iQuestion > 0 then begin
    edQuestion.Text := IntToStr(iQuestion);
    iFalse := iQuestion - iCorrect; edFalse.Text := IntToStr(iFalse);
    edCorrect.Text := IntToStr(iCorrect);
    DisplaySuccess(iQuestion, iCorrect);
  end;
  Inc(iQuestion);
  // If there are still exercises to do: Generate new question
  if iQuestion <= iQuestions then begin
    laExercice.Caption := 'Exercice ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    repeat
      ExerciseRandomQuestion(iVerbs, Verbes, sConjugaison, bActif, bPassif, bPronominal, bIndicatif, bSubjonctif,
        bMOthers, bTSimples, bTComposes, bMasculin, bFeminin, rcVerbe, sQuestion, sAnswer);
    until sAnswer <> 'n/a';                                                              // to avoid conugated verb forms that don't exist
    // Remove "irregularity tags"
    sAnswer := StringReplace(sAnswer, '!', '', []);
    sAnswer := StringReplace(sAnswer, '$', '', []);
    sAnswer := StringReplace(sAnswer, '^', '', []);
    sAnswer := StringReplace(sAnswer, '*', '', []);
    sAnswer := StringReplace(sAnswer, '+', '', []);
    edExercise.Text := sQuestion;
    edAnswer.Text := '';
    edEvaluation.Text := '';
    btAnswer.Enabled := True;
    edAnswer.SetFocus;
    bLetters := True;
  end
  // If the number of exercises to do has been reached: Display "end of exercise" message
  else
    EndOfExercise;
end;

{ Button "Réponse": Check user's answer }

procedure TfConjRE.btAnswerClick(Sender: TObject);

begin
  edQuestion.Text := IntToStr(iQuestion);
  sUserAnswer := edAnswer.Text;
  // Correct answer
  if sUserAnswer = sAnswer then begin
    Inc(iCorrect);
    edEvaluation.Text := 'Bonne réponse!';
    edEvaluation.Font.Color := clLime;
  end
  // Wrong answer
  else begin
    Inc(iFalse);
    edEvaluation.Text := 'Faux! La conjugaison correcte est: ' + sAnswer;
    edEvaluation.Font.Color := clRed;
  end;
  // Display correct and false answers and success percentage
  edCorrect.Text := IntToStr(iCorrect);
  edFalse.Text := IntToStr(iFalse);
  DisplaySuccess(iQuestion, iCorrect);
  // If there are still exercises to do: Wait for user to push "Question" button
  if iQuestion < iQuestions then
    btAnswer.Enabled := False
  // If the number of exercises to do has been reached: Display "end of exercise" message
  else
    EndOfExercise;
end;

{ Non-English letters buttons: Insert the corresponding letter into the user's answer string }

procedure TfConjRE.btLetter1Click(Sender: TObject);

begin
  InsertLetter(1);
end;

procedure TfConjRE.btLetter2Click(Sender: TObject);

begin
  InsertLetter(2);
end;

procedure TfConjRE.btLetter3Click(Sender: TObject);

begin
  InsertLetter(3);
end;

procedure TfConjRE.btLetter4Click(Sender: TObject);

begin
  InsertLetter(4);
end;

procedure TfConjRE.btLetter5Click(Sender: TObject);

begin
  InsertLetter(5);
end;

procedure TfConjRE.btLetter6Click(Sender: TObject);

begin
  InsertLetter(6);
end;

procedure TfConjRE.btLetter7Click(Sender: TObject);

begin
  InsertLetter(7);
end;

procedure TfConjRE.btLetter8Click(Sender: TObject);

begin
  InsertLetter(8);
end;

procedure TfConjRE.btLetter9Click(Sender: TObject);

begin
  InsertLetter(9);
end;

procedure TfConjRE.btLetter10Click(Sender: TObject);

begin
  InsertLetter(10);
end;

procedure TfConjRE.btLetter11Click(Sender: TObject);

begin
  InsertLetter(11);
end;

end.

