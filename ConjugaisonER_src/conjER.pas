{*****************************************************}
{* Unité principale pour l'application ConjugaisonER *}
{*****************************************************}

unit conjER;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  IniFiles, LazUTF8, conj, selectconj, unknownconj, selectex, common, help;

type
  { TfConjER }
  TfConjER = class(TForm)
    mMenu: TMainMenu;
    mMenuConj: TMenuItem;
    mConjAvoir, mConjEtre, mConjER, mConjFile, mConjConj, mConjExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsDisplay: TMenuItem; MenuItem1: TMenuItem;
    mOptionsDisplayIndicatif, mOptionsDisplaySep, mOptionsDisplaySepRacine, mOptionsDisplayIrreg, mOptionsDisplayExcept: TMenuItem;
    mOptionsConj: TMenuItem;
    mOptionsConjReforme, mOptionsConjKeepY: TMenuItem;
    mOptionsAux2: TMenuItem;
    mHelp: TMenuItem;
    mHelpConj, mHelpConjER, mHelpHelp, mHelpAbout: TMenuItem;
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
    procedure mConjAvoirClick(Sender: TObject);
    procedure mConjEtreClick(Sender: TObject);
    procedure mConjERClick(Sender: TObject);
    procedure mConjFileClick(Sender: TObject);
    procedure mConjConjClick(Sender: TObject);
    procedure mConjExitClick(Sender: TObject);
    procedure mOptionsDisplayIndicatifClick(Sender: TObject);
    procedure mOptionsDisplaySepClick(Sender: TObject);
    procedure mOptionsDisplaySepRacineClick(Sender: TObject);
    procedure mOptionsDisplayIrregClick(Sender: TObject);
    procedure mOptionsDisplayExceptClick(Sender: TObject);
    procedure mOptionsConjReformeClick(Sender: TObject);
    procedure mOptionsConjKeepYClick(Sender: TObject);
    procedure mHelpConjClick(Sender: TObject);
    procedure mHelpConjERClick(Sender: TObject);
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
    bTsimples, bTcomposes, bMasculin, bFeminin, bReforme1990, bKeepY, bLetters: Boolean;
    rcVerbe, rcVerbeType: TVerbe;
  end;

var
  fConjER: TfConjER;
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
  Assign(VFile, 'verbesER.txt'); Reset(VFile);
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
      // Read verb class, conjugation type and auxiliary (chars 21, 22 and 23)
      S := UTF8Copy(Line, 21, 1); VerbeClass  := S[1];
      S := UTF8Copy(Line, 22, 1); Conjugaison := S[1];
      S := UTF8Copy(Line, 23, 1); Auxiliaire  := S[1];
      // Read passive and pronominal voice info (chars 24 and 25)
      VoixPassive := False; VoixPronominale := False;
      if Upcase(UTF8Copy(Line, 24, 1)) = 'P' then
        VoixPassive := True;
      if Upcase(UTF8Copy(Line, 25, 1)) = 'P' then
        VoixPronominale := True;
    end;
  end;
end;

{ Get verb (full record) of given name by looking up the list read from file }

procedure GetVerbe(Verb: string; var Verbes: TVerbes; NVerbes: Integer; var Verbe: TVerbe);

const
  VerbeInconnu: TVerbe = (
    VerbeNom: ''; VoixPassive: False; VoixPronominale: False;
    VerbeClass: ' '; Auxiliaire: ' '; Conjugaison: '?';
  );

var
  I, IX: Integer;

begin
  // Auxiliaries (not in the -er verbs list)
  if Verb = 'avoir' then
    Verbe := VerbeAvoir
  else if Verb = 'être' then
     Verbe := VerbeEtre
  // -er verbs
  else begin
    I := 0; IX := -1;
    repeat
      Inc(I);
      if Verb = Verbes[I].VerbeNom then
        IX := I;                                                               // verb index in list
    until (IX <> -1) or (I = NVerbes);
    // Verb not found: set verb to 'unknown' (conjugation = '?') )
    if IX = - 1 then begin
      Verbe := VerbeInconnu;
      Verbe.VerbeNom := Verb;                                                  // fill-in the verb name
    end
    // Verb found in list: fill in record variables
    else begin
      with Verbe do begin
        VerbeNom        := Verb;
        VerbeClass      := Verbes[IX].VerbeClass;
        VoixPassive     := Verbes[IX].VoixPassive;
        VoixPronominale := Verbes[IX].VoixPronominale;
        Auxiliaire      := Verbes[IX].Auxiliaire;
        Conjugaison     := Verbes[IX].Conjugaison;
      end;
    end;
  end;
end;

{ Get typical -er verb (the one for which a conjugation file exists) }

procedure GetVerbeType(var Verbes: TVerbes; NVerbes: Integer; var Verbe: TVerbe);

var
  I: Integer;
  VerbeNom: string;

begin
  I := 0; VerbeNom := '';
  repeat
    Inc(I);
    if Verbes[I].Conjugaison = 'T' then                                        // verb class is 'T' for this verb
      VerbeNom := Verbes[I].VerbeNom;                                          // get verb name from list
  until VerbeNom <> '';
  GetVerbe(VerbeNom, Verbes, NVerbes, Verbe);                                  // fill in the verb record variables
end;

{ Choose file to be used as basis for conjugation  of -er verbs}

procedure ChooseVerbFile(Verbe, VerbeType: TVerbe);

var
  VerbFilename: string;

// Verbfiles are .ini like text files (access by section and key). The files for "avoir" and "être" are apart and
// always open. The file chosen (and opened if it isn't) concerns only -er verbs.

begin
  if not ((Verbe.VerbeNom = 'avoir') or (Verbe.VerbeNom = 'être')) then begin
    if Verbe.Conjugaison = 'I' then
      // One particular file for each irregular verb
      VerbFilename := Verbe.VerbeNom
    else if Verbe.Conjugaison = 'i' then begin
      // Usage of these files for verbs conjugating the same way
      if (Verbe.VerbeNom = 'en aller') or (Verbe.VerbeNom = 'y aller') then
        VerbFilename := 'aller'
      else if Verbe.VerbeNom = 'renvoyer' then
        VerbFilename := 'envoyer';          ;
    end
    else
      // Regular (or special) -er verbs: use "verbe type" file in this case
      VerbFilename := VerbeType.VerbeNom;
    VerbFilename += '.txt';
    if VerbFilename <> OldVerbFilename then begin
      // Open the file (unless it is already open)
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
  Conj: string;
  IrrAll, SpecAll, IrrNone, SpecNone: Boolean;

// The function returns a string which is decoded in order to determine if a given random verb
// is part of those, the user wants to do exercises with

begin
  Conj := '';
  IrrAll   := fSelectEx.cbAller.Checked and fSelectEx.cbEnvoyer.Checked;
  SpecAll  := fSelectEx.cbCER.Checked and fSelectEx.cbE_ER.Checked and fSelectEx.cbGER.Checked and fSelectEx.cbELER.Checked and fSelectEx.cbAYER.Checked;
  IrrNone  := not (fSelectEx.cbAller.Checked or fSelectEx.cbEnvoyer.Checked);
  SpecNone := not (fSelectEx.cbCER.Checked or fSelectEx.cbE_ER.Checked or fSelectEx.cbGER.Checked or fSelectEx.cbELER.Checked or fSelectEx.cbAYER.Checked);
  if fSelectEx.cbRegular.Checked and IrrAll and SpecAll then
    Conj := 'er_all'
  else begin
    if fSelectEx.cbRegular.Checked then
      Conj := 'er_allreg'
    else
      Conj := 'er_noreg';
    if IrrAll then
      Conj += '+allirr'
    else if IrrNone then
      Conj += '+noirr'
    else if fSelectEx.cbAller.Checked or fSelectEx.cbEnvoyer.Checked then
      Conj += '+someirr';
    if SpecAll then
      Conj += '+allspec'
    else if SpecNone then
      Conj += '+nospec'
    else if fSelectEx.cbCER.Checked or fSelectEx.cbE_ER.Checked or fSelectEx.cbGER.Checked or fSelectEx.cbELER.Checked or fSelectEx.cbAYER.Checked then
      Conj += '+somespec';
  end;
  if  not fSelectEx.cbRegular.Checked and SpecNone then begin
    if fSelectEx.cbAller.Checked and not fSelectEx.cbEnvoyer.Checked then
      Conj := 'aller'
    else if not fSelectEx.cbAller.Checked and fSelectEx.cbEnvoyer.Checked then
      Conj := 'envoyer';
  end;
  GetConjugaison := Conj;
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
  fConjER.laExercice.Caption := 'Exercice';
  fConjER.edExercise.Text := ''; fConjER.edAnswer.Text := '';
  fConjER.edEvaluation.Text := ''; fConjER.edEvaluation.Font.Color := clDefault;
  fConjER.edQuestion.Text := ''; fConjER.edCorrect.Text := ''; fConjER.edFalse.Text := '';
  fConjER.edSuccess.Text := ''; fConjER.edSuccess.Color := clDefault;
  fConjER.edQuestions.Enabled := True; fConjER.bLetters := False;
  fConjER.btQuestion.Enabled := True; fConjER.btAnswer.Enabled := False;
end;

{ Display success percentage }

procedure DisplaySuccess(Q, C: Integer);

var
  P: Real;

begin
  P := 100 * (C / Q);
  P := Int(100 * P) / 100;
  fConjER.edSuccess.Text := FloatToStr(P) + '%';
  if P < 50 then
    fConjER.edSuccess.Color := clRed
  else if P < 60 then
    fConjER.edSuccess.Color := clYellow
  else
    fConjER.edSuccess.Color := clLime;
end;

{ End of exercise session message }

procedure EndOfExercise;

begin
  fConjER.btQuestion.Enabled := False;
  fConjER.btAnswer.Enabled := False;
  MessageDlg('Exercice terminé', 'Utilisez le menu "Conjugaison" pour en démarrer un autre.', mtInformation, [mbOK], 0);
end;

{ Generate exercise with given random verb}

procedure ExerciseGenerate(Verbe, VerbeType: TVerbe; Actif, Passif, Pronominal,
  Ind, Subj, MOth, Simp, Comp, Masc, Fem, Reforme1990, KeepY: Boolean; var ExQuestion, ExAnswer: string);

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
      if (Verbe.VerbeClass = 'I') and (Mode = 'impératif') then                // impersonal verbs haven't an "impératif"
        Mode := '';
    until Mode <> '';
    // Get random tense (from those possible for given mode)
    repeat
      case M of
        1, 3: T := 1;
           2: T := Random(2) + 1;
           4: begin
                R := Random(8) + 1;
                if R in [1, 2] then                                            // less "présent" and "imparfait" than others
                  T := R
                else
                  T := (R + 3) div 2;;
              end;
           5: T := Random(2) + 1;
      end;
      // Get random tense group (from those possible and selected)
      Temps := '';
      R := Random(4);
      if (R = 0) and Comp then                                                 // 25% "temps composés"
        Temps := TempsComposes[M, T]
      else if (R > 0) and Simp then
        Temps := TempsSimples[M, T];
    until (Temps <> '') and (Temps <> '-');
    if (Voix <> 'actif') and (Mode = 'impératif') and (Temps = 'passé') then   // "impératif" only exists for "voix active"
      OK := False;
    if (Voix = 'pronominal') and (Mode = 'participe') and (Temps = 'passé') then  // "participe passé" doesn't exist for "voix pronominale"
      OK := False;
  until OK;
  Forme := Formes[1];                                                          // "forme" not implemented in this version
  Nombre := '-'; Personne := '-'; Genre := 'masculin';
  // Get random number (from those possible)
  if (Mode = 'indicatif') or (Mode = 'subjonctif') or (Mode = 'impératif') then begin
    if Verbe.VerbeClass = 'I' then
      // "Verbes impersonnels": "nombre" = "singulier"
      Nombre := Nombres[1]
    else begin
      // Other verbs: "nombre" = random
      R := Random(2) + 1; Nombre := Nombres[R];
    end;
    // Get random person (from those possible)
    if Mode = 'impératif' then begin
      // "Impératif": some persons do not exist
      if Nombre = 'singulier' then
        Personne := Personnes[2]
      else begin
        R := Random(2) + 1; Personne := Personnes[R];
      end;
    end
    else begin
      // Other modes
      if Verbe.VerbeClass = 'I' then
        // "Verbes impersonnels": "personne" = "3e personne"
        Personne := Personnes[3]
      else begin
        // Other verbs: "personne" = random
        R := Random(3) + 1; Personne := Personnes[R];
      end;
    end;
  end;
  // Get random genre (from those possible and selected)
  if Verbe.VerbeClass = 'I' then
    // "Verbes impersonnels": "genre" = "masculin"
    Genre := Genres[1]
  else begin
    // Other verbs: genre = random
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
  if (Mode <> 'indicatif') or (Temps = 'présent') or not fConjER.mOptionsDisplayIndicatif.Checked then
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
  ExAnswer := Conjugaison(Verbe, VerbeType, Voix, Mode, Temps, Forme, Nombre, Personne, Genre, Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
  ExAnswer := StringReplace(ExAnswer, '--', '', [rfReplaceAll]);               // remove the verb part seperators
end;

{ Choose random verb (from those conjugation groups selected by the user) and generate random exercise question }

procedure ExerciseRandomQuestion(Verbs: Integer; var Verbes: TVerbes; Conjugaison: string; VerbeType: TVerbe; Actif, Passif, Pronominal,
  Indicatif, Subjonctif, MOthers, TSimples, TComposes, Masculin, Feminin, Reforme1990, KeepY: Boolean; var  Verbe: TVerbe; var Question, Answer: string);

var
  R, P: Integer;
  VerbeNom, Verb, Term1, Term2: string;
  Pass, Pron, VerbeOK: Boolean;

begin
  // Repeat picking a random verb until you got one which is actually part of the conjugation group selected by the user
  repeat
    Pass := Passif; Pron := Pronominal; VerbeOK := False;
    // Mixed groups of -er verbs
    if LeftStr(Conjugaison, 3) = 'er_' then begin
      R := Random(Verbs) + 1;
      // Any verb...
      if Conjugaison = 'er_all' then
        VerbeOK := True
      // Regular -er verbs
      else if (LeftStr(Conjugaison, 9) = 'er_allreg') and ((Verbes[R].Conjugaison = '-') or (Verbes[R].Conjugaison = 'T')) then
        VerbeOK := True
      else if (LeftStr(Conjugaison, 8) = 'er_noreg') and ((Verbes[R].Conjugaison = '-') or (Verbes[R].Conjugaison = 'T')) then
        VerbeOK := False;
      if not VerbeOK then begin
        // Any irregular -er verb
        P := Pos('allirr', Conjugaison);
        if (P <> 0) and (UpperCase(Verbes[R].Conjugaison) = 'I') then
          VerbeOK := True;
        P := Pos('noirr', Conjugaison);
        if (P <> 0) and (UpperCase(Verbes[R].Conjugaison) = 'I') then
          VerbeOK := False;
      end;
      if not VerbeOK then begin
        // Any exception -er verb
        P := Pos('allspec', Conjugaison);
        if (P <> 0) and (Verbes[R].Conjugaison = 'S') then
          VerbeOK := True;
        P := Pos('nospec', Conjugaison);
        if (P <> 0) and (Verbes[R].Conjugaison = 'S') then
          VerbeOK := False;
      end;
      Verb := Verbes[R].VerbeNom;
      if not VerbeOK then begin
        // One particular irregular -er verb
        P := Pos('someirr', Conjugaison);
        if P <> 0 then begin
          if fSelectEx.cbAller.Checked and ((Verb = 'aller') or (Verb = 'en aller') or (Verb = 'y aller')) then
            VerbeOK := True
          else if fSelectEx.cbEnvoyer.Checked and ((Verb = 'envoyer') or (Verb = 'renvoyer')) then
            VerbeOK := True
        end;
      end;
      if not VerbeOK then begin
        // One particular group of exception -er verbs
        P := Pos('somespec', Conjugaison);
        if P <> 0 then begin
          if fSelectEx.cbCER.Checked and (RightStr(Verb, 3) = 'cer') then
            VerbeOK := True;
          if fSelectEx.cbGER.Checked and (RightStr(Verb, 3) = 'ger') then
            VerbeOK := True;
          if fSelectEx.cbELER.Checked and ((RightStr(Verb, 4) = 'eler') or (RightStr(Verb, 4) = 'eter'))then
            VerbeOK := True;
          if fSelectEx.cbAYER.Checked and ((RightStr(Verb, 4) = 'ayer') or (RightStr(Verb, 4) = 'oyer') or (RightStr(Verb, 4) = 'uyer')) then begin
            if (Verb = 'envoyer') or (Verb = 'renvoyer') then
              VerbeOK := False
            else
              VerbeOK := True;
          end;
          Term1 := 'e' + RightStr(Verb, 3); Term2 := 'é' + RightStr(Verb, 3);
          if fSelectEx.cbE_ER.Checked and ((RightStr(Verb, 4) = Term1) or (UTF8Copy(Verb, UTF8Length(Verb) - 3, 4) = Term2)) then begin
            VerbeOK := True;
            if (Term1 = 'ecer') or (Term2 = 'écer') then
              VerbeOK := False
            else if (Term1 = 'eger') or (Term2 = 'éger') then
              VerbeOK := False
            else if (Term1 = 'eter') or (Term1 = 'eler') then
              VerbeOK := False
          end;
        end;
      end;
      if VerbeOK then
        // Verb found!
        VerbeNom := Verbes[R].VerbeNom;
    end
    // Individual verb selections: different % for aller/y aller/s'en aller resp. envoyer/renvoyer
    else begin
      VerbeNom := Conjugaison; VerbeOK := True;
      R := Random(5);
      if (R = 2) or (R = 3) then begin
        if VerbeNom = 'aller' then
          VerbeNom := 'en aller'
        else if VerbeNom = 'envoyer' then
          VerbeNom := 'renvoyer';
      end
      else if R = 4 then begin
        if VerbeNom = 'aller' then
          VerbeNom := 'y aller'
      end;
    end;
    // Check if the random verb found may effectively be conjugated with the actual conjugation parameters
    if VerbeOK then begin
      GetVerbe(VerbeNom, Verbes, Verbs, Verbe);                                // get verb parameters (from -er verb list)
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
  ChooseVerbFile(Verbe, VerbeType);                                            // define the verb-file to be used for -er verbs
  Pass := Passif; Pron := Pronominal;
  if not Verbe.VoixPassive then                                                // be sure not trying to use "voix passive" if verb hasn't one
    Pass := False;
  if not Verbe.VoixPronominale then                                            // be sure not trying to use "voix pronominale" if verb hasn't one
    Pron := False;
  ExerciseGenerate(Verbe, VerbeType,
    Actif, Pass, Pron, Indicatif, Subjonctif, MOthers, TSimples, TComposes, Masculin, Feminin, Reforme1990, KeepY, Question, Answer);
end;

{ Show help text (after loading it from text file ) }

procedure ShowHelpText(FileName: string);

begin
  if FileName = 'conj' then
    fHelp.stTitle.Caption := 'Grammaire française: Conjugaison.'
  else if FileName = 'conjER' then
    fHelp.stTitle.Caption := 'Conjugaison des verbes en -er.'
  else
    fHelp.stTitle.Caption := 'ConjugaisonER aide programme.';
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
  if fConjER.bLetters then begin
    S := fConjER.edAnswer.Text;
    S += Letters[IX];
    fConjER.edAnswer.Text := S;
  end;
end;

{************}
{* TfConjER *}
{************}

{ Application start: Initialisation }

procedure TfConjER.FormCreate(Sender: TObject);

begin
  ReadVerbs(Verbes, iVerbs);                                                   // read -er verblist from file
  GetVerbeType(Verbes, iVerbs, rcVerbeType);                                   // get "verbe type -er" data
  AvoirFile := TINIFile.Create('avoir.txt');                                   // open "avoir" and "être" conjugation files
  EtreFile  := TINIFile.Create('être.txt');
  OldVerbFilename := '';
  iQuestions := 20;                                                            // initial number of questions
  sConjugaison := 'er_all';                                                    // all -er verbs in exercises
  bActif := True; bPassif := False; bPronominal := False;                      // other initial exercise settings
  bIndicatif := True; bSubjonctif := True; bMOthers := True;
  bTSimples := True; bTComposes := True;
  bMasculin := True; bFeminin := True;
  bReforme1990 := True; bKeepY := False;                                       // initial conjugation options settings
  Randomize;                                                                   // start of random generator
end;

{ Form activation: Clear form fields }

procedure TfConjER.FormActivate(Sender: TObject);

begin
  ExerciseReset(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Conjugaison > Exercices Avoir": Select "avoir" conjugation as exercises }

procedure TfConjER.mConjAvoirClick(Sender: TObject);

begin
  iCorrect := 0; iFalse := 0; iQuestion := 0;
  sConjugaison := 'avoir';
  bActif := True; bPassif := False; bPronominal := False;
  stTitle.Caption := 'Conjugaison: Verbe AVOIR';
  ExerciseReset(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Conjugaison > Exercices Etre": Select "être" conjugation as exercises }

procedure TfConjER.mConjEtreClick(Sender: TObject);

begin
  iCorrect := 0; iFalse := 0; iQuestion := 0;
  sConjugaison := 'être';
  bActif := True; bPassif := False; bPronominal := False;
  stTitle.Caption := 'Conjugaison: Verbe ETRE';
  ExerciseReset(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Conjugaison > Exercices verbes -er": Select "verbes -er" conjugation as exercises }

procedure TfConjER.mConjERClick(Sender: TObject);

begin
  iCorrect := 0; iFalse := 0; iQuestion := 0;
  // Open form for user to select verbs and conjugation options
  fSelectEx.ShowModal;
  // Get the "conjugation string" (verbs to include) for user selection
  sConjugaison := GetConjugaison;
  // Set wanted voice(s), mode(s), tense(s) and sexe(s)
  bActif  := fSelectEx.cbActif.Checked;   bPassif := fSelectEx.cbPassif.Checked;   bPronominal := fSelectEx.cbPron.Checked;
  bIndicatif  := fSelectEx.cbInd.Checked; bSubjonctif := fSelectEx.cbSubj.Checked; bMOthers := fSelectEx.cbMOthers.Checked;
  bTSimples := fSelectEx.cbTSimp.Checked; bTComposes := fSelectEx.cbTComp.Checked;
  bMasculin := fSelectEx.cbMasc.Checked;  bFeminin := fSelectEx.cbFem.Checked;
  // Set title
  if sConjugaison = 'aller' then
    stTitle.Caption := 'Conjugaison: Verbe ALLER'
  else if sConjugaison = 'envoyer' then
    stTitle.Caption := 'Conjugaison: Verbe ENVOYER'
  else
    stTitle.Caption := 'Conjugaison: Verbes en -ER';
  ExerciseReset(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Fichier exercices": Generate exercises files }

procedure TfConjER.mConjFileClick(Sender: TObject);

var
  I: Integer;
  Dir, FileName, FileExt, FileName1, FileName2: string;

begin
  iQuestions := StrToInt(edQuestions.Text);
  memoQuestions.Clear; memoAnswers.Clear;
  // Exercise questions are generated, using the verbs and conjugation options actually selected
  for I := 1 to iQuestions do begin
    // Generate an exercise question
    ExerciseRandomQuestion(iVerbs, Verbes, sConjugaison, rcVerbeType,
      bActif, bPassif, bPronominal, bIndicatif, bSubjonctif, bMOthers, bTSimples, bTComposes,
      bMasculin, bFeminin, bReforme1990, bKeepY, rcVerbe, sQuestion, sAnswer);
    // Append question and answer to corresponding Memo (invisible Memo, just use for storage)
    memoQuestions.Append(sQuestion);
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
    FileName1 := Dir + FileName + ' (Questions)' + FileExt;                    // questions file
    FileName2 := Dir + FileName + ' (Réponses)' + FileExt;                     // questions + answers file
    memoQuestions.Lines.SaveToFile(FileName1);
    memoAnswers.Lines.SaveToFile(FileName2);
  end;
end;

{ Menu item "Conjugaison > Affichage conjugaison": Do conjugation for given verb }

procedure TfConjER.mConjConjClick(Sender: TObject);

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
    // Check if the the verb entered is avoir, être or a verb ending in -er
    if (VerbeNom <> 'avoir') and (VerbeNom <> 'être') and (RightStr(VerbeNom, 2) <> 'er') then begin
      MessageDlg('Verbe invalide', 'Ce programme ne sait conjuguer que les verbes en -er!', mtError, [mbOK], 0);
      OK := False;
    end
    // User entered a verb handled by this program
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
      GetVerbe(VerbeNom, Verbes, iVerbs, rcVerbe);
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
      fConj.bReforme1990 := bReforme1990;
      fConj.bKeepY := bKeepY;
      fConj.bSepTerm := mOptionsDisplaySep.Checked;
      fConj.bSepRacine := mOptionsDisplaySepRacine.Checked;;
      fConj.bColorIrreg := mOptionsDisplayIrreg.Checked;
      fConj.bColorExcept := mOptionsDisplayExcept.Checked;
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

procedure TfConjER.mConjExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Affichage > Ne pas afficher 'Indicatif'": Choose display option for mode = 'Indicatif' }

procedure TfConjER.mOptionsDisplayIndicatifClick(Sender: TObject);

begin
  if mOptionsDisplayIndicatif.Checked then
    mOptionsDisplayIndicatif.Checked := False
  else
    mOptionsDisplayIndicatif.Checked := True;
end;

{ Menu item "Options > Affichage > Séparation entre le radical et la terminaison": Choose display option for 'terminaison' separator }

procedure TfConjER.mOptionsDisplaySepClick(Sender: TObject);

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

procedure TfConjER.mOptionsDisplaySepRacineClick(Sender: TObject);

begin
  if mOptionsDisplaySepRacine.Checked then
    mOptionsDisplaySepRacine.Checked := False
  else
    mOptionsDisplaySepRacine.Checked := True;
end;

{ Menu item "Options > Affichage > Afficher les irrégularités en couleur": Choose display option for irregular verbs }

procedure TfConjER.mOptionsDisplayIrregClick(Sender: TObject);

begin
  if mOptionsDisplayIrreg.Checked then
    mOptionsDisplayIrreg.Checked := False
  else
    mOptionsDisplayIrreg.Checked := True;
end;

{ Menu item "Options > Affichage > Afficher les irrégularités en couleur": Choose display option for 'special' verbs }

procedure TfConjER.mOptionsDisplayExceptClick(Sender: TObject);

begin
  if mOptionsDisplayExcept.Checked then
    mOptionsDisplayExcept.Checked := False
  else
    mOptionsDisplayExcept.Checked := True;
end;

{ Menu item "Options > Conjugaison > -eler/eter: selon la réforme 1990": Choose how to conjugate verbs in -eler/-eter }

procedure TfConjER.mOptionsConjKeepYClick(Sender: TObject);

begin
  if mOptionsConjKeepY.Checked then
    mOptionsConjKeepY.Checked := False
  else
    mOptionsConjKeepY.Checked := True;
  bKeepY := mOptionsConjKeepY.Checked;
end;

{ Menu item "Options > Conjugaison > -ayer: garder le y dans tous les cas": Choose how to conjugate verbs in -ayer }

procedure TfConjER.mOptionsConjReformeClick(Sender: TObject);

begin
  if mOptionsConjReforme.Checked then
    mOptionsConjReforme.Checked := False
  else
    mOptionsConjReforme.Checked := True;
  bReforme1990 := mOptionsConjReforme.Checked;
end;

{ Menu item "Aide > Conjugaison": Display "conjugation" help }

procedure TfConjER.mHelpConjClick(Sender: TObject);

begin
  ShowHelpText('conj');
end;

{ Menu item "Aide > Verbes en -er": Display "verbs in -er" help }

procedure TfConjER.mHelpConjERClick(Sender: TObject);

begin
  ShowHelpText('conjER');
end;

{ Menu item "Aide > Aide programme": Display application help }

procedure TfConjER.mHelpHelpClick(Sender: TObject);

begin
  ShowHelpText('help');
end;

{ Menu item "Aide > Info programme": Display application about }

procedure TfConjER.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Conjugaison des verbes français:' + Chr(13);
  S += 'Conjugaison des verbes en -er.' + Chr(13) + Chr(13);
  S += 'Version 1.1, © allu, mai 2018 - avril 2019.';
  MessageDlg('Grammaire française', S, mtInformation, [mbOK], 0);
end;

{ Button "Question": Generate an exercise with parameters as selected }

procedure TfConjER.btQuestionClick(Sender: TObject);

begin
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
  // If there are still exercises to do...
  if iQuestion <= iQuestions then begin
    laExercice.Caption := 'Exercice ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    ExerciseRandomQuestion(iVerbs, Verbes, sConjugaison, rcVerbeType, bActif, bPassif, bPronominal, bIndicatif, bSubjonctif, bMOthers,
      bTSimples, bTComposes, bMasculin, bFeminin, bReforme1990, bKeepY, rcVerbe, sQuestion, sAnswer);
    edExercise.Text := sQuestion;
    edAnswer.Text := '';
    edEvaluation.Text := '';
    btAnswer.Enabled := True;
    edAnswer.SetFocus;
    bLetters := True;
  end
  // If the number of exercises to do has been reached...
  else
    EndOfExercise;
end;

{ Button "Réponse": Check user's answer }

procedure TfConjER.btAnswerClick(Sender: TObject);

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
  // If there are still exercises to do...
  if iQuestion < iQuestions then
    btAnswer.Enabled := False
  // If the number of exercises to do has been reached...
  else
    EndOfExercise;
end;

{ Non-English letters buttons: Insert the corresponding letter into the user's answer string }

procedure TfConjER.btLetter1Click(Sender: TObject);

begin
  InsertLetter(1);
end;

procedure TfConjER.btLetter2Click(Sender: TObject);

begin
  InsertLetter(2);
end;

procedure TfConjER.btLetter3Click(Sender: TObject);

begin
  InsertLetter(3);
end;

procedure TfConjER.btLetter4Click(Sender: TObject);

begin
  InsertLetter(4);
end;

procedure TfConjER.btLetter5Click(Sender: TObject);

begin
  InsertLetter(5);
end;

procedure TfConjER.btLetter6Click(Sender: TObject);

begin
  InsertLetter(6);
end;

procedure TfConjER.btLetter7Click(Sender: TObject);

begin
  InsertLetter(7);
end;

procedure TfConjER.btLetter8Click(Sender: TObject);

begin
  InsertLetter(8);
end;

procedure TfConjER.btLetter9Click(Sender: TObject);

begin
  InsertLetter(9);
end;

procedure TfConjER.btLetter10Click(Sender: TObject);

begin
  InsertLetter(10);
end;

procedure TfConjER.btLetter11Click(Sender: TObject);

begin
  InsertLetter(11);
end;

end.

