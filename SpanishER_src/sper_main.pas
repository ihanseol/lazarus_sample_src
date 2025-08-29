{***************************************}
{* Main unit for SpanishER application *}
{***************************************}

unit sper_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  IniFiles, LazUTF8, sper_conj, sper_ex, sper_help, conjer;

type
  {*************}
  { TfSpanishER }
  {*************}
  TfSpanishER = class(TForm)
    mMenu: TMainMenu;
    mConj, mConjExercise, mConjConj, mConjExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsUseImpPron: TMenuItem;
    mOptionsDisplayConj, mOptionsDisplayConjSep, mOptionsDisplayConjSep2, mOptionsDisplayConjColorIrreg, mOptionsDisplayConjColorSame: TMenuItem;
    mOptionsDisplayEx, mOptionsDisplayExNoInd, mOptionsDisplayExNoAff: TMenuItem;
    mHelp, mHelpGrammar, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Label2, Label3, Label4, Label5: TLabel;
    laQuestion: TLabel;
    edExercise, edAnswer, edEvaluation: TEdit;
    edQuestion, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAnswer: TButton;
    btLetter1, btLetter2, btLetter3, btLetter4, btLetter5, btLetter6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mConjExerciseClick(Sender: TObject);
    procedure mConjConjClick(Sender: TObject);
    procedure mConjExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsDisplayConjSepClick(Sender: TObject);
    procedure mOptionsDisplayConjSep2Click(Sender: TObject);
    procedure mOptionsDisplayConjColorIrregClick(Sender: TObject);
    procedure mOptionsDisplayConjColorSameClick(Sender: TObject);
    procedure mOptionsDisplayExNoIndClick(Sender: TObject);
    procedure mOptionsDisplayExNoAffClick(Sender: TObject);
    procedure mOptionsUseImpPronClick(Sender: TObject);
    procedure mHelpGrammarClick(Sender: TObject);
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
  private
    iVerbs, iQuestions, iQuestionsTemp, iCorrect, iFalse, iQuestion: Integer;
    sConjugation, sQuestion, sAnswer, sUserAnswer: string;
    bIndicative, bSubjunctive, bImperative, bParticiple: Boolean;
    bAffirmative, bNegative, bMasculine, bFeminine, bImperativePronoun: Boolean;
    aVerbs: TVerbs;
    rcVerb, rcVerbType: TVerb;
  end;

var
  fSpanishER: TfSpanishER;
  VerbFile: TINIFile;
  OldVerbFilename: string;

implementation

{$R *.lfm}

{ Read verbs from text file }

procedure ReadVerbs(out Verbs: TVerbs; out NVerbs: Integer);

var
  Line: string;
  VFile: Text;

begin
  Assign(VFile, 'verbs.txt'); Reset(VFile);
  NVerbs := 0;
  while not EOF(VFile) and (NVerbs < NVerbsMax) do begin
    Inc(NVerbs);
    Readln(VFile, Line);
    if Line <> '' then begin
      with Verbs[NVerbs - 1] do begin
        // Read verb name (cols 1-15)
        VerbName := UTF8Trim(UTF8Copy(Line, 1, 15));
        // Read verb conjugation type (col 16)
        Conjugation := UTF8Copy(Line, 16, 1)[1];
        // Read English translation (col 18-)
        VerbNamesEng := UTF8Trim(UTF8Copy(Line, 18, UTF8Length(Line)));
      end;
    end;
  end;
end;

{ Get verb (full record) of given verb name by looking up the list read from file }

procedure GetVerb(VerbName: string; var Verbs: TVerbs; NVerbs: Integer; out Verb: TVerb);

var
  I, IX: Integer;

begin
  I := 0; IX := -1;
  repeat
    Inc(I);
    if VerbName = Verbs[I - 1].VerbName then
      IX := I - 1;                                                             // verb index in list
  until (IX <> -1) or (I = NVerbs);
  // Verb not found: set verb to 'unknown conjugation'
  if IX = - 1 then begin
    Verb := VerbUnknown;
    Verb.VerbName := VerbName;                                                 // fill-in the verb name
  end
  // Verb found in list: fill in record variables
  else begin
    with Verb do begin
      VerbName     := Verbs[IX].VerbName;
      Conjugation  := Verbs[IX].Conjugation;
      VerbNamesEng := Verbs[IX].VerbNamesEng;
    end;
  end;
end;

{ Get "verb type" for given verb }

procedure GetVerbType(var Verbs: TVerbs; NVerbs: Integer; Verb: TVerb; out VerbType: TVerb);

var
  I: Integer;
  VerbName: string;

begin
  VerbName := '';
  if (Verb.Conjugation = 'T') or (Verb.Conjugation = 'R') then begin
    // Regular verbs
    VerbName := 'vender';
  end
  else if Verb.Conjugation = 'I' then begin
    // Irregular verbs with own verb file
    VerbName := Verb.VerbName;
  end
  else if Verb.Conjugation = 'i' then begin
    // Irregular verbs without own verb file
    // These verbs are conjugated the same way as some other irregular verb
    // They are mostly compounds of this other irregular verb
    if UTF8Copy(Verb.VerbName, Length(Verb.VerbName) - 2, 3) = 'cer' then
      VerbName := 'hacer'
    else if UTF8Copy(Verb.VerbName, Length(Verb.VerbName) - 4, 5) = 'tener' then
      VerbName := 'tener'
    else if UTF8Copy(Verb.VerbName, Length(Verb.VerbName) - 4, 5) = 'poner' then
      VerbName := 'poner'
    else if UTF8Copy(Verb.VerbName, Length(Verb.VerbName) - 4, 5) = 'traer' then
      VerbName := 'traer'
    else if UTF8Copy(Verb.VerbName, Length(Verb.VerbName) - 3, 4) = 'caer' then
      VerbName := 'caer';
  end
  else if Verb.Conjugation = 'S' then begin
    // Semi-irregular verb type (with own verb file)
    VerbName := Verb.VerbName;
  end
  else if Verb.Conjugation = 's' then begin
    // Semi-irregular verb (without own verb file)
    // These verbs are conjugated the same way as the corresponding semi-irregular verb-type
    if UTF8Copy(Verb.VerbName, Length(Verb.VerbName) - 2, 3) = 'eer' then begin
      // Verbs in -eer: stem change "i" to "y" (in some cases)
      VerbName := 'leer';
    end
    else begin
      I := Length(Verb.VerbName) - 2;
      while (VerbName = '') and (I >= 1) do begin
        if UTF8Copy(Verb.VerbName, I, 1) = 'e' then begin
          // Verbs with stem vowal = "e": stem change "e" to "ie" (in some cases)
          VerbName := 'perder';
        end
        else if UTF8Copy(Verb.VerbName, I, 1) = 'o' then begin
          // Verbs with stem vowal = "o": stem change "o" to "ue" (in some cases)
          VerbName := 'mover';
        end;
        Dec(I);
      end;
    end;
  end;
  if VerbName = '' then
    VerbType := VerbUnknown
  else
    GetVerb(VerbName, Verbs, NVerbs, VerbType);
end;

{ Choose file to be used as base for conjugation of actual -er verb-type }

procedure ChooseVerbFile(VerbType: TVerb);

var
  VerbFilename: string;

begin
  VerbFilename := VerbType.VerbName;
  VerbFilename := './verbs/' + VerbFilename + '.ini'; DoDirSeparators(VerbFilename);
  // Create the INI file object (unless it already exits)
  if VerbFilename <> OldVerbFilename then begin
    if OldVerbFilename <> '' then
      // If there is another file object active, get rid of it
      VerbFile.Free;
    VerbFile := TINIFile.Create(VerbFilename);
    OldVerbFilename := VerbFilename;
  end;
end;

{ Determine the exercise-conjugation-type (based on user selections on the "fSelectEx" form) }

function GetConjugation(Regular, Irregular, SemiIrregular: Boolean): string;

var
  Conj: string;

// The function returns a string that is decoded in order to determine if a given random verb
// is part of those, the user wants to do exercises with

begin
  Conj := '';
  if Regular and Irregular and SemiIrregular then
    // All verbs
    Conj := 'er_all'
  else begin
    if Regular then
      // Regular verbs
      Conj := 'er_regall+';
    if SemiIrregular then
      // Semi-irregular verbs
      Conj += 'er_semirregall+';
    if Irregular then
      // Irregular verbs
      Conj += 'er_irregall';
    if RightStr(conj, 1) = '+' then
      Delete(Conj, Length(Conj), 1);
  end;
  Result := Conj;
end;

{ Get short display form of number, person and gender }

function ShortNPG(Number, Person, Gender: string): string;

var
  NPG: string;

begin
  NPG := '';
  if (Person <> '-') and (Number <> '-') then begin
    NPG := Person + ' ' + Number;
    if Gender = 'feminine' then
      NPG += ', ' + Gender;
  end
  else begin
    if Gender = 'feminine' then
      NPG := Gender;
  end;
  NPG := StringReplace(NPG, 'person', 'pers', []);
  NPG := StringReplace(NPG, 'singular', 'sg', []);
  NPG := StringReplace(NPG, 'plural', 'pl', []);
  NPG := StringReplace(NPG, 'feminine', 'fem', []);
  Result := NPG;
end;

{ Clear exercise form fields and reset variables }

procedure ExerciseReset(QuestionsTemp: Integer; out Questions, Question, AnswCorrect, AnswFalse: Integer);

begin
  Questions := QuestionsTemp;                                                  // number of questions entered by user becomes active now
  Question := 0; AnswCorrect := 0; AnswFalse := 0;
  fSpanishER.laQuestion.Caption := 'Question:';
  fSpanishER.edExercise.Text := ''; fSpanishER.edAnswer.Text := '';
  fSpanishER.edEvaluation.Text := ''; fSpanishER.edEvaluation.Font.Color := clDefault;
  fSpanishER.edQuestion.Text := ''; fSpanishER.edCorrect.Text := ''; fSpanishER.edFalse.Text := '';
  fSpanishER.edSuccess.Text := ''; fSpanishER.edSuccess.Color := clDefault;
  fSpanishER.btQuestion.Enabled := True; fSpanishER.btAnswer.Enabled := False;
end;

{ Display success percentage }

procedure DisplaySuccess(Q, C: Integer);

var
  P: Real;

begin
  P := 100 * (C / Q);
  P := Int(100 * P) / 100;
  fSpanishER.edSuccess.Text := FloatToStr(P) + '%';
  if P < 50 then
    fSpanishER.edSuccess.Color := clRed
  else if P < 60 then
    fSpanishER.edSuccess.Color := clYellow
  else
    fSpanishER.edSuccess.Color := clLime;
end;

{ End of test message }

procedure EndOfExercise;

begin
  fSpanishER.btQuestion.Enabled := False; fSpanishER.btAnswer.Enabled := False;
  MessageDlg('End of exercise', 'All exercise questions have been done.', mtInformation, [mbOK], 0);
end;

{ Generate exercise with given random verb}

procedure ExerciseGenerate(Verb, VerbType: TVerb; Ind, Subj, Imp, Part, Masc, Fem, Aff, Neg, ImpPron: Boolean; var ExQuestion, ExAnswer: string);

var
  M, T, R: Integer;
  VerbName, Mood, Tense, VForm, Number, Person, Gender, NPG: string;
  OK: Boolean;

begin
  // Get random mood (from those selected)
  repeat
    M := -1;
    R := Random(21);
    case R of
           0: if Part then M := 2;
        1..3: if Imp then M := 3;
       4..12: if Ind then M := 4;
      13..20: if Subj then M := 5;
    end;
  until M > 0;
  Mood := Moods[M];
  // Get random tense (from those possible for given mode)
  case M of
    2: T := Random(2) + 1;
    3: T := 1;
    4: T := Random(5) + 1;
    5: T := Random(2) + 1;
  end;
  Tense := Tenses[M, T];
  // Get random number and person (from those possible for given tense)
  Number := '-'; Person := '-';
  if Mood <> 'participle' then begin
    // Random number
    R := Random(2) + 1; Number := Numbers[R];
    // Random person (from those possible)
    repeat
      OK := True;
      R := Random(3) + 1;
      if (Mood = 'imperative') and (Number = 'singular') and (R = 1) then
        OK := False
      else
        Person := Persons[R];
    until OK;
  end;
  // Get random form (from those selected)
  if Aff and Neg then
    VForm := VForms[Random(2) + 1]
  else if Aff then
    VForm := VForms[1]
  else
    VForm := VForms[2];
  // Get random gender (from those selected)
  repeat
    Gender := '';
    R := Random(2) + 1;
    if ((R = 1) and Masc) or ((R = 2) and Fem) then
      Gender := Genders[R];
  until Gender <> '';
  // Create the question text
  VerbName := Verb.VerbName;
  ExQuestion := VerbName + ',';
  // Display mood
  // Do not display a mood for imperative and participle
  // Omit "indicative" if this option is selected
  if ((Mood = 'indicative') and not fSpanishER.mOptionsDisplayExNoInd.Checked) or (Mood = 'subjunctive') then begin
    ExQuestion += ' ' + Mood;
  end;
  // Display tense
  ExQuestion += ' ' + Tense;
  // Display number, person and gender (short form)
  NPG := ShortNPG(Number, Person, Gender);
  if NPG <> '' then
    ExQuestion += ', ' + NPG;
  // Display form (omit affirmative if this option is selected)
  if (VForm <> 'affirmative') or not fSpanishER.mOptionsDisplayExNoAff.Checked then begin
    ExQuestion += ' (' + VForm + ')';
  end;
  // Do the conjugation (with actual parameters)
  ExAnswer := Conjugation(Verb, VerbType, Mood, Tense, VForm, Number, Person, Gender, ImpPron, VerbFile);
  ExAnswer := StringReplace(ExAnswer, '-', '', [rfReplaceAll]);                // remove the verb-part seperators
  ExAnswer := StringReplace(ExAnswer, '$', '', []);                            // remove the irregularity tags
  ExAnswer := StringReplace(ExAnswer, '#', '', []);
  ExAnswer := StringReplace(ExAnswer, '*', '', []);
  ExAnswer := StringReplace(ExAnswer, '+', '', []);
end;

{ Choose random verb (from those conjugation groups selected by the user) and generate random exercise question }

procedure ExerciseRandomQuestion(NVerbs: Integer; var Verbs: TVerbs; Conjugation: string;
  Ind, Subj, Imp, Part, Masc, Fem, Aff, Neg, ImpPron: Boolean; var  Verb: TVerb; var Question, Answer: string);

var
  R, P1, P2: Integer;
  VerbOK: Boolean;
  VerbType: TVerb;

begin
  // Repeat picking a random verb until you get one which is actually part of the conjugation group selected by the user
  repeat
    VerbOK := False;
    R := Random(NVerbs);
    if Conjugation = 'er_all' then
      // Any -er verb...
      VerbOK := True
    else begin
      // Verb groups...
      P1 := Pos('er_semirregall', Conjugation); P2 := Pos('er_irregall', Conjugation);
      if (LeftStr(Conjugation, 9) = 'er_regall') and (Verbs[R].Conjugation = 'R') then
        // Regular -er verbs
        VerbOK := True;
      if (P1 <> 0) and (UpperCase(Verbs[R].Conjugation) = 'S') then
        // Semi-irregular -er verb
          VerbOK := True;
      if (P2 <> 0) and (UpperCase(Verbs[R].Conjugation) = 'I') then
        // Irregular -er verbs
        VerbOK := True;
    end;
  until VerbOK;
  Verb := Verbs[R];
  // Generate the exercise
  GetVerbType(Verbs, NVerbs, Verb, VerbType);
  ChooseVerbFile(VerbType);                                                    // determine the verb file to be used for actual verb-type
  ExerciseGenerate(Verb, VerbType, Ind, Subj, Imp, Part, Masc, Fem, Aff, Neg, ImpPron, Question, Answer);
end;

{ Show help text (after loading it from text file) }

procedure ShowHelpText(FileName: string);

begin
  if FileName = 'conj' then
    fHelp.stTitle.Caption := 'Spanish grammar: Conjugation of verbs in -er.'
  else
    fHelp.stTitle.Caption := '"SpanishER" application help.';
  FileName += '.txt';
  fHelp.memoHelp.Lines.Clear;
  fHelp.memoHelp.Lines.LoadFromFile(FileName);
  fHelp.Show;
end;

{ Insert non-ANSI letter corresponding to letter-button pushed by user }

procedure InsertLetter(IX: Integer);

const
  Letters: array[1..6] of string = (
    'á', 'é', 'í', 'ó', 'ú', 'ñ'
  );

var
  S: string;

begin
  S := fSpanishER.edAnswer.Text;
  S += Letters[IX];
  fSpanishER.edAnswer.Text := S;
end;

{*************}
{ TfSpanishER }
{*************}

{ Application start: Initialisation }

procedure TfSpanishER.FormCreate(Sender: TObject);

begin
  ReadVerbs(aVerbs, iVerbs);                                                   // read -er verblist from file
  OldVerbFilename := '';
  iQuestionsTemp := 20;                                                        // default initial number of questions
  sConjugation := 'er_all';                                                    // all -er verbs in exercises
  bIndicative := True; bSubjunctive := True;                                   // default mood, tense, etc in exercises
  bImperative := False; bParticiple := False;
  bAffirmative := True; bNegative := False;
  bMasculine := True; bFeminine := False;
  bImperativePronoun := False;                                                 // don't use pronouns with imperative by default
  Randomize;
end;

{ Form activation: Clear form fields }

procedure TfSpanishER.FormActivate(Sender: TObject);

begin
  ExerciseReset(iQuestionsTemp, iQuestions, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Conjugation > New exercise": Start new exercise }

procedure TfSpanishER.mConjExerciseClick(Sender: TObject);

var
  Regular, Irregular, SemiIrregular: Boolean;

begin
  // Open "fSelectEx" form for user to select verbs and conjugation options
  fSelectEx.ShowModal;
  // Get the "conjugation string" (verbs to include in exercises)
  Regular := fSelectEx.cbRegular.Checked;
  Irregular := fSelectEx.cbIrregular.Checked;
  SemiIrregular := fSelectEx.cbSemiIrregular.Checked;
  sConjugation := GetConjugation(Regular, Irregular, SemiIrregular);
  // Set wanted mode(s), tense(s), form(s) and sexe(s)
  bIndicative  := fSelectEx.cbInd.Checked; bSubjunctive := fSelectEx.cbSubj.Checked;
  bImperative := fSelectEx.cbImp.Checked; bParticiple := fSelectEx.cbPart.Checked;
  bAffirmative := fSelectEx.cbAff.Checked; bNegative := fSelectEx.cbNeg.Checked;
  bMasculine := fSelectEx.cbMasc.Checked;  bFeminine := fSelectEx.cbFem.Checked;
  // Clear form fields and reset evaluation counters
  ExerciseReset(iQuestionsTemp, iQuestions, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Conjugation > Conjugation table": Display conjugation table for verb entered by user }

procedure TfSpanishER.mConjConjClick(Sender: TObject);

var
  VerbName: string;
  OK: Boolean;

begin
  VerbName := InputBox('Spanish grammar', 'Enter verb to be conjugated', '');
  if VerbName <> '' then begin
    OK := True;
    if RightStr(VerbName, 2) <> 'er' then begin
      MessageDlg('Invalid verb', 'This application only conjuagtes verbs in -er!', mtError, [mbOK], 0);
      OK := False;
    end
    else begin
      GetVerb(VerbName, aVerbs, iVerbs, rcVerb);
      if rcVerb.Conjugation = '?' then begin
        MessageDlg('Unknown verb', 'This application only conjuagtes the 100 verbs in the list!', mtError, [mbOK], 0);
        OK := False;
      end;
    end;
    if OK then begin
      // Get verb-type
      GetVerbType(aVerbs, iVerbs, rcVerb, rcVerbType);
      if rcVerbType.VerbName = '' then
        MessageDlg('Unknown verb', 'Could not determine conjugation type for ' + rcVerb.VerbName, mtError, [mbOK], 0)
      else begin
        // Get verb conjugation file
        ChooseVerbFile(rcVerbType);
        // Pass all necessary data for conjugation of actual verb to the fConj form and display the conjugation table
        fConj.Verb := rcVerb;
        fConj.VerbType := rcVerbType;
        fConj.sForm := 'affirmative'; fConj.sGender := 'masculine';            // default settings (user may change them on fConj form)
        fConj.btAffirmative.Font.Style := [fsBold]; fConj.btNegative.Font.Style := [];
        fConj.bImpPron := bImperativePronoun;
        fConj.bSepSuffix := mOptionsDisplayConjSep.Checked;
        fConj.bSepRoot := mOptionsDisplayConjSep2.Checked;
        fConj.bColorIrreg := mOptionsDisplayConjColorIrreg.Checked;
        fConj.bColorSame := mOptionsDisplayConjColorSame.Checked;
        fConj.VerbFile  := VerbFile;
        // Show fConj form (with default settings conjugation)
        if not fConj.Visible then
          fConj.Show;
      end;
    end;
  end;
end;

{ Menu item "Conjugation > Exit": Exit application }

procedure TfSpanishER.mConjExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User input of number of exercise questions }

procedure TfSpanishER.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Spanish grammar', 'Number of conjugation exercises', IntToStr(iQuestionsTemp));
  if S <> '' then begin
    iQuestionsTemp := StrToInt(S);                                             // input value will become active with new exercise
    if iQuestionsTemp < 10 then
      iQuestionsTemp := 10;                                                    // arbitrarily chosen minimum
  end;
end;

{ Menu item "Options > Conjugation display options > Separation between stem and suffix": Toggle if separator should be used or not }

procedure TfSpanishER.mOptionsDisplayConjSepClick(Sender: TObject);

begin
  if mOptionsDisplayConjSep.Checked then begin
    mOptionsDisplayConjSep.Checked := False;
    // Disable "Separation between root and rest of stem" option
    mOptionsDisplayConjSep2.Checked := False;
    mOptionsDisplayConjSep2.Enabled := False;
  end
  else begin
    mOptionsDisplayConjSep.Checked := True;
    mOptionsDisplayConjSep2.Enabled := True;
  end;
end;

{ Menu item "Options > Conjugation display options > Separation between root and rest of stem": Toggle if separator should be used or not }

procedure TfSpanishER.mOptionsDisplayConjSep2Click(Sender: TObject);

begin
  if mOptionsDisplayConjSep2.Checked then
    mOptionsDisplayConjSep2.Checked := False
  else
    mOptionsDisplayConjSep2.Checked := True;
end;

{ Menu item "Options > Conjugation display options > Mark irregular verb forms by color": Toggle if irregularity highlighting should be used or not }

procedure TfSpanishER.mOptionsDisplayConjColorIrregClick(Sender: TObject);

begin
  if mOptionsDisplayConjColorIrreg.Checked then begin
    mOptionsDisplayConjColorIrreg.Checked := False;
    // Disable "Use same color for all irregularities" option
    mOptionsDisplayConjColorSame.Checked := False;
    mOptionsDisplayConjColorSame.Enabled := False;
  end
  else begin
    mOptionsDisplayConjColorIrreg.Checked := True;
    mOptionsDisplayConjColorSame.Enabled := True;
  end;
end;

{ Menu item "Options > Conjugation display options > Use same color for all irregularities": Toggle if same color highlighting should be used or not }

procedure TfSpanishER.mOptionsDisplayConjColorSameClick(Sender: TObject);

begin
  if mOptionsDisplayConjColorSame.Checked then
    mOptionsDisplayConjColorSame.Checked := False
  else
    mOptionsDisplayConjColorSame.Checked := True;
end;

{ Menu item "Options > Exercise display options > Do not display 'indicative'": Toggle if 'indicative' should be displayed or not }

procedure TfSpanishER.mOptionsDisplayExNoIndClick(Sender: TObject);

begin
  if mOptionsDisplayExNoInd.Checked then
    mOptionsDisplayExNoInd.Checked := False
  else
    mOptionsDisplayExNoInd.Checked := True;
end;

{ Menu item "Options > Exercise display options > Do not display 'affirmative'": Toggle if 'affirmative' should be displayed or not }

procedure TfSpanishER.mOptionsDisplayExNoAffClick(Sender: TObject);

begin
  if mOptionsDisplayExNoAff.Checked then
    mOptionsDisplayExNoAff.Checked := False
  else
    mOptionsDisplayExNoAff.Checked := True;
end;

{ Menu item "Options > Use imperative pronouns": Toggle if pronouns should be used with imperative or not }

procedure TfSpanishER.mOptionsUseImpPronClick(Sender: TObject);

begin
  if mOptionsUseImpPron.Checked then
    mOptionsUseImpPron.Checked := False
  else
    mOptionsUseImpPron.Checked := True;
  bImperativePronoun := mOptionsUseImpPron.Checked;
end;

{ Menu item "Help > Grammar help": Display Spanish grammar help }

procedure TfSpanishER.mHelpGrammarClick(Sender: TObject);

begin
  ShowHelpText('conj');
end;

{ Menu item "Help > Help": Display application help }

procedure TfSpanishER.mHelpHelpClick(Sender: TObject);

begin
  ShowHelpText('help');
end;

{ Menu item "Help > About": Display application about }

procedure TfSpanishER.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Spanish grammar:' + LineEnding;
  S += 'Conjugation of verbs in -er.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April 2022.';
  MessageDlg('About "SpanishER"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question": Generate exercise with parameters as selected }

procedure TfSpanishER.btQuestionClick(Sender: TObject);

begin
  // Do this for answers skipped (user pressing the "Question" button for next question without answering)
  if iQuestion > 0 then begin                                                  // avoid division by 0
    edQuestion.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    iFalse := iQuestion - iCorrect; edFalse.Text := IntToStr(iFalse);
    DisplaySuccess(iQuestion, iCorrect);
  end;
  Inc(iQuestion);
  // If there are still exercises left: Generate new question
  if iQuestion <= iQuestions then begin
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ':';
    repeat
      ExerciseRandomQuestion(iVerbs, aVerbs, sConjugation, bIndicative, bSubjunctive,
        bImperative, bParticiple, bMasculine, bFeminine, bAffirmative, bNegative, bImperativePronoun, rcVerb, sQuestion, sAnswer);
    until sAnswer <> 'n/a';                                                    // to avoid conjugated verb forms that don't exist
    edExercise.Text := sQuestion;
    edAnswer.Text := ''; edEvaluation.Text := '';
    btAnswer.Enabled := True;
    btAnswer.SetFocus;
    edAnswer.SetFocus;
  end
  // If the number of exercises to do has been reached: Display "end of exercise" message
  else
    EndOfExercise;
end;

{ Button "Answer": Check user's answer }

procedure TfSpanishER.btAnswerClick(Sender: TObject);

begin
  sUserAnswer := edAnswer.Text;
  if sUserAnswer = sAnswer then begin
    // Correct answer
    Inc(iCorrect);
    edEvaluation.Text := 'Correct answer!';
    edEvaluation.Font.Color := clLime;
  end
  else begin
    // Wrong answer
    Inc(iFalse);
    edEvaluation.Text := 'False! Correct answer is: ' + sAnswer;
    edEvaluation.Font.Color := clRed;
  end;
  // Display correct and false answers and success percentage
  edQuestion.Text := IntToStr(iQuestion);
  edCorrect.Text := IntToStr(iCorrect);
  iFalse := iQuestion - iCorrect; edFalse.Text := IntToStr(iFalse);
  DisplaySuccess(iQuestion, iCorrect);
  // If there are still exercises to do: Continue...
  if iQuestion < iQuestions then begin
    btAnswer.Enabled := False;
    btQuestion.SetFocus;
  end
  // If the number of exercises to do has been reached: End of exercise message
  else
    EndOfExercise;
end;

{ Non-ANSI letter buttons: Insert the corresponding letter into the user's answer string }

procedure TfSpanishER.btLetter1Click(Sender: TObject);

begin
  InsertLetter(1);
end;

procedure TfSpanishER.btLetter2Click(Sender: TObject);

begin
  InsertLetter(2);
end;

procedure TfSpanishER.btLetter3Click(Sender: TObject);

begin
  InsertLetter(3);
end;

procedure TfSpanishER.btLetter4Click(Sender: TObject);

begin
  InsertLetter(4);
end;

procedure TfSpanishER.btLetter5Click(Sender: TObject);

begin
  InsertLetter(5);
end;

procedure TfSpanishER.btLetter6Click(Sender: TObject);

begin
  InsertLetter(6);
end;

end.

