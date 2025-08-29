{***********************************}
{* Main unit for Temps application *}
{***********************************}

unit temps_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, ExtCtrls, temps_u2;

type
  TVerbe = record
    Present, Imparfait, Futur, Conditionnel: string;
    Mode: Char;
    ActionAnterieure: Boolean;
  end;
  TVerbes = array of TVerbe;
  {*********}
  { TfTemps }
  {*********}
  TfTemps = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionQuestions, mOptionsConj: TMenuItem;
    mHelp, mHelpGrammar, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, laQuestion: TLabel;
    edQuestion, edAnswer, edEval: TEdit;
    Shape1, Shape2: TShape;
    btIndPresent: TButton;
    btIndImparfait: TButton;
    btFutur: TButton;
    btConditionnel: TButton;
    btPasseComp: TButton;
    btPlusqueparfait: TButton;
    btFutur2: TButton;
    btConditionnel2: TButton;
    btSubjPresent: TButton;
    btSubjImparfait: TButton;
    btSubjPasse: TButton;
    btSubjPlusqueparfait: TButton;
    btQuestion: TButton;
    sgEval: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionQuestionsClick(Sender: TObject);
    procedure mOptionsConjClick(Sender: TObject);
    procedure mHelpGrammarClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btIndPresentClick(Sender: TObject);
    procedure btIndImparfaitClick(Sender: TObject);
    procedure btFuturClick(Sender: TObject);
    procedure btConditionnelClick(Sender: TObject);
    procedure btPasseCompClick(Sender: TObject);
    procedure btPlusqueparfaitClick(Sender: TObject);
    procedure btFutur2Click(Sender: TObject);
    procedure btConditionnel2Click(Sender: TObject);
    procedure btSubjPresentClick(Sender: TObject);
    procedure btSubjImparfaitClick(Sender: TObject);
    procedure btSubjPasseClick(Sender: TObject);
    procedure btSubjPlusqueparfaitClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuestionsTemp, iQuestions, iQuestion, iCorrect, iFalse: Integer;
    sAnswer: string;
    aVerbes: TVerbes;
  end;

const
  // Verbs used in the "subordonée"
  Verbs: array[0..5] of string = (
    'arriver', 'venir', 'partir', 's''en aller', 'comparaître', 'rejoindre'
  );
  ConjTemps: array[0..11] of string = (
    'indicatif présent', 'indicatif imparfait', 'futur simple', 'conditionnel présent',
    'passé composé', 'indicatif plus-que-parfait', 'futur antérieur', 'conditionnel passé',
    'subjonctif présent', 'subjonctif imparfait', 'subjonctif passé', 'subjonctif plus-que-parfait'
  );
var
  fTemps: TfTemps;

implementation

{$R *.lfm}

{ Format numbers in evaluation grid (right-alignment) }

function GridFormat(N: Integer): string;

var
  GF: string;

begin
  GF := IntToStr(N);
  if N < 10 then
    GF := '  ' + GF
  else if N < 100 then
    GF := ' ' + GF;
  GridFormat := GF;
end;

{ Get conjugated form of verb from tags specified in files}

function GetVerb(TagVerb, Present: string): string;

var
  Tag: Char;
  Verb: string;

// * = "imparfait", $ = "futur", # = "conditionnel"

begin
  if (TagVerb <> '*') and (TagVerb <> '$') and (TagVerb <> '#') then
    Verb := TagVerb
  else begin
    Verb := Present; Tag := TagVerb[1];
    if LeftStr(Present, 7) = 'je suis' then begin
      case Tag of
        '*': Verb := StringReplace(Verb, 'je suis', 'j''étais', []);
        '$': Verb := StringReplace(Verb, 'je suis', 'je serai', []);
        '#': Verb := StringReplace(Verb, 'je suis', 'je serais', []);
      end;
    end
    else if LeftStr(Present, 4) = 'j''ai' then begin
      case Tag of
        '*': Verb := StringReplace(Verb, 'j''ai', 'j''avais', []);
        '$': Verb := StringReplace(Verb, 'j''ai', 'j''aurai', []);
        '#': Verb := StringReplace(Verb, 'j''ai', 'j''aurais', []);
      end;
    end
    else if LeftStr(Present, 6) = 'il est' then begin
      case Tag of
        '*': Verb := StringReplace(Verb, 'il est', 'il était', []);
        '$': Verb := StringReplace(Verb, 'il est', 'il sera', []);
        '#': Verb := StringReplace(Verb, 'il est', 'il serait', []);
      end;
    end
    else begin
      // This is for verbs (not expressions) in -er only!
      // For all the rest, all 4 tenses haae to be specified in the file!
      Delete(Verb, Length(Verb), 1);
      if (LeftStr(Verb, 2) = 'je') or (LeftStr(Verb, 2) = 'j''') then begin
        case Tag of
          '*': Verb += 'ais';
          '$': Verb += 'erai';
          '#': Verb += 'erais';
        end;
      end
      else begin
        case Tag of
          '*': Verb += 'ait';
          '$': Verb += 'era';
          '#': Verb += 'erait';
        end;
      end;
    end;
  end;
  GetVerb := Verb;
end;

{ Determine negative form of conjugated verb }

function Negative(Verb: string): string;

const
  Sujets: array[0..2] of string = (
    'je', 'j''', 'il'
  );

var
  I, P: Integer;
  Sujet, Reflechi, Suppl: string;

begin
  // Determine subject
  for I := 0 to 2 do begin
    if LeftStr(Verb, Length(Sujets[I])) = Sujets[I] then
      Sujet := Sujets[I];
  end;
  Verb := StringReplace(Verb, Sujet, '', []);
  if LeftStr(Verb, 1) = ' ' then
    Delete(Verb, 1, 1);
  Reflechi := ''; Suppl := '';
  // Determine if there is something else
  P := Pos(' ', Verb);
  if P <> 0 then begin
    if (LeftStr(Verb, 3) = 'me ') or (LeftStr(Verb, 2) = 'm''') or (LeftStr(Verb, 3) = 'se ') or (LeftStr(Verb, 2) = 's''') then begin
      // This is a reflexive pronoun
      Reflechi := LeftStr(Verb, 2);
      if (LeftStr(Verb, 3) = 'me ') or (LeftStr(Verb, 3) = 'se ') then
        Delete(Verb, 1, 3)
      else
        Delete(Verb, 1, 2);
      // Determine if there is still something else
      P := Pos(' ', Verb);
    end;
  end;
  // There is something else (expressions like: j'ai besoin, il est nécessaire))
  if P <> 0 then begin
    Suppl := RightStr(Verb, Length(Verb) - P);
    Verb := LeftStr(Verb, P - 1);
  end;
  // Re-add the reflexive pronoun
  if Reflechi <> '' then
    Verb := Reflechi + ' ' + Verb;
  // Construct the negative form of the verb
  if (LeftStr(Verb, 2) = 'é') or (Verb[1] in ['i', 'u', 'e', 'o', 'a']) then
    Verb := 'n''' + Verb + ' pas'
  else
    Verb := 'ne ' + Verb + ' pas';
  // Re-add the subject
  if Sujet = 'j''' then
    Sujet := 'je';
  Verb := Sujet + ' ' + Verb;
  // Re-construct the expression (if it was one)
  if Suppl <> '' then
    Verb := Verb + ' ' + Suppl;
  Negative := Verb;
end;

{ Read the verbs from text files }

procedure ReadVerbes(out Verbes: TVerbes);

// One file for each case: indicatif, subjonctif, ind. with subj. when negative, subj. with ind. when negative

const
  Filename: array[0..3] of string = (
    'subjonctif.txt', 'indicatif.txt', 'indsubj.txt', 'subjind.txt'
  );

var
  I, P: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Verbes, 0);
  for I := 0 to 3 do begin
    Assign(InFile, Filename[I]); Reset(InFile);
    while not EoF(InFile) do begin
      Readln(InFile, Line);
      if Line <> '' then begin
        // Check if line contains special markers
        SetLength(Verbes, Length(Verbes) + 1);
        if LeftStr(Line, 1) = '?' then                                         // '?' indicates verbs, where I'm not really sure...
          Delete(Line, 1, 1);
        Verbes[Length(Verbes) - 1].ActionAnterieure := True;
        if LeftStr(Line, 1) = '-' then begin                                   // '-' indicates verbs, where action antérieurs maakes no sense
          Delete(Line, 1, 1);
          Verbes[Length(Verbes) - 1].ActionAnterieure := False;
        end;
        // Get "présent"
        P := Pos(',', Line);
        Verbes[Length(Verbes) - 1].Present := LeftStr(Line, P - 1);
        Delete(Line, 1, P + 1);
        // Get "imparfait" (transform tag to verb form, if necessary)
        P := Pos(',', Line);
        Verbes[Length(Verbes) - 1].Imparfait := GetVerb(LeftStr(Line, P - 1), Verbes[Length(Verbes) - 1].Present);
        Delete(Line, 1, P + 1);
        // Get "futur" (transform tag to verb form, if necessary)
        P := Pos(',', Line);
        Verbes[Length(Verbes) - 1].Futur := GetVerb(LeftStr(Line, P - 1), Verbes[Length(Verbes) - 1].Present);
        Delete(Line, 1, P + 1);
        // Get "conditionnel" (transform tag to verb form, if necessary)
        Verbes[Length(Verbes) - 1].Conditionnel := GetVerb(Line, Verbes[Length(Verbes) - 1].Present);
        // Get mode (X = ind. with subj. when negative, Y = contrary)
        case I of
          0: Verbes[Length(Verbes) - 1].Mode := 'S';
          1: Verbes[Length(Verbes) - 1].Mode := 'I';
          2: Verbes[Length(Verbes) - 1].Mode := 'X';
          3: Verbes[Length(Verbes) - 1].Mode := 'Y';
        end;
      end;
    end;
    Close(Infile);
  end;
end;

{ Start new exercise }

procedure NewExercise(QuestionsTemp: Integer; out Questions, Question, QCorrect, QFalse: Integer);

var
  I: Integer;

begin
  Questions := QuestionsTemp;                                                  // selected number of questions now becoming active
  Question := 0; QCorrect := 0; QFalse := 0;
  fTemps.laQuestion.Caption := 'Question';
  fTemps.edQuestion.Text := '';
  fTemps.edAnswer.Text := '';
  fTemps.edEval.Text := '';
  for I := 0 to 3 do
    fTemps.sgEval.Cells[1, I] := '';
  fTemps.btQuestion.Caption := 'Question';
  fTemps.btQuestion.Enabled := True;
end;

{ Display result of answer check; fill in the evaluation grid }

procedure Evaluation(Correct: Boolean; Answer: string; Questions: Integer; var Question, QCorrect, QFalse: Integer);

begin
  if Correct then begin
    Inc(QCorrect);
    fTemps.edEval.Text := 'Réponse correcte!';
  end
  else begin
    Inc(QFalse);
    fTemps.edEval.Text := 'Faux! Correct = ' + Answer;
  end;
  fTemps.sgEval.Cells[1, 0] := GridFormat(Question);
  fTemps.sgEval.Cells[1, 1] := GridFormat(QCorrect);
  fTemps.sgEval.Cells[1, 2] := GridFormat(QFalse);
  fTemps.sgEval.Cells[1, 3] := GridFormat(Round((100 * QCorrect) / Question));
  // All questiosn done: Display message
  if Question = Questions then begin
    MessageDlg('Fin de l''exercice', 'Toutes les questions ont été faites!', mtInformation, [mbOK], 0);
    Inc(Question);
  end;
end;

{ Check user answer; do evaluation (by calling the evoluation sub }

procedure CheckAnswer(Answer, UserAnswer: string; Questions: Integer; var Question, QCorrect, QFalse: Integer);

var
  Correct: Boolean;

begin
  if UserAnswer = Answer then
    Correct := True
  else
    Correct := False;
  Evaluation(Correct, Answer, Questions, Question, QCorrect, QFalse);
  fTemps.btQuestion.Enabled := True;
  fTemps.btQuestion.SetFocus;
end;

{ French grammar "Concordance des temps" routine }

function ConcordanceTemps(TempsPrinc: string; Mode: Char; ActionSub: string): string;

var
  TempsSub: string;

begin
  if Mode = 'I' then begin
    if TempsPrinc = 'présent' then begin
      if ActionSub = 'simultané' then
        TempsSub := 'indicatif présent'
      else if ActionSub = 'antérieur' then
        TempsSub := 'passé composé'
      else if ActionSub = 'postérieur' then
        TempsSub := 'futur simple';
    end
    else if TempsPrinc = 'futur' then begin
      if ActionSub = 'simultané' then
        TempsSub := 'futur simple'
      else if ActionSub = 'antérieur' then
        TempsSub := 'futur antérieur'
      else if ActionSub = 'postérieur' then
        TempsSub := 'futur simple';
    end
    else if TempsPrinc = 'imparfait' then begin
      if ActionSub = 'simultané' then
        TempsSub := 'indicatif imparfait'
      else if ActionSub = 'antérieur' then
        TempsSub := 'indicatif plus-que-parfait'
      else if ActionSub = 'postérieur' then
        TempsSub := 'conditionnel présent';
    end
    else if TempsPrinc = 'conditionnel' then begin
      if ActionSub = 'simultané' then
        TempsSub := 'conditionnel présent'
      else if ActionSub = 'antérieur' then
        TempsSub := 'conditionnel passé'
      else if ActionSub = 'postérieur' then
        TempsSub := 'conditionnel présent';
    end;
  end
  else begin
    TempsSub := 'subjonctif ';
    if (TempsPrinc = 'présent') or (TempsPrinc = 'futur') then begin
      if (ActionSub = 'simultané') or (ActionSub = 'postérieur') then
        TempsSub += 'présent'
      else if ActionSub = 'antérieur' then
        TempsSub += 'passé';
    end
    else if (TempsPrinc = 'imparfait') or (TempsPrinc = 'conditionnel') then begin
      if (ActionSub = 'simultané') or (ActionSub = 'postérieur') then
        TempsSub += 'imparfait'
      else if ActionSub = 'antérieur' then
        TempsSub += 'plus-que-parfait';
    end;
  end;
  ConcordanceTemps := TempsSub;
end;

{ Hard coded conjugation of the 6 verbs used in the "subordonnée" }

function Conjugate(Verb, Temps: string): string;

const
  Conjugation: array[0..5, 0..11] of string = (
    ( 'arrive', 'arrivait', 'arrivera', 'arriverait',
      'est arrivé', 'était arrivé', 'sera arrivé', 'serait arrivé',
      'arrive', 'arrivât', 'soit arrivé', 'fût arrivé'
    ),
    ( 'vient', 'venait', 'viendra', 'viendrait',
      'est venu', 'était venu', 'sera venu', 'serait venu',
      'vienne', 'vînt', 'soit venu', 'fût venu'
    ),
    ( 'part', 'partait', 'partira', 'partirait',
      'est parti', 'était parti', 'sera parti', 'serait parti',
      'parte', 'partît', 'soit parti', 'fût parti'
    ),
    ( 's''en va', 's''en allait', 's''en ira', 's''en irait',
      's''en est allé', 's''en était allé', 's''en sera allé', 's''en serait allé',
      's''en aille', 's''en allât', 's''en soit allé', 's''en fût allé'
    ),
    ( 'comparaît', 'comparaissait', 'comparaîtra', 'comparaîtrait',
      'a comparu', 'avait comparu', 'aura comparu', 'aurait comparu',
      'comparaisse', 'comparût', 'ait comparu', 'eût comparu'
    ),
    ( 'rejoint', 'rejoignait', 'rejoindra', 'rejoindrait',
      'a rejoint', 'avait rejoint', 'aura rejoint', 'aurait rejoint',
      'rejoigne', 'rejoignît', 'ait rejoint', 'eût rejoint'
    )
  );

var
  VX, TX, I: Integer;

begin
  for I := 0 to 5 do begin
    if Verb = Verbs[I] then
      VX := I;
  end;
  for I := 0 to 11 do begin
    if Temps = ConjTemps[I] then
      TX := I;
  end;
  Conjugate := Conjugation[VX, TX];
end;

{*********}
{ TfTemps }
{*********}

{ Application start: Initialisation }

procedure TfTemps.FormCreate(Sender: TObject);

begin
  ReadVerbes(aVerbes);
  iQuestionsTemp := 10;
  NewExercise(iQuestionsTemp, iQuestions, iQuestion, iCorrect, iFalse);
  Randomize;
end;

{ Menu item "Exercice > Nouveau": Start new exercise }

procedure TfTemps.mExerciseNewClick(Sender: TObject);

begin
  NewExercise(iQuestionsTemp, iQuestions, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfTemps.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Option > Nombre de questions": Get number of exercise questions }

procedure TfTemps.mOptionQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Exercice de grammaire', 'Nombre de questions', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
end;

{ Menu item "Option > Conjuguer les verbes": Select answer method (entering conjugated verb or using "tense buttons") }

procedure TfTemps.mOptionsConjClick(Sender: TObject);

begin
  if mOptionsConj.Checked then
    mOptionsConj.Checked := False
  else
    mOptionsConj.Checked := True;
end;

{ Menu item "Aide > Aide grammaire": Display "concordance des temps" help }

procedure TfTemps.mHelpGrammarClick(Sender: TObject);

begin
  fHelp.Memo1.Clear;
  fHelp.Memo1.Lines.LoadFromFile('grammar');
  fHelp.Show;
end;

{ Menu item "Aide > Aide programme": Display program help }

procedure TfTemps.mHelpHelpClick(Sender: TObject);

begin
  fHelp.Memo1.Clear;
  fHelp.Memo1.Lines.LoadFromFile('help');
  fHelp.Show;
end;

{ Menu item "Aide > Info programme": Display program about }

procedure TfTemps.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Grammaire françsise: La concordance des temps.' + LineEnding;
  S += 'Version 1.0, © allu, Décembre 2018.';
  MessageDlg('Concernant "Temps"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Réponse": Generate new question resp. check user answer }

procedure TfTemps.btQuestionClick(Sender: TObject);

const
  TempsPrincipale: array[0..3] of string = (
    'présent', 'imparfait', 'futur', 'conditionnel'
  );
  ActionsSubordonnee: array[0..2] of string = (
    'simultané', 'postérieur', 'antérieur'
  );

var
  V, TP, TS, F: Integer;
  Question, Verb, Verb2, TempsPrinc, ActionSub, TempsSub: string;
  Mode: Char;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    // Set all bottons' normal font style
    fTemps.btConditionnel.Font.Style := [];
    fTemps.btConditionnel2.Font.Style := [];
    fTemps.btFutur.Font.Style := [];
    fTemps.btFutur2.Font.Style := [];
    fTemps.btIndImparfait.Font.Style := [];
    fTemps.btIndPresent.Font.Style := [];
    fTemps.btPasseComp.Font.Style := [];
    fTemps.btPlusqueparfait.Font.Style := [];
    fTemps.btSubjImparfait.Font.Style := [];
    fTemps.btSubjPasse.Font.Style := [];
    fTemps.btSubjPlusqueparfait.Font.Style := [];
    fTemps.btSubjPresent.Font.Style := [];
    // Proceed if there are questions left
    if iQuestion < iQuestions then begin
      Inc(iQuestion);
      laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
      // Random verb and random tense
      V := Random(Length(aVerbes)); TP := Random(4);
      // Get conjugated form for current tense
      case TP of
        0: Verb := aVerbes[V].Present;
        1: Verb := aVerbes[V].Imparfait;
        2: Verb := aVerbes[V].Futur;
        3: Verb := aVerbes[V].Conditionnel;
      end;
      TempsPrinc := TempsPrincipale[TP];
      // Get random form (affirmative or negative)
      F := Random(2);
      if F = 1 then
        Verb := Negative(Verb);                                                // Get negative form of actual conjugated verb
      // Get mode to be used with current verb
      Mode := aVerbes[V].Mode;
      // Transform tagged modes to real modes (depending on current verb form)
      if Mode = 'X' then begin
        if F = 0 then
          Mode := 'I'
        else
          Mode := 'S';
      end
      else if Mode = 'Y' then begin
        if F = 0 then
          Mode := 'S'
        else
          Mode := 'I';
      end;
      // Get one of the 6 verbs for the "subordonnée"
      Verb2 := Verbs[Random(6)];
      // Build the question
      Question := Verb + ' qu''il ';
      if Verb2 = 'rejoindre' then
        Question += 'nous ';
      Question += '[' + Verb2 + '] ';
      // Random action position of the subordonnée
      if TempsPrinc = 'futur' then                                             // if "principale" = futur, do not use "simultané" in "subordonnée"
        TS := Random(2) + 1
      else if not aVerbes[V].ActionAnterieure then                             // do not use "antérieur" for certain verbs (see ReadVerbs routine)
        TS := Random(2)
      else                                                                     // normal case
        TS := Random(3);
      ActionSub := ActionsSubordonnee[TS];
      // Get expression to indicate the "subordonnée" action position to the user
      case TS of
        0: if TP = 1 then
             Question += 'le jour-même'
           else
             Question += 'aujourd''hui';
        1: if TP = 1 then
             Question += 'le jour après'
           else
             Question += 'demain';
        2: if TP = 1 then
             Question += 'le jour avant'
           else
             Question += 'hier';
      end;
      // Add this expression to the question text
      edQuestion.Text := Question;
      edAnswer.Text := '';
      fTemps.edEval.Text := '';
      // Determine the tense to be used in the "subordonnée"
      TempsSub := ConcordanceTemps(TempsPrinc, Mode, ActionSub);
      // Determine the answer to the actual question
      if mOptionsConj.Checked then
        // Answer is the conjugated form of the "subordonnée" verb
        sAnswer := Conjugate(Verb2, TempsSub)
      else
        // Answer is a tense
        sAnswer := TempsSub;
      // Set button properties, depending on answering method
      if mOptionsConj.Checked then begin
        btQuestion.Caption := 'Answer';
        edAnswer.SetFocus;
      end
      else begin
        btQuestion.Enabled := False;
        btIndPresent.SetFocus;
      end;
    end;
  end
  // Button "Réponse": Check user answer
  else begin
    if iQuestion <= iQuestions then begin
      CheckAnswer(sAnswer, edAnswer.Text, iQuestions, iQuestion, iCorrect, iFalse);
      // Set button properties
      btQuestion.Caption := 'Question';
      btQuestion.Enabled := True;
      btQuestion.SetFocus;
    end;
  end;
end;

{ "Tense buttons": Check user answer }

procedure TfTemps.btIndPresentClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btIndPresent.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[0], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btIndImparfaitClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btIndImparfait.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[1], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btFuturClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btFutur.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[2], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btConditionnelClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btConditionnel.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[3], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btPasseCompClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btPasseComp.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[4], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btPlusqueparfaitClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btPlusqueparfait.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[5], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btFutur2Click(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btFutur2.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[6], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btConditionnel2Click(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btConditionnel2.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[7], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btSubjPresentClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btSubjPresent.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[8], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btSubjImparfaitClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btSubjImparfait.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[9], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btSubjPasseClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btSubjPasse.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[10], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

procedure TfTemps.btSubjPlusqueparfaitClick(Sender: TObject);

begin
  if not mOptionsConj.Checked and not btQuestion.Enabled then begin
    btSubjPlusqueparfait.Font.Style := [fsbold];
    CheckAnswer(sAnswer, ConjTemps[11], iQuestions, iQuestion, iCorrect, iFalse);
  end;
end;

end.

