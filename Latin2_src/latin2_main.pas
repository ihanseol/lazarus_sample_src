{************************************}
{* Main unit for Latin2 application *}
{************************************}

unit latin2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, latin_declensions;

type
  TPreposition = record
    PPreposition, PAlternate, PCase1, PCase2: string;
  end;
  TPrepositions = array of TPreposition;
  TNoun = record
    NNominative, NGenitive: string;
    NGenus: Char;
    NDeclension: Integer;
  end;
  TNouns = array of TNoun;
  TSentenceNoun = record
    SNNoun: Integer;
    SNNumber: string;
  end;
  TSentenceNouns = array of TSentenceNoun;
  TSentence = record
    SSentence: string;
    SPreposition, sCasus: string;
    SSentenceNouns: TSentenceNouns;
  end;
  TSentences = array of TSentence;
  {**********}
  { TfLatin2 }
  {**********}
  TfLatin2 = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsQuestions, mHelp, mHelpAbout: TMenuItem;
    StaticText1, StaticText2: TStaticText;
    Label1, laQuestion, laAnswer: TLabel;
    edQuestion, edAnswer, edEval: TEdit;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuestions, iQuestions0, iQuestion, iCorrect, iPrepDoneMax: Integer;
    iPreposition, iNoun, iSentence: Integer;
    sPreposition, sCasus, sAnswer: string;
    aPrepositionsDone: array of Integer;
    aNounsDone: array of Boolean;
    aNouns: TNouns;
    aPrepositions: TPrepositions;
    aSentences: TSentences;
  end;

var
  fLatin2: TfLatin2;

implementation

{$R *.lfm}

{ Read nouns file }

procedure ReadNouns(out Nouns: TNouns);

var
  N, Decl, IX, I, P: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Nouns, 0);
  Assign(InFile, 'substantifs.txt'); Reset(InFile); N := 0; Decl := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> ';') then begin
      if LeftStr(Line, 1) = '*' then begin
        P := Pos('Substantifs', Line);
        if P > 0 then begin
          // Number of nouns in the file
          P := Pos('=', Line);
          N := StrToInt(Trim(Copy(Line, P + 1, Length(Line))));
          SetLength(Nouns, N);
          for I := 0 to N - 1 do
            Nouns[I].NNominative := '?';
        end
        else begin
          P := Pos('Declinaison', Line);
          if P > 0 then begin
            // Latin declension type (1 - 5)
            P := Pos('=', Line);
            Decl := StrToInt(Trim(Copy(Line, P + 1, Length(Line))));
          end;
        end;
      end
      else begin
        // Noun
        IX := StrToInt(LeftStr(Line, 3)); Delete(Line, 1, 4);
        if IX <= N then begin
          with Nouns[IX - 1] do begin
            P := Pos(',', Line);
            NNominative := Copy(Line, 1, P - 1); Delete(Line, 1, P + 1);
            P := Pos(',', Line);
            NGenitive := Copy(Line, 1, P - 1); Delete(Line, 1, P + 1);
            P := Pos(':', Line);
            NGenus := Trim(Copy(Line, 1, P - 1))[1];
            NDeclension := Decl;
          end;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Read prepositions file }

procedure ReadPrepositions(out Prepositions: TPrepositions);

var
  N, P: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Prepositions, 0);
  Assign(InFile, 'prepositions.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> ';') then begin
      Inc(N);
      SetLength(Prepositions, N);
      with Prepositions[N - 1] do begin
        PPreposition := Trim(Copy(Line, 1, 15)); PAlternate := '';
        P := Pos(',', PPreposition);
        if P > 0 then begin
          PAlternate := Copy(PPreposition, P + 1, Length(PPreposition) - P - 1);
          PPreposition := Copy(PPreposition, 1, P - 1);
        end;
        // Some prepositions are used with 2 cases
        PCase1 := Copy(Line, 16, 2);
        PCase2 := Copy(Line, 20, 2);
        if PCase2 = '--' then
          PCase2 := '';
      end;
    end;
  end;
  Close(InFile);
end;

{ Read sentences file }

procedure ReadSentences(out Sentences: TSentences);

var
  N, M, P: Integer;
  Line, Prep, PCase, SN: string;
  InFile: Text;

begin
  SetLength(Sentences, 0);
  Assign(InFile, 'phrases.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> ';') then begin
      if LeftStr(Line , 1) = '-' then begin
        // Preposition and case
        P := Pos('(', Line);
        Prep := Copy(Line, 3, P - 4);
        PCase := Copy(Line, P + 1, 2);
      end
      else begin
        // Index to nouns array and grammatical number(s) that may be used
        Inc(N);
        SetLength(Sentences, N);
        with Sentences[N - 1] do begin
          // Build this array element
          P := Pos(':', Line);
          SSentence := Trim(Copy(Line, 1, P - 1));                             // sentence
          SPreposition := Prep;                                                // preposition for this sentence
          sCasus := PCase;                                                     // case to be used in this sentence
          Delete(Line, 1, P); Line := Trim(Line);
          M := 0;                                                              // number of nouns available for this sentence
          repeat
            // Iterate the data line and extract all noun index / grammatical number pairs
            P := Pos(',', Line);
            if P > 0 then begin
              SN := Trim(LeftStr(Line, P - 1));
              Delete(Line, 1, P);
            end
            else begin
              SN := Trim(Line);
              Line := '';
            end;
            Inc(M);
            SetLength(SSentenceNouns, M);                                      // array with the available nouns
            // Grammatical number that may be used
            if RightStr(SN, 1) = 's' then
              SSentenceNouns[M - 1].SNNumber := 'sg'                           // singular
            else if RightStr(SN, 1) = 'p' then
              SSentenceNouns[M - 1].SNNumber := 'pl'                           // plural
            else
              SSentenceNouns[M - 1].SNNumber := '--';                          // both singular and plural may be used
            Delete(SN, Length(SN), 1);
            SSentenceNouns[M - 1].SNNoun := StrToInt(SN) - 1;                  // noun index in corresponding array
          until Line = '';
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Format grid items (right-align) }

function SFormat(S0: string): string;

var
  I: Integer;
  S: string;

begin
  S := S0;
  for I := 1 to 5 - Length(S0) do
    S := ' ' + S;
  Result := S;
end;

{**********}
{ TfLatin2 }
{**********}

{ Application start: Initialization }

procedure TfLatin2.FormCreate(Sender: TObject);

begin
  // Read nouns, prepositions and sentences from corr. files
  ReadNouns(aNouns); SetLength(aNounsDone, Length(aNouns));
  ReadPrepositions(aPrepositions); SetLength(aPrepositionsDone, Length(aPrepositions));
  ReadSentences(aSentences);
  // Start random number generatos
  Randomize;
  // Default number of questions
  iQuestions0 := 20;
  // Start a new exercice
  mExerciseNew.Click;
end;

{ Menu item "Exercice > Nouveau": Start a new exercise }

procedure TfLatin2.mExerciseNewClick(Sender: TObject);

var
  I: Integer;

begin
  // Reset variables and form components
  iQuestions := iQuestions0; iQuestion := 0; iCorrect := 0;
  laQuestion.Caption := 'Question:'; edQuestion.Text := ''; edAnswer.Text := '';
  edEval.Text := ''; edEval.Color := clDefault;
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  for I := 0 to Length(aNounsDone) - 1 do
    aNounsDone[I] := False;
  for I := 0 to Length(aPrepositionsDone) - 1 do
    aPrepositionsDone[I] := 0;
  iPrepDoneMax := iQuestions div Length(aPrepositions) + 1;                    // used to limit questions about a same preposition
  btQuestion.Caption := 'Question';
  btQuestion.Enabled := True;                                                  // re-enable button (will be disabled when exercise is done)
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfLatin2.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Nombre de questions": User input of number of questions }

procedure TfLatin2.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Grammaire latine', 'Nombre de questions', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 10 then                                                   // arbitrarily fixed minimum
      iQuestions0 := 10;
  end;
end;

{ Menu item "Aide > Info programme": Display application about }

procedure TfLatin2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Génération d''exercices de grammaire latine: ' + LineEnding;
  S += 'Utilisation des cas grammaticaux avec les prépositions.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, 2020-2024.';
  MessageDlg('Grammaire latine', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Réponse": Generate new question resp. check user answer }

procedure TfLatin2.btQuestionClick(Sender: TObject);

var
  Success, XNoun, XDeclension, Loop, I, P1, P2: Integer;
  SNoun, SAdjective, SNumber, SGenus, Masculine, Feminine, Neuter, UAnswer, S: string;
  OK: Boolean;
  Adjectives: array[0..1] of string;
  Noun: TNoun;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntTosTr(iQuestions);
    edAnswer.Text := ''; edEval.Text := ''; edEval.Color := clDefault;
    // Choose a random preposition
    repeat
      iPreposition := Random(Length(aPrepositions));
    until aPrepositionsDone[iPreposition] + 1 <= iPrepDoneMax;                 // limit usage of a same preposition
    Inc(aPrepositionsDone[iPreposition]);
    sPreposition := aPrepositions[iPreposition].PPreposition;
    // Grammatical case to be used (if there are two possibilities, choose randomly)
    if aPrepositions[iPreposition].PCase2 = '' then
      sCasus := aPrepositions[iPreposition].PCase1
    else begin
      if Random(2) = 0 then
        sCasus := aPrepositions[iPreposition].PCase1
      else
        sCasus := aPrepositions[iPreposition].PCase2;
    end;
    // Choose a random sentence (for this preposition and this grammatical case)
    repeat
      OK := True;
      iSentence := Random(Length(aSentences));
      if (aSentences[iSentence].SPreposition <> sPreposition) or (aSentences[iSentence].sCasus <> sCasus) then
        OK := False
      else begin
        Loop := 50;
        repeat
          // Choose a random noun for this sentence
          OK := True; Dec(Loop);
          XNoun := Random(Length(aSentences[iSentence].SSentenceNouns));
          iNoun := aSentences[iSentence].SSentenceNouns[XNoun].SNNoun;
          if aNouns[iNoun].NNominative = '?' then begin
            // To avoid program abortion if an index is missing in the nouns file (should not be the case...)
            OK := False;
          end
          else if (Loop > 0) and aNounsDone[iNoun] then
            // Use each noun only once, except if there isn't any unused noun available for this sentence
            OK := False;
        until OK;
      end;
    until OK;
    aNounsDone[iNoun] := True;                                                 // set this noun as having been used
    S := aSentences[iSentence].SSentence;
    // Check sentence for 1 or 2 adjectives (they are between square brackets)
    Adjectives[0] := ''; Adjectives[1] := '';
    for I := 0 to 1 do begin
      P1 := Pos('[',S); P2 := Pos(']', S);
      if (P1 > 0) and (P2 > 0) then begin
        Adjectives[I] := Copy(S, P1 + 1, P2 - P1 - 1);
        Delete(S, 1, P2);
      end;
    end;
    // Build the question text
    edQuestion.Text := aSentences[iSentence].SSentence;
    // Remove the adjective brackets
    edQuestion.Text := StringReplace(edQuestion.Text, '[', '', [rfReplaceAll]);
    edQuestion.Text := StringReplace(edQuestion.Text, ']', '', [rfReplaceAll]);
    // Replace the noun tag (#subst#) by the nominative of the actual noun
    Noun := aNouns[iNoun]; SGenus := aNouns[iNoun].NGenus; XDeclension := aNouns[iNoun].NDeclension;
    edQuestion.Text := StringReplace(edQuestion.Text, '#subst#', Noun.NNominative, []);
    // Replace possible adjectives (originally all 3 genus) by the adjective masculinum
    for I := 0 to 1 do begin
      if Adjectives[I] <> '' then begin
        P1 := Pos(',', Adjectives[I]);
        edQuestion.Text := StringReplace(edQuestion.Text, Adjectives[I], LeftStr(Adjectives[I], P1 - 1), []);
      end;
    end;
    // Add the  grammatical number
    SNumber := aSentences[iSentence].SSentenceNouns[XNoun].SNNumber;
    if SNumber = '--' then begin
      // Random number (if this is applicable)
      if Random(2) = 0 then
        SNumber := 'sg'
      else
        SNumber := 'pl';
    end;
    edQuestion.Text := edQuestion.Text + ' (' + SNumber + ')';
    // Build the answer text
    sAnswer := aSentences[iSentence].SSentence;
    // // Remove the adjective brackets
    sAnswer := StringReplace(sAnswer, '[', '', [rfReplaceAll]);
    sAnswer := StringReplace(sAnswer, ']', '', [rfReplaceAll]);
    SNoun := DeclensionNouns(XDeclension, Noun.NNominative, Noun.NGenitive, SGenus, sCasus, SNumber);
    // Replace the noun tag by the actual noun (case and number as required)
    sAnswer := StringReplace(sAnswer, '#subst#', SNoun, []);
    // Fill in possible adjectives (case, casus and number as required)
    for I := 0 to 1 do begin
      S := Adjectives[I];
      if S <> '' then begin
        P1 := Pos(',', S);
        Masculine := LeftStr(S, P1 - 1); Delete(S, 1, P1);
        P2 := Pos(';', S);
        Feminine := LeftStr(S, P2 - 1); Delete(S, 1, P2);
        Neuter := S;
        SAdjective := DeclensionAdjs(Masculine, Feminine, Neuter, SGenus, sCasus, SNumber, False);
        sAnswer := StringReplace(sAnswer, Adjectives[I], SAdjective, []);
      end;
    end;
    btQuestion.Caption := 'Réponse';                                           // next button push will be for answer checking
    edAnswer.SetFocus;
  end
  // Button "Réponse": Check user answer
  else begin
    // Get user answer from form
    UAnswer := edAnswer.Text;
    if UAnswer = sAnswer then begin
      // Correct answer
      edEval.Text := 'Cette réponse est correcte!';
      Inc(iCorrect);
    end
    else begin
      // False answer
      edEval.Text := 'Faux! La réponse correcte est: ' + sAnswer;
    end;
    // Fill in the evaluation grid
    Success := Round(100 * iCorrect / iQuestion);
    sgEval.Cells[1, 0] := SFormat(IntToStr(iQuestion));
    sgEval.Cells[1, 1] := SFormat(IntToStr(iCorrect));
    sgEval.Cells[1, 2] := SFormat(IntToStr(iQuestion - iCorrect));
    sgEval.Cells[1, 3] := SFormat(IntToStr(Success) + '%');
    // If all questions have been done, terminate the exercise
    if iQuestion = iQuestions then begin
      S := 'Test terminé. Succès = ' + IntToStr(Round(100 * iCorrect / iQuestion)) + '%.';
      if Success > 95 then
        S += ' Excellent résultat!'
      else if Success > 90 then
        S += ' Très bien travaillé!'
      else if Success > 80 then
        S += ' Bien travaillé!';
      MessageDlg('Grammaire latine', S, mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end
    // If there are questions left, continue with next question
    else begin
      btQuestion.Caption := 'Question';                                        // next button push will be for new question
      btQuestion.SetFocus;
    end;
  end;
end;

end.

