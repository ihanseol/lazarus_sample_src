{*******************************************}
{* Main unit for Prepositions1 application *}
{*******************************************}

unit prepositions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, Dos;

const
  NWords = 67; NSentences = 359;

type
  {****************}
  { TfPrepositions }
  {****************}
  TfPrepositions = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsQuestions: TMenuItem;
    mOptionsEval: TMenuItem;
    mOptionsEval3: TMenuItem;
    mOptionsEval2: TMenuItem;
    mOptionsEval1: TMenuItem;
    mExtras, mExtrasEx, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    laQuestion: TLabel;
    edQuestion, edAnswer, edSentences, edQFile, edAFile: TMemo;
    edQuestions, edCorrect, edFalse, edNoAnsw, edSuccess: TEdit;
    btQuestion: TButton;
    btAnswer: TButton;
    dlgPrepositions: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsEval1Click(Sender: TObject);
    procedure mOptionsEval2Click(Sender: TObject);
    procedure mOptionsEval3Click(Sender: TObject);
    procedure mExtrasExClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
  private
    iQuestions0, iQuestions, iQuestion, iCorrect, iFalse, iEvalFalse0, iEvalNoAnsw0, iEvalFalse, iEvalNoAnsw, iAnswer: Integer;
    sQuestion, sAnswer, sDir: string;
    aSentencesDone: array[0 .. NSentences - 1] of Boolean;
  end;

const
  // These words are used to replace {number} tags in the sentence strings
  // Not necessary all this, of course, but a simple possibility to create somewhat varying question sentences
  Words: array[0 .. NWords - 1] of array of string = (
    ( 'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December' ),
    ( 'spring', 'summer', 'fall', 'winter' ),
    ( '1962', '1975', '1980', '1985', '1992', '1996', '2000' ),
    ( 'morning', 'afternoon', 'evening' ),
    ( '19th century', '20th century', 'last century' ),
    ( '1 hour', '2 hours', 'half an hour', '15 minutes', '30 minutes' ),
    ( 'a year', 'a month', '2 months', '6 months', '2 weeks', '3 weeks' ),
    ( '7 o''clock', '7:30', '8:00', '8:30', '9 o''clock', '10 o''clock' ),
    ( 'noon', 'midnight', 'night', 'sunrise', 'sunset', 'dawn' ),
    ( 'breakfast', 'lunch', 'dinner', 'supper' ),
    ( '10', '12', '14', '15', '16' ),
    ( 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday' ),
    ( 'Mondays', 'Tuesdays', 'Wednesdays', 'Thursdays', 'Fridays', 'Saturdays', 'Sundays' ),
    ( 'week', 'month', 'year', 'semester' ),
    ( '2 weeks', '2 months', 'half a year', 'a year', 'several days' ),
    ( 'January 1st', 'April 2nd', 'May 10th', 'June 21st', 'July 18th' ),
    ( 'work', 'school', 'the cinema', 'a concert', 'a party', 'the library' ),
    ( 'my bed', 'the table', 'the chair', 'my desk' ),
    ( 'book', 'newspaper', 'magazine' ),
    ( 'shop', 'supermarket', 'bookstore', 'grocery store', 'bakery' ),
    ( 'go to work', 'went to bed', 'woke up', 'will go shopping', 'will meet her' ),
    ( 'works', 'does laundry', 'sleeps late', 'washes his car', 'visits his parents' ),
    ( 'New York', 'Montreal', 'Toronto', 'Chicago', 'Tokio', 'London', 'Paris'),
    ( 'my room', 'the lunchroom', 'the kitchen', 'the living room' ),
    ( 'the car', 'a taxi', 'a bar', 'a restaurant' ),
    ( 'left', 'right' ),
    ( 'first', 'second', 'third', 'fourth', 'fifth' ),
    ( 'bus', 'train', 'plane', 'subway' ),
    ( 'the keys', 'my phone', 'my umbrella' ),
    ( 'radio', 'TV', 'television' ),
    ( 'my bank', 'my lawyer', 'a friend', 'my boss' ),
    ( 'vegetables', 'carrots', 'peas', 'beans', 'patatoes' ),
    ( 'man', 'woman', 'girl', 'boy' ),
    ( 'men', 'women', 'girls', 'boys' ),
    ( 'deer', 'hare', 'rabbit', 'reindeer', 'boar' ),
    ( 'Luxembourg', 'France', 'Belgium', 'Germany' ),
    ( 'red', 'yellow', 'purple', 'pink', 'blue' ),
    ( 'Roger', 'Richard', 'Nick', 'David', 'Paul', 'Peter', 'Steven', 'Mark', 'John', 'George' ),
    ( 'Mary', 'Elisabeth', 'Melissa', 'Joan', 'Catherine', 'Jane', 'Jessica', 'Maria', 'Judy', 'Angelina' ),
    ( 'boss', 'doctor', 'teacher', 'neighbor' ),
    ( 'corner', 'bus stop', 'train station', 'roadside' ),
    ( 'an iron rod', 'a long knife', 'a beer bottle' ),
    ( 'the history of ancient Egypt', 'nuclear physics', 'modern music', 'computational biology' ),
    ( 'bird', 'eagle', 'raven', 'plane', 'hot air balloon' ),
    ( 'cups', 'glasses', 'bowls', 'plates' ),
    ( 'tree', 'drape', 'door', 'window' ),
    ( 'car', 'TV', 'smartphone', 'laptop', 'bicycle' ),
    ( 'mid-term exam', 'the end-of-year report', 'our assignment', 'your article in the Times' ),
    ( 'Twitter', 'Facebook', 'YouTube', 'the Internet' ),
    ( 'balcony', 'stairs', 'sidewalk', 'shore' ),
    ( 'a wedding', 'the bank', 'the beach', 'the post office', 'the ticket counter' ),
    ( 'fruit', 'coconut', 'banana', 'pineapple', 'kiwi' ),
    ( 'oranges', 'apples', 'pears', 'kiwis', 'pineapples' ),
    ( 'Christmas', 'Thanksgiving', 'Chinese New Year', 'Easter' ),
    ( 'son', 'daughter', 'father', 'mother', 'uncle', 'aunt' ),
    ( 'Bay Street', 'Baker Street', '4th Avenue', 'Hamilton Avenue', 'Main Street' ),
    ( 'chauffeur', 'cab driver', 'barkeeper', 'waiter', 'house painter' ),
    ( 'engineering', 'computer science', 'mechanics', 'electronics' ),
    ( 'football', 'basketball', 'handball', 'tennis', 'rugby' ),
    ( 'scientist', 'engineer', 'computer scientist', 'biologist' ),
    ( 'trousers', 'shirts', 'skirts', 'bathing suits' ),
    ( 'English', 'Spanish', 'French', 'Chinese' ),
    ( 'book', 'money', 'report', 'hand tools'),
    ( 'sister', 'brother', 'boyfriend', 'girlfriend' ),
    ( 'computers', 'laptops', 'cars', 'music albums'),
    ( 'shops', 'supermarkets', 'bookstores', 'grocery stores', 'bakeries' ),
    ( 'North Carolina', 'New York', 'Manchester', 'London')
  );

var
  fPrepositions: TfPrepositions;

implementation

{$R *.lfm}

{ Generate question and answer strings (randomly taken from one of the available sentences) }

procedure GenerateQuestion(Sentences: TMemo; var SentencesDone: array of Boolean; out Question, Answer: string; ExType: string);

var
  N, L, I, P, P1, P2: Integer;
  Prep, Tag, TagWord, S: string;

begin
  // Random sentence (among those not yet done)
  repeat
    N := Random(NSentences);
  until not SentencesDone[N];
  SentencesDone[N] := True;                                                    // mark this sentence as done
  S := Sentences.Lines[N];
  // Replace {number} tags by words (for each tag, randomly take a word among those available)
  for I := 0 to NWords - 1 do begin
    Tag := '{' + IntToStr(I) + '}';
    P := Pos(Tag, S);
    if P <> 0 then begin
      // This is a tag: Replace by a word
      L := Length(Words[I]);
      TagWord := Words[I][Random(L)];
      S := StringReplace(S, Tag, TagWord, [rfReplaceAll]);
    end;
  end;
  // Add period at end of sentence
  if not (RightStr(S, 1)[1] in ['.', '!', '?']) then
    S += '.';
  Question := S;
  // Answer string: Remove square brackets and make first letter of sentence uppercase
  Answer := StringReplace(S, '[', '', [rfReplaceAll]);
  Answer := StringReplace(Answer, ']', '', [rfReplaceAll]);
  Answer[1] := UpperCase(Answer[1])[1];
  // Question string: Replace prepositions (identified by square brackets) by placeholder (underscores)
  P1 := 0; P2 := 0;
  repeat
    P1 := Pos('[', Question);
    P2 := Pos(']', Question);
    if (P1 <> 0) and (P2 <> 0) and (P2 > P1) then begin
      Prep := Copy(Question, P1, P2 - P1 + 1);
      if ExType = 'file' then
        Question := StringReplace(Question, Prep, '________', [])
      else
        Question := StringReplace(Question, Prep, '___', []);
    end;
  until (P1 = 0) and (P2 = 0);
end;

{ Calculate success percentage using actual evaluation settings }

procedure Evaluation(Question, ACorrect, AFalse, EvalFalse, EvalNoAnsw: Integer);

var
  P, Success: Integer;

begin
  fPrepositions.edQuestions.Text := IntToStr(Question) + ' ';
  fPrepositions.edCorrect.Text := IntToStr(ACorrect) + ' ';
  fPrepositions.edFalse.Text := IntToStr(AFalse) + ' ';
  fPrepositions.edNoAnsw.Text := IntToStr(Question - ACorrect - AFalse) + ' ';
  P := ACorrect + EvalFalse * AFalse + EvalNoAnsw * (Question - ACorrect - AFalse);
  Success := Round(100 * (P / Question));
  if Success < 0 then
    Success := 0;
  fPrepositions.edSuccess.Text := IntToStr(Success)  + '%';
end;

{****************}
{ TfPrepositions }
{****************}

{ Application start: Initialization }

procedure TfPrepositions.FormCreate(Sender: TObject);

begin
  iQuestions0 := 20;
  iEvalFalse0 := 0; iEvalNoAnsw0 := 0;
  sDir := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\Documents';             // store exercise files in user Documents directory
  Randomize;
  mTestNew.Click;
end;

{ Menu item "Test > New": Start a new exercise }

procedure TfPrepositions.mTestNewClick(Sender: TObject);

var
  I: Integer;

begin
  iQuestions := iQuestions0;
  iEvalFalse := iEvalFalse0; iEvalNoAnsw := iEvalNoAnsw0;
  iQuestion := 0; iCorrect := 0; iFalse := 0; iAnswer := 0;
  laQuestion.Caption := 'Question';
  edQuestion.Lines.Clear; edAnswer.Lines.Clear;
  edQuestions.Text := ''; edCorrect.Text := '';
  edFalse.Text := ''; edNoAnsw.Text := ''; edSuccess.Text := '';
  for I := 0 to NSentences - 1 do
    aSentencesDone[I] := False;
  btQuestion.Enabled := True; btAnswer.Enabled := False;
end;

{ Menu item "Test > Exit": Exit application }

procedure TfPrepositions.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions...": User input of number of exercise sentences }

procedure TfPrepositions.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('English grammar', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 20 then
      iQuestions0 := 20                                                        // arbitrarly fixed minimum = 2ß
    else if iQuestions0 > NSentences then
      iQuestions0 := NSentences;                                               // imposed maximum ? total number of sentences
  end;
end;

{ Menu items "Options > Answer evaluation > ...": User selection of test evaluation method }

procedure TfPrepositions.mOptionsEval1Click(Sender: TObject);

// +1 for each correct answer (otherwise nothing)

begin
  mOptionsEval1.Checked := True; mOptionsEval2.Checked := False; mOptionsEval3.Checked := False;
  iEvalFalse0 := 0; iEvalNoAnsw0 := 0;
end;

procedure TfPrepositions.mOptionsEval2Click(Sender: TObject);

// +1 for each correct answer, -1 otherwise (false answer or question not ansswered)

begin
  mOptionsEval1.Checked := False; mOptionsEval2.Checked := True; mOptionsEval3.Checked := False;
  iEvalFalse0 := -1; iEvalNoAnsw0 := -1;
end;

procedure TfPrepositions.mOptionsEval3Click(Sender: TObject);

// +1 for each correct answer, -1 for each false answer (question not ansswered: nothing)

begin
  mOptionsEval1.Checked := False; mOptionsEval2.Checked := False; mOptionsEval3.Checked := True;
  iEvalFalse0 := -1; iEvalNoAnsw0 := 0;
end;

{ Menu item "Extras > Create exercise file": Generate 2 text files, one with the questions, the other with the answers }

procedure TfPrepositions.mExtrasExClick(Sender: TObject);

var
  I: Integer;
  Filename, Ext, QFile, AFile, SN: string;

begin
  for I := 0 to NSentences - 1 do
    aSentencesDone[I] := False;
  edQFile.Lines.Clear; edAFile.Lines.Clear;
  // Get question and answer strings and put them into (invisible) memos
  for I := 1 to iQuestions do begin
    GenerateQuestion(edSentences, aSentencesDone, sQuestion, sAnswer, 'file');
    SN := IntToStr(I);
    if I < 10 then
      SN := '  ' + SN
    else if I < 100 then
      SN := ' ' + SN;
    sQuestion := SN + '. ' + sQuestion + LineEnding;
    sAnswer := SN + '. ' + sAnswer + LineEnding;
    edQFile.Lines.Append(sQuestion);
    edAFile.Lines.Append(sAnswer);
  end;
  // Write the memo content to text file (with filename based on user input)
  dlgPrepositions.InitialDir := sDir;
  dlgPrepositions.FileName := 'Exercise.txt';
  if dlgPrepositions.Execute then begin
    // User entered a filename
    Filename := dlgPrepositions.FileName;
    sDir := ExtractFileDir(Filename);                                          // remember directory where user browsed to
    Ext := ExtractFileExt(Filename);
    Filename := Copy(Filename, 1, Length(Filename) - Length(Ext));
    QFile := Filename + ' (Questions)' + Ext;                                  // Questions file name
    AFile := Filename + ' (Answers)' + Ext;                                    // Answers file name
    // Write memo content to file
    edQFile.Lines.SaveToFile(QFile);
    edAFile.Lines.SaveToFile(AFile);
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfPrepositions.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'English grammar:' + LineEnding;
  S += 'Prepositions exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2023.';
  MessageDlg('About "Prepositions1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question" pushed: Generate new question }

procedure TfPrepositions.btQuestionClick(Sender: TObject);

begin
  edAnswer.Lines.Clear;
  if iAnswer < iQuestion then begin
    // This means that previous question does not have been answered to
    // Calculate appropriate success percentage and fill in evaluation values
    Evaluation(iQuestion, iCorrect, iFalse, iEvalFalse, iEvalNoAnsw);
  end;
  Inc(iQuestion);
  // If there are still questions left, proceed
  if iQuestion <= iQuestions then begin
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions);
    GenerateQuestion(edSentences, aSentencesDone, sQuestion, sAnswer, 'interactive');
    edQuestion.Lines.Clear;
    edQuestion.Lines.Append(sQuestion);
    btAnswer.Enabled := True;
  end
  // If there aren't any questions left, terminate the exercise
  else begin
    MessageDlg('English grammar', 'All questions done. Prepositions test finished.', mtInformation, [mbOK], 0);
    btQuestion.Enabled := False; btAnswer.Enabled := False;                    // The "Question" button will be re-enabled when a new test is started
  end;
end;

{ Button "Answer" pushed: Check user answer }

procedure TfPrepositions.btAnswerClick(Sender: TObject);

var
  UAnswer: string;

begin
  edAnswer.Lines.Clear;
  UAnswer := Trim(edQuestion.Text);                                            // read sentence with prepositions filled in by user from TMemo
  // All prepositions are correct (sentences identity)
  if UAnswer = sAnswer then begin
    Inc(iCorrect);
    edAnswer.Lines.Append('Your answer is correct!');
  end
  // One or more wrong prepositions (sentences difference)
  else begin
    if UAnswer <> '' then
      Inc(iFalse);
    edAnswer.Lines.Append(sAnswer);
  end;
  // Calculate success percentage and fill in evaluation values
  Evaluation(iQuestion, iCorrect, iFalse, iEvalFalse, iEvalNoAnsw);
  // If there are still questions left, proceed
  if iQuestion < iQuestions then begin
    iAnswer := iQuestion;
    btAnswer.Enabled := False;
  end
  // If there aren't any questions left, terminate the exercise
  else begin
    MessageDlg('English grammar', 'All questions done. Prepositions test finished.', mtInformation, [mbOK], 0);
    btQuestion.Enabled := False; btAnswer.Enabled := False;
  end;
end;

end.

