{**************************************}
{* Main unit for EngVerbs application *}
{**************************************}

unit enverbs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Buttons, vlist, help;

type
  TVerbsDone = array of Boolean;
  {************}
  { TfEngVerbs }
  {************}
  TfEngVerbs = class(TForm)
    mMenu: TMainMenu;
    mTest: TMenuItem;
    mTestRegIrreg, mTestYesNo, mTestConj, mTestAlt: TMenuItem;
    mTestExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsQuestions, mSettingsLItems: TMenuItem;
    mSettingsLItems5, mSettingsLItems10, mSettingsLItems15: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    memoDetails: TMemo;
    cbIrreg: TCheckBox;
    laQuestion, laInf, laPast, laParticiple: TLabel;
    edInf, edPast, edParticiple, edPast2, edParticiple2: TEdit;
    Shape1: TShape;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btCorrect: TButton;
    btFalse: TButton;
    imEval: TImage;
    tiList: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mTestRegIrregClick(Sender: TObject);
    procedure mTestYesNoClick(Sender: TObject);
    procedure mTestConjClick(Sender: TObject);
    procedure mTestAltClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsLItems5Click(Sender: TObject);
    procedure mSettingsLItems10Click(Sender: TObject);
    procedure mSettingsLItems15Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btCorrectClick(Sender: TObject);
    procedure btFalseClick(Sender: TObject);
    procedure tiListTimer(Sender: TObject);
  private
    iTest, iVerbs, iVerb, iSQuestions, iQuestions, iSList, iList, iQuestion, iCorrect: Integer;
    bAppStart: Boolean;
    aVerbs: TVerbs;
    bVerbs: TVerbsDone;
  end;

var
  fEngVerbs: TfEngVerbs;

implementation

{$R *.lfm}

{ Read verbs and verb data from files }

procedure ReadVerbs(out Verbs: TVerbs);

var
  N, P0, P1, P2, P, I: Integer;
  Line, Inf, Past, Participle: string;
  Pasts, Participles: array[1 .. 3] of string;
  InFile: Text;

begin
  for I := 0 to 3 do begin
    Pasts[I] := ''; Participles[I] := '';
  end;
  // Read list of regular verbs
  Assign(InFile, 'regular.txt'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Verbs, N);
      Verbs[N - 1].Irregular := False;
      Verbs[N - 1].VerForms := 1;
      Verbs[N - 1].Tenses[1].Infinitive := Trim(LeftStr(Line, 25));
      Verbs[N - 1].Tenses[1].SimplePast := Trim(RightStr(Line, 25));
      Verbs[N - 1].Tenses[1].PastParticiple := Verbs[N - 1].Tenses[1].SimplePast;
      Verbs[N - 1].RegularForm := True;
    end;
  end;
  Close(InFile);
  // Read list of irregular verbs
  Assign(InFile, 'irregular.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      for I := 1 to 3 do begin
        Pasts[I] := ''; Participles[I] := '';
      end;
      Inf := Trim(LeftStr(Line, 25)); Delete(Line, 1, 25);
      Past := Trim(LeftStr(Line, 25)); Delete(Line, 1, 25);
      Participle := Trim(Line);
      P1 := 0; P2 := 0;
      // Find from 1 to 3 alternate forms of simple past / past participle
      repeat
        P := Pos(',', Past); Inc(P1);
        if P > 0 then begin
          Pasts[P1] := LeftStr(Past, P - 1);
          Delete(Past, 1, P);
        end
        else
          Pasts[P1] := Past;
      until P = 0;
      repeat
        P := Pos(',', Participle); Inc(P2);
        if P > 0 then begin
          Participles[P2] := LeftStr(Participle, P - 1);
          Delete(Participle, 1, P);
        end
        else
          Participles[P2] := Participle;
      until P = 0;
      P0 := P1;
      if P2 > P1 then
        P0 := P2;
      Inc(N); SetLength(Verbs, N);
      Verbs[N - 1].Irregular := True;
      Verbs[N - 1].VerForms := P0;
      if RightStr(Inf, 1) = '*' then begin                                     // '*' at end of infinitive indicates irregular verb with regular form
        Verbs[N - 1].RegularForm := True;
        Inf := LeftStr(Inf, Length(Inf) - 1);
      end
      else
        Verbs[N - 1].RegularForm := False;
      // Fill-in all found forms of simple past and past participle
      for I := 1 to P0 do begin
        Verbs[N - 1].Tenses[I].Infinitive := Inf;
        if Pasts[I] <> '' then
          Verbs[N - 1].Tenses[I].SimplePast := Pasts[I]
        else
          Verbs[N - 1].Tenses[I].SimplePast := Pasts[1];
        if Participles[I] <> '' then
          Verbs[N - 1].Tenses[I].PastParticiple := Participles[I]
        else
          Verbs[N - 1].Tenses[I].PastParticiple := Participles[1];
      end;
    end;
  end;
  Close(InFile);
  // Read data files; they will be used with "yes/no" conjugation exercises (in order to display reasonable and incorrect verb forms)
  for I := 0 to Length(Verbs) - 1 do begin
    Verbs[I].Tenses[0].Infinitive := '';
    Verbs[I].Tenses[0].SimplePast := '';
    Verbs[I].Tenses[0].PastParticiple := '';
  end;
  // The file 'regular.dat' is supposed to correspond line by line with the verb file 'regular.txt'!
  Assign(InFile, 'regular.dat'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      // Char 1 is 'Y' or 'N', indicating if or not to double the consonant when forming a past/participle by adding -ed to the infinitive
      Verbs[N - 1].ConsonantDouble := False;
      if LeftStr(Line, 1) = 'Y' then
        Verbs[N - 1].ConsonantDouble := True;
      // Chars 3-27 (if not empty) are an incorrect form of the simple past for this verb
      // Chars 28- (if not empty) are an incorrect form of the past participle for this verb
      if Length(Line) > 3 then begin
        Verbs[N - 1].Tenses[0].Infinitive := Verbs[N - 1].Tenses[1].Infinitive;
        Verbs[N - 1].Tenses[0].SimplePast := Trim(Copy(Line, 3, 25));
        Verbs[N - 1].Tenses[0].PastParticiple := Trim(Copy(Line, 28, 25));
      end;
    end;
  end;
  Close(InFile);
  // The file 'irregular.dat' is supposed to correspond line by line with the verb file 'irregular.txt'!
  Assign(InFile, 'irregular.dat'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      // Char 1 is 'Y' or 'N', indicating if or not to double the consonant when forming a past/participle by adding -ed to the infinitive
      Verbs[N - 1].ConsonantDouble := False;
      if LeftStr(Line, 1) = 'Y' then
        Verbs[N - 1].ConsonantDouble := True;
      // Chars 3-27 (if not empty) are an incorrect form of the simple past for this verb
      // Chars 28- (if not empty) are an incorrect form of the past participle for this verb
      if Length(Line) > 3 then begin
        Verbs[N - 1].Tenses[0].Infinitive := Verbs[N - 1].Tenses[1].Infinitive;
        Verbs[N - 1].Tenses[0].SimplePast := Trim(Copy(Line, 3, 25));
        Verbs[N - 1].Tenses[0].PastParticiple := Trim(Copy(Line, 28, 25));
      end;
    end;
  end;
  Close(InFile);
end;

{ Prepare for starting a new test }

procedure NewTest(Test, SQuestions, SList: Integer; AppStart: Boolean; out Questions, List, Question, Correct: Integer);

const
  Titles: array[1 .. 4] of string = (
    'Regular/irregular verbs test', 'Tenses yes/no test', 'Tenses Conjugation test', 'Tenses alternate forms test'
  );
  Details: array[1 .. 4] of string = (
    'In a list of randomly selected verbs, find those that have a regular/irregular simple past and/or past participle.',
    'Select if the conjugated form of the simple past and the past participle of the verbs displayed are correct or not.',
    'Enter the conjugated form of the simple past and the past participle of randomly selected verbs.',
    'Enter all possibilities of the simple past and past participle of verbs, that have several conjugations for these tenses.'
  );

begin
  fEngVerbs.stTitle.Caption := Titles[Test];
  fEngVerbs.memoDetails.Lines.Clear;
  fEngVerbs.memoDetails.Lines.AddText(Details[Test]);
  // "Regular/irregular verbs" test: hide form controls (as this test runs in a different window)
  if Test = 1 then begin
    fEngVerbs.cbIrreg.Visible := False;
    fEngVerbs.laQuestion.Visible := False;
    fEngVerbs.laInf.Visible := False; fEngVerbs.edInf.Visible := False;
    fEngVerbs.laPast.Visible := False; fEngVerbs.edPast.Visible := False;
    fEngVerbs.laParticiple.Visible := False; fEngVerbs.edParticiple.Visible := False;
    fEngVerbs.edPast2.Visible := False; fEngVerbs.edParticiple2.Visible := False;
  end
  // Other tests: Hide/enable/set form controls as need
  else begin
    fEngVerbs.cbIrreg.Visible := True;
    fEngVerbs.laQuestion.Visible := True; fEngVerbs.laQuestion.Caption := 'Question';
    fEngVerbs.laInf.Visible := True; fEngVerbs.edInf.Visible := True;
    fEngVerbs.laPast.Visible := True; fEngVerbs.edPast.Visible := True;
    fEngVerbs.laParticiple.Visible := True; fEngVerbs.edParticiple.Visible := True;
    fEngVerbs.edInf.Text := ''; fEngVerbs.edPast.Text := ''; fEngVerbs.edParticiple.Text := '';
    if Test = 2 then begin
      fEngVerbs.cbIrreg.Checked := False;
      fEngVerbs.cbIrreg.Enabled := False;
    end
    else if Test = 3 then begin
      fEngVerbs.cbIrreg.Enabled := True;
    end
    else begin
      fEngVerbs.cbIrreg.Checked := True;
      fEngVerbs.cbIrreg.Enabled := False;
    end;
    if Test = 2 then begin
      fEngVerbs.edPast.TabStop := False; fEngVerbs.edPast.ReadOnly := True; fEngVerbs.edPast.Color := clForm;
      fEngVerbs.edParticiple.TabStop := False; fEngVerbs.edParticiple.ReadOnly := True; fEngVerbs.edParticiple.Color := clForm;
      fEngVerbs.btCorrect.Visible := True; fEngVerbs.btFalse.Visible := True;
      fEngVerbs.btCorrect.Enabled := False; fEngVerbs.btFalse.Enabled := False;
    end
    else begin
      fEngVerbs.edPast.TabStop := True; fEngVerbs.edPast.ReadOnly := False; fEngVerbs.edPast.Color := clDefault;
      fEngVerbs.edParticiple.TabStop := True; fEngVerbs.edParticiple.ReadOnly := False; fEngVerbs.edParticiple.Color := clDefault;
      fEngVerbs.btCorrect.Visible := False; fEngVerbs.btFalse.Visible := False;
    end;
    if Test = 4 then begin
      fEngVerbs.edPast2.Visible := True; fEngVerbs.edParticiple2.Visible := True;
      fEngVerbs.edPast2.Color := clDefault; fEngVerbs.edParticiple2.Color := clDefault;
    end
    else if Test <> 1 then begin
      fEngVerbs.edPast2.Visible := False; fEngVerbs.edParticiple2.Visible := False;
    end;
  end;
  // Reset evaluation counters
  fEngVerbs.edQuestions.Text := ''; fEngVerbs.edCorrect.Text := ''; fEngVerbs.edFalse.Text := ''; fEngVerbs.edSuccess.Text := '';
  fEngVerbs.edSuccess.Color := clForm;
  fEngVerbs.imEval.Picture.Clear;
  fEngVerbs.btQuestion.Caption := 'Start'; fEngVerbs.btQuestion.Enabled := True;
  if not AppStart then                                                                   // to avoid "Can't focus" error
    fEngVerbs.btQuestion.SetFocus;
  Questions := SQuestions; List := SList; Question := 0; Correct := 0;
end;

{ Create a "regular" simple past and/or past participle for given verb }

procedure RegularConj(Verb: TVerb; var Past, Participle: string);

var
  Infinitive: string;

begin
  Infinitive := Verb.Tenses[1].Infinitive;
  if RightStr(Infinitive, 1) = 'e' then
    // Remove ending -e in order not to get -eed
    Infinitive := LeftStr(Infinitive, Length(Infinitive) - 1);
  if Verb.ConsonantDouble then
    // If data file info tells the ending consonant should be doubled, do so
    Infinitive += RightStr(Infinitive, 1);
  // Construct the "regular" simple past (only if it had not been constructed elsewhere )
  if Past = '' then
    Past := Infinitive + 'ed';
  // Construct the "regular" simple past (only if it had not been constructed elsewhere )
  if Participle = '' then
    Participle := Infinitive + 'ed';
  // For both simple past and past participle being "regular", adapt the verb forms to avoid "to obvious" answers
  if (RightStr(Past, 2) = 'ed') and (RightStr(Participle, 2) = 'ed') then begin
    if Length(Past) > Length(Participle) then
      // Adapt conjugation to be displayed when participle ends in 1 and past in 2 (same) consonants
      Delete(Past, Length(Past) - 2, 1)
    else if Length(Past) < Length(Participle) then
      // Adapt conjugation to be displayed when past ends in 1 and participle in 2 (same) consonants
      Delete(Participle, Length(Participle) - 2, 1);
    // Adapt conjugation to be displayed when participle in -ied, when past ends in -yed
    if (Copy(Past, Length(Past) - 2, 1) = 'y') and (Copy(Participle, Length(Participle) - 2, 1) = 'i') then
      Participle[Length(Participle) - 2] := 'y';
    // Adapt conjugation to be displayed when past in -ied, when participle ends in -yed
    if (Copy(Past, Length(Past) - 2, 1) = 'i') and (Copy(Participle, Length(Participle) - 2, 1) = 'y') then
      Past[Length(Past) - 2] := 'y';
  end;
end;

{ Check answer to tenses conjugation question }

function CheckConj(Verb: TVerb; Past, Participle: string; Colour: TColor): Boolean;

var
  I: Integer;
  PastOK, ParticipleOK: Boolean;

begin
  // Transformations to avoid not accurate 'false' result
  if Participle = '' then
    Participle := '-';
  if (Verb.Tenses[1].Infinitive = 'be') and (Past = 'were') then
    Past := 'was';
  PastOK := False; ParticipleOK := False;
  // Check simple past and past participle vs. all their available forms (if one of them matches, result = true )
  for I := 1 to Verb.VerForms do begin
    if Past = Verb.Tenses[I].SimplePast then
      PastOK := True;
    if Participle = Verb.Tenses[I].PastParticiple then
      ParticipleOK := True;
  end;
  // Color fields with false conjugation
  if not PastOK then
    fEngVerbs.edPast.Color := Colour;
  if not ParticipleOK then
    fEngVerbs.edParticiple.Color := Colour;
  // Result = correct if both simple past and past participle are correct
  CheckConj := PastOK and ParticipleOK;
end;

{ Check answer to tenses yes/no question }

function CheckYesNo(Verb: TVerb; Past, Participle: string; Answer: Boolean): Boolean;

var
  OK: Boolean;

begin
  // Answer is correct if it is equal (true/false) to the result given by checking the dislayed conjugated forms
  if Answer = CheckConj(Verb, Past, Participle, clYellow) then
    OK := True
  else
    OK := False;
  CheckYesNo := OK;
end;

{ Check answer to alternate conjugation forms question }

function CheckAlt(Verb: TVerb; Past, Participle, Past2, Participle2: string): Boolean;

var
  PastOK, ParticipleOK: Boolean;

begin
  if Participle2 = '' then
    Participle2 := Participle;
  if Past2 = '' then
    Past2 := Past;
  PastOK := False; ParticipleOK := False;
  // Both forms of the past must be correct
  if (Past = Verb.Tenses[1].SimplePast) and (Past2 = Verb.Tenses[2].SimplePast) then
    PastOK := True
  else if (Past2 = Verb.Tenses[1].SimplePast) and (Past = Verb.Tenses[2].SimplePast) then
    PastOK := True;
  // Both forms of the participle must be correct
  if (Participle = Verb.Tenses[1].PastParticiple) and (Participle2 = Verb.Tenses[2].PastParticiple) then
    ParticipleOK := True
  else if (Participle2 = Verb.Tenses[1].PastParticiple) and (Participle = Verb.Tenses[2].PastParticiple) then
    ParticipleOK := True;
  // Color mistakes in red, repeated (correct) conjugated form in magenta
  if not PastOK then begin
    if (Past <> Verb.Tenses[1].SimplePast) and (Past <> Verb.Tenses[2].SimplePast) then
      fEngVerbs.edPast.Color := clRed;
    if (Past2 <> Verb.Tenses[1].SimplePast) and (Past2 <> Verb.Tenses[2].SimplePast) then
      fEngVerbs.edPast2.Color := clRed;
    if (fEngVerbs.edPast.Color = clDefault) and (fEngVerbs.edPast2.Color = clDefault) and (Past = Past2) then
      fEngVerbs.edPast2.Color := clFuchsia;
  end;
  if not ParticipleOK then begin
    if (Participle <> Verb.Tenses[1].PastParticiple) and (Participle <> Verb.Tenses[2].PastParticiple) then
      fEngVerbs.edParticiple.Color := clRed;
    if (Participle2 <> Verb.Tenses[1].PastParticiple) and (Participle2 <> Verb.Tenses[2].PastParticiple) then
      fEngVerbs.edParticiple2.Color := clRed;
    if (fEngVerbs.edParticiple.Color = clDefault) and (fEngVerbs.edParticiple2.Color = clDefault) and (Participle = Participle2) then
      fEngVerbs.edParticiple2.Color := clFuchsia;
  end;
  // Result is correct if all past and all participle forms are correct
  CheckAlt := PastOK and ParticipleOK;
end;

{ Display evaluation counters }

procedure DisplayEval(Question, Correct: Integer);

var
  P: Integer;

begin
  fEngVerbs.edQuestions.Text := IntToStr(Question);
  fEngVerbs.edCorrect.Text := IntToStr(Correct);
  fEngVerbs.edFalse.Text := IntToStr(Question - Correct);
  P := Round(100 * (Correct) / Question);
  fEngVerbs.edSuccess.Text := IntToStr(P) + '%';
  // Usage of different colors deending on success percentage
  if P >= 60 then
    fEngVerbs.edSuccess.Color := clLime
  else if P >= 50 then
    fEngVerbs.edSuccess.Color := clYellow
  else
    fEngVerbs.edSuccess.Color := clRed;
end;

{ Update (and display) evaluation counters }

procedure UpdateEval(AnswerOK: Boolean; Question: Integer; var Correct: Integer);

begin
  if AnswerOK then begin
    Inc(Correct);
    fEngVerbs.imEval.Picture.LoadFromFile('correct.jpg');
  end
  else begin
    fEngVerbs.imEval.Picture.LoadFromFile('false.jpg');
  end;
  DisplayEval(Question, Correct);
end;

{ Display end of test message }

procedure EndOfTest;

var
  S: string;

begin
  S := 'All questions of the test have been done' + LineEnding;
  S += 'Please, use the "New" menu items to start a new one...';
  MessageDlg('End of test', S, mtInformation, [mbOK], 0);
  // User will have to start a new test
  fEngVerbs.btQuestion.Caption := 'Start';
  fEngVerbs.btQuestion.Enabled := False;
end;

{************}
{ TfEngVerbs }
{************}

{ Application start: Initialisation }

procedure TfEngVerbs.FormCreate(Sender: TObject);

begin
  bAppStart := True;
  SetLength(aVerbs, 0);
  ReadVerbs(aVerbs); iVerbs := Length(aVerbs);
  SetLength(bVerbs, iVerbs);
  iTest := 3; iSQuestions := 20; iSList := 10;
  // Prepare for default exercises type test
  NewTest(iTest, iSQuestions, iSList, bAppStart, iQuestions, iList, iQuestion, iCorrect); bAppStart := False;
  Randomize;
end;

{ Prepare for "regular/irregular verbs" test }


procedure TfEngVerbs.mTestRegIrregClick(Sender: TObject);

begin
  iTest := 1;
  NewTest(iTest, iSQuestions, iSList, bAppStart, iQuestions, iList, iQuestion, iCorrect);
  fEngVerbs.btQuestion.Enabled := False;
  // Simulate a click on "Start" button; this will open the "regular/irregular verbs" test window
  fEngVerbs.btQuestion.Click;
end;

{ Prepare for "tenses yes/no" test }

procedure TfEngVerbs.mTestYesNoClick(Sender: TObject);

begin
  iTest := 2;
  NewTest(iTest, iSQuestions, iSList, bAppStart, iQuestions, iList, iQuestion, iCorrect);
end;

{ Prepare for "tenses conjugation" test }

procedure TfEngVerbs.mTestConjClick(Sender: TObject);

begin
  iTest := 3;
  NewTest(iTest, iSQuestions, iSList, bAppStart, iQuestions, iList, iQuestion, iCorrect);
end;

{ Prepare for "tenses alternate forms" test }

procedure TfEngVerbs.mTestAltClick(Sender: TObject);

begin
  iTest := 4;
  NewTest(iTest, iSQuestions, iSList, bAppStart, iQuestions, iList, iQuestion, iCorrect);
end;

{ Exit the application }

procedure TfEngVerbs.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Enter number of test questions }

procedure TfEngVerbs.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('English verbs test', 'Number of questions', IntToStr(iSQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                    // minimum of questions arbitrarily fixed to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);                                              // temporarily variable will be active if a new test is started
  end;
end;

{ Enter number of items in the "regular/irregular verbs" test verbs list }

procedure TfEngVerbs.mSettingsLItems5Click(Sender: TObject);

begin
  mSettingsLItems5.Checked := True;
  mSettingsLItems10.Checked := False;
  mSettingsLItems15.Checked := False;
  iSList := 5;                                                                 // temporarily variable will be active if a new test is started
end;

procedure TfEngVerbs.mSettingsLItems10Click(Sender: TObject);

begin
  mSettingsLItems5.Checked := False;
  mSettingsLItems10.Checked := True;
  mSettingsLItems15.Checked := False;
  iSList := 10;
end;

procedure TfEngVerbs.mSettingsLItems15Click(Sender: TObject);

begin
  mSettingsLItems5.Checked := False;
  mSettingsLItems10.Checked := False;
  mSettingsLItems15.Checked := True;
  iSList := 15;
end;

{ Dislay program help text }

procedure TfEngVerbs.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else begin
    fHelp.memoHelp.Lines.Clear;
    fHelp.memoHelp.Lines.LoadFromFile('help.txt');
    fHelp.Show;
  end;
end;

{ Display program about text }

procedure TfEngVerbs.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Language trainer: English verbs exercice generator.' + LineEnding;
  S += 'Version 1.0, © allu, January-February 2019.';
  MessageDlg('About "EngVerbs"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new exercise resp. check user answer}

procedure TfEngVerbs.btQuestionClick(Sender: TObject);

var
  R, I: Integer;
  Past, Participle: string;
  OK, Correct: Boolean;

begin
  // Button "Start/Question": Generate new exercise
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    Inc(iQuestion);
    // "Regular/irregular verbs" test
    if iTest = 1 then begin
      // Pass data to fList form and open this form
      // All code relative of this kind of text in the vlist unit; the vlist unit code will set
      // variables, read by the main form's timer routine to update evaluation and close the window
      fList.iVerbs := iVerbs;
      fList.iQuestions := iQuestions;
      fList.iList := iList;
      fList.aVerbs := aVerbs;
      fList.rbReg.Enabled := True; fList.rbIrreg.Enabled := True; fList.cbInclude.Enabled := True;
      fList.lbVerbs.Clear; fList.lbAnswers.Clear;
      fList.btVerbAdd.Enabled := False; fList.btVerbRemove.Enabled := False;
      fList.imEval.Picture.Clear;
      fList.bDoneQuestion := False; fList.bDoneAll := False; fList.bDoneTest := False;
      fList.btQuestion.Caption := 'Start';
      tiList.Enabled := True;
      fList.ShowModal;
    end
    // All other exercise types tests
    else begin
      if btQuestion.Caption = 'Start' then begin
        for I := 0 to iVerbs - 1 do
          bVerbs[I] := False;
      end;
      laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions);
      // Take a random verb
      repeat
        OK := True;
        iVerb := Random(iVerbs);
        if bVerbs[iVerb] then
          // Use verbs only once
          OK := False
        else begin
          if iTest = 3 then begin
            // For "tenses conjugation" test, consider the "irregular verbs only" checkbox
            if cbIrreg.Checked and not aVerbs[iVerb].Irregular then
              OK := False;
          end
          else if iTest = 4 then begin
            // For "tenses alternate forms" test, use only verbs with 2 forms of past and/or participle
            if aVerbs[iVerb].VerForms <> 2 then
              OK := False;
          end;
        end;
      until OK;
      bVerbs[iVerb] := True;                                                   // mark this verb as 'done'
      edInf.Text := aVerbs[iVerb].Tenses[1].Infinitive;
      edPast.Text := ''; edParticiple.Text := '';
      edPast2.Text := ''; edParticiple2.Text := '';
      // Reset colors of input fields
      if (iTest = 3) or (iTest = 4) then begin
        edPast.Color := clDefault; edParticiple.Color := clDefault;
        edPast2.Color := clDefault; edParticiple2.Color := clDefault;
      end
      else begin
        edPast.Color := clForm; edParticiple.Color := clForm;
        edPast2.Color := clForm; edParticiple2.Color := clForm;
      end;
      fEngVerbs.imEval.Picture.Clear;
      // "Tenses yes/no" test
      if iTest = 2 then begin
        Past := ''; Participle := '';
        if aVerbs[iVerb].Tenses[0].Infinitive <> '' then
          // If the verb has "wrong" conjugated forms, consider to use them
          R := Random(8)
        else
          R := Random(6);
        // Randomly choose a mix of correct and false conjugated forms (displayed by the program)
        if (R = 0) or (R = 1) then begin
          // Create "regular" past and participle
          RegularConj(aVerbs[iVerb], Past, Participle);
        end
        else if R = 2 then begin
          // Use one of the existing "real" forms for the particile
          R := Random(aVerbs[iVerb].VerForms) + 1;
          Participle := aVerbs[iVerb].Tenses[R].PastParticiple;
          // Create "regular" past
          RegularConj(aVerbs[iVerb], Past, Participle);
        end
        else if R = 3 then begin
          // Use one of the existing "real" forms for the past
          R := Random(aVerbs[iVerb].VerForms) + 1;
          Past := aVerbs[iVerb].Tenses[R].SimplePast;
          // Create "regular" participle
          RegularConj(aVerbs[iVerb], Past, Participle);
        end
        else if (R = 4) or (R = 5) then begin
          // Use one of the existing "real" forms for the past and the participle
          R := Random(aVerbs[iVerb].VerForms) + 1;
          Past := aVerbs[iVerb].Tenses[R].SimplePast;
          R := Random(aVerbs[iVerb].VerForms) + 1;
          Participle := aVerbs[iVerb].Tenses[R].PastParticiple;
        end
        else begin
          // Use the "wrong" forms from the data files for the past and the participle
          Past := aVerbs[iVerb].Tenses[0].SimplePast;
          Participle := aVerbs[iVerb].Tenses[0].PastParticiple;
        end;
        edPast.Text := Past; edParticiple.Text := Participle;
      end;
    end;
    // For "regular/irregular verbs" and "tenses yes/no" test, the "Answer" button will not be used
    if (iTest = 1) or (iTest = 2) then begin
      btQuestion.Enabled := False;
      if iTest = 2 then begin
        // For "tenses yes/no" test, use the "Correct" and the "False" buttons
        btCorrect.Enabled := True; btFalse.Enabled := True;
      end;
    end
    // For "tenses conjugation" and "tenses alternate forms" test, the "Answer" button will be used
    else
      btQuestion.Caption := 'Answer';
    // Set focus depending on exercises type test
    if iTest = 2 then
      btCorrect.SetFocus
    else if (iTest = 3) or (iTest = 4) then
      edPast.SetFocus;
  end
  // Button "Answer": Check user answer
  // This applies to "tenses conjugation" and "tenses alternate forms" test only
  // For "regular/irregular verbs" test, running in the fList form, the code in unit vlist applies
  // For "tenses yes/no" test, the "Correct" and "False" buttons will be used to give the user's answer
  else begin
    Correct := False;
    // "Tenses conjugation" test
    if iTest = 3 then
      Correct := CheckConj(aVerbs[iVerb], edPast.Text, edParticiple.Text, clRed)
    // "Tenses alternate forms" test
    else
      Correct := CheckAlt(aVerbs[iVerb], edPast.Text, edParticiple.Text, edPast2.Text, edParticiple2.Text);
    // Update evaluation counters
    UpdateEval(Correct, iQuestion, iCorrect);
    // Set button caption for next question
    btQuestion.Caption := 'Question';
    // If all questions have been done, display end of test message
    if iQuestion = iQuestions then
      EndOfTest
    else
      btQuestion.Enabled := True;
  end;
end;

{ Buttons "Correct" and "False": Check user's answer; update evaluation; display message if all questions done }

procedure TfEngVerbs.btCorrectClick(Sender: TObject);

var
  Correct: Boolean;

begin
  Correct := CheckYesNo(aVerbs[iVerb], edPast.Text, edParticiple.Text, True);
  UpdateEval(Correct, iQuestion, iCorrect);
  btCorrect.Enabled := False; btFalse.Enabled := False;
  btQuestion.Caption := 'Question';
  if iQuestion = iQuestions then
    EndOfTest
  else
    btQuestion.Enabled := True;
end;

procedure TfEngVerbs.btFalseClick(Sender: TObject);

var
  Correct: Boolean;

begin
  Correct := CheckYesNo(aVerbs[iVerb], edPast.Text, edParticiple.Text, False);
  UpdateEval(Correct, iQuestion, iCorrect);
  btCorrect.Enabled := False; btFalse.Enabled := False;
  btQuestion.Caption := 'Question';
  if iQuestion = iQuestions then
    EndOfTest
  else
    btQuestion.Enabled := True;
end;

{ Timer routine: Read variables on fList form and take appropriate action }

procedure TfEngVerbs.tiListTimer(Sender: TObject);

begin
  // Variable bDoneQuestion = true: Display evaluation counters
  if fList.bDoneQuestion then begin
    DisplayEval(fList.iQuestion, fList.iCorrect);
    fList.bDoneQuestion := False;
  end;
  // Variable bDoneAll = true: Stop the timer and close the form
  if fList.bDoneAll then begin
    tiList.Enabled := False;
    if fList.bDoneTest then
      // If bDoneTest = true: Display end of test message (if not, user pushed the "Close" button)
      EndOfTest;
    fList.Close;
  end;
end;

end.

