{*****************************************}
{* Main unit for Luxembourg1 application *}
{*****************************************}

unit luxbg1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, ExtCtrls, LazUTF8, emblems;

type
  TTownship = record
    Township, Canton, Ward: string;
    Inhabitants: Integer;
  end;
  TTownships = array of TTownship;
  TTownshipsDone = array of Boolean;
  {**********}
  { TfLuxbg1 }
  {**********}
  TfLuxbg1 = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuiz1, mQuiz2, mQuiz3, mQuiz4, mQuizExit: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laQuiz, laQuestion, Label1, Label2: TLabel;
    edQuestion: TEdit;
    cobCanton, cobInhabitants, cobWards: TComboBox;
    imEmblem: TImage;
    btEmblem: TButton;
    edQuestions, edEval: TEdit;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mQuiz1Click(Sender: TObject);
    procedure mQuiz2Click(Sender: TObject);
    procedure mQuiz3Click(Sender: TObject);
    procedure mQuiz4Click(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btEmblemClick(Sender: TObject);
  private
    iQuiz, iCanton, iTownship, iQuestions, iQuestion, iCorrect: Integer;
    sCanton: string;
    bStart: Boolean;
    aCantons: array[0..11] of string;
    bCantonsDone: array[0..11] of Boolean;
    aTownships: TTownships;
    bTownshipsDone: TTownshipsDone;
  end;

var
  fLuxbg1: TfLuxbg1;

implementation

{$R *.lfm}

{ Right-align stringgrid (evaluation) values }

function GridFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN += S;                                                                        // adding the '%' sign
  Result := SN;
end;

{ Read canton and township data from text file }

procedure ReadTownships(out Cantons: array of string; out Townships: TTownships);

var
  N, M: Integer;
  Line, Ward, OldCanton: string;
  InFile: Text;

begin
  Assign(InFile, 'Gemengen.txt'); Reset(InFile);
  N := 0; M := 0; OldCanton := '';
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin                        // lines starting with '#' are comments
      Inc(N); SetLength(Townships, N);
      Townships[N - 1].Township := UTF8Trim(UTF8Copy(Line, 1, 20));                 // chars  1-20: township
      Townships[N - 1].Canton := UTF8Trim(UTF8Copy(Line, 21, 15));                  // chars 21-35: canton
      Townships[N - 1].Inhabitants := StrToInt(UTF8Copy(Line, 36, 6));              // chars 36-41: inhabitants (6-digit field)
      case RightStr(Line, 1)[1] of                                                  // char     43: first letter of ward
        'N': Ward := 'Norden';
        'O': Ward := 'Osten';
        'S': Ward := 'Süden';
        'Z': Ward := 'Zentrum';
      end;
      Townships[N - 1].Ward := Ward;
      // Fill canton array (to be used with "emblems quiz")
      if Townships[N - 1].Canton <> OldCanton then begin
        // If canton (cantons ordered in the file) changes, it's another canton and has to be inserted into the array
        Inc(M);
        Cantons[M - 1] := Townships[N - 1].Canton;
        OldCanton := Cantons[M - 1];
      end;
    end;
  end;
  Close(InFile);
end;

{ Starting a new quiz }

procedure NewQuiz(Quiz: Integer; out CantonsDone: array of Boolean; var TownshipsDone: TTownshipsDone; out Question, Correct: Integer);

const
  QuizTitles: array[1..4] of string = (
    'Rode vum Kanton an deem d''Gemenge leien',
    'Rode vun der Awunnerzuel vun de Gemengen',
    'Rode vum Wahlbezierk zu deem d''Gemenge gehéieren',
    'Rode vum Wopen, dat zu de Kantone gehéiert'
  );

var
  I: Integer;

begin
  fLuxbg1.laQuiz.Caption := QuizTitles[Quiz] + '.';
  fLuxbg1.laQuestion.Caption := 'Fro';
  fLuxbg1.btEmblem.Visible := False; fLuxbg1.imEmblem.Visible := False;
  fLuxbg1.edQuestion.Text := '';
  fLuxbg1.edEval.Text := ''; fLuxbg1.edEval.Color := clDefault;
  fLuxbg1.edQuestions.ReadOnly := False;
  // Show/hide the appropriate controls (depending on quiz type)
  case Quiz of
    1: begin
         fLuxbg1.cobCanton.Visible := True; fLuxbg1.cobCanton.ItemIndex := 0;
         fLuxbg1.cobInhabitants.Visible := False; fLuxbg1.cobWards.Visible := False;
    end;
    2: begin
         fLuxbg1.cobInhabitants.Visible := True; fLuxbg1.cobInhabitants.ItemIndex := 0;
         fLuxbg1.cobCanton.Visible := False; fLuxbg1.cobWards.Visible := False;
    end;
    3: begin
        fLuxbg1.cobWards.Visible := True; fLuxbg1.cobWards.ItemIndex := 0;
        fLuxbg1.cobCanton.Visible := False; fLuxbg1.cobInhabitants.Visible := False;
    end;
    4: begin
        fLuxbg1.cobCanton.Visible := False; fLuxbg1.cobInhabitants.Visible := False; fLuxbg1.cobWards.Visible := False;
        fLuxbg1.btEmblem.Visible := True; fLuxbg1.btEmblem.Enabled := False;        // button to open the window with the emblems
        fLuxbg1.imEmblem.Visible := True; fLuxbg1.imEmblem.Picture.Clear;           // image of the emblem, the user will choose
        fLuxbg1.edQuestions.Text := '12'; fLuxbg1.edQuestions.ReadOnly := True;     // always 12 questions for the 12-emblems quiz
    end;
  end;
  // Clear the evaluation grid
  for I := 0 to 3 do
    fLuxbg1.sgEval.Cells[1, I] := '';
  // Set all fields of "cantons/townships already done" array to false
  for I := 0 to Length(CantonsDone) - 1 do
    CantonsDone[I] := False;
  for I := 0 to Length(TownshipsDone) - 1 do
    TownshipsDone[I] := False;
  // Reset counters
  Question := 0; Correct := 0;
  // Enable button for new quiz
  fLuxbg1.btQuestion.Enabled := True; fLuxbg1.btQuestion.Caption := 'Fro';
end;

{**********}
{ TfLuxbg1 }
{**********}

{ Application start: Initialisation }

procedure TfLuxbg1.FormCreate(Sender: TObject);

begin
  ReadTownships(aCantons, aTownships);                                              // read quiz data from file
  SetLength(bTownshipsDone, Length(aTownships));
  bStart := True;                                                                   // start of application flag (used in FormActivate method)
  Randomize;
  mQuiz1.Click;                                                                     // start new quiz by simulating a button push
end;

{ (First) window activation: Pass canton data to fEmblems form }

procedure TfLuxbg1.FormActivate(Sender: TObject);

// The problem with passing data from the main form to a secondary form at application start, is that the secondary form has not yet
// been created at that moment and trying to do this in the FormCreate method would result in an application abortion. A workaround is
// to do it in the FormActivate method (the main window shows up only after all forms have actually been created). If you want to avoid
// that the code is executed each time, the window is activated (i.e. to ensure, that it's executed only once), you can use a Boolean to
// tell the FormActivate method, if it's the first time or not, that the window is activated (if it is application start or not)

begin
  if bStart then begin
    fEmblems.aCantons := aCantons;
    bStart := False;
  end;
end;

{ Menu item "Quiz > Neie Kanton-Quiz": Start a new canton quiz }

procedure TfLuxbg1.mQuiz1Click(Sender: TObject);

begin
  iQuiz := 1;
  NewQuiz(iQuiz, bCantonsDone, bTownshipsDone, iQuestion, iCorrect);
  if not bStart then
    fLuxbg1.btQuestion.SetFocus;
end;

{ Menu item "Quiz > Neien Awunner-Quiz": Start a new inhabitants quiz }

procedure TfLuxbg1.mQuiz2Click(Sender: TObject);

begin
  iQuiz := 2;
  NewQuiz(iQuiz, bCantonsDone, bTownshipsDone, iQuestion, iCorrect);
  if not bStart then
    fLuxbg1.btQuestion.SetFocus;
end;

{ Menu item "Quiz > Neie Wahlbezierk-Quiz": Start a new ward quiz }

procedure TfLuxbg1.mQuiz3Click(Sender: TObject);

begin
  iQuiz := 3;
  NewQuiz(iQuiz, bCantonsDone, bTownshipsDone, iQuestion, iCorrect);
  if not bStart then
    fLuxbg1.btQuestion.SetFocus;
end;

{ Menu item "Quiz > Neie Wope-Quiz": Start a new emblem quiz }

procedure TfLuxbg1.mQuiz4Click(Sender: TObject);

begin
  iQuiz := 4;
  NewQuiz(iQuiz, bCantonsDone, bTownshipsDone, iQuestion, iCorrect);
  if not bStart then
    fLuxbg1.btQuestion.SetFocus;
end;

{ Menu item "Quiz > Verloossen": Exit application }

procedure TfLuxbg1.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Iwwer": Display application about }

procedure TfLuxbg1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Quiz: Lëtzebuerger Kantonen a Gemengen.' + LineEnding;
  S += 'Rode vum Kanton, der Awunnerzuel oder dem Wahlbezierk vun de lëtzebuerger Gemengen. Rode vum Wope vun de Kantonen.' + LineEnding + LineEnding;
  S += 'Versioun 2.0, © allu, Oktober 2019 - November 2020.';
  MessageDlg('Iwwer "Luxembourg1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Fro/Äntwert": Generate new question resp. check user answer }

procedure TfLuxbg1.btQuestionClick(Sender: TObject);

var
  Inhabitants, UserInhabitants1, UserInhabitants2, Eval, P: Integer;
  Answer, UserAnswer, S: string;
  IsCorrect: Boolean;

begin
  // Button "Fro": Generate new question
  if btQuestion.Caption = 'Fro' then begin
    // Get number of questions for this quiz (from form)
    if edQuestions.Text = '' then
      iQuestions := 0
    else
      iQuestions := StrToInt(edQuestions.Text);
    if iQuestions < 10 then begin
      iQuestions := 10; edQuestions.Text := '10';                                   // minimum of questions arbitrarily fixed to 10
    end;
    fLuxbg1.edQuestions.ReadOnly := True;                                           // do not allow changing number of questions during quiz
    Inc(iQuestion);
    laQuestion.Caption := 'Fro ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + '.';
    if iQuiz = 4 then begin
      // Emblem quiz: Choose a random canton
      repeat
        iCanton := Random(Length(aCantons));                                        // do not use cantons already done
      until not bCantonsDone[iCanton];
      bCantonsDone[iCanton] := True;                                                // mark canton as 'done'
      edQuestion.Text := aCantons[iCanton];
      btEmblem.Enabled := True; btEmblem.SetFocus;                                  // enable the "Wopen" button
      imEmblem.Picture.Clear;
      sCanton := '';                                                                // canton corr. to emblem, that user will select
    end
    else begin
      // Other quizes: Choose a random township
      repeat
        iTownship := Random(Length(aTownships));                                    // do not use townships already done
      until not bTownshipsDone[iTownship];
      bTownshipsDone[iTownship] := True;                                            // mark township as 'done'
      edQuestion.Text := aTownships[iTownship].Township;
      btQuestion.SetFocus;
    end;
    fLuxbg1.edEval.Text := ''; fLuxbg1.edEval.Color := clDefault;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Äntwert';
  end
  // Button "Äntwert": Check user answer
  else begin
    IsCorrect := False;
    if iQuiz in [1, 3] then begin
      // For canton and ward quiz, just compare user answer and actual answer (as strings)
      if iQuiz = 1 then begin
        // Canton quiz
        Answer := aTownships[iTownship].Canton;
        if cobCanton.ItemIndex = -1 then
          UserAnswer := ''
        else
          UserAnswer := cobCanton.Items[cobCanton.ItemIndex];
      end
      else begin
        // Ward quiz
        Answer := aTownships[iTownship].Ward;
        if cobWards.ItemIndex = -1 then
          UserAnswer := ''
        else
          UserAnswer := cobWards.Items[cobWards.ItemIndex];
      end;
      if UserAnswer = Answer then
        IsCorrect := True;
    end
    else if iQuiz = 2 then begin
      // For inhabitants quiz, check if actual answer is number between the 2 limits given in the user selected combobox item
      Inhabitants := aTownships[iTownship].Inhabitants;
      if cobInhabitants.ItemIndex = -1 then
        UserAnswer := ''
      else
        UserAnswer := cobInhabitants.Items[cobInhabitants.ItemIndex];
      UserAnswer := StringReplace(UserAnswer, '.', '', [rfReplaceAll]);             // remove the 'thousand' markers
      // Check for 'less than'
      P := Pos('manner wéi ', UserAnswer);
      if P > 0 then begin
        UserAnswer := StringReplace(UserAnswer, 'manner wéi ', '', []);
        UserInhabitants1 := 0; UserInhabitants2 := StrToInt(UserAnswer);
      end
      else begin
        // Check for 'greater than'
        P := Pos('iwwer ', UserAnswer);
        if P > 0 then begin
          UserAnswer := StringReplace(UserAnswer, 'iwwer ', '', []);
          UserInhabitants1 := StrToInt(UserAnswer); UserInhabitants2 := MaxInt;
        end
        else begin
          // Normal case: 2 limit numbers
          P := Pos(' bis ', UserAnswer);
          UserInhabitants1 := StrToInt(Copy(UserAnswer, 1, P - 1)); UserInhabitants2 := StrToInt(Copy(UserAnswer, P + 5, Length(UserAnswer)));
        end;
      end;
      if (Inhabitants > UserInhabitants1) and (Inhabitants <= UserInhabitants2) then
        IsCorrect := True;
    end
    else begin
      // For emblem quiz, check if the actual canton is equal to the canton corresponding to the emblem selected by user
      btEmblem.Enabled := False;                                                    // block access to "Wopen" button after answer has been given
      if (sCanton <> '') and (sCanton = aCantons[iCanton]) then
        IsCorrect := True;
    end;
    // Evaluation
    if IsCorrect then begin
      Inc(iCorrect);
      edEval.Text := 'Dat ass richteg!';
      edEval.Color := clLime;
    end
    else begin
      edEval.Text := 'Falsch! ';
      if iQuiz <> 4 then
        edEval.Text := edEval.Text + aTownships[iTownship].Township + ' ';
      if iQuiz = 1 then
        edEval.Text := edEval.Text + 'läit am Kanton ' + Answer + '.'
      else if iQuiz = 2 then
        edEval.Text := edEval.Text + 'huet ' + FloatToStrF(Inhabitants, ffNumber, 0, 0) + ' Awunner.'
      else if iQuiz = 3 then
        edEval.Text := edEval.Text + 'gehéiert zum Wahlbezierk ' + Answer + '.'
      else begin
        if sCanton <> '' then begin
          edEval.Text := edEval.Text + 'Dat ausgewielte Wopen ass dat vun ' + sCanton;
          if LeftStr(sCanton, 1)[1] in ['B', 'C', 'F', 'G', 'J', 'K', 'L', 'M', 'P', 'Q', 'R', 'S', 'V', 'W', 'X', 'Y'] then
            edEval.Text := StringReplace(edEval.Text, 'vun', 'vu', []);
        end;
      end;
      edEval.Color := clRed;
    end;
    // Evaluation grid fill-in
    sgEval.Cells[1, 0] := GridFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GridFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GridFormat(iQuestion - iCorrect, '');
    Eval := Round(100 * (iCorrect / iQuestion));
    sgEval.Cells[1, 3] := GridFormat(Eval, '%');
    // Next button push will be to generate a new question
    btQuestion.Caption := 'Fro'; btQuestion.SetFocus;
    // All question done: Display message, depending on evaluation result
    if iQuestion = iQuestions then begin
      if Eval >= 80 then
        S := 'Bravo! Do kann ee wohl soen: Een dee sech mat de lëtzebuerger Gemengen auskennt!'
      else if Eval >= 60 then
        S := 'Net schlecht a mat bëssen üben ass bestëmmt nach méi dran!'
      else if Eval >= 50 then
        S := 'Naja, dat geet! Awer firwat net bëssen üben an eis Gemenge richteg kenne léieren?'
      else if Eval >= 30 then
        S := 'An der Schoul wir dat eng Genügend, awer weider übe wier schon ubruecht, oder?'
      else if Eval >= 10 then
        S := 'Eng Datz nennt een dat an der Schoul! Dat eenzegt dat do hëlleft, ass weider üben!'
      else
        S := 'Kee Grond fir opzeginn! De Courage an zwou Hänn huelen a weider üben!';
      if iQuiz = 4 then begin
        S := StringReplace(S, 'lëtzebuerger Gemengen', 'Wope vun de lëtzebuerger Kantonen', []);
        S := StringReplace(S, 'an eis Gemenge', 'an d''Wope vun eise Kantone', []);
      end;
      MessageDlg('Enn vum Quiz"', S, mtInformation, [mbOK], 0);
      edQuestions.ReadOnly := False;                                                // number of questions may be changed again now
      btQuestion.Enabled := False;                                                  // block access to button, until a new quiz is started
    end;
  end;
end;

{ Button "Wopen": Open "emblem selection" window and get answer (canton corr. to emblem selected) from fEmblems form }

procedure TfLuxbg1.btEmblemClick(Sender: TObject);

var
  FilePath: string;

begin
  fEmblems.ShowModal;
  sCanton := fEmblems.sCanton;                                                      // canton corr. to emblem selected = user answer
  if sCanton <> '' then begin
    Filepath := './Wopen/' + sCanton + '.jpg'; DoDirSeparators(Filepath);          // display emblem selected on main form
    imEmblem.Picture.LoadFromFile(FilePath);
  end;
end;

end.

