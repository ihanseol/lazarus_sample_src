{*****************************************}
{* Main unit for Luxembourg2 application *}
{*****************************************}

unit luxbg2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8;

type
  TLocal = record
    Local, LocalFr, LocalDe, Township, Canton: string[25];
    Ward: Char;
  end;
  TLocals = array of TLocal;
  TLocalsFile = file of TLocal;
  TLocalsDone = array of Boolean;
  {**********}
  { TfLuxbg2 }
  {**********}
  TfLuxbg2 = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizLocalLux, mQuizLocalGerFra, mQuizTownship, mQuizCanton: TMenuItem;
    mQuizWard, mQuizExit, mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laQuestion, laLocal1, laLocal2, Label2, Label3: TLabel;
    edLocal0, edLocal0b, edLocal1, edLocal2, edEval: TEdit;
    cobLocal1: TComboBox;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btEAigu: TButton;
    btETrema: TButton;
    btAE: TButton;
    btOE: TButton;
    btUE: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizLocalLuxClick(Sender: TObject);
    procedure mQuizLocalGerFraClick(Sender: TObject);
    procedure mQuizTownshipClick(Sender: TObject);
    procedure mQuizCantonClick(Sender: TObject);
    procedure mQuizWardClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btEAiguClick(Sender: TObject);
    procedure btETremaClick(Sender: TObject);
    procedure btAEClick(Sender: TObject);
    procedure btOEClick(Sender: TObject);
    procedure btUEClick(Sender: TObject);
    procedure edLocal1Enter(Sender: TObject);
    procedure edLocal2Enter(Sender: TObject);
  private
    iQuiz, iLocal, iQuestion, iCorrect: Integer;
    sEditField: string;                                                        // variable to keep track of actually edited input field
    aLocals: TLocals;
    aLocalsDone: TLocalsDone;
  end;

var
  fLuxbg2: TfLuxbg2;


implementation

{$R *.lfm}

{ Format values for the grid (right-align) }

function GridFormat(N: Integer; S: string): string;

var
  NS: string;

begin
  NS := ' ' + IntToStr(N);
  if N < 10 then
    NS := '  ' + NS
  else if N < 100 then
    NS := ' ' + NS;
  if S = '' then
    NS := ' ' + NS
  else
    NS += S;                                                                   // this adds the '%' sign
  Result := NS;
end;

{ Read locals from luxbg2.dat file }

procedure ReadLocals(out Locals: TLocals);

var
  N: Integer;
  InFile: TLocalsFile;

begin
  Assign(InFile, 'luxbg2.dat'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Inc(N); SetLength(Locals, N);
    Read(InFile, Locals[N - 1]);
  end;
  Close(InFile);
end;

{ Prepare for a new quiz }

procedure NewQuiz(Quiz: Integer; out EdtField: string);

const
  Titles: array[1..5] of string = (
    'Lëtzebuerger Numm', 'Däitsche/franséische Numm', 'Numm vun der Gemeng', 'Numm vum Kanton', 'Landesdeel (Wahlbezierk)'
  );

var
  I: Integer;

begin
  fLuxbg2.stTitle.Caption := 'Uertschafte Quiz: ' + Titles[Quiz] + '.';
  fLuxbg2.laQuestion.Caption := 'Fro';
  fLuxbg2.edLocal0b.Visible := False;
  fLuxbg2.laLocal2.Visible := False; fLuxbg2.edLocal2.Visible := False;
  fLuxbg2.edLocal1.Visible := True; fLuxbg2.cobLocal1.Visible := False;
  fLuxbg2.edLocal0.Text := ''; fLuxbg2.edLocal0b.Text := '';
  fLuxbg2.edLocal1.Text := ''; fLuxbg2.edLocal2.Text := '';
  fLuxbg2.edEval.Text := '';
  // Controls visible or not and labels with appropriate caption, depending on actual quiz
  case Quiz of
    1: begin
         fLuxbg2.edLocal0b.Visible := True;
         fLuxbg2.laLocal1.Caption := 'Lëtzebuerger Numm';
       end;
    2: begin
         fLuxbg2.laLocal1.Caption := 'Däitsche Numm';
         fLuxbg2.laLocal2.Visible := True; fLuxbg2.edLocal2.Visible := True;
       end;
    3: begin
         fLuxbg2.laLocal1.Caption := 'Gemeng';
       end;
    4: begin
         fLuxbg2.laLocal1.Caption := 'Kanton';
       end;
    5: begin
         fLuxbg2.laLocal1.Caption := 'Landesdeel';
         fLuxbg2.edLocal1.Visible := False;                                    // the edit field is replaced by a combobox in this case
         fLuxbg2.cobLocal1.Visible := True; fLuxbg2.cobLocal1.ItemIndex := -1;
       end;
  end;
  // Clear evaluation grid
  for I := 0 to 3 do
    fLuxbg2.sgEval.Cells[1, I] := '';
  // "Disable" letter buttons
  EdtField := '';
  // Re-enable question/answer button
  fLuxbg2.btQuestion.Enabled := True; fLuxbg2.btQuestion.Caption := 'Start';
end;

{ Insert letter in currently edited input field }

procedure InsertLetter(EdtField, Letter: string);

begin
  if EdtField = 'local1' then begin                                            // this argument tells, which input field to use
    fLuxbg2.edLocal1.SetFocus;
    if fLuxbg2.edLocal1.Text = '' then                                         // first letter has to be uppercase
      fLuxbg2.edLocal1.Text := UTF8UpperCase(Letter)
    else
      fLuxbg2.edLocal1.Text := fLuxbg2.edLocal1.Text + Letter;
  end
  else if EdtField = 'local2' then begin
    fLuxbg2.edLocal2.SetFocus;
    if fLuxbg2.edLocal2.Text = '' then
      fLuxbg2.edLocal2.Text := UTF8UpperCase(Letter)
    else
      fLuxbg2.edLocal2.Text := fLuxbg2.edLocal2.Text + Letter;
  end;
end;

{**********}
{ TfLuxbg2 }
{**********}

{ Application start: Initialisation }

procedure TfLuxbg2.FormCreate(Sender: TObject);

begin
  Randomize;
  ReadLocals(aLocals);
  SetLength(aLocalsDone, Length(aLocals));
  mQuizTownship.Click;                                                         // start with townships quiz
end;

{ Menu item "Quiz > Lëtzebuerger Numm": Start a new "Luxemburgish name" quiz }

procedure TfLuxbg2.mQuizLocalLuxClick(Sender: TObject);

begin
  iQuiz := 1;
  NewQuiz(iQuiz, sEditField);
end;

{ Menu item "Quiz > Däitsche/franséische Numm": Start a new "German/French names" quiz }

procedure TfLuxbg2.mQuizLocalGerFraClick(Sender: TObject);

begin
  iQuiz := 2;
  NewQuiz(iQuiz, sEditField);
end;

{ Menu item "Quiz > Numm vun der Gemeng": Start a new "townships" quiz }

procedure TfLuxbg2.mQuizTownshipClick(Sender: TObject);

begin
  iQuiz := 3;
  NewQuiz(iQuiz, sEditField);
end;

{ Menu item "Quiz > Numm vum Kanton": Start a new "cantons" quiz }

procedure TfLuxbg2.mQuizCantonClick(Sender: TObject);

begin
  iQuiz := 4;
  NewQuiz(iQuiz, sEditField);
end;

{ Menu item "Quiz > Landesdeel (Wahlbezierk)": Start a new "part of the country (ward)" quiz }

procedure TfLuxbg2.mQuizWardClick(Sender: TObject);

begin
  iQuiz := 5;
  NewQuiz(iQuiz, sEditField);
end;

{ Menu item "Quiz > Verloossen": Exit application }

procedure TfLuxbg2.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Hëllef > Iwwer": Display program about }

procedure TfLuxbg2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Lëtzebuerger Uertschafte Quiz.' + LineEnding;
  S += 'Errode vum lëtzebuerger oder däitschen a franséische Numm, vun der Gemeng, dem Kanton oder dem Landesdeel.' + LineEnding + LineEnding;
  S += 'Versioun 1.0, © allu, Januar 2020.';
  MessageDlg('Iwwer "Luxembourg2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Fro/Äntwert": Generate question resp. check user answer(s) }

procedure TfLuxbg2.btQuestionClick(Sender: TObject);

var
  I: Integer;
  Answer1, Answer2, UAnswer1, UAnswer2: string;

begin
  // Button "Start/Fro": Generate quiz question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Fro') then begin
    if btQuestion.Caption = 'Start' then begin
      // Re-init variables at the start of a new quiz
      iQuestion := 0; iCorrect := 0;
      for I := 0 to Length(aLocalsDone) - 1 do
        aLocalsDone[I] := False;
    end;
    // Proceed only if there are still questions left
    if iQuestion < Length(aLocals) then begin
      // Random question (from those not yet asked)
      repeat
        iLocal := Random(Length(aLocals));
      until not aLocalsDone[iLocal];
      Inc(iQuestion); fLuxbg2.laQuestion.Caption := 'Fro' + IntToStr(iQuestion);
      aLocalsDone[iLocal] := True;                                             // mark this local as "been done"
      // Question item(s) to display depends on quiz actually running
      if iQuiz = 1 then begin
        // "Lëtzebuerger Numm" quiz: Dispaly German and French local names
        edLocal0.Text := UTF8Trim(aLocals[iLocal].LocalDe);
        edLocal0b.Text := UTF8Trim(aLocals[iLocal].LocalFr);
      end
      else begin
        // All other cases: Display Luxembourgish local name
        edLocal0.Text := UTF8Trim(aLocals[iLocal].Local);
      end;
      edLocal1.Text := ''; edLocal2.Text := ''; edEval.Text := '';
      // Next button push will be to check user answer
      if iQuiz = 5 then
        cobLocal1.SetFocus
      else
        edLocal1.SetFocus;
      sEditField := 'local1';                                                  // user editing starts in first input field
      btQuestion.Caption := 'Äntwert';
    end
    // End the quiz if all locals have been done
    else begin
      MessageDlg('Uertschafte Quiz', 'All Uertschafte gefrot. Enn vum Quiz.', mtInformation, [mbOK], 0);
      btQuestion.Caption := 'Start';
      btQuestion.Enabled := False;                                             // disable button until new quiz is started
    end;
  end
  // Button "Äntwert": Check user answer(s) and update evaluation
  else begin
    // Answer(s) to be given depends of quiz actually running, of course
    case iQuiz of
      1: Answer1 := UTF8Trim(aLocals[iLocal].Local);
      2: Answer1 := UTF8Trim(aLocals[iLocal].LocalDe);
      3: Answer1 := UTF8Trim(aLocals[iLocal].Township);
      4: Answer1 := UTF8Trim(aLocals[iLocal].Canton);
      5: case aLocals[iLocal].Ward of
           'N': Answer1 := 'Norden';
           'O': Answer1 := 'Osten';
           'S': Answer1 := 'Süden';
           'Z': Answer1 := 'Zentrum';
         end;
    end;
    // Get user answer(s) from form
    if iQuiz = 5 then
      UAnswer1 := cobLocal1.Text                                               // answer to be taken from combobox
    else
      UAnswer1 := edLocal1.Text;                                               // answer to be taken from edit field
    if iQuiz = 2 then begin                                                    // 2 answers for "Däitsche/franséische Numm" quiz
      Answer2 := UTF8Trim(aLocals[iLocal].LocalFr);
      UAnswer2 := edLocal2.Text;
    end
    else begin                                                                 // for simpler code, just consider twice the same answer
      Answer2 := Answer1; UAnswer2 := UAnswer1;
    end;
    // Check user answer(s)
    if (UAnswer1 = Answer1) and (UAnswer2 = Answer2) then begin
      // Correct answer(s)
      edEval.Text := 'Dat ass richteg!';
      Inc(iCorrect);
    end
    else begin
      // False answer
      edEval.Text := 'Falsch! Richteg ass: ' + Answer1;
      if iQuiz = 2 then
        edEval.Text := edEval.Text + ', ' + Answer2;
    end;
    // Update evaluation counters
    sgEval.Cells[1, 0] := GridFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GridFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GridFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GridFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be to generate a new question
    sEditField := '';                                                          // this "disables" letter buttons action
    btQuestion.Caption := 'Fro';
  end;
end;

{ Letter buttons: Insert corresponding letter into actually edited input field }

procedure TfLuxbg2.btEAiguClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'é');
end;

procedure TfLuxbg2.btETremaClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ë');
end;

procedure TfLuxbg2.btAEClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ä');
end;

procedure TfLuxbg2.btOEClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ö');
end;

procedure TfLuxbg2.btUEClick(Sender: TObject);

begin
  InsertLetter(sEditField, 'ü');
end;

{ Mark input field as "currently edited" if user enters it }

procedure TfLuxbg2.edLocal1Enter(Sender: TObject);

begin
  if btQuestion.Caption = 'Äntwert' then                                       // do only if user is about to enter her answer
    sEditField := 'local1';
end;

procedure TfLuxbg2.edLocal2Enter(Sender: TObject);

begin
  if btQuestion.Caption = 'Äntwert' then
    sEditField := 'local2';
end;

end.

