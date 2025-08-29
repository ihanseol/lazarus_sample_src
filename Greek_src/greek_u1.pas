{***********************************}
{* Main unit for Greek application *}
{***********************************}

unit greek_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8;

type
  TLetters = array[1..25] of string;
  TLettersDone = array[1..25] of Boolean;
  {*********}
  { TfGreek }
  {*********}
  TfGreek = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileLetters, mFileNames, mFileAlphabet, mFileExit: TMenuItem;
    mOptions, mOptionsCase, mOptionsLowercase, mOptionsUppercase, mOptionsRepeat: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laLetter, laName: TLabel;
    edLetter, edName, edEval: TEdit;
    btQuestion: TButton;
    btCancel: TButton;
    bt01, bt02, bt03, bt04, bt05, bt06, bt07, bt08, bt09, bt10, bt11, bt12, bt13: TButton;
    bt14, bt15, bt16, bt17, bt18, bt19, bt20, bt21, bt22, bt23, bt24, bt25: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileLettersClick(Sender: TObject);
    procedure mFileNamesClick(Sender: TObject);
    procedure mFileAlphabetClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsLowercaseClick(Sender: TObject);
    procedure mOptionsUppercaseClick(Sender: TObject);
    procedure mOptionsRepeatClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure bt01Click(Sender: TObject);
    procedure bt02Click(Sender: TObject);
    procedure bt03Click(Sender: TObject);
    procedure bt04Click(Sender: TObject);
    procedure bt05Click(Sender: TObject);
    procedure bt06Click(Sender: TObject);
    procedure bt07Click(Sender: TObject);
    procedure bt08Click(Sender: TObject);
    procedure bt09Click(Sender: TObject);
    procedure bt10Click(Sender: TObject);
    procedure bt11Click(Sender: TObject);
    procedure bt12Click(Sender: TObject);
    procedure bt13Click(Sender: TObject);
    procedure bt14Click(Sender: TObject);
    procedure bt15Click(Sender: TObject);
    procedure bt16Click(Sender: TObject);
    procedure bt17Click(Sender: TObject);
    procedure bt18Click(Sender: TObject);
    procedure bt19Click(Sender: TObject);
    procedure bt20Click(Sender: TObject);
    procedure bt21Click(Sender: TObject);
    procedure bt22Click(Sender: TObject);
    procedure bt23Click(Sender: TObject);
    procedure bt24Click(Sender: TObject);
    procedure bt25Click(Sender: TObject);
  private
    iTest, iLetters, iCorrect, iLetter: Integer;
    sLetters: string;
    bDone: TLettersDone;
    btLetters: array[1..25] of TButton;
  end;

const
  LetterNames: TLetters = (
    'alpha', 'beta', 'gamma', 'delta', 'epsilon', 'zeta', 'eta', 'theta', 'iota', 'kappa', 'lamda', 'mu', 'nu',
    'xi', 'omicron', 'pi', 'rho', 'sigma', 'tau', 'upsilon', 'phi', 'chi', 'psi', 'omega', 'sigma (final)'
  );
  LettersUppercase = 'ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ';
  LettersLowercase = 'αβγδεζηθικλμνξοπρστυφχψως';

var
  fGreek: TfGreek;

implementation

{$R *.lfm}

{ Insert letter (corr. to buttton pushed) into edit field }

procedure InsertLetter(Test: Integer; Action, Letter: string);

begin
  if Action = 'Answer' then begin                                              // do only if a user entry is waited for
    // Insert letter into correct field (depending on actual test)
    if Test = 1 then
      fGreek.edLetter.Text := Letter
    else
      fGreek.edName.Text := Letter;
  end;
end;

{*********}
{ TfGreek }
{*********}

{ Application start: Initialisation }

procedure TfGreek.FormCreate(Sender: TObject);

begin
  // Create array with letter buttons
  btLetters[1] := bt01; btLetters[2] := bt02; btLetters[3] := bt03; btLetters[4] := bt04; btLetters[5] := bt05;
  btLetters[6] := bt06; btLetters[7] := bt07; btLetters[8] := bt08; btLetters[9] := bt09; btLetters[10] := bt10;
  btLetters[11] := bt11; btLetters[12] := bt12; btLetters[13] := bt13; btLetters[14] := bt14; btLetters[15] := bt15;
  btLetters[16] := bt16; btLetters[17] := bt17; btLetters[18] := bt18; btLetters[19] := bt19; btLetters[20] := bt20;
  btLetters[21] := bt21; btLetters[22] := bt22; btLetters[23] := bt23; btLetters[24] := bt24; btLetters[25] := bt25;
  // Start random number generator
  Randomize;
  // Start with "Greek letters" test
  mFileLetters.Click;
end;

{ Menu item "File > Greek letters": New test, where user has to enter the letter symbol }

procedure TfGreek.mFileLettersClick(Sender: TObject);

var
  L1, L2, I: Integer;
  Temp: string;

begin
  iTest := 1;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Caption := 'Letter name'; edName.Left := 112;
  edName.ReadOnly := True; edName.TabStop := False;
  laLetter.Visible := True; edLetter.Visible := True; edLetter.ReadOnly := False; edLetter.TabStop := True;
  // Use lower- or uppercase letters (as user has selected)
  if mOptionsUppercase.Checked then begin
    iLetters := 24; sLetters := LettersUppercase;
  end
  else begin
    iLetters := 25; sLetters := LettersLowercase;
  end;
  // Show letter buttons (with selected case symbols)
  for I := 1 to 25 do begin
    if I <= iLetters then begin
      // 24 or 25 letter symbols, depending on case
      btLetters[I].Visible := True;
      btLetters[I].Caption := UTF8Copy(sLetters, I, 1);
    end
    else
      // No special symbol for uppercase final sigma
      btLetters[I].Visible := False;
  end;
  // Shuffle letter symbols (random sequence on buttons)
  for I := 1 to 100 do begin
    L1 := Random(iLetters) + 1; L2 := Random(iLetters) + 1;
    Temp := btLetters[L1].Caption; btLetters[L1].Caption := btLetters[L2].Caption; btLetters[L2].Caption := Temp;
  end;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Greek letter names": New test, where user has to enter the letter name }

procedure TfGreek.mFileNamesClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 2;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Caption := 'Letter name';
  edName.ReadOnly := False; edName.TabStop := True; edName.Left := 112;
  laLetter.Visible := True; edLetter.Visible := True; edLetter.ReadOnly := True; edLetter.TabStop := False;
  // Use lower- or uppercase letters (as user has selected)
  if mOptionsUppercase.Checked then begin
    iLetters := 24; sLetters := LettersUppercase;
  end
  else begin
    iLetters := 25; sLetters := LettersLowercase;
  end;
  // Hide the letter buttons (letter name has to be entered manually)
  for I := 1 to 25 do
    btLetters[I].Visible := False;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Greek alphabet": New test, where user has to enter the letter at a given position within the alphabet }

procedure TfGreek.mFileAlphabetClick(Sender: TObject);

var
  L1, L2, I: Integer;
  Temp: string;

begin
  iTest := 3;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Caption := 'The nth letter of the Greek alphabet is';
  edName.ReadOnly := False; edName.TabStop := True; edName.Left := 317;
  laLetter.Visible := False; edLetter.Visible := False;
  iLetters := 24;                                                              // final sigma never used here
  // Use lower- or uppercase letters (as user has selected)
  if mOptionsUppercase.Checked then
    sLetters := LettersUppercase
  else
    sLetters := LettersLowercase;
  // Show letter buttons
  for I := 1 to 24 do begin
    btLetters[I].Visible := True;
    btLetters[I].Caption := UTF8Copy(sLetters, I, 1);
  end;
  btLetters[25].Visible := False;                                              // final sigma button not used
  // Shuffle letter symbols (random sequence on buttons)
  for I := 1 to 100 do begin
    L1 := Random(iLetters) + 1; L2 := Random(iLetters) + 1;
    Temp := btLetters[L1].Caption; btLetters[L1].Caption := btLetters[L2].Caption; btLetters[L2].Caption := Temp;
  end;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Exit": Exit application }

procedure TfGreek.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Option > Letter case > ...": Toggle lowercase/uppercase letters }

procedure TfGreek.mOptionsLowercaseClick(Sender: TObject);

begin
  mOptionsLowercase.Checked := True;
  mOptionsUppercase.Checked := False;
end;

procedure TfGreek.mOptionsUppercaseClick(Sender: TObject);

begin
  mOptionsLowercase.Checked := False;
  mOptionsUppercase.Checked := True;
end;

{ Menu item "Option > Repeat wrong answers": Toggle repetition or not of not known letters }

procedure TfGreek.mOptionsRepeatClick(Sender: TObject);

begin
  if mOptionsRepeat.Checked then
    mOptionsRepeat.Checked := False
  else
    mOptionsRepeat.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfGreek.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Learn the Greek alphabet (Greek letters).' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, January 2020 - January 2021.';
  MessageDlg('About "Greek', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfGreek.btQuestionClick(Sender: TObject);

var
  I: Integer;
  Answer1, Answer2, UAnswer, S: string;
  Done: Boolean;

begin
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Button "Start/Question": Generate new question
    if btQuestion.Caption = 'Start' then begin
      // Some initialisations at start of test
      for I := 1 to 25 do
        bDone[I] := False;                                                     // all questions "not yet asked"
      iCorrect := 0;
      mOptions.Enabled := False;                                               // disable options until test is over (or canceled by user)
    end;
    edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
    // Random letter (among those not yet asked resp. not correctly answered)
    repeat
      iLetter := Random(iLetters) + 1;
    until not bDone[iLetter];
    // Fill form fields, depending on actual test type
    if iTest = 1 then begin
      // Letter to find with name given
      edName.Text := LetterNames[iLetter];
      edLetter.SetFocus;
    end
    else if iTest = 2 then begin
      // Name to find with letter given
      edLetter.Text := UTF8Copy(sLetters, iLetter, 1);
      edName.SetFocus;
    end
    else begin
      // Letter at given position within alphabet to find
      laName.Caption := 'The ' + IntToStr(iLetter);
      if iLetter = 1 then
        laName.Caption := laName.Caption + 'st'
      else if iLetter = 2 then
        laName.Caption := laName.Caption + 'nd'
      else if iLetter = 3 then
        laName.Caption := laName.Caption + 'rd'
      else
        laName.Caption := laName.Caption + 'th';
      laName.Caption := laName.Caption + ' letter of the Greek alphabet is';
      edName.SetFocus;
    end;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Answer';
  end
  else begin
    // Button "Answer": Check user answer
    if iTest = 1 then begin
      // Answer is letter symbol
      Answer1  := UTF8Copy(sLetters, iLetter, 1); Answer2 := Answer1;
      UAnswer := edLetter.Text;
    end
    else if iTest = 2 then begin
      // Answer is letter name
      Answer1  := LetterNames[iLetter]; Answer2 := Answer1;
      UAnswer := edName.Text;
    end
    else begin
      // Answer is letter symbol or name
      Answer1  := LetterNames[iLetter]; Answer2  := UTF8Copy(sLetters, iLetter, 1);
      UAnswer := edName.Text;
    end;
    // Check the answer
    if (UAnswer = Answer1) or (UAnswer = Answer2) then begin
      // Correct answer
      edEval.Text := 'Correct!';
      Inc(iCorrect);
      bDone[iLetter] := True;                                                  // Mark letter as "done"
    end
    else begin
      // Wrong answer
      edEval.Text := 'False!';
      if not mOptionsRepeat.Checked then
        bDone[iLetter] := True;;                                               // Mark letter as "done", unless "Repeat wrong letter" is checked
    end;
    // Check if there are "undone" letters left
    Done := True;
    for I := 1 to iLetters do begin
      if not bDone[I] then
        Done := False;
    end;
    if Done then begin
      // All letters "done": end of test
      S := 'All letters have been done.';
      if not mOptionsRepeat.Checked then
        S += ' ' + IntToStr(iCorrect) + ' of ' + IntToStr(iLetters) + ' correct answers.';
      MessageDlg('End of test', S, mtInformation, [mbOK], 0);
      btQuestion.Caption := 'Start'; btQuestion.Enabled := False;              // disable button until new test started
      mOptions.Enabled := True;                                                // re-enable options (so user can change them before starting new test)
    end
    else begin
      // Still "undone" letters there: set button for next question
      btQuestion.Caption := 'Question';
      btQuestion.SetFocus;
    end;
  end;
end;

{ Button "Cancel": Cancel the actual test }

procedure TfGreek.btCancelClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Start' then begin
    btQuestion.Caption := 'Start'; btQuestion.Enabled := False;                // disable button until new test started
    mOptions.Enabled := True;                                                  // re-enable options (so user can change them before starting new test)
  end;
end;

{ Letter buttons: Insert letter in (test type depending) edit field }

procedure TfGreek.bt01Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt01.Caption);
end;

procedure TfGreek.bt02Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt02.Caption);
end;

procedure TfGreek.bt03Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt03.Caption);
end;

procedure TfGreek.bt04Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt04.Caption);
end;

procedure TfGreek.bt05Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt05.Caption);
end;

procedure TfGreek.bt06Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt06.Caption);
end;

procedure TfGreek.bt07Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt07.Caption);
end;

procedure TfGreek.bt08Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt08.Caption);
end;

procedure TfGreek.bt09Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt09.Caption);
end;

procedure TfGreek.bt10Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt10.Caption);
end;

procedure TfGreek.bt11Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt11.Caption);
end;

procedure TfGreek.bt12Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt12.Caption);
end;

procedure TfGreek.bt13Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt13.Caption);
end;

procedure TfGreek.bt14Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt14.Caption);
end;

procedure TfGreek.bt15Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt15.Caption);
end;

procedure TfGreek.bt16Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt16.Caption);
end;

procedure TfGreek.bt17Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt17.Caption);
end;

procedure TfGreek.bt18Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt18.Caption);
end;

procedure TfGreek.bt19Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt19.Caption);
end;

procedure TfGreek.bt20Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt20.Caption);
end;

procedure TfGreek.bt21Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt21.Caption);
end;

procedure TfGreek.bt22Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt22.Caption);
end;

procedure TfGreek.bt23Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt23.Caption);
end;

procedure TfGreek.bt24Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt24.Caption);
end;

procedure TfGreek.bt25Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt25.Caption);
end;

end.

