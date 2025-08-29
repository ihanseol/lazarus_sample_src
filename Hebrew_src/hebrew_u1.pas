{************************************}
{* Main unit for Hebrew application *}
{************************************}

unit hebrew_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8;

type
  {*********}
  { TfHebrew }
  {*********}
  TfHebrew = class(TForm)
    mOptionsFinals: TMenuItem;
    mMenu: TMainMenu;
    mFile, mFileLetters, mFileNames, mFileAlphabet, mFileExit: TMenuItem;
    mOptions, mOptionsRepeat: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laLetter, laName: TLabel;
    edLetter, edName, edEval: TEdit;
    btQuestion: TButton;
    btCancel: TButton;
    bt01, bt02, bt03, bt04, bt05, bt06, bt07, bt08, bt09, bt10, bt11, bt12, bt13: TButton;
    bt14, bt15, bt16, bt17, bt18, bt19, bt20, bt21, bt22, bt23, bt24, bt25, bt26, bt27: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileLettersClick(Sender: TObject);
    procedure mFileNamesClick(Sender: TObject);
    procedure mFileAlphabetClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsFinalsClick(Sender: TObject);
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
    procedure bt26Click(Sender: TObject);
    procedure bt27Click(Sender: TObject);
  private
    iTest, iLetters, iCorrect, iLetter: Integer;
    bDone: array[1..27] of Boolean;
    btLetters: array[1..27] of TButton;
  end;

const
  LetterNames: array[1..27] of string = (
    'Alef', 'Bet', 'Gimel', 'Dalet', 'He', 'Waw', 'Zayin', 'Chet', 'Tet', 'Yod', 'Kaf',
    'Lamed', 'Mem', 'Nun', 'Samech', 'Ayin', 'Pe', 'Tsade', 'Qof', 'Resh', 'Shin', 'Tav',
    'Kaf (final)', 'Mem (final)', 'Nun (final)', 'Pe (final)', 'Tsade (final)'
  );
  Letters = 'אבגדהוזחטיכלמנסעפצקרשתךםןףץ';

var
  fHebrew: TfHebrew;

implementation

{$R *.lfm}

{ Insert letter (corr. to buttton pushed) into edit field }

procedure InsertLetter(Test: Integer; Action, Letter: string);

begin
  if Action = 'Answer' then begin                                              // do only if a user entry is waited for
    // Insert letter into correct field (depending on actual test)
    if Test = 1 then
      fHebrew.edLetter.Text := Letter
    else
      fHebrew.edName.Text := Letter;
  end;
end;

{**********}
{ TfHebrew }
{**********}

{ Application start: Initialisation }

procedure TfHebrew.FormCreate(Sender: TObject);

begin
  // Create array with letter buttons
  btLetters[1] := bt01; btLetters[2] := bt02; btLetters[3] := bt03; btLetters[4] := bt04; btLetters[5] := bt05;
  btLetters[6] := bt06; btLetters[7] := bt07; btLetters[8] := bt08; btLetters[9] := bt09; btLetters[10] := bt10;
  btLetters[11] := bt11; btLetters[12] := bt12; btLetters[13] := bt13; btLetters[14] := bt14; btLetters[15] := bt15;
  btLetters[16] := bt16; btLetters[17] := bt17; btLetters[18] := bt18; btLetters[19] := bt19; btLetters[20] := bt20;
  btLetters[21] := bt21; btLetters[22] := bt22; btLetters[23] := bt23; btLetters[24] := bt24; btLetters[25] := bt25;
  btLetters[26] := bt26; btLetters[27] := bt27;
  // Start random number generator
  Randomize;
  // Start with "Hebrew letters" test
  mFileLetters.Click;
end;

{ Menu item "File > Hebrew letters": New test, where user has to enter the letter symbol }

procedure TfHebrew.mFileLettersClick(Sender: TObject);

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
  // Set total number of letters (depending on finals used or not)
  if mOptionsFinals.Checked then
    iLetters := 27
  else
    iLetters := 22;
  // Show letter buttons
  for I := 1 to 27 do begin
    if I <= iLetters then begin
      btLetters[I].Visible := True;
      btLetters[I].Caption := UTF8Copy(Letters, I, 1);
    end
    else
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

{ Menu item "File > Hebrew letter names": New test, where user has to enter the letter name }

procedure TfHebrew.mFileNamesClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 2;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Caption := 'Letter name';
  edName.ReadOnly := False; edName.TabStop := True; edName.Left := 112;
  laLetter.Visible := True; edLetter.Visible := True; edLetter.ReadOnly := True; edLetter.TabStop := False;
  // Set total number of letters (depending on finals used or not)
  if mOptionsFinals.Checked then
    iLetters := 27
  else
    iLetters := 22;
  // Hide the letter buttons (letter name has to be entered manually)
  for I := 1 to 27 do
    btLetters[I].Visible := False;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Hebrew alphabet": New test, where user has to enter the letter at a given position within the alphabet }

procedure TfHebrew.mFileAlphabetClick(Sender: TObject);

var
  L1, L2, I: Integer;
  Temp: string;

begin
  iTest := 3;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Caption := 'The nth letter of the Hebrew alphabet is';
  edName.ReadOnly := False; edName.TabStop := True; edName.Left := 330;
  laLetter.Visible := False; edLetter.Visible := False;
  // Set total number of letters (finals not used here)
  iLetters := 22;
  // Show letter buttons
  for I := 1 to 27 do begin
    if I <= iLetters then begin
      btLetters[I].Visible := True;
      btLetters[I].Caption := UTF8Copy(Letters, I, 1);
    end
    else
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

{ Menu item "File > Exit": Exit application }

procedure TfHebrew.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Option > Use final letters": Toggle usage or not of final letters }

procedure TfHebrew.mOptionsFinalsClick(Sender: TObject);

begin
  if mOptionsFinals.Checked then
    mOptionsFinals.Checked := False
  else
    mOptionsFinals.Checked := True;
end;

{ Menu item "Option > Repeat wrong answers": Toggle repetition or not of not known letters }

procedure TfHebrew.mOptionsRepeatClick(Sender: TObject);

begin
  if mOptionsRepeat.Checked then
    mOptionsRepeat.Checked := False
  else
    mOptionsRepeat.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfHebrew.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Learn the Hebrew alphabet (Hebrew letters).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2021.';
  MessageDlg('About "Hebrew"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfHebrew.btQuestionClick(Sender: TObject);

var
  I: Integer;
  Answer1, Answer2, UAnswer, S: string;
  Done: Boolean;

begin
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Button "Start/Question": Generate new question
    if btQuestion.Caption = 'Start' then begin
      // Some initialisations at start of test
      for I := 1 to iLetters do
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
      edLetter.Text := UTF8Copy(Letters, iLetter, 1);
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
      laName.Caption := laName.Caption + ' letter of the Hebrew alphabet is';
      edName.SetFocus;
    end;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Answer';
  end
  else begin
    // Button "Answer": Check user answer
    if iTest = 1 then begin
      // Answer is letter symbol
      Answer1  := UTF8Copy(Letters, iLetter, 1); Answer2 := Answer1;
      UAnswer := edLetter.Text;
    end
    else if iTest = 2 then begin
      // Answer is letter name
      Answer1  := LetterNames[iLetter]; Answer2 := Answer1;
      UAnswer := edName.Text;
    end
    else begin
      // Answer is letter symbol or name
      Answer1  := LetterNames[iLetter]; Answer2  := UTF8Copy(Letters, iLetter, 1);
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

procedure TfHebrew.btCancelClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Start' then begin
    btQuestion.Caption := 'Start'; btQuestion.Enabled := False;                // disable button until new test started
    mOptions.Enabled := True;                                                  // re-enable options (so user can change them before starting new test)
  end;
end;

{ Letter buttons: Insert letter in (test type depending) edit field }

procedure TfHebrew.bt01Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt01.Caption);
end;

procedure TfHebrew.bt02Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt02.Caption);
end;

procedure TfHebrew.bt03Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt03.Caption);
end;

procedure TfHebrew.bt04Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt04.Caption);
end;

procedure TfHebrew.bt05Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt05.Caption);
end;

procedure TfHebrew.bt06Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt06.Caption);
end;

procedure TfHebrew.bt07Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt07.Caption);
end;

procedure TfHebrew.bt08Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt08.Caption);
end;

procedure TfHebrew.bt09Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt09.Caption);
end;

procedure TfHebrew.bt10Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt10.Caption);
end;

procedure TfHebrew.bt11Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt11.Caption);
end;

procedure TfHebrew.bt12Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt12.Caption);
end;

procedure TfHebrew.bt13Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt13.Caption);
end;

procedure TfHebrew.bt14Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt14.Caption);
end;

procedure TfHebrew.bt15Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt15.Caption);
end;

procedure TfHebrew.bt16Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt16.Caption);
end;

procedure TfHebrew.bt17Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt17.Caption);
end;

procedure TfHebrew.bt18Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt18.Caption);
end;

procedure TfHebrew.bt19Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt19.Caption);
end;

procedure TfHebrew.bt20Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt20.Caption);
end;

procedure TfHebrew.bt21Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt21.Caption);
end;

procedure TfHebrew.bt22Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt22.Caption);
end;

procedure TfHebrew.bt23Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt23.Caption);
end;

procedure TfHebrew.bt24Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt24.Caption);
end;

procedure TfHebrew.bt25Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt25.Caption);
end;

procedure TfHebrew.bt26Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt26.Caption);
end;

procedure TfHebrew.bt27Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt27.Caption);
end;

end.

