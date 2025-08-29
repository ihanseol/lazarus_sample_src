{************************************}
{* Main unit for Hebrew application *}
{************************************}

unit arabic_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8;

type
  {**********}
  { TfArabic }
  {**********}
  TfArabic = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileLetters, mFileNames, mFileTransliteration, mFilePhonetics, mFileExit: TMenuItem;
    mOptions, mOptionsForm, mOptionsRepeat: TMenuItem;
    mOptionsFormIsolated, mOptionsFormInitial, mOptionsFormMiddle, mOptionsFormFinal: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laLetter, laName: TLabel;
    edLetter, edName, edEval: TEdit;
    cobName: TComboBox;
    btQuestion: TButton;
    btCancel: TButton;
    bt01, bt02, bt03, bt04, bt05, bt06, bt07, bt08, bt09, bt10, bt11, bt12, bt13, bt14: TButton;
    bt15, bt16, bt17, bt18, bt19, bt20, bt21, bt22, bt23, bt24, bt25, bt26, bt27, bt28: TButton;
    procedure bt28Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mFileLettersClick(Sender: TObject);
    procedure mFileNamesClick(Sender: TObject);
    procedure mFileTransliterationClick(Sender: TObject);
    procedure mFilePhoneticsClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsFormIsolatedClick(Sender: TObject);
    procedure mOptionsFormInitialClick(Sender: TObject);
    procedure mOptionsFormMiddleClick(Sender: TObject);
    procedure mOptionsFormFinalClick(Sender: TObject);
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
    iTest, iForm, iLetters, iCorrect, iLetter: Integer;
    bDone: array[1..28] of Boolean;
    btLetters: array[1..28] of TButton;
  end;

const
  LetterNames: array[1..28] of string = (
    'ʾalif', 'bāʾ', 'tāʾ', 'thāʾ', 'jīm', 'ḥāʾ', 'khāʾ', 'dāl', 'dhāl', 'rāʾ', 'zāy', 'sīn', 'shīn', 'ṣād',
    'ḍād', 'ṭāʾ', 'ẓāʾ', 'ʿayn', 'ghayn', 'fāʾ', 'qāf', 'kāf', 'lām', 'mīm', 'nūn', 'hāʾ', 'wāw', 'yāʾ'
  );
  Letters: array[1..4, 1..28] of string = (
    ( 'ا', 'ب', 'ت', 'ث', 'ج', 'ح', 'خ', 'د', 'ذ', 'ر', 'ز', 'س', 'ش', 'ص', 'ض', 'ط', 'ظ', 'ع', 'غ', 'ف', 'ق', 'ك', 'ل', 'م', 'ن', 'ه', 'و', 'ي'
    ),
    ( 'ا', 'بـ', 'تـ', 'ثـ', 'جـ', 'حـ', 'خـ', 'د', 'ذ', 'ر', 'ز', 'سـ', 'شـ', 'صـ', 'ضـ', 'طـ', 'ظـ', 'عـ', 'غـ', 'فـ', 'قـ', 'كـ', 'لـ', 'مـ', 'نـ', 'هـ', 'و', 'يـ'
    ),
    ( 'ـا', 'ـبـ', 'ـتـ', 'ـثـ', 'ـجـ', 'ـحـ', 'ـخـ', 'ـد', 'ـذ', 'ـر', 'ـز', 'ـسـ', 'ـشـ', 'ـصـ', 'ـضـ', 'ـطـ', 'ـظـ', 'ـعـ', 'ـغـ', 'ـفـ', 'ـقـ', 'ـكـ', 'ـلـ', 'ـمـ', 'ـنـ', 'ـهـ', 'ـو', 'ـيـ'
    ),
    ( 'ـا', 'ـب', 'ـت', 'ـث', 'ـج', 'ـح', 'ـخ', 'ـد', 'ـذ', 'ـر', 'ـز', 'ـس', 'ـش', 'ـص', 'ـض', 'ـط', 'ـظ', 'ـع', 'ـغ', 'ـف', 'ـق', 'ـك', 'ـل', 'ـم', 'ـن', 'ـه', 'ـو', 'ـي'
    )
  );
  Transliteration: array[1..28] of string = (
    'ā', 'b', 't', 'th', 'j', 'ḥ', 'kh', 'd', 'dh', 'r', 'z', 's', 'sh', 'ṣ',
    'ḍ', 'ṭ', 'ẓ', 'ʿ', 'gh', 'f', 'q', 'k', 'l', 'm', 'n', 'h', 'w', 'y'
  );
  Phonetics: array[1..28] of string = (
    '/aː/', '/b/', '/t/', '/θ/', '/dӡ/', '/ħ/', 'x', 'd', '/ð/', 'r', 'z', 's', '/ʃ/', '/sˤ/',
    '/dˤ/', '/tˤ/', '/ðˤ/', '/ʕ/', '/ɣ/', '/f/', '/q/', '/k/', '/l/', '/m/', '/n/', '/h/', '/w/', '/j/'
  );

var
  fArabic: TfArabic;

implementation

{$R *.lfm}

{ Insert letter (corr. to buttton pushed) into edit field }

procedure InsertLetter(Test: Integer; Action, Letter: string);

begin
  if Action = 'Answer' then begin                                              // do only if a user entry is waited for
    // Insert letter into correct field (depending on actual test)
    if (Test = 1) or (Test = 4) then
      fArabic.edLetter.Text := Letter
    else
      fArabic.edName.Text := Letter;
  end;
end;

{**********}
{ TfArabic }
{**********}

{ Application start: Initialisation }

procedure TfArabic.FormCreate(Sender: TObject);

begin
  // Create array with letter buttons
  btLetters[1] := bt01;  btLetters[2]  := bt02; btLetters[3]  := bt03; btLetters[4]  := bt04; btLetters[5]  := bt05;
  btLetters[6] := bt06;  btLetters[7]  := bt07; btLetters[8]  := bt08; btLetters[9]  := bt09; btLetters[10] := bt10;
  btLetters[11] := bt11; btLetters[12] := bt12; btLetters[13] := bt13; btLetters[14] := bt14; btLetters[15] := bt15;
  btLetters[16] := bt16; btLetters[17] := bt17; btLetters[18] := bt18; btLetters[19] := bt19; btLetters[20] := bt20;
  btLetters[21] := bt21; btLetters[22] := bt22; btLetters[23] := bt23; btLetters[24] := bt24; btLetters[25] := bt25;
  btLetters[26] := bt26; btLetters[27] := bt27; btLetters[28] := bt28;
  // Start random number generator
  Randomize;
  // Start with "Arabic letters" test
  iLetters := 28; iForm := 1;
  mFileLetters.Click;
end;

{ Menu item "File > Arabic letters": New test, where user has to enter the letter symbol for the letter name asked }

procedure TfArabic.mFileLettersClick(Sender: TObject);

var
  L1, L2, I: Integer;
  Temp: string;

begin
  iTest := 1;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Visible := True; edName.Visible := True;
  edName.ReadOnly := True; edName.TabStop := False;
  laName.Caption := 'Letter name';
  cobName.Visible := False;
  laLetter.Visible := True; edLetter.Visible := True;
  edLetter.ReadOnly := False; edLetter.TabStop := True;
  // Show letter buttons
  for I := 1 to iLetters do begin
    btLetters[I].Caption := Letters[iForm, I];
    btLetters[I].Visible := True;
  end;
  // Shuffle letter symbols (random sequence on buttons)
  for I := 1 to 100 do begin
    L1 := Random(iLetters) + 1; L2 := Random(iLetters) + 1;
    Temp := btLetters[L1].Caption; btLetters[L1].Caption := btLetters[L2].Caption; btLetters[L2].Caption := Temp;
  end;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Letter names": New test, where user has to enter the letter name for the letter symbol asked }

procedure TfArabic.mFileNamesClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 2;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Visible := True; edName.Visible := False;
  laName.Caption := 'Letter name';
  cobName.Visible := True; cobName.Left := 108;
  laLetter.Visible := True; edLetter.Visible := True;
  edLetter.ReadOnly := True; edLetter.TabStop := False;
  // Fill the combobox
  cobName.Items.Clear;
  for I := 1 to iLetters do
    cobName.Items.AddText(LetterNames[I]);
  cobName.ItemIndex := 0;
  // Hide the letter buttons
  for I := 1 to iLetters do
    btLetters[I].Visible := False;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Transcription": New test, where user has to enter the Latin transliteration for the letter symbol asked }

procedure TfArabic.mFileTransliterationClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 3;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Visible := True; edName.Visible := False;
  laName.Caption := 'Transliteration';
  cobName.Visible := True; cobName.Left := 124;
  laLetter.Visible := True; edLetter.Visible := True;
  edLetter.ReadOnly := True; edLetter.TabStop := False;
  // Fill the combobox
  cobName.Items.Clear;
  for I := 1 to iLetters do
    cobName.Items.AddText(Transliteration[I]);
  cobName.ItemIndex := 0;
  // Hide the letter buttons
  for I := 1 to iLetters do
    btLetters[I].Visible := False;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Phonetics": New test, where user has to enter the letter symbol for the phonetic symbol asked }

procedure TfArabic.mFilePhoneticsClick(Sender: TObject);

var
  L1, L2, I: Integer;
  Temp: string;

begin
  iTest := 4;
  edName.Text := ''; edLetter.Text := ''; edEval.Text := '';
  // Show controls as needed for this test
  laName.Visible := True; edName.Visible := True;
  edName.ReadOnly := True; edName.TabStop := False;
  laName.Caption := 'Phonetics';
  cobName.Visible := False;
  laLetter.Visible := True; edLetter.Visible := True;
  edLetter.ReadOnly := False; edLetter.TabStop := True;
  // Show letter buttons
  for I := 1 to iLetters do begin
    btLetters[I].Caption := Letters[iForm, I];
    btLetters[I].Visible := True;
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

procedure TfArabic.mFileExitClick(Sender: TObject);

begin
  Close;
end;

procedure TfArabic.mOptionsFormIsolatedClick(Sender: TObject);

begin
  mOptionsFormIsolated.Checked := True; mOptionsFormInitial.Checked := False;
  mOptionsFormMiddle.Checked := False;  mOptionsFormFinal.Checked := False;
  iForm := 1;
end;

procedure TfArabic.mOptionsFormInitialClick(Sender: TObject);

begin
  mOptionsFormIsolated.Checked := False; mOptionsFormInitial.Checked := True;
  mOptionsFormMiddle.Checked := False;   mOptionsFormFinal.Checked := False;
  iForm := 2;
end;

procedure TfArabic.mOptionsFormMiddleClick(Sender: TObject);

begin
  mOptionsFormIsolated.Checked := False; mOptionsFormInitial.Checked := False;
  mOptionsFormMiddle.Checked   := True;  mOptionsFormFinal.Checked := False;
  iForm := 3;
end;

procedure TfArabic.mOptionsFormFinalClick(Sender: TObject);

begin
  mOptionsFormIsolated.Checked := False; mOptionsFormInitial.Checked := False;
  mOptionsFormMiddle.Checked   := False; mOptionsFormFinal.Checked := True;
  iForm := 4;
end;

{ Menu item "Option > Repeat wrong answers": Toggle repetition or not of not known letters }

procedure TfArabic.mOptionsRepeatClick(Sender: TObject);

begin
  if mOptionsRepeat.Checked then
    mOptionsRepeat.Checked := False
  else
    mOptionsRepeat.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfArabic.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Learn the Arabic alphabet (Arabic letters).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2022.';
  MessageDlg('About "Arabic"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfArabic.btQuestionClick(Sender: TObject);

var
  I: Integer;
  Answer, UAnswer, S: string;
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
      edLetter.Text := Letters[iForm, iLetter];
      cobName.SetFocus;
    end
    else if iTest = 3 then begin
      // Transcription to find with letter given
      edLetter.Text := Letters[iForm, iLetter];
      cobName.SetFocus;
    end
    else begin
      // Letter to find with phonetics given
      edName.Text := Phonetics[iLetter];
      edLetter.SetFocus;
    end;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Answer';
  end
  else begin
    // Button "Answer": Check user answer
    if (iTest = 1) or (iTest = 4) then begin
      // Answer is letter symbol
      Answer  := Letters[iForm, iLetter];
      UAnswer := edLetter.Text;
    end
    else if iTest = 2 then begin
      // Answer is letter name
      Answer  := LetterNames[iLetter];
      UAnswer := cobName.Text;
    end
    else begin
      // Answer is transliteration
      Answer  := Transliteration[iLetter];
      UAnswer := cobName.Text;
    end;
    // Check the answer
    if UAnswer = Answer then begin
      // Correct answer
      edEval.Text := 'Correct!';
      Inc(iCorrect);
      bDone[iLetter] := True;                                                  // Mark letter as "done"
    end
    else begin
      // Wrong answer
      edEval.Text := 'False!';
      if not mOptionsRepeat.Checked then begin
        bDone[iLetter] := True;;                                               // Mark letter as "done", unless "Repeat wrong letter" is checked
        edEval.Text := edEval.Text + ' Correct answer is: ' + Answer;
      end;
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

procedure TfArabic.btCancelClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Start' then begin
    btQuestion.Caption := 'Start'; btQuestion.Enabled := False;                // disable button until new test started
    mOptions.Enabled := True;                                                  // re-enable options (so user can change them before starting new test)
  end;
end;

{ Letter buttons: Insert letter in (test type depending) edit field }

procedure TfArabic.bt01Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt01.Caption);
end;

procedure TfArabic.bt02Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt02.Caption);
end;

procedure TfArabic.bt03Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt03.Caption);
end;

procedure TfArabic.bt04Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt04.Caption);
end;

procedure TfArabic.bt05Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt05.Caption);
end;

procedure TfArabic.bt06Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt06.Caption);
end;

procedure TfArabic.bt07Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt07.Caption);
end;

procedure TfArabic.bt08Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt08.Caption);
end;

procedure TfArabic.bt09Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt09.Caption);
end;

procedure TfArabic.bt10Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt10.Caption);
end;

procedure TfArabic.bt11Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt11.Caption);
end;

procedure TfArabic.bt12Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt12.Caption);
end;

procedure TfArabic.bt13Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt13.Caption);
end;

procedure TfArabic.bt14Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt14.Caption);
end;

procedure TfArabic.bt15Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt15.Caption);
end;

procedure TfArabic.bt16Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt16.Caption);
end;

procedure TfArabic.bt17Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt17.Caption);
end;

procedure TfArabic.bt18Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt18.Caption);
end;

procedure TfArabic.bt19Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt19.Caption);
end;

procedure TfArabic.bt20Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt20.Caption);
end;

procedure TfArabic.bt21Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt21.Caption);
end;

procedure TfArabic.bt22Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt22.Caption);
end;

procedure TfArabic.bt23Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt23.Caption);
end;

procedure TfArabic.bt24Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt24.Caption);
end;

procedure TfArabic.bt25Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt25.Caption);
end;

procedure TfArabic.bt26Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt26.Caption);
end;

procedure TfArabic.bt27Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt27.Caption);
end;

procedure TfArabic.bt28Click(Sender: TObject);

begin
  InsertLetter(iTest, btQuestion.Caption, bt28.Caption);
end;

end.

