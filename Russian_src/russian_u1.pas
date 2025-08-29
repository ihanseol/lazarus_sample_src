{*************************************}
{* Main unit for Russian application *}
{*************************************}

unit russian_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8;

type
  TAlphabet = array[1..33] of record
    RussianUpper, RussianLower: string;
    Transcription, TranscriptionANSI, Transcription2, Transcription3: string;
    Phonetics, PhoneticsANSI, Phonetics2: string;
  end;
  TLettersDone = array[1..33] of Boolean;
  {***********}
  { TfRussian }
  {***********}
  TfRussian = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileRussian, mFileLatin, mFileExit: TMenuItem;
    mOptions, mOptionsCase, mOptionsLowercase, mOptionsUppercase, mOptionsRepeat: TMenuItem;
    mOptionsPhonetics, mOptionsTranscriptionANSI, mOptionsPhoneticsANSI, MenuItem1: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laLetter, laLatin, laTranscription, laPhonetics: TLabel;
    edRussian, edLatin, edEval: TEdit;
    btQuestion: TButton;
    btCancel: TButton;
    bt01, bt02, bt03, bt04, bt05, bt06, bt07, bt08, bt09, bt10, bt11, bt12: TButton;
    bt13, bt14, bt15, bt16, bt17, bt18, bt19, bt20, bt21, bt22, bt23, bt24: TButton;
    bt25, bt26, bt27, bt28, bt29, bt30, bt31, bt32, bt33: TButton;
    btTranscription1, btTranscription2, btTranscription3: TButton;
    btTranscription4, btTranscription5, btTranscription6: TButton;
    btPhonetics1, btPhonetics2, btPhonetics3, btPhonetics4: TButton;
    btPhonetics5, btPhonetics6, btPhonetics7, btPhonetics8: TButton;
    btPhonetics9, btPhonetics10, btPhonetics11, btPhonetics12: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileRussianClick(Sender: TObject);
    procedure mFileLatinClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsLowercaseClick(Sender: TObject);
    procedure mOptionsUppercaseClick(Sender: TObject);
    procedure mOptionsPhoneticsClick(Sender: TObject);
    procedure mOptionsTranscriptionANSIClick(Sender: TObject);
    procedure mOptionsPhoneticsANSIClick(Sender: TObject);
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
    procedure bt28Click(Sender: TObject);
    procedure bt29Click(Sender: TObject);
    procedure bt30Click(Sender: TObject);
    procedure bt31Click(Sender: TObject);
    procedure bt32Click(Sender: TObject);
    procedure bt33Click(Sender: TObject);
    procedure btTranscription1Click(Sender: TObject);
    procedure btTranscription2Click(Sender: TObject);
    procedure btTranscription3Click(Sender: TObject);
    procedure btTranscription4Click(Sender: TObject);
    procedure btTranscription5Click(Sender: TObject);
    procedure btTranscription6Click(Sender: TObject);
    procedure btPhonetics1Click(Sender: TObject);
    procedure btPhonetics2Click(Sender: TObject);
    procedure btPhonetics3Click(Sender: TObject);
    procedure btPhonetics4Click(Sender: TObject);
    procedure btPhonetics5Click(Sender: TObject);
    procedure btPhonetics6Click(Sender: TObject);
    procedure btPhonetics7Click(Sender: TObject);
    procedure btPhonetics8Click(Sender: TObject);
    procedure btPhonetics9Click(Sender: TObject);
    procedure btPhonetics10Click(Sender: TObject);
    procedure btPhonetics11Click(Sender: TObject);
    procedure btPhonetics12Click(Sender: TObject);
  private
    iTest, iLetter, iLetters, iCorrect: Integer;
    aAlphabet: TAlphabet;
    aDone: TLettersDone;
    btLetters: array[1..33] of TButton;
    btTranscription: array[1..6] of TButton;
    btPhonetics: array[1..12] of TButton;
  end;

var
  fRussian: TfRussian;

implementation

{$R *.lfm}

{ Read alphabet from text file }

procedure ReadAlphabet(out Alphabet: TAlphabet);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'alphabet.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      Alphabet[N].RussianUpper := UTF8Copy(Line, 1, 1);
      Alphabet[N].RussianLower := UTF8Copy(Line, 6, 1);
      Alphabet[N].Transcription := UTF8Trim(UTF8Copy(Line, 11, 5));
      if UTF8Copy(Line, 16, 5) = '     ' then
        Alphabet[N].TranscriptionANSI := Alphabet[N].Transcription
      else
        Alphabet[N].TranscriptionANSI := UTF8Trim(UTF8Copy(Line, 16, 5));
      if UTF8Copy(Line, 21, 5) = '-----' then
        Alphabet[N].Phonetics :=  ''
      else
        Alphabet[N].Phonetics := UTF8Trim(UTF8Copy(Line, 21, 5));
      if (UTF8Length(Line) > 28) and (UTF8Copy(Line, 28, 5) <> '     ') then
        Alphabet[N].PhoneticsANSI := UTF8Trim(UTF8Copy(Line, 28, 5))
      else
        Alphabet[N].PhoneticsANSI := Alphabet[N].Phonetics;
      Alphabet[N].Transcription2 := ''; Alphabet[N].Transcription3 := ''; Alphabet[N].Phonetics2 := '';
      if UTF8Length(Line) > 34 then
        Alphabet[N].Transcription2 := UTF8Trim(UTF8Copy(Line, 34, 5));
      if UTF8Length(Line) > 39 then
        Alphabet[N].Transcription3 := UTF8Trim(UTF8Copy(Line, 39, 5));
      if UTF8Length(Line) > 44 then
        Alphabet[N].Phonetics2 := UTF8Trim(UTF8Copy(Line, 44, 5));
    end;
  end;
  Close(InFile);
end;

{ First character uppercase }

function UpperFirst(S: string): string;

begin
  Result := StringReplace(S, UTF8Copy(S, 1, 1), UTF8UpperCase(UTF8Copy(S, 1, 1)), []);
end;

{ Insert Russian letter (corr. to buttton pushed) into Russian letter edit field }

procedure InsertLetter(Action, Letter: string);

begin
  if Action = 'Answer' then                                                    // do only if a user entry is waited for
    fRussian.edRussian.Text := Letter;
end;

{ Insert transcription character (corr. to buttton pushed) into Latin letter edit field }

procedure InsertChar(Action, Char: string);

begin
  if Action = 'Answer' then begin
    if fRussian.mOptionsUppercase.Checked then
      Char := UpperFirst(Char)
    else
      Char := UTF8LowerCase(Char);
    fRussian.edLatin.Text := Char;
  end;
end;

{ Insert phonetics symbol (corr. to buttton pushed) into Latin letter edit field }

procedure InsertSymbol(Action, Symbol: string);

begin
  if Action = 'Answer' then
    fRussian.edLatin.Text := Symbol;
end;

{***********}
{ TfRussian }
{***********}

{ Application start: Initialisation }

procedure TfRussian.FormCreate(Sender: TObject);

begin
  // Create array with Russian letter buttons
  btLetters[1]  := bt01; btLetters[2]  := bt02; btLetters[3]  := bt03; btLetters[4]  := bt04; btLetters[5]  := bt05;
  btLetters[6]  := bt06; btLetters[7]  := bt07; btLetters[8]  := bt08; btLetters[9]  := bt09; btLetters[10] := bt10;
  btLetters[11] := bt11; btLetters[12] := bt12; btLetters[13] := bt13; btLetters[14] := bt14; btLetters[15] := bt15;
  btLetters[16] := bt16; btLetters[17] := bt17; btLetters[18] := bt18; btLetters[19] := bt19; btLetters[20] := bt20;
  btLetters[21] := bt21; btLetters[22] := bt22; btLetters[23] := bt23; btLetters[24] := bt24; btLetters[25] := bt25;
  btLetters[26] := bt26; btLetters[27] := bt27; btLetters[28] := bt28; btLetters[29] := bt29; btLetters[30] := bt30;
  btLetters[31] := bt31; btLetters[32] := bt32; btLetters[33] := bt33;
  // Create arrays with transcription characters buttons and phonetics symbols buttons
  btTranscription[1] := btTranscription1; btTranscription[2] := btTranscription2; btTranscription[3] := btTranscription3;
  btTranscription[4] := btTranscription4; btTranscription[5] := btTranscription5; btTranscription[6] := btTranscription6;
  btPhonetics[1]  := btPhonetics1;  btPhonetics[2]  := btPhonetics2;  btPhonetics[3]  := btPhonetics3;
  btPhonetics[4]  := btPhonetics4;  btPhonetics[5]  := btPhonetics11; btPhonetics[6]  := btPhonetics5;
  btPhonetics[7]  := btPhonetics12; btPhonetics[8]  := btPhonetics6;  btPhonetics[9]  := btPhonetics7;
  btPhonetics[10] := btPhonetics8;  btPhonetics[11] := btPhonetics9;  btPhonetics[12] := btPhonetics10;
  // Read alphabet data
  ReadAlphabet(aAlphabet);
  // Start random number generator
  Randomize;
  // Start with "Russian letters" test
  mFileRussian.Click;
end;

{ Menu item "File > Russian letters": New test, where user has to enter the Russian letter }

procedure TfRussian.mFileRussianClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 1;
  edLatin.Text := ''; edRussian.Text := ''; edEval.Text := '';
  edLatin.ReadOnly := True; edLatin.TabStop := False;
  edRussian.ReadOnly := False; edRussian.TabStop := True;
  // Show Russian letters on buttons (with selected case)
  for I := 1 to 33 do begin
    btLetters[I].Visible := True;
    if mOptionsUppercase.Checked then
      btLetters[I].Caption := aAlphabet[I].RussianUpper
    else
      btLetters[I].Caption := aAlphabet[I].RussianLower;
  end;
  // Hide transcription and phonetics buttons
  laTranscription.Visible := False; laPhonetics.Visible := False;
  for I := 1 to 6 do
    btTranscription[I].Visible := False;
  for I := 1 to 12 do
    btPhonetics[I].Visible := False;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Latin letters": New test, where user has to enter the letter transcription/phonetics }

procedure TfRussian.mFileLatinClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 2;
  edLatin.Text := ''; edRussian.Text := ''; edEval.Text := '';
  edLatin.ReadOnly := False; edLatin.TabStop := True;
  edRussian.ReadOnly := True; edRussian.TabStop := False;
  // Hide the Russion letter buttons (answer has to be entered manually)
  for I := 1 to 33 do
    btLetters[I].Visible := False;
  // Show transcription or phonetics buttons
  if mOptionsPhonetics.Checked then begin
    laTranscription.Visible := False; laPhonetics.Visible := True;
    for I := 1 to 6 do
      btTranscription[I].Visible := False;
    for I := 1 to 12 do
      btPhonetics[I].Visible := True;
  end
  else begin
    laTranscription.Visible := True; laPhonetics.Visible := False;
    for I := 1 to 6 do
      btTranscription[I].Visible := True;
    for I := 1 to 12 do
      btPhonetics[I].Visible := False;
  end;
  // Set button for start of new test
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
end;

{ Menu item "File > Exit": Exit application }

procedure TfRussian.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Option > Letter case > ...": Toggle lowercase/uppercase letters }

procedure TfRussian.mOptionsUppercaseClick(Sender: TObject);

var
  I: Integer;

begin
  mOptionsUppercase.Checked := True; mOptionsLowercase.Checked := False;
  for I := 1 to 33 do
    btLetters[I].Caption := aAlphabet[I].RussianUpper;
end;

procedure TfRussian.mOptionsLowercaseClick(Sender: TObject);

var
  I: Integer;

begin
  mOptionsUppercase.Checked := False; mOptionsLowercase.Checked := True;
  for I := 1 to 33 do
    btLetters[I].Caption := aAlphabet[I].RussianLower;
end;

{ Menu item "Options > Use IPA instead of transcription": Toggle "latin letters" as transcription or phonetics }

procedure TfRussian.mOptionsPhoneticsClick(Sender: TObject);

var
  I: Integer;

begin
  if mOptionsPhonetics.Checked then begin
    // Use transcription character(s)
    mOptionsPhonetics.Checked := False;
    laLatin.Caption := 'Transcription';
    if iTest = 2 then begin
      // Show transcription characters buttons
      laTranscription.Visible := True; laPhonetics.Visible := False;
      for I := 1 to 6 do begin
        btTranscription[I].Visible := True;
      end;
      for I := 1 to 12 do
        btPhonetics[I].Visible := False;
    end;
  end
  else begin
    // Use phonetics symbol
    mOptionsPhonetics.Checked := True;
    laLatin.Caption := 'Phonetics';
    if iTest = 2 then begin
      // Show phonetics symbols buttons
      laTranscription.Visible := False; laPhonetics.Visible := True;
      for I := 1 to 6 do begin
        btTranscription[I].Visible := False;
      end;
      for I := 1 to 12 do
        btPhonetics[I].Visible := True;
    end;
  end;
end;

{ Menu item "Options > Use all ANSI character transcription": Toggle transcription chars as all ANSI or not }

procedure TfRussian.mOptionsTranscriptionANSIClick(Sender: TObject);

var
  I: Integer;

begin
  if mOptionsTranscriptionANSI.Checked then begin
    // Use non-ANSI characters
    mOptionsTranscriptionANSI.Checked := False;
    for I := 3 to 6 do
      btTranscription[I].Enabled := True;                                      // show non-ANSI buttons
  end
  else begin
    // Use ANSI-characters only
    mOptionsTranscriptionANSI.Checked := True;
    for I := 3 to 6 do
      btTranscription[I].Enabled := False;                                     // hide non-ANSI buttons
  end;
end;

{ Menu item "Options > Use ANSI characters vowel phonetics": Toggle ANSI characters vowel phonetics or not }

procedure TfRussian.mOptionsPhoneticsANSIClick(Sender: TObject);

var
  I: Integer;

begin
  if mOptionsPhoneticsANSI.Checked then begin
    // Use non-ANSI characters vowel phonetics
    mOptionsPhoneticsANSI.Checked := False;
    for I := 8 to 12 do
      btPhonetics[I].Enabled := True;                                          // show non-ANSI buttons
  end
  else begin
    // Use ANSI characters vowel phonetics
    mOptionsPhoneticsANSI.Checked := True;
    for I := 8 to 12 do
      btPhonetics[I].Enabled := False;                                         // hide non-ANSI buttons
  end;
end;

{ Menu item "Option > Repeat wrong answers": Toggle repetition of not known letters or not }

procedure TfRussian.mOptionsRepeatClick(Sender: TObject);

begin
  if mOptionsRepeat.Checked then
    mOptionsRepeat.Checked := False
  else
    mOptionsRepeat.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfRussian.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Learn the Russian alphabet (Cyrillic script letters).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, November 2020.';
  MessageDlg('About "Russian"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfRussian.btQuestionClick(Sender: TObject);

var
  I: Integer;
  Answer, Answer2, Answer3, UAnswer, S: string;
  Correct, Done: Boolean;

begin
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Button "Start/Question": Generate new question (randomly choose new letter)
    if btQuestion.Caption = 'Start' then begin
      // Some initialisations at start of test
      iLetters := 33; iCorrect := 0;
      if mOptionsPhonetics.Checked then
        iLetters := 32;                                                        // there is no phonetic symbol for "ъ"
      for I := 1 to 33 do begin
        aDone[I] := False;                                                     // all questions "not yet asked"
        if mOptionsPhonetics.Checked and (aAlphabet[I].Phonetics = '') then
          aDone[I] := True;                                                    // can't ask "ъ" (no phonetics symbol)
      end;
      mOptions.Enabled := False;                                               // disable options until test is over (or canceled by user)
    end;
    edLatin.Text := ''; edRussian.Text := ''; edEval.Text := '';
    // Random letter (among those not yet asked resp. not correctly answered)
    repeat
      iLetter := Random(33) + 1;
    until not aDone[iLetter];
    // Fill form fields, depending on actual test type
    if iTest = 1 then begin
      // Russian letter to find with transcription/phonetics given
      if mOptionsPhonetics.Checked then begin
        // Phonetics symbol given
        if mOptionsPhoneticsANSI.Checked then
          edLatin.Text := aAlphabet[iLetter].PhoneticsANSI
        else
          edLatin.Text := aAlphabet[iLetter].Phonetics;
      end
      else begin
        // Transcription character(s) given
        if mOptionsTranscriptionANSI.Checked then
          edLatin.Text := aAlphabet[iLetter].TranscriptionANSI
        else
          edLatin.Text := aAlphabet[iLetter].Transcription;
        if mOptionsUpperCase.Checked then
          edLatin.Text := UpperFirst(edLatin.Text);                            // consider case selected
      end;
      edRussian.SetFocus;
    end
    else begin
      // Transcription/phonetics to find with Russian letter given
      if mOptionsUppercase.Checked then
        edRussian.Text := aAlphabet[iLetter].RussianUpper
      else
        edRussian.Text := aAlphabet[iLetter].RussianLower;
      edLatin.SetFocus;
    end;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Answer';
  end
  else begin
    // Button "Answer": Check user answer
    Correct := False;
    if iTest = 1 then begin
      // "Find Russian letter" test
      UAnswer := edRussian.Text;
      if mOptionsUpperCase.Checked then
        Answer := aAlphabet[iLetter].RussianUpper
      else
        Answer := aAlphabet[iLetter].RussianLower;
      if UAnswer = Answer then
        // Correct answer = Russian letter (with correct case)
        Correct := True;
    end
    else begin
      // "Find Russian letter transcription/phonetics" test
      UAnswer := edLatin.Text;
      if UAnswer <> '' then begin
        if mOptionsPhonetics.Checked then begin
          // "Find Russian letter phonetics" test
          if mOptionsPhoneticsANSI.Checked then
            Answer := aAlphabet[iLetter].PhoneticsANSI
          else
            Answer := aAlphabet[iLetter].Phonetics;
          if (UAnswer = Answer) or (UAnswer = aAlphabet[iLetter].Phonetics2) then
            // Correct answer = ANSI/non-ANSI symbol or alternate phonetics
            Correct := True;
        end
        else begin
          // "Find Russian letter transcription" test
          if mOptionsTranscriptionANSI.Checked then
            Answer := aAlphabet[iLetter].TranscriptionANSI
          else
            Answer := aAlphabet[iLetter].Transcription;
          Answer2 := aAlphabet[iLetter].Transcription2; Answer3 := aAlphabet[iLetter].Transcription3;
          if mOptionsUpperCase.Checked then begin
            // Consider case selected
            Answer := UpperFirst(Answer); Answer2 := UpperFirst(Answer2); Answer3 := UpperFirst(Answer3);
          end;
          if (UAnswer = Answer) or (UAnswer = Answer2) or (UAnswer = Answer3) then
            // Correct answer = ANSI/non-ANSI transcription or one of the alternate transcriptions
            Correct := True;
        end;
      end;
    end;
    // Evaluation
    if Correct then begin
      // Correct answer
      edEval.Text := 'Correct!';
      Inc(iCorrect);
      aDone[iLetter] := True;                                                  // mark letter as "done"
    end
    else begin
      // False answer
      edEval.Text := 'False!';
      if not mOptionsRepeat.Checked then
        aDone[iLetter] := True;;                                               // mark letter as "done", unless "Repeat wrong letter" is checked
    end;
    // Check if there are "undone" letters left
    Done := True;
    for I := 1 to 33 do begin
      if not aDone[I] then
        Done := False;
    end;
    if Done then begin
      // All letters "done": End of test
      S := 'All letters have been done...';
      if not mOptionsRepeat.Checked then begin
        S +=  LineEnding;
        S += IntToStr(iCorrect) + ' letters of ' + IntToStr(iLetters) +  ' correct = ';
        S += IntToStr(Round(100 * iCorrect / iLetters)) + '%';
      end;
      MessageDlg('End of test', S, mtInformation, [mbOK], 0);
      btQuestion.Caption := 'Start'; btQuestion.Enabled := False;              // disable button until new test started
      mOptions.Enabled := True;                                                // re-enable options (so user can change them before starting new test)
    end
    else begin
      // Still "undone" letters there: Set button for next question
      btQuestion.Caption := 'Question';
      btQuestion.SetFocus;
    end;
  end;
end;

{ Button "Cancel": Cancel the actual test }

procedure TfRussian.btCancelClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Start' then begin
    btQuestion.Caption := 'Start'; btQuestion.Enabled := False;                // disable button until new test started
    mOptions.Enabled := True;                                                  // re-enable options (so user can change them before starting new test)
  end;
end;

{ Russian letter buttons: Insert letter into Russian letter edit field }

procedure TfRussian.bt01Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt01.Caption);
end;

procedure TfRussian.bt02Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt02.Caption);
end;

procedure TfRussian.bt03Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt03.Caption);
end;

procedure TfRussian.bt04Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt04.Caption);
end;

procedure TfRussian.bt05Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt05.Caption);
end;

procedure TfRussian.bt06Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt06.Caption);
end;

procedure TfRussian.bt07Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt07.Caption);
end;

procedure TfRussian.bt08Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt08.Caption);
end;

procedure TfRussian.bt09Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt09.Caption);
end;

procedure TfRussian.bt10Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt10.Caption);
end;

procedure TfRussian.bt11Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt11.Caption);
end;

procedure TfRussian.bt12Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt12.Caption);
end;

procedure TfRussian.bt13Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt13.Caption);
end;

procedure TfRussian.bt14Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt14.Caption);
end;

procedure TfRussian.bt15Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt15.Caption);
end;

procedure TfRussian.bt16Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt16.Caption);
end;

procedure TfRussian.bt17Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt17.Caption);
end;

procedure TfRussian.bt18Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt18.Caption);
end;

procedure TfRussian.bt19Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt19.Caption);
end;

procedure TfRussian.bt20Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt20.Caption);
end;

procedure TfRussian.bt21Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt21.Caption);
end;

procedure TfRussian.bt22Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt22.Caption);
end;

procedure TfRussian.bt23Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt23.Caption);
end;

procedure TfRussian.bt24Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt24.Caption);
end;

procedure TfRussian.bt25Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt25.Caption);
end;

procedure TfRussian.bt26Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt26.Caption);
end;

procedure TfRussian.bt27Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt27.Caption);
end;

procedure TfRussian.bt28Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt28.Caption);
end;

procedure TfRussian.bt29Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt29.Caption);
end;

procedure TfRussian.bt30Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt30.Caption);
end;

procedure TfRussian.bt31Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt31.Caption);
end;

procedure TfRussian.bt32Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt32.Caption);
end;

procedure TfRussian.bt33Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt33.Caption);
end;

{ Transcription characters buttons: Insert transcription character(s) into Latin letter edit field }

procedure TfRussian.btTranscription1Click(Sender: TObject);

begin
  InsertChar(btQuestion.Caption, btTranscription1.Caption);
end;

procedure TfRussian.btTranscription2Click(Sender: TObject);

begin
  InsertChar(btQuestion.Caption, btTranscription2.Caption);
end;

procedure TfRussian.btTranscription3Click(Sender: TObject);

begin
  InsertChar(btQuestion.Caption, btTranscription3.Caption);
end;

procedure TfRussian.btTranscription4Click(Sender: TObject);

begin
  InsertChar(btQuestion.Caption, btTranscription4.Caption);
end;

procedure TfRussian.btTranscription5Click(Sender: TObject);

begin
  InsertChar(btQuestion.Caption, btTranscription5.Caption);
end;

procedure TfRussian.btTranscription6Click(Sender: TObject);

begin
  InsertChar(btQuestion.Caption, btTranscription6.Caption);
end;

{ Phonetics symbols buttons: Insert phonetic symbol into Latin letter edit field }

procedure TfRussian.btPhonetics1Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics1.Caption);
end;

procedure TfRussian.btPhonetics2Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics2.Caption);
end;

procedure TfRussian.btPhonetics3Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics3.Caption);
end;

procedure TfRussian.btPhonetics4Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics4.Caption);
end;

procedure TfRussian.btPhonetics5Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics5.Caption);
end;

procedure TfRussian.btPhonetics6Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics6.Caption);
end;

procedure TfRussian.btPhonetics7Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics7.Caption);
end;

procedure TfRussian.btPhonetics8Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics8.Caption);
end;

procedure TfRussian.btPhonetics9Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics9.Caption);
end;

procedure TfRussian.btPhonetics10Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics10.Caption);
end;

procedure TfRussian.btPhonetics11Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics11.Caption);
end;

procedure TfRussian.btPhonetics12Click(Sender: TObject);

begin
  InsertSymbol(btQuestion.Caption, btPhonetics12.Caption);
end;

end.

