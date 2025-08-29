{**************************************}
{* Main unit for Sanskrit application *}
{**************************************}

unit sanskrit_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8;

type
  TLetter = record
    LetterType: Char;
    Sanskrit, Transcription, Phonetics: string;
  end;
  TAllLetters = array[1..49] of TLetter;
  TButtons = array[1..49] of TButton;
  TLetters = array of TLetter;
  TLettersDone = array of Boolean;
  {************}
  { TfSanskrit }
  {************}
  TfSanskrit = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileSanskrit, mFileLatin, mFileExit: TMenuItem;
    mOptions, mOptionsSanskrit, mOptionsLettersVowels, mOptionsLettersConsonants1, mOptionsLettersConsonants2: TMenuItem;
    mOptionsLatin, mOptionsLatinTrans, mOptionsLatinPhon: TMenuItem;
    mOptionsDisableVC, mOptionsDisableAM: TMenuItem;
    mOptionsRepeat, MenuItem1, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laLetterUser, laLetterGiven: TLabel;
    edLetterUser, edLetterGiven, edEval: TEdit;
    btQuestion: TButton;
    bt01, bt02, bt03, bt04, bt05, bt06, bt07, bt08, bt09, bt10, bt11, bt12: TButton;
    bt13, bt14, bt15, bt16, bt17, bt18, bt19, bt20, bt21, bt22, bt23, bt24: TButton;
    bt25, bt26, bt27, bt28, bt29, bt30, bt31, bt32, bt33, bt34, bt35, bt36: TButton;
    bt37, bt38, bt39, bt40, bt41, bt42, bt43, bt44, bt45, bt46, bt47, bt48, bt49: TButton;
    Memo1: TMemo;
    laFont12, laFont20: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure mFileSanskritClick(Sender: TObject);
    procedure mFileLatinClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsLettersVowelsClick(Sender: TObject);
    procedure mOptionsLettersConsonants1Click(Sender: TObject);
    procedure mOptionsLettersConsonants2Click(Sender: TObject);
    procedure mOptionsLatinTransClick(Sender: TObject);
    procedure mOptionsLatinPhonClick(Sender: TObject);
    procedure mOptionsDisableVCClick(Sender: TObject);
    procedure mOptionsDisableAMClick(Sender: TObject);
    procedure mOptionsRepeatClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
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
    procedure bt34Click(Sender: TObject);
    procedure bt35Click(Sender: TObject);
    procedure bt36Click(Sender: TObject);
    procedure bt37Click(Sender: TObject);
    procedure bt38Click(Sender: TObject);
    procedure bt39Click(Sender: TObject);
    procedure bt40Click(Sender: TObject);
    procedure bt41Click(Sender: TObject);
    procedure bt42Click(Sender: TObject);
    procedure bt43Click(Sender: TObject);
    procedure bt44Click(Sender: TObject);
    procedure bt45Click(Sender: TObject);
    procedure bt46Click(Sender: TObject);
    procedure bt47Click(Sender: TObject);
    procedure bt48Click(Sender: TObject);
    procedure bt49Click(Sender: TObject);
  private
    iTest, iLetter, iLetters, iCorrect: Integer;
    aAllLetters: TAllLetters;
    aLetters: TLetters;
    aLettersDone: TLettersDone;
    btLetters: TButtons;
  end;

var
  fSanskrit: TfSanskrit;

implementation

{$R *.lfm}

{ Read letters from text file }

procedure ReadLetters(out Letters: TAllLetters);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'letters.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      Letters[N].Sanskrit := UTF8Trim(UTF8Copy(Line, 1, 5));
      Letters[N].Transcription := UTF8Trim(UTF8Copy(Line, 6, 5));
      Letters[N].Phonetics := UTF8Trim(UTF8Copy(Line, 11, 10));
      Letters[N].LetterType := UTF8Copy(Line, 21, 11)[1];
    end;
  end;
  Close(InFile);
end;

{ Get letters for test (those actually selected: vowels, alpaprāṇa, mahāprāṇa) }

procedure GetLetters(var AllLetters: TAllLetters; out Letters: TLetters; out LettersDone: TLettersDone);

var
  N, I: Integer;
  Insert: Boolean;

begin
  N := 0;
  for I := 1 to 49 do begin
    Insert := False;
    if ((AllLetters[I].LetterType = 'S') or (AllLetters[I].LetterType = 'L')) and fSanskrit.mOptionsLettersVowels.Checked then
      Insert := True
    else if (AllLetters[I].LetterType = 'A') and fSanskrit.mOptionsLettersConsonants1.Checked then
      Insert := True
    else if (AllLetters[I].LetterType = 'M') and fSanskrit.mOptionsLettersConsonants2.Checked then
      Insert := True;
    if Insert then begin
      Inc(N); SetLength(Letters, N); SetLength(LettersDone, N);
      Letters[N - 1] := AllLetters[I];
      LettersDone[N - 1] := False;
    end;
  end;
end;

{ Show letters (Sanskrit letter, transcription or phonetics) on buttons }

procedure ShowLetters(Test: Integer; var Letters: TAllLetters; var Buttons: TButtons);

var
  I: Integer;

begin
  if (not fSanskrit.mOptionsLettersVowels.Checked) and (not fSanskrit.mOptionsLettersConsonants1.Checked) and (not fSanskrit.mOptionsLettersConsonants2.Checked) then
    MessageDlg('Invalid selection', 'There must be at least one letter type selected!', mtError, [mbOK], 0)
  else begin
    for I := 1 to 49 do begin
      Buttons[I].Enabled := True;
      if Test = 1 then begin
        // Sanskrit letters
        Buttons[I].Caption := Letters[I].Sanskrit;
        Buttons[I].Font.Size := fSanskrit.laFont20.Font.Size;
      end
      else begin
        // Transcription or phonetics
        if fSanskrit.mOptionsLatinTrans.Checked then
          Buttons[I].Caption := Letters[I].Transcription
        else
          Buttons[I].Caption := Letters[I].Phonetics;
        Buttons[I].Font.Size := fSanskrit.laFont12.Font.Size;
        // Disable buttons with letters actually not used (if these options are selected)
        if fSanskrit.mOptionsDisableVC.Checked then begin
          // Disable vowals or consonants
          if (not fSanskrit.mOptionsLettersConsonants1.Checked) and (not fSanskrit.mOptionsLettersConsonants2.Checked) then begin
            if (Letters[I].LetterType <> 'S') and (Letters[I].LetterType <> 'L') then
              Buttons[I].Enabled := False;
          end
          else if not fSanskrit.mOptionsLettersVowels.Checked then begin
            if (Letters[I].LetterType = 'S') or (Letters[I].LetterType = 'L') then
              Buttons[I].Enabled := False;
          end;
        end;
        if fSanskrit.mOptionsDisableAM.Checked then begin
          // Disable alpaprāṇa or mahāprāṇa
          if (fSanskrit.mOptionsLettersConsonants1.Checked) and (not fSanskrit.mOptionsLettersConsonants2.Checked) then begin
            if (Letters[I].LetterType = 'M') then
              Buttons[I].Enabled := False;
          end
          else if (fSanskrit.mOptionsLettersConsonants2.Checked) and (not fSanskrit.mOptionsLettersConsonants1.Checked) then begin
            if (Letters[I].LetterType = 'A') then
              Buttons[I].Enabled := False;
          end;
        end;
      end;
    end;
  end;
end;

{ Insert letter/transcription/phonetics corr. to buttton pushed into user letter edit field }

procedure InsertLetter(Action, Letter: string);

begin
  if Action = 'Answer' then                                                    // do only if a user input is waited for
    fSanskrit.edLetterUser.Text := Letter;
end;

{************}
{ TfSanskrit }
{************}

{ Application start: Initialisation }

procedure TfSanskrit.FormCreate(Sender: TObject);

begin
  // Create array with letter buttons
  btLetters[1]  := bt01; btLetters[2]  := bt02; btLetters[3]  := bt03; btLetters[4]  := bt04; btLetters[5]  := bt05;
  btLetters[6]  := bt06; btLetters[7]  := bt07; btLetters[8]  := bt08; btLetters[9]  := bt09; btLetters[10] := bt10;
  btLetters[11] := bt11; btLetters[12] := bt12; btLetters[13] := bt13; btLetters[14] := bt14; btLetters[15] := bt15;
  btLetters[16] := bt16; btLetters[17] := bt17; btLetters[18] := bt18; btLetters[19] := bt19; btLetters[20] := bt20;
  btLetters[21] := bt21; btLetters[22] := bt22; btLetters[23] := bt23; btLetters[24] := bt24; btLetters[25] := bt25;
  btLetters[26] := bt26; btLetters[27] := bt27; btLetters[28] := bt28; btLetters[29] := bt29; btLetters[30] := bt30;
  btLetters[31] := bt31; btLetters[32] := bt32; btLetters[33] := bt33; btLetters[34] := bt34; btLetters[35] := bt35;
  btLetters[36] := bt36; btLetters[37] := bt37; btLetters[38] := bt38; btLetters[39] := bt39; btLetters[40] := bt40;
  btLetters[41] := bt41; btLetters[42] := bt42; btLetters[43] := bt43; btLetters[44] := bt44; btLetters[45] := bt45;
  btLetters[46] := bt46; btLetters[47] := bt47; btLetters[48] := bt48; btLetters[49] := bt49;
  // Read letters data
  ReadLetters(aAllLetters);
  // Start random number generator
  Randomize;
  // Start with "Sanskrit letters" test
  mFileSanskrit.Click;
end;

{ Menu item "File > Sanskrit letters": New test, where user has to enter the Sanskrit letter }

procedure TfSanskrit.mFileSanskritClick(Sender: TObject);

begin
  iTest := 1;
  edLetterGiven.Text := ''; edLetterUser.Text := ''; edEval.Text := '';
  laLetterUser.Caption := 'Letter';
  edLetterUser.Left := 307;
  // In this test, either the transcription, or the phonetics are given
  if mOptionsLatinTrans.Checked then begin
    laLetterGiven.Caption := 'Transcription';
    edLetterGiven.Left := 115;
  end
  else begin
    laLetterGiven.Caption := 'Phonetics';
    edLetterGiven.Left := 93;
  end;
  // Adapt font size (implies adaption of edit field top position)
  edLetterGiven.Font.Size := laFont12.Font.Size; edLetterGiven.Top := 64;
  edLetterUser.Font.Size  := laFont20.Font.Size; edLetterUser.Top  := 56;
  // Show Sanskrit letters on buttons
  ShowLetters(iTest, aAllLetters, btLetters);
  // Set button for start of new test and (re-)enable "Options" menu
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
  mOptions.Enabled := True;
end;

{ Menu item "File > Latin letters": New test, where user has to enter the Latin transcription/phonetics }

procedure TfSanskrit.mFileLatinClick(Sender: TObject);

begin
  iTest := 2;
  edLetterGiven.Text := ''; edLetterUser.Text := ''; edEval.Text := '';
  laLetterGiven.Caption := 'Letter';
  edLetterGiven.Left := 64;
  // The user has to enter either the transcription or the phonetics
  if mOptionsLatinTrans.Checked then begin
    laLetterUser.Caption := 'Transcription';
    edLetterUser.Left := 360;
  end
  else begin
    laLetterUser.Caption := 'Phonetics';
    edLetterUser.Left := 336;
  end;
  // Adapt font size (implies adaption of edit field top position)
  edLetterGiven.Font.Size := laFont20.Font.Size; edLetterGiven.Top := 56;
  edLetterUser.Font.Size  := laFont12.Font.Size; edLetterUser.Top  := 64;
  // Show Latin transcription/phonetics on buttons
  ShowLetters(iTest, aAllLetters, btLetters);
  // Set button for start of new test and (re-)enable "Options" menu
  btQuestion.Caption := 'Start'; btQuestion.Enabled := True;
  mOptions.Enabled := True;
end;

{ Menu item "File > Exit": Exit application }

procedure TfSanskrit.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Sanskrit letters > ...": Choose letter set to be included for test }

procedure TfSanskrit.mOptionsLettersVowelsClick(Sender: TObject);

begin
  if mOptionsLettersVowels.Checked then
    mOptionsLettersVowels.Checked := False
  else
    mOptionsLettersVowels.Checked := True;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

procedure TfSanskrit.mOptionsLettersConsonants1Click(Sender: TObject);

begin
  if mOptionsLettersConsonants1.Checked then
    mOptionsLettersConsonants1.Checked := False
  else
    mOptionsLettersConsonants1.Checked := True;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

procedure TfSanskrit.mOptionsLettersConsonants2Click(Sender: TObject);

begin
  if mOptionsLettersConsonants2.Checked then
    mOptionsLettersConsonants2.Checked := False
  else
    mOptionsLettersConsonants2.Checked := True;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

{ Menu items "Options > Latin letters > ...": Select Latin letters as transcription or as phonetics }

procedure TfSanskrit.mOptionsLatinTransClick(Sender: TObject);

begin
  mOptionsLatinTrans.Checked := True; mOptionsLatinPhon.Checked := False;
  if iTest = 1 then begin
    laLetterGiven.Caption := 'Transcription';
    edLetterGiven.Left := 115;
  end
  else begin
    laLetterUser.Caption := 'Transcription';
    edLetterUser.Left := 360;
  end;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

procedure TfSanskrit.mOptionsLatinPhonClick(Sender: TObject);

begin
  mOptionsLatinTrans.Checked := False; mOptionsLatinPhon.Checked := True;
  if iTest = 1 then begin
    laLetterGiven.Caption := 'Phonetics';
    edLetterGiven.Left := 93;
  end
  else begin
    laLetterUser.Caption := 'Phonetics';
    edLetterUser.Left := 336;
  end;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

{ Menu item "Options > Disable vowals/consonants buttons": Toggle between enabling all buttons/disabling the letters not used }

procedure TfSanskrit.mOptionsDisableVCClick(Sender: TObject);

begin
  if mOptionsDisableVC.Checked then
    mOptionsDisableVC.Checked := False
  else
    mOptionsDisableVC.Checked := True;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

{ Menu item "Options > Disable alpaprāṇa/mahāprāṇa buttons": Toggle between enabling all buttons/disabling the consonants not used }

procedure TfSanskrit.mOptionsDisableAMClick(Sender: TObject);

begin
  if mOptionsDisableAM.Checked then
    mOptionsDisableAM.Checked := False
  else
    mOptionsDisableAM.Checked := True;
  if iTest = 2 then
    ShowLetters(iTest, aAllLetters, btLetters);
end;

{ Menu item "Option > Repeat wrong answers": Toggle repetition of not known letters or not }

procedure TfSanskrit.mOptionsRepeatClick(Sender: TObject);

begin
  if mOptionsRepeat.Checked then
    mOptionsRepeat.Checked := False
  else
    mOptionsRepeat.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfSanskrit.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Learn the Sanskrit letters.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, May 2021.';
  MessageDlg('About "Sanskrit"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfSanskrit.btQuestionClick(Sender: TObject);

var
  I, J, I1, I2: Integer;
  Answer, UAnswer, S: string;
  Done, E: Boolean;

begin
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    if (not fSanskrit.mOptionsLettersVowels.Checked) and (not fSanskrit.mOptionsLettersConsonants1.Checked) and (not fSanskrit.mOptionsLettersConsonants2.Checked) then
      MessageDlg('Invalid selection', 'There must be at least one letter type selected!', mtError, [mbOK], 0)
    else begin
      // Button "Start/Question": Generate new question (randomly choose new letter)
      if btQuestion.Caption = 'Start' then begin
        if iTest = 1 then begin
          // Change button captions: Random letter distribution
          for I := 1 to 100 do begin
            I1 := Random(49) + 1; I2 := Random(49) + 1;
            S := btLetters[I1].Caption; btLetters[I1].Caption := btLetters[I2].Caption; btLetters[I2].Caption := S;
          end;
        end
        else begin
          // Change button captions: Move disabled letters to end of letter-grid
          for I := 1 to 48 do begin
            for J := I + 1 to 49 do begin
              if btLetters[J].Enabled and not btLetters[I].Enabled then begin
                S := btLetters[I].Caption; btLetters[I].Caption := btLetters[J].Caption; btLetters[J].Caption := S;
                E := btLetters[I].Enabled; btLetters[I].Enabled := btLetters[J].Enabled; btLetters[J].Enabled := E;
              end;
            end;
          end;
        end;
        // Choose letter set as selected by user
        GetLetters(aAllLetters, aLetters, aLettersDone);
        iLetters := Length(aLetters); iCorrect := 0;
        // Disable "Options" menu until test is over (or user starts a new test)
        mOptions.Enabled := False;
      end;
      edLetterGiven.Text := ''; edLetterUser.Text := ''; edEval.Text := '';
      // Random letter (in current set, among those not yet asked resp. not correctly answered)
      repeat
        iLetter := Random(iLetters);
      until not aLettersDone[iLetter];
      // Fill form fields, depending on actual test type
      if iTest = 1 then begin
        // Sanskrit letter to find with transcription/phonetics given
        if mOptionsLatin.Checked then begin
          // Phonetics symbol given
          edLetterGiven.Text := aLetters[iLetter].Phonetics;
        end
        else begin
          // Transcription character(s) given
          edLetterGiven.Text := aLetters[iLetter].Transcription;
        end;
      end
      else begin
        // Transcription/phonetics to find with Sanskrit letter given
        edLetterGiven.Text := aLetters[iLetter].Sanskrit;
      end;
      // Next button push will be to check user answer
      edLetterUser.SetFocus;
      btQuestion.Caption := 'Answer';
    end;
  end
  else begin
    // Button "Answer": Check user answer
    UAnswer := edLetterUser.Text;
    if iTest = 1 then begin
      // "Find Sanskrit letter" test
      Answer := aLetters[iLetter].Sanskrit;
    end
    else begin
      // "Find Sanskrit letter transcription/phonetics" test
      if mOptionsLatinTrans.Checked then begin
        // "Find Sanskrit letter transcription" test
        Answer := aLetters[iLetter].Transcription;
      end
      else begin
        // "Find Sanskrit letter phonetics" test
        Answer := aLetters[iLetter].Phonetics;
      end;
    end;
    // Evaluation
    if UAnswer = Answer then begin
      // Correct answer
      edEval.Text := 'Correct!';
      Inc(iCorrect);
      aLettersDone[iLetter] := True;                                           // mark letter as "done"
    end
    else begin
      // False answer
      edEval.Text := 'False!';
      if not mOptionsRepeat.Checked then
        aLettersDone[iLetter] := True;;                                        // mark letter as "done", unless "Repeat wrong letter" is checked
    end;
    // Check if there are "undone" letters left
    Done := True;
    for I := 0 to iLetters - 1 do begin
      if not aLettersDone[I] then
        Done := False;
    end;
    if Done then begin
      // All letters "done": End of test
      S := 'All letters have been done...';
      if not mOptionsRepeat.Checked then begin
        // Tell users what their success percentage is
        S += LineEnding;
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

{ Letter buttons: Insert Sanskrit letter or Latin transcription/phonetics into user letter edit field }

procedure TfSanskrit.bt01Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt01.Caption);
end;

procedure TfSanskrit.bt02Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt02.Caption);
end;

procedure TfSanskrit.bt03Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt03.Caption);
end;

procedure TfSanskrit.bt04Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt04.Caption);
end;

procedure TfSanskrit.bt05Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt05.Caption);
end;

procedure TfSanskrit.bt06Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt06.Caption);
end;

procedure TfSanskrit.bt07Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt07.Caption);
end;

procedure TfSanskrit.bt08Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt08.Caption);
end;

procedure TfSanskrit.bt09Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt09.Caption);
end;

procedure TfSanskrit.bt10Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt10.Caption);
end;

procedure TfSanskrit.bt11Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt11.Caption);
end;

procedure TfSanskrit.bt12Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt12.Caption);
end;

procedure TfSanskrit.bt13Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt13.Caption);
end;

procedure TfSanskrit.bt14Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt14.Caption);
end;

procedure TfSanskrit.bt15Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt15.Caption);
end;

procedure TfSanskrit.bt16Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt16.Caption);
end;

procedure TfSanskrit.bt17Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt17.Caption);
end;

procedure TfSanskrit.bt18Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt18.Caption);
end;

procedure TfSanskrit.bt19Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt19.Caption);
end;

procedure TfSanskrit.bt20Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt20.Caption);
end;

procedure TfSanskrit.bt21Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt21.Caption);
end;

procedure TfSanskrit.bt22Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt22.Caption);
end;

procedure TfSanskrit.bt23Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt23.Caption);
end;

procedure TfSanskrit.bt24Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt24.Caption);
end;

procedure TfSanskrit.bt25Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt25.Caption);
end;

procedure TfSanskrit.bt26Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt26.Caption);
end;

procedure TfSanskrit.bt27Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt27.Caption);
end;

procedure TfSanskrit.bt28Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt28.Caption);
end;

procedure TfSanskrit.bt29Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt29.Caption);
end;

procedure TfSanskrit.bt30Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt30.Caption);
end;

procedure TfSanskrit.bt31Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt31.Caption);
end;

procedure TfSanskrit.bt32Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt32.Caption);
end;

procedure TfSanskrit.bt33Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt33.Caption);
end;

procedure TfSanskrit.bt34Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt34.Caption);
end;

procedure TfSanskrit.bt35Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt35.Caption);
end;

procedure TfSanskrit.bt36Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt36.Caption);
end;

procedure TfSanskrit.bt37Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt37.Caption);
end;

procedure TfSanskrit.bt38Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt38.Caption);
end;

procedure TfSanskrit.bt39Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt39.Caption);
end;

procedure TfSanskrit.bt40Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt40.Caption);
end;

procedure TfSanskrit.bt41Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt41.Caption);
end;

procedure TfSanskrit.bt42Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt42.Caption);
end;

procedure TfSanskrit.bt43Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt43.Caption);
end;

procedure TfSanskrit.bt44Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt44.Caption);
end;

procedure TfSanskrit.bt45Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt45.Caption);
end;

procedure TfSanskrit.bt46Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt46.Caption);
end;

procedure TfSanskrit.bt47Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt47.Caption);
end;

procedure TfSanskrit.bt48Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt48.Caption);
end;

procedure TfSanskrit.bt49Click(Sender: TObject);

begin
  InsertLetter(btQuestion.Caption, bt49.Caption);
end;

end.

