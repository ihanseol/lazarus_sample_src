{***********************************}
{* Main unit for Roman application *}
{***********************************}

unit roman_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, roman_u2;

type
  {*********}
  { TfRoman }
  {*********}
  TfRoman = class(TForm)
    stTitle: TStaticText;
    mMenu: TMainMenu;
    mFile, mFileTest, mFileConversion, mFileExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsMax, mSettingsMax100, mSettingsMax1000, mSettingsMax3999: TMenuItem;
    mHelp, mHelpHelp, mHelpRoman, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    cbRomArab: TCheckBox;
    cbArabRom: TCheckBox;
    edRoman, edArabic, edEval: TEdit;
    edQuestions, edCorrect, edSuccess: TEdit;
    btQuestion: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileTestClick(Sender: TObject);
    procedure mFileConversionClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsMax100Click(Sender: TObject);
    procedure mSettingsMax1000Click(Sender: TObject);
    procedure mSettingsMax3999Click(Sender: TObject);
    procedure mHelpRomanClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    iSMax, iMax, iSQuestions, iQuestions, iQuestion, iCorrect, iTest: Integer;
    sAnswer: string;
    aDone: array of Integer;
  end;

var
  fRoman: TfRoman;

implementation

{$R *.lfm}

{ Clear form controls }

procedure ClearForm;

begin
  fRoman.edRoman.Text := ''; fRoman.edArabic.Text := '';
  fRoman.edRoman.ReadOnly := False; fRoman.edArabic.ReadOnly := False;
  fRoman.edRoman.TabStop := True; fRoman.edArabic.TabStop := True;
  fRoman.edEval.Text := ''; fRoman.edEval.Color := clForm;
  fRoman.edRoman.Color := clDefault; fRoman.edArabic.Color := clDefault;
  fRoman.edQuestions.Text := ''; fRoman.edCorrect.Text := '';
  fRoman.edSuccess.Text := ''; fRoman.edSuccess.Color := clForm;
end;

{ Prepare to start a new test }

procedure NewTest(SQuestions, SMax: Integer; StartUp: Boolean; out Questions, Max, Question, Correct: Integer);

begin
  ClearForm;
  fRoman.stTitle.Caption := 'Roman numerals test.';
  fRoman.cbArabRom.Enabled := True; fRoman.cbRomArab.Enabled := True;
  fRoman.btQuestion.Caption := 'Start';
  fRoman.btQuestion.Enabled := True;
  if not StartUp then                                                          // to avoid "Can't focus" error
    fRoman.btQuestion.SetFocus;
  fRoman.btClear.Enabled := False;
  Questions := SQuestions;                                                     // temporarily variable becomes now active
  Max := SMax;                                                                 // temporarily variable becomes now active
  Question := 0; Correct := 0;
end;

{ Test if string is all numerical digits }

function IsAllIntegerDigits(S: string): Boolean;

var
  N, Code: Integer;
  OK: Boolean;

begin
  OK := False;
  Val(S, N, Code);
  if Code = 0 then begin
    if N = Int(N) then
      OK := True;
  end;
  IsAllIntegerDigits := OK;
end;

{ Test if string is all characters allowed in Roman numerals }

function IsAllRomanDigits(S: string): Boolean;

var
  I: Integer;
  OK: Boolean;

begin
  OK := False; S := UpperCase(S);
  if Length(S) > 0 then begin
    OK := True;
    for I := 1 to Length(S) do begin
      if not (S[I] in ['I', 'V', 'X', 'L', 'C', 'D', 'M']) then
        OK := False;
    end;
  end;
  IsAllRomanDigits := OK;
end;

{ Convert Arabic numeral to Roman numeral }

function ArabicToRoman(Arabic: Integer): string;

var
  Roman: string;

begin
  Roman := '';
  if Arabic <= 3999 then begin                                                 // maximum Roman number with 'simple' numeric system
    while Arabic >= 1000 do begin
      Roman += 'M';                                                            // each 1000 = 'M'
      Arabic -= 1000;
    end;
    if Arabic >= 900 then begin
      Roman += 'CM';                                                           // 900 = 'CM'
      Arabic -= 900;
    end
    else if Arabic >= 500 then begin
      Roman += 'D';                                                            // 500 = 'D'
      Arabic -= 500;
    end
    else if Arabic >= 400 then begin
      Roman += 'CD';                                                           // 400 = 'CD'
      Arabic -= 400;
    end;
    while Arabic >= 100 do begin
      Roman += 'C';                                                            // each 100 = 'C'
      Arabic -= 100;
    end;
    if Arabic >= 90 then begin
      Roman += 'XC';                                                           // 90 = 'XC'
      Arabic -= 90;
    end
    else if Arabic >= 50 then begin
      Roman += 'L';                                                            // 50 = 'L'
      Arabic -= 50;
    end
    else if Arabic >= 40 then begin
      Roman += 'XL';                                                           // 40 = 'XL'
      Arabic -= 40;
    end;
    while Arabic >= 10 do begin
      Roman += 'X';                                                            // each 10 = 'X'
      Arabic -= 10;
    end;
    if Arabic = 9 then begin
      Roman += 'IX';                                                           // 9 = 'IX'
      Arabic -= 9;
    end
    else if Arabic >= 5 then begin
      Roman += 'V';                                                            // 5 = 'V'
      Arabic -= 5;
    end
    else if Arabic = 4 then begin
      Roman += 'IV';                                                           // 4 = 'IV'
      Arabic -= 4;
    end;
    while Arabic >= 1 do begin
      Roman += 'I';                                                            // each 1 = 'I'
      Arabic -= 1;
    end;
  end;
  ArabicToRoman := Roman;
end;

{ Convert Roman numeral to Arabic numeral }

function RomanToArabic(Roman: string): Integer;

var
  Arabic, C, P: Integer;
  Err: Boolean;

begin
  Arabic := 0; Roman := Uppercase(Roman); Err := False;
  C := 0;
  while LeftStr(Roman, 1) = 'M' do begin
    Arabic += 1000; Inc(C);                                                    // each 'M' = 1000
    Delete(Roman, 1, 1);
  end;
  if C > 3 then
    Err := True
  else begin
    if LeftStr(Roman, 2) = 'CM' then begin
      Arabic += 900;                                                           // 'CM' = 900
      Delete(Roman, 1, 2);
      P := Pos('C', Roman);
      if (P <> 0) and (Copy(Roman, P - 1, 2) <> 'XC') then
        Err := True;
      P := Pos('D', Roman);
      if P <> 0 then
        Err := True;
    end;
  end;
  if not Err then begin
    P := Pos('M', Roman);
    if P <> 0 then
      Err := True;
  end;
  if not Err then begin
    if LeftStr(Roman, 1) = 'D' then begin
      Arabic += 500;                                                           // 'D' = 500
      Delete(Roman, 1, 1);
    end
    else if LeftStr(Roman, 2) = 'CD' then begin
      Arabic += 400;                                                           // 'CD' = 400
      Delete(Roman, 1, 2);
      P := Pos('C', Roman);
      if (P <> 0) and (Copy(Roman, P - 1, 2) <> 'XC') then
        Err := True;
    end;
  end;
  if not Err then begin
    P := Pos('D', Roman);
    if P <> 0 then
      Err := True;
  end;
  if not Err then begin
    C := 0;
    while LeftStr(Roman, 1) = 'C' do begin
      Arabic += 100; Inc(C);                                                   // each 'C' = 100
      Delete(Roman, 1, 1);
    end;
    if C > 3 then
      Err := True;
  end;
  if not Err then begin
    if LeftStr(Roman, 2) = 'XC' then begin
      Arabic += 90;                                                            // 'XC' = 90
      Delete(Roman, 1, 2);
      P := Pos('X', Roman);
      if (P <> 0) and (Copy(Roman, P - 1, 2) <> 'IX') then
        Err := True;
      P := Pos('L', Roman);
      if P <> 0 then
        Err := True;
    end;
  end;
  if not Err then begin
    P := Pos('C', Roman);
    if P <> 0 then
      Err := True;
  end;
  if not Err then begin
    if LeftStr(Roman, 1) = 'L' then begin
      Arabic += 50;                                                            // 'L' = 50
      Delete(Roman, 1, 1);
    end
    else if LeftStr(Roman, 2) = 'XL' then begin
      Arabic += 40;                                                            // 'XL' = 40
      Delete(Roman, 1, 2);
      P := Pos('X', Roman);
      if (P <> 0) and (Copy(Roman, P - 1, 2) <> 'IX') then
        Err := True;
    end;
    P := Pos('L', Roman);
    if P <> 0 then
      Err := True;
  end;
  if not Err then begin
    C := 0;
    while LeftStr(Roman, 1) = 'X' do begin
      Arabic += 10; Inc(C);                                                    // each 'X' = 10
      Delete(Roman, 1, 1);
    end;
    if C > 3 then
      Err := True;
  end;
  if not Err then begin
    if LeftStr(Roman, 2) = 'IX' then begin
      Arabic += 9;                                                             // 'IX' = 9
      Delete(Roman, 1, 2);
      P := Pos('V', Roman);
      if P <> 0 then
        Err := True;
      P := Pos('I', Roman);
      if P <> 0 then
        Err := True;
    end;
  end;
  if not Err then begin
    P := Pos('X', Roman);
    if P <> 0 then
      Err := True;
  end;
  if not Err then begin
    if LeftStr(Roman, 1) = 'V' then begin
      Arabic += 5;                                                             // 'V' = 5
      Delete(Roman, 1, 1);
    end
    else if LeftStr(Roman, 2) = 'IV' then begin
      Arabic += 4;                                                             // 'IV' = 4
      Delete(Roman, 1, 2);
      P := Pos('I', Roman);
      if P <> 0 then
        Err := True;
    end;
    P := Pos('V', Roman);
    if P <> 0 then
      Err := True;
  end;
  if not Err then begin
    C := 0;
    while LeftStr(Roman, 1) = 'I' do begin
      Arabic += 1; Inc(C);                                                     // each 'I' = 1
      Delete(Roman, 1, 1);
    end;
    if C > 3 then
      Err := True;
  end;
  if Err then
    Arabic := -1;                                                              // returning -1 = invalid Roman numeral
  RomanToArabic := Arabic;
end;

{*********}
{ TfRoman }
{*********}

{ Application start: Initialisation }

procedure TfRoman.FormCreate(Sender: TObject);

begin
  iSQuestions := 20; iSMax := 1000;
  NewTest(iSQuestions, iSMax, True, iQuestions, iMax, iQuestion, iCorrect);          // prepare to start a new test
  Randomize;
end;

{ Menu item "File > New test": Prepare to start a new test }

procedure TfRoman.mFileTestClick(Sender: TObject);

begin
  cbRomArab.Enabled := False; cbArabRom.Enabled := False;
  NewTest(iSQuestions, iSMax, False, iQuestions, iMax, iQuestion, iCorrect);
end;

{ Menu item "File > Conversion": Prepare for Roman to Arabic numeral or vice-versa conversion }

procedure TfRoman.mFileConversionClick(Sender: TObject);

begin
  ClearForm;
  fRoman.stTitle.Caption := 'Roman numerals conversion.';
  cbRomArab.Checked := True; cbArabRom.Checked := True;
  cbRomArab.Enabled := False; cbArabRom.Enabled := False;
  btQuestion.Caption := 'Convert';
  btQuestion.Enabled := True;
  btClear.Enabled := True;
  edRoman.SetFocus;
end;

{ Menu item "File > Exit": Exit application }

procedure TfRoman.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of questions": Get number of test questions from user }

procedure TfRoman.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Roman numerals test', 'Number of questions', IntToStr(iSQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                    // minimum of questions arbitrarily fixed to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);                                              // temporarily variable will be active if a new test is started
  end;
end;

{ Menu items "Settings > Maximum > ...": Set maximum number to be used in exercises }

procedure TfRoman.mSettingsMax100Click(Sender: TObject);

begin
  mSettingsMax100.Checked := True;
  mSettingsMax1000.Checked := False;
  mSettingsMax3999.Checked := False;
  iSMax := 100;
end;

procedure TfRoman.mSettingsMax1000Click(Sender: TObject);

begin
  mSettingsMax100.Checked := False;
  mSettingsMax1000.Checked := True;
  mSettingsMax3999.Checked := False;
  iSMax := 1000;
end;

procedure TfRoman.mSettingsMax3999Click(Sender: TObject);

begin
  mSettingsMax100.Checked := False;
  mSettingsMax1000.Checked := False;
  mSettingsMax3999.Checked := True;
  iSMax := 3999;
end;

{ Menu item "Help > Roman numerals": Display help concerning Roman numerals }

procedure TfRoman.mHelpRomanClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('roman.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display help concerning program usage }

procedure TfRoman.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfRoman.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Roman numerals conversion and knowledge test.' + LineEnding;
  S += 'Version 1.0, © allu, February 2019.';
  MessageDlg('About "Roman"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next/Answer/Convert": Take action, depending on button caption }

procedure TfRoman.btQuestionClick(Sender: TObject);

var
  Arabic, I: Integer;
  UAnswer: string;
  OK: Boolean;

begin
  // Button "Start/Next": Generate test new question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Next') then begin
    if btQuestion.Caption = 'Start' then
      // The aDone array will store those numbers, that have already been done
      SetLength(aDone, 0);
    // If one or both checkboxes are selected, proceed
    if cbRomArab.Checked or cbArabRom.Checked then begin
      Inc(iQuestion);
      // Determine if question will be Roman-Arabic or Arabic-Roman conversion
      if cbRomArab.Checked and cbArabRom.Checked then
        iTest := Random(2)
      else if cbRomArab.Checked then
        iTest := 1
      else
        iTest := 0;
      // Get random number between 1 and maximum selected (that hasn't yet been done)
      repeat
        OK := True;
        Arabic := Random(iMax) + 1;
        for I := 0 to Length(aDone) - 1 do begin
          if Arabic = aDone[I] then
            OK := False;
        end;
      until OK;
      SetLength(aDone, Length(aDone) + 1);
      aDone[Length(aDone) - 1] := Arabic;                                      // mark the number as done
      // Test = Arabic numeral to Roman numeral conversion
      if iTest = 0 then begin
        edArabic.Color := clCream; edRoman.Color := clDefault;
        edArabic.ReadOnly := True; edRoman.ReadOnly := False;
        edArabic.TabStop := False; edRoman.TabStop := True;
        edArabic.Text := IntToStr(Arabic); edRoman.Text := '';
        sAnswer := ArabicToRoman(Arabic);                                      // convert number to Roman numeral
        btQuestion.Caption := 'Answer';
        edRoman.SetFocus;
      end
      // Test = Roman numeral to Arabic numeral conversion
      else begin
        edArabic.Color := clDefault; edRoman.Color := clCream;
        edArabic.ReadOnly := False; edRoman.ReadOnly := True;
        edArabic.TabStop := True; edRoman.TabStop := False;
        edArabic.Text := ''; edRoman.Text := ArabicToRoman(Arabic);            // convert number to Roman numeral
        sAnswer := IntToStr(Arabic);
        edArabic.SetFocus;
        btQuestion.Caption := 'Answer';
      end;
    end
    // If no checkbox selected, display error message
    else
      MessageDlg('Selection error', 'You must select at least one of the 2 checkboxes!', mtError, [mbOK], 0);
  end
  // Button "Answer": Check user's answer
  else if btQuestion.Caption = 'Answer' then begin
    if iTest = 0 then
      // Arabic to Roman conversion
      UAnswer := UpperCase(edRoman.Text)
    else
      // Roman to Arabic conversion
      UAnswer := edArabic.Text;
    // User answer is correct
    if UAnswer = sAnswer then begin
      Inc(iCorrect);
      edEval.Text := 'Correct!';
      edEval.Color := clForm;
    end
    // User answer is false
    else begin
      edEval.Text := 'False! Correct answer = ' + sAnswer + '.';
      edEval.Color := clRed;
    end;
    // Update evaluation counters
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edSuccess.Text := IntToStr(Round(100 * (iCorrect) / iQuestion)) + '%';
    if iQuestion = iQuestions then begin
      MessageDlg('End of test', 'All questions done. You may use the "File" menu to start a new test.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // User must use "File" menu item to proceed
    end
    else
      btQuestion.Caption := 'Next';
  end
  // Button "Convert": Convert Roman to Arabic numeral or vice-versa
  else begin
    // If one of the edit fields contains a value, proceed
    if (edRoman.Text <> '') or (edArabic.Text <> '') then begin
      if (edRoman.Text <> '') and (edArabic.Text <> '') then
        // If both edit fields contain values, display error message
        MessageDlg('Data error', 'You can''t enter both a Roman and an Arabic numeral!', mtError, [mbOK], 0)
      else begin
        // If one of the edit fields is empty, do the conversion (to this format)
        if edRoman.Text <> '' then begin
          // Roman to Arabic numeral conversion
          // ----------------------------------
          if IsAllRomanDigits(edRoman.Text) then begin
            // If the value entered only contains I, V, X, L, C, D, M, try the conversion
            Arabic := RomanToArabic(edRoman.Text);                             // Convert Roman to Arabic numeral
            if Arabic > 0 then
              // Conversion was successful
              edArabic.Text := IntToStr(Arabic)
            else
              // Conversion did not succeed (invalid Roman numeral): Display error message
              MessageDlg('Data error', 'The Roman numeral entered is invalid!', mtError, [mbOK], 0);
            btClear.SetFocus;
          end
          else
            // If the value entered contains characters other than I, V, X, L, C, D, M, display error message
            MessageDlg('Data error', 'The Roman numeral entered contains invalid characters!', mtError, [mbOK], 0);
        end
        else begin
          // Roman to Arabic numeral conversion
          // ----------------------------------
          if IsAllIntegerDigits(edArabic.Text) then begin
            // If value entered is an integer number, try the conversion
            Arabic := StrToInt(edArabic.Text);
            if (Arabic > 0) and (Arabic <= 3999) then begin
              // Maximum number that may be represented by I - M system = 3999
              edRoman.Text := ArabicToRoman(Arabic);                           // Convert Arabic to Roman numeral
              btClear.SetFocus;
            end
            else
              // Numbers bigger than 3999 can't be converted using the I - M system: Display error message
              MessageDlg('Data error', 'Cannot convert this number to Roman numeral!', mtError, [mbOK], 0);
          end
          else
            // If value entered isn't an integer number, display error message
            MessageDlg('User error', 'The Arabic numeral entered is not a valid integer number!', mtError, [mbOK], 0)
        end;
      end;
    end
    // If both edit fields are empty, display error message
    else
      MessageDlg('User error', 'You must enter a Roman or an Arabic numeral!', mtError, [mbOK], 0);
  end;
end;

{ Button "Clear": Clear edit fields }

procedure TfRoman.btClearClick(Sender: TObject);

begin
  edRoman.Text := ''; edArabic.Text := '';
end;

end.

