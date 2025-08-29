{**************************************}
{* Main unit for Numerals application *}
{**************************************}

unit num10;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8;

type
  TNumerals = array of record
    Language: string;
    Group: Integer;
    Regional: Boolean;
    Numerals: array[1..10] of string;
  end;
  {************}
  { TfNumerals }
  {************}
  TfNumerals = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestLanguage, mTestNumber, mTextExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsRegional: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laQuestion, laLanguage, laNumeral: TLabel;
    edLanguage, edNumeral: TEdit;
    laNum1, laNum2, laNum3, laNum4, laNum5: TLabel;
    edNum1, edNum2, edNum3, edNum4, edNum5: TEdit;
    cbNum1, cbNum2, cbNum3, cbNum4, cbNum5: TCheckBox;
    stNumLang1, stNumLang2, stNumLang3, stNumLang4, stNumLang5: TStaticText;
    edLang1, edLang2, edLang3, edLang4, edLang5: TEdit;
    cbNumLang1, cbNumLang2, cbNumLang3, cbNumLang4, cbNumLang5: TCheckBox;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestLanguageClick(Sender: TObject);
    procedure mTestNumberClick(Sender: TObject);
    procedure mTextExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsRegionalClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iLanguages, iQuestions, iTest, iQuestions0, iQuestion, iCorrect, iXLang, iXNum: Integer;
    bRegional, bRegional0, bSelCorrect: Boolean;
    aXLangs: array[0..4] of Integer;
    laNums: array[0..4] of TLabel;
    edNums, edLangs: array[0..4] of TEdit;
    stNumLangs: array[0..4] of TStaticText;
    cbNums, cbNumLangs: array[0..4] of TCheckbox;
    aNumerals: TNumerals;
  end;

var
  fNumerals: TfNumerals;

implementation

{$R *.lfm}

{ Read numerals from text file }

procedure ReadNumerals(out Numerals: TNumerals; out N: Integer);

var
  I: Integer;
  Line, Lang: string;
  Reg: Boolean;
  InFile: Text;

begin
  N := 0; SetLength(Numerals, 0);
  Assign(InFile, 'numerals.txt'); Reset(InFile);
  while not EoF(Infile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Numerals, N);
      Numerals[N - 1].Group := StrToInt(Line[1]);                                   // language group
      Delete(Line, 1, 2);
      Lang := UTF8Trim(UTF8Copy(Line, 1, 15));                                      // language
      Reg := False;
      if UTF8Copy(Lang, UTF8Length(Lang), 1) = '*' then begin                       // '*' indicates a regional language
        Reg := True;
        UTF8Delete(Lang, UTF8Length(Lang), 1);
      end;
      Numerals[N - 1].Language := Lang;
      Numerals[N - 1].Regional := Reg;
      for I := 1 to 10 do
        Numerals[N - 1].Numerals[I] := UTF8Trim(UTF8Copy(Line, I * 10 + 6, 10));    // numerals for this language
    end;
  end;
  Close(InFile);
end;

{************}
{ TfNumerals }
{************}

{ Application start: Initialization }

procedure TfNumerals.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create arrays with labels, edit fields and checkboxes
  laNums[0] := laNum1; laNums[1] := laNum2; laNums[2] := laNum3; laNums[3] := laNum4; laNums[4] := laNum5;
  edNums[0] := edNum1; edNums[1] := edNum2; edNums[2] := edNum3; edNums[3] := edNum4; edNums[4] := edNum5;
  cbNums[0] := cbNum1; cbNums[1] := cbNum2; cbNums[2] := cbNum3; cbNums[3] := cbNum4; cbNums[4] := cbNum5;
  stNumLangs[0] := stNumLang1; stNumLangs[1] := stNumLang2; stNumLangs[2] := stNumLang3; stNumLangs[3] := stNumLang4; stNumLangs[4] := stNumLang5;
  edLangs[0] := edLang1; edLangs[1] := edLang2; edLangs[2] := edLang3; edLangs[3] := edLang4; edLangs[4] := edLang5;
  cbNumLangs[0] := cbNumLang1; cbNumLangs[1] := cbNumLang2; cbNumLangs[2] := cbNumLang3; cbNumLangs[3] := cbNumLang4; cbNumLangs[4] := cbNumLang5;
  // Move test 2 components to same vertical position as test 1 items
  laNumeral.Top := laLanguage.Top; edNumeral.Top := edLanguage.Top;
  for I := 0 to 4 do begin
    stNumLangs[I].Top := edNums[I].Top - 5;
    edLangs[I].Top := cbNums[I].Top - 15;
    cbNumLangs[I].Top := edLangs[I].Top + 40;
  end;
  btQuestion.Top := btQuestion.Top - 70;
  fNumerals.Height := fNumerals.Height - 70;
  // Read numerals from text file
  ReadNumerals(aNumerals, iLanguages);
  if iLanguages <> 45 then
    stTitle.Caption := StringReplace(stTitle.Caption, '45', IntToStr(iLanguages), []);
  // Initialize options
  iQuestions0 := 20; bRegional0 := False;
  // Start random number generator
  Randomize;
  // Start a language test
  mTestLanguage.Click;
end;

{ Menu item "Test > Language test": Start a language test }

procedure TfNumerals.mTestLanguageClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 1;
  // Show test 1 components, hide test 2 components
  laLanguage.Visible := True;  edLanguage.Visible := True;
  laNumeral.Visible  := False; edNumeral.Visible  := False;
  laQuestion.Caption := 'Indicate the incorrect numerals for this language.';
  for I := 0 to 4 do begin
    laNums[I].Visible := True;
    edNums[I].Visible := True;
    cbNums[I].Visible := True;
    stNumLangs[I].Visible := False;
    edLangs[I].Visible := False;
    cbNumLangs[I].Visible := False;
  end;
  // Enable "Start" button with caption "Start"
  btQuestion.Caption := 'Start';
  btQuestion.Enabled := True;
end;

{ Menu item "Test > Numerals test": Start a numerals test }

procedure TfNumerals.mTestNumberClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 2;
  // Show test 2 components, hide test 1 components
  laLanguage.Visible := False; edLanguage.Visible := False;
  laNumeral.Visible  := True;  edNumeral.Visible  := True;
  laQuestion.Caption := 'Indicate the incorrect language for this numeral.';
  for I := 0 to 4 do begin
    laNums[I].Visible := False;
    edNums[I].Visible := False;
    cbNums[I].Visible := False;
    stNumLangs[I].Visible := True;
    edLangs[I].Visible := True;
    cbNumLangs[I].Visible := True;
  end;
  // Enable "Start" button with caption "Start"
  btQuestion.Caption := 'Start';
  btQuestion.Enabled := True;
end;

{ Menu item "Test > Exit": Exit application }

procedure TfNumerals.mTextExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User input of number of test questions }

procedure TfNumerals.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Numerals', 'Number of questions', IntToStr(iQuestions0));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);                                                     // input value will become active with new test
    if iQuestions0 < 10 then
      iQuestions0 := 10;                                                            // arbitrarily chosen minimum
  end;
end;

{ Menu item "Options > Include regional languages": Toggle include/exclude regional languages }

procedure TfNumerals.mOptionsRegionalClick(Sender: TObject);

begin
  if mOptionsRegional.Checked then
    mOptionsRegional.Checked := False
  else
    mOptionsRegional.Checked := True;
  bRegional0 := mOptionsRegional.Checked;
end;

{ Menu item "Help > About": Display application about }

procedure TfNumerals.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Foreign languages:' + LineEnding;
  S += 'Numerals from 1 to 10 in 45 languages.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, December 2023.';
  MessageDlg('About "Numerals"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer" pushed: Generate question resp. check answer }

procedure TfNumerals.btQuestionClick(Sender: TObject);

var
  Group, N, C, L, XL, I, J, K: Integer;
  Num, S: string;
  OK, Correct, ItemCorrect: Boolean;
  Nums: array[0..4] of Integer;

begin
  // Button "Start/Question" pushed: Generate question }
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Button "Start" pushed: Some initialization to do
    if btQuestion.Caption = 'Start' then begin
      iQuestions := iQuestions0; bRegional := bRegional0;                           // options become active now
      iQuestion := 0; iCorrect := 0;
    end;
    Inc(iQuestion);
    // Randomly choose if user should find correct or incorrect proposals
    if Random(3) = 0 then begin
      bSelCorrect := True;
      laQuestion.Caption := StringReplace(laQuestion.Caption, ' incorrect ', ' correct ', []);
    end
    else begin
      bSelCorrect := False;
      laQuestion.Caption := StringReplace(laQuestion.Caption, ' correct ', ' incorrect ', []);
    end;
    // Generate language test
    if iTest = 1 then begin
      // Random language
      repeat
        iXLang:= Random(iLanguages);
      until (bRegional) or (not aNumerals[iXLang].Regional);
      // Random numerals (all different)
      Nums[0] := Random(10) + 1;
      for I := 1 to 4 do begin
        repeat
          N := Random(10) + 1; OK := True;
          for J := 0 to I - 1 do begin
            if N = Nums[J] then
              OK := False;
          end;
        until OK;
        Nums[I] := N;
      end;
      // Display language
      edLanguage.Text := aNumerals[iXLang].Language;
      // Sort and display numerals
      for I := 0 to 3 do begin
        for J := I + 1 to 4 do begin
          if Nums[J] < Nums[I] then begin
            N := Nums[I]; Nums[I] := Nums[J]; Nums[J] := N;
          end;
        end;
      end;
      for I := 0 to 4 do begin
        laNums[I].Caption := IntToStr(Nums[I]);
        edNums[I].Text := '';
        edNums[I].Font.Color := clDefault;
      end;
      // Generate answer proposals
      C := Random(5) + 1;                                                           // random number of correct proposals
      for I := 0 to 4 do begin
        // Random "position" for this proposal
        repeat
          J := Random(5);
        until edNums[J].Text = '';                                                  // this position must not already be filled in
        // This position will contain a correct answer proposal
        if C > 0 then begin
          edNums[J].Text := aNumerals[iXLang].Numerals[StrToInt(laNums[J].Caption)];
          Dec(C);
        end
        // This position will contain an incorrect answer proposal
        else begin
          // Answer proposal is this numeral in an incorrect language
          repeat
            OK := True;
            // Choose a random language within the same group, regional only
            // if this option is selected
            L := Random(iLanguages);
            if L = iXLang then
              OK := False
            else if aNumerals[L].Group <> aNumerals[iXLang].Group then
              OK := False
            else if (not bRegional) and aNumerals[L].Regional then
              OK := False
            else begin
              Num := aNumerals[L].Numerals[StrToInt(laNums[J].Caption)];
            end;
          until OK;
          edNums[J].Text := Num;
        end;
      end;
    end
    // Generate numerals test
    else begin
      for I := 0 to 4 do begin
        edLangs[I].Text := '';
        edLangs[I].Font.Color := clDefault;
      end;
      // Random numeral
      iXNum:= Random(10) + 1; edNumeral.Text := IntToStr(iXNum);
      C := Random(5) + 1;                                                           // random number of correct proposals
      Group := 0;
      for I := 0 to 4 do begin
        // Random "position" of this proposal
        repeat
          J := Random(5);
        until edLangs[J].Text = '';                                                 // this position must not already be filled in
        repeat
          OK := True;
          // Choose a random language within the same group, regional only
          // if this option is selected and all languages having to be different
          XL:= Random(iLanguages);
          if (not bRegional) and aNumerals[XL].Regional then
            OK := False
          else if (Group <> 0) and (aNumerals[XL].Group <> Group) then
            OK := False
          else begin
            for K := 0 to 4 do begin
              if aNumerals[XL].Language = edLangs[K].Text then
                OK := False;
            end;
          end;
        until OK;
        // Take the group of the first language as group for all languages
        if Group = 0 then
          Group := aNumerals[XL].Group;
        // This position will contain a correct answer proposal
        if C > 0 then begin
          stNumLangs[J].Caption := aNumerals[XL].Numerals[iXNum];
          edLangs[J].Text := aNumerals[XL].Language;
          Dec(C);
        end
        // This position will contain an incorrect answer proposal
        else begin
          // Answer proposal is the numeral in an incorrect language
          repeat
            OK := True;
            // Choose a random language within the same group, regional only
            // if this option is selected
            L := Random(iLanguages);
            if L = iXLang then
              OK := False
            else if (not bRegional) and aNumerals[L].Regional then
              OK := False
            else if (Group <> 0) and (aNumerals[L].Group <> Group) then
            OK := False;
          until OK;
          stNumLangs[J].Caption := aNumerals[L].Numerals[iXNum];
          edLangs[J].Text := aNumerals[XL].Language;
        end;
        // Save language used for answers check
        aXLangs[J] := XL;
      end;
    end;
    // Next button push will be answer checking
    btQuestion.Caption := 'Answer';
  end
  // Button "Answer" pushed: Check user answer
  else begin
    Correct := True;
    for I := 0 to 4 do begin
      // Language test
      if iTest = 1 then begin
        Num := aNumerals[iXLang].Numerals[StrToInt(laNums[I].Caption)]; ItemCorrect := True;
        if bSelCorrect then begin
          if ((cbNums[I].Checked) and (Num <> edNums[I].Text)) or ((not cbNums[I].Checked) and (Num = edNums[I].Text)) then
            ItemCorrect := False;
        end
        else begin
          if ((cbNums[I].Checked) and (Num = edNums[I].Text)) or ((not cbNums[I].Checked) and (Num <> edNums[I].Text)) then
            ItemCorrect := False;
        end;
        // Mark wrong answers using red font color
        if not ItemCorrect then begin
          edNums[I].Font.Color := clRed;
          Correct := False;
        end;
      end
      // Numerals test
      else begin
        Num := aNumerals[aXLangs[I]].Numerals[iXNum]; ItemCorrect := True;
        if bSelCorrect then begin
          if ((cbNumLangs[I].Checked) and (Num <> stNumLangs[I].Caption)) or ((not cbNumLangs[I].Checked) and (Num = stNumLangs[I].Caption)) then
            ItemCorrect := False;
        end
        else begin
          if ((cbNumLangs[I].Checked) and (Num = stNumLangs[I].Caption)) or ((not cbNumLangs[I].Checked) and (Num <> stNumLangs[I].Caption)) then
            ItemCorrect := False;
        end;
        // Mark wrong answers using red font color
        if not ItemCorrect then begin
          edLangs[I].Font.Color := clRed;
          Correct := False;
        end;
      end;
      // Adapt correct abswers counter
      if Correct then
        Inc(iCorrect);
    end;
    // Next button push will be a new question generation
    btQuestion.Caption := 'Question';
    // If all questions have been done, display test score
    if iQuestion = iQuestions then begin
      S := 'From ' + IntToStr(iQuestions) + ' questions, you answered ' + IntToStr(iCorrect) + ' correctly. ';
      S += 'Your score is ' + IntToStr(Round(100 * iCorrect / iQuestions)) + '%.';
      MessageDlg('End of Numerals test', S, mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                                  // disable button (user must start a new test from the "Test" menu)
    end;
  end;
end;

end.

