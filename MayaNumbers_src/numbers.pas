{*****************************************}
{* Main unit for MayaNumbers application *}
{*****************************************}

unit numbers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Math, help;

type
  {***********}
  { TfNumbers }
  {***********}
  TfNumbers = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileTest, mFileConversion, mFileExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsMax, mSettingsMax100, mSettingsMax1000, mSettingsMax10000, mSettingsMax1000000: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    Label8: TLabel;
    cbMayaArab, cbArabMaya: TCheckBox;
    edArabic, edQuestions, edCorrect, edFalse, edSuccess, edEval: TEdit;
    imMaya1, imMaya2, imMaya3, imMaya4, imMaya5: TImage;
    Shape1, shMaya1, shMaya2, shMaya3, shMaya4, shMaya5: TShape;
    imNumeral0, imNumeral1, imNumeral2, imNumeral3, imNumeral4: TImage;
    imNumeral5, imNumeral6, imNumeral7, imNumeral8, imNumeral9: TImage;
    imNumeral10, imNumeral11, imNumeral12, imNumeral13, imNumeral14: TImage;
    imNumeral15, imNumeral16, imNumeral17, imNumeral18, imNumeral19: TImage;
    btQuestion: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileTestClick(Sender: TObject);
    procedure mFileConversionClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsMax100Click(Sender: TObject);
    procedure mSettingsMax1000Click(Sender: TObject);
    procedure mSettingsMax10000Click(Sender: TObject);
    procedure mSettingsMax1000000Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure imMaya1Click(Sender: TObject);
    procedure imMaya2Click(Sender: TObject);
    procedure imMaya3Click(Sender: TObject);
    procedure imMaya4Click(Sender: TObject);
    procedure imMaya5Click(Sender: TObject);
    procedure imNumeral0DblClick(Sender: TObject);
    procedure imNumeral10DblClick(Sender: TObject);
    procedure imNumeral11DblClick(Sender: TObject);
    procedure imNumeral12DblClick(Sender: TObject);
    procedure imNumeral13DblClick(Sender: TObject);
    procedure imNumeral14DblClick(Sender: TObject);
    procedure imNumeral15DblClick(Sender: TObject);
    procedure imNumeral16DblClick(Sender: TObject);
    procedure imNumeral17DblClick(Sender: TObject);
    procedure imNumeral18DblClick(Sender: TObject);
    procedure imNumeral19DblClick(Sender: TObject);
    procedure imNumeral1DblClick(Sender: TObject);
    procedure imNumeral2DblClick(Sender: TObject);
    procedure imNumeral3DblClick(Sender: TObject);
    procedure imNumeral4DblClick(Sender: TObject);
    procedure imNumeral5DblClick(Sender: TObject);
    procedure imNumeral6DblClick(Sender: TObject);
    procedure imNumeral7DblClick(Sender: TObject);
    procedure imNumeral8DblClick(Sender: TObject);
    procedure imNumeral9DblClick(Sender: TObject);
    procedure cbArabMayaChange(Sender: TObject);
    procedure cbMayaArabChange(Sender: TObject);
  private
    iSMax, iMax, iSQuestions, iQuestions, iQuestion, iCorrect, iTest, iSel: Integer;
    iAnswer: LongInt;
    sEmpty: string;
    aMaya, aMayaUser: array[0..4] of Integer;
    aDone: array of Integer;
    imMaya: array[0..4] of TImage;
    shMaya: array[0..4] of TShape;
    imNumerals: array[0..19] of TImage;
  end;

var
  fNumbers: TfNumbers;

implementation

{$R *.lfm}

{ Clear form controls }

procedure ClearForm(Empty: string);

var
  I: Integer;

begin
  fNumbers.edArabic.Text := '';
  fNumbers.edArabic.ReadOnly := False; fNumbers.edArabic.TabStop := True;
  fNumbers.edEval.Text := ''; fNumbers.edEval.Color := clForm;
  fNumbers.edArabic.Color := clDefault;
  fNumbers.edQuestions.Text := ''; fNumbers.edCorrect.Text := ''; fNumbers.edFalse.Text := '';
  fNumbers.edSuccess.Text := ''; fNumbers.edSuccess.Color := clForm;
  for I := 0 to 4 do begin
    fNumbers.imMaya[I].Picture.LoadFromFile(Empty);
    fNumbers.shMaya[I].Visible := False;
  end;
end;

{ Prepare to start a new test }

procedure NewTest(SQuestions, SMax: Integer; StartUp: Boolean; out Questions, Max, Question, Correct, Sel: Integer; Empty: string);

begin
  ClearForm(Empty);
  fNumbers.stTitle.Caption := 'Maya numbers test.';
  fNumbers.btQuestion.Caption := 'Start';
  fNumbers.btQuestion.Enabled := True;
  if not StartUp then                                                          // to avoid "Can't focus" error
    fNumbers.btQuestion.SetFocus;
  Questions := SQuestions; Max := SMax;                                        // selected settings become active now
  Question := 0; Correct := 0;
  Sel := -1;                                                                   // no answer picture selected
end;

{ Select Maya number symbol field }

procedure SelectMayaSymbolField(Test: Integer; var Sel: Integer; MX: Integer);

begin

  if ((Test = 0) and (fNumbers.btQuestion.Caption = 'Answer')) or
     (fNumbers.btQuestion.Caption = 'Convert') and fNumbers.cbMayaArab.Checked then begin
    if fNumbers.shMaya[MX].Visible then begin
      // If field is selected, unselect it
      fNumbers.shMaya[MX].Visible := False;
      Sel := -1;
    end
    else begin
      // If field not selected, select it
      if Sel <> -1 then
        // Unselect field previously selected
        fNumbers.shMaya[Sel].Visible := False;
      fNumbers.shMaya[MX].Visible := True;
      Sel := MX;
    end;
  end;
end;

{ Select Maya number symbol }

procedure SelectMayaNumeral(Test, MX, NX: Integer; var Maya: array of Integer);

begin
  if ((Test = 0) and (fNumbers.btQuestion.Caption = 'Answer')) or
     (fNumbers.btQuestion.Caption = 'Convert') and fNumbers.cbMayaArab.Checked then begin
    if MX = -1 then begin
      MessageDlg('Input error', 'You must first select a field, where to place this numeral!', mtError, [mbOK], 0);
    end
    else begin
      fNumbers.imMaya[MX].Picture := fNumbers.imNumerals[NX].Picture;
      Maya[MX] := NX;
    end;
  end;
end;

{ Arabic number to Maya number conversion }

procedure ArabicToMaya(Arabic: LongInt; var Maya: array of Integer);

var
  IX, I: Integer;
  Quotient, Value: LongInt;
  Remainder: array of LongInt;

begin
  for I := 0 to 4 do
    Maya[I] := -1;
  Quotient := Arabic;
  IX := 0; SetLength(Remainder, 0);
  while Quotient <> 0 do begin
    Value := Quotient;
    SetLength(Remainder, Length(Remainder) + 1);
    Remainder[IX] := Value mod 20;
    Quotient := Value div 20;
    Inc(IX);
  end;
  IX := 0;
  for I := Length(Remainder) - 1 downto 0 do begin
    Maya[IX] := Remainder[I];
    Inc(IX);
  end;
end;

{ Maya number to Arabic number conversion }

procedure MayaToArabic(Maya: array of Integer; out Arabic: LongInt);

var
  N, P, I: Integer;

begin
  Arabic := -1;
  N := 0;
  repeat
    Inc(N);
  until (Maya[N] = -1) or (N = 5);
  Arabic := 0; P := 0;
  for I := N - 1 downto 0 do begin
    Arabic += Maya[I] * Round(IntPower(20, P));
    Inc(P);
  end;
end;

{***********}
{ TfNumbers }
{***********}

{ Application start: Initialisation }

procedure TfNumbers.FormCreate(Sender: TObject);

begin
  imMaya[0] := imMaya1; imMaya[1] := imMaya2;
  imMaya[2] := imMaya3; imMaya[3] := imMaya4; imMaya[4] := imMaya5;
  shMaya[0] := shMaya1; shMaya[1] := shMaya2;
  shMaya[2] := shMaya3; shMaya[3] := shMaya4; shMaya[4] := shMaya5;
  imNumerals[0] := imNumeral0; imNumerals[1] := imNumeral1; imNumerals[2] := imNumeral2;
  imNumerals[3] := imNumeral3; imNumerals[4] := imNumeral4; imNumerals[5] := imNumeral5;
  imNumerals[6] := imNumeral6; imNumerals[7] := imNumeral7; imNumerals[8] := imNumeral8;
  imNumerals[9] := imNumeral9; imNumerals[10] := imNumeral10; imNumerals[11] := imNumeral11;
  imNumerals[12] := imNumeral12; imNumerals[13] := imNumeral13; imNumerals[14] := imNumeral14;
  imNumerals[15] := imNumeral15; imNumerals[16] := imNumeral16; imNumerals[17] := imNumeral17;
  imNumerals[18] := imNumeral18; imNumerals[19] := imNumeral19;
  sEmpty := GetCurrentDir + '/pics/' + 'empty.jpg';
  DoDirSeparators(sEmpty);
  iSQuestions := 20; iSMax := 100;
  Randomize;
  // Start application with a new test (20 questions; max number to convert = 100)
  NewTest(iSQuestions, iSMax, True, iQuestions, iMax, iQuestion, iCorrect, iSel, sEmpty);
end;

{ Menu item "File > New test": Prepare to start a new test }

procedure TfNumbers.mFileTestClick(Sender: TObject);

begin
  NewTest(iSQuestions, iSMax, False, iQuestions, iMax, iQuestion, iCorrect, iSel, sEmpty);
end;

{ Menu item "File > Conversion": Prepare for Maya number to Arabic number or vice-versa conversion }

procedure TfNumbers.mFileConversionClick(Sender: TObject);

var
  I: Integer;
  Mess: string;

begin
  ClearForm(sEmpty); Mess := '';
  stTitle.Caption := 'Maya numbers conversion.';
  if cbMayaArab.Checked and cbArabMaya.Checked then
    Mess := 'You cannot select both conversion check boxes at the same time'
  else if (not cbMayaArab.Checked) and (not cbArabMaya.Checked) then
    Mess := 'You must select one of the two conversion check boxes';
  if Mess = '' then begin
    for I := 0 to 4 do begin
      aMayaUser[I] := -1;
      shMaya[I].Visible := False;
    end;
    iSel := -1;
    if cbArabMaya.Checked then begin
      edArabic.SetFocus;
    end
    else begin
      iSel := 0;
      shMaya[iSel].Visible := True;
    end;
    btQuestion.Enabled := True;
    btQuestion.Caption := 'Convert';
  end
  else begin
    MessageDlg('Input error', Mess + '!', mtError, [mbOK], 0);
    btQuestion.Enabled := False;
  end;
end;

{ Menu item "File > Exit": Exit application }

procedure TfNumbers.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of questions": Get number of test questions from user }

procedure TfNumbers.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Maya numbers test', 'Number of questions', IntToStr(iSQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                    // minimum of questions arbitrarily fixed to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);                                              // temporarily variable will be active if a new test is started
  end;
end;

{ Menu items "Settings > Maximum > ...": Set maximum number to be used in exercises }

procedure TfNumbers.mSettingsMax100Click(Sender: TObject);

begin
  mSettingsMax100.Checked := True;    mSettingsMax1000.Checked := False;
  mSettingsMax10000.Checked := False; mSettingsMax1000000.Checked := False;
  iSMax := 100;
end;

procedure TfNumbers.mSettingsMax1000Click(Sender: TObject);

begin
  mSettingsMax100.Checked := False;   mSettingsMax1000.Checked := True;
  mSettingsMax10000.Checked := False; mSettingsMax1000000.Checked := False;
  iSMax := 1000;
end;

procedure TfNumbers.mSettingsMax10000Click(Sender: TObject);

begin
  mSettingsMax100.Checked := False;  mSettingsMax1000.Checked := False;
  mSettingsMax10000.Checked := True; mSettingsMax1000000.Checked := False;
  iSMax := 10000;
end;

procedure TfNumbers.mSettingsMax1000000Click(Sender: TObject);

begin
  mSettingsMax100.Checked := False;   mSettingsMax1000.Checked := False;
  mSettingsMax10000.Checked := False; mSettingsMax1000000.Checked := True;
  iSMax := 1000000;
end;

{ Menu item "Help > Help": Display Maya numerals and application help }

procedure TfNumbers.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfNumbers.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Maya numbers conversion and knowledge test.' + LineEnding;
  S += 'Version 1.0, © allu, October 2022.';
  MessageDlg('About "MayaNumbers"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next/Answer/Convert": Take action, depending on button caption }

procedure TfNumbers.btQuestionClick(Sender: TObject);

var
  Min, IX, I: Integer;
  Arabic: LongInt;
  Mess: string;
  OK, Correct: Boolean;

begin
  // Button "Start/Next": Generate new test question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Next') then begin
    if btQuestion.Caption = 'Start' then
      // The aDone array will store those numbers, that have already been done
      SetLength(aDone, 0);
    // If one or both checkboxes are selected, proceed
    if cbMayaArab.Checked or cbArabMaya.Checked then begin
      Inc(iQuestion);
      // Determine if question will be Maya-Arabic or Arabic-Maya conversion
      if cbMayaArab.Checked and cbArabMaya.Checked then
        iTest := Random(2)
      else if cbMayaArab.Checked then
        iTest := 1
      else
        iTest := 0;
      // Get random number between 1 and maximum selected (that hasn't yet been done)
      repeat
        OK := True;
        if Random(20) = 0 then                                                 // avoid to much "too easy" numbers between 0 and 20
          Min := 0
        else
          Min := 20;
        Arabic := Random(iMax - Min + 1) + Min;
        // Check if this number has already been done
        for I := 0 to Length(aDone) - 1 do begin
          if Arabic = aDone[I] then
            OK := False;
        end;
      until OK;
      SetLength(aDone, Length(aDone) + 1);
      aDone[Length(aDone) - 1] := Arabic;                                      // mark the number as done
      ArabicToMaya(Arabic, aMaya);                                             // convert Arabic number to Maya number
      // Test = Arabic numeral to Maya numeral conversion
      if iTest = 0 then begin
        edArabic.ReadOnly := True; edArabic.TabStop := False;
        edArabic.Text := IntToStr(Arabic);
        for I := 0 to 4 do begin
          imMaya[I].Picture.LoadFromFile(sEmpty);
          shMaya[I].Visible := False;
          aMayaUser[I] := -1;                                                  // the aMayaUser array will be filled when the user doubel-clicks some Maya numeral picture
        end;
        shMaya[0].Visible := True; iSel := 0;                                  // select (focus) first Maya number symbol field
      end
      // Test = Maya numeral to Arabic numeral conversion
      else begin
        for I := 0 to 4 do begin
          // Display the Maya number
          if aMaya[I] = -1 then
            imMaya[I].Picture.LoadFromFile(sEmpty)
          else
            imMaya[I].Picture := imNumerals[aMaya[I]].Picture;
          shMaya[I].Visible := False;
        end;
        iSel := -1;
        iAnswer := Arabic;                                                     // correct answer to this question
        edArabic.ReadOnly := False; edArabic.TabStop := True;
        edArabic.Text := '';
        edArabic.SetFocus;                                                     // focus Arabic number input field
      end;
      btQuestion.Caption := 'Answer';
    end
    // If no checkbox selected, display error message
    else
      MessageDlg('Selection error', 'You must select at least one of the 2 checkboxes!', mtError, [mbOK], 0);
  end
  // Button "Answer": Check user's answer
  else if btQuestion.Caption = 'Answer' then begin
    if iTest = 0 then begin
      // Arabic to Maya conversion
      Correct := True;
      for I := 0 to 4 do begin
        // If one of the symbols "entered" by the user differs from the actual Maya number's one, the user answer is false
        if aMayaUser[I] <> aMaya[I] then
          Correct := False;
      end;
    end
    else begin
      // Maya to Arabic conversion
      Correct := False;
      if (edArabic.Text <> '') and (StrToInt(edArabic.Text) = iAnswer) then begin
        // User answer must be the numeric value of the actual number
        Correct := True;
      end;
    end;
    // Evaluation
    if Correct then begin
      // User answer is correct
      Inc(iCorrect);
      edEval.Text := 'Correct answer!';
      edEval.Color := clForm;
    end
    else begin
      // User answer is false
      edEval.Text := 'False answer!';
      edEval.Color := clRed;
    end;
    // Update evaluation counters
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iQuestion - iCorrect);
    edSuccess.Text := IntToStr(Round(100 * (iCorrect) / iQuestion)) + '%';
    // Check if all questions have been done
    if iQuestion = iQuestions then begin
      MessageDlg('End of test', 'All questions done. You may use the "File" menu to start a new test.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // User must use "File" menu item to proceed
    end
    else
      btQuestion.Caption := 'Next';
  end
  // Button "Convert": Convert Maya number to Arabic number or vice-versa
  else begin
    Mess := '';
    if cbArabMaya.Checked then begin
      // Convert Arabic number to Maya number
      if edArabic.Text = '' then begin
        Mess := 'You must enter the Arabic number to be converted';
        edArabic.SetFocus;
      end
      else begin
        Arabic := StrToInt(edArabic.Text);
        if Arabic >= IntPower(20, 5) then begin
          Mess := 'The number entered is too big to fit in 5 Maya numeral symbol fields';
          edArabic.SetFocus;
        end
        else begin
          ArabicToMaya(Arabic, aMaya);                                         // convert Arabic number to Maya number
          // Display Maya number
          for I := 0 to 4 do begin
            if aMaya[I] = -1 then
              imMaya[I].Picture.LoadFromFile(sEmpty)
            else
              imMaya[I].Picture := imNumerals[aMaya[I]].Picture;
          end;
        end;
      end;
    end
    else begin
      // Convert Maya number to Arabic number
      OK := False;
      for I := 0 to 4 do begin
        if aMayaUser[I] <> -1 then                                             // there must be at least one non empty numeral symbol field
          OK := True;
      end;
      if not OK then begin
        Mess := 'You must enter the Maya number to be converted';
        iSel := 0;
        shMaya[iSel].Visible := True;
      end
      else begin
        if aMayaUser[0] = -1 then                                              // the first numeral symbol field must always be filled
          OK := False;
        if OK then begin
          // Check if the numeral symbol fields are filled without "empty fields"
          IX := 0;
          repeat
            Inc(IX);
          until (aMayaUser[IX] = -1) or (IX = 4);
          for I := IX + 1 to 4 do begin
            if aMayaUser[I] <> -1 then
              OK := False;
          end;
        end;
        if not OK then
          Mess := 'You must enter the Maya number from top to bottom without letting empty number symbol fields';
      end;
      if Mess = '' then begin
        // User input ok: Do the conversion
        MayaToArabic(aMayaUser, Arabic);                                       // convert Maya number to Arabic number
        edArabic.Text := IntToStr(Arabic);
        shMaya[iSel].Visible := False;
        iSel := -1;
      end;
    end;
    if Mess <> '' then
      MessageDlg('Input error', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Button "Clear": Clear Maya number symbol field resp. clear all }

procedure TfNumbers.btClearClick(Sender: TObject);

var
  I: Integer;

begin
  if (btQuestion.Caption = 'Answer') or (btQuestion.Caption = 'Convert') then begin
    if iSel = -1 then begin
      // If no Maya number symbol is selected, the "Clear" button clears all input controls
      if ((btQuestion.Caption = 'Answer') and (iTest = 1)) or (btQuestion.Caption = 'Convert') then
        edArabic.Text := '';
      if ((btQuestion.Caption = 'Answer') and (iTest = 0)) or (btQuestion.Caption = 'Convert') then begin
        for I := 0 to 4 do begin
          aMayaUser[I] := -1;
          imMaya[I].Picture.LoadFromFile(sEmpty);
          shMaya[I].Visible := False;
        end;
      end;
    end
    else begin
      // If a Maya number symbol is selected, the "Clear" button clears only this one
      if ((btQuestion.Caption = 'Answer') and (iTest = 0)) or (btQuestion.Caption = 'Convert') then begin
        aMayaUser[iSel] := -1;
        imMaya[iSel].Picture.LoadFromFile(sEmpty);
        shMaya[iSel].Visible := False;
        iSel := -1;
      end;
    end;
  end;
end;

{ Maya numeral symbol fields selection (by user click on image) }

procedure TfNumbers.imMaya1Click(Sender: TObject);

begin
  SelectMayaSymbolField(iTest, iSel, 0);
end;

procedure TfNumbers.imMaya2Click(Sender: TObject);

begin
  SelectMayaSymbolField(iTest, iSel, 1);
end;

procedure TfNumbers.imMaya3Click(Sender: TObject);

begin
  SelectMayaSymbolField(iTest, iSel, 2);
end;

procedure TfNumbers.imMaya4Click(Sender: TObject);

begin
  SelectMayaSymbolField(iTest, iSel, 3);
end;

procedure TfNumbers.imMaya5Click(Sender: TObject);

begin
  SelectMayaSymbolField(iTest, iSel, 4);
end;

{ Maya numeral symbol double-clicked by user (to be placed in actually selected numeral symbol field) }

procedure TfNumbers.imNumeral0DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 0, aMayaUser);
end;

procedure TfNumbers.imNumeral1DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 1, aMayaUser);
end;

procedure TfNumbers.imNumeral2DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 2, aMayaUser);
end;

procedure TfNumbers.imNumeral3DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 3, aMayaUser);
end;

procedure TfNumbers.imNumeral4DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 4, aMayaUser);
end;

procedure TfNumbers.imNumeral5DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 5, aMayaUser);
end;

procedure TfNumbers.imNumeral6DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 6, aMayaUser);
end;

procedure TfNumbers.imNumeral7DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 7, aMayaUser);
end;

procedure TfNumbers.imNumeral8DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 8, aMayaUser);
end;

procedure TfNumbers.imNumeral9DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 9, aMayaUser);
end;

procedure TfNumbers.imNumeral10DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 10, aMayaUser);
end;

procedure TfNumbers.imNumeral11DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 11, aMayaUser);
end;

procedure TfNumbers.imNumeral12DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 12, aMayaUser);
end;

procedure TfNumbers.imNumeral13DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 13, aMayaUser);
end;

procedure TfNumbers.imNumeral14DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 14, aMayaUser);
end;

procedure TfNumbers.imNumeral15DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 15, aMayaUser);
end;

procedure TfNumbers.imNumeral16DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 16, aMayaUser);
end;

procedure TfNumbers.imNumeral17DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 17, aMayaUser);
end;

procedure TfNumbers.imNumeral18DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 18, aMayaUser);
end;

procedure TfNumbers.imNumeral19DblClick(Sender: TObject);

begin
  SelectMayaNumeral(iTest, iSel, 19, aMayaUser);
end;

{ Type of conversion change (checkbox clicked by user) }

procedure TfNumbers.cbArabMayaChange(Sender: TObject);

begin
  if cbArabMaya.Checked then begin
    if btQuestion.Caption = 'Convert' then begin
      cbMayaArab.Checked := False;                                             // automatically uncheck "Maya to Arabic" conversion
      mFileConversion.Click;
    end;
  end;
end;

procedure TfNumbers.cbMayaArabChange(Sender: TObject);

begin
  if cbMayaArab.Checked then begin
    if btQuestion.Caption = 'Convert' then begin
      cbArabMaya.Checked := False;                                             // automatically uncheck "Arabic to Maya" conversion
      mFileConversion.Click;
    end;
  end;
end;

end.

