{*****************************************}
{* Main unit for Arithmetic7 application *}
{*****************************************}

unit arithmetic7_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Math, arithmetic7_help;

type
  TOperators = array of Char;
  TOperands = array of Real;
  TArray8 = array[1..8] of Integer;
  TEditArray8 = array[1..8] of TEdit;
  TEditArray58 = array[1..5, 1..8] of TEdit;
  {***************}
  { TfArithmetic7 }
  {***************}
  TfArithmetic7 = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsDecimal, mOptionsAuto, MenuItem3: TMenuItem;
    mOptionsOperations, mOptionsOperationsPlus, mOptionsOperationsMinus: TMenuItem;
    mOptionsMax, mOptionsMax4, mOptionsMax5, mOptionsMax6, mOptionsMax7: TMenuItem;
    mOptionsOperandsPlus, mOptionsOperandsPlus3, mOptionsOperandsPlus4, mOptionsOperandsPlus5: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laQuestion: TLabel;
    edQuestion: TEdit;
    ed11, ed12, ed13, ed14, ed15, ed16, ed17, ed18: TEdit;
    ed21, ed22, ed23, ed24, ed25, ed26, ed27, ed28: TEdit;
    ed31, ed32, ed33, ed34, ed35, ed36, ed37, ed38: TEdit;
    ed41, ed42, ed43, ed44, ed45, ed46, ed47, ed48: TEdit;
    ed51, ed52, ed53, ed54, ed55, ed56, ed57, ed58: TEdit;
    edOperator: TEdit;
    edR1, edR2, edR3, edR4, edR5, edR6, edR7, edR8: TEdit;
    edC1, edC2, edC3, edC4, edC5, edC6, edC7, edC8: TEdit;
    edB1, edB2, edB3, edB4, edB5, edB6, edB7, edB8: TEdit;
    shLine: TShape;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    imSuccess: TImage;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsOperationsPlusClick(Sender: TObject);
    procedure mOptionsOperationsMinusClick(Sender: TObject);
    procedure mOptionsMax4Click(Sender: TObject);
    procedure mOptionsMax5Click(Sender: TObject);
    procedure mOptionsMax6Click(Sender: TObject);
    procedure mOptionsMax7Click(Sender: TObject);
    procedure mOptionsOperandsPlus3Click(Sender: TObject);
    procedure mOptionsOperandsPlus4Click(Sender: TObject);
    procedure mOptionsOperandsPlus5Click(Sender: TObject);
    procedure mOptionsDecimalClick(Sender: TObject);
    procedure mOptionsAutoClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure edB1EditingDone(Sender: TObject);
    procedure edB2EditingDone(Sender: TObject);
    procedure edB3EditingDone(Sender: TObject);
    procedure edB4EditingDone(Sender: TObject);
    procedure edB5EditingDone(Sender: TObject);
    procedure edB6EditingDone(Sender: TObject);
    procedure edB7EditingDone(Sender: TObject);
  private
    iRows, iCols, iSQuestions, iQuestions, iMax, iOperands, iOperandsPlus, iOperandsMinus, iQuestion, iCorrect: Integer;
    rResult: Real;
    cOperator: Char;
    bDecimal, bAuto: Boolean;
    aOldBorrow: TArray8;
    aOperators: TOperators;
    rOperands: TOperands;
    edColumns: TEditArray58;
    edResult, edCarry, edBorrow: TEditArray8;
  end;

var
  fArithmetic7: TfArithmetic7;

implementation

{$R *.lfm}

{ Clear the column grid hiding all not used fields }

procedure ClearColumns(RowMax, ColumnMax: Integer; Auto: Boolean; Oprator: Char; var Columns: TEditArray58; var Result, Carry, Borrow: TEditArray8);

var
  I, J: Integer;

begin
  for J := 1 to 5 do begin
    for I := 1 to 8 do begin
      if (J <= RowMax) and (I <= ColumnMax) then begin
        // Fields that are used (are shown)
        Columns[J, I].Visible := True;
        if Auto then begin
          // If auto fil-in is selected, make fields read-only
          Columns[J, I].ReadOnly := True; Columns[J, I].TabStop := False;
        end
        else begin
          // If auto fil-in is not selected, make fields accessible to user input
          Columns[J, I].ReadOnly := False; Columns[J, I].TabStop := True;
        end;
        Columns[J, I].Text := '';
      end
      else begin
        // Fields that are not used (are hidden)
        Columns[J, I].Visible := False;
      end;
      if J = 1 then
        // Reset font style (maybe there was a borrow done)
        Columns[J, I].Font.Style := [];
    end;
  end;
  for I := 1 to 8 do begin
    if I <= ColumnMax then begin
      // Columns that are used: Show result and carry resp. borrow fields
      Result[I].Visible := True; Carry[I].Visible := True; Borrow[I].Visible := True;
      if Oprator = '+' then begin
        // Show carry fields for addition
        Carry[I].Enabled := True;
        Borrow[I].Enabled := False;
      end
      else begin
        // Show borrow fields for subtraction
        Carry[I].Enabled := False;
        Borrow[I].Enabled := True;
      end;
      // Adapt the position of result and carry fields (depending of number of rows = number of operands)
      Result[I].Top := 435 - (5 - RowMax) * 40;
      Carry[I].Top := 385 - (5 - RowMax) * 40;
      Result[I].Text := ''; Carry[I].Text := ''; Borrow[I].Text := '';
    end
    else begin
      // Columns that are not used: Hide result and carry resp. borrow fields
      Result[I].Visible := False; Carry[I].Visible := False; Borrow[I].Visible := False;
    end;
  end;
  // Hide carry field for last column (never used)
  Carry[ColumnMax].Visible := False;
  // Adapt position and length of the line
  fArithmetic7.shLine.Top:= 420 - (5 - RowMax) * 40;
  fArithmetic7.shLine.Width := 313 - (8 - ColumnMax) * 35;
  // Adapt position of operator field
  fArithmetic7.edOperator.Left := 305 - (8 - ColumnMax) * 35;
  fArithmetic7.edOperator.Text := '';
  // Re-enable pushbutton
  fArithmetic7.btQuestion.Enabled := True; fArithmetic7.btQuestion.Caption := 'Question';
end;

{ Get operators to be used in exercises (depending on user selection) }

function GetOperators(Plus, Minus: Boolean): TOperators;

var
  Operators: TOperators;

begin
  if Plus and Minus then begin
    // Both additions and subtractions
    SetLength(Operators, 2);
    Operators[0] := '+'; Operators[1] := '-';
  end
  else begin
    SetLength(Operators, 1);
    if Plus then
      // Only additions
      Operators[0] := '+'
    else
      // Only subtractions
      Operators[0] := '-';
  end;
  Result := Operators;
end;

{ Srike out value in column from which user borrowed }

procedure DoBorrow(Borrow:TEditArray8; Col:Integer; var OldBorrow: TArray8; var Columns: TEditArray58);

begin
  if Borrow[Col].Text = '' then begin
    // Borrow value has been cleared
    Columns[1, Col].Font.Style := [];
    OldBorrow[Col] := 0;
  end
  else begin
    if ((OldBorrow[Col] = 0) and (StrToInt(Borrow[Col].Text) < 10)) or ((OldBorrow[Col] <> 0) and (OldBorrow[Col] - StrToInt(Borrow[Col].Text) = 1)) then begin
      // A borrow has been made from this column
      Columns[1, Col].Font.Style := [fsStrikeOut];
      OldBorrow[Col] := StrToInt(Borrow[Col].Text);
    end;
  end;
end;

{***************}
{ TfArithmetic7 }
{***************}

{ Application start: Initialisation }

procedure TfArithmetic7.FormCreate(Sender: TObject);

begin
  // Create 2-dimensional array with column edit fields
  edColumns[1, 1] := ed11; edColumns[1, 2] := ed12; edColumns[1, 3] := ed13; edColumns[1, 4] := ed14;
  edColumns[1, 5] := ed15; edColumns[1, 6] := ed16; edColumns[1, 7] := ed17; edColumns[1, 8] := ed18;
  edColumns[2, 1] := ed21; edColumns[2, 2] := ed22; edColumns[2, 3] := ed23; edColumns[2, 4] := ed24;
  edColumns[2, 5] := ed25; edColumns[2, 6] := ed26; edColumns[2, 7] := ed27; edColumns[2, 8] := ed28;
  edColumns[3, 1] := ed31; edColumns[3, 2] := ed32; edColumns[3, 3] := ed33; edColumns[3, 4] := ed34;
  edColumns[3, 5] := ed35; edColumns[3, 6] := ed36; edColumns[3, 7] := ed37; edColumns[3, 8] := ed38;
  edColumns[4, 1] := ed41; edColumns[4, 2] := ed42; edColumns[4, 3] := ed43; edColumns[4, 4] := ed44;
  edColumns[4, 5] := ed45; edColumns[4, 6] := ed46; edColumns[4, 7] := ed47; edColumns[4, 8] := ed48;
  edColumns[5, 1] := ed51; edColumns[5, 2] := ed52; edColumns[5, 3] := ed53; edColumns[5, 4] := ed54;
  edColumns[5, 5] := ed55; edColumns[5, 6] := ed56; edColumns[5, 7] := ed57; edColumns[5, 8] := ed58;
  // Create arrays with result, carry and borrow edit fields
  edResult[1] := edR1; edResult[2] := edR2; edResult[3] := edR3; edResult[4] := edR4;
  edResult[5] := edR5; edResult[6] := edR6; edResult[7] := edR7; edResult[8] := edR8;
  edCarry[1] := edC1; edCarry[2] := edC2; edCarry[3] := edC3; edCarry[4] := edC4;
  edCarry[5] := edC5; edCarry[6] := edC6; edCarry[7] := edC7; edCarry[8] := edC8;
  edBorrow[1] := edB1; edBorrow[2] := edB2; edBorrow[3] := edB3; edBorrow[4] := edB4;
  edBorrow[5] := edB5; edBorrow[6] := edB6; edBorrow[7] := edB7; edBorrow[8] := edB8;
  // Set application start-up values
  iSQuestions := 10;
  SetLength(aOperators, 1); aOperators[0] := '+';
  iMax := 1000; cOperator := '+';
  iOperandsPlus := 3; iOperandsMinus := 2; iOperands := 3;
  bDecimal := False; bAuto := False;
  iRows := iOperands; iCols := Length(IntToStr(iMax));
  // Start random number generator
  Randomize;
  // Start a new test (by simulating selection of menu item "Test > New")
  mTestNew.Click;
end;

{ Menu item "Test > New": Start a new test }

procedure TfArithmetic7.mTestNewClick(Sender: TObject);

begin
  laQuestion.Caption := 'Question'; edQuestion.Text := '';
  // Clear column grid
  ClearColumns(iRows, iCols, bAuto, cOperator, edColumns, edResult, edCarry, edBorrow);
  // Clear evaluation fields and correct/false picture
  edQuestions.Text := ''; edCorrect.Text := ''; edFalse.Text := '';
  edSuccess.Text := ''; edSuccess.Color := clDefault;
  imSuccess.Picture.Clear;
  // Reset evaluation counters
  iQuestions := iSQuestions; iQuestion := 0; iCorrect := 0;
end;

{ Menu item "Test > Exit": Exit application }

procedure TfArithmetic7.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions...": Ask user for number of test questions }

procedure TfArithmetic7.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Algebra test', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                              // Minimum number of questions arbitrarily set to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);
  end;
end;

{ Menu items "Options > Math. operation > ...": Select mathematical operation }

procedure TfArithmetic7.mOptionsOperationsPlusClick(Sender: TObject);

begin
  if mOptionsOperationsPlus.Checked then
    mOptionsOperationsPlus.Checked := False
  else
    mOptionsOperationsPlus.Checked := True;
  aOperators := GetOperators(mOptionsOperationsPlus.Checked, mOptionsOperationsMinus.Checked);
end;

procedure TfArithmetic7.mOptionsOperationsMinusClick(Sender: TObject);

begin
  if mOptionsOperationsMinus.Checked then
    mOptionsOperationsMinus.Checked := False
  else
    mOptionsOperationsMinus.Checked := True;
  aOperators := GetOperators(mOptionsOperationsPlus.Checked, mOptionsOperationsMinus.Checked);
end;

{ Menu items "Options > Number maximum > ...": Select maximum value of operands and result }

procedure TfArithmetic7.mOptionsMax4Click(Sender: TObject);

begin
  mOptionsMax4.Checked := True;
  mOptionsMax5.Checked := False;
  mOptionsMax6.Checked := False;
  mOptionsMax7.Checked := False;
  iMax := 1000;
end;

procedure TfArithmetic7.mOptionsMax5Click(Sender: TObject);

begin
  mOptionsMax4.Checked := False;
  mOptionsMax5.Checked := True;
  mOptionsMax6.Checked := False;
  mOptionsMax7.Checked := False;
  iMax := 10000;
end;

procedure TfArithmetic7.mOptionsMax6Click(Sender: TObject);

begin
  mOptionsMax4.Checked := False;
  mOptionsMax5.Checked := False;
  mOptionsMax6.Checked := True;
  mOptionsMax7.Checked := False;
  iMax := 100000;
end;

procedure TfArithmetic7.mOptionsMax7Click(Sender: TObject);

begin
  mOptionsMax4.Checked := False;
  mOptionsMax5.Checked := False;
  mOptionsMax6.Checked := False;
  mOptionsMax7.Checked := True;
  iMax := 1000000;
end;

{ Menu items "Options > Addition operands > ...": Select number of operands for additions }

procedure TfArithmetic7.mOptionsOperandsPlus3Click(Sender: TObject);

begin
  mOptionsOperandsPlus3.Checked := True;
  mOptionsOperandsPlus4.Checked := False;
  mOptionsOperandsPlus5.Checked := False;
  iOperandsPlus := 3;
end;

procedure TfArithmetic7.mOptionsOperandsPlus4Click(Sender: TObject);

begin
  mOptionsOperandsPlus3.Checked := False;
  mOptionsOperandsPlus4.Checked := True;
  mOptionsOperandsPlus5.Checked := False;
  iOperandsPlus := 4;
end;

procedure TfArithmetic7.mOptionsOperandsPlus5Click(Sender: TObject);

begin
  mOptionsOperandsPlus3.Checked := False;
  mOptionsOperandsPlus4.Checked := False;
  mOptionsOperandsPlus5.Checked := True;
  iOperandsPlus := 5;
end;

{ Menu item "Options > Decimal numbers": Toggle if decimal numbers have to be used or not }

procedure TfArithmetic7.mOptionsDecimalClick(Sender: TObject);

begin
  if mOptionsDecimal.Checked then
    mOptionsDecimal.Checked := False
  else
    mOptionsDecimal.Checked := True;
  bDecimal := mOptionsDecimal.Checked;
end;

{ Menu item "Options > Auto column fill-in": Toggle if columns have to be filled-in automatically or not }

procedure TfArithmetic7.mOptionsAutoClick(Sender: TObject);

begin
  if mOptionsAuto.Checked then
    mOptionsAuto.Checked := False
  else
    mOptionsAuto.Checked := True;
  bAuto := mOptionsAuto.Checked;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfArithmetic7.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about text }

procedure TfArithmetic7.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Algebra: Column addition and subtraction.' + LineEnding;
  S += 'Primary school maths exercise genertaor.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2019.';
  MessageDlg('About "Arithmetic7"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new exercise question resp. check user answer }

procedure TfArithmetic7.btQuestionClick(Sender: TObject);

var
  Len, LenMax, Dec, DecMax, Success, I, J, K: Integer;
  Answer: Real;
  Operand, SAnswer: string;
  OK: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    if mOptionsOperationsPlus.Checked or mOptionsOperationsMinus.Checked then begin
      // Reset borrow values
      for I := 1 to 8 do
        aOldBorrow[I] := 0;
      Inc(iQuestion);
      laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
      // Random operator (among those selected)
      cOperator := aOperators[Random(Length(aOperators))];
      // Get number of operands for this exercise (depending on user selections)
      if cOperator = '+' then
        iOperands := iOperandsPlus
      else
        iOperands := iOperandsMinus;
      SetLength(rOperands, iOperands);
      // Generate the operands (do until they are "valid")
      repeat
        OK := True; LenMax := 0; DecMax := 0;
        for I := 0 to iOperands - 1 do begin
          rOperands[I] := Random(iMax + 1);
          if bDecimal then begin
            // If "Decimal numbers" is selected, randomly use integer or decimal value
            if Random(2) = 1 then begin
              Dec := Random(3) + 1;
              if (Dec > DecMax) and (Round(rOperands[I]) mod Round(intpower(10, Dec)) <> 0) then
                // Store the maximum number of decimal digits used (this will be used for automatic operands line-up)
                DecMax := Dec;
              rOperands[I] /= intpower(10, Dec);                                         // number with 1, 2 or 3 decimal digits
            end;
          end;
        end;
        if cOperator = '+' then
          rResult := 0
        else
          rResult := rOperands[0];
        // Check if numbers fit into 8 columns and calculate the addition/subtraction result
        for I := 0 to iOperands - 1 do begin
          Len := Length(FloatToStrF(rOperands[I], ffFixed, 0, DecMax));
          if Len > 8 then
            // Invalid number: To long to fit in 8 columns
            OK := False
          else begin
            // Addition: Add operand to result
            if cOperator = '+' then
              rResult += rOperands[I]
            else begin
              // Subtraction
              if I > 0 then
                // Subtract operand (from result set to first operand)
                rResult -= rOperands[I];
            end;
            if Len > LenMax then
              // Store maximum operand length (this will have effect on number of columns)
              LenMax := Len;
          end;
        end;
        if (rResult <= 0) or (rResult > iMax) then
          // Invalid numbers: Result should be between 1 and maximum selected
          OK := False
        else if Length(FloatToStrF(rResult, ffFixed, 0, DecMax)) > 8 then
          // Invalid numbers: Result to long to fit in 8 columns
          OK := False;
      until OK;
      // Correct maximum number length (if result length > max operand length)
      if Length(FloatToStrF(rResult, ffFixed, 0, DecMax)) > LenMax then
        LenMax := Length(FloatToStrF(rResult, ffFixed, 0, DecMax));
      // Display the question
      edQuestion.Text := FloatToStr(rOperands[0]);
      for I := 1 to iOperands - 1 do
        edQuestion.Text := edQuestion.Text + ' ' + cOperator + ' ' + FloatToStr(rOperands[I]);
      // Determine number of grid rows and columns
      iRows := iOperands; iCols := LenMax;
      // Clear the column grid
      ClearColumns(iRows, iCols, bAuto, cOperator, edColumns, edResult, edCarry, edBorrow);
      // Display the operator
      edOperator.Text := cOperator;
      // if "Auto column fill-in" is selected, do so (with proper line-up!)
      if bAuto then begin
        for I := 1 to iOperands do begin
          Operand := FloatToStrF(rOperands[I - 1], ffFixed, 0, DecMax);
          // Leave blank left fields corresponding to unused digits
          K := 1;
          for J := 1 to LenMax - Length(Operand) do begin
            edColumns[I, J].Text := '';
            Inc(K);
          end;
          // Fill in number, starting from correct line-up position (i.e. after blank fields)
          for J := K to LenMax do begin
            edColumns[I, J].Text := Operand[J - K + 1];
          end;
        end;
      end;
      // Clear correct/false picture
      imSuccess.Picture.Clear;
      // Set focus
      if bAuto then
        edResult[iCols].SetFocus
      else
        edColumns[1, 1].SetFocus;
      // Next button push will be for user answer
      btQuestion.Caption := 'Answer';
    end
    else
      MessageDlg('Algebra test', 'Error: No math. operation selected!', mtError, [mbOK], 0);
  end
  // Button "Answer": Check user answer and update evaluation values
  else begin
    SAnswer := '';
    // Get user answer from result column fields
    for I := 1 to iCols do begin
      if edResult[I].Text = '' then
        SAnswer += '0'
      else
        SAnswer += edResult[I].Text
    end;
    Answer := StrToFloat(SAnswer);
    // Evaluation
    if Answer = rResult then begin
      // Correct answer
      Inc(iCorrect);
      imSuccess.Picture.LoadFromFile('correct.png');
    end
    else begin
      // False answer
      imSuccess.Picture.LoadFromFile('false.png');
    end;
    // Fill in evaluation values
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iQuestion - iCorrect);;
    Success := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Success) + '%';
    // Color success field depending on success percentage
    if Success >= 60 then
      edSuccess.Color := clLime
    else if Success >= 50 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clRed;
    // If all questions have been done, end the test
    if iQuestion = iQuestions then begin
      MessageDlg('Algebra test', 'All questions have been done. End of test.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end
    // If there are questions left, set button for next question generation
    else begin
      btQuestion.Caption := 'Question';
      btQuestion.Focused;
    end;
  end;
end;

{ Changes in borrow fields done: If it is a borrow: Automatically strike out corr. column value }

procedure TfArithmetic7.edB1EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 1, aOldBorrow, edColumns);
end;

procedure TfArithmetic7.edB2EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 2, aOldBorrow, edColumns);
end;

procedure TfArithmetic7.edB3EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 3, aOldBorrow, edColumns);
end;

procedure TfArithmetic7.edB4EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 4, aOldBorrow, edColumns);
end;

procedure TfArithmetic7.edB5EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 5, aOldBorrow, edColumns);
end;

procedure TfArithmetic7.edB6EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 6, aOldBorrow, edColumns);
end;

procedure TfArithmetic7.edB7EditingDone(Sender: TObject);

begin
  DoBorrow(edBorrow, 7, aOldBorrow, edColumns);
end;

end.

