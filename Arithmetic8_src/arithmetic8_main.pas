{*****************************************}
{* Main unit for Arithmetic8 application *}
{*****************************************}

unit arithmetic8_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Math, arithmetic8_help;

type
  TArray8 = array[1..8] of Integer;
  TEditArray8 = array[1..8] of TEdit;
  TEditArray68 = array[1..6, 1..8] of TEdit;
  {***************}
  { TfArithmetic8 }
  {***************}
  TfArithmetic8 = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExample, mTestExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsDecimal: TMenuItem;
    mOptionsMultiplier, mOptionsMultiplier99, mOptionsMultiplier999, mOptionsMultiplier9999: TMenuItem;
    mOptionsMultiplicand, mOptionsMultiplicand99, mOptionsMultiplicand999, mOptionsMultiplicand9999: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    shLine1: TShape;
    StaticText1: TStaticText;
    laQuestion: TLabel;
    edQuestion: TEdit;
    ed11, ed12, ed13, ed14, ed15, ed16, ed17, ed18: TEdit;
    ed21, ed22, ed23, ed24, ed25, ed26, ed27, ed28: TEdit;
    ed31, ed32, ed33, ed34, ed35, ed36, ed37, ed38: TEdit;
    ed41, ed42, ed43, ed44, ed45, ed46, ed47, ed48: TEdit;
    ed51, ed52, ed53, ed54, ed55, ed56, ed57, ed58: TEdit;
    ed61, ed62, ed63, ed64, ed65, ed66, ed67, ed68: TEdit;
    edOperator: TEdit;
    edR1, edR2, edR3, edR4, edR5, edR6, edR7, edR8: TEdit;
    edC1, edC2, edC3, edC4, edC5, edC6, edC7, edC8: TEdit;
    shLine: TShape;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    imSuccess: TImage;
    btQuestion: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExampleClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsMultiplicand99Click(Sender: TObject);
    procedure mOptionsMultiplicand999Click(Sender: TObject);
    procedure mOptionsMultiplicand9999Click(Sender: TObject);
    procedure mOptionsMultiplier99Click(Sender: TObject);
    procedure mOptionsMultiplier999Click(Sender: TObject);
    procedure mOptionsMultiplier9999Click(Sender: TObject);
    procedure mOptionsDecimalClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    iMultiplicandMax, iMultiplierMax, iSQuestions, iQuestions, iQuestion, iCorrect: Integer;
    rMultiplicand, rMultiplier, rResult: Real;
    bDecimal: Boolean;
    edColumns: TEditArray68;
    edResult, edCarry: TEditArray8;
  end;

var
  fArithmetic8: TfArithmetic8;

implementation

{$R *.lfm}

{ Get local decimal separator }

function DecimalSeparator: Char;

begin
  Result := Copy(FloatToStr(1.2), 2, 1)[1];
end;

{ Clear the column grid }

procedure ClearColumns(var Columns: TEditArray68; var Result, Carry: TEditArray8);

var
  I, J: Integer;

begin
  for J := 1 to 6 do begin
    for I := 1 to 8 do begin
      Columns[J, I].Text := '';
    end;
  end;
  for I := 1 to 8 do begin
    Result[I].Text := ''; Carry[I].Text := '';
  end;
  // Re-enable pushbutton
  fArithmetic8.btQuestion.Enabled := True;
  fArithmetic8.btQuestion.Caption := 'Question';
end;

{***************}
{ TfArithmetic8 }
{***************}

{ Application start: Initialisation }

procedure TfArithmetic8.FormCreate(Sender: TObject);

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
  edColumns[6, 1] := ed61; edColumns[6, 2] := ed62; edColumns[6, 3] := ed63; edColumns[6, 4] := ed64;
  edColumns[6, 5] := ed65; edColumns[6, 6] := ed66; edColumns[6, 7] := ed67; edColumns[6, 8] := ed68;
  // Create arrays with result and carry edit fields
  edResult[1] := edR1; edResult[2] := edR2; edResult[3] := edR3; edResult[4] := edR4;
  edResult[5] := edR5; edResult[6] := edR6; edResult[7] := edR7; edResult[8] := edR8;
  edCarry[1]  := edC1; edCarry[2]  := edC2; edCarry[3]  := edC3; edCarry[4]  := edC4;
  edCarry[5]  := edC5; edCarry[6]  := edC6; edCarry[7]  := edC7; edCarry[8]  := edC8;
  // Set application start-up values
  iSQuestions := 10;
  iMultiplicandMax := 9999; iMultiplierMax := 99;
  bDecimal := False;
  // Start random number generator
  Randomize;
  // Start a new test (by simulating selection of menu item "Test > New")
  mTestNew.Click;
end;

{ Menu item "Test > New": Start a new test }

procedure TfArithmetic8.mTestNewClick(Sender: TObject);

begin
  laQuestion.Caption := 'Question'; edQuestion.Text := '';
  // Clear column grid
  ClearColumns(edColumns, edResult, edCarry);
  // Clear evaluation fields and correct/false picture
  edQuestions.Text := ''; edCorrect.Text := ''; edFalse.Text := '';
  edSuccess.Text := ''; edSuccess.Color := clDefault;
  imSuccess.Picture.Clear;
  // Reset evaluation counters
  iQuestions := iSQuestions; iQuestion := 0; iCorrect := 0;
end;

{ Menu item "Test > Example": Display table multiplication example }

procedure TfArithmetic8.mTestExampleClick(Sender: TObject);

const
  Multiplicand = 123.45; Multiplier = 6.77;
  Rows: array[1..7] of string = (
    '  123.45', '    66.7', '   86415', '  74070 ', ' 74070  ', '        ', '8234.115'
  );

var
  I, J: Integer;
  Row: string;

begin
  ClearColumns(edColumns, edResult, edCarry);
  edQuestion.Text := FloatToStr(Multiplicand) + ' x ' + FloatToStr(Multiplier) + ' = ? ';
  for I := 1 to 6 do begin
    Row := StringReplace(Rows[I], '.', DecimalSeparator, []);
    for J := 1 to 8 do
      edColumns[I, J].Text := Row[J];
  end;
  Row := StringReplace(Rows[7], '.', DecimalSeparator, []);
  for J := 1 to 8 do
    edResult[J].Text := Row[J];
end;

{ Menu item "Test > Exit": Exit application }

procedure TfArithmetic8.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions...": Ask user for number of test questions }

procedure TfArithmetic8.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Arithmetic test', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then begin
    if StrToInt(S) < 5 then                                                              // Minimum number of questions arbitrarily set to 5
      iSQuestions := 5
    else
      iSQuestions := StrToInt(S);
  end;
end;

{ Menu items "Options > Multiplicand maximum > ...": Ask user for maximum number to be used as multiplicand (first operand) }

procedure TfArithmetic8.mOptionsMultiplicand99Click(Sender: TObject);

begin
  mOptionsMultiplicand99.Checked := True; mOptionsMultiplicand999.Checked := False; mOptionsMultiplicand9999.Checked := False;
  iMultiplicandMax := 99;
end;

procedure TfArithmetic8.mOptionsMultiplicand999Click(Sender: TObject);

begin
  mOptionsMultiplicand99.Checked := False; mOptionsMultiplicand999.Checked := True; mOptionsMultiplicand9999.Checked := False;
  iMultiplicandMax := 999;
end;

procedure TfArithmetic8.mOptionsMultiplicand9999Click(Sender: TObject);

begin
  mOptionsMultiplicand99.Checked := False; mOptionsMultiplicand999.Checked := False; mOptionsMultiplicand9999.Checked := True;
  iMultiplicandMax := 9999;
end;

{ Menu items "Options > Multiplier maximum > ...": Ask user for maximum number to be used as multiplier (second operand) }

procedure TfArithmetic8.mOptionsMultiplier99Click(Sender: TObject);

begin
  mOptionsMultiplier99.Checked := True; mOptionsMultiplier999.Checked := False; mOptionsMultiplier9999.Checked := False;
  iMultiplierMax := 99;
end;

procedure TfArithmetic8.mOptionsMultiplier999Click(Sender: TObject);

begin
  mOptionsMultiplier99.Checked := False; mOptionsMultiplier999.Checked := True; mOptionsMultiplier9999.Checked := False;
  iMultiplierMax := 999;
end;

procedure TfArithmetic8.mOptionsMultiplier9999Click(Sender: TObject);

begin
  mOptionsMultiplier99.Checked := False; mOptionsMultiplier999.Checked := False; mOptionsMultiplier9999.Checked := True;
  iMultiplierMax := 9999;
end;

{ Menu item "Options > Decimal numbers": Toggle if decimal numbers may be used or not }

procedure TfArithmetic8.mOptionsDecimalClick(Sender: TObject);

begin
  if mOptionsDecimal.Checked then
    mOptionsDecimal.Checked := False
  else
    mOptionsDecimal.Checked := True;
  bDecimal := mOptionsDecimal.Checked;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfArithmetic8.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about text }

procedure TfArithmetic8.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Arithmetic: Column multiplication.' + LineEnding;
  S += 'Primary school maths exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, December 2020 - January 2021.';
  MessageDlg('About "Arithmetic8"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new exercise question resp. check user answer }

procedure TfArithmetic8.btQuestionClick(Sender: TObject);

var
  R, Dec1, Dec2, LDecSep, Success, I, J: Integer;
  Answer: string;
  OK, ResultStart: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    // Generate random operands (do until they are "valid")
    R := Random(5);
    repeat
      OK := True;
      rMultiplicand := Random(iMultiplicandMax + 1);
      rMultiplier   := Random(iMultiplierMax + 1);
      Dec1 := 0; Dec2 := 0; LDecSep := 0;
      if bDecimal then begin
        // If "Decimal numbers" is selected, randomly use integer or decimal value
        if R >= 2 then begin
          Dec1 := Random(3) + 1; Dec2 := Random(3) + 1; LDecSep := 1;
          case R of
            3: rMultiplicand /= intpower(10, Dec1);
            2: rMultiplier   /= intpower(10, Dec2);
            4: begin
               rMultiplicand /= intpower(10, Dec1); rMultiplier   /= intpower(10, Dec2);
            end;
          end;
        end;
      end;
      if (RightStr(FloatToStr(rMultiplicand), 1) = '0') or (RightStr(FloatToStr(rMultiplier), 1) = '0') then
        // Do not use operands ending in '0'
        OK := False
      else begin
        rResult := rMultiplicand * rMultiplier;
        if Length(FloatToStr(Int(rResult))) + Dec1 + Dec2 + LDecSep > 8 then
          // Be sure, that the result fit in the 8 columns
          OK := False
        else if RightStr(FloatToStr(rResult), 1) = '0' then
          // Do not use operands that give a result ending in '0'
          OK := False;
      end;
    until OK;
    // Clear the column grid
    ClearColumns(edColumns, edResult, edCarry);
    // Display the question
    edQuestion.Text := FloatToStr(rMultiplicand) + ' x ' + FloatToStr(rMultiplier) + ' = ? ';
    J := 8 - Length(FloatToStr(rMultiplicand));
    for I := Length(FloatToStr(rMultiplicand)) downto 1 do
      edColumns[1, I + J].Text := FloatToStr(rMultiplicand)[I];
    J := 8 - Length(FloatToStr(rMultiplier));
    for I := Length(FloatToStr(rMultiplier)) downto 1 do
      edColumns[2, I + J].Text := FloatToStr(rMultiplier)[I];
    // Clear correct/false picture
    imSuccess.Picture.Clear;
    // Set edit field focus
    edColumns[3, 8].SetFocus;
    // Next button push will be for user answer
    btQuestion.Caption := 'Answer';
  end
  // Button "Answer": Check user answer and update evaluation values
  else begin
    Answer := '';
    // Get user answer from result column fields
    ResultStart := False;
    for I := 1 to 8 do begin
      if (edResult[I].Text <> '') and (edResult[I].Text[1] in ['0' .. '9']) then
        ResultStart := True;
      if ResultStart then begin
        if edResult[I].Text = '' then
          Answer += 'X'
        else
          Answer += edResult[I].Text;
      end;
    end;
    // Evaluation
    if Answer = FloatToStr(rResult) then begin
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
      btQuestion.SetFocus;
    end;
  end;
end;

{ Button "Clear": Clear all carry edit fields }

procedure TfArithmetic8.btClearClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 8 do
    edCarry[I].Text := '';
end;

end.

