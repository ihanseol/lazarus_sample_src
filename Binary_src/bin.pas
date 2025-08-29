{************************************}
{* Main unit for Binary application *}
{************************************}

unit bin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids;

type
  TBits = 0 .. 1;
  TBinary = array of Boolean;
  TNBinary = array of TBits;
  {**********}
  { TfBinary }
  {**********}
  TfBinary = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestConversion, mTestArithmetic, mTestLogic, mTestExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsSize, mSettingsSize8, mSettingsSize16, mSettingsSize32: TMenuItem;
    mSettingsDefinition, mSettingsDefinitionSigned, mSettingsDefinitionUnsigned: TMenuItem;
    mSettingsSeparation, mSettingsSeparation4, mSettingsSeparation8, mSettingsSeparation0: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laQuestion, laQuestionText, laQuestion1, laQuestion2, laQuestion3: TLabel;
    edQuestion1, edQuestion2, edQuestion3, edEval: TEdit;
    Label1: TLabel;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestConversionClick(Sender: TObject);
    procedure mTestArithmeticClick(Sender: TObject);
    procedure mTestLogicClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsSize8Click(Sender: TObject);
    procedure mSettingsSize16Click(Sender: TObject);
    procedure mSettingsSize32Click(Sender: TObject);
    procedure mSettingsDefinitionSignedClick(Sender: TObject);
    procedure mSettingsDefinitionUnsignedClick(Sender: TObject);
    procedure mSettingsSeparation4Click(Sender: TObject);
    procedure mSettingsSeparation8Click(Sender: TObject);
    procedure mSettingsSeparation0Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iTest, iTest1, iSize, iBitsSep, iQuestion, iCorrect: Integer;
    sAnswer1, sAnswer2, sAnswer3, sBinAnswer: string;
    bNegative: Boolean;
  end;

var
  fBinary: TfBinary;

implementation

{$R *.lfm}

procedure AddBinary(NBin1, NBin2: TNBinary; out NBin: TNBinary; out Carry: TBits) forward;

{ Format integer value for grid display (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN += S;                                                                             // % sign
  Result := SN;
end;

{ Prepare for a new test }

procedure NewTest(Test: Integer; out Question, Correct: Integer);

const
  Titles: array[1..3] of string = (
    'Binary numbers conversion', 'Binary numbers arithmetic operations', 'Binary numbers logical operations'
  );
  QuestionText: array[1..3] of string = (
    'Convert a binary number', 'Add two binary numbers', 'Do and AND operation of two binary numbers'
  );

var
  I: Integer;

begin
  fBinary.stTitle.Caption := Titles[Test] + '.';
  fBinary.laQuestion.Caption := 'Question';
  fBinary.laQuestionText.Caption := QuestionText[Test] + '.';
  if Test = 1 then begin
    fBinary.laQuestion1.Caption := 'Binary';
    fBinary.laQuestion2.Caption := 'Decimal';
    fBinary.laQuestion3.Caption := 'Hexadecimal';
  end
  else begin
    fBinary.laQuestion1.Caption := 'Number 1';
    fBinary.laQuestion2.Caption := 'Number 2';
    fBinary.laQuestion3.Caption := 'Result';
  end;
  fBinary.edQuestion1.Text := ''; fBinary.edQuestion2.Text := ''; fBinary.edQuestion3.Text := '';
  fBinary.edQuestion1.Color := clDefault; fBinary.edQuestion2.Color := clDefault; fBinary.edQuestion3.Color := clDefault;
  fBinary.edEval.Text := ''; fBinary.edEval.Color := clDefault;
  for I := 0 to 3 do
    fBinary.sgEval.Cells[1, I] := '';
  Question := 0; Correct := 0;
  fBinary.btQuestion.Caption := 'Question';
end;

{ Generate a random binary number (of given size) }

function RandomBinary(BSize: Integer): TBinary;

var
  I: Integer;
  Bin: TBinary;

begin
  SetLength(Bin, BSize);
  for I := 0 to BSize - 1 do begin
    if Random(2) = 0 then
      Bin[I] := False
    else
      Bin[I] := True;
  end;
  Result := Bin;
end;

{ Convert a "true binary" array (true, false elements) to a "number binary" (1, 0 values) }

function NumberBinary(Bin: TBinary): TNBinary;

var
  I: Integer;
  NBin: TNBinary;

begin
  SetLength(NBin, Length(Bin));
  for I := 0 to Length(Bin) - 1 do begin
    if Bin[I] then
      NBin[I] := 1
    else
      NBin[I] := 0;
  end;
  Result := NBin;
end;

{ Convert a "numbered binary" to a string (to be displayed with eventual bit-groups separation) }

function DisplayBinary(NBin: TNBinary; BitsSep: Integer): string;

var
  I: Integer;
  DBin: string;

begin
  DBin := '';
  for I := 0 to Length(NBin) - 1 do begin
    if (BitsSep <> 0) and (I mod BitSSep = 0) and (I <> 0) then
      // Insert space as bit-group separator
      DBin += ' ';
    DBin += IntToStr(NBin[I]);
  end;
  Result := DBin;
end;

{ Caculate 1's complement of a binary number }

function Complement1(NBin: TNBinary): TNBinary;

var
  I: Integer;
  Comp1: TNBinary;

begin
  SetLength(Comp1, Length(NBin));
  for I := 0 to Length(NBin) - 1 do begin
    if NBin[I] = 0 then
      Comp1[I] := 1
    else
      Comp1[I] := 0;
  end;
  Result := Comp1;
end;

{ Caculate 2's complement of a binary number }

function Complement2(NBin: TNBinary): TNBinary;

var
  I: Integer;
  Carry: TBits;
  One, Compl1, Compl2: TNBinary;

begin
  SetLength(One, Length(NBin)); SetLength(Compl2, Length(NBin));
  for I := 0 to Length(One) - 2 do
    One[I] := 0;
  One[Length(One) - 1] := 1;                                                             // binary 1 as TBinary array
  Compl1 := Complement1(NBin);                                                           // form 1's complement
  AddBinary(Compl1, One, Compl2, Carry);                                                 // add a binary 1
  Result := Compl2;
end;

{ Binary to decimal conversion }

function BinaryToDecimal(NBin: TNBinary; Neg: Boolean): LongInt;

var
  First, N, I: Integer;
  Dec: LongInt;
  NBin2: TNBinary;

begin
  NBin2 := NBin; First := 0;
  if Neg then begin
    // Binary is considered to be signed
    if NBin[0] = 1 then begin
      // MSB = 1: Number is negative
      NBin2 := Complement2(NBin2);                                                       // decimal corresponds to (opposite of = negative value of) 2's complement
      First := 1;                                                                        // first bit to be considered as part of the number
    end;
  end;
  N := 1; Dec := 0;
  for I := Length(NBin2) - 1 downto First do begin                                       // MSB considered or not being part of the number
    Dec += NBin2[I] * N;
    N *= 2;
  end;
  if Neg and (NBin[0] = 1) then
    Dec := -Dec;                                                                         // take opposite (add - sign) for negative number
  Result := Dec;
end;

{ Binary to hexadecimal conversion }

function BinaryToHexaDecimal(NBin: TNBinary): string;

const
  HexDigits: array[0..15] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  );

var
  H, N, I, J: Integer;
  Hex: string;

begin
  Hex := '';
  for I := 0 to Length(NBin) div 4 - 1 do begin
    N := 1; H := 0;
    // Calculate decimal value for 4 bits
    for J := 3 downto 0 do begin
      H += NBin[I * 4 + J] * N;
      N *= 2;
    end;
    // Convert decimal value to hexadecimal digit
    Hex += HexDigits[H];
  end;
  Result := Hex;
end;

{ Addition of 2 binary numbers (assumed being same size!) }

procedure AddBinary(NBin1, NBin2: TNBinary; out NBin: TNBinary; out Carry: TBits);

// The procedure returns the binary sum and the (last) carry

var
  Res, I: Integer;

begin
  SetLength(NBin, Length(NBin1));
  Carry := 0;
  for I := Length(NBin1) - 1 downto 0 do begin
    Res := NBin1[I] + NBin2[I] + Carry;
    if Res <= 1 then begin
      NBin[I] := Res;
      Carry := 0;
    end
    else begin
      NBin[I] := Res - 2;
      Carry := 1;
    end;
  end;
end;

{ Subtraction of 2 binary numbers (assumed to be same size!) }

procedure SubtractBinary(NBin1, NBin2: TNBinary; out NBin: TNBinary; out Carry: TBits);

// Subtraction is done by adding the 2's complement of the binary to be subtracted
// The procedure returns the binary difference and the (last) carry

var
  Compl2: TNBinary;

begin
  SetLength(Compl2, Length(NBin1)); SetLength(NBin, Length(NBin1));
  Compl2 := Complement2(NBin2);
  AddBinary(NBin1, Compl2, NBin, Carry);
end;

{ Perform a logical operation on 2 binary numbers (assumed being same size!) }

function LogicalOpBinary(Bin1, Bin2: TBinary; Op: string): TBinary;

var
  I: Integer;
  Bin: TBinary;

begin
  SetLength(Bin, Length(Bin1));
  for I := 0 to Length(Bin1) - 1 do begin
    if Op = 'AND' then
      Bin[I] := Bin1[I] and Bin2[I]
    else if Op = 'NAND' then
      Bin[I] := not (Bin1[I] and Bin2[I])
    else if Op = 'OR' then
      Bin[I] := Bin1[I] or Bin2[I]
    else if Op = 'NOR' then
      Bin[I] := not (Bin1[I] or Bin2[I])
    else if Op = 'XOR' then
      Bin[I] := Bin1[I] xor Bin2[I]
    else
      Bin[I] := not (Bin1[I] xor Bin2[I]);
  end;
  Result := Bin;
end;

{**********}
{ TfBinary }
{**********}

{ Application start: Initialisation }

procedure TfBinary.FormCreate(Sender: TObject);

begin
  iSize := 8; bNegative := False; iBitsSep := 4;
  Randomize;
  mTestConversion.Click;
end;

{ Menu item "Test > Base conversion": Prepare for a new conversion test }

procedure TfBinary.mTestConversionClick(Sender: TObject);

begin
  iTest := 1;
  mSettingsDefinition.Enabled := True;
  NewTest(iTest, iQuestion, iCorrect);
end;

{ Menu item "Test > Arithmetic operations": Prepare for a new arithmetic test }

procedure TfBinary.mTestArithmeticClick(Sender: TObject);

begin
  iTest := 2;
  mSettingsDefinition.Enabled := False;                                                  // always consider numbers as unsigned
  NewTest(iTest, iQuestion, iCorrect);
end;

{ Menu item "Test > Logical operations": Prepare for a new logical operations test }

procedure TfBinary.mTestLogicClick(Sender: TObject);

begin
  iTest := 3;
  mSettingsDefinition.Enabled := False;                                                  // always consider numbers as unsigned
  NewTest(iTest, iQuestion, iCorrect);
end;

{ Menu item "Test > Exit": Exit the application }

procedure TfBinary.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Binary size > ...": Select size (number of bits) of binary numbers }

procedure TfBinary.mSettingsSize8Click(Sender: TObject);

begin
  mSettingsSize8.Checked := True;
  mSettingsSize16.Checked := False;
  mSettingsSize32.Checked := False;
  iSize := 8;
end;

procedure TfBinary.mSettingsSize16Click(Sender: TObject);

begin
  mSettingsSize8.Checked := False;
  mSettingsSize16.Checked := True;
  mSettingsSize32.Checked := False;
  iSize := 16;
end;

procedure TfBinary.mSettingsSize32Click(Sender: TObject);

begin
  mSettingsSize8.Checked := False;
  mSettingsSize16.Checked := False;
  mSettingsSize32.Checked := True;
  iSize := 32;
end;

{ Menu items "Settings > Binary definition > ...": Select if binary numbers should be considered as signed or unsigned }

procedure TfBinary.mSettingsDefinitionSignedClick(Sender: TObject);

begin
  mSettingsDefinitionSigned.Checked := True;
  mSettingsDefinitionUnsigned.Checked := False;
  bNegative := True;
end;

procedure TfBinary.mSettingsDefinitionUnsignedClick(Sender: TObject);

begin
  mSettingsDefinitionSigned.Checked := False;
  mSettingsDefinitionUnsigned.Checked := True;
  bNegative := False;
end;

{ Menu items "Settings > Bit separation > ...": Select how binary numbers should be displayed (separated bit groups or not) }

procedure TfBinary.mSettingsSeparation4Click(Sender: TObject);

begin
  mSettingsSeparation4.Checked := True;
  mSettingsSeparation8.Checked := False;
  mSettingsSeparation0.Checked := False;
  iBitsSep := 4;
end;

procedure TfBinary.mSettingsSeparation8Click(Sender: TObject);

begin
  mSettingsSeparation4.Checked := False;
  mSettingsSeparation8.Checked := True;
  mSettingsSeparation0.Checked := False;
  iBitsSep := 8;
end;

procedure TfBinary.mSettingsSeparation0Click(Sender: TObject);

begin
  mSettingsSeparation4.Checked := False;
  mSettingsSeparation8.Checked := False;
  mSettingsSeparation0.Checked := True;
  iBitsSep := 0;
end;

{ Menu item "Help > About": Display application about }

procedure TfBinary.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics trainer: Binary numbers.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March 2020.';
  MessageDlg('About "Binary"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate exercise question resp. check user answer }

procedure TfBinary.btQuestionClick(Sender: TObject);

const
  LogicalOperators: array[0..5] of string = (
    'AND', 'NAND', 'OR', 'NOR', 'XOR', 'XNOR'
  );

var
  Dec: LongInt;
  UAnswer1, UAnswer2, UAnswer3, Hex: string;
  Carry, CarryChk: TBits;
  Bin1, Bin2, Bin: TBinary;
  NBin1, NBin2, NBin: TNBinary;

begin
  // Button "Question": Generate exercise question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ':';
    edQuestion1.Text := ''; edQuestion2.Text := ''; edQuestion3.Text := '';
    edEval.Text := ''; edEval.Color := clDefault;
    // Conversion test
    if iTest = 1 then begin
      // Take a random binary and calculate decimal and hexadecimal value
      Bin1 := RandomBinary(iSize);
      NBin1 := NumberBinary(Bin1);
      Dec := BinaryToDecimal(NBin1, bNegative);
      Hex := BinaryToHexaDecimal(NBin1);
      edQuestion1.ReadOnly := False; edQuestion1.TabStop := True; edQuestion1.Color := clDefault;
      edQuestion2.ReadOnly := False; edQuestion2.TabStop := True; edQuestion2.Color := clDefault;
      edQuestion3.ReadOnly := False; edQuestion3.TabStop := True; edQuestion3.Color := clDefault;
      // Randomly selection of "given" value
      iTest1 := Random(3);
      if iTest1 = 0 then begin
        // Binary value is given
        edQuestion1.ReadOnly := True; edQuestion1.TabStop := False; edQuestion1.Color := clCream;
        edQuestion1.Text := DisplayBinary(NBin1, iBitsSep);
        edQuestion2.SetFocus;
      end
      else if iTest1 = 1 then begin
        // Decimal value is given
        edQuestion2.ReadOnly := True; edQuestion2.TabStop := False; edQuestion2.Color := clCream;
        edQuestion2.Text := IntToStr(Dec);
        edQuestion1.SetFocus;
      end
      else begin
        // Hexadecimal value is given
        edQuestion3.ReadOnly := True; edQuestion3.TabStop := False; edQuestion3.Color := clCream;
        edQuestion3.Text := Hex;
        edQuestion1.SetFocus;
      end;
      // Save all 3 values (as strings)
      sAnswer1 := DisplayBinary(NBin1, 0);
      sAnswer2 := IntToStr(Dec);
      sAnswer3 := Hex;
      sBinAnswer := DisplayBinary(NBin1, iBitsSep);                                      // binary value with separators (used for display in case of "false answer")
    end
    // Arithmetic or logical test
    else begin
      edQuestion1.ReadOnly := True; edQuestion1.TabStop := False; edQuestion1.Color := clCream;
      edQuestion2.ReadOnly := True; edQuestion2.TabStop := False; edQuestion2.Color := clCream;
      edQuestion3.ReadOnly := False; edQuestion3.TabStop := True; edQuestion3.Color := clDefault;
      // Arithmetic test
      if iTest = 2 then begin
        laQuestionText.Caption := 'Add two binary numbers.';
        // Randomly select if addition or subtraction
        iTest1 := Random(2);
        if iTest1 = 0 then begin
          // Addition: the carry of the addition should be 0 to avoid numbers "out of size"
          CarryChk := 0;
        end
        else begin
          // Subtraction: the carry of the 2's complement addition should be 1 to avoid negative numbers
          laQuestionText.Caption := StringReplace(laQuestionText.Caption, 'Add', 'Subtract', []);
          CarryChk := 1;
        end;
        repeat
          // Generate 2 random binary numbers until the carry of the operation is as wanted
          Bin1  := RandomBinary(iSize); Bin2 := RandomBinary(iSize);
          NBin1 := NumberBinary(Bin1);  NBin2 := NumberBinary(Bin2);
          if iTest1 = 0 then
            AddBinary(NBin1, NBin2, NBin, Carry)
          else
            SubtractBinary(NBin1, NBin2, NBin, Carry);
        until Carry = CarryChk;
      end
      // Logical operations test
      else begin
        laQuestionText.Caption := 'Do and AND operation of two binary numbers.';
        // Randomly select one of 6 logical operators
        iTest1 := Random(6);
        laQuestionText.Caption := StringReplace(laQuestionText.Caption, 'AND', LogicalOperators[iTest1], []);
        // Generate 2 random binary numbers
        Bin1  := RandomBinary(iSize); Bin2 := RandomBinary(iSize);
        NBin1 := NumberBinary(Bin1);  NBin2 := NumberBinary(Bin2);
        // Perform the logical operation
        Bin := LogicalOpBinary(Bin1, Bin2, LogicalOperators[iTest1]);
        NBin := NumberBinary(Bin);
      end;
      // Save all 3 values (as strings)
      edQuestion1.Text := DisplayBinary(NBin1, iBitsSep);
      edQuestion2.Text := DisplayBinary(NBin2, iBitsSep);
      edQuestion3.SetFocus;
      sAnswer1 := DisplayBinary(NBin1, 0);
      sAnswer2 := DisplayBinary(NBin2, 0);
      sAnswer3 := DisplayBinary(NBin, 0);
      sBinAnswer := DisplayBinary(NBin, iBitsSep);                                       // binary value of result with separators (used for display in case of "false answer")
    end;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Answer';
  end
  // Button "Answer": Check user answer
  else begin
    // Read content of all 3 input fields from form (eliminating eventual spaces)
    UAnswer1 := StringReplace(edQuestion1.Text, ' ', '', [rfReplaceAll]);
    UAnswer2 := StringReplace(edQuestion2.Text, ' ', '', [rfReplaceAll]);
    UAnswer3 := StringReplace(edQuestion3.Text, ' ', '', [rfReplaceAll]);
    // Compare user values to program values
    if (UAnswer1 = sAnswer1) and (UAnswer2 = sAnswer2) and (UAnswer3 = sAnswer3) then begin
      // Correct answer
      edEval.Text := 'This is correct!'; edEval.Color := clLime;
      Inc(iCorrect);
    end
    else begin
      // False answer
      edEval.Text := 'False! Correct = ';
      // Display correct answer in this case
      if iTest = 1 then begin
        if iTest1 = 0 then
          edEval.Text := edEval.Text + sAnswer2 + ' / ' + sAnswer3
        else if iTest1 = 1 then
          edEval.Text := edEval.Text + sBinAnswer + ' / ' + sAnswer3
        else
          edEval.Text := edEval.Text + sBinAnswer + ' / ' + sAnswer2;
      end
      else
        edEval.Text := edEval.Text + sBinAnswer;
      edEval.Color := clRed;
    end;
    // Update evaluation counters
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    btQuestion.Caption := 'Question';
  end;
end;

end.

