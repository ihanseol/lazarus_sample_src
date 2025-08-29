{*******************************************}
{* Main unit for HexCalculator application *}
{*******************************************}

unit calculator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls;

type
  {**************}
  { TfCalculator }
  {**************}
  TfCalculator = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mOptions, mOptionsMessOff, mHelp, mHelpAbout: TMenuItem;
    Shape1: TShape;
    edDisplayDec, edDisplayHex, edDisplay: TEdit;
    cobSize: TComboBox;
    btMR, btMS, btMC, btClear: TButton;
    btN0, btN1, btN2, btN3, btN4, btN5, btN6, btN7: TButton;
    btN8, btN9, btN10, btN11, btN12, btN13, btN14, btN15: TButton;
    btAdd, btSubtract, btMultiply, btDiv, btMod, btResult: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsMessOffClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure cobSizeChange(Sender: TObject);
    procedure btMRClick(Sender: TObject);
    procedure btMSClick(Sender: TObject);
    procedure btMCClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btN0Click(Sender: TObject);
    procedure btN1Click(Sender: TObject);
    procedure btN2Click(Sender: TObject);
    procedure btN3Click(Sender: TObject);
    procedure btN4Click(Sender: TObject);
    procedure btN5Click(Sender: TObject);
    procedure btN6Click(Sender: TObject);
    procedure btN7Click(Sender: TObject);
    procedure btN8Click(Sender: TObject);
    procedure btN9Click(Sender: TObject);
    procedure btN10Click(Sender: TObject);
    procedure btN11Click(Sender: TObject);
    procedure btN12Click(Sender: TObject);
    procedure btN13Click(Sender: TObject);
    procedure btN14Click(Sender: TObject);
    procedure btN15Click(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btSubtractClick(Sender: TObject);
    procedure btMultiplyClick(Sender: TObject);
    procedure btDivClick(Sender: TObject);
    procedure btModClick(Sender: TObject);
    procedure btResultClick(Sender: TObject);
  private
    iSize, iClear: Integer;
    iInput, iResult: Int64;
    sInput, sResult, sMemory, sDisplay, sDisplayHex, sDisplayDec: string;
    cOperation, cLastOperation: Char;
  end;

var
  fCalculator: TfCalculator;

implementation

{$R *.lfm}

{ Simplified power function (M**N; M,N = positive integers) }

function Power(M, N: Word): QWord;

var
  I: Integer;
  Pow: QWord;

begin
  Pow := 1;
  for I := 1 to N do
    Pow *= M;
  Result := Pow;
end;

{ Hexadecimal to decimal conversion }

function HexToDec(H: string): Int64;

// Numbers are supposed to be 2, 4, 8, or 16 hexadecimal digits
// Left-most digit decides of decimal number sign

var
  I, D0: Integer;
  D, M: Int64;
  IsNegative: Boolean;

begin
  IsNegative := False;
  if LeftStr(H, 1) > '7' then begin
    // Negative number: Transform to "corresponding positive number"
    IsNegative := True;                                                        // Mark number as being negative
    for I := 1 to Length(H) do begin
      if H[I] in ['0'..'9'] then
        D0 := 15 - StrToInt(H[I])
      else
        D0 := 15 - (10 + Ord(H[I]) - Ord('A'));
      if D0 in [0..9] then
        H[I] := IntToStr(D0)[1]
      else
        H[I] := Chr(D0 - 10 + Ord('A'));
    end;
  end;
  // Positive numbers hexadecimal to decimal conversion
  D := 0; M := 1;
  for I := Length(H) downto 1 do begin
    if H[I] in ['0'..'9'] then
      D += M * StrToInt(H[I])
    else
      D += M * (10 + Ord(H[I]) - Ord('A'));
    M *= 16;
  end;
  // Adapt decimal number if number has to be negative
  if IsNegative then
    D := -D - 1;
  Result := D;
end;

{ Decimal to hexadecimal conversion }

function DecToHex(D: Int64; Size: Integer): string;

// Actually used number size decides on hexadecimal representation of negative numbers
// Ex: for bytes: FF to 80 are negative, for words these are positive, whereas negative numbers are 8000 to FFFF

var
  R, I: Integer;
  D0, Q: QWord;
  H: string;

begin
  H := '';
  // Transform number to positive decimal
  if D >= 0 then
    D0 := D
  else
    D0 := Power(2, Size) - Abs(D);
  // Decimal to hexadecimal conversion by successive divisions
  while D0 > 0 do begin
    Q := D0 div 16; R := D0 mod 16;
    if R in [0..9] then
      H := IntToStr(R) + H
    else
      H := Chr(R - 10 + Ord('A')) + H;
    D0 := Q;
  end;
  // Add zeros on the left to get hexadecimal numbers with 2, 4, 8, or 16 digits
  for I := 1 to Size div 4 - Length(H) do
    H := '0' + H;
  Result := H;
end;

{ Hexadecimal digit input (user pushed keys '0' - '9', 'A' - 'F') }

procedure InputDigit(Digit: Char; var Input, Display: string; var Clear: Integer; Size: Integer);

var
  S: string;

begin
  if (Display <> 'Error') and (Display <> 'Overflow') then begin
    S := Input + Digit;
    if Length(S) > Size / 4 then begin
      // Number may only be 2 digits for byte, 4 digits for word, etc
      if not fCalculator.mOptionsMessOff.Checked then
        MessageDlg('Input error', 'Too many hexadecimal digits for actual number size!', mtError, [mbOK], 0);
    end
    else begin
      Input += Digit;
      if Length(Input) = 1 then begin
        // This is the first digit of the number input
        Display := '';
      end;
      Display += Digit;
      fCalculator.edDisplay.Text := Display;
    end;
    Clear := 0;                                                                // 'Clear' button will erase number input
  end;
end;

{ Operator input (user pushed keys '+', '-', '×', '/', '%', '=') }

procedure InputOperation(Operation: Char; var LastOperation: Char;
  var Input, Result, Display, HDisplay, DDisplay: string; var NInput, NResult: Int64; var Clear: Integer; Size: Integer);

var
  I: Integer;

function DisplayDecimal(N: Int64): string;

var
  SN: string;

begin
  SN := '';
  if N < 0 then
    SN := '(';
  SN += IntToStr(N);
  if N < 0 then
    SN += ')';
  Result := SN;
end;

begin
  if (Display <> 'Error') and (Display <> 'Overflow') then begin
    if Input <> '' then begin
      for I := 1 to Size div 4 - Length(Input) do begin
        // Add leading zeros
        Input := '0' + Input;
      end;
      if Result = '' then begin
        // This is for the very first operand
        NInput := HexToDec(Input); NResult := NInput;
        Result := Input; Display := Input;
        HDisplay := Input + ' ' + Operation;
        DDisplay := DisplayDecimal(NInput) + ' ' + Operation;;
        Input := '';
        LastOperation := Operation;
      end
      else begin
        // This is for any not first operand
        NResult := HexToDec(Result); NInput := HexToDec(Input);
        case LastOperation of
          // Compute result using previously entered operator
          '+': NResult += NInput;
          '-': NResult -= NInput;
          'x': NResult *= NInput;
          '/': begin
                 if NInput = 0 then begin
                   Display := 'Error';
                   HDisplay += ' ' + Input;
                   DDisplay += ' 0';
                 end
                 else
                   NResult := NResult div NInput;
               end;
          '%': begin
                 if NInput = 0 then begin
                   Display := 'Error';
                   HDisplay += ' ' + Input;
                   DDisplay += ' 0';
                 end
                 else
                   NResult := NResult mod NInput;
               end;
        end;
        if Display <> 'Error' then begin
          // Check for overflow
          if ((NResult > 0) and (NResult > Power(2, Size) div 2 - 1)) or ((NResult < 0) and (-NResult > Power(2, Size) div 2)) then begin
            Display := 'Overflow';
            HDisplay += ' ' + Input;
            DDisplay += ' ' + DisplayDecimal(NInput);
          end
          else begin
            Result := DecToHex(NResult, Size); Display := Result;
            if Operation = '=' then begin
              // Actual operation is '='
              HDisplay += ' ' + Input + ' = ' + Result;
              DDisplay += ' ' + DisplayDecimal(NInput) + ' = ' + IntToStr(NResult);
              Input := Result; Result := '';
              LastOperation := ' ';
            end
            else begin
              // Actual operation is arithmetic operator
              HDisplay += ' ' + Input + ' ' + Operation;
              DDisplay += ' ' + DisplayDecimal(NInput) + ' ' + Operation;
              Input := '';
              LastOperation := Operation;
            end;
          end;
        end;
      end;
      fCalculator.edDisplay.Text := Display;
      fCalculator.edDisplayHex.Text := HDisplay;
      fCalculator.edDisplayDec.Text := DDisplay;
    end;
   end;
   Clear := 1                                                                  // 'Clear' button will reset the calculator
end;

{**************}
{ TfCalculator }
{**************}

{ Application start: Initialization }

procedure TfCalculator.FormCreate(Sender: TObject);

begin
  iSize := 16; iClear := -1;
  sInput := ''; sResult := ''; sMemory := '0000';
  sDisplay := '';
  cLastOperation := ' ';
end;

{ Menu item "File > Exit": Exit application }

procedure TfCalculator.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Turn messages off": Toggle message display on/off }

procedure TfCalculator.mOptionsMessOffClick(Sender: TObject);

begin
  if mOptionsMessOff.Checked then
    mOptionsMessOff.Checked := False
  else
    mOptionsMessOff.Checked := True;
end;

{ Menu item "Help > About": Display application about }

procedure TfCalculator.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Simple hexadecimal calculator.' + LineEnding;
  S += 'Choose the number size (byte, word, double word, quad word), then use the calculator to do hexadecimal additions, ';
  S += 'subtractions, multiplications or integer divisions (/ = division result, % = division remainder). Note, that there is ';
  S += 'no operator precedence.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, November 2024 - January 2025.';
  MessageDlg('About "SimpleHexCalculator"', S, mtInformation, [mbOK], 0);
end;

{ Hexadecimal digit button pushed }

procedure TfCalculator.btN0Click(Sender: TObject);

begin
  InputDigit('0', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN1Click(Sender: TObject);

begin
  InputDigit('1', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN2Click(Sender: TObject);

begin
  InputDigit('2', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN3Click(Sender: TObject);

begin
  InputDigit('3', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN4Click(Sender: TObject);

begin
  InputDigit('4', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN5Click(Sender: TObject);

begin
  InputDigit('5', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN6Click(Sender: TObject);

begin
  InputDigit('6', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN7Click(Sender: TObject);

begin
  InputDigit('7', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN8Click(Sender: TObject);

begin
  InputDigit('8', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN9Click(Sender: TObject);

begin
  InputDigit('9', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN10Click(Sender: TObject);

begin
  InputDigit('A', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN11Click(Sender: TObject);

begin
  InputDigit('B', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN12Click(Sender: TObject);

begin
  InputDigit('C', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN13Click(Sender: TObject);

begin
  InputDigit('D', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN14Click(Sender: TObject);

begin
  InputDigit('E', sInput, sDisplay, iClear, iSize);
end;

procedure TfCalculator.btN15Click(Sender: TObject);

begin
  InputDigit('F', sInput, sDisplay, iClear, iSize);
end;

{ Operation bautton pushed }

procedure TfCalculator.btAddClick(Sender: TObject);

begin
  cOperation := '+';
  InputOperation(cOperation, cLastOperation, sInput, sResult, sDisplay, sDisplayHex, sDisplayDec, iInput, iResult, iClear, iSize);
end;

procedure TfCalculator.btSubtractClick(Sender: TObject);

begin
  cOperation := '-';
  InputOperation(cOperation, cLastOperation, sInput, sResult, sDisplay, sDisplayHex, sDisplayDec, iInput, iResult, iClear, iSize);
end;

procedure TfCalculator.btMultiplyClick(Sender: TObject);

begin
  cOperation := 'x';
  InputOperation(cOperation, cLastOperation, sInput, sResult, sDisplay, sDisplayHex, sDisplayDec, iInput, iResult, iClear, iSize);
end;

procedure TfCalculator.btDivClick(Sender: TObject);

begin
  cOperation := '/';
  InputOperation(cOperation, cLastOperation, sInput, sResult, sDisplay, sDisplayHex, sDisplayDec, iInput, iResult, iClear, iSize);
end;

procedure TfCalculator.btModClick(Sender: TObject);

begin
  cOperation := '%';
  InputOperation(cOperation, cLastOperation, sInput, sResult, sDisplay, sDisplayHex, sDisplayDec, iInput, iResult, iClear, iSize);
end;

procedure TfCalculator.btResultClick(Sender: TObject);

begin
  cOperation := '=';
  InputOperation(cOperation, cLastOperation, sInput, sResult, sDisplay, sDisplayHex, sDisplayDec, iInput, iResult, iClear, iSize);
end;

{ 'Memory clear' button pushed }

procedure TfCalculator.btMCClick(Sender: TObject);

var
  I: Integer;

begin
  sMemory := '';
  for I := 1 to iSize div 4 do
    sMemory += '0';
end;

{ 'Memory recall' button pushed }

procedure TfCalculator.btMRClick(Sender: TObject);

begin
  // Only works if user hasn't started to enter a number,
  // i.e. after an operator button, '=', or the 'Clear' button
  if sInput = '' then begin
    sInput := sMemory; sDisplay := sInput;
    edDisplay.Text := sDisplay;
  end;
end;

{ 'Memory save' button pushed }

procedure TfCalculator.btMSClick(Sender: TObject);

var
  I: Integer;
  Input: string;

begin
  if sInput <> '' then begin
    // Only works if user has actually started to enter a number
    Input := sInput;
    for I := 1 to iSize div 4 - Length(sInput) do
      Input := '0' + Input;
    sMemory := Input;
  end;
end;

{ 'Clear' button pushed }

procedure TfCalculator.btClearClick(Sender: TObject);

begin
  if iClear = 0 then begin
    // Erase number actually being entered
    sDisplay := Copy(sDisplay, 1, Length(sDisplay) - Length(sInput));
    sInput := ''; sDisplay := '';
    iClear := 1;                                                               // a further button push will reset the calculator
  end
  else if iClear = 1 then begin
    // Reset the calculator (except for memory)
    sInput := ''; sResult := ''; sDisplay := '';
    sDisplayHex := ''; sDisplayDec := '';
    iClear := -1;                                                              // a further button push will have no effect
  end;
  edDisplay.Text := sDisplay; edDisplayHex.Text := sDisplayHex; edDisplayDec.Text := sDisplayDec;
end;

{ Operand size selection (user changed actual combobox item) }

procedure TfCalculator.cobSizeChange(Sender: TObject);

var
  Ret: Cardinal;

begin
  if not fCalculator.mOptionsMessOff.Checked and (sDisplay <> '') then
    Ret := MessageDlg('Hex Calculator', 'Changing the number size will reset the calculator. Continue?', mtConfirmation, [mbYes, mbNo], 0, mbYes);
  if fCalculator.mOptionsMessOff.Checked or (sDisplay = '') or (Ret = mrYes) then begin
    case cobSize.ItemIndex of
      0: iSize := 8;
      1: iSize := 16;
      2: iSize := 32;
      3: iSize := 64;
    end;
    iClear := 1;
    btClear.Click;
    btMC.Click;
  end;
end;

end.

