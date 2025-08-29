{**************************************}
{* Main unit for SimpleCplxCalculator *}
{**************************************}

unit calculator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, UComplex, help;

type
  {**************}
  { TfCalculator }
  {**************}
  TfCalculator = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    edResult: TEdit;
    edCalc: TEdit;
    Shape1: TShape;
    bt1, bt2, bt3, bt4, bt5, bt6, bt7, bt8, bt9, bt0: TButton;
    btDS: TButton;
    btSgn: TButton;
    btImPlus, btImMinus: TButton;
    btPlus, btMinus, btMult, btDiv: TButton;
    btResult: TButton;
    btSquare, btCube, btInverse, btConjugate: TButton;
    btMS, btMPlus, btMMinus, btMR, btMC: TButton;
    btCA, btCE, btC1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure bt1Click(Sender: TObject);
    procedure bt2Click(Sender: TObject);
    procedure bt3Click(Sender: TObject);
    procedure bt4Click(Sender: TObject);
    procedure bt5Click(Sender: TObject);
    procedure bt6Click(Sender: TObject);
    procedure bt7Click(Sender: TObject);
    procedure bt8Click(Sender: TObject);
    procedure bt9Click(Sender: TObject);
    procedure bt0Click(Sender: TObject);
    procedure btDSClick(Sender: TObject);
    procedure btSgnClick(Sender: TObject);
    procedure btImPlusClick(Sender: TObject);
    procedure btImMinusClick(Sender: TObject);
    procedure btPlusClick(Sender: TObject);
    procedure btMinusClick(Sender: TObject);
    procedure btMultClick(Sender: TObject);
    procedure btDivClick(Sender: TObject);
    procedure btResultClick(Sender: TObject);
    procedure btSquareClick(Sender: TObject);
    procedure btCubeClick(Sender: TObject);
    procedure btInverseClick(Sender: TObject);
    procedure btConjugateClick(Sender: TObject);
    procedure btMSClick(Sender: TObject);
    procedure btMPlusClick(Sender: TObject);
    procedure btMMinusClick(Sender: TObject);
    procedure btMRClick(Sender: TObject);
    procedure btMCClick(Sender: TObject);
    procedure btCAClick(Sender: TObject);
    procedure btCEClick(Sender: TObject);
    procedure btC1Click(Sender: TObject);
  private
    sCplxPart, sOperationNext, sOperationNow: string;
    bReDecSep, bImDecSep, bError: Boolean;
    cResult, cMemory: Complex;
  end;

var
  fCalculator: TfCalculator;

implementation

{$R *.lfm}

{ Convert complex number to string }

function ComplexToStr(CplxNumber: Complex): string;

// As a difference with "CStr" of the UComplex unit, this function returns a properly formatted string representation of a complex number
// Important: Real/imaginary values with an absolute value < 1E-7 are considered to be 0!

var
  L: Integer;
  SComplex, SReal, SImag: string;

begin
  // Complex z = 0: return '0'
  if (Abs(CplxNumber.Re) < 1E-7) and (Abs(CplxNumber.Im) < 1E-7) then
    SComplex := '0'
  // Complex z <> 0: return formatted string
  else begin
    // Round real and decimal part to 7 digits and remove all non-significant zeros
    Str(CplxNumber.Re:0:7, SReal); Str(Abs(CplxNumber.Im):0:7, SImag);
    L := Length(SReal);
    while SReal[L] = '0' do
      Dec(L);                                                                  // non-significant zeros
    if SReal[L] = '.' then
      Dec(L);                                                                  // decimal separator (without fractional digits)
    SReal := LeftStr(SReal, L);
    L := Length(SImag);
    while SImag[L] = '0' do
      Dec(L);
    if SImag[L] = '.' then
      Dec(L);
    SImag := LeftStr(SImag, L);
    SComplex := '';
    // Keep real part only if it is <> 0
    if Abs(CplxNumber.Re) >= 1E-7 then
      SComplex := SReal;
    // Keep imaginary part only if <> 0
    if Abs(CplxNumber.Im) >= 1E-7 then begin
      // Complex with real and imaginary part <> 0
      if Abs(CplxNumber.Re) >= 1E-7 then begin
        if CplxNumber.Im < 0 then
          SComplex += '-'
        else
          SComplex += '+';
      end
      // Complex with imaginary part only <> 0
      else begin
        if CplxNumber.Im < 0 then
          SComplex += '-'                                                      // minus sign has to be displayed (plus sign has not)
      end;
      if Abs(CplxNumber.Im) <> 1 then                                          // use "i" instead of "1i"
        SComplex += SImag;
      SComplex += 'i';
    end;
  end;
  Result := SComplex;
end;

{ Transform string to complex number }

function StrToComplex(SNumber: string): Complex;

// The string is supposed to be properly formatted (as the one returned by the ComplexToStr function)

var
  Sign, P: Integer;
  NumberRe, NumberIm: Real;

begin
  // To avoid problems, be sure that the imaginary part is given as a number + "i"
  if SNumber = 'i' then
    SNumber := '1i'
  else if SNumber = '-i' then
    SNumber := '-1i';
  // Complex number contains an imaginary part
  if RightStr(SNumber, 1) = 'i' then begin
    SNumber := LeftStr(SNumber, Length(SNumber) - 1);
    Sign := 1;
    // This "-" is the sign of the real part (if this one <> 0) or the sign of the imaginary part (if there is no real part)
    if LeftStr(SNumber, 1) = '-' then begin
      SNumber := RightStr(SNumber, Length(SNumber) - 1);
      Sign := -1;
    end;
    // This "+" resp. "-" sign starts the imaginary part; if both are absent, the real part = 0 (cf. ComplexToStr function)
    P := Pos('+', SNumber);
    if P = 0 then
      P := Pos('-', SNumber);
    if P = 0 then begin
      NumberRe := 0; Val(SNumber, NumberIm); NumberIm *= Sign;
    end
    else begin
      Val(LeftStr(SNumber, P - 1), NumberRe); NumberRe *= Sign;
      Val(RightStr(SNumber, Length(SNumber) - P + 1), NumberIm);
    end;
  end
  // Complex number contains no imaginary part
  else begin
    Val(SNumber, NumberRe); NumberIm := 0;
  end;
  StrToComplex := cinit(NumberRe, NumberIm);
end;

{ Procedure called when a digit (number) button has been pushed }

procedure NumberButton(Digit, CplxPart: string; ReDecSep, ImDecSep: Boolean; var Err: Boolean; var Operation: string);

var
  P: Integer;
  Number: Real;
  DecSep: Boolean;
  SZ: string;

begin
  // If there was an error (division by 0) before, execute a "clear all"
  if Err then begin
    fCalculator.btCA.Click;
    Err := False;
  end;
  // If any non digit button has been pushed before, start a new number
  if Operation <> '' then begin
    Operation := '';
    fCalculator.edResult.Text := '0';
  end;
  // "Create" actual number by adding the digit pressed
  SZ := fCalculator.edResult.Text;
  // Digit is part of the complexes real part
  if CplxPart = 'Re' then begin
    Val(SZ, Number);
    DecSep := ReDecSep;
  end
  // Digit is part of the complexes imaginary part
  else begin
    SZ := LeftStr(SZ, Length(SZ) - 1);                                         // remove the "i" (re-added later)
    P := Pos('+', SZ);
    if P = 0 then
      P := Pos('-', SZ);
    if P = Length(SZ) then                                                     // last digit is sign of complex numbers imaginary part
      Number := -1                                                             // set to -1 in order not to delete last digit in this case
    else
      Val(Copy(SZ, P + 1, Length(SZ) - P), Number);
    DecSep := ImDecSep;
  end;
  // Remove leading 0 (unless there is a decimal seperator present)
  if (Number = 0) and not DecSep then
    SZ := LeftStr(SZ, Length(SZ) - 1);
  // Add the digit
  SZ += Digit;
  // Don't forget to re-add the "i" (if digit was added to imaginary part)
  if CPlxPart = 'Im' then
    SZ += 'i';
  fCalculator.edResult.Text := SZ;
end;

{ Procedure called when an operation button (+, -, x, :) has been pushed }

procedure OperationButton(OperationNow: string; SNumber: string; var Result: Complex; var CplxPart: string; var ReDecSep, ImDecSep, Err: Boolean);

var
  Op: Char;
  Number: Complex;

begin
  Number := StrToComplex(SNumber);
  Op := OperationNow[1];
  // Display error message if division by 0
  if (Op = ':') and (Number = 0) then begin
    Result := cinit(0, 0);
    fCalculator.edResult.Text := 'Error: Can''t divide by 0';
    fCalculator.edCalc.Text := '';
    Err := True;
  end
  // Perform arithmetic operation (operator overloading for complex numbers defined in the UComplex unit)
  else begin
    Err := False;
    case Op of
      '+': Result := Result + Number;
      '-': Result := Result - Number;
      'x': Result := Result * Number;
      ':': Result := Result / Number;
    end;
    fCalculator.edResult.Text := ComplexToStr(Result);
    SNumber := ComplexToStr(Number);
    fCalculator.edCalc.Text := fCalculator.edCalc.Text + SNumber;
  end;
  // Digits will be added à priori to complexes real part
  CplxPart := 'Re';
  // Digits will added to integer parts of complex parts
  ReDecSep := False; ImDecSep := False;
end;

{**************}
{ TfCalculator }
{**************}

{ Application start: Initialisation }

procedure TfCalculator.FormCreate(Sender: TObject);

begin
  sCplxPart := 'Re';                                                           // if digit is entered, it is à priori part of the complexes real part
  sOperationNext := '+';                                                       // next operation to perform
  sOperationNow := '';                                                         // actual operation
  bReDecSep := False; bImDecSep := False;                                      // set to true if complexes real resp. imaginary part contains decimal seperator
  bError := False;                                                             // set if an error (division by 0) occured
  cResult := cinit(0, 0);                                                      // actual complex number displayed
  cMemory := cinit(0, 0);                                                      // memory content
end;

{ Menu item "File > Exit": Exit the application }

procedure TfCalculator.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display program help }

procedure TfCalculator.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfCalculator.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Basic arithmetic operations with complex numbers.' + LineEnding;
  S += 'Version 1.1, © allu, April 2025.';
  MessageDlg('About "SimpleCplxCalculator"', S, mtInformation, [mbOK], 0);
end;

{ Digits buttons (1..0) pushed: Add digit to actual number }

procedure TfCalculator.bt1Click(Sender: TObject);

begin
  NumberButton('1', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt2Click(Sender: TObject);

begin
  NumberButton('2', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt3Click(Sender: TObject);

begin
  NumberButton('3', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt4Click(Sender: TObject);

begin
  NumberButton('4', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt5Click(Sender: TObject);

begin
  NumberButton('5', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt6Click(Sender: TObject);

begin
  NumberButton('6', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt7Click(Sender: TObject);

begin
  NumberButton('7', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt8Click(Sender: TObject);

begin
  NumberButton('8', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt9Click(Sender: TObject);

begin
  NumberButton('9', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

procedure TfCalculator.bt0Click(Sender: TObject);

begin
  NumberButton('0', sCplxPart, bReDecSep, bImDecSep, bError, sOperationNow);
end;

{ Decimal separator (.) button pushed: Add "." to complexes real or imaginary part }

procedure TfCalculator.btDSClick(Sender: TObject);

var
  SZ: string;

begin
  // If there was an error, clear all
  if bError then begin
    btCA.Click;
    bError := False;
  end
  // Add decimal separator
  else begin
    SZ := edResult.Text;
    if (sCplxPart = 'Re') and not bReDecSep then begin
      // Add decimal separator to complexes real part
      SZ += '.';
      bReDecSep := True;
    end
    else if (sCplxPart = 'Im') and not bImDecSep then begin
      // Add decimal separator to complexes imaginary part
      SZ := LeftStr(SZ, Length(SZ) - 1) + '.i';
      bImDecSep := True;
    end;
    edResult.Text := SZ;
  end;
end;

{ Sign button (+/-) pushed: Change sign of complexes real part }

procedure TfCalculator.btSgnClick(Sender: TObject);

var
  SZ: string;

begin
  // If there was an error, clear all
  if bError then begin
    btCA.Click;
    bError := False;
  end
  // Change complexes part sign
  else begin
    SZ := edResult.Text;
    if (sCplxPart = 'Re') and (SZ <> '') then begin
      if LeftStr(SZ, 1) = '-' then
        SZ := RightStr(SZ, Length(SZ) - 1)
      else
        SZ := '-' + SZ;
    end;
    edResult.Text := SZ;
  end;
end;

{ "Imaginary" buttons (+i resp -i) pushed: Start complexes imaginary part }

procedure TfCalculator.btImPlusClick(Sender: TObject);

// +i button for complex with positive imaginary part

var
  SZ: string;

begin
  // If there was an error, clear all
  if bError then begin
    btCA.Click;
    bError := False;
  end;
  // If there was any operation just before, this starts a new number (actually a complex with Re(z) = 0 )
  if sOperationNow <> '' then begin
    sOperationNow := '';
    fCalculator.edResult.Text := '0';
  end;
  // Start the imaginary part of the complex by displaying "+0i" ("0i", if Re(z) = 0)
  SZ := edResult.Text;
  if (sCplxPart = 'Re') and (RightStr(SZ, 1) <> '+') and (RightStr(SZ, 1) <> '-') then begin
    if SZ = '0' then
      SZ := '0i';
    if SZ <> '0i' then
      SZ += '+0i';
    sCplxPart := 'Im';
    edResult.Text := SZ;
  end;
end;

procedure TfCalculator.btImMinusClick(Sender: TObject);

// -i button for complex with negative imaginary part

var
  SZ: string;

begin
  // If there was an error, clear all
  if bError then begin
    btCA.Click;
    bError := False;
  end;
  // If there was any operation just before, this starts a new number (actually a complex with Re(z) = 0 )
  if sOperationNow <> '' then begin
    sOperationNow := '';
    fCalculator.edResult.Text := '0';
  end;
  // Start the imaginary part of the complex by displaying "-0i"
  SZ := edResult.Text;
  if (sCplxPart = 'Re') and (RightStr(SZ, 1) <> '+') and (RightStr(SZ, 1) <> '-') then begin
    if SZ = '0' then
      SZ := '-0i';
    if SZ <> '-0i' then
      SZ += '-0i';
    sCplxPart := 'Im';
    edResult.Text := SZ;
  end;
end;

{ Operations buttons (+, -, x, :) pushed: Perform corr. arithmetic operation }

procedure TfCalculator.btPlusClick(Sender: TObject);

// Addition (+) button

begin
  // Actual operation has to be performed; next operation will be '+'
  if (sOperationNow = '') or (sOperationNow = '=') then begin
    sOperationNow := sOperationNext;
    sOperationNext := '+';
    OperationButton(sOperationNow, edResult.Text, cResult, sCplxPart, bReDecSep, bImDecSep, bError);
    edCalc.Text := edCalc.Text + ' ' + '+' + ' ';
  end
  // Actual operation has already been performed; next operation will be '+'
  // (this occurs if user pushes 2 operations buttons directly one after the other)
  else begin
    sOperationNext := '+';
    edCalc.Text := LeftStr(edCalc.Text, Length(edCalc.Text) - 3) + ' ' + '+' + ' ';
  end;
end;

procedure TfCalculator.btMinusClick(Sender: TObject);

// Subtraction (-) button

begin
  // Actual operation has to be performed; next operation will be '-'
  if (sOperationNow = '') or (sOperationNow = '=') then begin
    sOperationNow := sOperationNext;
    sOperationNext := '-';
    OperationButton(sOperationNow, edResult.Text, cResult, sCplxPart, bReDecSep, bImDecSep, bError);
    edCalc.Text := edCalc.Text + ' ' + '-' + ' ';
  end
  // Actual operation has already been performed; next operation will be '-'
  else begin
    sOperationNext := '-';
    edCalc.Text := LeftStr(edCalc.Text, Length(edCalc.Text) - 3) + ' ' + '-' + ' ';
  end;
end;

procedure TfCalculator.btMultClick(Sender: TObject);

// Multiplication (x) button

begin
  // Actual operation has to be performed; next operation will be 'x'
  if (sOperationNow = '') or (sOperationNow = '=') then begin
    sOperationNow := sOperationNext;
    sOperationNext := 'x';
    OperationButton(sOperationNow, edResult.Text, cResult, sCplxPart, bReDecSep, bImDecSep, bError);
    edCalc.Text := edCalc.Text + ' ' + 'x' + ' ';
  end
  // Actual operation has already been performed; next operation will be 'x'
  else begin
    sOperationNext := 'x';
    edCalc.Text := LeftStr(edCalc.Text, Length(edCalc.Text) - 3) + ' ' + 'x' + ' ';
  end;
end;

procedure TfCalculator.btDivClick(Sender: TObject);

// Division (:) button

begin
  // Actual operation has to be performed; next operation will be ':'
  if (sOperationNow = '') or (sOperationNow = '=') then begin
    sOperationNow := sOperationNext;
    sOperationNext := ':';
    OperationButton(sOperationNow, edResult.Text, cResult, sCplxPart, bReDecSep, bImDecSep, bError);
    edCalc.Text := edCalc.Text + ' ' + ':' + ' ';
  end
  // Actual operation has already been performed; next operation will be ':'
  else begin
    sOperationNext := ':';
    edCalc.Text := LeftStr(edCalc.Text, Length(edCalc.Text) - 3) + ' ' + ':' + ' ';
  end;
end;

{ Result (=) button: Perform actual operation to give final result }

procedure TfCalculator.btResultClick(Sender: TObject);

begin
  // Do only if not yet done ('=' button only works once)
  if sOperationNext <> '' then begin
    // Perform actual operation (for '=', as for operation buttons, there is always yet an operation to be done!)
    // If the last push was on an operation button, the operation is performed with both operands equal to the actual number
    sOperationNow := sOperationNext;
    OperationButton(sOperationNow, edResult.Text, cResult, sCplxPart, bReDecSep, bImDecSep, bError);
    // Prepare for new calculation
    edCalc.Text := ''; cResult := 0;
    sOperationNow := '='; sOperationNext := '+';
  end;
end;

{ Function (a², a³, 1/a, ā) buttons: Perform the corr. function }

procedure TfCalculator.btSquareClick(Sender: TObject);

// Square (a²) button

var
  Number: Complex;

begin
  Number := StrToComplex(edResult.Text);
  Number := Number * Number;                                                   // operator overload defined in UComplex unit
  edResult.Text := ComplexToStr(Number);
  sOperationNow := '=';
end;

procedure TfCalculator.btCubeClick(Sender: TObject);

// Cube (a³) button

var
  Number: Complex;

begin
  Number := StrToComplex(edResult.Text);
  Number := Number * Number * Number;                                          // operator overload defined in UComplex unit
  edResult.Text := ComplexToStr(Number);
  sOperationNow := '=';
end;

procedure TfCalculator.btInverseClick(Sender: TObject);

// Inverse (1/a) button

var
  Number: Complex;

begin
  Number := StrToComplex(edResult.Text);
  // If division by 0, display error message...
  if Number = 0 then begin
    cResult := cinit(0, 0);
    fCalculator.edResult.Text := 'Error: Can''t divide by 0';
    fCalculator.edCalc.Text := '';
    bError := True;
  end
  // ...otherwise calculate 1/a
  else begin
    Number := cinv(Number);                                                    // cinv(a) = 1/a defined in the UComplex unit
    edResult.Text := ComplexToStr(Number);
    sOperationNow := '=';
  end;
end;

procedure TfCalculator.btConjugateClick(Sender: TObject);

// Conjugate (ā) button

var
  Number: Complex;

begin
  Number := StrToComplex(edResult.Text);
  Number := cong(Number);                                                      // cong(a) = ā defined in the UComplex unit
  edResult.Text := ComplexToStr(Number);
  sOperationNow := '=';
end;

{ Memory buttons (MS, M+, M-, MR, MC): Perform memory operation }

procedure TfCalculator.btMSClick(Sender: TObject);

// Memory Store (MS) button

begin
  cMemory := StrToComplex(edResult.Text);                                      // store actual number into memory
  sOperationNow := '=';
end;

procedure TfCalculator.btMPlusClick(Sender: TObject);

// Memory Addition (M+) button

begin
  cMemory := cMemory + StrToComplex(edResult.Text);                            // add actual number to memory content
  sOperationNow := '=';
end;

procedure TfCalculator.btMMinusClick(Sender: TObject);

// Memory Subtraction (M-) button

begin
  cMemory := cMemory - StrToComplex(edResult.Text);                            // subtract actual number from memory content
  sOperationNow := '=';
end;

procedure TfCalculator.btMRClick(Sender: TObject);

// Memory Recall (MR) button

begin
  edResult.Text := ComplexToStr(cMemory);                                      // retrieve memory content (becoming actual number)
  sOperationNow := '=';
end;

procedure TfCalculator.btMCClick(Sender: TObject);

// Memory Clear (MC) button

begin
  cMemory := cinit(0, 0);                                                      // reset memory to 0
end;

{ Clear buttons (CA, CE, <-): Clear all/entry/last digit }

procedure TfCalculator.btCAClick(Sender: TObject);

// Clear all (CA) button

begin
  edResult.Text := '0'; edCalc.Text := '';
  sCplxPart := 'Re';
  bReDecSep := False; bImDecSep := False;
  sOperationNext := '+'; sOperationNow := '';
  cResult := cinit(0, 0);
end;

procedure TfCalculator.btCEClick(Sender: TObject);

// Clear entry (CE) button

begin
  // If there was an error, clear entire calculation...
  if bError then begin
    btCA.Click;
    bError := False;
  end
  // ...otherwise clear entry (actual number)
  else begin
    edResult.Text := '0';
    sCplxPart := 'Re';
    bReDecSep := False; bImDecSep := False;
  end;
end;

procedure TfCalculator.btC1Click(Sender: TObject);

// Clear last digit (<-) button

var
  SZ, Ch: string;

begin
  // If there was an error, clear entire calculation...
  if bError then begin
    btCA.Click;
    bError := False;
  end
  // ...otherwise clear last digit of actual number
  else begin
    SZ := edResult.Text;
    if sCplxPart = 'Im' then
      SZ := LeftStr(SZ, Length(SZ) - 1);                                       // remove "i" (re-add later if needed so)
    Ch := RightStr(SZ, 1);
    // Decimal separator removed or complexes entire imaginary part removed
    if sCplxPart = 'Re' then begin
      if Ch = '.' then
        // If decimal separator of complexes real part is removed, update corr. Boolean variable
        bReDecSep := False;
    end
    else begin
      if Ch = '.' then
        // If decimal separator of complexes real part is removed, update corr. Boolean variable
        bImDecSep := False
      else if (Ch = '+') or (Ch = '-') then
        // If complexes entire imaginary part has been removed, new digits will be added to its real part
        sCplxPart := 'Re';
    end;
    // If there is nothing more left, display "0"...
    if Length(SZ) = 1 then
      SZ := '0'
    // ...otherwise display what's left
    else
      SZ := LeftStr(SZ, Length(SZ) - 1);
    // If there is still an imaginary part, re-add the "i"
    if sCplxPart = 'Im' then
      SZ += 'i';
    edResult.Text := SZ;
  end;
end;

end.

