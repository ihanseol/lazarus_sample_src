{*****************************************}
{* Main unit for Eigenvalues application *}
{*****************************************}

unit eigenv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Buttons, StdCtrls;

type
  TMatrix2 = array[0..1, 0..1] of Real;
  TValues  = array of Real;
  TVector  = array of Real;
  TVectors = array of TVector;
  {***************}
  { TfEigenvalues }
  {***************}
  TfEigenvalues = class(TForm)
    mMenu: TMainMenu;
    mMatrix, mMatrix2, mMatrixExit: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label6, Label7, Label8: TLabel;
    edMatrix1, edMatrix2, edMatrix4, edMatrix5: TEdit;
    edValue1, edValue2, edVector1, edVector2: TEdit;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mMatrix2Click(Sender: TObject);
    procedure mMatrixExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
  private
    iSize: Integer;
    bStart: Boolean;
    aMatrix2: TMatrix2;
    aValues: TValues;
    aVectors: TVectors;
    edMatrix: array[0..2, 0..2] of TEdit;
    edEigenvalues, edEigenvectors: array[0..2] of TEdit;
  end;

var
  fEigenvalues: TfEigenvalues;

implementation

{$R *.lfm}

{ Integer power of a real number }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Format of a real number to specified number of decimal digits }

function RFormat(R: Real; F: Integer): string;

var
  R0: Real;
  SR: string;

begin
  SR := '';
  if R = 0 then
    SR := '0'
  else begin
    if F >= 0 then begin
      R0 := Round(R * Power(10, F)) / Power(10, F);
      if Abs(R0) < Power(10, -F) then
        SR := FloatToStrF(R, ffExponent, F, 0)             // use exponential format if number is to small for wanted format
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Reset form components }

procedure FormReset(N: Integer);

var
  I, J: Integer;

begin
  for I := 0 to N - 1 do begin
    for J := 0 to N - 1 do begin
      fEigenvalues.edMatrix[I, J].Text := '';
      fEigenvalues.edEigenvalues[I].Text := '';
      fEigenvalues.edEigenvectors[I].Text := '';
    end;
  end;
end;

{ Read matrix values from form edit fields }

procedure ReadMatrix(N: Integer; out Matrix2: TMatrix2; out Mess: string);

var
  I, J: Integer;

begin
  Mess := '';
  for I := 0 to N - 1 do begin
    for J := 0 to N - 1 do begin
      if fEigenvalues.edMatrix[I, J].Text = '' then begin
        Mess := 'Missing matrix value(s)';
        fEigenvalues.edMatrix[I, J].SetFocus;
        Exit;
      end
      else begin
        Matrix2[I, J] := StrToFloat(fEigenvalues.edMatrix[I, J].Text)
      end;
    end;
    if Mess <> '' then
      Exit;
  end;
end;

{ Solve 2nd degree equation }

procedure SolveEquation2(A, B, C: Real; var Roots: TValues; out Mess: string);

var
  Delta: Real;

begin
  Roots[0] := 0; Roots[1] := 0; Mess := '';
  Delta := Sqr(B) - 4 * A * C;
  if Delta < 0 then
    Mess := 'no real solution'
  else begin
    Roots[0] := (-B + Sqrt(Delta)) / (2*A);
    Roots[1] := (-B - Sqrt(Delta)) / (2*A);
  end;
end;

{***************}
{ TfEigenvalues }
{***************}

{ Application start: Initialization }

procedure TfEigenvalues.FormCreate(Sender: TObject);

begin
  edMatrix[0, 0] := edMatrix1; edMatrix[0, 1] := edMatrix2;
  edMatrix[1, 0] := edMatrix4; edMatrix[1, 1] := edMatrix5;
  edEigenvalues[0] := edValue1; edEigenvalues[1] := edValue2;
  edEigenvectors[0] := edVector1; edEigenvectors[1] := edVector2;
  bStart := True;
  mMatrix2.Click;                                          // new matrix input
end;

{ Menu item "Matrix > New": New matrix input }

procedure TfEigenvalues.mMatrix2Click(Sender: TObject);

begin
  iSize := 2;
  FormReset(iSize);                                        // reset form components
  // SetFocus cannot be used before the form craetion is terminated!
  if bStart then
    bStart := False
  else
    edMatrix[0, 0].SetFocus;
end;

{ Menu item "Matrix > Exit": Exit application }

procedure TfEigenvalues.mMatrixExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfEigenvalues.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics:' + LineEnding;
  S += 'Calculation of (real) eigenvalues and eigenvectors of a 2x2 matrix.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2022 - January 2024.';
  MessageDlg('About "Eigenvalues"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculation" pushed: Calculate eigenvalues and eigenvectors }

procedure TfEigenvalues.btCalcClick(Sender: TObject);

var
  I, J, N: Integer;
  A, B, C: Real;
  Mess: string;

begin
  ReadMatrix(iSize, aMatrix2, Mess);                       // read matrix values from form
  if Mess = '' then begin
    for I := 0 to iSize - 1 do begin
      edEigenvalues[I].Text  := '';
      edEigenvectors[I].Text := '';
    end;
    // Do the calculations
    SetLength(aValues, 2);
    A := 1;
    B := -(aMatrix2[0, 0] + aMatrix2[1, 1]);
    C := aMatrix2[0, 0] * aMatrix2[1, 1] - aMatrix2[0, 1] * aMatrix2[1, 0];
    SolveEquation2(A, B, C, aValues, Mess);                // solve 2nd degree equation
    if Mess = '' then begin
      // Real eigenvalues
      if aValues[0] = aValues[1] then
        N := 1
      else
        N := 2;
      SetLength(aVectors, N);
      for I := 0 to N - 1 do begin
        SetLength(aVectors[I], 2);
        aVectors[I][0] := 1;
        if aMatrix2[0, 1] = 0 then
          aVectors[I][1] := 0
        else
          aVectors[I][1] := (aValues[I] - aMatrix2[0, 0]) / aMatrix2[0, 1];
      end;
    end
    else begin
      // Complex eigenvalues
      edEigenvalues[0].Text := 'not real ';
      edEigenvalues[1].Text := 'not real ';
    end;
    // Display calculation results
    for I := 0 to N - 1 do begin
      edEigenvalues[I].Text := RFormat(aValues[I], 3);
      edEigenvectors[I].Text := '( ';
      for J := 0 to Length(aVectors[I]) - 1 do begin
        if J > 0 then
          edEigenvectors[I].Text := edEigenvectors[I].Text + '  ;  ';
        edEigenvectors[I].Text := edEigenvectors[I].Text + RFormat(aVectors[I][J], 3);
      end;
      edEigenvectors[I].Text := edEigenvectors[I].Text + ' )';
    end;
  end
  else begin
    // Invalid user input (missing values)
    MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
  end;
end;

end.

