{**********************************************}
{* Common unit for BridgeCircuits application *}
{**********************************************}

unit common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls;

const
  SUP_2 = #$C2#$B2;
  SUB_Digits: array[1..4] of string = (
    #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84
  );

// Public functions and procedures

function RFormat(R: Real; F: Integer): string;
function ApplySubscripts(S: string): string;
procedure GetData(var EdtData: array of TEdit; N: Integer; FirstEmpty: Boolean; var Data: array of Real; out Mess: string);

implementation

{ Get nth power of real number }

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

{ Format real number (with F decimal digits, without displaying unsignificant zeros) }

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
        SR := FloatToStrF(R, ffExponent, F, 0)
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Apply subscripts (1 - 4) for R, C, L and Z }

function ApplySubscripts(S: string): string;

var
  I: Integer;

begin
  for I := 1 to 4 do begin
    S := StringReplace(S, 'R' + IntToStr(I), 'R' + SUB_Digits[I], [rfReplaceAll]);
    S := StringReplace(S, 'C' + IntToStr(I), 'C' + SUB_Digits[I], [rfReplaceAll]);
    S := StringReplace(S, 'L' + IntToStr(I), 'L' + SUB_Digits[I], [rfReplaceAll]);
    S := StringReplace(S, 'Z' + IntToStr(I), 'Z' + SUB_Digits[I], [rfReplaceAll]);
  end;
  Result := S;
end;

{ Read user input data }

procedure GetData(var EdtData: array of TEdit; N: Integer; FirstEmpty: Boolean; var Data: array of Real; out Mess: string);

// Input data is passed to the routine as an array of edit fields

var
  I: Integer;

begin
  Mess := '';
  for I := 0 to N - 1 do begin
    if Mess = '' then begin
      if ((I = 0) and FirstEmpty and (EdtData[I].Text = '')) or ((EdtData[I].Text <> '')) then begin
        // The first input field may be left empty in some cases (FirstEmpty = True)
        if EdtData[I].Text <> '' then begin
          Data[I] := StrToFloat(EdtData[I].Text);
          if Data[I] <= 0 then begin
            Mess := 'Parameter value must be positive';
            EdtData[I].SetFocus;
          end;
        end;
      end
      else begin
        Mess := 'Missing parameter value';
        EdtData[I].SetFocus;
      end;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Input error', Mess + '!', mtError, [mbOK], 0);
end;

end.

