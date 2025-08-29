{*********************************************}
{* Help text unit for Capacitors application *}
{*********************************************}

unit capacitors_u4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    StaticText1: TStaticText;
    Memo1: TMemo;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

const
  SUB_Digits: array[0..3] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83
  );
  SUP_Digits: array[1..2] of string = (
    #$C2#$B9, #$C2#$B2
  );
  SUP_Minus = #$E2#$81#$BB;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

procedure TfHelp.FormCreate(Sender: TObject);

begin
  // Apply subscripts to help text
  Memo1.Text := StringReplace(Memo1.Text, 'ε0', 'ε' + SUB_Digits[0], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'r1', 'r' + SUB_Digits[1], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'r2', 'r' + SUB_Digits[2], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'C1', 'C' + SUB_Digits[1], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'C2', 'C' + SUB_Digits[2], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'C3', 'C' + SUB_Digits[3], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'V0', 'V' + SUB_Digits[0], [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, '10-12', '10' + SUP_Minus + SUP_Digits[1] + SUP_Digits[2], []);
end;

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

