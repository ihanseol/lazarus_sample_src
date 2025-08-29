{***************************************************}
{* Help text display unit for AcidBase application *}
{***************************************************}

unit acidbase_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    Memo1: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

const
  SUB_Digits: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84, #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application start: Apply subscripts to formulas in help text }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  Memo1.Text := StringReplace(Memo1.Text, 'Na3PO4', 'Na' + SUB_Digits[3] + 'PO' + SUB_Digits[4], []);
  Memo1.Text := StringReplace(Memo1.Text, 'Na2HPO4', 'Na' + SUB_Digits[2] + 'HPO' + SUB_Digits[4], []);
  Memo1.Text := StringReplace(Memo1.Text, 'NaH2PO4', 'NaH' + SUB_Digits[2] + 'PO' + SUB_Digits[4], []);
end;

{ Button "Close": Close help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

