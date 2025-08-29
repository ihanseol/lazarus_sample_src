{**************************************}
{* Help unit for TempHeat application *}
{**************************************}

unit tempheat_help;

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
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

const
  SUP_MINUS = #$E2#$81#$BB;
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82;
  SUP_2 = #$C2#$B2; SUP_4 = #$E2#$81#$B4; SUP_6 = #$E2#$81#$B6;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application start: Apply sub- and superscripts to help text }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  Memo1.Text := StringReplace(Memo1.Text, 'T14', 'T' + SUB_1 + SUP_4, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'T24', 'T' + SUB_2 + SUP_4, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'T1', 'T' + SUB_1, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'T2', 'T' + SUB_2, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'm2', 'm' + SUP_2, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'K4', 'K' + SUP_2, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, '10-6', '10' + SUP_Minus + SUP_6, [rfReplaceAll]);
end;

{ Button "Close": Close the help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

