{*******************************************}
{* Help text unit of Solutions application *}
{*******************************************}

unit help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    Label1: TLabel;
    edHelp: TMemo;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

const
  // Subscripts for propper chemical formulas (also used by main unit)
  SUB_Digits: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84,
    #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application start: Apply subscripts to help text }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  edHelp.Text := StringReplace(edHelp.Text, 'H2O', 'H' + SUB_Digits[2] + 'O', [rfReplaceAll]);
  edHelp.Text := StringReplace(edHelp.Text, 'I2', 'I' + SUB_Digits[2], [rfReplaceAll]);
  edHelp.Text := StringReplace(edHelp.Text, 'H3PO4', 'H' + SUB_Digits[3] + 'PO' + SUB_Digits[4], [rfReplaceAll]);
  edHelp.Text := StringReplace(edHelp.Text, 'Hi', 'Hᵢ', []);
  edHelp.Text := StringReplace(edHelp.Text, '(OH)i', '(OH)ᵢ', []);
  edHelp.Text := StringReplace(edHelp.Text, '(COOH)i', '(COOH)ᵢ', []);
end;

{ Button "Close" pushed: Close help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

