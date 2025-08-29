{********************************************}
{* Help text unit for the Beats application *}
{********************************************}

unit beats_help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

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
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application startup: Apply subscripts to help text }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  Memo1.Text := StringReplace(Memo1.Text, 'f1', 'f' + SUB_1, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'f2', 'f' + SUB_2, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'y1', 'y' + SUB_1, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'y2', 'y' + SUB_2, [rfReplaceAll]);
end;

{ Button "Close" pushed: Close Help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

