{*****************************************}
{* Help text unit for Musica application *}
{*****************************************}

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
    Memo1: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Button "Close": Close the help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Application start: Apply subscripts to help text }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  Memo1.Text := StringReplace(Memo1.Text, 'C4', 'C' + #$E2#$82#$84, [rfReplaceAll]);
  Memo1.Text := StringReplace(Memo1.Text, 'B7', 'B' + #$E2#$82#$87, [rfReplaceAll]);
end;

end.

