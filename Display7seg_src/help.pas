{**********************************************}
{* Help text unit for Display7seg application *}
{**********************************************}

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
    StaticText1: TStaticText;
    edHelp: TMemo;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application start: Initializations }

procedure TfHelp.FormCreate(Sender: TObject);

const
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  SUB_3 = #$E2#$82#$83;

begin
  // Load help text from text file and apply subscripts to switch labels
  edHelp.Lines.LoadFromFile('help.txt');
  edHelp.Text := StringReplace(edHelp.Text, 'S1', 'S' + SUB_1, [rfReplaceAll]);
  edHelp.Text := StringReplace(edHelp.Text, 'S2', 'S' + SUB_2, [rfReplaceAll]);
  edHelp.Text := StringReplace(edHelp.Text, 'S3', 'S' + SUB_3, [rfReplaceAll]);
end;

{ Button "Close" pushed: Close the help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

