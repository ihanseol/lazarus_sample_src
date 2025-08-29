{*******************************************************}
{* Display help text unit for FiveDiceGrid application *}
{*******************************************************}

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

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application start: Load help text from file }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  edHelp.Lines.LoadFromFile('help.txt');
end;

{ Button "Close": Close "Help" window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

