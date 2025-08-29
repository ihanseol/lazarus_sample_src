{*************************************************}
{* Help text display unit for Matrix application *}
{*************************************************}

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

{ Application start: Load help text from text file }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  edHelp.Lines.LoadFromFile('help.txt');
end;

{ Button "Close" pushed: Close the help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

