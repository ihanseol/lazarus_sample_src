{**********************************************}
{ Help text unit for RGBColorMixer application }
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
    Memo1: TMemo;
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
  Memo1.Lines.LoadFromFile('help.txt');
end;

{ Button "Close": Close help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

