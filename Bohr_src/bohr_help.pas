{*******************************************}
{* Help text unit for the Bohr application *}
{*******************************************}

unit bohr_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfHelp }

  TfHelp = class(TForm)
    btClose: TButton;
    Memo1: TMemo;
    StaticText1: TStaticText;
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{ TfHelp }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

procedure TfHelp.FormCreate(Sender: TObject);

begin
  Memo1.Lines.LoadFromFile('help.txt');
end;

end.

