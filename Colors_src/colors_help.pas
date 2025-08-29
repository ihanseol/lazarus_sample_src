{*****************************************}
{* Help text unit for Colors application *}
{*****************************************}

unit colors_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfHelp }

  TfHelp = class(TForm)
    btClose: TButton;
    laTitle: TLabel;
    edHelp: TMemo;
    procedure btCloseClick(Sender: TObject);
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

end.

