{*****************************************}
{* Help text unit for GRgods application *}
{*****************************************}

unit grgods_help;

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
    Memo1: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Button "Close" pushed: Close the help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

