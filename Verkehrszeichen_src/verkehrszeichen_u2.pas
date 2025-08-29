{**************************************************}
{* Help text unit for Verkehrszeichen application *}
{**************************************************}

unit verkehrszeichen_u2;

{$mode objfpc}{$H+}

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

{ Button "Schließen": Close help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

