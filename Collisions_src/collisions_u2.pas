{*****************************************************}
{* Help text display unit for Collisions application *}
{*****************************************************}

unit collisions_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    edHelp: TMemo;
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

{ Button "Close": Close Help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

