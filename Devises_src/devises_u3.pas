{******************************************}
{* Help text unit for Devises application *}
{******************************************}

unit devises_u3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    memoHelp: TMemo;
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

{ Button "Fermer": Close Help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

