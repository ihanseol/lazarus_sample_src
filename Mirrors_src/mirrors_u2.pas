{********************************************}
{* Help text unit for "Mirrors" application *}
{********************************************}

unit mirrors_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfHelp }
  TfHelp = class(TForm)
    btClose: TButton;
    Memo1: TMemo;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Button "Close": Close the window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

