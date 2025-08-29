{**********************************************}
{* Help text unit for Arithmetic8 application *}
{**********************************************}

unit arithmetic8_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
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

{ Button "Close": Close help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

