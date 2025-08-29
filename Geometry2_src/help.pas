{*******************************************}
{* Help unit for the Geometry2 application *}
{*******************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfHelp }
  TfHelp = class(TForm)
    HelpText: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{**********}
{* TfHelp *}
{**********}

{ Button "Close": Close help text }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

