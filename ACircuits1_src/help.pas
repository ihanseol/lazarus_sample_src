{****************************************}
{*      Electronics:  RLC circuits      *}
{*--------------------------------------*}
{* Simple physics problems generator    *}
{* Help unit for ACircuits1 application *}
{****************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    HelpText: TMemo;
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

{ Button "Close": Close the help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

