{*********************************************************}
{* Display help text unit for LotkaVolterra2 application *}
{*********************************************************}

unit lv2_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfLV2H }
  {********}
  TfLV2H = class(TForm)
    memoHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fLV2H: TfLV2H;

implementation

{$R *.lfm}

{********}
{ TfLV2H }
{********}

{ Button "Close": Close the help window }

procedure TfLV2H.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

