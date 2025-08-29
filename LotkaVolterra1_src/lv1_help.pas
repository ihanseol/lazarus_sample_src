{*************************************************}
{* Help text unit for LotkaVolterra1 application *}
{*************************************************}

unit lv1_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfLV1H }
  {********}
  TfLV1H = class(TForm)
    memoHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fLV1H: TfLV1H;

implementation

{$R *.lfm}

{********}
{ TfLV1H }
{********}

{ Button "Close": Close the window }

procedure TfLV1H.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

