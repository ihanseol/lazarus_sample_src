{********************************************}
{* Help text unit for Bacteria1 application *}
{********************************************}

unit bacteria1_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfBacteria1H }
  TfBacteria1H = class(TForm)
    memoHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fBacteria1H: TfBacteria1H;

implementation

{$R *.lfm}

{**************}
{ TfBacteria1H }
{**************}

{ Button "Close": Close the window }

procedure TfBacteria1H.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

