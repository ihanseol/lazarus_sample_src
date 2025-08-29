{********************************************}
{* Help text unit for Bacteria2 application *}
{********************************************}

unit bacteria2_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfBacteria2H }
  TfBacteria2H = class(TForm)
    memoHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fBacteria2H: TfBacteria2H;

implementation

{$R *.lfm}

{**************}
{ TfBacteria2H }
{**************}

{ Button "Close": Close the window }

procedure TfBacteria2H.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

