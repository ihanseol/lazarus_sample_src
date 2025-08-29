{********************************************}
{* Help text unit for Bacteria3 application *}
{********************************************}

unit bacteria3_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfBacteria3H }
  TfBacteria3H = class(TForm)
    memoHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fBacteria3H: TfBacteria3H;

implementation

{$R *.lfm}

{**************}
{ TfBacteria3H }
{**************}

{ Button "Close": Close the help window }

procedure TfBacteria3H.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

