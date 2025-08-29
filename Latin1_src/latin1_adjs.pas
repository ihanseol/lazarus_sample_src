{*****************************************************************}
{* Adjective declensions display unit for the Latin1 application *}
{*****************************************************************}

unit latin1_adjs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  { TfAdjs }
  TfAdjs = class(TForm)
    btClose: TButton;
    sgDecelension: TStringGrid;
    stTitle: TStaticText;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fAdjs: TfAdjs;

implementation

{$R *.lfm}

{********}
{ TfAdjs }
{********}

{ Button "Close": Close the adjective declensions window }

procedure TfAdjs.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

