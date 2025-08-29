{***************************************************************}
{* Chemical composition display unit for MolWeight application *}
{***************************************************************}

unit comp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  { TfComposition }
  TfComposition = class(TForm)
    stTitle: TStaticText;
    grComposition: TStringGrid;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fComposition: TfComposition;

implementation

{$R *.lfm}

{*****************}
{* TfComposition *}
{*****************}

{ Button "Close": Close chemical composition display window }

procedure TfComposition.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

