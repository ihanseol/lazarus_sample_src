{***************************************************}
{* Nucleotide codes unit for DNABasics application *}
{***************************************************}

unit bases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type
  {*********}
  { TfBases }
  {*********}
  TfBases = class(TForm)
    sgTable: TStringGrid;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fBases: TfBases;

implementation

{$R *.lfm}

{*********}
{ TfBases }
{*********}

{ Button "Close" pushed: Close "Nucleotide codes" window }

procedure TfBases.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

