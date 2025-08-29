{************************************************************}
{* Chemostat growth analysis unit for Bacteria3 application *}
{************************************************************}

unit bacteria3_analysis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  { TfBacteria3A }
  TfBacteria3A = class(TForm)
    StaticText1: TStaticText;
    sgAnalysis: TStringGrid;
    stEquilibrium: TStaticText;
    btClose: TButton;
    Label1: TLabel;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fBacteria3A: TfBacteria3A;

implementation

{ The fill-in of the grid is done by the main unit }

{$R *.lfm}

{**************}
{ TfBacteria3A }
{**************}

{ Button "Close": Close the analysis window }

procedure TfBacteria3A.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

