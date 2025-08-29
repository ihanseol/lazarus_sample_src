{**********************************************}
{* Periodic table unit for Decay1 application *}
{**********************************************}

unit decay1_elements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

type
  {************}
  { TfElements }
  {************}
  TfElements = class(TForm)
    sgElements1: TStringGrid;
    sgElements2: TStringGrid;
    sgElements3: TStringGrid;
    sgElements4: TStringGrid;
    sgElements5: TStringGrid;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    sgElements: array[0..4] of TStringGrid;
  end;

var
  fElements: TfElements;

implementation

{$R *.lfm}

{************}
{ TfElements }
{************}

{ Application start: Create table of stringgrids (the 5 partial tables) }

procedure TfElements.FormCreate(Sender: TObject);

begin
  sgElements[0] := sgElements1; sgElements[1] := sgElements2; sgElements[2] := sgElements3;
  sgElements[3] := sgElements4; sgElements[4] := sgElements5;
end;

{ Button "Close": Close periodic table window }

procedure TfElements.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

