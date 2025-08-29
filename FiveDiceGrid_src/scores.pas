{**********************************************************}
{* Display scores table unit for FiveDiceGrid application *}
{**********************************************************}

unit scores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  {**********}
  { TfScores }
  {**********}
  TfScores = class(TForm)
    Label1: TLabel;
    stScores: TStringGrid;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fScores: TfScores;

implementation

{$R *.lfm}

{**********}
{ TfScores }
{**********}

{ Button "Close": Close the 'Scores' window }

procedure TfScores.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

