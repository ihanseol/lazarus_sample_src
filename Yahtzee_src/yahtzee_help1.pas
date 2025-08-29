{**************************************************}
{* Scoring help text unit for Yahtzee application *}
{**************************************************}

unit yahtzee_help1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  {***********}
  { TfScoring }
  {***********}
  TfScoring = class(TForm)
    StaticText1, StaticText2, StaticText3, StaticText4: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    sgUpper: TStringGrid;
    sgLower: TStringGrid;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fScoring: TfScoring;

implementation

{$R *.lfm}

{***********}
{ TfScoring }
{***********}

{ Button "Close": Close help window }

procedure TfScoring.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

