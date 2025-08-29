{************************************************}
{* Reactants list unit for AcidBase application *}
{************************************************}

unit acidbase_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids;

type
  {********}
  { TfList }
  {********}
  TfList = class(TForm)
    Label1, Label2: TLabel;
    sgAcids: TStringGrid;
    sgBases: TStringGrid;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fList: TfList;

implementation

{$R *.lfm}

{********}
{ TfList }
{********}

{ Button "Close": Close molecules list window }

procedure TfList.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

