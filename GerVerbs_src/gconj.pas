{*****************************************************}
{* Display conjugation unit for GerVerbs application *}
{*****************************************************}

unit gconj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  {********}
  { TfConj }
  {********}
  TfConj = class(TForm)
    stTitle: TStaticText;
    StaticText1, StaticText2, StaticText3: TStaticText;
    sgConjPT: TStringGrid;
    sgConjSP: TStringGrid;
    sgConjPF: TStringGrid;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fConj: TfConj;

implementation

{$R *.lfm}

{ All "display conjugation" code in the main unit, that fills in the string-grid }

{********}
{ TfConj }
{********}

{ Button "Schließen": Close the window }

procedure TfConj.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

