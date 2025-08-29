{***********************************************************}
{* Noun declension display unit for the Latin1 application *}
{***********************************************************}

unit latin1_nouns;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  { TfNouns }
  TfNouns = class(TForm)
    btClose: TButton;
    stTitle: TStaticText;
    sgDecelension: TStringGrid;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fNouns: TfNouns;

implementation

{$R *.lfm}

{*********}
{ TfNouns }
{*********}

{ Button "Close": Close the noun declensions window }

procedure TfNouns.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

