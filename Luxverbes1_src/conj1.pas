{*********************************************************}
{* "Display conjugation" unit for Luxverbes1 application *}
{*********************************************************}

unit conj1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids;

type
  {**********}
  {* TfConj *}
  {**********}
  TfConj = class(TForm)
    stTitle: TStaticText;
    sgConj: TStringGrid;
    edVerbDetails: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fConj: TfConj;

implementation

{$R *.lfm}

{ All "display conjugation" code in main unit, that fills in the string-grid }

{**********}
{* TfConj *}
{**********}

{ Button "Zoumaachen": Close the window }

procedure TfConj.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

