{**********************************************************}
{* Unité "afficher aide" pour l'application ConjugaisonER *}
{**********************************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfHelp }
  TfHelp = class(TForm)
    stTitle: TStaticText;
    btClose: TButton;
    memoHelp: TMemo;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{**********}
{* TfHelp *}
{**********}

{ Button "Fermer": Close the help text form }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

