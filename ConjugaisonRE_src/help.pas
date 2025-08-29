{**********************************************************}
{* Unité "afficher aide" pour l'application ConjugaisonRE *}
{**********************************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    stTitle: TStaticText;
    memoHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Button "Fermer": Close the help text form }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

