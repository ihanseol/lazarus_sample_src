{**************************************************}
{* Currency download unit for Devises application *}
{**************************************************}

unit devises_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {************}
  { TfDownload }
  {************}
  TfDownload = class(TForm)
    Label1: TLabel;
    memoCmd: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fDownload: TfDownload;

implementation

{ All currency download related code in main unit! }

{$R *.lfm}

{************}
{ TfDownload }
{************}

{ Button "Fermer": Close Download ("console") window }

procedure TfDownload.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

