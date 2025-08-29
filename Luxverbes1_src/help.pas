{*******************************************************}
{* "Display help text" unit for Luxverbes1 application *}
{*******************************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {**********}
  {* TfHelp *}
  {**********}
  TfHelp = class(TForm)
    Memo1: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{**********}
{* TfHelp *}
{**********}

{ Button "Zoumaachen": Close the window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

