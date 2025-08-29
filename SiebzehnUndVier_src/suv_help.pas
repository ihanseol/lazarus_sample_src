{**************************************************************}
{* Application help text unit for SiebzehnUndVier application *}
{**************************************************************}

unit suv_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    StaticText1: TStaticText;
    Memo1: TMemo;
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

{ Button "Schließen": Close the help window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

