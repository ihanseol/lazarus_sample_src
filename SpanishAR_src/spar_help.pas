{********************************************}
{* Help text unit for SpanishAR application *}
{********************************************}

unit spar_help;

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

{ Button "Close": Close the help text window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

