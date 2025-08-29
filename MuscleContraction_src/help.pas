{****************************************************}
{* Help text unit for MuscleContraction application *}
{****************************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    StaticText1: TStaticText;
    edHelp: TMemo;
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

