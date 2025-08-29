{*******************************************}
{*    Electrical circuits:  RC circuits    *}
{*-----------------------------------------*}
{* Help unit for Condensateurs application *}
{*******************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TFormHelp }
  TFormHelp = class(TForm)
    Title: TStaticText;
    HelpText: TMemo;
    ButtonClose: TButton;
    procedure ButtonCloseClick(Sender: TObject);
  end;

var
  FormHelp: TFormHelp;

implementation

{$R *.lfm}

{ TFormHelp }

{ Button "Fermer": Close the help text form }

procedure TFormHelp.ButtonCloseClick(Sender: TObject);

begin
  Close;
end;

end.

