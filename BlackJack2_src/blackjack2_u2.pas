{*********************************************}
{* Help text unit for BlackJack2 application *}
{*********************************************}

unit blackjack2_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    HelpText: TMemo;
    ButtonClose: TButton;
    procedure ButtonCloseClick(Sender: TObject);
  end;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Button "Close": Close the help text window }

procedure TfHelp.ButtonCloseClick(Sender: TObject);

begin
  Close;
end;

end.

