{******************************************}
{* Help text unit for WGuess2 application *}
{******************************************}

unit wguess2_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { THelpForm }
  THelpForm = class(TForm)
    HelpText: TMemo;
    ButtonClose: TButton;
    procedure ButtonCloseClick(Sender: TObject);
  end;

var
  HelpForm: THelpForm;

implementation

{$R *.lfm}

{***********}
{ THelpForm }
{***********}

{ Button "Close": Close the help text window }

procedure THelpForm.ButtonCloseClick(Sender: TObject);

begin
  Close;
end;

end.

