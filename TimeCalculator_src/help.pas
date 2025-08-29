{ ********************************************** }
{ Help text unit for Time Calculator application }
{ ********************************************** }

unit help;

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

{ --------- }
{ THelpForm }
{ --------- }

// Button "Close": Close Help Windoe

procedure THelpForm.ButtonCloseClick(Sender: TObject);

begin
  Close();
end;

end.

