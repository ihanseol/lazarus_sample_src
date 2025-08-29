//
// Help text unit for WGuess application
//

unit wguess_u2;

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

{ THelpForm }

//
// Button "Close": Close the Help text form
//

procedure THelpForm.ButtonCloseClick(Sender: TObject);

begin
  Close();
end;

end.

