//
// Arithmetic3: "Die Kinder zählen die Autos" (user data entry unit)
//

unit arithmetics3_userdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TFormUserData }
  TFormUserData = class(TForm)
    Label1: TLabel;
    T2Questions: TEdit;
    ButtonOK: TButton;
    procedure ButtonOKClick(Sender: TObject);
  end;

var
  FormUserData: TFormUserData;

implementation

{$R *.lfm}

{ TFormUserData }

{ Button "OK": Close the form }

procedure TFormUserData.ButtonOKClick(Sender: TObject);

// The data entered on the form is read by arithmetics_main
// when this form is closed and the main form gets focus again

begin
  Close;
end;

end.

