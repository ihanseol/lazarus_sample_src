//
// Arithmetic3: "Die Kinder zählen die Autos" (help form unit)
//

unit arithmetics3_help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TFormHelp }
  TFormHelp = class(TForm)
    ButtonClose: TButton;
    HelpText: TMemo;
    procedure ButtonCloseClick(Sender: TObject);
  end;

var
  FormHelp: TFormHelp;

implementation

{$R *.lfm}

{ TFormHelp }

{ Button "Close": Close the help form}

procedure TFormHelp.ButtonCloseClick(Sender: TObject);

begin
  Close;
end;

end.

