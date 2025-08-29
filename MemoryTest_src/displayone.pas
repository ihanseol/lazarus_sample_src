{**********************************************}
{* MemoryTest display objects one by one unit *}
{**********************************************}

unit displayone;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TfOne }
  TfOne = class(TForm)
    stObject: TStaticText;
    imObject: TImage;
    btCancel: TButton;
    edCancel: TEdit;
    procedure btCancelClick(Sender: TObject);
  end;

var
  fOne: TfOne;

implementation

// This unit actually "does nothing". It defines the form for the objects
// one by one display, the display itself being coded in the main unit.

{$R *.lfm}

{*********}
{* TfOne *}
{*********}

{ Button "Cancel": Close the form }

procedure TfOne.btCancelClick(Sender: TObject);

begin
  edCancel.Text := 'C';                                                        // set this to tell the main unit that the user pushed "Cancel"
  Close;
end;

end.

