{***************************************************}
{* MemoryTest display objects all together unit    *}
{* Unit also used during user memory objects check *}
{***************************************************}

unit displayall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TfAll }
  TfAll = class(TForm)
    Image1:  TImage;  Image2:  TImage;  Image3:  TImage;  Image4:  TImage;  Image5:  TImage;
    Image6:  TImage;  Image7:  TImage;  Image8:  TImage;  Image9:  TImage;  Image10: TImage;
    Image11: TImage;  Image12: TImage;  Image13: TImage;  Image14: TImage;  Image15: TImage;
    Image16: TImage;  Image17: TImage;  Image18: TImage;  Image19: TImage;  Image20: TImage;
    Image21: TImage;  Image22: TImage;  Image23: TImage;  Image24: TImage;  Image25: TImage;
    Image26: TImage;  Image27: TImage;  Image28: TImage;  Image29: TImage;  Image30: TImage;
    Image31: TImage;  Image32: TImage;  Image33: TImage;  Image34: TImage;  Image35: TImage;
    Image36: TImage;  Image37: TImage;  Image38: TImage;  Image39: TImage;  Image40: TImage;
    Image41: TImage;  Image42: TImage;  Image43: TImage;  Image44: TImage;  Image45: TImage;
    Image46: TImage;  Image47: TImage;  Image48: TImage;  Image49: TImage;  Image50: TImage;
    Label1: TLabel;
    edTime: TEdit;
    btOK: TButton;
    btCancel: TButton;
    edButton: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  public
    ObjImg: array[1..50] of TImage;                                            // objects are displayed by the main unit setting this public variable
  end;

var
  fAll: TfAll;

implementation

// This unit actually "does nothing". It defines the form for the objects
// all together display, the display itself being coded in the main unit.

{$R *.lfm}

{*********}
{* TfAll *}
{*********}

{ Form creation: create public array with the form's images }

procedure TfAll.FormCreate(Sender: TObject);

begin
  ObjImg[1]  := Image1;  ObjImg[2]  := Image2;  ObjImg[3]  := Image3;  ObjImg[4]  := Image4;  ObjImg[5]  := Image5;
  ObjImg[6]  := Image6;  ObjImg[7]  := Image7;  ObjImg[8]  := Image8;  ObjImg[9]  := Image9;  ObjImg[10] := Image10;
  ObjImg[11] := Image11; ObjImg[12] := Image12; ObjImg[13] := Image13; ObjImg[14] := Image14; ObjImg[15] := Image15;
  ObjImg[16] := Image16; ObjImg[17] := Image17; ObjImg[18] := Image18; ObjImg[19] := Image19; ObjImg[20] := Image20;
  ObjImg[21] := Image21; ObjImg[22] := Image22; ObjImg[23] := Image23; ObjImg[24] := Image24; ObjImg[25] := Image25;
  ObjImg[26] := Image26; ObjImg[27] := Image27; ObjImg[28] := Image28; ObjImg[29] := Image29; ObjImg[30] := Image30;
  ObjImg[31] := Image31; ObjImg[32] := Image32; ObjImg[33] := Image33; ObjImg[34] := Image34; ObjImg[35] := Image35;
  ObjImg[36] := Image36; ObjImg[37] := Image37; ObjImg[38] := Image38; ObjImg[39] := Image39; ObjImg[40] := Image40;
  ObjImg[41] := Image41; ObjImg[42] := Image42; ObjImg[43] := Image43; ObjImg[44] := Image44; ObjImg[45] := Image45;
  ObjImg[46] := Image46; ObjImg[47] := Image47; ObjImg[48] := Image48; ObjImg[49] := Image49; ObjImg[50] := Image50;
end;

{ Button "OK": Close the form }

procedure TfAll.btOKClick(Sender: TObject);

begin
  edButton.Text := 'ok';                                                       // set this to tell main unit that user pushed "OK"
  Close;
end;

{ Button "Cancel": Close the form }

procedure TfAll.btCancelClick(Sender: TObject);

begin
  edButton.Text := 'cancel';                                                   // set this to tell main unit that user pushed "Cancel"
  Close;
end;

end.

