{****************************************************}
{* Flower selection unit for Blumenquiz application *}
{****************************************************}

unit flowers;

// All code in the main unit...

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {***********}
  { TfFlowers }
  {***********}
  TfFlowers = class(TForm)
    laQuestion: TLabel;
    imFlower1, imFlower2, imFlower3, imFlower4, imFlower5, imFlower6: TImage;
    cbFlower1, cbFlower2, cbFlower3, cbFlower4, cbFlower5, cbFlower6: TCheckBox;
    btAnswer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
  public
    imFlowers: array[0..5] of TImage;
    cbFlowers: array[0..5] of TCheckBox;
  end;

var
  fFlowers: TfFlowers;

implementation

{$R *.lfm}

{***********}
{ TfFlowers }
{***********}

{ Application start: Create arrays with images and checkboxes }

procedure TfFlowers.FormCreate(Sender: TObject);

begin
  imFlowers[0] := imFlower1; imFlowers[1] := imFlower2; imFlowers[2] := imFlower3;
  imFlowers[3] := imFlower4; imFlowers[4] := imFlower5; imFlowers[5] := imFlower6;
  cbFlowers[0] := cbFlower1; cbFlowers[1] := cbFlower2; cbFlowers[2] := cbFlower3;
  cbFlowers[3] := cbFlower4; cbFlowers[4] := cbFlower5; cbFlowers[5] := cbFlower6;
end;

{ Button "Antwort": Close the window}

procedure TfFlowers.btAnswerClick(Sender: TObject);

begin
  Close;
end;

end.

