{*********************************************}
{* Graph unit for LotkaVolterra1 application *}
{*********************************************}

unit lv1_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {********}
  { TfLV1G }
  {********}
  TfLV1G = class(TForm)
    stTitle: TStaticText;
    stLV: TStaticText;
    imGraph: TImage;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom: Integer;
    Bitmap: TBitmap;
  end;

var
  fLV1G: TfLV1G;

implementation

{ The drawing of the graph is done by the main unit }

{$R *.lfm}

{********}
{ TfLV1G }
{********}

{ Application start: Create the drawing surface }

procedure TfLV1G.FormCreate(Sender: TObject);

begin
  // Graph area
  iImageWidth := imGraph.Width; iImageHeight := imGraph.Height;
  iGraphLeft  := 70; iGraphRight  := iImageWidth - 40;
  iGraphTop   := 60; iGraphBottom := iImageHeight - 40;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imGraph.Picture.Graphic := Bitmap;
end;

{ Button "Close": Close the window }

procedure TfLV1G.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

