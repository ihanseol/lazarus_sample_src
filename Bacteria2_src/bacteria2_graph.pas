{****************************************}
{* Graph unit for Bacteria2 application *}
{****************************************}

unit bacteria2_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TfBacteria2G }
  TfBacteria2G = class(TForm)
    imLegend: TImage;
    stTitle: TStaticText;
    imGraph: TImage;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom: Integer;
    Bitmap, Bitmap2: TBitmap;
  end;

var
  fBacteria2G: TfBacteria2G;

implementation

{ The drawing of the graph is done by the main unit }

{$R *.lfm}

{**************}
{ TfBacteria2G }
{**************}

{ Application start: Create the drawing surface and the legend area }

procedure TfBacteria2G.FormCreate(Sender: TObject);

begin
  // Graph area data
  iImageWidth := imGraph.Width; iImageHeight := imGraph.Height;
  iGraphLeft  := 90; iGraphRight  := iImageWidth - 40;
  iGraphTop   := 60; iGraphBottom := iImageHeight - 40;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imGraph.Picture.Graphic := Bitmap;
  // Create another bitmap object and assign dimensions
  Bitmap2 := TBitmap.Create;
  Bitmap.Width := imLegend.Width;
  Bitmap.Height := imLegend.Height;
  //Assign the bitmap to the image component (the legend area)
  imLegend.Picture.Graphic := Bitmap;
  // Set canvas color to form color
  imLegend.Picture.Bitmap.Canvas.Brush.Color := clForm;
  imLegend.Picture.Bitmap.Canvas.Rectangle(0, 0, Bitmap.Width, Bitmap.Height);
  // Display legend
  imLegend.Picture.Bitmap.Canvas.Font.Color := clRed;
  imLegend.Picture.Bitmap.Canvas.TextOut(20, 8, 'Bacteria');
  imLegend.Picture.Bitmap.Canvas.Font.Color := clBlue;
  imLegend.Picture.Bitmap.Canvas.TextOut(170, 8, 'Substrate 1');
  imLegend.Picture.Bitmap.Canvas.Font.Color := clGreen;
  imLegend.Picture.Bitmap.Canvas.TextOut(330, 8, 'Substrate 2');
end;

{ Button "Close": Close the window }

procedure TfBacteria2G.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

