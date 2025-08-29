{*****************************************************}
{* Display graph unit for LotkaVolterra2 application *}
{*****************************************************}

unit lv2_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {********}
  { TfLV2G }
  {********}
  TfLV2G = class(TForm)
    stTitle, stLV: TStaticText;
    imGraph: TImage;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom: Integer;
    Bitmap: TBitmap;
  end;

var
  fLV2G: TfLV2G;

implementation

{ The drawing of the graph is done by the main unit }

{$R *.lfm}

{********}
{ TfLV2G }
{********}

{ Application start: Initialisation }

procedure TfLV2G.FormCreate(Sender: TObject);

begin
  // Define the drawing surface
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

{ Button "Close": Close the graph window }

procedure TfLV2G.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

