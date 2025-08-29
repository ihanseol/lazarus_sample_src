{****************************************}
{* Graph unit for Bacteria1 application *}
{****************************************}

unit bacteria1_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TfBacteria1G }
  TfBacteria1G = class(TForm)
    stTitle: TStaticText;
    stMonod: TStaticText;
    imGraph: TImage;
    StaticText1: TStaticText;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom: Integer;
    Bitmap: TBitmap;
  end;

var
  fBacteria1G: TfBacteria1G;

implementation

{ The drawing of the graph is done by the main unit }

{$R *.lfm}

{**************}
{ TfBacteria1G }
{**************}

{ Application start: Create the drawing surface }

procedure TfBacteria1G.FormCreate(Sender: TObject);

begin
  // Graph area
  iImageWidth := imGraph.Width; iImageHeight := imGraph.Height;
  iGraphLeft  := 90; iGraphRight  := iImageWidth - 40;
  iGraphTop   := 60; iGraphBottom := iImageHeight - 40;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imGraph.Picture.Graphic := Bitmap;
end;

{ Button "Close": Close the window }

procedure TfBacteria1G.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

