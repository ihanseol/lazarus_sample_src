{****************************************}
{* Graph unit for Bacteria3 application *}
{****************************************}

unit bacteria3_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  { TfBacteria3G }
  TfBacteria3G = class(TForm)
    stTitle, stChemostat, stMonod: TStaticText;
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
  fBacteria3G: TfBacteria3G;

implementation

{ The drawing of the graph is done by the main unit }

{$R *.lfm}

{**************}
{ TfBacteria3G }
{**************}

{ Application start: Create the drawing surface }

procedure TfBacteria3G.FormCreate(Sender: TObject);

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

procedure TfBacteria3G.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

