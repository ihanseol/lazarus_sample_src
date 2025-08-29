{***********************************************************}
{* Draw planet orbits unit for PlanetaryOrbits application *}
{***********************************************************}

unit orbits_draw;

// All code in the main unit

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  {********}
  { TfDraw }
  {********}
  TfDraw = class(TForm)
    imOrbits: TImage;
    edStep: TEdit;
    imMercury, imVenus, imMars, imEarth, imJupiter: TImage;
    imSaturn, imUranus, imNeptune, imPluto: TImage;
    shSun5, shSun10: TShape;
    shMercury, shVenus, shEarth, shMars, shJupiter: TShape;
    shSaturn, shUranus, shNeptune, shPluto: TShape;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    iImageWidth, iImageHeight: Cardinal;
    Bitmap : TBitmap;
  public
    shPlanets10: array[1..9] of TShape;
    imPlanets: array[1..9] of TImage;
  end;

var
  fDraw: TfDraw;

implementation

{$R *.lfm}

{********}
{ TfDraw }
{********}

{ Application start: Initialisation }

procedure TfDraw.FormCreate(Sender: TObject);

begin
  // Create array with planet shapes as elements
  shPlanets10[1] := shMercury; shPlanets10[2] := shVenus;   shPlanets10[3] := shEarth;
  shPlanets10[4] := shMars;    shPlanets10[5] := shJupiter; shPlanets10[6] := shSaturn;
  shPlanets10[7] := shUranus;  shPlanets10[8] := shNeptune; shPlanets10[9] := shPluto;
  // Create array with planet images as elements
  imPlanets[1] := imMercury; imPlanets[2] := imVenus;   imPlanets[3] := imEarth;
  imPlanets[4] := imMars;    imPlanets[5] := imJupiter; imPlanets[6] := imSaturn;
  imPlanets[7] := imUranus;  imPlanets[8] := imNeptune; imPlanets[9] := imPluto;
  iImageWidth := imOrbits.Width; iImageHeight := imOrbits.Height;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the drawing surface
  imOrbits.Picture.Graphic := Bitmap;
  // Paint the drawing surface all black
  fDraw.imOrbits.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  fDraw.imOrbits.Picture.Bitmap.Canvas.Clear;
end;

{ Button "Close": Close the orbits display window }

procedure TfDraw.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

