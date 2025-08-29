program printing8;

uses
  Classes, Controls, Graphics, ExtCtrls, Interfaces, Printers, OsPrinters, printer4lazarus;

const
  Title = 'Free Pascal printing example.';

var
  XPos, YPos, TitleLength, XCenter, YCenter, WPic, HPic, WSPic, HSPic: Integer;
  Scale: Real;
  imPic: TImage;
  PicRect: TRect;
  PaperSize: TPaperSize;

begin
  imPic := TImage.Create(imPic);
  imPic.Picture.LoadFromFile('castle.jpg');
  WPic := imPic.Picture.Graphic.Width;
  HPic := imPic.Picture.Graphic.Height;
  WSPic := 800; Scale := WSPic / WPic;
  HSPic := Round(HPic * Scale);
  Printer.BeginDoc;
  PaperSize := Printer.PaperSize;
  XCenter := PaperSize.Width div 2;
  YCenter := PaperSize.Height div 2;
  Printer.Canvas.Font.Name  := 'Arial Black';
  Printer.Canvas.Font.Size  := 18;
  Printer.Canvas.Font.Color := $000000;
  TitleLength := Printer.Canvas.TextWidth(Title);
  XPos := (PaperSize.Width - TitleLength) div 2;
  YPos := 250;
  Printer.Canvas.TextOut(XPos, YPos, Title);
  PicRect := Rect(XCenter - WSPic - 100, YCenter - HSPic div 2, XCenter - 100, YCenter + HSPic div 2);
  Printer.Canvas.StretchDraw(PicRect, imPic.Picture.Graphic);
  PicRect := Rect(XCenter + 100, YCenter - HSPic div 2, XCenter + 100 + WSPic, YCenter + HSPic div 2);
  Printer.Canvas.StretchDraw(PicRect, imPic.Picture.Graphic);
  Printer.EndDoc;
end.

