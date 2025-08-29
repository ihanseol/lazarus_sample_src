program printing6;

uses
  Interfaces, Printers, OsPrinters, printer4lazarus;

const
  Title   = 'Free Pascal printing example.';

var
  XPos, YPos, TitleLength, XCenter, YCenter: Integer;
  PaperSize: TPaperSize;

begin
  Printer.BeginDoc;
  PaperSize := Printer.PaperSize;
  Printer.Canvas.Font.Name  := 'Arial Black';
  Printer.Canvas.Font.Size  := 18;
  Printer.Canvas.Font.Color := $000000;
  TitleLength := Printer.Canvas.TextWidth(Title);
  XPos := (PaperSize.Width - TitleLength) div 2;
  YPos := 250;
  Printer.Canvas.TextOut(XPos, YPos, Title);
  XCenter := PaperSize.Width div 2;
  YCenter := PaperSize.Height div 2;
  Printer.Canvas.Brush.Color := $ff901e;
  Printer.Canvas.FillRect(XCenter - 850, YCenter - 850, XCenter - 50, YCenter - 50);
  Printer.Canvas.Brush.Color := $9afa00;
  Printer.Canvas.FillRect(XCenter + 50, YCenter - 850, XCenter + 850, YCenter - 50);
  Printer.Canvas.Brush.Color := $00a5ff;
  Printer.Canvas.FillRect(XCenter - 850, YCenter + 50, XCenter - 50, YCenter + 850);
  Printer.Canvas.Brush.Color := $b469ff;
  Printer.Canvas.FillRect(XCenter + 50, YCenter + 50, XCenter + 850, YCenter + 850);
  Printer.EndDoc;
end.

