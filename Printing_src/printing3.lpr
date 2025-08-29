program printing3;

uses
  Interfaces, Printers, OsPrinters, printer4lazarus;

var
  PaperSize: TPaperSize;

begin
  PaperSize := Printer.PaperSize;
  Writeln('Paper size:  Width  = ', PaperSize.Width:5);
  Writeln('             Height = ', PaperSize.Height:5);
end.

