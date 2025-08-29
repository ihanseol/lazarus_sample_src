program printing1;

uses
  Interfaces, Printers, OsPrinters, printer4lazarus;

begin
  Printer.BeginDoc;
  Printer.Canvas.Font.Name := 'Courier New';
  Printer.Canvas.Font.Size := 20;
  Printer.Canvas.Font.Color := $ff0000;
  Printer.Canvas.TextOut(100, 100, 'Hello, World!');
  Printer.EndDoc;
end.

