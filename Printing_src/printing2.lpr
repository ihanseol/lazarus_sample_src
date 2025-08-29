program printing2;

uses
  Interfaces, Printers, OsPrinters, printer4lazarus;

const
  HMargin = 100;
  VMargin = 100;

var
  YPos: Integer;
  Line: string;
  InFile: Text;

begin
  Printer.BeginDoc;
  Printer.Canvas.Font.Name  := 'Courier New';
  Printer.Canvas.Font.Size  := 12;
  Printer.Canvas.Font.Color := $000000;
  Assign(InFile, 'printing1.lpr'); Reset(InFile);
  YPos := VMargin;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Printer.Canvas.TextOut(HMargin, YPos, Line);
      YPos += Round(1.2 * Abs(Printer.Canvas.TextHeight('I')));
    end;
  end;
  Close(InFile);
  Printer.EndDoc;
end.

