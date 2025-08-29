program printing4;

uses
  Interfaces, Printers, OsPrinters, printer4lazarus;

const
  Title   = 'Free Pascal printing example.';
  Author  = 'allu, May 2025.';
  HMargin = 100;
  VMargin = 100;

var
  LineHeight, XPos, YPos, TitleLength, AuthorLength: Integer;
  Line: string;
  PaperSize: TPaperSize;
  InFile: Text;

begin
  Printer.BeginDoc;
  PaperSize := Printer.PaperSize;
  Printer.Canvas.Font.Name  := 'Courier New';
  Printer.Canvas.Font.Size  := 12;
  Printer.Canvas.Font.Color := $000000;
  LineHeight := Round(1.2 * Abs(Printer.Canvas.TextHeight('I')));
  TitleLength := Printer.Canvas.TextWidth(Title);
  XPos := (PaperSize.Width - TitleLength) div 2;
  YPos := VMargin;
  Printer.Canvas.TextOut(XPos, YPos, Title);
  XPos := HMargin;
  YPos += 2 * LineHeight;
  Assign(InFile, 'printing1.lpr'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Printer.Canvas.TextOut(XPos, YPos, Line);
      YPos += LineHeight;
    end;
  end;
  Close(InFile);
  AuthorLength := Printer.Canvas.TextWidth(Author);
  XPos := PaperSize.Width - HMargin - AuthorLength;
  YPos += 2 * LineHeight;
  Printer.Canvas.TextOut(XPos, YPos, Author);
  Printer.EndDoc;
end.

