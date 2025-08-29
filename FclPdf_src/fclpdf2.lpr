{*********************************************}
{*          fcl-pdf (fppdf) example          *}
{* Drawing a stairway on a custom sized page *}
{*********************************************}

program fclpdf2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fppdf;

const
  Width = 150; Height = 100;

var
  I: Integer;
  Document: TPDFDocument;
  Section: TPDFSection;
  Page: TPDFPage;
  LStart, LEnd: TPDFCoord;
  Paper: TPDFPaper;

begin
  Document := TPDFDocument.Create(Nil);
  Document.DefaultOrientation := ppoLandscape;
  Document.DefaultPaperType := ptCustom;
  Document.StartDocument;
  Section := Document.Sections.AddSection;
  Page := Document.Pages.AddPage;
  Paper.W := Round(MMtoPDF(Width));
  Paper.H := Round(MMtoPDF(Height));
  Page.Paper := Paper;
  Page.SetColor(clBlack);
  Section.AddPage(Page);
  for I := 0 to 4 do begin
    LStart.X := 37.5 + 15 * I; LStart.Y := 25 + 10 * I;
    LEnd.X   := LStart.X; LEnd.Y := 25 + 10 * (I + 1);
    if I = 0 then
      Page.MoveTo(LStart);
    Page.DrawLine(LStart, LEnd, 2, False);
    LStart.X := 37.5 + 15 * I; LStart.Y := 25 + 10 * (I + 1);
    LEnd.X   := 37.5 + 15 * (I + 1); LEnd.Y := 25 + 10 * (I + 1);
    Page.DrawLine(LStart, LEnd, 2, False);
  end;
  Page.StrokePath;
  Document.SaveToFile('fclpdf2.pdf');
end.

