{**************************************}
{*      fcl-pdf (fppdf)  example      *}
{* Drawing the diagonal of an A4 page *}
{**************************************}

program fclpdf1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fppdf;

var
  Document: TPDFDocument;
  Section: TPDFSection;
  Page: TPDFPage;
  LStart, LEnd: TPDFCoord;

begin
  Document := TPDFDocument.Create(Nil);
  Document.Options := Document.Options + [poPageOriginAtTop];
  Document.StartDocument;
  Section := Document.Sections.AddSection;
  Page := Document.Pages.AddPage;
  Section.AddPage(Page);
  Page.SetColor(clMagenta);
  LStart.X := 15; LStart.Y := 15;
  LEnd.X   := 195; LEnd.Y := 285;
  Page.DrawLine(LStart, LEnd, 2);
  Document.SaveToFile('fclpdf1.pdf');
end.

