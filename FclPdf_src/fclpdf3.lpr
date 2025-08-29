{**************************************}
{*       fcl-pdf (fppdf) example      *}
{* Drawing of rectangles and ellipses *}
{**************************************}

program fclpdf3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fppdf;

const
  H = 15; V = 15;
  Triangle: array[1..3] of TPDFCoord = (
    (X: H + 50 / 2; Y: V;),
    (X: H + 50; Y: V + 75;),
    (X: H; Y: V + 75;)
  );

var
  Document: TPDFDocument;
  Section: TPDFSection;
  Page: TPDFPage;

begin
  Document := TPDFDocument.Create(Nil);
  Document.Options := Document.Options + [poPageOriginAtTop];
  Document.StartDocument;
  Section := Document.Sections.AddSection;
  Page := Document.Pages.AddPage;
  Section.AddPage(Page);
  Page.DrawPolygon(Triangle, 1);
  Page.SetColor(clLime, False);
  Page.FillStrokePath;
  Page.SetColor(clBlue, True);
  Page.DrawRect(2 * H + 50, V + 75, 50, 75, 5, False, True);
  Page.SetColor(clAqua, False);
  Page.DrawRoundedRect(3 * H + 100, V + 75, 50, 75, 5, 2, True, True);
  Page.DrawRect(2 * H + 50, 2 * V + 75 + Sin(Pi / 4) * 75, 50, 75, 5, True, True, -45);
  Page.SetColor(clBlack, True);
  Page.SetColor($FFA500, False);
  Page.DrawEllipse(H, 3 * V + 150 + Sin(Pi / 4) * 75 + Sin(Pi / 4) * 50, 50, 75, 1, True, True);
  Page.SetColor(clRed, True);
  Page.DrawEllipse(3 * H + 100, 3 * V + 75 + 50 + Sin(Pi / 4) * 75 + Sin(Pi / 4) * 50, 50, 50, 5, False, True);
  Page.SetColor($FFA500, True);
  Page.DrawEllipse(3 * H + 100, 3 * V + 75 + 75 + Sin(Pi / 4) * 75 + Sin(Pi / 4) * 50, 50, 20, 5, False, True);
  Document.SaveToFile('fclpdf3.pdf');
end.

