{*************************************}
{*      fcl-pdf (fppdf) example      *}
{* Text width and height calculation *}
{*************************************}

program fclpdf6;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fppdf, fpttf;

const
  MyText   = 'HELLO WORLD FROM FREE PASCAL!';
  MyFDir   = 'C:\Windows\Fonts';
  MyFont   = 'Castellar';
  MyFFile  = 'CASTELAR.TTF';
  MyFSize  = 20;
  MyFColor = clBlue;

var
  Document: TPDFDocument;
  Section: TPDFSection;
  Page: TPDFPage;
  Fonts: TFPFontCacheList;
  Font: TFPFontCacheItem;
  FontID: Integer;
  CX, CY, W, H, DH, X, Y: TPDFFloat;
  RX, RY, RW, RH: TPDFFloat;

begin
  // Build the font cache
  Fonts := TFPFontCacheList.Create;
  Fonts.SearchPath.Add(MyFDir);
  Fonts.BuildFontCache;
  // Create the document
  Document := TPDFDocument.Create(Nil);
  Document.DefaultPaperType := ptA5;
  Document.DefaultOrientation := ppoLandscape;
  Document.Options := Document.Options + [poPageOriginAtTop];
  Document.StartDocument;
  // Register the font to be used
  Document.FontDirectory := MyFDir;
  FontID := Document.AddFont(MyFFile, MyFont);
  // Create the page
  Section := Document.Sections.AddSection;
  Page := Document.Pages.AddPage;
  Section.AddPage(Page);
  // Calculate the text hight and width
  Font := Fonts.Find(MyFont, False, False);
  W  := Font.TextWidth(MyText, MyFSize);
  H  := Font.TextHeight(MyText, MyFSize, DH);
  W  := (W * 25.4) / Fonts.DPI;
  H  := (H * 25.4) / Fonts.DPI ;
  DH := (DH * 25.4) / Fonts.DPI ;
  // Calculate text position
  CX := PDFToMM(Page.Paper.W) / 2;
  CY := PDFToMM(Page.Paper.H) / 2;
  X  := CX - W / 2;
  Y  := CY + (H + DH) / 2;
  // Write the text to the center of the page
  Page.SetFont(FontID, MyFSize);
  Page.SetColor(MyFColor, False);
  Page.WriteText(X, Y, MyText);
  // Calculate border position and dimensions
  RX := X - 4;
  RY := Y + 4;
  RW := 2 * 4 + W;
  RH := 2 * 4 + (H + DH);
  // Draw the border (as rectangle)
  Page.SetColor(MyFColor, True);
  Page.DrawRect(RX, RY, RW, RH, 3, False, True);
  Document.SaveToFile('fclpdf6.pdf');
end.

