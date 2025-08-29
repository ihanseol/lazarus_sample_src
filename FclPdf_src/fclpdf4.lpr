{***************************************************}
{*             fcl-pdf (fppdf) example             *}
{* Writing some text to the page of a PDF document *}
{***************************************************}

program fclpdf4;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  fppdf;

const
  Fonts: array[0..4] of string = (
    'Arial', 'Arial Bold', 'Arial Italic', 'Old English Text MT', 'Helvetica'
  );
  Files: array of string = (
    'arial.ttf', 'arialbd.ttf', 'ariali.ttf', 'OLDENGL.TTF', ''
  );

var
  I: Integer;
  Document: TPDFDocument;
  Section: TPDFSection;
  Page: TPDFPage;
  FontIDs: array[0..4] of Integer;

begin
  Document := TPDFDocument.Create(Nil);
  Document.DefaultPaperType := ptA5;
  Document.DefaultOrientation := ppoLandscape;
  Document.Options := Document.Options + [poPageOriginAtTop];
  Document.FontDirectory := 'C:\Windows\Fonts';
  Document.StartDocument;
  for I := 0 to 4 do begin
    if Files[I] = '' then
      FontIDs[I] := Document.AddFont(Fonts[I])
    else
      FontIDs[I] := Document.AddFont(Files[I], Fonts[I]);
  end;
  Section := Document.Sections.AddSection;
  Page := Document.Pages.AddPage;
  Section.AddPage(Page);
  Page.SetFont(FontIDs[0], 12);
  Page.WriteText(20, 20, 'This is Arial, black, 12 points.');
  Page.WriteText(20, 30, 'This is the same but underlined.', 0, True);
  Page.SetFont(FontIDs[1], 11);
  Page.SetColor(clBlue, False);
  Page.WriteText(20, 40, 'This is Arial, blue, 11 points, bold.');
  Page.SetFont(FontIDs[2], 11);
  Page.WriteText(20, 50, 'This is the same but italic instead of bold.');
  Page.SetFont(FontIDs[3], 16);
  Page.SetColor(clRed, False);
  Page.WriteText(20, 60, 'This is Old English Text MT, red, 16 points');
  Page.SetFont(FontIDs[4], 15);
  Page.SetColor(clBlack, False);
  Page.WriteText(40, 70, 'Vertical printing', -90);
  Document.SaveToFile('fclpdf4.pdf');
end.

