{*************************************}
{* Main unit for FclPDF7 application *}
{*************************************}

unit pdf7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, fppdf;

type
  {********}
  { TfPDF7 }
  {********}
  TfPDF7 = class(TForm)
    Label7: TLabel;
    Label8: TLabel;
    mMenu: TMainMenu;
    mFile, mFileExit, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    rbPortrait, rbLandscape, rbAutomatic: TRadioButton;
    cobSize: TComboBox;
    edImage, edPDF, edWidth, edHeight: TEdit;
    btBrowse: TButton;
    btCreate: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure cobSizeChange(Sender: TObject);
  private
    rResize: Real;
    sDir, sImage, sPDF, sMess: string;
    Document: TPDFDocument;
    Section: TPDFSection;
    Page: TPDFPage;
  end;

const
  Margin = 10;

var
  fPDF7: TfPDF7;

implementation

{$R *.lfm}

{********}
{ TfPDF7 }
{********}

{ Application start: Initialization }

procedure TfPDF7.FormCreate(Sender: TObject);

begin
  rResize := 50;
  sDir := GetCurrentDir;
end;

{ Menu item "File > Exit": Exit application }

procedure TfPDF7.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfPDF7.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Free Pascal fcl-pdf tutorial.' + LineEnding;
  S += 'FclPDF7 example program:' + LineEnding;
  S += 'Drawing an image onto an A4 page. ' + LineEnding + LineEnding;
  S += 'For details concerning the tutorial, please, visit the "Lazarus/Free Pascal programming" ';
  S += 'section at https://www.streetinfo.lu.' + LineEnding + LineEnding;
  MessageDlg('About "FclPDF7"', S, mtInformation, [mbOK], 0);
end;

{ Button "Browse" pushed: Browse for image file }

procedure TfPDF7.btBrowseClick(Sender: TObject);

var
  ImageExt: string;

begin
  dlgOpen.InitialDir := sDir;
  dlgOpen.FileName := '';
  if dlgOpen.Execute then begin
    // User has selected a file
    sImage := dlgOpen.FileName;
    sDir := ExtractFileDir(sImage);                                            // save directory (to open next time at same location)
    edImage.Text := sImage;
    sPDF := ExtractFileName(sImage);
    ImageExt  := ExtractFileExt(sPDF);
    sPDF := StringReplace(sPDF, ImageExt, '.pdf', []);                         // PDF file with same name as image and saved with image
    edPDF.Text := sPDF;
  end;
end;

{ Button "Create" pushed: Craete PDF document }

procedure TfPDF7.btCreateClick(Sender: TObject);

var
  ImgID, ImgWidth, ImgHeight, TotalWidth, TotalHeight, MaxWidth, MaxHeight: Integer;
  DWidth, DHeight, W1, W2, H1, H2: Integer;
  X, Y: TPDFFloat;

begin
  sMess := '';
  // Check user input
  if edImage.Text = '' then
    sMess := 'Image file name is missing!'
  else if edPDF.Text = '' then
    sMess := 'PDF file name is missing!'
  else if (cobSize.ItemIndex = 4) and ((edWidth.Text = '') or (edHeight.Text = '')) then
    sMess := 'Image dimensions are incomplete/missing!'
  else if (cobSize.ItemIndex = 4) and ((StrToInt(edWidth.Text) <= 0) or (StrToInt(edHeight.Text) <= 0)) then
    sMess := 'Image dimensions are invalid!'
  else if not FileExists(edImage.Text) then
    sMess := 'Image file not found!'
  // User input is ok: Determine parameters for image creation
  else begin
    // Create the document
    Document := TPDFDocument.Create(Nil);
    if rbLandscape.Checked then
      Document.DefaultOrientation := ppoLandscape
    else
      Document.DefaultOrientation := ppoPortrait;
    Document.DefaultUnitofMeasure := uomPixels;
    Document.StartDocument;
    // Add image ot document's image list
    ImgID := Document.Images.AddFromFile(sImage, False);
    // Create the page
    Section := Document.Sections.AddSection;
    Page := Document.Pages.AddPage;
    Section.AddPage(Page);
    // Get original image width and height
    ImgWidth  := Document.Images[ImgID].Width;
    ImgHeight := Document.Images[ImgID].Height;
    // Automatic page orientation: Adapt orientation if necessary
    if ImgWidth > ImgHeight then begin
      if rbAutomatic.Checked then
        Page.Orientation := ppoLandscape;
    end;
    // Total width and height
    TotalWidth  := Page.Paper.W;
    TotalHeight := Page.Paper.H;
    if cobSize.ItemIndex = 1 then begin
      // Only one half of the page will be used
      if Page.Orientation = ppoPortrait then
        TotalHeight := TotalHeight div 2
      else
        TotalWidth := TotalWidth div 2;
    end;
    // Calculate maximum width and height (-> drawing area = page area - margins)
    MaxWidth  := Round(TotalWidth - 2 * MMtoPDF(Margin));
    MaxHeight := Round(TotalHeight - 2 * MMtoPDF(Margin));
    // Calculate drawn image width and height
    if cobSize.ItemIndex in [2..4] then begin
      // Size options "Original", "Resize", "Custom"
      if cobSize.ItemIndex = 2 then begin
        // Size option "Original": Drawn image dimensions = original image dimensions
        edWidth.Text  := IntToStr(ImgWidth);
        edHeight.Text := IntToStr(ImgHeight);
      end
      else if cobSize.ItemIndex = 3 then begin
        // Size option "Resize": Drawn image dimensions = original image dimensions x user specified resize factor
        edWidth.Text  := IntToStr(Round(ImgWidth * rResize / 100));
        edHeight.Text := IntToStr(Round(ImgHeight * rResize / 100));
      end;
      // Drawn image width and height read from form input fields
      DWidth  := StrToInt(edWidth.Text);
      DHeight := StrToInt(edHeight.Text);
      // Draw image only if, with specified dimensions, it fits into the page drawing area
      if (DWidth > MaxWidth) or (DHeight > MaxHeight) then begin
        if cobSize.ItemIndex = 2 then
          sMess := 'Original image does not fit on page!'
        else
          sMess := 'Custom size image does not fit on page!';
      end;
    end
    else begin
      // Size options "Page", "Half page"
      W1 := MaxWidth;  H1 := Round(ImgHeight * (MaxWidth / ImgWidth));
      H2 := MaxHeight; W2 := Round(ImgWidth * (MaxHeight / ImgHeight));
      if (H1 <= MaxHeight) and (W2 <= MaxWidth) then begin
        // If maximum width and maximum height formats fit both into the drawing area,
        // choose the one that fills it the most (greatest surface); if the surfaces
        // are equal, too, make choice depending on page orientation
        if W1 * H1 > W2 * H2 then begin
          DWidth := W1; DHeight := H1;
        end
        else if W1 * H1 < W2 * H2 then begin
          DWidth := W2; DHeight := H2;
        end
        else begin
          if Page.Orientation = ppoPortrait then begin
            DWidth := W2; DHeight := H2;
          end
          else begin
            DWidth := W1; DHeight := H1;
          end;
        end;
      end
      else if H1 <= MaxHeight then begin
        // If only the maximum width format fits into the drawing area,
        // choose this format
        DWidth := W1; DHeight := H1;
      end
      else begin
        // If only the maximum height format fits into the drawing area,
        // choose this format
        DWidth := W2; DHeight := H2;
      end;
      // Fill width and height values into form text fields
      edWidth.Text  := IntToStr(DWidth);
      edHeight.Text := IntToStr(DHeight);
    end;
    if sMess = '' then begin
      // Calculate image position (= bottom-left corner position) on the page
      X := TotalWidth div 2 - DWidth div 2;
      if (cobSize.ItemIndex = 1) and (Page.Orientation = ppoPortrait) then
        // Y-coordinate special case if outout size = half page
        Y := 3 * TotalHeight div 2 - DHeight div 2
      else
        // Y-coordinate normal case
        Y := TotalHeight div 2 - DHeight div 2;
      // Draw the image
      Page.DrawImage(X, Y, DWidth, DHeight, ImgID);
      // Save the image to PDF file
      Document.SaveToFile(sDir + '\' + sPDF);
    end;
    // Free the document
    Document.Free;
  end;
  // If there was an error, display a message
  if sMess <> '' then
    MessageDlg('FclPDF7', sMess, mtError, [mbOK], 0);
end;

{ Combobox selection changes (user chose a different output size) }

procedure TfPDF7.cobSizeChange(Sender: TObject);

var
  S: string;

begin
  if cobSize.ItemIndex = 4 then begin
    // Enable width and height input fields if user chose output size = custom
    edWidth.Enabled := True; edHeight.Enabled := True;
  end
  else begin
    // Ask for resizing factor if user chose output size = resize
    if cobSize.ItemIndex = 3 then begin
      S := InputBox('FclPDF7', 'Image resize factor (%)', FloatToStr(rResize));
      if S <> '' then begin
        if StrToFloat(S) > 0 then
          rResize := StrToFloat(S);
      end;
    end;
    // Disable width and height input fields
    edWidth.Text := ''; edHeight.Text := '';
    edWidth.Enabled := False; edHeight.Enabled := False;
  end;
end;

end.

