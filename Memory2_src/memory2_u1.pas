{***************************************}
{* Main unit for application "Memory2" *}
{* *************************************}

unit memory2_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls;

type
  TStrings  = array[1 .. 36] of string;
  TBooleans = array[1 .. 36] of Boolean;
  TImages   = array[1 .. 36] of TImage;
  {***********}
  { TfMemory2 }
  {***********}
  TfMemory2 = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mGameNew1, mGameNew2, mGameNew3, mGameNew4, mGameNew5: TMenuItem;
    mSettings, mSettingsSize, mSettingsSize46, mSettingsSize56, mSettingsSize66: TMenuItem;
    mSettingsMode, mSettingsMode2, mSettingsMode3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Image1, Image2, Image3, Image4, Image5, Image6:  TImage;
    Image7, Image8, Image9, Image10, Image11, Image12: TImage;
    Image13, Image14, Image15, Image16, Image17, Image18: TImage;
    Image19, Image20, Image21, Image22, Image23, Image24: TImage;
    Image25, Image26, Image27, Image28, Image29, Image30: TImage;
    Image31, Image32, Image33, Image34, Image35, Image36: TImage;
    procedure FormCreate(Sender: TObject);
    procedure mGameNew1Click(Sender: TObject);
    procedure mGameNew2Click(Sender: TObject);
    procedure mGameNew3Click(Sender: TObject);
    procedure mGameNew4Click(Sender: TObject);
    procedure mGameNew5Click(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsSize46Click(Sender: TObject);
    procedure mSettingsSize56Click(Sender: TObject);
    procedure mSettingsSize66Click(Sender: TObject);
    procedure mSettingsMode2Click(Sender: TObject);
    procedure mSettingsMode3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);   procedure Image2Click(Sender: TObject);   procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);   procedure Image5Click(Sender: TObject);   procedure Image6Click(Sender: TObject);
    procedure Image7Click(Sender: TObject);   procedure Image8Click(Sender: TObject);   procedure Image9Click(Sender: TObject);
    procedure Image10Click(Sender: TObject);  procedure Image11Click(Sender: TObject);  procedure Image12Click(Sender: TObject);
    procedure Image13Click(Sender: TObject);  procedure Image14Click(Sender: TObject);  procedure Image15Click(Sender: TObject);
    procedure Image16Click(Sender: TObject);  procedure Image17Click(Sender: TObject);  procedure Image18Click(Sender: TObject);
    procedure Image19Click(Sender: TObject);  procedure Image20Click(Sender: TObject);  procedure Image21Click(Sender: TObject);
    procedure Image22Click(Sender: TObject);  procedure Image23Click(Sender: TObject);  procedure Image24Click(Sender: TObject);
    procedure Image25Click(Sender: TObject);  procedure Image26Click(Sender: TObject);  procedure Image27Click(Sender: TObject);
    procedure Image28Click(Sender: TObject);  procedure Image29Click(Sender: TObject);  procedure Image30Click(Sender: TObject);
    procedure Image31Click(Sender: TObject);  procedure Image32Click(Sender: TObject);  procedure Image33Click(Sender: TObject);
    procedure Image34Click(Sender: TObject);  procedure Image35Click(Sender: TObject);  procedure Image36Click(Sender: TObject);
  private
    iSize, iSizeSel, iIdentical, iIdenticalSel: Integer;
    iCount, iClick, iImg1, iImg2, iImg3: Integer;
    sPicPrefix, sPicSuffix: string;
    aImageFiles: TStrings;
    aImageFound: TBooleans;
    aImages: TImages;
  end;

var
  fMemory2: TfMemory2;

implementation

{$R *.lfm}

{ Start a new Memory game }

procedure NewGame(SizeSel, IdenticalSel: Integer; PicPrefix, PicSuffix: string; var Size, Identical: Integer;
  var Images: TImages; var ImageFiles: TStrings; var ImagesFound: TBooleans);

var
  I, F: Integer;
  PicFile: string;
  PicFileUsed: array[1 .. 18] of Integer;
  PicFileOk: Boolean;

begin
  // Make game settings (as selected by user) active
  Size := SizeSel;
  Identical := IdenticalSel;
  // Hide the squares not used (in 4x6 and 5x6 game)
  for I := 1 to 36 do
    Images[I].Visible := True;
  for I := Size + 1 to 36 do
    Images[I].Visible := False;
  // Display all "No picture" images
  for I := 1 to Size div 2 do
    PicFileUsed[I] := 0;
  PicFile := 'pics/' + PicPrefix + '00' + PicSuffix + '.jpg';  DoDirSeparators(PicFile);
  for I := 1 to Size do
    Images[I].Picture.LoadFromFile(PicFile);
  // Randomly distribute the pictures onto the grid
  for I := 1 to Size do begin
    // For all 24/30/36 squares (images)
    repeat
      PicFileOk := True;
      F := Random(Size div Identical) + 1;                                               // get a random picture file
      if PicFileUsed[F] < Identical then begin
        // Every image has to be used twice (resp. three times)
        Inc(PicFileUsed[F]);
        PicFile := IntToStr(F);
        if Length(PicFile) = 1 then
          PicFile := '0' + PicFile;
        PicFile := 'pics/' + PicPrefix + PicFile + PicSuffix + '.jpg';  DoDirSeparators(PicFile);
        ImageFiles[I] := PicFile;                                                        // save picture file name for each square
        ImagesFound[I] := False;                                                         // mark image as "not yet found"
      end
      else begin
        // Picture already used twice (resp. three times)
        PicFileOk := False;
      end;
    until PicFileOk;
  end;
end;

{ Play the Memory game }

procedure PlayGame(Prefix, Suffix: string; Size, Img: Integer; var Click, Img1, Img2, Img3, Count: Integer;
  var ImgFiles: TStrings; var ImgFound:TBooleans; var Images: TImages);

var
  NoPicFile: string;

begin
  NoPicFile := 'pics/' + Prefix + '00' + Suffix + '.jpg';  DoDirSeparators(NoPicFile);
  if Click = 1 then begin
    // First click
    if (Img1 > 0) and (not ImgFound[Img1]) then                                          // if image (first click before) not yet found...
      Images[Img1].Picture.LoadFromFile(NoPicFile);                                      // ...reset to default picture
    if (Img2 > 0) and (not ImgFound[Img2]) then                                          // if image (second click before) not yet found...
      Images[Img2].Picture.LoadFromFile(NoPicFile);                                      // ...reset to default picture
    if fMemory2.mSettingsMode3.Checked and (Img3 > 0) and (not ImgFound[Img3]) then      // if image (third click before) not yet found...
      Images[Img3].Picture.LoadFromFile(NoPicFile);                                      // ...reset to default picture
    Images[Img].Picture.LoadFromFile(ImgFiles[Img]);                                     // display first picture (of pair or triplet)}
    Img1 := Img;                                                                         // index of first image clicked
    Click := 2;                                                                          // next click will be a second one
  end
  else if Click = 2 then begin
    // Second click
    if fMemory2.mSettingsMode3.Checked then begin
      // Find triplets game mode
      Images[Img].Picture.LoadFromFile(ImgFiles[Img]);                                   // display second picture (of triplet)}
      Img2 := Img;                                                                       // index of second image clicked
      Click := 3;                                                                        // next click will be a third one
    end
    else begin
      // Find pairs game mode
      Img2 := Img;                                                                       // index of second image clicked
      if (Img1 <> Img2) and (ImgFiles[Img1] = ImgFiles[Img2]) then begin
        // Picture files are equal (picture pair found)
        ImgFound[Img1] := True; ImgFound[Img2] := True;
        Inc(Count);
      end;
      Images[Img].Picture.LoadFromFile(ImgFiles[Img]);                                   // display second picture (of pair)
      if Count = Size div 2 then begin
        // All pairs have been found
        MessageDlg('Memory','Game over : All picture pairs found!', mtInformation, [mbOK], 0);
      end
      else begin
        // Still picture pairs to find
        Click := 1;                                                                      // next click will be a first one
      end;
    end;
  end
  else begin
    // Third click
    if fMemory2.mSettingsMode3.Checked then begin
      // Find triplets game mode, only
      Img3 := Img;                                                                       // index of third image clicked
      if (Img1 <> Img2) and (Img1 <> Img3) and (ImgFiles[Img1] = ImgFiles[Img2]) and (ImgFiles[Img1] = ImgFiles[Img3]) then begin
        // Picture files are equal (picture triplet found)
        ImgFound[Img1] := True; ImgFound[Img2] := True; ImgFound[Img3] := True;
        Inc(Count);
      end;
      Images[Img].Picture.LoadFromFile(ImgFiles[Img]);                                   // display third picture (of triplet)
      if Count = Size div 3 then begin
        // All triplets have been found
        MessageDlg('Memory','Game over : All picture triplets found!', mtInformation, [mbOK], 0);
      end
      else begin
        // Still picture triplets to find
        Click := 1;                                                                      // next click will be a first one
      end;
    end;
  end;
end;

{***********}
{ TfMemory2 }
{***********}

{ Application start: Initialization }

procedure TfMemory2.FormCreate(Sender: TObject);

begin
  // Create an array containing the 36 images
  aImages[1]  := Image1;  aImages[2]  := Image2;  aImages[3] := Image3;   aImages[4]  := Image4;  aImages[5]  := Image5;  aImages[6]  := Image6;
  aImages[7]  := Image7;  aImages[8]  := Image8;  aImages[9] := Image9;   aImages[10] := Image10; aImages[11] := Image11; aImages[12] := Image12;
  aImages[13] := Image13; aImages[14] := Image14; aImages[15] := Image15; aImages[16] := Image16; aImages[17] := Image17; aImages[18] := Image18;
  aImages[19] := Image19; aImages[20] := Image20; aImages[21] := Image21; aImages[22] := Image22; aImages[23] := Image23; aImages[24] := Image24;
  aImages[25] := Image25; aImages[26] := Image26; aImages[27] := Image27; aImages[28] := Image28; aImages[29] := Image29; aImages[30] := Image30;
  aImages[31] := Image31; aImages[32] := Image32; aImages[33] := Image33; aImages[34] := Image34; aImages[35] := Image35; aImages[36] := Image36;
  // Startup settings
  iSizeSel := 36;                                                                        // 6x6 squares puzzle
  iIdenticalSel := 2;                                                                    // classic game: find picture pairs
  // Start random number generator
  Randomize;
  // Start a new game (Nativity I picture set)
  mGameNew1.Click;
end;

{ Menu item "File > New > Nativity I": Start new game with "Nativity I" picture set }

procedure TfMemory2.mGameNew1Click(Sender: TObject);

begin
  sPicPrefix := 'n'; sPicSuffix := 'a';
  NewGame(iSizeSel, iIdenticalSel, sPicPrefix, sPicSuffix, iSize, iIdentical, aImages, aImageFiles, aImageFound);
  iCount := 0;                                                                           // pairs/triplets found counter
  iClick := 1;                                                                           // image click will be the first one
  iImg1 := -1; iImg2 := -1; iImg3 := -1;                                                 // "disable" image indexes
end;

{ Menu item "File > New > Nativity II": Start new game with "Nativity II" picture set }

procedure TfMemory2.mGameNew2Click(Sender: TObject);

begin
  sPicPrefix := 'n'; sPicSuffix := 'b';
  NewGame(iSizeSel, iIdenticalSel, sPicPrefix, sPicSuffix, iSize, iIdentical, aImages, aImageFiles, aImageFound);
  iCount := 0;
  iClick := 1;
  iImg1 := -1; iImg2 := -1; iImg3 := -1;
end;

{ Menu item "File > New > Santa Claus": Start new game with "Santa Claus" picture set }

procedure TfMemory2.mGameNew3Click(Sender: TObject);

begin
  sPicPrefix := 's'; sPicSuffix := '';
  NewGame(iSizeSel, iIdenticalSel, sPicPrefix, sPicSuffix, iSize, iIdentical, aImages, aImageFiles, aImageFound);
  iCount := 0;
  iClick := 1;
  iImg1 := -1; iImg2 := -1; iImg3 := -1;
end;

{ Menu item "File > New > Christmas trees": Start new game with "Christmas trees" picture set }

procedure TfMemory2.mGameNew4Click(Sender: TObject);

begin
  sPicPrefix := 't'; sPicSuffix := '';
  NewGame(iSizeSel, iIdenticalSel, sPicPrefix, sPicSuffix, iSize, iIdentical, aImages, aImageFiles, aImageFound);
  iCount := 0;
  iClick := 1;
  iImg1 := -1; iImg2 := -1; iImg3 := -1;
end;

{ Menu item "File > New > Christmas bells": Start new game with "Christmas bells" picture set }

procedure TfMemory2.mGameNew5Click(Sender: TObject);

begin
  sPicPrefix := 'b'; sPicSuffix := '';
  NewGame(iSizeSel, iIdenticalSel, sPicPrefix, sPicSuffix, iSize, iIdentical, aImages, aImageFiles, aImageFound);
  iCount := 0;
  iClick := 1;
  iImg1 := -1; iImg2 := -1; iImg3 := -1;
end;

{ Menu item "Game > Exit": Exit application }

procedure TfMemory2.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Puzzle size > ...": Choose puzzle size (4x6, 5x6 or 6x6) }

procedure TfMemory2.mSettingsSize46Click(Sender: TObject);

begin
  mSettingsSize46.Checked := True;  mSettingsSize56.Checked := False;  mSettingsSize66.Checked := False;
  iSizeSel := 24;
end;

procedure TfMemory2.mSettingsSize56Click(Sender: TObject);

begin
  mSettingsSize46.Checked := False;  mSettingsSize56.Checked := True;  mSettingsSize66.Checked := False;
  iSizeSel := 30;
end;

procedure TfMemory2.mSettingsSize66Click(Sender: TObject);

begin
  mSettingsSize46.Checked := False;  mSettingsSize56.Checked := False;  mSettingsSize66.Checked := True;
  iSizeSel := 36;
end;

{ Menu items "Settings > Game mode > ...": Choose game mode (picture pairs or triplets to find) }

procedure TfMemory2.mSettingsMode2Click(Sender: TObject);

begin
  mSettingsMode2.Checked := True; mSettingsMode3.Checked := False;
  iIdenticalSel := 2;
end;

procedure TfMemory2.mSettingsMode3Click(Sender: TObject);

begin
  mSettingsMode2.Checked := False; mSettingsMode3.Checked := True;
  iIdenticalSel := 3;
end;

{ Menu item "Help > Help": Display (brief) application help }

procedure TfMemory2.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Aim of the game: Find all picture pairs (resp. triplets) by clicking 2 (resp. 3) identical images subsequently.' + LineEnding;
  S += 'Use the "New" commands in the "Game" menu to start a new game. Use the "Settings" menu items to choose the number of pictures, ';
  S += 'and the game mode (pairs or triplets to find). Please, note that the settings become only active when a new game is started.';
  MessageDlg('"Memory2" Help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfMemory2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Memory game - Christmas Edition.' + LineEnding;
  S += 'Find picture pairs or triplets in this classic Memory game with 5 Christmas related picture sets.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, 2016 - 2024.';
  MessageDlg('About "Memory2"', S, mtInformation, [mbOK], 0);
end;

{ User click on squares (images) 1 - 36: Play the game for this picture }

procedure TfMemory2.Image1Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 1, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image2Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 2, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image3Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 3, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image4Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 4, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image5Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 5, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image6Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 6, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image7Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 7, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image8Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 8, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image9Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 9, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image10Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 10, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image11Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 11, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image12Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 12, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image13Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 13, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image14Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 14, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image15Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 15, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image16Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 16, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image17Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 17, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image18Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 18, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image19Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 19, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image20Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 20, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image21Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 21, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image22Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 22, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image23Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 23, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image24Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 24, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image25Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 25, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image26Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 26, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image27Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 27, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image28Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 28, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image29Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 29, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image30Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 30, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image31Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 31, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image32Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 32, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image33Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 33, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image34Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 34, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image35Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 35, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory2.Image36Click(Sender: TObject);

begin
  PlayGame(sPicPrefix, sPicSuffix, iSize, 36, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

end.

