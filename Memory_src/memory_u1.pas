{**************************************}
{* Main unit for application "Memory" *}
{* ************************************}

unit memory_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, LCLIntf;

type
  TStrings  = array[1 .. 36] of string;
  TBooleans = array[1 .. 36] of Boolean;
  TImages   = array[1 .. 36] of TImage;
  {**********}
  { TfMemory }
  {**********}
  TfMemory = class(TForm)
    mGameRestart: TMenuItem;
    mMenu: TMainMenu;
    mGame, mGameNew, mGameDownload, mGameExit: TMenuItem;
    mSettings, mSettingsSize, mSettingsSize46, mSettingsSize56, mSettingsSize66: TMenuItem;
    mSettingsMode, mSettingsMode2, mSettingsMode3: TMenuItem;
    mSettingsSeries, mSettingsSeries1, mSettingsSeries2: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Image1, Image2, Image3, Image4, Image5, Image6:  TImage;
    Image7, Image8, Image9, Image10, Image11, Image12: TImage;
    Image13, Image14, Image15, Image16, Image17, Image18: TImage;
    Image19, Image20, Image21, Image22, Image23, Image24: TImage;
    Image25, Image26, Image27, Image28, Image29, Image30: TImage;
    Image31, Image32, Image33, Image34, Image35, Image36: TImage;
    dlgPicSet: TSelectDirectoryDialog;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameDownloadClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mGameRestartClick(Sender: TObject);
    procedure mSettingsSize46Click(Sender: TObject);
    procedure mSettingsSize56Click(Sender: TObject);
    procedure mSettingsSize66Click(Sender: TObject);
    procedure mSettingsMode2Click(Sender: TObject);
    procedure mSettingsMode3Click(Sender: TObject);
    procedure mSettingsSeries1Click(Sender: TObject);
    procedure mSettingsSeries2Click(Sender: TObject);
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
    iSeries, iSeriesSel, iSize, iSizeSel, iIdentical, iIdenticalSel: Integer;
    iCount, iClick, iImg1, iImg2, iImg3: Integer;
    sPicDir, sPicSet: string;
    bStart, bRestart: Boolean;
    aImageFiles: TStrings;
    aImageFound: TBooleans;
    aImages: TImages;
  end;

const
  sDefDir  = 'pics';
  sDefPics = 'orchids';
  sNoPic   = '00.jpg';

var
  fMemory: TfMemory;

implementation

{$R *.lfm}

{ Play the Memory game }

procedure PlayGame(PicDir, PicSet: string; Size, Img: Integer; var Click, Img1, Img2, Img3, Count: Integer;
  var ImgFiles: TStrings; var ImgFound:TBooleans; var Images: TImages);

var
  NoPicFile: string;

begin
  NoPicFile := PicDir + '/' + PicSet + '/' + sNoPic;  DoDirSeparators(NoPicFile);
  if Click = 1 then begin
    // First click
    if (Img1 > 0) and (not ImgFound[Img1]) then                                          // if image (first click before) not yet found...
      Images[Img1].Picture.LoadFromFile(NoPicFile);                                      // ...reset to default picture
    if (Img2 > 0) and (not ImgFound[Img2]) then                                          // if image (second click before) not yet found...
      Images[Img2].Picture.LoadFromFile(NoPicFile);                                      // ...reset to default picture
    if fMemory.mSettingsMode3.Checked and (Img3 > 0) and (not ImgFound[Img3]) then       // if image (third click before) not yet found...
      Images[Img3].Picture.LoadFromFile(NoPicFile);                                      // ...reset to default picture
    Images[Img].Picture.LoadFromFile(ImgFiles[Img]);                                     // display first picture (of pair or triplet)}
    Img1 := Img;                                                                         // index of first image clicked
    Click := 2;                                                                          // next click will be a second one
  end
  else if Click = 2 then begin
    // Second click
    if fMemory.mSettingsMode3.Checked then begin
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
    if fMemory.mSettingsMode3.Checked then begin
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

{**********}
{ TfMemory }
{**********}

{ Application start: Initialization }

procedure TfMemory.FormCreate(Sender: TObject);

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
  bStart := True;                                                                        // start of game flag
  bRestart := False;                                                                     // restart of game (no folder browsing) flag
  // Start random number generator
  Randomize;
  // Start a new game
  mGameNew.Click;
end;

{ Menu item "Game > New": Start a new Memory game }

procedure TfMemory.mGameNewClick(Sender: TObject);

var
  PicPath: string;

  I, F: Integer;
  PicFile, Ser2File1, Ser2File2: string;
  PicFileUsed: array[1 .. 18] of Integer;
  PicFileOk: Boolean;

begin
  if bStart then begin
    // At start of game, load default picture set
    sPicDir := GetCurrentDir + '/' + sDefDir;  DoDirSeparators(sPicDir);
    sPicSet := sDefPics;
    bStart  := False;
  end
  else begin
    // During game session, browse for picture set or use picture set from previous game
    if bRestart then begin
      // "Restart" button: Use picture set from previous game
      bRestart := False;
    end
    else begin
      // "New" button: Browse for picture set
      dlgPicSet.InitialDir := sPicDir;
      if dlgPicSet.Execute then begin
        // User has selected a directory
        PicPath := dlgPicSet.FileName;
        sPicDir := ExtractFilePath(PicPath);
        sPicSet := ExtractFileName(PicPath);
      end;
    end;
  end;
  // Make game settings (as selected by user) active
  iSize := iSizeSel;
  iIdentical := iIdenticalSel;
  iSeries := iSeriesSel;
  // Hide the squares not used (in 4x6 and 5x6 game)
  for I := 1 to 36 do
    aImages[I].Visible := True;
  if (iSize = 24) or (iSize = 30) then begin
    for I := iSize + 1 to 36 do
      aImages[I].Visible := False;
  end;
  // Display all "No picture" images
  for I := 1 to iSize div 2 do
    PicFileUsed[I] := 0;
  PicFile := sPicDir + '/' + sPicSet + '/' + sNoPic;  DoDirSeparators(PicFile);
  for I := 1 to iSize do
    aImages[I].Picture.LoadFromFile(PicFile);
  // Check if picture set has a second image series
  if iSeries = 1 then begin
    // The application just check if {image}19.jpg and {image}36.jpg (first and last of set) exist
    Ser2File1 := sPicDir + '/' + sPicSet + '/' + '19.jpg';  DoDirSeparators(PicFile);
    Ser2File2 := sPicDir + '/' + sPicSet + '/' + '36.jpg';  DoDirSeparators(PicFile);
    if not FileExists(Ser2File1) or not FileExists(Ser2File2) then begin
      MessageDlg('Invalid settings', 'This picture set has only one image series. Settings will be changed by the application.', mtWarning, [mbOK], 0);
      mSettingsSeries1.Checked := True; mSettingsSeries2.Checked := False;
      iSeriesSel := 0; iSeries := 0;
    end;
  end;
  // Randomly distribute the pictures onto the grid
  for I := 1 to iSize do begin
    // For all 24/30/36 squares (images)
    repeat
      PicFileOk := True;
      F := Random(iSize div iIdentical) + 1;                                             // get a random picture file
      if PicFileUsed[F] < iIdentical then begin
        // Every image has to be used twice (resp. three times)
        Inc(PicFileUsed[F]);
        PicFile := IntToStr(iSeries * 18 + F);
        if Length(PicFile) = 1 then
          PicFile := '0' + PicFile;
        PicFile := sPicDir + '/' + sPicSet + '/' + PicFile + '.jpg';  DoDirSeparators(PicFile);
        aImageFiles[I] := PicFile;                                                       // save picture file name for each square
        aImageFound[I] := False;                                                         // mark image as "not found"
      end
      else begin
        // Picture already used twice (resp. three times)
        PicFileOk := False;
      end;
    until PicFileOk;
  end;
  iCount := 0;                                                                           // pairs/triplets found counter
  iClick := 1;                                                                           // image click will be the first one
  iImg1 := -1; iImg2 := -1; iImg3 := -1;                                                 // "disable" image indexes
end;

{ Menu item "Game > Restart": Restart the Memory game with same picture set (no folder browsing) }

procedure TfMemory.mGameRestartClick(Sender: TObject);

begin
  bRestart := True;
  mGameNew.Click;
end;

{ Menu item "Game > Download": Point web browser to picture sets download page }

procedure TfMemory.mGameDownloadClick(Sender: TObject);

var
  URL: string;

begin
  URL := 'http://www.streetinfo.lu/computing/lazarus/files/data/Memory/';
  OpenURL(URL);
end;

{ Menu item "Game > Exit": Exit application }

procedure TfMemory.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Puzzle size > ...": Choose puzzle size (4x6, 5x6 or 6x6) }

procedure TfMemory.mSettingsSize46Click(Sender: TObject);

begin
  mSettingsSize46.Checked := True;  mSettingsSize56.Checked := False;  mSettingsSize66.Checked := False;
  iSizeSel := 24;
end;

procedure TfMemory.mSettingsSize56Click(Sender: TObject);

begin
  mSettingsSize46.Checked := False;  mSettingsSize56.Checked := True;  mSettingsSize66.Checked := False;
  iSizeSel := 30;
end;

procedure TfMemory.mSettingsSize66Click(Sender: TObject);

begin
  mSettingsSize46.Checked := False;  mSettingsSize56.Checked := False;  mSettingsSize66.Checked := True;
  iSizeSel := 36;
end;

{ Menu items "Settings > Game mode > ...": Choose game mode (picture pairs or triplets to find) }

procedure TfMemory.mSettingsMode2Click(Sender: TObject);

begin
  mSettingsMode2.Checked := True; mSettingsMode3.Checked := False;
  iIdenticalSel := 2;
end;

procedure TfMemory.mSettingsMode3Click(Sender: TObject);

begin
  mSettingsMode2.Checked := False; mSettingsMode3.Checked := True;
  iIdenticalSel := 3;
end;

{ Menu items "Settings > Picture series > ...": Choose picture series (series 1, or series 2 if available) }

procedure TfMemory.mSettingsSeries1Click(Sender: TObject);

begin
  mSettingsSeries1.Checked := True; mSettingsSeries2.Checked := False;
  iSeriesSel := 0;
end;

procedure TfMemory.mSettingsSeries2Click(Sender: TObject);

begin
  mSettingsSeries1.Checked := False; mSettingsSeries2.Checked := True;
  iSeriesSel := 1;
end;

{ Menu item "Help > Help": Display (brief) application help }

procedure TfMemory.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Aim of the game: Find all picture pairs (resp. triplets) by clicking 2 (resp. 3) identical images subsequently.' + LineEnding;
  S += 'Use the "New" command in the "Game" menu to start a new game. This will open a "Select Directory" dialog, where you may ';
  S += 'choose the picture set, that you want to use. Use the "Restart" command to restart the game with the actual picture set. ';
  S += 'To download further picture sets, please, use the "Download" command in the "Games" menu.';
  MessageDlg('"Memory" Help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfMemory.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Memory game.' + LineEnding + LineEnding;
  S += 'Version 3.2, © allu, May 2016 - May 2023.';
  MessageDlg('About "Memory"', S, mtInformation, [mbOK], 0);
end;

{ User click on squares (images) 1 - 36: Play the game for this picture }

procedure TfMemory.Image1Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 1, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image2Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 2, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image3Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 3, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image4Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 4, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image5Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 5, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image6Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 6, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image7Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 7, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image8Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 8, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image9Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 9, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image10Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 10, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image11Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 11, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image12Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 12, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image13Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 13, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image14Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 14, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image15Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 15, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image16Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 16, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image17Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 17, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image18Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 18, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image19Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 19, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image20Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 20, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image21Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 21, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image22Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 22, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image23Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 23, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image24Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 24, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image25Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 25, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image26Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 26, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image27Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 27, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image28Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 28, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image29Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 29, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image30Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 30, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image31Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 31, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image32Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 32, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image33Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 33, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image34Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 34, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image35Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 35, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

procedure TfMemory.Image36Click(Sender: TObject);

begin
  PlayGame(sPicDir, sPicSet, iSize, 36, iClick, iImg1, iImg2, iImg3, iCount, aImageFiles, aImageFound, aImages);
end;

end.

