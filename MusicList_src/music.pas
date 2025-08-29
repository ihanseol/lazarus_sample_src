{***************************************}
{* Main unit for MusicList application *}
{***************************************}

unit music;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, LazUTF8, WinLibs, ID3v2, help;

type
  TMP3Tags = record
    Artist, Album, Title, Genre, Year, Track: string;
  end;
  TMP3Files = array of TMP3Tags;
  {*********}
  { TfMusic }
  {*********}
  TfMusic = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, Separator1, Separator2, Separator3: TMenuItem;
    mSettings, mSettingsContents, mSettingsContentsFiles, mSettingsContentsAlbums: TMenuItem;
    mSettingsContentsTrackTitle, mSettingsContentsFileName, mSettingsAlbumFolders: TMenuItem;
    mSettingsGroup1, mSettingsGroup2, mSettingsGroup0: TMenuItem;
    mSettingsFormat, mSettingsFormat1, mSettingsFormat2, mSettingsFormat3: TMenuItem;
    mSettingsSort, mSettingsSort3, mSettingsSort2, mSettingsSort1, mSettingsSort4, mSettingsSort5: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    cbLibPrivate, cbLibPublic, cbLibCustom: TCheckBox;
    Label1: TLabel;
    edList: TMemo;
    dlgSaveList: TSaveDialog;
    StaticText1: TStaticText;
    btSearch: TButton;
    btList: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsContentsFilesClick(Sender: TObject);
    procedure mSettingsContentsAlbumsClick(Sender: TObject);
    procedure mSettingsContentsTrackTitleClick(Sender: TObject);
    procedure mSettingsContentsFileNameClick(Sender: TObject);
    procedure mSettingsAlbumFoldersClick(Sender: TObject);
    procedure mSettingsFormat1Click(Sender: TObject);
    procedure mSettingsFormat2Click(Sender: TObject);
    procedure mSettingsFormat3Click(Sender: TObject);
    procedure mSettingsGroup1Click(Sender: TObject);
    procedure mSettingsGroup2Click(Sender: TObject);
    procedure mSettingsGroup0Click(Sender: TObject);
    procedure mSettingsSort1Click(Sender: TObject);
    procedure mSettingsSort2Click(Sender: TObject);
    procedure mSettingsSort3Click(Sender: TObject);
    procedure mSettingsSort4Click(Sender: TObject);
    procedure mSettingsSort5Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure cbLibPrivateChange(Sender: TObject);
    procedure cbLibPublicChange(Sender: TObject);
    procedure cbLibCustomChange(Sender: TObject);
    procedure btListClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
  private
    iGroup, iSort: Integer;
    sList, sAlbumsId, sFormat, sDir: string;
    bAlbumFolders, bPriv, bPub, bCustom: Boolean;
    slMusicFilesList: TStringList;
    aMusicPaths, aMusicFiles: array of string;
    aAllMP3Files, aMP3Files: TMP3Files;
    Libs: TLibraries;
    Folders: TLibFolderInfo;
  end;

var
  fMusic: TfMusic;

implementation

{$R *.lfm}

{ Create given length string (by adding blanks to its end) }

function Fill(S0: string; L: Integer): string;

var
  I: Integer;
  S: string;

begin
  S := S0;
  for I := UTF8Length(S0) to L - 1 do
    S += ' ';
  Result := S;
end;

{ Fill a string with N occurences of given character }

function CharN(Ch: Char; N: Integer): string;

var
  I: Integer;
  S: string;

begin
  S := '';
  for I := 1 to N do
    S += Ch;
  Result := S;
end;

{ Transform accents and umlauts to correct characters }

function StringTransform(Str: string): string;

// Strings returned by the ID3v2 unit do not correctly display if they contain accents or umlauts.
// This function searches these strings for the codes of a certain number of such characters and
// transforms the corresponding string element to the correct letter

const
  NChars = 20;
  AsciiCodes: array[1..NChars] of Integer = (
    196, 201, 203, 214, 220, 223, 224, 226, 228, 231, 232, 233, 234, 235, 238, 239, 241, 244, 246, 252
  );
  Characters: array[1..NChars] of string = (
    'Ä', 'É', 'Ë', 'Ö', 'Ü', 'ß', 'à', 'â', 'ä', 'ç', 'è', 'é', 'ê', 'ë', 'î', 'ï', 'ñ', 'ô', 'ö', 'ü'
  );

var
  I, J: Integer;
  S, SCh: string;
  Ch: Char;

begin
  S := '';
  for I := 1 to Length(Str) do begin
    Ch := Str[I]; SCh := Ch;
    if Ord(Ch) > 160 then begin
      SCh := '?';
      for J := 1 to NChars do begin
        if Ord(Ch) = AsciiCodes[J] then
          SCh := Characters[J];
      end;
    end;
    S += SCh;
  end;
  StringTransform := S;
end;

{ Read MP3 tags (using the ID3v2 unit) }

procedure ReadMP3Tags(MP3File: string; out Tags: TMP3Tags);

var
  MP3Tags: TID3v2;

begin
  MP3Tags := TID3v2.Create;
  try
    with MP3Tags do begin
      ReadFromFile(MP3File);
      Tags.Artist := StringTransform(Artist);
      Tags.Album := StringTransform(Album);
      Tags.Title := StringTransform(Title);
      Tags.Genre := StringTransform(Genre);
      Tags.Year  := Year;
      if Tags.Year = '' then
        Tags.Year := '????';
      Tags.Track := IntToStr(Track);
      if Tags.Track = '' then
        Tags.Track := '0';
    end;
  finally
    MP3Tags.Free;
  end;
end;

{ Sort text array elements on sortkey of given length }

procedure DoSort(var Lines: array of string; KeyLength: Integer);

var
  I, J: Integer;
  S: string;

begin
  for I := 0 to Length(Lines) - 2 do begin
    for J := I + 1 to Length(Lines) - 1 do begin
      if UTF8Copy(Lines[J], 1, KeyLength) < UTF8Copy(Lines[I], 1, KeyLength) then begin
        S := Lines[I]; Lines[I] := Lines[J]; Lines[J] := S;
      end;
    end;
  end;
end;

{ Create the music files list }

procedure MusicList(var MP3Files: TMP3Files; Group, Sort: Integer; Format, Filename: string);

var
  SortKeyLen, LArt, LAlb, LTit, LGen, LYea, LTra, I, P: Integer;
  SortKey, LineGroup, OldGroup, Line, Line2, Item, Colour: string;
  SortLines: array of string;
  FH: Text;

begin
  SetLength(SortLines, Length(MP3Files));
  // Determine maximum length of each of the fields that may be part of the sortkey
  LArt := 0; LAlb := 0; LTit := 0; LGen := 0; LTra := 0; LYea := 4;
  for I := 0 to Length(MP3Files) - 1 do begin
    if UTF8Length(MP3Files[I].Artist) > LArt then
      LArt := UTF8Length(MP3Files[I].Artist);
    if UTF8Length(MP3Files[I].Album) > LAlb then
      LAlb := UTF8Length(MP3Files[I].Album);
    if UTF8Length(MP3Files[I].Title) > LTit then
      LTit := UTF8Length(MP3Files[I].Title);
    if UTF8Length(MP3Files[I].Genre) > LGen then
      LGen := UTF8Length(MP3Files[I].Genre);
    if UTF8Length(MP3Files[I].Track) > LTra then
      LTra := UTF8Length(MP3Files[I].Track);
  end;
  // Add 5 to each of these length
  // This allows to directly use the array elements to create a proper formatted text list
  // and it will give a very simple way to extract the different information parts to create the other lists
  LArt += 5; LAlb += 5; LGen += 5; LTit += 5; LYea += 5; LTra += 5;
  // Create array elements containing sortkey + data (as will appear in the list)
  for I := 0 to Length(MP3Files) - 1 do begin
    if Group = 1 then begin
      // Grouping by music genre
      SortLines[I] := Fill(MP3Files[I].Genre, LGen);
      case Sort of
        1: begin
          SortLines[I] += Fill(MP3Files[I].Artist, LArt) + Fill(MP3Files[I].Album, LAlb);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        2: begin
          SortLines[I] += Fill(MP3Files[I].Artist, LArt) + Fill(MP3Files[I].Year, LYea);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Album, LAlb);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        3: begin
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Artist, LArt);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        4: begin
          SortLines[I] += Fill(MP3Files[I].Year, LYea) + Fill(MP3Files[I].Artist, LArt);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Album, LAlb);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        5: begin
          SortLines[I] += Fill(MP3Files[I].Title, LTit) + Fill(MP3Files[I].Artist, LArt);
          SortKey := SortLines[I];
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra);
        end;
      end;
    end
    else if Group = 2 then begin
      // Grouping by music artist
      SortLines[I] := Fill(MP3Files[I].Artist, LArt);
      case Sort of
        1, 3: begin
          SortLines[I] += Fill(MP3Files[I].Album, LAlb);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Genre, LGen) + Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        2, 4: begin
          SortLines[I] += Fill(MP3Files[I].Year, LYea);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Genre, LGen);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        5: begin
          SortLines[I] += Fill(MP3Files[I].Title, LTit);
          SortKey := SortLines[I];
          SortLines[I] += Fill(MP3Files[I].Artist, LArt) + Fill(MP3Files[I].Album, LAlb);
          SortLines[I] += Fill(MP3Files[I].Genre, LGen) + Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra);
        end;
      end;
    end
    else begin
      // No grouping
      SortLines[I] := '';
      case Sort of
        1: begin
          SortLines[I] += Fill(MP3Files[I].Artist, LArt) + Fill(MP3Files[I].Album, LAlb);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Genre, LGen) + Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        2: begin
          SortLines[I] += Fill(MP3Files[I].Artist, LArt) + Fill(MP3Files[I].Year, LYea);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Genre, LGen);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        3: begin
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Artist, LArt);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Genre, LGen) + Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        4: begin
          SortLines[I] += Fill(MP3Files[I].Year, LYea) + Fill(MP3Files[I].Artist, LArt);
          SortKey := SortLines[I] + Fill(MP3Files[I].Track, LTra);
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Genre, LGen);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra) + Fill(MP3Files[I].Title, LTit);
        end;
        5: begin
          SortLines[I] += Fill(MP3Files[I].Title, LTit) + Fill(MP3Files[I].Artist, LArt);
          SortKey := SortLines[I];
          SortLines[I] += Fill(MP3Files[I].Album, LAlb) + Fill(MP3Files[I].Genre, LGen);
          SortLines[I] += Fill(MP3Files[I].Year, LYea);
          if fMusic.mSettingsContentsFiles.Checked then
            SortLines[I] += Fill(MP3Files[I].Track, LTra);
        end;
      end;
    end;
    SortKeyLen := UTF8Length(SortKey);                                         // array elements will be sorted on a substring of this length
    SortLines[I] := SortKey + SortLines[I];                                    // this substring is the sortkey, here added to the left of the data
  end;
  // Sort the array elements
  DoSort(SortLines, SortKeyLen);
  // Create the list (= file)
  Assign(FH, Filename); Rewrite(FH);
  OldGroup := '';
  if Format = 'html' then begin
    // Special lines to be added at the beginning of the HTML document
    Line2 := '<!DOCTYPE html><html>' + LineEnding;
    Line2 += '<head>';
    Line2 += '<meta http-equiv="content-type" content="text/html; charset=UTF-8"/>';
    Line2 += '<meta name="viewport" content="width=device-width, initial-scale=1"/>';
    Line2 += '<title>Music library MP3 list</title>';
    Line2 += '</head>' + LineEnding;
    Line2 += '<body>' + LineEnding;
    Line2 += '<h1>Music library MP3 list.</h1>' + LineEnding;
    Line2 += '<table border="0">';
    Writeln(FH, Line2);
  end;
  Colour := 'AliceBlue';
  for I := 0 to Length(SortLines) - 1 do begin
    // Proceed array element by array element (producing 1 list entry)
    Line := SortLines[I]; UTF8Delete(Line, 1, SortKeyLen);                     // remove the sortkey
    Line := StringReplace(Line, '     0     ', '     -     ', []);             // this is to display track = 0 (in my whole-album files) with blank
    if (Format <> 'csv') and (Group <> 0) then begin
      // For HTML and TXT: Create group header (if grouping is selected)
      if Group = 1 then
        LineGroup := UTF8Copy(Line, 1, LGen)
      else
        Linegroup := UTF8Copy(Line, 1, LArt);
      if Linegroup <> OldGroup then begin
        if Format = 'html' then begin
          // HTML: Add a header row to the table
          Writeln(FH, '<tr><td colspan="10" style="background:blue; color:white">', UTF8UpperCase(UTF8Trim(LineGroup)), '</td></tr>');
          Colour := 'AliceBlue';
        end
        else begin
          // TXT: Add a header line, underlined with '=' symbols
          if OldGroup <> '' then
            Writeln(FH);                                                       // add empty line at end of group
          Writeln(FH, UTF8UpperCase(UTF8Trim(LineGroup)));
          Writeln(FH, CharN('=', UTF8Length(UTF8Trim(LineGroup))));
        end;
        OldGroup := LineGroup;
      end;
      UTF8Delete(Line, 1, UTF8Length(LineGroup));                              // remove the data corr. to the group (will not be displayed in "normal" row/line)
    end;
    if Format = 'text' then begin
      // TXT: Just write the array element
      Writeln(FH, Line);
    end
    else begin
      // TXT or CSV: Create output-file-dependent line
      Line2 := '';
      if Format = 'html' then begin
        // Begin of table row
        Line2 += '<tr style="background:' + Colour + '">';
      end;
      while Line <> '' do begin
        // Use the 5 blanks separating the different info parts of the line to extract these different parts
        // Delete the extracted part and continue this until there isn't nothing more left
        P := UTF8Pos('     ', Line);
        if P = 0 then begin
          // Last info part
          Item := UTF8Trim(Line);
          Line := '';
        end
        else begin
          // There are other info parts remaining
          Item := UTF8Trim(UTF8Copy(Line, 1, P));
          UTF8Delete(Line, 1, P); Line := UTF8Trim(Line);
        end;
        if Format = 'html' then begin
          // HTML: Create a table column with the info value, followed by a spacing column
          Line2 += '<td>' + Item + '</td>';
          if Line <> '' then
            Line2 += '<td width="25px">&nbsp;</td>';
        end
        else begin
          // CSV: Put data between double quotes and use comma as separator
          Line2 += '"' + Item + '"';
          if Line <> '' then
            Line2 += ',';
        end;
      end;
      if Format = 'html' then begin
        // End of table row
        Line2 += '</tr>';
        // Alternate table row column for better lisibility
        if Colour = 'AliceBlue' then
          Colour := 'White'
        else
          Colour := 'AliceBlue';
      end;
      // Write the HTML/CSV line to the file
      Writeln(FH, Line2);
    end;
  end;
  if Format = 'html' then begin
    // HTML end of document additions
    Line2 := '</table>' + LineEnding;
    Line2 += '</html>' + LineEnding;
    Writeln(FH, Line2);
  end;
  Close(FH);
end;

{*********}
{ TfMusic }
{*********}

{ Application start: Initialisations }

procedure TfMusic.FormCreate(Sender: TObject);

begin
  sList := 'all'; sAlbumsId := 'album'; bAlbumFolders := False;
  iGroup := 1;  iSort := 1;
  sFormat := 'html';
  bPriv := True; bPub := True; bCustom := True;
  sDir := GetCurrentDir;
  Libs := TLibraries.Create(False);                                            // TLibraries object as defined in WinLibs unit
end;

{ Menu item "File > Exit": Exit application }

procedure TfMusic.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > List contents > All files": Selection of list with all MP3 found }

procedure TfMusic.mSettingsContentsFilesClick(Sender: TObject);

begin
  mSettingsContentsFiles.Checked := True; mSettingsContentsAlbums.Checked := False;
  mSettingsContentsTrackTitle.Enabled := False; mSettingsContentsFileName.Enabled := False;
  mSettingsAlbumFolders.Enabled := False;
  mSettingsSort5.Enabled := True;
  sList := 'all';
end;

{ Menu item "Settings > List contents > Albums only": Selection of list with only album MP3 files }

procedure TfMusic.mSettingsContentsAlbumsClick(Sender: TObject);

begin
  mSettingsContentsFiles.Checked := False; mSettingsContentsAlbums.Checked := True;
  mSettingsContentsTrackTitle.Enabled := True; mSettingsContentsFileName.Enabled := True;
  mSettingsAlbumFolders.Enabled := True;
  if mSettingsSort5.Checked then begin
    // Sort on song title makes no sense with albums list
    mSettingsSort5.Checked := False;
    mSettingsSort3.Checked := True;
    iSort := 3;
  end;
  mSettingsSort5.Enabled := False;
  sList := 'albums';
end;

{ Menu items "Settings > List contents > Albums identified by ...": Select album identification (in track title or in file name) }

procedure TfMusic.mSettingsContentsTrackTitleClick(Sender: TObject);

begin
  mSettingsContentsTrackTitle.Checked := True; mSettingsContentsFileName.Checked := False;
  sAlbumsId := 'title';
end;

procedure TfMusic.mSettingsContentsFileNameClick(Sender: TObject);

begin
  mSettingsContentsTrackTitle.Checked := False; mSettingsContentsFileName.Checked := True;
  sAlbumsId := 'file';
end;

{ Menu item "Settings > List contents > Search for album folders": Toggle to search or not for album folders }

procedure TfMusic.mSettingsAlbumFoldersClick(Sender: TObject);

begin
  if mSettingsAlbumFolders.Checked then
    mSettingsAlbumFolders.Checked := False
  else
    mSettingsAlbumFolders.Checked := True;
  bAlbumFolders := mSettingsAlbumFolders.Checked;
end;

{ Menu items "Settings > List format > ...": Selection of output format (HTML, TXT, CSV) }

procedure TfMusic.mSettingsFormat1Click(Sender: TObject);

begin
  mSettingsFormat1.Checked := True; mSettingsFormat2.Checked := False; mSettingsFormat3.Checked := False;
  sFormat := 'html';
end;

procedure TfMusic.mSettingsFormat2Click(Sender: TObject);

begin
  mSettingsFormat1.Checked := False; mSettingsFormat2.Checked := True; mSettingsFormat3.Checked := False;
  sFormat := 'text';
end;

procedure TfMusic.mSettingsFormat3Click(Sender: TObject);

begin
  mSettingsFormat1.Checked := False; mSettingsFormat2.Checked := False; mSettingsFormat3.Checked := True;
  sFormat := 'csv';
end;

{ Menu items "Settings > List sort order > [Grouping]": Selection of how the music files should be grouped }

procedure TfMusic.mSettingsGroup1Click(Sender: TObject);

begin
  mSettingsGroup1.Checked := True; mSettingsGroup2.Checked := False; mSettingsGroup0.Checked := False;
  iGroup := 1;
end;

procedure TfMusic.mSettingsGroup2Click(Sender: TObject);

begin
  mSettingsGroup1.Checked := False; mSettingsGroup2.Checked := True; mSettingsGroup0.Checked := False;
  iGroup := 2;
end;

procedure TfMusic.mSettingsGroup0Click(Sender: TObject);

begin
  mSettingsGroup1.Checked := False; mSettingsGroup2.Checked := False; mSettingsGroup0.Checked := True;
  iGroup := 0;
end;

{ Menu items "Settings > List sort order > [Sorting]": Selection of how the music files should be sorted }

procedure TfMusic.mSettingsSort1Click(Sender: TObject);

begin
  mSettingsSort1.Checked := True; mSettingsSort2.Checked := False; mSettingsSort3.Checked := False;
  mSettingsSort4.Checked := False; mSettingsSort5.Checked := False;
  isort := 1;
end;

procedure TfMusic.mSettingsSort2Click(Sender: TObject);

begin
  mSettingsSort1.Checked := False; mSettingsSort2.Checked := True; mSettingsSort3.Checked := False;
  mSettingsSort4.Checked := False; mSettingsSort5.Checked := False;
  isort := 2;
end;

procedure TfMusic.mSettingsSort3Click(Sender: TObject);

begin
  mSettingsSort1.Checked := False; mSettingsSort2.Checked := False; mSettingsSort3.Checked := True;
  mSettingsSort4.Checked := False; mSettingsSort5.Checked := False;
  isort := 3;
end;

procedure TfMusic.mSettingsSort4Click(Sender: TObject);

begin
  mSettingsSort1.Checked := False; mSettingsSort2.Checked := False; mSettingsSort3.Checked := False;
  mSettingsSort4.Checked := True; mSettingsSort5.Checked := False;
  isort := 4;
end;

procedure TfMusic.mSettingsSort5Click(Sender: TObject);

begin
  mSettingsSort1.Checked := False; mSettingsSort2.Checked := False; mSettingsSort3.Checked := False;
  mSettingsSort4.Checked := False; mSettingsSort5.Checked := True;
  isort := 5;
end;

{ Menu item "Help > Help": Display application help }

procedure TfMusic.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfMusic.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Music files list.' + LineEnding;
  S += 'Sorted formatted text, CSV or HTML list of MP3 files stored in the Windows "Music" library.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June-July 2022.';
  MessageDlg('About "MusicList"', S, mtInformation, [mbOK], 0);
end;

{ Libraries checkboxes selection/unselection (by user) }

procedure TfMusic.cbLibPrivateChange(Sender: TObject);

begin
  bPriv := cbLibPrivate.Checked;
end;

procedure TfMusic.cbLibPublicChange(Sender: TObject);

begin
  bPub := cbLibPublic.Checked;
end;

procedure TfMusic.cbLibCustomChange(Sender: TObject);

begin
  bCustom := cbLibCustom.Checked;
end;

{ Button "Search files": Search all MP3 files in the "music" library (libraries) selected by the user }

procedure TfMusic.btSearchClick(Sender: TObject);

var
  N, I, J: Integer;
  Temp: string;

begin
  N := 0; SetLength(aMusicPaths, 0); SetLength(aMusicFiles, 0);
  edList.Lines.Clear;
  btList.Enabled := False;
  Folders := Libs.GetLibraryFolderInfo('Music', bPriv, bPub, bCustom);         // get folders being part of "Muisc" (WinLibs unit)
  for I := 0 to Length(Folders) - 1 do begin
    // Get music files (.mp3 files) list
    slMusicFilesList := FindAllFiles(Folders[I].FolderPath, '*.mp3', True);    // get all MP3 files for this "Music" folder
    // Store music file paths and names into arrays
    for J := 0 to slMusicFilesList.Count - 1 do begin
      Inc(N); SetLength(aMusicPaths, N); SetLength(aMusicFiles, N);
      aMusicPaths[N - 1] := slMusicFilesList.Strings[J];
      aMusicFiles[N - 1] := StringReplace(ExtractFilename(slMusicFilesList.Strings[J]), '.mp3', '', [rfReplaceAll]);
    end;
    slMusicFilesList.Free;
  end;
  if Length(aMusicFiles) = 0 then begin
    edList.Lines.Append('No MP3 files found...');
  end
  else begin
    // Sort filenames by name and fill them into memo
    for I := 0 to Length(aMusicFiles) - 2 do begin
      for J := I + 1 to Length(aMusicFiles) - 1 do begin
        if aMusicFiles[J] < aMusicFiles[I] then begin
          Temp := aMusicFiles[I]; aMusicFiles[I] := aMusicFiles[J]; aMusicFiles[J] := Temp;
        end;
      end;
    end;
    for I := 0 to Length(aMusicFiles) - 1 do
      edList.Lines.Append(aMusicFiles[I]);
    edList.Lines.Append(LineEnding + 'Total number of MP3 files = ' + IntToStr(Length(aMusicFiles)));
    btList.Enabled := True;
  end;
end;

{ Button "Create list": Create music files list (as defined by the different settings) }

procedure TfMusic.btListClick(Sender: TObject);

var
  N, I, P1, P2: Integer;
  Filename, Album, OldAlbum: string;
  DoInsert: Boolean;

begin
  N := 0; SetLength(aAllMP3Files, Length(aMusicPaths)); SetLength(aMP3Files, 0);
  OldAlbum := '';
  for I := 0 to Length(aMusicPaths) - 1 do begin
    // Proceed file after file
    ReadMP3Tags(aMusicPaths[I], aAllMP3Files[I]);                              // get MP3 tags for this file (ID3v2 unit)
    DoInsert := True;
    // If the list has to be an albums-only list, eliminate all non-whole-album files
    // (identified by MP3 Title tag or filename, depending on setttings)
    if sList = 'albums' then begin
      if sAlbumsId = 'file' then begin
        P1 := UTF8Pos('Album', ExtractFilename(aMusicPaths[I]));
        P2 := UTF8Pos('album', ExtractFilename(aMusicPaths[I]));
        if (P1 = 0) and (P2 = 0) then
          DoInsert := False;
      end
      else begin
        P1 := UTF8Pos('Album', aAllMP3Files[I].Title);
        P2 := UTF8Pos('album', aAllMP3Files[I].Title);
        if (P1 = 0) and (P2 = 0) then
          DoInsert := False;
      end;
    end;
    // For non-whole-album files, check if they are located in a subfolder with the same name as the album title (MP3 Album tag)
    // If this is the case, you'll have to add this album to the list (otherwise individual-track-albums would not be listed)
    // Do this check only if search for album folders is selected and do it only once for the concerned subfolder
    if not DoInsert then begin
      if mSettingsAlbumFolders.Checked then begin
        if aAllMP3Files[I].Album <> OldAlbum then begin
          Album := UTF8Copy(ExtractFileDir(aMusicPaths[I]), Length(ExtractFileDir(aMusicPaths[I])) - Length(aAllMP3Files[I].Album) + 1, Length(aAllMP3Files[I].Album));
          if Album = aAllMP3Files[I].Album then
            DoInsert := True;
          OldAlbum := aAllMP3Files[I].Album;
        end;
      end;
    end;
    // If the file is one of those that should be in the list, add it to the (definitive) list now
    if DoInsert then begin
      Inc(N); SetLength(aMP3Files, N);
      aMP3Files[N - 1] := aAllMP3Files[I];
    end;
  end;
  // Save the list (as .html, .csv or .txt file)
  dlgSaveList.InitialDir := sDir;
  dlgSaveList.FileName := '';
  if sFormat = 'html' then
    dlgSaveList.Filter := 'HTML document|*.html'
  else if sFormat = 'csv' then
    dlgSaveList.Filter := 'CSV file|*.csv'
  else
    dlgSaveList.Filter := 'Formatted text file|*.txt';
  if dlgSaveList.Execute then begin
    // User has selected a file
    Filename := dlgSaveList.FileName;
    sDir := ExtractFileDir(Filename);                                          // save directory (to open next time at same location)
    MusicList(aMP3Files, iGroup, iSort, sFormat, Filename);                    // generate the list (create the file)
  end;
end;


end.
