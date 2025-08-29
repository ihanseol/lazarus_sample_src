{***************************************}
{* Main unit for SearchMP3 application *}
{***************************************}

unit mp3serach;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, PopupNotifier, ExtCtrls,
  DateUtils, LCLIntf, Windows, ID3v2, mp3files;

const
  GenresMax = 50;
  GFileName = 'genres.txt';
  FilesAllMax = 7500;                                                          // arbitrary limit of MP3 file number in search directory (file list)

type
  TGenres = array[1..GenresMax] of string;
  TFilesAll = array[1..FilesAllMax] of Integer;
  { TfSearchMP3 }
  TfSearchMP3 = class(TForm)
    mMenu: TMainMenu;
    mFile: TMenuItem;
    mFileExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsTitleOR, mOptionsCase, mOptionsCreationDate: TMenuItem;
    MenuItem1: TMenuItem;
    mOptionsFileInfo, mOptionsFullPath: TMenuItem;
    MenuItem2: TMenuItem;
    mOptionsExcludeMain, mOptionsExcludePers: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    rbSearchFiles: TRadioButton;
    cbFileName: TCheckBox;    cboFileName: TComboBox;    edFileName: TEdit;
    cbFileDate: TCheckBox;    cboFileDate: TComboBox;    edFileDate1, edFileDate2: TEdit;
    cbFileTime: TCheckBox;    cboFileTime: TComboBox;    edFileTime1, edFileTime2: TEdit;
    cbFileSize: TCheckBox;    cboFileSize: TComboBox;    edFileSize1, edFileSize2: TEdit;
    rbSearchTags: TRadioButton;
    cbArtistName: TCheckBox;  cboArtistName: TComboBox;  edArtistName: TEdit;
    cbAlbumName: TCheckBox;   cboAlbumName: TComboBox;   edAlbumName: TEdit;
    cbTrackName: TCheckBox;   cboTrackName: TComboBox;   edTrackName: TEdit;
    cbAlbumGenre: TCheckBox;  cboGenres: TComboBox;
    cbReleaseYear: TCheckBox;   cboReleaseYear: TComboBox;   edReleaseYear1, edReleaseYear2: TEdit;
    cbPlayTime: TCheckBox; cboPlayTime: TComboBox; edPlayTime1, edPlayTime2: TEdit;
    Label1: TLabel; edDir: TEdit; imDir: TImage; cbSubDirs: TCheckBox;
    btSearch: TButton;
    memoHelp: TMemo;
    pnAbout: TPopupNotifier;
    dlgDir: TSelectDirectoryDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsTitleORClick(Sender: TObject);
    procedure mOptionsCaseClick(Sender: TObject);
    procedure mOptionsCreationDateClick(Sender: TObject);
    procedure mOptionsFileInfoClick(Sender: TObject);
    procedure mOptionsFullPathClick(Sender: TObject);
    procedure mOptionsExcludeMainClick(Sender: TObject);
    procedure mOptionsExcludePersClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure imDirClick(Sender: TObject);
    procedure cbSubDirsChange(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure cbFileNameChange(Sender: TObject);
    procedure cbFileDateChange(Sender: TObject);
    procedure cbFileTimeChange(Sender: TObject);
    procedure cbFileSizeChange(Sender: TObject);
    procedure cbArtistNameChange(Sender: TObject);
    procedure cbAlbumNameChange(Sender: TObject);
    procedure cbTrackNameChange(Sender: TObject);
    procedure cbAlbumGenreChange(Sender: TObject);
    procedure cbReleaseYearChange(Sender: TObject);
    procedure cbPlayTimeChange(Sender: TObject);
    procedure cboFileNameChange(Sender: TObject);
    procedure cboFileDateChange(Sender: TObject);
    procedure cboFileTimeChange(Sender: TObject);
    procedure cboFileSizeChange(Sender: TObject);
    procedure cboArtistNameChange(Sender: TObject);
    procedure cboAlbumNameChange(Sender: TObject);
    procedure cboTrackNameChange(Sender: TObject);
    procedure cboGenresChange(Sender: TObject);
    procedure cboReleaseYearChange(Sender: TObject);
    procedure cboPlayTimeChange(Sender: TObject);
    procedure edFileNameChange(Sender: TObject);
    procedure edFileDate1Change(Sender: TObject);
    procedure edFileDate2Change(Sender: TObject);
    procedure edFileTime1Change(Sender: TObject);
    procedure edFileTime2Change(Sender: TObject);
    procedure edFileSize1Change(Sender: TObject);
    procedure edFileSize2Change(Sender: TObject);
    procedure edArtistNameChange(Sender: TObject);
    procedure edAlbumNameChange(Sender: TObject);
    procedure edTrackNameChange(Sender: TObject);
    procedure edReleaseYear1Change(Sender: TObject);
    procedure edReleaseYear2Change(Sender: TObject);
    procedure edPlayTime1Change(Sender: TObject);
    procedure edPlayTime2Change(Sender: TObject);
  private
    iGenres, iFilesTotal, iFilesFound: Integer;
    sInitialDir, sDir, sSavedDir, sMP3File: string;
    bFindFiles, bSubDirs, bSubDirsDone, bORTitle, bCreationDate, bIgnoreCase, bFileInfo, bFullPath, bExcludeMain, bExcludePers, bEoF: Boolean;
    aGenres: TGenres;
    aFilesAll: TFilesAll;
    aFilesFound: TFilesFound;
  end;

var
  fSearchMP3: TfSearchMP3;
  MP3FileList: TStringList;
  TempFile: Text;

implementation

{$R *.lfm}

{ Swap integer numbers }

procedure SwapInteger(var Int1, Int2: Integer);

var
  Temp: Integer;

begin
  Temp := Int1; Int1 := Int2; Int2 := Temp;
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

{ Check if a string is a multi-word text or a single word }

function IsWord(Str: string): Boolean;

// The string is considered to be a single word, if
//   1. it starts with a space
//   2. it ends with a space or end with one of the valid characters defined below, followed by a space
// Other cases (such as apostrophe or brackets) are NOT handled by this routine

const
  ValidCharsAfter: array[1..7] of string = (
    ' ', '.', ',', ';', '!', '?', ':'
  );

var
  I: Integer;
  OK: Boolean;

begin
  OK := True;
  if LeftStr(Str, 1) <> ' ' then
    // First character isn't a space
    OK := False
  else begin
    OK := False;
    for I := 1 to 7 do begin
      if Copy(Str, Length(Str) - 1, 1) = ValidCharsAfter[I] then
        OK := True;
      if Copy(Str, Length(Str) - 1, 1) <> ' ' then begin
        if RightStr(Str, 1) <> ' ' then
          OK := False;
      end;
    end;
  end;
  IsWord := OK;
end;

{ Check if a string contains a given text or word (at beginning, end or any position) }

function FindString(Str, StrSub, StrPos, StrType: string): Boolean;

const
  ValidCharsAfter: array[1..7] of string = (
    ' ', '.', ',', ';', '!', '?', ':'
  );

var
  P, I: Integer;
  Found: Boolean;

begin
  Found := False;
  P := Pos(StrSub, Str);
  if P > 0 then begin
    // String contains the text
    if StrPos = 'begin' then begin
      // Text has to be at beginning of string
      if P = 1 then begin
        if StrType = 'text' then
          // Simple text search at beginning of string
          Found := True
        else begin
          // Word search at beginning at string
          Str := ' ' + Str;                                                    // word at beginning of string is like preceded by a space
          if IsWord(Copy(Str, 1, Length(StrSub) + 3)) then
            Found := True;
        end;
      end;
    end
    else if StrPos = 'end' then begin
      // Text has to be at end of string
      if RightStr(Str, Length(StrSub)) = StrSub then begin
        // Text has been found at end of string
        if StrType = 'text' then
          // Simple text search at end of string
          Found := True
        else begin
          // Word serach at end of string
          Str += '  ';                                                         // add 2 spaces as the IsWord function analyses the 2 last characters
          if IsWord(Copy(Str, P - 1, Length(StrSub) + 3)) then
            Found := True;
        end;
      end
      else if Copy(Str, Length(Str) - Length(StrSub), Length(StrSub)) = StrSub then begin
        // Text has been found at end of string, but followed by 1 character
        if StrType = 'text' then begin
          // Simple text search: Last character must be one of the valid ones defines
          for I := 1 to 7 do begin
            if RightStr(Str, 1) = ValidCharsAfter[I] then
              Found := True;
          end;
        end
        else begin
          // Word search: Validation of the last character may be done by the IsWord routine
          Str += ' ';                                                          // // word at end of string is like followed by a space
          if IsWord(Copy(Str, P - 1, Length(StrSub) + 3)) then
            Found := True;
        end;
      end;
    end
    else begin
      // Text at any position of string
      if StrType = 'text' then
        // Simple text search
        Found := True
      else begin
        // Word search
        if P = 1 then                                                          // text found at beginning of string
          Str := ' ' + Str
        else if P = Length(Str) - Length(StrSub) + 1 then                      // text found at end of string
          Str += '  '
        else if P = Length(Str) - Length(StrSub) then                          // text found at end of string, followed by one character
          Str += ' ';
        if IsWord(Copy(Str, P - 1, Length(StrSub) + 3)) then
          Found := True;
      end;
    end;
  end;
  FindString := Found;
end;

{ Search for text or word, at beginning, end or any position, depending on user selection }

function FindStrings(SearchOptions: TComboBox; SearchString, SearchSubString: string; IgnoreCase: Boolean): Boolean;

var
  Found: Boolean;

begin
  Found := False;
  if IgnoreCase then begin
    SearchString := LowerCase(SearchString);
    SearchSubString := LowerCase(SearchSubString);
  end;
  case SearchOptions.ItemIndex of
    0: if SearchSubString = SearchString then Found := True;
    1: Found := FindString(SearchString, SearchSubString, 'all', 'text');
    2: Found := FindString(SearchString, SearchSubString, 'all', 'word');
    3: Found := FindString(SearchString, SearchSubString, 'begin', 'text');
    4: Found := FindString(SearchString, SearchSubString, 'begin', 'word');
    5: Found := FindString(SearchString, SearchSubString, 'end', 'text');
    6: Found := FindString(SearchString, SearchSubString, 'end', 'word');
  end;
  FindStrings := Found;
end;

{ Search for number bigger or less than other one or between 2 other ones, depending on user selection }

function FindNumber(SearchOptions: TComboBox; Number, SearchNumber1, SearchNumber2: Real): Boolean;

var
  Found: Boolean;

begin
  Found := False;
  case SearchOptions.ItemIndex of
    0: if Number = SearchNumber1 then
         Found := True;
    1: if Number > SearchNumber1 then
         Found := True;
    2: if Number < SearchNumber1 then
         Found := True;
    3: if (Number >= SearchNumber1) and (Number <= SearchNumber2) then
         Found := True;
  end;
  FindNumber := Found;
end;

{ Read music genres from text file }

procedure ReadGenres(var Genres: TGenres; var N: Integer);

var
  Genre: string;
  GFile: Text;

begin
  Assign(GFile, GFileName); Reset(GFile); N := 0;
  while not EoF(GFile) and (N < GenresMax) do begin
    Readln(GFile, Genre);
    if Genre <> '' then begin
      Inc(N);
      Genres[N] := Genre;
    end;
  end;
  Close(GFile);
end;

{ Fill music genres combobox }

procedure FillGenres(var Genres: TGenres; N: Integer; ExclMain, ExclPers: Boolean);

var
  I: Integer;
  Include: Boolean;

begin
  fSearchMP3.cboGenres.Clear;
  for I := 1 to N do begin
    Include := True;
    if (RightStr(Genres[I], 3) = '(M)') and ExclMain then                      // Non-ID3 main genres to exclude (if selected)
      Include := False;
    if (RightStr(Genres[I], 3) = '(-)') and ExclPers then                      // (My) personal genres to exclude (if selected)
      Include := False;
    if Include then
      fSearchMP3.cboGenres.Items.Append(Genres[I]);
  end;
end;

{ Get file info search criteria (as selected by user) }

procedure GetFileCriteria(var Name: string; var Date1, Date2, Size1, Size2: Integer; var ErrMess: string);

var
  FileTime1, FileTime2: Integer;
  YY, MM, DD: Word;
  FileDate1, FileDate2: TDateTime;

begin
  Name := '';
  Date1 := 0; Date2 := 0;
  Size1 := 0; Size2 := 0;
  ErrMess := '';
  // File name
  if fSearchMP3.cbFileName.Checked then begin
    if fSearchMP3.edFileName.Text = '' then
      ErrMess := 'No file name specified!'
    else
      Name := fSearchMP3.edFileName.Text;
  end;
  // File dates
  if (ErrMess = '') and fSearchMP3.cbFileDate.Checked then begin
    if not TryStrToDate(fSearchMP3.edFileDate1.Text, FileDate1) then
      ErrMess := 'The date''s format is different from dd.mm.yyyy!';
    if (ErrMess = '') and (fSearchMP3.cboFileDate.ItemIndex = 3) then begin    // itemindex = 3 is 'between'
      if not TryStrToDate(fSearchMP3.edFileDate2.Text, FileDate2) then
        ErrMess := 'The end date''s format is different from dd.mm.yyyy!';
    end;

    if ErrMess = '' then begin
      DecodeDate(FileDate1, YY, MM, DD);
      Date1 := 10000 * YY + 100 * MM + DD;                                     // transform dates to integer (easier to handle/compare)
      if fSearchMP3.cboFileDate.ItemIndex = 3 then begin
        DecodeDate(FileDate2, YY, MM, DD);
        Date2 := 10000 * YY + 100 * MM + DD;
        if Date1 > Date2 then                                                  // swap dates if start < end
          SwapInteger(Date1, Date2);
      end;
    end;
  end;
  // File times (days passed since modification/creation)
  if (ErrMess = '') and fSearchMP3.cbFileTime.Checked then begin
    if fSearchMP3.edFileTime1.Text = '' then
      ErrMess := 'No file time specified!';
    if ErrMess = '' then begin
      FileTime1 := StrToInt(fSearchMP3.edFileTime1.Text);
      DecodeDate(IncDay(Date, -FileTime1), YY, MM, DD);                        // calculate date corresponding to time passed
      Date1 := 10000 * YY + 100 * MM + DD;                                     // and transform to integer (as above)
      if fSearchMP3.cboFileTime.ItemIndex = 3 then begin
        if fSearchMP3.edFileTime2.Text = '' then
          ErrMess := 'No ending file time specified!';
        if ErrMess = '' then begin
          FileTime2 := StrToInt(fSearchMP3.edFileTime2.Text);
          DecodeDate(IncDay(Date, -FileTime2), YY, MM, DD);
          Date2 := 10000 * YY + 100 * MM + DD;
          if Date1 > Date2 then
            SwapInteger(Date1, Date2);
        end;
      end;
    end;
  end;
  // File size
  if (ErrMess = '') and fSearchMP3.cbFileSize.Checked then begin
    if fSearchMP3.edFileSize1.Text = '' then
      Size1 := 0
    else
      Size1 := StrToInt(fSearchMP3.edFileSize1.Text);
    if Size1 = 0 then
      ErrMess := 'File size must be greater than 0!';
    if (ErrMess = '') and (fSearchMP3.cboFileSize.ItemIndex = 3) then begin    // itemindex = 3 is 'between'
      if fSearchMP3.edFileSize2.Text = '' then
        Size2 := 0
      else
        Size2 := StrToInt(fSearchMP3.edFileSize2.Text);
      if Size2 = 0 then
        ErrMess := 'End file size must be greater than 0!'
      else begin
        if Size1 > Size2 then
          SwapInteger(Size1, Size2);                                           // swap sizes if start < end
      end;
    end;
  end;
end;

{ Get MP3 tags info search criteria (as selected by user) }

procedure GetTagsCriteria(var Artist, Album, Track, Genre: string; var Year1, Year2: Integer; var ErrMess: string);

begin
  Artist := ''; Album := ''; Track := ''; Genre := '';
  Year1 := 0; Year2 := 0;
  ErrMess := '';
  // Artist name
  if fSearchMP3.cbArtistName.Checked then begin
    if fSearchMP3.edArtistName.Text = '' then
      ErrMess := 'No artist name specified!'
    else
      Artist := fSearchMP3.edArtistName.Text;
  end;
  // Album stTitle
  if (ErrMess = '') and fSearchMP3.cbAlbumName.Checked then begin
    if fSearchMP3.edAlbumName.Text = '' then
      ErrMess := 'No album title specified!'
    else
      Album := fSearchMP3.edAlbumName.Text;
  end;
  // Track stTitle
  if (ErrMess = '') and fSearchMP3.cbTrackName.Checked then begin
    if fSearchMP3.edTrackName.Text = '' then
      ErrMess := 'No track title specified!'
    else
      Track := fSearchMP3.edTrackName.Text;
  end;
  // Music genre
  if (ErrMess = '') and fSearchMP3.cbAlbumGenre.Checked then begin
    if fSearchMP3.cboGenres.Text = '' then
      ErrMess := 'No album genre specified!'
    else begin
      Genre := fSearchMP3.cboGenres.Text;
      Genre := StringReplace(Genre, ' (M)', '', []);                           // in the combobox (M) identifies non-ID3 main music genres
      Genre := StringReplace(Genre, ' (-)', '', []);                           // in the combobox (-) identifies (my) personal music genres
    end;
  end;
  // Album year
  if (ErrMess = '') and fSearchMP3.cbReleaseYear.Checked then begin
    if fSearchMP3.edReleaseYear1.Text = '' then
      ErrMess := 'No album year specified!';
    if ErrMess = '' then begin
      Year1 := StrToInt(fSearchMP3.edReleaseYear1.Text);
      if fSearchMP3.cboReleaseYear.ItemIndex = 3 then begin                      // itemindex = 3 is 'between'
        if fSearchMP3.edReleaseYear2.Text = '' then
          ErrMess := 'No album year specified!'
        else begin
          Year2 := StrToInt(fSearchMP3.edReleaseYear2.Text);
          if Year1 > Year2 then
            SwapInteger(Year1, Year2);                                         // swap years if start < end
        end;
      end;
    end;
  end;
end;

{ Read MP3 tags (using the ID3v2 unit) }

procedure ReadMP3Tags(var MP3File, SArtist, SAlbum, STitle, STrack, SGenre, SYear: string);

var
  MP3Tags: TID3v2;

begin
  MP3Tags := TID3v2.Create;
  try
    with MP3Tags do begin
      ReadFromFile(MP3File);
      SArtist := StringTransform(Artist);
      SAlbum := StringTransform(Album);
      STitle := StringTransform(Title);
      SYear  := Year;
      SGenre := StringTransform(Genre);
      STrack := IntToStr(Track);
    end;
  finally
    MP3Tags.Free;
  end;
end;

{ Sorting search results (in fFilesMP3 memo filed), updating the files-found array file indexes }

procedure SRQuickSort(var FilesFound: TFilesFound; NFilesFound: Cardinal);

  // Quicksort routine from: Referat am 18.11.1996 in Proseminar "Grundlagen der Programmierung" (Florian Michahelles, 1997)

  procedure Partition(Left, Right: Cardinal);

  var
    BArrow, TArrow: Cardinal;
    ITemp: Integer;
    Pivot, STemp: string;

  begin
    BArrow := Left;
    TArrow := Right;
    Pivot := fFilesMP3.memoFiles.Lines[(Left + Right) div 2];
    repeat
      while fFilesMP3.memoFiles.Lines[BArrow - 1] < Pivot do
        Inc(BArrow);
      while fFilesMP3.memoFiles.Lines[TArrow - 1] > Pivot do
        Dec(TArrow);
      if BArrow <= TArrow then begin
        STemp := fFilesMP3.memoFiles.Lines[BArrow - 1]; ITemp := FilesFound[BArrow];
        fFilesMP3.memoFiles.Lines[BArrow - 1] := fFilesMP3.memoFiles.Lines[TArrow - 1]; FilesFound[BArrow] := FilesFound[TArrow];
        fFilesMP3.memoFiles.Lines[TArrow - 1] := STemp; FilesFound[TArrow] := ITemp;
        Inc(BArrow);
        Dec(TArrow);
      end;
    until BArrow > TArrow;
    if Left < TArrow then
      Partition(Left, TArrow);
    if Right > BArrow then
      Partition(BArrow, Right);
  end;

begin
  Partition(1, NFilesFound);
end;

{***************}
{* TfSearchMP3 *}
{***************}

{ Application start: Initialisation }

procedure TfSearchMP3.FormCreate(Sender: TObject);

begin
  // Directories
  sInitialDir := GetUserDir;
  sDir := sInitialDir + 'Music';                                               // default start = user's Music directory
  bSubDirs := True; bSubDirsDone := False;
  if not DirectoryExists(sDir) then begin
    sInitialDir := 'C:'; sDir := GetUserDir;                                   // if no Music directory found start at top level on C drive
  end;
  edDir.Text := sDir;
  bSubDirs := True;                                                            // default search = including subdirectories
  // Fill music genres combobox from text file
  ReadGenres(aGenres, iGenres);
  bExcludeMain := False; bExcludePers := False;
  FillGenres(aGenres, iGenres, bExcludeMain, bExcludePers);
  // Default values for Options menu items
  bORTitle := True; bIgnoreCase := True; bCreationDate := False;
  bFullPath := True; bFileInfo := False;
  // Other variables initialisation
  iFilesTotal := 0;                                                            // number of directory files found (lines of temporary text file)
  iFilesFound := 0;                                                            // number of files found by actual search
  bFindFiles := True;                                                          // directory search (with temporary file creation) has to be done
end;

{ Application exit: Delete temporary file }

procedure TfSearchMP3.FormClose(Sender: TObject);

begin
  DeleteFile(TempFileName);
end;

{ Menu item "File > Exit": Exit the application }

procedure TfSearchMP3.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Use OR instead of AND for simultaneous album/track title searches": set "use and/or in album/track title searches" flag }

procedure TfSearchMP3.mOptionsTitleORClick(Sender: TObject);

begin
  if mOptionsTitleOR.Checked then
    mOptionsTitleOR.Checked := False
  else
    mOptionsTitleOR.Checked := True;
  bORTitle := mOptionsTitleOR.Checked;                                         // if unchecked, use OR instead of AND
end;

{ Menu item "Options > Ignore upper/lower case when searching text fields": set "ignore/consider letter case when searching" flag }

procedure TfSearchMP3.mOptionsCaseClick(Sender: TObject);

begin
  if mOptionsCase.Checked then
    mOptionsCase.Checked := False
  else
    mOptionsCase.Checked := True;
  bIgnoreCase := mOptionsCase.Checked;                                         // if unchecked, uppercase/lowercase is cnsidered
end;

{ Menu item "Options > Use file creation date instead of file modification date": set "search using file modification/creation date" flag }

procedure TfSearchMP3.mOptionsCreationDateClick(Sender: TObject);

begin
  if mOptionsCreationDate.Checked then begin
    mOptionsCreationDate.Checked := False;
    cbFileDate.Caption := ' File modification date';
    cbFileTime.Caption := ' Time since file modification (days)';
  end
  else begin
    mOptionsCreationDate.Checked := True;
    cbFileDate.Caption := ' File creation date';
    cbFileTime.Caption := ' Time since file creation (days)';
  end;
  bCreationDate := mOptionsCreationDate.Checked;                               // if checked, creation date is used instead of modification date
end;

{ Menu item "Options > Display file information instead of MP3 tags": set "display MP3 tags/file information" flag}

procedure TfSearchMP3.mOptionsFileInfoClick(Sender: TObject);

begin
  if mOptionsFileInfo.Checked then
    mOptionsFileInfo.Checked := False
  else
    mOptionsFileInfo.Checked := True;
  bFileInfo := mOptionsFileInfo.Checked;                                       // if checked, file properties are displayed instead of MP3 tag info
end;

{ Menu item "Options > Display full path name of MP3 files found": set "display file name/path" flag }

procedure TfSearchMP3.mOptionsFullPathClick(Sender: TObject);

begin
  if mOptionsFullPath.Checked then
    mOptionsFullPath.Checked := False
  else
    mOptionsFullPath.Checked := True;
  bFullPath := mOptionsFullPath.Checked;                                       // if unchecked, the simple file name is displayed instead of the full path
end;

{ Menu item "Options > Exclude non-ID3 main genres from selection list": set "include/exclude non-ID3 main genres in genres list" flag }

procedure TfSearchMP3.mOptionsExcludeMainClick(Sender: TObject);

begin
  if mOptionsExcludeMain.Checked then
    mOptionsExcludeMain.Checked := False
  else
    mOptionsExcludeMain.Checked := True;
  bExcludeMain := mOptionsExcludeMain.Checked;                                 // if checked, non-ID3 main genres are excluded
  FillGenres(aGenres, iGenres, bExcludeMain, bExcludePers);                    // update the genres selection combobox
end;

{ Menu item "Options > Exclude personal genres from selection list": set "include/exclude (my) personal genres in genres list" flag }

procedure TfSearchMP3.mOptionsExcludePersClick(Sender: TObject);

begin
  if mOptionsExcludePers.Checked then
    mOptionsExcludePers.Checked := False
  else
    mOptionsExcludePers.Checked := True;
  bExcludePers := mOptionsExcludePers.Checked;                                 // if checked, (my) personal genres are excluded
  FillGenres(aGenres, iGenres, bExcludeMain, bExcludePers);                    // update the genres seelction combobox
end;

{ Menu item "Help > Help": Display program help }

procedure TfSearchMP3.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.txt');                                                    // open help file in text editor
end;

{ Menu item "Help > About": Display program info }

procedure TfSearchMP3.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'Search for MP3 files on local computer.' + Chr(13);
    S += 'Search may be done based on the file properties,' + Chr(13);
    S += 'as well as on the MP3 tags' + Chr(13) + Chr(13);
    S += '© allu, May, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ "Select directory" icon click: Choose actual search directory }

procedure TfSearchMP3.imDirClick(Sender: TObject);

var
  L, P: Integer;
  S: string;

// The variables set by this routine, affect the behaviour of the "TfSearchMP3.btSearchClick" method:
//  - sDir: current search directory
//  - sInitialDir: directory that will be opened in the "Select directory" dialog
//  - bFindFiles: if set True, a new FindAllFiles search has to be done in the current search directory (see below)


begin
  dlgDir.InitialDir := sInitialDir;
  dlgDir.FileName := sDir;
  // Set variables if the user selected a directory
  if dlgDir.Execute then begin
    // Select directory to be used as actual search directory
    sDir := dlgDir.FileName;
    if RightStr(sDir, 1) = '\' then
      sDir := LeftStr(sDir, Length(sDir) - 1);
    edDir.Text := sDir;
    // Set initial directory (for next "Select directory") to 1 level above actual search directory
    S := sDir; L := 0;
    while S <> '' do begin
      P := Pos('\', S);
      if P > 0 then begin
        S := RightStr(S, Length(S) - P);
        L := Length(S);
      end
      else
        S := '';
    end;
    if L = 0 then
      sInitialDir := sDir
    else
      sInitialDir := LeftStr(sDir, Length(sDir) - L - 1);
    // Set bFindFiles variable:
    // If set to True, the next time the "Search" button is pressed, a list of all MP3 files in actual search
    // directory (and its subdirectories if this is selected) is created (using the FindAllFiles statement) and
    // the filenames are written to a temporary text file (SearchMP3.txt). The flag is only set if the search
    // will be done outside the directory searched before or if subdirectories are to be searched and were not
    // before (as otherwise, the temporary file already contains all MP3 file info needed)
    bFindFiles := False;
    P := Pos(sSavedDir, sDir);
    if (P = 0) or not bSubDirsDone then
      bFindFiles := True;
  end;
end;

{ Checking/unchecking "Include subdirectories": set the "include subdirectries when searching" flag }

procedure TfSearchMP3.cbSubDirsChange(Sender: TObject);

begin
  bSubDirs := cbSubDirs.Checked;
end;

{ Button "Search": Execute the search and fill the memo on the fMP3Files form with the search results }

procedure TfSearchMP3.btSearchClick(Sender: TObject);

var
  FileDate, P, I, J: Integer;
  SearchDate1, SearchDate2, SearchSize1, SearchSize2, SearchYear1, SearchYear2: Integer;
  SearchFile, SearchArtist, SearchAlbum, SearchTrack, SearchGenre: string;
  MP3FilePath, MP3FileName, MP3Artist, MP3Album, MP3Title, MP3Genre, MP3Year, MP3Track, ErrMess, DateType, S: string;
  YY, MM, DD: Word;
  FileSize: Real;
  FileInDir, FileFound, FileFound1, FileFound2: Boolean;
  // Temporary variables used by the get file properties code
  FDate: TDateTime;
  MP3Search: TSearchRec;
  LFT : TFileTime;
  DT : LongRec;

begin
  // Get search criteria selected by user
  if rbSearchFiles.Checked then
    GetFileCriteria(SearchFile, SearchDate1, SearchDate2, SearchSize1, SearchSize2, ErrMess)
  else
    GetTagsCriteria(SearchArtist, SearchAlbum, SearchTrack, SearchGenre, SearchYear1, SearchYear2, ErrMess);
  if ErrMess = '' then begin
    // Reset directory- and found-files arrays
    for I := 1 to FilesAllMax do
      aFilesAll[I] := -1;
    for I := 1 to FilesFoundMax do
      aFilesFound[I] := -1;
    iFilesFound := 0;
    // If a new directory search is necessary, do so and create the temporary file containing the MP3 files paths of this directory
    // I do this in order to avoid unnecessary FindAllFiles commands, which may take time even on fast computers
    if bFindFiles or (bSubDirs and not bSubDirsDone) then begin
      try
        MP3FileList := FindAllFiles(sDir, '*.mp3', bSubDirs);                  // find all MP3 files in selected directory (and, if selected, its subdirectories)
        iFilesTotal := MP3FileList.Count;                                      // the actual number of directory MP3 files
        if iFilesTotal <= FilesAllMax then                                     // arbitrary limit of this number
          MP3FileList.SaveToFile(TempFileName)                                 // save the files list to temporary file
        else
          MessageDlg('Program error', 'Number of files in searched directory exceeds limit of ' + IntToStr(FilesAllMax) + '!', mtError, [mbOK], 0);
      finally
        MP3FileList.Free;
        sSavedDir := sDir;                                                     // the directory where the FindAllFiles has been done
        bFindFiles := False;                                                   // suppose here that directory search is now unnecessary (as done)
        if bSubDirs then                                                       // subdirectories of search directory have been done or not
          bSubDirsDone := True
        else
          bSubDirsDone := False;
      end;
    end;
    // For all files found by the FindAllFiles directory search (all files in the temporary file), check
    //   1. if they are included in the actual search directory (which may be a subdirectory of the all-files directory)
    //   2. if they meet the search criteria selected by the user
    OpenFile(TempFile, bEoF); I := 0;
    repeat
      // Read next line in the file
      ReadFile(TempFile, sMP3File, bEoF);
      if not bEoF and (sMP3File <> '') then begin
        Inc(I);                                                                // I = index of file in the all-files list (= line of the temporary file)
        MP3FilePath := ExtractFilePath(sMP3File);                              // MP3 file path
        // Check if file is included in actual search directory
        FileInDir := False;
        if bSubDirs then begin
          P := Pos(sDir, MP3FilePath);
          if P > 0 then
            FileInDir := True;
        end
        else begin
          if sDir + '\' = MP3FilePath then
            FileInDir := True;
        end;
        // If file is included in actual search directory, continue with checking it against search criteria
        if FileInDir then begin
          MP3FileName := ExtractFileName(sMP3File);
          MP3FileName := StringReplace(MP3FileName, '.mp3', '', [rfIgnoreCase]);
          FileFound := True; ErrMess := '';
          // Selection is file properties search criteria
          if rbSearchFiles.Checked then begin
            // Search using file name
            if cbFileName.Checked then begin
              FileFound := FindStrings(cboFileName, MP3FileName, SearchFile, bIgnoreCase);
            end;
            // Read file properties (if needed)
            if (cbFileDate.Checked or cbFileTime.Checked or cbFileSize.Checked) and FileFound then begin
              FindFirst(sMP3File, faAnyFile, MP3Search);
              sysutils.FindClose(MP3Search);
            end;
            // Search using file date or file "passed time"
            if (cbFileDate.Checked or cbFileTime.Checked) and FileFound then begin
              if bCreationDate then begin
                // Get creation date of MP3 file
                FDate := 0;
                FileTimeToLocalFileTime(MP3Search.FindData.ftCreationTime, LFT);
                if FileTimeToDosDateTime(LFT, DT.Hi, DT.Lo) then
                  FDate := FileDateToDateTime(Longint(DT));
              end
              else
                FDate := FileDateTodateTime(MP3Search.Time);
              // Transform the date to integer (for easy number compare)
              DecodeDate(FDate, YY, MM, DD);
              FileDate := 10000 * YY + 100 * MM + DD;
              if cbFileDate.Checked then
                FileFound := FindNumber(cboFileDate, FileDate, SearchDate1, SearchDate2)
              else
                FileFound := FindNumber(cboFileTime, FileDate, SearchDate1, SearchDate2);
            end;
            // Search using file size
            if cbFileSize.Checked and FileFound then begin
              FileSize := MP3Search.Size / (1024 * 1024);
              FileFound := FindNumber(cboFileSize, FileSize, SearchSize1, SearchSize2);
            end;
          end
          // Selection is MP3 tags search criteria
          else if rbSearchTags.Checked then begin
            // Read MP3 tags
            ReadMP3Tags(sMP3File, MP3Artist, MP3Album, MP3Title, MP3Track, MP3Genre, MP3Year);
            // Search using artist name
            if cbArtistName.Checked then begin
              FileFound := FindStrings(cboArtistName, MP3Artist, SearchArtist, bIgnoreCase);
            end;
            // Search using album or track name
            if cbAlbumName.Checked or cbTrackName.Checked then begin
              if bORTitle then begin
                // Consider as found if search string is included in album OR track stTitle (default)
                FileFound1 := False; FileFound2 := False;
                if cbAlbumName.Checked and FileFound then
                  FileFound1 := FindStrings(cboAlbumName, MP3Album, SearchAlbum, bIgnoreCase);
                if cbTrackName.Checked and FileFound then begin
                  if MP3Track = '0' then
                    FileFound2 := False
                  else
                    FileFound2 := FindStrings(cboTrackName, MP3Title, SearchTrack, bIgnoreCase);
                end;
                FileFound := FileFound1 or FileFound2;
              end
              else begin
                // Consider as found if search string is included in album AND track stTitle
                if cbAlbumName.Checked and FileFound then
                  FileFound := FindStrings(cboAlbumName, MP3Album, SearchAlbum, bIgnoreCase);
                if cbTrackName.Checked and FileFound then begin
                  if MP3Track = '0' then
                    FileFound := False
                  else
                    FileFound := FindStrings(cboTrackName, MP3Title, SearchTrack, bIgnoreCase);
                end;
              end;
            end;
            // Search using music genre
            if cbAlbumGenre.Checked and FileFound then begin
              if LowerCase(MP3Genre) = LowerCase(SearchGenre) then
                FileFound := True
              else
                FileFound := False;
            end;
            // Search using album year
            if cbReleaseYear.Checked and FileFound then begin
              FileFound := FindNumber(cboReleaseYear, StrToInt(MP3Year), SearchYear1, SearchYear2);
            end;
          end;
        end;
        // The actually MP3 file meets all selected search criteria (is part of the search result)
        if FileFound then begin
          Inc(iFilesFound);                                                    // actual number of files found
          aFilesAll[I] := iFilesFound;                                         // all-files-array: array[I] = files-found index (memo line number)
          aFilesFound[iFilesFound] := I;                                       // files-found-array: array[I] = all-files index (temporary file line number)
        end;
      end;
    until bEoF;
    CloseFile(TempFile, bEoF);
    // Number of files found > arbitrary limit
    if iFilesFound > FilesFoundMax then
      MessageDlg('Program error', 'Number of files found by actual search exceeds limit of ' + IntToStr(FilesFoundMax) + '!', mtError, [mbOK], 0)
    // Number of files found = ok and not 0:
    // For all files in the FindAllFiles directory (all files listed in the temporary file), check
    // if it is part of the search result and if so, read all file resp. MP3 tags info for this file
    // NOTE:
    // If I choose to create the memo in two steps (with a certain file properties resp MP3 tags read redundancy, it's because I think that
    // there is less to read if I limit the reading for all files in the search directory to the info being used as search criteria and then
    // reading the full info (as reported in the memo) for only those files which are part of the search result.
    else if iFilesFound > 0 then begin
      if bFileInfo then begin
        fFilesMP3.Caption := 'MP3 file information (' + sDir + ')';
        fFilesMP3.dlgSave.Title := 'Save file information as';
        if bCreationDate then
          DateType := 'created '
        else
          DateType := 'last modified ';
      end
      else begin
        fFilesMP3.Caption := 'MP3 album/track information (' + sDir + ')';
        fFilesMP3.dlgSave.Title := 'Save album/track information as';
      end;
      OpenFile(TempFile, bEoF);
      fFilesMP3.memoFiles.Lines.Clear; J := 0;
      // For all MP3 files in the FindAllFiles directory
      for I := 1 to iFilesTotal do begin
        // Continue if file is part of the search result
        if aFilesAll[I] <> - 1 then begin
          repeat
            // Read temporary file's lines until the correct line has been found
            ReadFile(TempFile, sMP3File, bEoF);
            Inc(J);
          until J = I;
          // Search using file properties has been selected
          if bFileInfo then begin
            // Get file properties
            FindFirst(sMP3File, faAnyFile, MP3Search);
            sysutils.FindClose(MP3Search);
            if bCreationDate then begin
              // Get file creation date
              FDate := 0;
              FileTimeToLocalFileTime(MP3Search.FindData.ftCreationTime, LFT);
              if FileTimeToDosDateTime(LFT, DT.Hi, DT.Lo) then
                FDate := FileDateToDateTime(Longint(DT));
            end
            else
              // Get file modification date
              FDate := FileDateTodateTime(MP3Search.Time);
            if not bFullPath then begin
              // Use file name instead of file path
              MP3FileName := ExtractFileName(sMP3File);
              MP3FileName := StringReplace(MP3FileName, '.mp3', '', [rfIgnoreCase]);
              S := MP3FileName;
            end
            else
              S := sMP3File;
            // Search result (memo): Display file size and file date together file name
            S += ' (' + FloatToStr(Int(100 * (MP3Search.Size / (1024 * 1024))) / 100) + ' MB, ' + DateType + FormatDateTime('ddddd', FDate) + ')';
          end
          // Search using MP3 tags has been selected
          else begin
            // Read MP3 tags
            ReadMP3Tags(sMP3File, MP3Artist, MP3Album, MP3Title, MP3Track, MP3Genre, MP3Year);
            // Search result (memo): Display artist, album, stTitle, genre and year
            S := MP3Artist + ', "' + MP3Album + '" (' + MP3Genre + ' , ' + MP3Year + ') ';
            // This is personal staff: Most of my MP3 files are complete albums, identified by track number = 0. There is no sense
            // to include the track stTitle into the display in such cases
            if MP3Track <> '0' then
              S += ': ' + MP3Track + '. ' + MP3Title;
          end;
          // Fill the search result into the memo on the fFilesMP3 form
          fFilesMP3.memoFiles.Lines.AddText(S);
        end;
      end;
      CloseFile(TempFile, bEoF);
      // If serach has been done using MP3 tags or if the file path is not to be displayed, resort the memo
      // As the line number in the memo chnages the indexes contained in the files-found-array have to be updated accordingly
      if not bFileInfo or (bFileInfo and not bFullPath) then
        // Sort the memo in these cases (Quicksort algorithm)
        SRQuickSort(aFilesFound, iFilesFound);
      // Pass all variables needed to the fFilesMP3 form
      fFilesMP3.aFilesFound := @aFilesFound;                                   // files-found array (as a pointer)
      fFilesMP3.iFilesFound := iFilesFound;                                    // number of array elements (= number of files found)
      // Show up the search results (the fFilesMP3 form)
      fFilesMP3.Show;
    end
    // No search results with actual search criteria
    else
      ShowMessage('No MP3 files found whith selected search criteria!');
  end
  // Missing or invalid user input
  else
    MessageDlg('Invalid input data', ErrMess, mtError, [mbOK], 0);
end;

{ Checkboxes "if value has been changed" methods }

// Beside not allowing to check "search by date" and "search by time passed" simultaneously, these "user-friendly"
// routines automatically check the appropriate search by file/tag radio button

procedure TfSearchMP3.cbFileNameChange(Sender: TObject);

begin
  if cbFileName.Checked then
    rbSearchFiles.Checked := True;
end;

procedure TfSearchMP3.cbFileDateChange(Sender: TObject);

begin
  if cbFileDate.Checked then begin
    if cbFileTime.Checked then begin
      MessageDlg('Invalid selection', 'File date and file time can''t be checked both! File time will be UNchecked', mtWarning, [mbOK], 0);
      cbFileTime.Checked := False;                                             // automatically uncheck "search by time passed"
    end
    else
      rbSearchFiles.Checked := True;
  end;
end;

procedure TfSearchMP3.cbFileTimeChange(Sender: TObject);

begin
  if cbFileTime.Checked then begin
    if cbFileDate.Checked then begin
      MessageDlg('Invalid selection', 'File date and file time can''t be checked both! File date will be UNchecked', mtWarning, [mbOK], 0);
      cbFileDate.Checked := False;                                             // automatically uncheck "search by date"
    end
    else
      rbSearchFiles.Checked := True;
  end;
end;

procedure TfSearchMP3.cbFileSizeChange(Sender: TObject);

begin
  if cbFileSize.Checked then
    rbSearchFiles.Checked := True;
end;

procedure TfSearchMP3.cbArtistNameChange(Sender: TObject);

begin
  if cbArtistName.Checked then
    rbSearchTags.Checked := True;
end;

procedure TfSearchMP3.cbAlbumNameChange(Sender: TObject);

begin
  if cbAlbumName.Checked then
    rbSearchTags.Checked := True;
end;

procedure TfSearchMP3.cbTrackNameChange(Sender: TObject);

begin
  if cbTrackName.Checked then
    rbSearchTags.Checked := True;
end;

procedure TfSearchMP3.cbAlbumGenreChange(Sender: TObject);

begin
  if cbAlbumGenre.Checked then
    rbSearchTags.Checked := True;
end;

procedure TfSearchMP3.cbReleaseYearChange(Sender: TObject);

begin
  if cbReleaseYear.Checked then
    rbSearchTags.Checked := True;
end;

procedure TfSearchMP3.cbPlayTimeChange(Sender: TObject);

begin
  if cbPlayTime.Checked then
    rbSearchTags.Checked := True;
end;

{ Comboboxes "if value has been changed" methods }

// Beside enabling/disabling the end value text field as required, these "user-friendly"
// routines automatically check the appropriate search criterium check box

procedure TfSearchMP3.cboFileNameChange(Sender: TObject);

begin
  cbFileName.Checked := True;
end;

procedure TfSearchMP3.cboFileDateChange(Sender: TObject);

begin
  if cboFileDate.ItemIndex = 3 then
    edFileDate2.Enabled := True
  else
    edFileDate2.Enabled := False;
  cbFileDate.Checked := True;
end;

procedure TfSearchMP3.cboFileTimeChange(Sender: TObject);

begin
  if cboFileTime.ItemIndex = 3 then
    edFileTime2.Enabled := True
  else
    edFileTime2.Enabled := False;
  cbFileTime.Checked := True;
end;

procedure TfSearchMP3.cboFileSizeChange(Sender: TObject);

begin
  if cboFileSize.ItemIndex = 3 then
    edFileSize2.Enabled := True
  else
    edFileSize2.Enabled := False;
  cbFileSize.Checked := True;
end;

procedure TfSearchMP3.cboArtistNameChange(Sender: TObject);

begin
  cbArtistName.Checked := True;
end;

procedure TfSearchMP3.cboAlbumNameChange(Sender: TObject);

begin
  cbAlbumName.Checked := True;
end;

procedure TfSearchMP3.cboTrackNameChange(Sender: TObject);

begin
  cbTrackName.Checked := True;
end;

procedure TfSearchMP3.cboGenresChange(Sender: TObject);

begin
  if cboGenres.Text = '' then
    cbAlbumGenre.Checked := False
  else
    cbAlbumGenre.Checked := True;
end;

procedure TfSearchMP3.cboReleaseYearChange(Sender: TObject);

begin
  if cboReleaseYear.ItemIndex = 3 then
    edReleaseYear2.Enabled := True
  else
    edReleaseYear2.Enabled := False;
  cbReleaseYear.Checked := True;
end;

procedure TfSearchMP3.cboPlayTimeChange(Sender: TObject);

begin
  if cboPlayTime.ItemIndex = 3 then
    edPlayTime2.Enabled := True
  else
    edPlayTime2.Enabled := False;
  cbPlayTime.Checked := True;
end;

{ Edit fields "if value has been changed" methods }

// The only purpose of these "user-friendly" routines is to automatically check/uncheck the appropriate search criterium check box

procedure TfSearchMP3.edFileNameChange(Sender: TObject);

begin
  if edFileName.Text = '' then
    cbFileName.Checked := False
  else
    cbFileName.Checked := True;
end;

procedure TfSearchMP3.edFileDate1Change(Sender: TObject);

begin
  if (edFileDate1.Text = '') and (edFileDate2.Text = '') then
    cbFileDate.Checked := False
  else
    cbFileDate.Checked := True;
end;

procedure TfSearchMP3.edFileDate2Change(Sender: TObject);

begin
  if (edFileDate1.Text = '') and (edFileDate2.Text = '') then
    cbFileDate.Checked := False
  else
    cbFileDate.Checked := True;
end;

procedure TfSearchMP3.edFileTime1Change(Sender: TObject);

begin
  if (edFileTime1.Text = '') and (edFileTime2.Text = '') then
    cbFileTime.Checked := False
  else
    cbFileTime.Checked := True;
end;

procedure TfSearchMP3.edFileTime2Change(Sender: TObject);

begin
  if (edFileTime1.Text = '') and (edFileTime2.Text = '') then
    cbFileTime.Checked := False
  else
    cbFileTime.Checked := True;
end;

procedure TfSearchMP3.edFileSize1Change(Sender: TObject);

begin
  if (edFileSize1.Text = '') and (edFileSize2.Text = '') then
    cbFileSize.Checked := False
  else
    cbFileSize.Checked := True;
end;

procedure TfSearchMP3.edFileSize2Change(Sender: TObject);

begin
  if (edFileSize1.Text = '') and (edFileSize2.Text = '') then
    cbFileSize.Checked := False
  else
    cbFileSize.Checked := True;
end;

procedure TfSearchMP3.edArtistNameChange(Sender: TObject);

begin
  if edArtistName.Text = '' then
    cbArtistName.Checked := False
  else
    cbArtistName.Checked := True;
end;

procedure TfSearchMP3.edAlbumNameChange(Sender: TObject);

begin
  if edAlbumName.Text = '' then
    cbAlbumName.Checked := False
  else
    cbAlbumName.Checked := True;
end;

procedure TfSearchMP3.edTrackNameChange(Sender: TObject);

begin
  if edTrackName.Text = '' then
    cbTrackName.Checked := False
  else
    cbTrackName.Checked := True;
end;

procedure TfSearchMP3.edReleaseYear1Change(Sender: TObject);

begin
  if (edReleaseYear1.Text = '') and (edReleaseYear2.Text = '') then
    cbReleaseYear.Checked := False
  else
    cbReleaseYear.Checked := True;
end;

procedure TfSearchMP3.edReleaseYear2Change(Sender: TObject);

begin
  if (edReleaseYear1.Text = '') and (edReleaseYear2.Text = '') then
    cbReleaseYear.Checked := False
  else
    cbReleaseYear.Checked := True;
end;

procedure TfSearchMP3.edPlayTime1Change(Sender: TObject);

begin
  if (edPlayTime1.Text = '') and (edPlayTime2.Text = '') then
    cbPlayTime.Checked := False
  else
    cbPlayTime.Checked := True;
end;

procedure TfSearchMP3.edPlayTime2Change(Sender: TObject);

begin
  if (edPlayTime1.Text = '') and (edPlayTime2.Text = '') then
    cbPlayTime.Checked := False
  else
    cbPlayTime.Checked := True;
end;

end.

