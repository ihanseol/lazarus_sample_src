unit ID3v2;
{$mode objfpc}{$H+}

interface 

uses 
  Classes, SysUtils; 

const 
  TAG_VERSION_2_2 = 2; 
  TAG_VERSION_2_3 = 3; 
  TAG_VERSION_2_4 = 4; 

type 
  TID3v2 = class(TObject) 
  private 
    FExists: Boolean; 
    FVersionID: Byte; 
    FSize: Integer; 
    FTitle: string; 
    FArtist: string; 
    FAlbum: string; 
    FTrack: Word; 
    FTrackString: string; 
    FYear: string; 
    FGenre: string; 
    FComment: string; 
    FComposer: string; 
    FEncoder: string; 
    FCopyright: string; 
    FLanguage: string; 
    FLink: string; 
    procedure FSetTitle(const NewTitle: string); 
    procedure FSetArtist(const NewArtist: string); 
    procedure FSetAlbum(const NewAlbum: string); 
    procedure FSetTrack(const NewTrack: Word); 
    procedure FSetYear(const NewYear: string); 
    procedure FSetGenre(const NewGenre: string); 
    procedure FSetComment(const NewComment: string); 
    procedure FSetComposer(const NewComposer: string); 
    procedure FSetEncoder(const NewEncoder: string); 
    procedure FSetCopyright(const NewCopyright: string); 
    procedure FSetLanguage(const NewLanguage: string); 
    procedure FSetLink(const NewLink: string);

  public 
    constructor Create;
    procedure ResetData; 
    function ReadFromFile  (const FileName: string): Boolean; 
    function SaveToFile    (const FileName: string): Boolean; 
    function RemoveFromFile(const FileName: string): Boolean; 
    property Exists      : Boolean read FExists; 
    property VersionID   : Byte    read FVersionID; 
    property Size        : Integer read FSize; 
    property Title       : String  read FTitle     write FSetTitle; 
    property Artist      : String  read FArtist    write FSetArtist; 
    property Album       : String  read FAlbum     write FSetAlbum; 
    property Track       : Word    read FTrack     write FSetTrack; 
    property TrackString : String  read FTrackString; 
    property Year        : String  read FYear      write FSetYear; 
    property Genre       : String  read FGenre     write FSetGenre; 
    property Comment     : String  read FComment   write FSetComment; 
    property Composer    : String  read FComposer  write FSetComposer; 
    property Encoder     : String  read FEncoder   write FSetEncoder; 
    property Copyright   : String  read FCopyright write FSetCopyright;
    property Language    : String  read FLanguage  write FSetLanguage;
    property Link        : String  read FLink      write FSetLink;
  end;

implementation

const
  ID3V2_ID = 'ID3';
  ID3V2_FRAME_COUNT = 16;
  ID3V2_FRAME_DESC: array [1..ID3V2_FRAME_COUNT] of string =
    ('Title/songname/content description',
     'Lead performer(s)/Soloist(s)',
     'Album/Movie/Show title',
     'Track number/Position in set',
     'Year',
     'Content type',
     'Comments',
     'Composer',
     'Encoded by',
     'Copyright message',
     'Language(s)',
     'User defined URL link frame',
     'Recording time',
     'Original artist(s)/performer(s)',
     'Content group description',
     'Original album/movie/show title');
  ID3V2_FRAME_NEW: array [1..ID3V2_FRAME_COUNT] of string =
    ('TIT2',
     'TPE1',
     'TALB',
     'TRCK',
     'TYER',
     'TCON',
     'COMM',
     'TCOM',
     'TENC',
     'TCOP',
     'TLAN',
     'WXXX',
     'TDRC',
     'TOPE',
     'TIT1',
     'TOAL');
  ID3V2_FRAME_OLD: array [1..ID3V2_FRAME_COUNT] of string =
    ('TT2',
     'TP1',
     'TAL',
     'TRK',
     'TYE',
     'TCO',
     'COM',
     'TCM',
     'TEN',
     'TCR',
     'TLA',
     'WXX',
     'TOR',
     'TOA',
     'TT1',
     'TOT');
  ID3V2_MAX_SIZE = 4096;
  UNICODE_ID = #1;

// %artist% = %title% = %album% = %year% = %genre% = %track% = %_filename_ext% = %comment%

{ --------------------------------------------------------------------------- }
type
  FrameHeaderNew = record
    ID    : array [1..4] of Char;
    Size  : Integer;
    Flags : Word;
  end;

  FrameHeaderOld = record
    ID   : array [1..3] of Char; 
    Size : array [1..3] of Byte; 
  end; 

  TagInfo = record 
    ID          : array [1..3] of Char; 
    Version     : Byte; 
    Revision    : Byte; 
    Flags       : Byte; 
    Size        : array [1..4] of Byte; 
    FileSize    : Integer; 
    Frame       : array [1..ID3V2_FRAME_COUNT] of string; 
    NeedRewrite : Boolean; 
    PaddingSize : Integer; 
  end; 
{ --------------------------------------------------------------------------- } 
function ReadHeader(const FileName: string; var Tag: TagInfo): Boolean; 
var 
  SourceFile: file; 
  Transferred: Integer; 
begin 
  try 
    Result := true; 
    AssignFile(SourceFile, FileName); 
    FileMode := 0; 
    Reset(SourceFile, 1); 
    BlockRead(SourceFile, Tag, 10, Transferred);
    Tag.FileSize := FileSize(SourceFile); 
    CloseFile(SourceFile); 
    if Transferred < 10 then Result := false; 
  except 
    Result := false; 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
function GetTagSize(const Tag: TagInfo): Integer; 
begin
  Result := 
    Tag.Size[1] * $200000 + 
    Tag.Size[2] * $4000 + 
    Tag.Size[3] * $80 + 
    Tag.Size[4] + 10; 
  if Tag.Flags and $10 = $10 then Inc(Result, 10); 
  if Result > Tag.FileSize then Result := 0; 
end; 
{ --------------------------------------------------------------------------- } 
procedure SetTagItem(const ID, Data: string; var Tag: TagInfo); 
var 
  Iterator: Byte; 
  FrameID: string; 
begin 
  for Iterator := 1 to ID3V2_FRAME_COUNT do 
  begin 
    if Tag.Version > TAG_VERSION_2_2 then 
      FrameID := ID3V2_FRAME_NEW[Iterator] 
    else 
      FrameID := ID3V2_FRAME_OLD[Iterator]; 
    if (FrameID = ID) and (Data[1] <= UNICODE_ID) then 
      Tag.Frame[Iterator] := Data; 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
function Swap32(const Figure: Integer): Integer; 
var 
  ByteArray: array [1..4] of Byte absolute Figure; 
begin 
  Result := 
    ByteArray[1] * $1000000 + 
    ByteArray[2] * $10000 + 
    ByteArray[3] * $100 + 
    ByteArray[4]; 
end; 
{ --------------------------------------------------------------------------- }
procedure ReadFramesNew(const FileName: string; var Tag: TagInfo); 
var 
  SourceFile: file; 
  Frame: FrameHeaderNew; 
  Data: array [1..500] of Char; 
  DataPosition, DataSize: Integer; 
begin 
  try 
    AssignFile(SourceFile, FileName); 
    FileMode := 0;
    Reset(SourceFile, 1); 
    Seek(SourceFile, 10); 
    while (FilePos(SourceFile) < GetTagSize(Tag)) and (not EOF(SourceFile)) do 
    begin 
      FillChar(Data, SizeOf(Data), 0); 
      BlockRead(SourceFile, Frame, 10); 
      if not (Frame.ID[1] in ['A'..'Z']) then break; 
      DataPosition := FilePos(SourceFile); 
      if Swap32(Frame.Size) > SizeOf(Data) then DataSize := SizeOf(Data) 
      else DataSize := Swap32(Frame.Size); 
      BlockRead(SourceFile, Data, DataSize); 
      if Frame.Flags and $8000 <> $8000 then SetTagItem(Frame.ID, Data, Tag); 
      Seek(SourceFile, DataPosition + Swap32(Frame.Size)); 
    end; 
    CloseFile(SourceFile); 
  except 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
procedure ReadFramesOld(const FileName: string; var Tag: TagInfo); 
var 
  SourceFile: file; 
  Frame: FrameHeaderOld; 
  Data: array [1..500] of Char; 
  DataPosition, FrameSize, DataSize: Integer; 
begin 
  try 
    AssignFile(SourceFile, FileName); 
    FileMode := 0; 
    Reset(SourceFile, 1); 
    Seek(SourceFile, 10); 
    while (FilePos(SourceFile) < GetTagSize(Tag)) and (not EOF(SourceFile)) do 
    begin 
      FillChar(Data, SizeOf(Data), 0); 
      BlockRead(SourceFile, Frame, 6); 
      if not (Frame.ID[1] in ['A'..'Z']) then break;
      DataPosition := FilePos(SourceFile); 
      FrameSize := Frame.Size[1] shl 16 + Frame.Size[2] shl 8 + Frame.Size[3]; 
      if FrameSize > SizeOf(Data) then DataSize := SizeOf(Data) 
      else DataSize := FrameSize; 
      BlockRead(SourceFile, Data, DataSize); 
      SetTagItem(Frame.ID, Data, Tag); 
      Seek(SourceFile, DataPosition + FrameSize); 
    end; 
    CloseFile(SourceFile); 
  except
  end; 
end; 
{ --------------------------------------------------------------------------- } 
function GetANSI(const Source: string): string; 
var 
  Index: Integer; 
  FirstByte, SecondByte: Byte; 
  UnicodeChar: WideChar; 
begin 
  if (Length(Source) > 0) and (Source[1] = UNICODE_ID) then 
  begin 
    Result := ''; 
    for Index := 1 to ((Length(Source) - 1) div 2) do 
    begin 
      FirstByte := Ord(Source[Index * 2]); 
      SecondByte := Ord(Source[Index * 2 + 1]); 
      UnicodeChar := WideChar(FirstByte or (SecondByte shl 8)); 
      if UnicodeChar = #0 then break; 
      if FirstByte < $FF then Result := Result + UnicodeChar; 
    end; 
    Result := Trim(Result); 
  end 
  else 
    Result := Trim(Source); 
end; 
{ --------------------------------------------------------------------------- } 
function GetContent(const Content1, Content2: string): string; 
begin 
  Result := GetANSI(Content1); 
  if Result = '' then Result := GetANSI(Content2); 
end; 
{ --------------------------------------------------------------------------- } 
function ExtractTrack(const TrackString: string): Word; 
var 
  Track: string; 
  Index, Value, Code: Integer;
begin 
  Track := GetANSI(TrackString); 
  Index := Pos('/', Track); 
  if Index = 0 then Val(Track, Value, Code) 
  else Val(Copy(Track, 1, Index - 1), Value, Code); 
  if Code = 0 then Result := Value 
  else Result := 0; 
end; 
{ --------------------------------------------------------------------------- } 
function ExtractYear(const YearString, DateString: string): string;
begin 
  Result := GetANSI(YearString); 
  if Result = '' then Result := Copy(GetANSI(DateString), 1, 4); 
end; 
{ --------------------------------------------------------------------------- } 
function ExtractGenre(const GenreString: string): string; 
begin 
  Result := GetANSI(GenreString); 
  if Pos(')', Result) > 0 then Delete(Result, 1, LastDelimiter(')', Result)); 
end; 
{ --------------------------------------------------------------------------- } 
function ExtractText(const SourceString: string; LanguageID: Boolean): string; 
var 
  Source, Separator: string; 
  EncodingID: Char; 
begin 
  Source := SourceString; 
  Result := ''; 
  if Length(Source) > 0 then 
  begin 
    EncodingID := Source[1]; 
    if EncodingID = UNICODE_ID then Separator := #0#0 
    else Separator := #0; 
    if LanguageID then  Delete(Source, 1, 4) 
    else Delete(Source, 1, 1); 
    Delete(Source, 1, Pos(Separator, Source) + Length(Separator) - 1); 
    Result := GetANSI(EncodingID + Source); 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
procedure BuildHeader(var Tag: TagInfo); 
var 
  Iterator, TagSize: Integer; 
begin 
  TagSize := 10; 
  for Iterator := 1 to ID3V2_FRAME_COUNT do
    if Tag.Frame[Iterator] <> '' then 
      Inc(TagSize, Length(Tag.Frame[Iterator]) + 11); 
  Tag.NeedRewrite := 
    (Tag.ID <> ID3V2_ID) or 
    (GetTagSize(Tag) < TagSize) or 
    (GetTagSize(Tag) > ID3V2_MAX_SIZE); 
  if Tag.NeedRewrite then Tag.PaddingSize := ID3V2_MAX_SIZE - TagSize 
  else Tag.PaddingSize := GetTagSize(Tag) - TagSize; 
  if Tag.PaddingSize > 0 then Inc(TagSize, Tag.PaddingSize); 
  Tag.ID := ID3V2_ID;
  Tag.Version := TAG_VERSION_2_3; 
  Tag.Revision := 0; 
  Tag.Flags := 0; 
  for Iterator := 1 to 4 do 
    Tag.Size[Iterator] := ((TagSize - 10) shr ((4 - Iterator) * 7)) and $7F; 
end; 
{ --------------------------------------------------------------------------- } 
function ReplaceTag(const FileName: string; TagData: TStream): Boolean; 
var 
  Destination: TFileStream; 
begin 
  Result := false; 
  if (not FileExists(FileName)) or (FileSetAttr(FileName, 0) <> 0) then exit; 
  try 
    TagData.Position := 0; 
    Destination := TFileStream.Create(FileName, fmOpenReadWrite); 
    Destination.CopyFrom(TagData, TagData.Size); 
    Destination.Free; 
    Result := true; 
  except 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
function RebuildFile(const FileName: string; TagData: TStream): Boolean; 
var 
  Tag: TagInfo; 
  Source, Destination: TFileStream; 
  BufferName: string; 
begin 
  Result := false; 
  if (not FileExists(FileName)) or (FileSetAttr(FileName, 0) <> 0) then exit; 
  if not ReadHeader(FileName, Tag) then exit; 
  if (TagData = nil) and (Tag.ID <> ID3V2_ID) then exit; 
  try 
    BufferName := FileName + '~'; 
    Source := TFileStream.Create(FileName, fmOpenRead);
    Destination := TFileStream.Create(BufferName, fmCreate); 
    if Tag.ID = ID3V2_ID then Source.Seek(GetTagSize(Tag), soFromBeginning); 
    if TagData <> nil then Destination.CopyFrom(TagData, 0); 
    Destination.CopyFrom(Source, Source.Size - Source.Position); 
    Source.Free; 
    Destination.Free; 
    if (DeleteFile(FileName)) and (RenameFile(BufferName, FileName)) then 
      Result := true 
    else 
      raise Exception.Create('');
  except 
    if FileExists(BufferName) then DeleteFile(BufferName); 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
function SaveTag(const FileName: string; Tag: TagInfo): Boolean; 
var 
  TagData: TStringStream; 
  Iterator, FrameSize: Integer; 
  Padding: array [1..ID3V2_MAX_SIZE] of Byte; 
begin 
  TagData := TStringStream.Create(''); 
  BuildHeader(Tag); 
  TagData.Write(Tag, 10); 
  for Iterator := 1 to ID3V2_FRAME_COUNT do 
    if Tag.Frame[Iterator] <> '' then 
    begin 
      TagData.WriteString(ID3V2_FRAME_NEW[Iterator]); 
      FrameSize := Swap32(Length(Tag.Frame[Iterator]) + 1); 
      TagData.Write(FrameSize, SizeOf(FrameSize)); 
      TagData.WriteString(#0#0#0 + Tag.Frame[Iterator]); 
    end; 
  FillChar(Padding, SizeOf(Padding), 0); 
  if Tag.PaddingSize > 0 then TagData.Write(Padding, Tag.PaddingSize); 
  if Tag.NeedRewrite then Result := RebuildFile(FileName, TagData) 
  else Result := ReplaceTag(FileName, TagData); 
  TagData.Free; 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetTitle(const NewTitle: string); 
begin 
  FTitle := Trim(NewTitle); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetArtist(const NewArtist: string); 
begin
  FArtist := Trim(NewArtist); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetAlbum(const NewAlbum: string); 
begin 
  FAlbum := Trim(NewAlbum); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetTrack(const NewTrack: Word); 
begin
  FTrack := NewTrack; 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetYear(const NewYear: string); 
begin 
  FYear := Trim(NewYear); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetGenre(const NewGenre: string); 
begin 
  FGenre := Trim(NewGenre); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetComment(const NewComment: string); 
begin 
  FComment := Trim(NewComment); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetComposer(const NewComposer: string); 
begin 
  FComposer := Trim(NewComposer); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetEncoder(const NewEncoder: string); 
begin 
  FEncoder := Trim(NewEncoder); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetCopyright(const NewCopyright: string); 
begin 
  FCopyright := Trim(NewCopyright); 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetLanguage(const NewLanguage: string); 
begin 
  FLanguage := Trim(NewLanguage);
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.FSetLink(const NewLink: string); 
begin 
  FLink := Trim(NewLink); 
end; 
{ --------------------------------------------------------------------------- } 
constructor TID3v2.Create; 
begin 
  inherited;
  ResetData; 
end; 
{ --------------------------------------------------------------------------- } 
procedure TID3v2.ResetData; 
begin 
  FExists      := false; 
  FVersionID   := 0; 
  FSize        := 0; 
  FTitle       := ''; 
  FArtist      := ''; 
  FAlbum       := ''; 
  FTrack       := 0; 
  FTrackString := ''; 
  FYear        := ''; 
  FGenre       := ''; 
  FComment     := ''; 
  FComposer    := ''; 
  FEncoder     := ''; 
  FCopyright   := ''; 
  FLanguage    := ''; 
  FLink        := ''; 
end; 
{ --------------------------------------------------------------------------- } 
function TID3v2.ReadFromFile(const FileName: string): Boolean; 
var 
  Tag: TagInfo; 
begin 
  ResetData; 
  Result := ReadHeader(FileName, Tag); 
  if (Result) and (Tag.ID = ID3V2_ID) then 
  begin 
    FExists := true; 
    FVersionID := Tag.Version; 
    FSize := GetTagSize(Tag); 
    if (FVersionID in [TAG_VERSION_2_2..TAG_VERSION_2_4]) and (FSize > 0) then 
    begin
      if FVersionID > TAG_VERSION_2_2 then ReadFramesNew(FileName, Tag) 
      else ReadFramesOld(FileName, Tag); 
      FTitle := GetContent(Tag.Frame[1], Tag.Frame[15]); 
      FArtist := GetContent(Tag.Frame[2], Tag.Frame[14]); 
      FAlbum := GetContent(Tag.Frame[3], Tag.Frame[16]); 
      FTrack := ExtractTrack(Tag.Frame[4]); 
      FTrackString := GetANSI(Tag.Frame[4]); 
      FYear := ExtractYear(Tag.Frame[5], Tag.Frame[13]); 
      FGenre := ExtractGenre(Tag.Frame[6]); 
      FComment := ExtractText(Tag.Frame[7], true);
      FComposer := GetANSI(Tag.Frame[8]); 
      FEncoder := GetANSI(Tag.Frame[9]); 
      FCopyright := GetANSI(Tag.Frame[10]); 
      FLanguage := GetANSI(Tag.Frame[11]); 
      FLink := ExtractText(Tag.Frame[12], false); 
    end; 
  end; 
end; 
{ --------------------------------------------------------------------------- } 
function TID3v2.SaveToFile(const FileName: string): Boolean; 
var 
  Tag: TagInfo; 
begin 
  FillChar(Tag, SizeOf(Tag), 0); 
  ReadHeader(FileName, Tag); 
  Tag.Frame[1] := FTitle; 
  Tag.Frame[2] := FArtist; 
  Tag.Frame[3] := FAlbum; 
  if FTrack > 0 then Tag.Frame[4] := IntToStr(FTrack); 
  Tag.Frame[5] := FYear; 
  Tag.Frame[6] := FGenre; 
  if FComment <> '' then Tag.Frame[7] := 'eng' + #0 + FComment; 
  Tag.Frame[8] := FComposer; 
  Tag.Frame[9] := FEncoder; 
  Tag.Frame[10] := FCopyright; 
  Tag.Frame[11] := FLanguage; 
  if FLink <> '' then Tag.Frame[12] := #0 + FLink; 
  Result := SaveTag(FileName, Tag); 
end; 
{ --------------------------------------------------------------------------- } 
function TID3v2.RemoveFromFile(const FileName: string): Boolean; 
begin 
  Result := RebuildFile(FileName, nil); 
end; 
{ --------------------------------------------------------------------------- } 
end.
