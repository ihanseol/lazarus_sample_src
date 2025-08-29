{*********************************************************************}
{* WinLibs: A Free Pascal unit to access Microsoft Windows libraries *}
{*********************************************************************}

unit WinLibs;

// For details, please visit https://www.streetinfo.lu/computing/lazarus/programming/winlibs.html

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8, Dos;

type
  // Type returned by the TLibraries.GetLibraryNames method
  TLibNames = array of string;
  // Type returned by the TLibraries.GetLibraryInfo method
  TLibInfo = record
    LibName, LibType, LibContent: string;
    NLibFolders: Integer;
  end;
  // Type returned by the TLibraries.GetLibFolderInfo method
  TLibFolderInfo = array of record
    FolderName, FolderPath, FolderType: string;
  end;
  // TLibrary objects contain the data concerning a given library. Having all private
  // methods, they may not directly be accessed by the user. Access is done by using
  // a method of the TLibraries object, that actually contains an array with TLibrary
  // objects as elements
  TLibrary = class
    constructor Create(LName: string);
    destructor Destroy; override;
    private
      LibName, LibType, LibContent: string;
      NLibFolders: Integer;
      LibFolderInfo: TLibFolderInfo;
      procedure ReadLibraryData(LDir: string);
  end;
  // The TLibraries object conains the data concerning all libraries. Access (read-only)
  // is done using one of the 3 following methods:
  //   - GetLibraryNames: Get names of your Windows libraries
  //   - GetLibraryInfo: Get info about a given library
  //   - GetLibraryFolderInfo: Get info about all (first child) folders being part of a
  //     given library, in particular, these folders' path within the filesystem
  TLibraries = class
    constructor Create(IncludeSpecial: Boolean = False);
    destructor Destroy; override;
    function GetLibraryNames: TLibNames;
    function GetLibraryInfo(LName: string): TLibInfo;
    function GetLibraryFolderInfo(LName: string; IncludeUser: Boolean = True;
      IncludePublic: Boolean = True; IncludeOther: Boolean = True): TLibFolderInfo;
    private
      NLibraries: Integer;
      Libraries: array of TLibrary;
      procedure Add(Ldir, LName: string);
    public
  end;

implementation

{ Create TLibraries object }

constructor TLibraries.Create(IncludeSpecial: Boolean);

// The method reads the .library-ms files to get the library names and then, for each library found,
// calls the private TLibraries.Add method to add the libraries (as TLibrary objects).
// The method ignores by default the normally not used Windows 10 libraries "Camera Roll" and "Saved Pictures";
// to include them, set IncludeSpecial = True.

var
  N, I, P: Integer;
  Dir, Filename: string;
  LibFiles: TStringList;

begin
  inherited Create;
  NLibraries := 0;
  // Find all library description files
  Dir := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\AppData\Roaming\Microsoft\Windows\Libraries';
  LibFiles := FindAllFiles(Dir, '*.library-ms', False);
  N := LibFiles.Count;
  // For each file found, add the corresponding library to the TLibraries object
  if N > 0 then begin
    for I := 0 to N - 1 do begin
      Filename := ExtractFileName(LibFiles.Strings[I]);
      P := UTF8Pos('.', Filename);
      Filename := UTF8Copy(Filename, 1, P - 1);
      if IncludeSpecial or ((Filename <> 'CameraRoll') and (Filename <> 'SavedPictures')) then begin
        // Include "Camera Roll" and "Saved Pictures" only if explicitely said to do so
        Add(Dir, Filename);
      end;
    end;
  end;
  LibFiles.Free;
end;

{ Destroy TLibraries object }

destructor TLibraries.Destroy;

var
  I: Integer;

begin
  for I := 0 to NLibraries - 1 do begin
    // Distroy TLibrary objects
    Libraries[I].Destroy;
  end;
  inherited Destroy;
end;

{ Add the library, given by its name, to the TLibraries object }

procedure TLibraries.Add(LDir, LName: string);

var
  I: Integer;
  Insert: Boolean;

begin
  Insert := True;
  // Check if the library doesn't already exist
  if NLibraries > 0 then begin
    for I := 0 to NLibraries - 1 do begin
      if Libraries[I].LibName = LName then
        Insert := False;
    end;
  end;
  if Insert then begin
    // Add the library by creating a new TLibrary object
    // Call the private TLibrary.ReadLibraryData method to fill in the library data
    Inc(NLibraries); SetLength(Libraries, NLibraries);
    Libraries[NLibraries - 1] := TLibrary.Create(LName);
    Libraries[NLibraries - 1].ReadLibraryData(LDir);
  end;
end;

{ Get the names of all libraries }

function TLibraries.GetLibraryNames: TLibNames;

// The method returns the names as a TLibNames (= array of strings) type

var
  I: Integer;
  LibNames: TLibNames;

begin
  SetLength(LibNames, NLibraries);
  if NLibraries > 0 then begin
    for I := 0 to NLibraries - 1 do
      LibNames[I] := Libraries[I].LibName;
  end;
  Result := LibNames;
end;

{ Get info concerning the library given by its name }

function TLibraries.GetLibraryInfo(LName: string): TLibInfo;

// The method returns the info as a TLibInfo (record) type. Record fields:
//   LibName:     library name
//   LibType:     library type (standard Windows or custom library)
//   LibContent:  library content (standard Windows library content or "other")
//   NLibFolders: Number of library folders

var
  I: Integer;
  Info: TLibInfo;

begin
  Info.NLibFolders := 0;
  Info.LibName := '';
  Info.LibType := '';
  Info.LibContent := '';
  for I := 0 to NLibraries - 1 do begin
    if Libraries[I].LibName = LName then begin
      Info.NLibFolders := Libraries[I].NLibFolders;
      Info.LibName := Libraries[I].LibName;
      Info.LibType := Libraries[I].LibType;
      Info.LibContent := Libraries[I].LibContent;
    end;
  end;
  Result := Info;
end;

{ Get info concerning the folders of a library given by its name }

function TLibraries.GetLibraryFolderInfo(LName: string; IncludeUser, IncludePublic, IncludeOther: Boolean): TLibFolderInfo;

// The method returns the info as a TLibFolderInfo (array of records) type. Records are for one library folder; fields:
//   FolderName: Name of the folder
//   FolderPath: Path of the folder within the filesystem
//   FolderType: Folder storage type: user or public save location, otherwise blank
// The 3 (optional) Boolean arguments allow to explicitely exclude user, public or other folders

var
  N, I, J: Integer;
  GetLib: Boolean;
  Folders: TLibFolderInfo;

begin
  SetLength(Folders, 0); N := 0;
  for I := 0 to NLibraries - 1 do begin
    if Libraries[I].LibName = LName then begin
      // Do for the library with name passed as argument
      for J := 0 to Libraries[I].NLibFolders - 1 do begin
        // Create one record for each folder (except if this folder type is said to be excluded)
        GetLib := True;
        if (LeftStr(Libraries[I].LibFolderInfo[J].FolderType, 7) = 'default') and (not IncludeUser) then
          GetLib := False
        else if (LeftStr(Libraries[I].LibFolderInfo[J].FolderType, 6) = 'public') and (not IncludePublic) then
          GetLib := False
        else if (Libraries[I].LibFolderInfo[J].FolderType = '') and (not IncludeOther) then
          GetLib := False;
        if GetLib then begin
          // This is a folder to be added to the return array: Fill in the record fields
          Inc(N); SetLength(Folders, N);
          Folders[N - 1].FolderName := Libraries[I].LibFolderInfo[J].FolderName;
          Folders[N - 1].FolderPath := Libraries[I].LibFolderInfo[J].FolderPath;
          Folders[N - 1].FolderType := Libraries[I].LibFolderInfo[J].FolderType;
        end;
      end;
    end;
  end;
  Result := Folders;
end;

{ Create TLibrary object for library with given name }

constructor TLibrary.Create(LName: string);

begin
  inherited Create;
  LibName := LName;
  SetLength(LibFolderInfo, 0);
end;

{ Distroy TLibrary object }

destructor TLibrary.Destroy;

begin
  inherited Destroy;
end;

{ Fill in library data as read from the corresponding library description (.library-ms) file }

procedure TLibrary.ReadLibraryData(LDir: string);

// The directory passed as argument is the filepath to the folder where the .library-ms files are located

const
  // Standard windows folder content identifiers
  FolderContentIds: array[0..3] of string = (
    '7d49d726-3c21-4f05-99aa-fdc2c9474656', 'b3690e58-e961-423b-b687-386ebfd83239',
    '94d6ddcc-4a68-4175-a374-bd584a510b78', '5fa96407-7e77-483c-ac93-691d05850de8'
  );
  FolderContents: array[0..3] of string = (
    'documents', 'pictures', 'music', 'videos'
  );
  // Standard windows folder type identifiers
  FolderTypeIds: array[0..9] of string = (
    'FDD39AD0-238F-46AF-ADB4-6C85480369C7', '33E28130-4E1E-4676-835A-98395C3BC3BB', '4BD8D571-6D19-48D3-BE97-422220080E43',
    '18989B1D-99B5-455B-841C-AB7C74E4DDFC', 'AB5FB87B-7CE2-4F83-915D-550846C9537B', '3B193882-D3AD-4eab-965A-69829D1FB59F',
    'ED4824AF-DCE4-45A8-81E2-FC7965083634', 'B6EBFB86-6907-413C-9AF7-4FC2ABF07CC5', '3214FAB5-9757-4298-BB61-92A9DEAA44FF',
    '2400183A-6185-49FB-A2D8-4A392A602BA3'
  );
  FolderTypes: array[0..9] of string = (
    'user documents', 'user pictures', 'user music', 'user videos', 'camera roll',
    'saved pictures', 'public documents', 'public pictures', 'public music', 'public videos'
  );

var
  N, I, J, P: Integer;
  Filename, Line, Folder: string;
  DefaultUser, DefaultPublic: Boolean;
  LibData: TStringList;

begin
  // Read description file for this library
  Filename := LDir + '\' + LibName + '.library-ms';
  LibData := TStringList.Create;
  LibData.LoadFromFile(Filename);
  NLibFolders := 0;
  // Parse the file content (1st pass)
  for I := 0 to LibData.Count - 1 do begin
    Line := LibData.Strings[I];
    // Determine the library content
    P := UTF8Pos('<folderType>', Line);
    if P > 0 then begin
      LibContent := 'other';
      for J := 0 to 3 do begin
        P := UTF8Pos(FolderContentIds[J], Line);
        if P > 0 then
          LibContent := FolderContents[J];                                     // folder with standard Windows content (documents, pictures, ...)
      end;
    end;
    // Determine the library type
    P := UTF8Pos('<url>', Line);
    if P > 0 then begin
      for J := 0 to 9 do begin
        // If one of the folder URLs is a standard Windows folder type identifier,
        // the library is a standard Windows library
        if LibType = '' then begin
          P := UTF8Pos(FolderTypeIds[J], Line);
          if P > 0 then begin
            LibType := 'standard ' + FolderTypes[J] + ' library';
            LibType := StringReplace(LibType, 'user ', '', []);
            LibType := StringReplace(LibType, 'public ', '', []);
          end;
        end;
      end;
      Inc(NLibFolders);
      SetLength(LibFolderInfo, NLibFolders);
    end;
  end;
  // If the folder type couldn't be determined, it must be a custom library (library created by the user)
  if LibType = '' then
    LibType := 'custom library';
  // Parse the file content (2nd pass)
  N := 0; DefaultUser := False; DefaultPublic := False; Folder := '';
  for I := 0 to LibData.Count - 1 do begin
    // Determine the folder type and the folder path
    Line := LibData.Strings[I];
    P := UTF8Pos('<isDefaultSaveLocation>', Line);
    if P > 0 then begin
      // This folder is a "default save location" folder
      if UTF8Copy(Line, P + 23, 4) = 'true' then
        DefaultUser := True;
    end
    else begin
      P := UTF8Pos('<isDefaultNonOwnerSaveLocation>', Line);
      if P > 0 then begin
        if UTF8Copy(Line, P + 31, 4) = 'true' then
          // This folder is a "public save location" folder
          DefaultPublic := True;
      end
    end;
    P := UTF8Pos('<url>', Line);
    if P > 0 then begin
      // The folder URL contains information about the path:
      // For standard library folders, the URL is a Windows library folder identifier (and the folder path is standard on all Windows systems);
      // For other library folders, the URL contains the full folder path
      for J := 0 to 9 do begin
        if Folder = '' then begin
          P := UTF8Pos(FolderTypeIds[J], Line);
          if P > 0 then begin
            // Standard library folder
            Folder := StringReplace(LibType, 'standard ', '', []);
            Folder := StringReplace(Folder, ' library', '', []);
            Folder[1] := UpperCase(Folder[1])[1];
            if DefaultUser then begin
              // Standard library default save location folders:
              // They are located at C:\Users\{User name}\, except for "Camera Roll" and "Saved Pictures",
              // that are subfolders of C:\Users\{User name}\Pictures\
              if LibType = 'standard camera roll library' then
                LibFolderInfo[N].FolderPath := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\Pictures\Camera Roll'
              else if LibType = 'standard saved pictures library' then
                LibFolderInfo[N].FolderPath := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\Pictures\Saved Pictures'
              else
                LibFolderInfo[N].FolderPath := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\' + Folder;
              LibFolderInfo[N].FolderType := 'default save location';
            end
            else if DefaultPublic then begin
              // Standard library public save location folders:
              // They are located at C:\Users\Public\ ("Camera Roll" and "Saved Pictures" don't exist by default and are not considered)
              LibFolderInfo[N].FolderPath := GetEnv('HOMEDRIVE') + '\Users\Public\' + Folder;
              LibFolderInfo[N].FolderType := 'public save location';
            end
            else begin
              // Other folders: Custom path, as chosen by user, when adding the folder to the library
              // Full folder path is given as such by URL value
              LibFolderInfo[N].FolderPath := UTF8Trim(StringReplace(Line, '<url>', '', []));
              LibFolderInfo[N].FolderPath := StringReplace(LibFolderInfo[N].FolderPath, '</url>', '', []);
              LibFolderInfo[N].FolderType := '';
            end;
          end;
        end;
        if Folder = '' then begin
          // If none of the folders is a standared library folder type, the library itself is custom (created by user)
          // The folder path is always custom (and is given as such by URL value)
          LibFolderInfo[N].FolderPath := UTF8Trim(StringReplace(Line, '<url>', '', []));
          LibFolderInfo[N].FolderPath := StringReplace(LibFolderInfo[N].FolderPath, '</url>', '', []);
          // Folders of a custom library may, however, just as standard-library folders be a default or public save location
          if DefaultUser then
            LibFolderInfo[N].FolderType := 'default save location'
          else if DefaultPublic then
            LibFolderInfo[N].FolderType := 'public save location'
          else
            LibFolderInfo[N].FolderType := '';
        end;
      end;
      // Extract folder name from folder path
      LibFolderInfo[N].FolderName := ExtractFilename(LibFolderInfo[N].FolderPath);
      // Set variables for next folder of the library
      Inc(N); DefaultUser := False; DefaultPublic := False; Folder := '';
    end;
  end;
  LibData.Free;
end;

end.

