{*********************************************************************}
{* Simple command line program to show the usage of the WinLibs unit *}
{*********************************************************************}

program liblist;

{$mode objfpc}{$H+}

uses
  // ----------------------------------
  Classes, SysUtils, FileUtil, WinLibs;
  // ----------------------------------

var
  N, I, J: Integer;
  Temp: string;
  MusicFilesList: TStringList;
  MusicFiles: array of string;
  // --- WinLibs types ---
  Libs: TLibraries;                                                            // main WinLibs class
  LibNames: TLibNames;                                                         // array of strings
  LibInfo: TLibInfo;                                                           // record (library name, type, content)
  Folders: TLibFolderInfo;                                                     // array of records (folder name, path, type)
  // ---------------------

begin
  // ----------------------------------------------
  // Usage example 1: List of libraries directories
  // ----------------------------------------------
  Writeln('List of Windows libraries directories'); Writeln;
  // Create the TLibraries object
  Libs := TLibraries.Create(True);                                             // TLibraries method: create the TLibraries object
  // Get and display names of all directories
  LibNames := Libs.GetLibraryNames;                                            // TLibraries method: get library names
  Writeln('Libraries:');
  for I := 0 to Length(LibNames) - 1 do
    Write('  ', LibNames[I], ' ');
  Writeln; Writeln;
  // For each library, get and display library and folder info
  for I := 0 to Length(LibNames) - 1 do begin
    LibInfo := Libs.GetLibraryInfo(LibNames[I]);                               // TLibraries method: get library info
    Folders := Libs.GetLibraryFolderInfo(LibNames[I]);                         // TLibraries method: get folder info
    // For each library, display library info
    Writeln(LibInfo.LibName, ': ', LibInfo.LibType, ' (', LibInfo.LibContent, '), Folders = ', LibInfo.NLibFolders);
    for J := 0 to Length(Folders) - 1 do begin
      // For each library folder, display folder info
      Write('  ', Folders[J].FolderPath);
      if Folders[J].FolderType <> '' then
        Write(' (', Folders[J].FolderType, ')');
      Writeln;
    end;
    Writeln;
  end;
  // ------------------------------------------------------------
  // Usage example 2: List of album and song MP3 in Music library
  // ------------------------------------------------------------
  Writeln('List of all MP3 albums and songs in the Music library'); Writeln;
  // Get music library folder info. The call of the method as done here, includes the user music folder and any extra folders,
  // excluding the public music folder. That is how music files are stored on my system: the best albums and songs in the user
  // music folders, the others on an USB disk (the public music folder containing non-album/songs related MP3).
  Folders := Libs.GetLibraryFolderInfo('Music', True, False, True);            // TLibraries method: get folder info
  // For each folder (including its subfolders), read MP3 files
  N := 0;
  for I := 0 to Length(Folders) - 1 do begin
    // Get music files list
    MusicFilesList := FindAllFiles(Folders[I].FolderPath, '*.mp3', True);
    // Store music filenames into array
    for J := 0 to MusicFilesList.Count - 1 do begin
      Inc(N); SetLength(MusicFiles, N);
      MusicFiles[N - 1] := ExtractFilename(MusicFilesList.Strings[J]);
    end;
    MusicFilesList.Free;
  end;
  // Sort the music files array
  for I := 0 to Length(MusicFiles) - 2 do begin
    for J := I + 1 to Length(MusicFiles) - 1 do begin
      if MusicFiles[I] > MusicFiles[J] then begin
        Temp := MusicFiles[I]; MusicFiles[I] := MusicFiles[J]; MusicFiles[J] := Temp;
      end;
    end;
  end;
  // Display the music files names
  for I := 0 to Length(MusicFiles) - 1 do begin
    Writeln(MusicFiles[I]);
  end;
  Libs.Destroy;
  Writeln; Write('Hit ENTER to terminate the program...'); Readln;
end.

