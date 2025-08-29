{******************************************************}
{* (Very simple) downloads count command line program *}
{******************************************************}

program dwnldcount;

// The program counts files of a given type (with a given extension), located in one or two given directories on the webserver,
// as defined in the configuration file dwnldcount.ini. Counting is done for the year entered by the user and on a monthly base.
// The input data is assumed to be read from text files with the (normally all) downloads for a given month (cf. ReadMe.txt file
// in the .zip archive of the program download).

// Change log:
// Version 1.0 (September 2020): Original program
// Version 1.1 (December 2020):
//   - Number of files considered for counting set to number of server files minus files to be excluded (thus files_server may be
//     set to total number of files for considered extension)
// Version 1.2 (January 2021):
//   - Asking for the year, for which the counts have to be done (instead of assuming the current year)
//   - For the current year, including the current month (instead of doing only until the preceding one)
// Version 1.3 (April 2021):
//   - Adding a parameter to specify the program output directory
// Version 2.0 (January 2022):
//   - Adding a parameter to specify possible filename suffixes, allowing multiple month files
//   - Adding the possibility to specify two webserver directories (instead of just one) for the different download counts

uses
  LazUTF8, IniFiles, SysUtils, Crt;

type
  TStatsDownload = record
    ServerTotal: Integer;
    Description, DirectoryURL1, DirectoryURL2, FileExtension, StatsFilename: string;
    FileExcludes: array of string;
  end;
  TStatsDownloads = array[1..25] of TStatsDownload;
  TMonthlyDownloads = array[1..13, 1..25] of record
    FileDownloads, TotalDownloads: Integer;
  end;
  TFilenameSuffixes = array of string;

const
  Months: array[1..13] of string = (
    'January', 'February', 'March', 'April', 'May ', 'June',
    'July', 'August', 'September', 'October', 'November', 'December', 'Total'
  );

var
  CountYear, CountLastMonth, NStatsDownloads, Count, Total, FX, L, I, J, K, Code, P: Integer;
  YY, MM, DD: Word;
  StatsDir, StatsFileName, StatsFileDate, StatsFileExt, OutputDir, OutputFilename, Mess: string;
  SYear, MonthFileName0, MonthFileName, MonthFileDate, Line, URI, DirURL, FileName, FileExt, SCount: string;
  DoCount: Boolean;
  StatsDownloads: TStatsDownloads;
  MonthlyDownloads: TMonthlyDownloads;
  FilenameSuffixes: TFilenameSuffixes;
  InFile, OutFile: Text;
  Config: TINIFile;

{ Create string with given number of space characters }

function SpaceString(N: Integer): string;

var
  I: Integer;
  Sp: string;

begin
  Sp := '';
  for I := 1 to N do
    Sp += ' ';
  Result := Sp;
end;

{ Read the statistics files data from configuration file }

procedure ReadConfigStats(out StatsDir, OutputDir: string; out Downloads: Integer;
  out FileName, FileDate, FileExt, Mess: string; out Suffixes: TFilenameSuffixes);

var
  NS, P: Integer;
  SDownloads, AllSuffixes: string;

begin
  Mess := '';
  StatsDir := UTF8Trim(Config.ReadString('stats', 'stats_files_dir', ''));                    // local directory, where the statistics files are located
  if not DirectoryExists(StatsDir) then begin
    Mess := 'Invalid statistics files directory';
  end
  else begin
    SDownloads := UTF8Trim(Config.ReadString('stats', 'stats_download_counts', ''));          // downloads count = number of dir/ext counts (described in congif file) to do
    Val(SDownloads, Downloads, Code);
    if (Code <> 0) or (Downloads < 1) or (Downloads > 25) then                                // max. of 25 = arbitrarily fixed
      Mess := 'Invalid downloads count'
    else begin
      OutputDir := UTF8Trim(Config.ReadString('stats', 'output_dir', ''));                     // directory, where the statistics output file should be saved
      if OutputDir = '' then
        OutputDir := GetCurrentDir;
      if not DirectoryExists(OutputDir) then
        Mess := 'Invalid output directory';
    end;
  end;
  if Mess = '' then begin
    FileName := UTF8Trim(Config.ReadString('stats', 'stats_files_name', ''));                 // statistics files name (without suffix)
    FileDate := UTF8Copy(FileName, UTF8Length(FileName) - 5, 6);
    // Get format of filenames date-part (YYMM or YYYYMM)
    if UpperCase(FileDate) <> 'YYYYMM' then begin
      FileDate := UTF8Copy(FileName, UTF8Length(FileName) - 3, 4);
      if UpperCase(FileDate) <> 'YYMM' then
        Mess := 'Invalid statistics files name';
    end;
    if Mess = '' then begin
      FileExt := UTF8Trim(Config.ReadString('stats', 'stats_files_ext', ''));                 // statistics files extension (normally .txt)
      if FileExt = '' then
        FileExt := '.txt';
      if LeftStr(FileExt, 1) <> '.' then
        Mess := 'Invalid statistics files extension';
    end;
  end;
  if Mess = '' then begin
    AllSuffixes := UTF8Trim(Config.ReadString('stats', 'stats_files_suffixes', ''));          // list of possible filename suffixes
    NS := 0; SetLength(Suffixes, 0);
    // Extract the filename suffixes from the file list (= string with the different suffixes separated by semicolon)
    while AllSuffixes <> '' do begin
      Inc(NS); SetLength(Suffixes, NS);
      P := Pos(';', AllSuffixes);
      if P > 1 then begin
        Suffixes[NS - 1] := UTF8Trim(UTF8Copy(AllSuffixes, 1, P - 1));
        Delete(AllSuffixes, 1, P);
      end
      else begin
        Suffixes[NS - 1] := UTF8Trim(AllSuffixes);
        AllSuffixes := '';
      end;
    end;
  end;
end;

{ Read the downlaods files data from configuration file; in particular what files (-> extension) to count in what server directories }

procedure ReadConfigDownloads(ND: Integer; out StatsDownloads: TStatsDownloads; out Mess: string);

var
  NE, I, P, Code: Integer;
  Section, AllExcludes: string;

begin
  Mess := '';
  for I := 1 to ND do begin
    Section := 'downloads' + IntToStr(I);
    with StatsDownloads[I] do begin
      Description := UTF8Trim(Config.ReadString(Section, 'files_description', ''));           // download description
      if Description = '' then
        Description := 'Downloads ' + IntToStr(I);
      DirectoryURL1 := UTF8Trim(Config.ReadString(Section, 'files_url1', ''));                // server directory 1 (= relative URL)
      DirectoryURL2 := UTF8Trim(Config.ReadString(Section, 'files_url2', ''));                // server directory 2 (= relative URL)
      if (DirectoryURL1 = '') or (LeftStr(DirectoryURL1, 1) <> '/') then
        Mess := 'Invalid download URL (1)'
      else if (DirectoryURL2 <> '') and (LeftStr(DirectoryURL2, 1) <> '/') then
        Mess := 'Invalid download URL (2)'
      else begin
        FileExtension := UTF8Trim(Config.ReadString(Section, 'files_ext', ''));               // file extension (files being counted in these directories)
        if (FileExtension = '') or (LeftStr(FileExtension, 1) <> '.') then
          Mess := 'Invalid download file extension'
        else begin
          Val(UTF8Trim(Config.ReadString(Section, 'files_server', '')), ServerTotal, Code);   // total server files with this extension in these directories
          if Code <> 0 then
            Mess := 'Invalid number of server files';
        end;
      end;
      if Mess = '' then begin
        AllExcludes := UTF8Trim(Config.ReadString(Section, 'files_exclude', ''));             // list of files to be excluded from count
        NE := 0; SetLength(FileExcludes, 0);
        // Extract the files to be excluded from the file list (= string with the different files separated by semicolon)
        while AllExcludes <> '' do begin
          Inc(NE); SetLength(FileExcludes, NE);
          P := Pos(';', AllExcludes);
          if P > 1 then begin
            FileExcludes[NE - 1] := UTF8Trim(UTF8Copy(AllExcludes, 1, P - 1)) + FileExtension;
            Delete(AllExcludes, 1, P);
          end
          else begin
            FileExcludes[NE - 1] := UTF8Trim(AllExcludes) + FileExtension;
            AllExcludes := '';
          end;
        end;
        if ServerTotal > 0 then
          Description += ' (' + IntToStr(ServerTotal - NE) + ' files)';
      end;
    end;
  end;
end;

{****************}
{* Main program *}
{****************}

begin
  ClrScr;
  Writeln('dwnldcount v2.0, (c) allu, September 2020 - January 2022'); Writeln;
  // Create .INI file object
  Config := TINIFile.Create('dwnldcount.ini');
  // Read statistics files configuration
  ReadConfigStats(StatsDir, OutputDir, NStatsDownloads, StatsFileName, StatsFileDate, StatsFileExt, Mess, FilenameSuffixes);
  if (Mess = '') and (NStatsDownloads > 0) then begin
    // Read download URLs configuration
    ReadConfigDownloads(NStatsDownloads, StatsDownloads, Mess);
  end;
  if (Mess = '') and (NStatsDownloads > 0) then begin
    // Set monthly counters to zero
    for I := 1 to 13 do begin
      for J := 1 to 25 do begin
        MonthlyDownloads[I, J].FileDownloads := 0;
        MonthlyDownloads[I, J].TotalDownloads := 0;
      end;
    end;
    // Ask user for statistics year
    DeCodeDate(Date, YY, MM, DD);                                                             // current date
    repeat
      Write('Enter the year, for which the download counts have to be done? ');
      Readln(SYear);
      if SYear = '' then begin
        CountYear := YY; CountLastMonth := MM                                                 // default = current year (January until current month)
      end
      else begin
        Val(SYear, CountYear, Code);
        if Code = 0 then begin
          if CountYear = YY then
            CountLastMonth := MM                                                              // current year: counts until current month
          else
            CountLastMonth := 12;                                                             // other years: counts until December
        end;
        if (Code <> 0) or (CountYear > YY) then                                               // invalid year
          CountYear := -1;
      end;
    until CountYear > 0;
    Writeln;
    // Do counts for each month of the year
    Total := 0;
    for I := 1 to CountLastMonth do begin
      // Get monthly statistics filename(s)
      if Length(StatsFileDate) = 6 then
        MonthFileDate := IntToStr(CountYear)
      else
        MonthFileDate := IntToStr(CountYear - 2000);
      if I < 10 then
        MonthFileDate += '0' + IntToStr(I)
      else
        MonthFileDate += IntToStr(I);
      MonthFileName0 := StatsDir + '/' + StatsFileName;
      MonthFileName0 := StringReplace(MonthFileName0, StatsFileDate, MonthFileDate, []);      // this means that base filenames must have given format (ending with year and month)
      // Condider all files for this month (filename = basefilename with or without suffix)
      for FX := -1 to Length(FilenameSuffixes) - 1 do begin
        if FX = -1 then
          MonthFilename := MonthFilename0 + StatsFileExt
        else
          MonthFilename := MonthFilename0 + ' ' + FilenameSuffixes[FX] + StatsFileExt;
        DoDirSeparators(MonthFileName0);
        // If there is a statistics file with this name for actual month do the download counts
        if FileExists(MonthFilename) then begin
          Assign(InFile, MonthFileName); Reset(InFile);
          while not EoF(InFile) and (Mess = '') do begin                                        // read until end of file or until there is an error
            Readln(InFile, Line); Line := UTF8Trim(Line);
            if (Line <> '') and (LeftStr(Line, 1) = '/') then begin                             // server directory as relative URL (starting with "/")
              Line := StringReplace(Line, #9, ' ', [rfReplaceAll]);                             // transform tabs to spaces
              // For each URI in the statistics file: If it is a file to be counted, do the count
              DirURL := ''; FileExt := '';
              P := UTF8Pos(' ', Line);                                                          // space (or original tab) separating URI and number of downloads
              if P <> 0 then begin
                // Extract download directory + filename
                URI := UTF8Trim(UTF8Copy(Line, 1, P - 1));
                DirURL  := ExtractFilePath(URI); FileName := ExtractFileName(URI); FileExt := ExtractFileExt(URI);
                if (DirURL = '') or (FileExt = '') then
                  Mess := 'Invalid URI in statistic file of ' + Months[I]
                else begin
                  // Extract number of downloads for this file
                  Code := -1; Count := 0;
                  Delete(Line, 1, P);
                  if Line <> '' then begin
                    P := UTF8Pos(' ', Line);                                                    // space (or original tab) separating number of downloads from following data
                    if P = 0 then
                      SCount := UTF8Trim(UTF8Copy(Line, 1, Length(Line)))                       // in the case there is no data following the download count
                    else
                      SCount := UTF8Trim(UTF8Copy(Line, 1, P - 1));                             // usual case, with further data after download count
                    Val(SCount, Count, Code);
                  end;
                  if Code <> 0 then
                    Mess := 'Invalid download count in statistic file of ' + Months[I];
                end;
              end;
              // If the actual download directory and the actual file type (extension) is among those, that
              // have to be counted, do so (for actual month), unless this file is part of the exlusion list
              if (Mess = '') and (NStatsDownloads > 0) then begin
                for J := 1 to NStatsDownloads do begin
                  if ((DirURL = StatsDownloads[J].DirectoryURL1) or (DirURL = StatsDownloads[J].DirectoryURL2))
                  and (FileExt = StatsDownloads[J].FileExtension) then begin
                    // Check if file isn't part of the exclusion list
                    DoCount := True;
                    if Length(StatsDownloads[J].FileExcludes) > 0 then begin
                      for K := 0 to Length(StatsDownloads[J].FileExcludes) - 1 do begin
                        if FileName = StatsDownloads[J].FileExcludes[K] then
                          DoCount := False;
                      end;
                    end;
                    if DoCount then begin
                      // This download has to be counted (for actual month)
                      MonthlyDownloads[I, J].FileDownloads  += 1;                               // monthly downloads (different files)
                      MonthlyDownloads[I, J].TotalDownloads += Count;                           // monthly downloads (total files)
                      MonthlyDownloads[13, J].TotalDownloads += Count;                          // yearly downloads (total files)
                      Total += Count;                                                           // yearly downloads (global total)
                    end;
                  end;
                end;
              end;
            end;
          end;
          Close(InFile);
        end;
      end;
    end;
  end;
  if (Mess = '') and (NStatsDownloads > 0) then begin
    // Write download statistics to text file
    // Formatting text as a table with FreePascal is not so evident
    // (unless there are functions, that I do not know about?)
    OutputFilename := OutputDir + '/downloads' + IntToStr(CountYear) + '.txt'; DoDirseparators(OutputFilename);
    Assign(OutFile, OutputFilename); ReWrite(OutFile);
    Writeln(OutFile, 'Download statistics ' + IntToStr(CountYear) + '.'); Writeln(OutFile);
    L := Length(StatsDownloads[1].Description);
    for I := 2 to NStatsDownloads do begin
      if Length(StatsDownloads[I].Description) > L then
        L := Length(StatsDownloads[I].Description);
    end;
    L += 5;
    // Write month names
    Write(OutFile, SpaceString(L));
    for I := 1 to 13 do begin
      if (I <= CountLastMonth) or (I = 13) then
        Write(OutFile, Months[I], SpaceString(5));
    end;
    Writeln(OutFile);
    // Write counts for each of the downloads counts (as defined in the config file)
    for I := 1 to NStatsDownloads do begin
      // Downloads descriptions
      Write(OutFile, StatsDownloads[I].Description);
      Write(OutFile, SpaceString(L - Length(StatsDownloads[I].Description)));
      // Different files downloads counts (for each month)
      for J := 1 to 13 do begin
        if J <= CountLastMonth then begin
          Write(OutFile, SpaceString(Length(Months[J]) - 4));
          Write(OutFile, MonthlyDownloads[J, I].FileDownloads:4, SpaceString(5));
        end;
      end;
      Writeln(OutFile);
      // Total files downloads counts (for each month + total)
      Write(OutFile, SpaceString(L));
      for J := 1 to 13 do begin
        if (J <= CountLastMonth) or (J = 13) then begin
          Write(OutFile, SpaceString(Length(Months[J]) - 4));
          Write(OutFile, MonthlyDownloads[J, I].TotalDownloads:4, SpaceString(5));
        end;
      end;
      Writeln(OutFile);
    end;
    Close(OutFile);
  end;
  if Mess = '' then begin
    // Program terminated normally
    StatsFilename := 'downloads' + IntToStr(CountYear) + '.txt';
    if Total > 0 then
      Writeln('Total downloads = ', Total, ' - statistics in file ', StatsFilename)
    else begin
      Writeln('No downloads found! Error in statistics filename or bad year?');
      DeleteFile(StatsFilename);
    end;
  end
  else begin
    // Program terminated with error
    Writeln('Program error: ', Mess, '!');
  end;
  Writeln;
  Write('Hit ENTER to terminate the program...'); Readln;
end.

