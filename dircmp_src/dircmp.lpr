{*****************************}
{* Compare directory content *}
{*****************************}

program dircmp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, LazUTF8, Dos;

type
  TFile = record
    FName0, FName, FExt: string;
    FType: Char;
    FDate: string;
    FSize: Int64;
  end;
  TFiles = array of TFile;

const
  DefaultCmpFile = 'dircmp.txt';

var
  Err, PX, PC, PO, NSource, NDest, NResults, SX, DX, P: Integer;
  Display, Source, Dest, CmpFile: string;
  ConsiderCase, IgnoreSecs: Boolean;
  Sources, Dests, CmpResults: TFiles;

{ Ckeck if difference between two date-time strings is less than a minute }

function SameDateTime(DateTime1, DateTime2: string; IgnoreSecs: Boolean): Boolean;

// String format: "yyyy-mm-dd hh:nn:ss"
// The function only works correctly for both times being the same date!
var
  T1, T2: Integer;
  SD: Boolean;

begin
  SD := True;
  if DateTime1 <> DateTime2 then begin
    SD := False;
    if IgnoreSecs then begin
      // Check only, if "Ignore seconds" option is enabled
      if LeftStr(DateTime1, 10) = LeftStr(DateTime2, 10) then begin
        // Check is actually only done for two times at same date
        T1 := 3600 * StrToInt(Copy(DateTime1, 12, 2)) + 60 * StrToInt(Copy(DateTime1, 15, 2)) + StrToInt(Copy(DateTime1, 18, 2));
        T2 := 3600 * StrToInt(Copy(DateTime2, 12, 2)) + 60 * StrToInt(Copy(DateTime2, 15, 2)) + StrToInt(Copy(DateTime2, 18, 2));
        if Abs(T1 - T2) < 60 then
          SD := True;
      end;
    end;
  end;
  Result := SD;
end;

{ Sort array of TFile records }

procedure Sort(var Files: TFiles);

// Sort by filename, then by extension

var
  I, J: Integer;
  Swap: Boolean;
  F: TFile;

begin
  for I := 0 to Length(Files) - 2 do begin
    for J := I + 1 to Length(Files) - 1 do begin
      Swap := False;
      if Files[I].FType > Files[J].FType then
        Swap := True
      else if Files[I].FType = Files[J].FType then begin
        if (Files[I].FName > Files[J].FName) or ((Files[I].FName = Files[J].FName) and (Files[I].FExt > Files[J].FExt)) then
          Swap := True;
      end;
      if Swap then begin
        F := Files[I]; Files[I] := Files[J]; Files[J] := F;
      end;
    end;
  end;
end;

{ Display program title }

procedure DisplayTitle;

const
  Version = '1.0';
  Author = 'allu';
  DateWritten = 'January 2024';

var
  S: string;

begin
  S := 'Directory compare, v' + Version + ', (c) ' + Author + ', ' + DateWritten;
  Writeln(S);
end;

{ Display program help }

procedure DisplayHelp;

const
  HelpFile = 'help.txt';

var
  Line: string;
  Handle: Text;

begin
  Assign(Handle, HelpFile); Reset(Handle);
  while not EoF(Handle) do begin
    Readln(Handle, Line);
    if Line <> '' then
      Writeln(Line);
  end;
  Close(Handle);
end;

{ Display error message }

procedure DisplayError(Err: Integer);

const
  Errs: array[1..6] of string = (
    'invalid number of parameters', 'invalid source directory', 'invalid destination directory', 'invalid display parameter',
    'invalid option parameter', 'invalid -o option: filename missing'
  );

var
  S: string;

begin
  S := 'dircmp error: ' + Errs[Err];
  if (Err <> 2) and (Err <> 3) and (Err <> 6) then
    S += '; run "dircmp --help" for help';
  Writeln(S);
end;

{ Find all files in given directory }

procedure FindFiles(Dir: string; out Files: TFiles; out N: Integer);

var
  FileInfo: SysUtils.TSearchRec;

begin
  N := 0; SetLength(Files, 0);
  if SysUtils.FindFirst(Dir + '\*', faAnyFile, FileInfo) = 0 then begin
    // If there are any files or subdirectories, get them
    repeat
      // Consider only "real" directories
      if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then begin
        SetLength(Files, N + 1);
        Files[N].FName0 := FileInfo.Name;
        // "FName0" is full filename (as displayed in compare results)
        // "FName" and "FExt" will be used for compare (if case is not considered, set them to lowercase)
        if ConsiderCase then
          Files[N].FName  := Files[N].FName0
        else
          Files[N].FName  := UTF8Lowercase(Files[N].FName0);
        Files[N].FExt  := ExtractFileExt(Files[N].FName);
        Files[N].FName := UTF8Copy(Files[N].FName, 1, Length(Files[N].FName) - Length(Files[N].FExt));
        Files[N].FExt  := UTF8Copy(Files[N].FExt, 2, Length(Files[N].FExt));
        // File date
        Files[N].FDate := FormatDateTime('yyyy-mm-dd hh:nn:ss', FileDateToDateTime(FileInfo.Time));
        // File size
        if ((FileInfo.Attr and faDirectory) <> 0) then begin
          // This is a folder (subdirectory)
          Files[N].FType := 'D';
          Files[N].FSize := 0;
        end
        else begin
          // This is a file
          Files[N].FType := 'F';
          Files[N].FSize := FileInfo.Size;
        end;
        Inc(N);
      end;
    until SysUtils.FindNext(FileInfo) <> 0;
    Sort(Files);                                                               // Sort the TFiles array (on "FName", then on "FExt")
  end;
end;

{ Update the compare results TFile record }

procedure UpdateResults(SDFile: TFile; var Results: TFiles; var NR: Integer);

begin
  Inc(NR); SetLength(Results, NR);
  with Results[NR - 1] do begin
    FName0 := SDFile.FName0;
    FName  := SDFile.FName;
    FExt   := SDFile.FExt;
    FType  := SDFile.FType;
    FDate  := SDFile.FDate;
    FSize  := SDFile.FSize;
  end;
end;

{ Write compare results to text file (or console) }

procedure WriteResults(var Results: TFiles; NR: Integer; CmpFile: string);

var
  I: Integer;
  Handle: Text;

begin
  if FileExists(CmpFile) then begin
    if UTF8UpperCase(CmpFile) <> 'CON:' then                                   // CON: outputs to the console
      DeleteFile(CmpFile);
  end;
  if NR = 0 then
    Writeln('No files found for criteria specified...')
  else begin
    Writeln('Number of files found: ', NR);
    Assign(Handle, CmpFile); Rewrite(Handle);
    for I := 0 to NR - 1 do begin
      if Results[I].FType = 'D' then
        Writeln(Handle, '[', Results[I].FName0, ']')
      else
        Writeln(Handle, Results[I].FName0);
    end;
    Close(Handle);
  end;
end;

{****************}
{* Main program *}
{****************}

begin
  Err := 0;
  DisplayTitle;
  // If single parameter is --help, display help text
  if (ParamCount = 1) and (ParamStr(1) = '--help') then
    DisplayHelp
  // All other cases
  else begin
    PX := 0; Display := '--diff';
    if LeftStr(ParamStr(1), 2) = '--' then begin
      Display := ParamStr(1);
      if ((Display = '--source') or (Display = '--dest') or (Display = '--same') or (Display = '--diff') or
         (Display = '--size') or (Display = '--date')) then
        PX := 1
      else
        Err := 4;                                                              // invalid display option
    end;
    if Err = 0 then begin
      if ParamCount < PX + 2 then
        Err := 1                                                               // too few parameters
      else begin
        Inc(PX); Source := ParamStr(PX);
        Inc(PX); Dest := ParamStr(PX);
        if not DirectoryExists(Source) then
          Err := 2                                                             // invalid source directory
        else if (not DirectoryExists(Dest)) or (Dest = Source) then
          Err := 3;                                                            // invalid destination directory
      end;
      if Err = 0 then begin
        ConsiderCase := False; IgnoreSecs := False; CmpFile := DefaultCmpFile;
        if ParamCount > PX + 4 then
          Err := 1                                                             // too many parameters
        else if ParamCount > PX then begin
          // Check program options
          PC := ParamCount - PX; PO := 0; P := PX;
          repeat
            Inc(P);
            if ParamStr(P) = '-c' then begin
              ConsiderCase := True; Dec(PC);
            end
            else if ParamStr(P) = '-s' then begin
              IgnoreSecs := True; Dec(PC);
            end
            else if ParamStr(P) = '-o' then begin
              if P < ParamCount then begin
                if LeftStr(ParamStr(P + 1), 1) = '-' then                      // if parameter starts with "-", it is considered to be an option
                  PO := -1
                else begin
                  PO := P + 1;
                  CmpFile := ParamStr(PO);                                     // if parameter doesn't start with "-", it is considered to be the output filename
                  Dec(PC); Inc(P);
                end;
              end
              else
                Err := 6;                                                      // missing output filename (with -o option specified)
              Dec(PC);
            end
            else begin
              Err := 5;                                                        // invalid option parameter
              Dec(PC);
            end;
          until PC = 0;
          if Err = 0 then begin
            if PO = -1 then
              Err := 6;                                                        // missing output filename (with -o option specified followed by parameter starting with "-")
          end;
        end;
      end;
    end;
    // All commandline parameters ok, do the directory compare
    if Err = 0 then begin
      FindFiles(Source, Sources, NSource);                                     // get files in source directory
      FindFiles(Dest, Dests, NDest);                                           // get files in destination directory
      // Both directories are empty
      if (NSource = 0) and (NDest = 0) then
        Writeln('Source and destination directories are empty!')
      // All other cases: Do the compare depending on "Display" parameter (and options)
      else begin
        SX := 0; DX := 0;
        NResults := 0; SetLength(CmpResults, 0);
        repeat
          if SX >= NSource then begin
            // Source files have all been processed
            if Display = '--dest' then
              UpdateResults(Dests[DX], CmpResults, NResults);
            Inc(DX);
          end
          else if DX >= NDest then begin
            // Destination files have all been processed
            if Display = '--source' then
              UpdateResults(Sources[SX], CmpResults, NResults);
            Inc(SX);
          end
          else begin
            // There are still files in both lists
            if Sources[SX].FType = Dests[DX].FType then begin
              if Sources[SX].FName = Dests[DX].FName then begin
                // Files with same filename
                if Sources[SX].FExt = Dests[DX].FExt then begin
                  // The file is present in both directories
                  if Display = '--same' then begin
                    // Results = files with same date and same size
                    if SameDateTime(Sources[SX].FDate, Dests[DX].FDate, IgnoreSecs) and (Sources[SX].FSize = Dests[DX].FSize) then
                      UpdateResults(Sources[SX], CmpResults, NResults);
                  end
                  else if Display = '--diff' then begin
                    // Results = files with different date or different size
                    if (not SameDateTime(Sources[SX].FDate, Dests[DX].FDate, IgnoreSecs)) or (Sources[SX].FSize <> Dests[DX].FSize) then
                      UpdateResults(Sources[SX], CmpResults, NResults);
                  end
                  else if Display = '--date' then begin
                    // Results = files with same size, but different data
                    if (not SameDateTime(Sources[SX].FDate, Dests[DX].FDate, IgnoreSecs)) and (Sources[SX].FSize = Dests[DX].FSize) then
                      UpdateResults(Sources[SX], CmpResults, NResults);
                  end
                  else if Display = '--size' then begin
                    // Results = files with same date, but different size
                    if SameDateTime(Sources[SX].FDate, Dests[DX].FDate, IgnoreSecs) and (Sources[SX].FSize <> Dests[DX].FSize) then
                      UpdateResults(Sources[SX], CmpResults, NResults);
                  end;
                  Inc(SX); Inc(DX);
                end
                else begin
                  // The file is only present in one of the directories
                  if Sources[SX].FExt < Dests[DX].FExt then begin
                    // File only present in source directory
                    if Display = '--source' then
                      UpdateResults(Sources[SX], CmpResults, NResults);
                    Inc(SX);
                  end
                  else begin
                    // File only present in destination directory
                    if Display = '--dest' then
                      UpdateResults(Dests[DX], CmpResults, NResults);
                    Inc(DX);
                  end;
                end;
              end
              else begin
                // The file is only present in one of the directories)
                if Sources[SX].FName < Dests[DX].FName then begin
                  // File only present in source directory
                  if Display = '--source' then
                    UpdateResults(Sources[SX], CmpResults, NResults);
                  Inc(SX);
                end
                else begin
                  // File only present in destination directory
                  if Display = '--dest' then
                    UpdateResults(Dests[DX], CmpResults, NResults);
                  Inc(DX);
                end;
              end;
            end
            else begin
              // The file is only present in one of the directories)
              if Sources[SX].FType < Dests[DX].FType then begin
                // File only present in source directory
                if Display = '--source' then
                  UpdateResults(Sources[SX], CmpResults, NResults);
                Inc(SX);
              end
              else begin
                // File only present in destination directory
                if Display = '--dest' then
                  UpdateResults(Dests[DX], CmpResults, NResults);
                Inc(DX);
              end;
            end;
          end;
        until (SX >= NSource) and (DX >= NDest);
      end;
      WriteResults(CmpResults, NResults, CmpFile);                             // write compare results to text file (or console)
    end;
  end;
  if Err <> 0 then
    DisplayError(Err);                                                         // display error message if invalid parameters on command line
end.

