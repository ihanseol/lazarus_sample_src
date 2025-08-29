{***********************************************}
{* Simple text file merge command line program *}
{***********************************************}

program tmerge;

// Program similar to tsort.pas, but with several input files.
// The program actually does not a merge operation, but creates a
// temporary file by appending the lines of all input files and
// then sorts this file with creation of the output file.

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils, LazUTF8;

type
  TSortCol = record
    Param: string;
    CStart, CEnd: Integer;
  end;
  TSortCols = array of TSortCol;
  TAccentChar = record
    ALetter, BLetter: string;
  end;
  TAccentChars = array of TAccentChar;

const
  Version = '1.0';
  DateWritten = 'August-September 2021';
  ErrMess: array[1..9] of string = (
    'Invalid number of parameters',
    'Invalid parameter(s)',
    'No output file specified',
    '',
    'Input file not found',
    'Can''t create output file',
    'No sort columns specified',
    'Invalid sort columns format',
    ''
  );

var
  ColLen, SortLen, L, NInTotal, NOutTotal, PX, C, Err, I, J, P, Code: Integer;
  Param, OutputFile, TempFile1, TempFile2, SortOrder, SortKey, SortKeys, OldSortKeys, Line: string;
  SimpleSort, SortCase, SortSpaces, SortSymbols, SortByCharCode, RemoveDuplicates, FileOverride, RenameOK: Boolean;
  InputFiles, Lines: array of string;
  SortCols: TSortCols;
  AccentChars: TAccentChars;
  InFile, OutFile: Text;

{ Read table of letters with accents from text file }

procedure ReadAccentChars(out AccentChars: TAccentChars);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'accents.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(AccentChars, N);
      AccentChars[N - 1].ALetter := UTF8Trim(UTF8Copy(Line, 1, 1));
      AccentChars[N - 1].BLetter := UTF8Trim(UTF8Copy(Line, 3, UTF8Length(Line)));
    end;
  end;
  Close(InFile);
end;

{ Text array sort on sortkey (substring with given length at pos. 1 of array elements), using Quicksort algorithm }

procedure Quicksort(var Data: array of string; NOutTotal: Integer; SortOder: string; SortLen: Integer);

  procedure Partition(Left, Right: Integer);

  var
    LeftArrow, RightArrow: Integer;
    Pivot, Temp: string;

  begin
    LeftArrow := Left; RightArrow := Right;
    Pivot := Data[(Left + Right) div 2];
    repeat
      if SortOder = 'ASC' then begin
        // Ascending sort order
        if SortLen = -1 then begin
          // Simple sort (no keyfield)
          while Data[LeftArrow] < Pivot do
            Inc(LeftArrow);
          while Data[RightArrow] > Pivot do
            Dec(RightArrow);
        end
        else begin
          // Sort on keyfield (substring)
          while UTF8Copy(Data[LeftArrow], 1, SortLen) < UTF8Copy(Pivot, 1, SortLen) do
            Inc(LeftArrow);
          while UTF8Copy(Data[RightArrow], 1, SortLen) > UTF8Copy(Pivot, 1, SortLen) do
            Dec(RightArrow);
        end;
      end
      else begin
        // Descending sort order
        if SortLen = -1 then begin
          // Simple sort (no keyfield)
          while Data[LeftArrow] > Pivot do
            Inc(LeftArrow);
          while Data[RightArrow] < Pivot do
            Dec(RightArrow);
        end
        else begin
          // Sort on keyfield (substring)
          while UTF8Copy(Data[LeftArrow], 1, SortLen) > UTf8Copy(Pivot, 1, SortLen) do
            Inc(LeftArrow);
          while UTF8Copy(Data[RightArrow], 1, SortLen) < UTF8Copy(Pivot, 1, SortLen) do
            Dec(RightArrow);
        end;
      end;
      if LeftArrow <= RightArrow then begin
        Temp := Data[LeftArrow]; Data[LeftArrow] := Data[RightArrow]; Data[RightArrow] := Temp;
        Inc(LeftArrow); Dec(RightArrow);
      end;
    until LeftArrow > RightArrow;
    // Recursive calls to partitionning procedure if sort not yet complete
    if Left < RightArrow then
      Partition(Left, RightArrow);
    if Right > LeftArrow then
      Partition(LeftArrow, Right);
  end;

begin
  Partition(0, NOutTotal);                                                     // initial call to partitionning procedure
end;

{ Display of program help (from text file) }

procedure DisplayHelp;

var
  Line: string;
  HelpFile: Text;

begin
  Assign(HelpFile, 'help.txt'); Reset(HelpFile);
  Writeln;
  while not EoF(HelpFile) do begin
    Readln(HelpFile, Line); Writeln(Line);
  end;
  Close(HelpFile);
end;

{****************}
{* Main program *}
{****************}

begin
  Writeln;
  Writeln('tmerge, version ', Version, ', (c) allu ', DateWritten, '.');
  ReadAccentChars(AccentChars);
  SimpleSort := True; SortOrder := 'ASC';
  SortCase := False; SortSpaces := False; SortSymbols := False;
  SortByCharCode := False; RemoveDuplicates := False; FileOverride := False;
  Err := 0;
  // If program is launched as "tmerge help": Display program help
  if (ParamCount = 1) and (UpperCase(ParamStr(1)) = 'HELP') then begin
    DisplayHelp;
  end
  // If program is launched as "tmerge file-1 file-2 ...": Check parameters and (if ok) do the merge
  else begin
    if ParamCount < 3 then
      Err := 1
    else begin
      // First parameters are the input file names (there must be at least 2)
      PX := 1;
      repeat
        SetLength(InputFiles, PX);
        if (PX <= 2) or (LeftStr(ParamStr(PX), 3) <> 'OUT') then
          InputFiles[PX - 1] := ParamStr(PX);
        if PX < ParamCount then
          Inc(PX);
      until (LeftStr(ParamStr(PX), 3) = 'OUT') or (PX = ParamCount);
      for I := 1 to Length(InputFiles) - 1 do begin
        if Err = 0 then begin
          P := UTF8Pos('/', InputFiles[I]);
          if P = 0 then
            P := UTF8Pos('\', InputFiles[I]);
          if P = 0 then                                                        // if the path contains no directory, use the one of file-1
            InputFiles[I] := ExtractFilePath(InputFiles[0]) + InputFiles[I];
          if not FileExists(InputFiles[I]) then
            Err := 5;
        end;
      end;
      // Input filenames are ok, continue with check of next parameter (if there is any)
      if Err = 0 then begin
        if ParamCount >= PX then begin
          // Mandatory parameter 'OUT' specifying the output file name
          if UpperCase(LeftStr(ParamStr(PX), 4)) = 'OUT:' then begin
            OutputFile := UTF8Copy(ParamStr(PX), 5, UTF8Length(ParamStr(PX)));
            P := UTF8Pos('/', OutputFile);
            if P = 0 then
              P := UTF8Pos('\', OutputFile);
            if P = 0 then                                                      // if the path contains no directory, use the one of file-1
              OutputFile := ExtractFilePath(InputFiles[0]) + OutputFile;
          end
          else
            Err := 3;
        end;
      end;
      // Input files and output file are ok, continue with check of next parameter (if there is any)
      if Err = 0 then begin
        if ParamCount > PX then begin
          // If sortorder parameter is given...
          if (UpperCase(ParamStr(PX + 1)) = 'ASC') or (UpperCase(ParamStr(PX + 1)) = 'DESC') then begin
            Inc(PX);
            SortOrder := UpperCase(ParamStr(PX));
          end;
        end;
        if ParamCount > PX then begin
          // If sortcolums list parameter is given...
          if UpperCase(LeftStr(ParamStr(PX + 1), 5)) = 'COLS:' then begin
            Inc(PX);
            Param := ParamStr(PX); Delete(Param, 1, 5);
            SimpleSort := False; C := 0;
            // Parse sortcolums list parameter and extract individual sortcolumn parameters
            while Param <> '' do begin
              Inc(C); SetLength(SortCols, C);
              P := Pos(',', Param);                                            // individual sortcolumn parameters separated by comma
              if P = 0 then begin
                SortCols[C - 1].Param := Param;
                Param := '';
              end
              else begin
                SortCols[C - 1].Param := LeftStr(Param, P - 1);
                Delete(Param, 1, P);
              end;
            end;
            if C = 0 then                                                      // parameter was "COLS:" without any column info
              Err := 7
            else begin
              // For each sortcolumn parameter, extract column start and end position
              for I := 0 to Length(SortCols) - 1 do begin
                P := Pos('-', SortCols[I].Param);                              // start and end position separated by hyphen
                if (P = 0) or (P = Length(SortCols[I].Param)) then             // start or end value missing
                  Err := 8
                else begin
                  // Check if start and end values are positive numbers; start must be less or equal to end
                  Val(LeftStr(SortCols[I].Param, P - 1), SortCols[I].CStart, Code);
                  if (Code <> 0) and (Err = 0) then
                    Err := 8
                  else begin
                    Val(Copy(SortCols[I].Param, P + 1, Length(SortCols[I].Param)), SortCols[I].CEnd, Code);
                    if (Code <> 0) and (Err = 0) then
                      Err := 8;
                    if (Err = 0) and (SortCols[I].CStart > SortCols[I].CEnd) then
                      Err := 8;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    // Fixed position parameters ok, check options parameters (anything entered at end of command line string)
    if Err = 0 then begin
      if ParamCount > PX then begin
        Inc(PX);
        // For all resting parameters, check if they are valid options (and set these from default to given value)
        for I := PX to ParamCount do begin
          if (not SimpleSort) and (UpperCase(ParamStr(I)) = '-A') then
            SortByCharCode := True
          else if (not SimpleSort) and (UpperCase(ParamStr(I)) = '-C') then
            SortCase := True
          else if (not SimpleSort) and (UpperCase(ParamStr(I)) = '-D') then
            RemoveDuplicates := True
          else if UpperCase(ParamStr(I)) = '-O' then
            FileOverride := True
          else if (not SimpleSort) and (UpperCase(ParamStr(I)) = '-S') then
            SortSpaces := True
          else if (not SimpleSort) and (UpperCase(ParamStr(I)) = '-Y') then
            SortSymbols := True
          else
            Err := 2;                                                          // anything, that is not recognized as a valid parameter
        end;
      end;
    end;
    // All parameters ok: Read input files and write the whole to one temporary file
    if Err = 0 then begin
      TempFile1 := 'tmerge.tmp';
      for I := 0 to Length(InputFiles) - 1 do begin
        if Err = 0 then begin
          Assign(InFile, InputFiles[I]); Reset(InFile);
          {$I-}
          Assign(OutFile, TempFile1);
          if I = 0 then
            // Lines of first input file will be written to newly created file
            Rewrite(OutFile)
          else
            // Lines of other input files will be appended to this file
            Append(OutFile);
          if IOresult <> 0 then
            Err := 6;                                                          // this should normally not happen (at least on Windows)
          {$I+}
          if Err = 0 then begin
            while not EoF(InFile) do begin
              Readln(InFile, Line);
              Writeln(OutFile, Line);
            end;
            Close(OutFile);
          end;
          Close(InFile);
        end;
      end;
    end;
    // Use temporary file with lines from all input files as input, another temporary file as output
    if Err = 0 then begin
      TempFile2 := 'tmerge_sorted.tmp';
      Assign(InFile, TempFile1); Reset(InFile);
      {$I-}
      Assign(OutFile, TempFile2); Rewrite(OutFile);
      if IOresult <> 0 then
        Err := 6;                                                              // this should normally not happen (at least on Windows)
      {$I+}
    end;
    // Read input lines, sort them and write them to (temporary) output file
    if Err = 0 then begin
      NInTotal := 0; NOutTotal := 0;
      // Create array with file lines, plus sortkey added, if there are sortcolumns given, as elements
      while not EoF(InFile) do begin
        // Read file line by line
        Readln(InFile, Line); Inc(NInTotal);
        // Create array with data lines
        if Line <> '' then begin                                               // ignore blank lines
          Inc(NOutTotal);
          SetLength(Lines, NOutTotal);
          // Create sort key (if this has to be done)
          if SimpleSort then begin
            // No sort columns given: array element = inputfile line
            SortLen := -1;                                                     // sortlength of -1 indicating a simple sort
          end
          else begin
            // Sort columns given: array element = sortkey + inputfile line
            SortKeys := ''; SortLen := 0;
            for I := 0 to Length(SortCols) - 1 do begin
              // Global sort key = concatenation of individual column sort keys
              SortKey := '';
              ColLen := SortCols[I].CEnd - SortCols[I].CStart + 1;             // sort key length for this column
              SortKey := UTF8Copy(Line, SortCols[I].CStart, ColLen);           // sort key for this column
              // Transform sortkey, depending on sort options given
              if not SortCase then
                SortKey := UTF8LowerCase(SortKey);
              if not SortSpaces then
                SortKey := StringReplace(SortKey, ' ', '', [rfReplaceAll]);
              if not SortSymbols then begin
                SortKey := StringReplace(SortKey, '-', '', [rfReplaceAll]);
                SortKey := StringReplace(SortKey, '''', '', [rfReplaceAll]);
              end;
              if not SortByCharCode then begin
                for J := 0 to Length(AccentChars) - 1 do
                  SortKey := StringReplace(SortKey, AccentChars[J].ALetter, AccentChars[J].BLetter, [rfReplaceAll]);
              end;
              // All sortkeys must have same length (as given by this sortcolumn parameter start and end values)
              // If actual length of sortkey < character number of this column, add spaces to the right of the key
              L := UTF8Length(SortKey);
              for J := L to ColLen - 1 do
                SortKey += ' ';
              // Add individual column sortkey and sortkey length to global sortkey variables
              SortKeys += SortKey;
              SortLen += UTF8Length(SortKey);
            end;
            // Global sort key added at pos. 1 of string to be passed to the Sort procedure
            Line := SortKeys + Line;
          end;
          // Store line (with or without sort key added) into array
          Lines[NOutTotal - 1] := Line;
        end;
      end;
      Close(InFile);
    end;
    if Err = 0 then begin
      // Do the sort
      Quicksort(Lines, Length(Lines) - 1, SortOrder, SortLen);
      // Write sorted lines to (temporary) output file
      OldSortKeys := '';
      for I := 0 to Length(Lines) - 1 do begin
        if SimpleSort then begin
          // Simple sort: Write all lines to output file
          Writeln(OutFile, Lines[I]);
        end
        else begin
          // Key-based sort: Remove duplicates (if this option is selected)
          SortKeys := UTF8Copy(Lines[I], 1, SortLen);
          if (not RemoveDuplicates) or (RemoveDuplicates and (SortKeys <> OldSortKeys)) then begin
            // Line with new sort key: Write line to output file
            UTF8Delete(Lines[I], 1, SortLen);                                  // remove the sortkey at pos. 1 of array element
            Writeln(OutFile, Lines[I]);
            OldSortKeys := SortKeys;
          end
          else begin
            // Line with sort key equal to the one of previous line: Duplicate to ignore
            Dec(NOutTotal);
          end;
        end;
      end;
      Close(OutFile);
    end;
    // To create the final output file with all input files lines sorted, simply rename the temporary output file created before
    if Err = 0 then begin
      // Overriding an existing output files is only permitted with the -o option specified!
      if FileExists(OutputFile) and (not FileOverride) then
        Err := 6
      else begin
        if FileExists(OutputFile) then
          DeleteFile(OutputFile);
        RenameOK := RenameFile(TempFile2, OutputFile);
        if not RenameOK then
          Err := 6;                                                              // this shoul normally never happen
        DeleteFile(TempFile1);
        if FileExists(TempFile2) then
          DeleteFile(TempFile2);
      end;
    end;
    if Err = 0 then begin
      Writeln('Input file total lines = ', NInTotal, '; output file total lines = ', NOutTotal);
    end
    else begin
      // Message if any parameter or file error
      Write('Program error: ', ErrMess[Err]);
      if not (Err in [5..6]) then begin
        // Command line parameters format (non file) errors
        Write(' - run "tmerge help" for help on program usage');
      end;
    end;
    Writeln;
  end;
end.

